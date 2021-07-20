#!/usr/bin/env bash

set -e
set -x

# Create and publish a MIR that transfers funds to ${ROOT}/addresses/user1-stake.addr
#  and a custom stake address.
#
# Usage:
#
# This scrips needs to be run from the repository root, like so:
#
#   scripts/voltaire-prototype/mir-proposal_two-addrs.sh <address_2> <reward_amount_2>
#
# Where the first argument is the second stake address and the second argument
# is the amount to be sent to this address.

ADDRESS2="$1"
REWARD2="$2"

ROOT=example
INITIAL_FUNDS=1000000000
REWARD1=100000000

source scripts/voltaire-prototype/_windows_socket-path.sh
export CARDANO_NODE_SOCKET_PATH=${WINDOWS_SOCKET_PREFIX}${ROOT}/node-bft1/node.sock

EPOCH=$(cardano-cli query tip --prototype-mode --testnet-magic 42 | jq .epoch)

cardano-cli governance create-mir-proposal \
            --out-file ${ROOT}/mir-proposal-example-multi \
            --epoch "${EPOCH}" \
            --genesis-verification-key-file ${ROOT}/genesis-keys/genesis1.vkey \
            --genesis-verification-key-file ${ROOT}/genesis-keys/genesis2.vkey \
            --reserves \
            --stake-address "$(cat ${ROOT}/addresses/user1-stake.addr)" \
            --reward ${REWARD1} \
            --stake-address "${ADDRESS2}" \
            --reward "${REWARD2}"

TX_IN="$(cardano-cli query utxo --prototype-mode --testnet-magic 42| awk '{print $1}' |sed -n '3,3p')#0"
cardano-cli transaction build-raw --prototype-era-two \
            --invalid-hereafter 100000 \
            --fee 0 \
            --tx-in "${TX_IN}" \
            --tx-out "$(cat ${ROOT}/addresses/user1.addr)"+$INITIAL_FUNDS \
            --mir-proposal-file ${ROOT}/mir-proposal-example-multi \
            --out-file ${ROOT}/tx4.txbody

# So we'll need to sign this with a bunch of keys:
# 1. the initial utxo spending key, for the funds
# 2. the genesis delegate keys, due to the MIR proposal

cardano-cli transaction sign \
            --signing-key-file ${ROOT}/addresses/user1.skey \
            --signing-key-file ${ROOT}/delegate-keys/delegate1.skey \
            --signing-key-file ${ROOT}/delegate-keys/delegate2.skey \
            --testnet-magic 42 \
            --tx-body-file  ${ROOT}/tx4.txbody \
            --out-file      ${ROOT}/tx4.tx

cardano-cli transaction submit --prototype-mode --tx-file ${ROOT}/tx4.tx --testnet-magic 42
