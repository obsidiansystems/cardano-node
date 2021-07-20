#!/usr/bin/env bash

set -e
set -x

# Create and publish a MIR proposal, voted on by both genesis delegates,
#  that pays the specified amount to the specified address.
#
# Usage:
#
# This scrips needs to be run from the repository root, like so:
#   scripts/voltaire-prototype/mir-proposal.sh <reward_address> <reward_amount>

ADDRESS="$1"
AMOUNT="$2"

ROOT=example
INITIAL_FUNDS=1000000000

source scripts/voltaire-prototype/_windows_socket-path.sh
export CARDANO_NODE_SOCKET_PATH=${WINDOWS_SOCKET_PREFIX}${ROOT}/node-bft1/node.sock

EPOCH=$(cardano-cli query tip --prototype-mode --testnet-magic 42 | jq .epoch)

cardano-cli governance create-mir-proposal \
            --out-file ${ROOT}/mir-proposal-example \
            --epoch "${EPOCH}" \
            --genesis-verification-key-file ${ROOT}/genesis-keys/genesis1.vkey \
            --genesis-verification-key-file ${ROOT}/genesis-keys/genesis2.vkey \
            --reserves \
            --stake-address "${ADDRESS}" \
            --reward "${AMOUNT}"

TX_IN="$(cardano-cli query utxo --prototype-mode --testnet-magic 42| awk '{print $1}' |sed -n '3,3p')#0"
cardano-cli transaction build-raw --prototype-era-two \
            --invalid-hereafter 100000 \
            --fee 0 \
            --tx-in "${TX_IN}" \
            --tx-out "$(cat ${ROOT}/addresses/user1.addr)"+$INITIAL_FUNDS \
            --mir-proposal-file ${ROOT}/mir-proposal-example \
            --out-file ${ROOT}/tx3.txbody

# So we'll need to sign this with two keys:
# 1. the initial utxo spending key, for the funds
# 2. the genesis delegate keys, due to the MIR proposal

cardano-cli transaction sign \
            --signing-key-file ${ROOT}/addresses/user1.skey \
            --signing-key-file ${ROOT}/delegate-keys/delegate1.skey \
            --signing-key-file ${ROOT}/delegate-keys/delegate2.skey \
            --testnet-magic 42 \
            --tx-body-file  ${ROOT}/tx3.txbody \
            --out-file      ${ROOT}/tx3.tx

cardano-cli transaction submit --prototype-mode --tx-file ${ROOT}/tx3.tx --testnet-magic 42
