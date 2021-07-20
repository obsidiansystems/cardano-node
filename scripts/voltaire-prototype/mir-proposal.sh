#!/usr/bin/env bash

set -e
set -x

# Usage:
#
# This scrips needs to be run from the repository root, like so:
#   scripts/voltaire-prototype/mir-proposal.sh

ROOT=example
INITIAL_FUNDS=1000000000
REWARD=250000000

source scripts/voltaire-prototype/_windows_socket-path.sh
export CARDANO_NODE_SOCKET_PATH=${WINDOWS_SOCKET_PREFIX}${ROOT}/node-bft1/node.sock

EPOCH=$(cardano-cli query tip --prototype-mode --testnet-magic 42 | jq .epoch)

cardano-cli governance create-mir-proposal \
            --out-file ${ROOT}/mir-proposal-example \
            --epoch "${EPOCH}" \
            --genesis-verification-key-file ${ROOT}/genesis-keys/genesis1.vkey \
            --genesis-verification-key-file ${ROOT}/genesis-keys/genesis2.vkey \
            --reserves \
            --stake-address "$(cat ${ROOT}/addresses/user1-stake.addr)" \
            --reward ${REWARD}

cardano-cli transaction build-raw --prototype-era-two \
            --invalid-hereafter 100000 \
            --fee 0 \
            --tx-in "$(cardano-cli query utxo --prototype-mode --testnet-magic 42|grep lovelace|awk '{print $1}')#0" \
            --tx-out "$(cat ${ROOT}/addresses/user1.addr)"+$INITIAL_FUNDS \
            --mir-proposal-file ${ROOT}/mir-proposal-example \
            --out-file ${ROOT}/tx3.txbody

# So we'll need to sign this with a bunch of keys:
# 1. the initial utxo spending key, for the funds
# 2. the user1 stake address key, due to the delegatation cert
# 3. the pool1 owner key, due to the pool registration cert
# 4. the pool1 operator key, due to the pool registration cert
# 5. the genesis delegate keys, due to the update proposal

cardano-cli transaction sign \
            --signing-key-file ${ROOT}/addresses/user1.skey \
            --signing-key-file ${ROOT}/genesis-keys/genesis1.skey \
            --signing-key-file ${ROOT}/genesis-keys/genesis2.skey \
            --signing-key-file ${ROOT}/delegate-keys/delegate1.skey \
            --signing-key-file ${ROOT}/delegate-keys/delegate2.skey \
            --testnet-magic 42 \
            --tx-body-file  ${ROOT}/tx3.txbody \
            --out-file      ${ROOT}/tx3.tx

cardano-cli transaction submit --prototype-mode --tx-file ${ROOT}/tx3.tx --testnet-magic 42
