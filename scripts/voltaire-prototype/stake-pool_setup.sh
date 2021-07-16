#!/usr/bin/env bash

set -e
set -x

# This script will setup staking for the testnet

ROOT=example
TRANSFER_AMOUNT=1000000000

source scripts/voltaire-prototype/_windows_socket-path.sh
export CARDANO_NODE_SOCKET_PATH=${WINDOWS_SOCKET_PREFIX}node-bft1

# Transfer all the funds to the user1, which delegates to pool1.
# Register certs to:
#  1. register the pool-owner1 stake address
#  2. register the stake pool 1
#  3. register the user1 stake address
#  4. delegate from the user1 stake address to the stake pool

cardano-cli transaction build-raw --shelley-era \
            --invalid-hereafter 100000 \
            --fee 0 \
            --tx-in "$(cardano-cli genesis initial-txin \
                --testnet-magic 42 \
                --verification-key-file ${ROOT}/utxo-keys/utxo1.vkey)" \
            --tx-out "$(cat ${ROOT}/addresses/user1.addr)"+${TRANSFER_AMOUNT} \
            --certificate-file ${ROOT}/addresses/pool-owner1-stake.reg.cert \
            --certificate-file ${ROOT}/node-pool1/registration.cert \
            --certificate-file ${ROOT}/addresses/user1-stake.reg.cert \
            --certificate-file ${ROOT}/addresses/user1-stake.deleg.cert \
            --out-file ${ROOT}/tx1.txbody

# So we'll need to sign this with a bunch of keys:
# 1. the initial utxo spending key, for the funds
# 2. the user1 stake address key, due to the delegatation cert
# 3. the pool1 owner key, due to the pool registration cert
# 4. the pool1 operator key, due to the pool registration cert

cardano-cli transaction sign \
            --signing-key-file ${ROOT}/utxo-keys/utxo1.skey \
            --signing-key-file ${ROOT}/addresses/user1-stake.skey \
            --signing-key-file ${ROOT}/node-pool1/owner.skey \
            --signing-key-file ${ROOT}/node-pool1/operator.skey \
            --testnet-magic 42 \
            --tx-body-file  ${ROOT}/tx1.txbody \
            --out-file      ${ROOT}/tx1.tx

cardano-cli transaction submit --prototype-mode --tx-file ${ROOT}/tx1.tx --testnet-magic 42
