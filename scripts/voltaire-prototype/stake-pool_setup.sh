#!/usr/bin/env bash

set -e
set -x

# This script will setup staking for the testnet

ROOT=example
TRANSFER_AMOUNT=1000000000

pushd ${ROOT}

export CARDANO_NODE_SOCKET_PATH=node-bft1/node.sock

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
                --verification-key-file utxo-keys/utxo1.vkey)" \
            --tx-out "$(cat addresses/user1.addr)"+${TRANSFER_AMOUNT} \
            --certificate-file addresses/pool-owner1-stake.reg.cert \
            --certificate-file node-pool1/registration.cert \
            --certificate-file addresses/user1-stake.reg.cert \
            --certificate-file addresses/user1-stake.deleg.cert \
            --out-file tx1.txbody

# So we'll need to sign this with a bunch of keys:
# 1. the initial utxo spending key, for the funds
# 2. the user1 stake address key, due to the delegatation cert
# 3. the pool1 owner key, due to the pool registration cert
# 4. the pool1 operator key, due to the pool registration cert

cardano-cli transaction sign \
            --signing-key-file utxo-keys/utxo1.skey \
            --signing-key-file addresses/user1-stake.skey \
            --signing-key-file node-pool1/owner.skey \
            --signing-key-file node-pool1/operator.skey \
            --testnet-magic 42 \
            --tx-body-file  tx1.txbody \
            --out-file      tx1.tx

cardano-cli transaction submit --prototype-mode --tx-file tx1.tx --testnet-magic 42

popd