#!/usr/bin/env bash

set -e
set -x

# This script will initiate the transition to protocol version 2 (Example).

# You need to provide the current epoch as a positional argument (the Shelley
# update system requires this to be includded in the update proposal). Run
# the command "cardano-cli query tip" to get the current epoch number.


# You need to restart the nodes after running this script in order for the
# update to be endorsed by the nodes.

if [ ! "$1" ]; then echo "update.sh: expects an <N> epoch argument"; exit; fi

EPOCH=$1
VERSION=1

ROOT=example
SUPPLY=1000000000

pushd ${ROOT}

export CARDANO_NODE_SOCKET_PATH=node-bft1/node.sock

cardano-cli governance create-update-proposal \
            --out-file update-proposal-example \
            --epoch ${EPOCH} \
            --genesis-verification-key-file genesis-keys/genesis1.vkey \
            --genesis-verification-key-file genesis-keys/genesis2.vkey \
            --protocol-major-version ${VERSION} \
            --protocol-minor-version 0

# Now we'll construct one whopper of a transaction that does everything
# just to show off that we can, and to make the script shorter

# We'll transfer all the funds to the user1, which delegates to pool1
# We'll register certs to:
#  1. register the pool-owner1 stake address
#  2. register the stake pool 1
#  3. register the user1 stake address
#  4. delegate from the user1 stake address to the stake pool
# We'll include the update proposal

cardano-cli transaction build-raw --shelley-era \
            --invalid-hereafter 100000 \
            --fee 0 \
            --tx-in $(cardano-cli genesis initial-txin \
                --testnet-magic 42 \
                --verification-key-file utxo-keys/utxo1.vkey) \
            --tx-out $(cat addresses/user1.addr)+${SUPPLY} \
            --certificate-file addresses/pool-owner1-stake.reg.cert \
            --certificate-file node-pool1/registration.cert \
            --certificate-file addresses/user1-stake.reg.cert \
            --certificate-file addresses/user1-stake.deleg.cert \
            --update-proposal-file update-proposal-example \
            --out-file tx1.txbody

# So we'll need to sign this with a bunch of keys:
# 1. the initial utxo spending key, for the funds
# 2. the user1 stake address key, due to the delegatation cert
# 3. the pool1 owner key, due to the pool registration cert
# 4. the pool1 operator key, due to the pool registration cert
# 5. the genesis delegate keys, due to the update proposal

cardano-cli transaction sign \
            --signing-key-file utxo-keys/utxo1.skey \
            --signing-key-file addresses/user1-stake.skey \
            --signing-key-file node-pool1/owner.skey \
            --signing-key-file node-pool1/operator.skey \
            --signing-key-file genesis-keys/genesis1.skey \
            --signing-key-file genesis-keys/genesis2.skey \
            --signing-key-file delegate-keys/delegate1.skey \
            --signing-key-file delegate-keys/delegate2.skey \
            --testnet-magic 42 \
            --tx-body-file  tx1.txbody \
            --out-file      tx1.tx


cardano-cli transaction submit --prototype-mode --tx-file tx1.tx --testnet-magic 42

popd