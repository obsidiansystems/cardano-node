#!/usr/bin/env bash

set -e
set -x

# This script will initiate a transition to the specified protocol major version.

ROOT=example
SUPPLY=1000000000

pushd ${ROOT}

export CARDANO_NODE_SOCKET_PATH=node-bft1/node.sock

if [ ! "$1" ] || [ ! "$2" ]; then
    echo "Usage: $0 <major_version> <current_era>";
    echo "Example: $0 2 prototype-era-one"
    exit 1;
fi

VERSION=$1
CURRENT_ERA=$2

EPOCH=$(cardano-cli query tip --prototype-mode --testnet-magic 42 | jq .epoch)


cardano-cli governance create-update-proposal \
            --out-file "update-proposal-example-v${VERSION}" \
            --epoch "${EPOCH}" \
            --genesis-verification-key-file genesis-keys/genesis1.vkey \
            --genesis-verification-key-file genesis-keys/genesis2.vkey \
            --protocol-major-version "${VERSION}" \
            --protocol-minor-version 0

# Now we'll construct a transaction that contains the update proposal

TX_IN=$(cardano-cli query utxo --prototype-mode --testnet-magic 42| awk '{print $1}' |sed -n '3,3p')
cardano-cli transaction build-raw --"${CURRENT_ERA}" \
            --invalid-hereafter 100000 \
            --fee 0 \
            --tx-in "${TX_IN}#0" \
            --tx-out "$(cat addresses/user1.addr)"+${SUPPLY} \
            --update-proposal-file "update-proposal-example-v${VERSION}" \
            --out-file "tx${VERSION}.txbody"

# So we'll need to sign this with a bunch of keys:
# 1. the initial utxo spending key, for the funds
# 2. the user1 stake address key, due to the delegatation cert
# 3. the pool1 owner key, due to the pool registration cert
# 4. the pool1 operator key, due to the pool registration cert
# 5. the genesis delegate keys, due to the update proposal

cardano-cli transaction sign \
            --signing-key-file addresses/user1.skey \
            --signing-key-file utxo-keys/utxo1.skey \
            --signing-key-file addresses/user1-stake.skey \
            --signing-key-file node-pool1/owner.skey \
            --signing-key-file node-pool1/operator.skey \
            --signing-key-file genesis-keys/genesis1.skey \
            --signing-key-file genesis-keys/genesis2.skey \
            --signing-key-file delegate-keys/delegate1.skey \
            --signing-key-file delegate-keys/delegate2.skey \
            --testnet-magic 42 \
            --tx-body-file  "tx${VERSION}.txbody" \
            --out-file      "tx${VERSION}.tx"


cardano-cli transaction submit --prototype-mode --tx-file "tx${VERSION}.tx" --testnet-magic 42

popd