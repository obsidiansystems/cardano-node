#!/usr/bin/env bash

set -e
set -x

# This script will initiate a transition to the specified protocol major version.
#
# To upgrade from Shelley to VoltairePrototypeOne, run the command:
#   scripts/voltaire-prototype/update.sh 1 shelley-era
# To upgrade from VoltairePrototypeOne to VoltairePrototypeTwo, run the command:
#   scripts/voltaire-prototype/update.sh 2 prototype-era-one

ROOT=example
SUPPLY=1000000000

source scripts/voltaire-prototype/_windows_socket-path.sh
export CARDANO_NODE_SOCKET_PATH=${WINDOWS_SOCKET_PREFIX}node-bft1

if [ ! "$1" ] || [ ! "$2" ]; then
    echo "Usage: $0 <major_version> <current_era>";
    echo "Example: $0 2 prototype-era-one"
    exit 1;
fi

VERSION=$1
CURRENT_ERA=$2

EPOCH=$(cardano-cli query tip --prototype-mode --testnet-magic 42 | jq .epoch)

UPDATE_PROPOSAL_FILE="${ROOT}/update-proposal-example-v${VERSION}"

cardano-cli governance create-update-proposal \
            --out-file "$UPDATE_PROPOSAL_FILE" \
            --epoch "${EPOCH}" \
            --genesis-verification-key-file ${ROOT}/genesis-keys/genesis1.vkey \
            --genesis-verification-key-file ${ROOT}/genesis-keys/genesis2.vkey \
            --protocol-major-version "${VERSION}" \
            --protocol-minor-version 0

# Now we'll construct a transaction that contains the update proposal

TX_FILENAME="${ROOT}/tx${VERSION}.tx"
TX_IN=$(cardano-cli query utxo --prototype-mode --testnet-magic 42| awk '{print $1}' |sed -n '3,3p')
cardano-cli transaction build-raw --"${CURRENT_ERA}" \
            --invalid-hereafter 100000 \
            --fee 0 \
            --tx-in "${TX_IN}#0" \
            --tx-out "$(cat ${ROOT}/addresses/user1.addr)"+${SUPPLY} \
            --update-proposal-file "$UPDATE_PROPOSAL_FILE" \
            --out-file "${TX_FILENAME}body"

# So we'll need to sign this with a bunch of keys:
# 1. the initial utxo spending key, for the funds
# 2. the user1 stake address key, due to the delegatation cert
# 3. the pool1 owner key, due to the pool registration cert
# 4. the pool1 operator key, due to the pool registration cert
# 5. the genesis delegate keys, due to the update proposal

cardano-cli transaction sign \
            --signing-key-file ${ROOT}/addresses/user1.skey \
            --signing-key-file ${ROOT}/utxo-keys/utxo1.skey \
            --signing-key-file ${ROOT}/addresses/user1-stake.skey \
            --signing-key-file ${ROOT}/node-pool1/owner.skey \
            --signing-key-file ${ROOT}/node-pool1/operator.skey \
            --signing-key-file ${ROOT}/genesis-keys/genesis1.skey \
            --signing-key-file ${ROOT}/genesis-keys/genesis2.skey \
            --signing-key-file ${ROOT}/delegate-keys/delegate1.skey \
            --signing-key-file ${ROOT}/delegate-keys/delegate2.skey \
            --testnet-magic 42 \
            --tx-body-file  "${TX_FILENAME}body" \
            --out-file      "${TX_FILENAME}"


cardano-cli transaction submit --prototype-mode --tx-file "${TX_FILENAME}" --testnet-magic 42
