#!/usr/bin/env bash

set -e
set -x

# Create and publish a transaction for each of the two genesis delegates
#  that votes on the same MIR proposal.
#
# Usage:
#
# This scrips needs to be run from the repository root, like so:
#   scripts/voltaire-prototype/mir-proposal_separate-txs.sh <reward_amount>

REWARD="$1"
ROOT=example
INITIAL_FUNDS=1000000000

source scripts/voltaire-prototype/_windows_socket-path.sh
export CARDANO_NODE_SOCKET_PATH=${WINDOWS_SOCKET_PREFIX}${ROOT}/node-bft1/node.sock

EPOCH=$(cardano-cli query tip --prototype-mode --testnet-magic 42 | jq .epoch)

# Create MIR proposal voted on by genesis1 only
cardano-cli governance create-mir-proposal \
            --out-file ${ROOT}/mir-proposal-example-multi-genesis1 \
            --epoch "${EPOCH}" \
            --genesis-verification-key-file ${ROOT}/genesis-keys/genesis1.vkey \
            --reserves \
            --stake-address "$(cat ${ROOT}/addresses/user1-stake.addr)" \
            --reward ${REWARD}

# Create MIR proposal voted on by genesis2 only
cardano-cli governance create-mir-proposal \
            --out-file ${ROOT}/mir-proposal-example-multi-genesis2 \
            --epoch "${EPOCH}" \
            --genesis-verification-key-file ${ROOT}/genesis-keys/genesis2.vkey \
            --reserves \
            --stake-address "$(cat ${ROOT}/addresses/user1-stake.addr)" \
            --reward ${REWARD}

TX_IN1="$(cardano-cli query utxo --prototype-mode --testnet-magic 42| awk '{print $1}' |sed -n '3,3p')#0"
TX_OUT="$(cat ${ROOT}/addresses/user1.addr)"+$INITIAL_FUNDS

# Create transaction body for genesis1 proposal
cardano-cli transaction build-raw --prototype-era-two \
            --invalid-hereafter 100000 \
            --fee 0 \
            --tx-in "${TX_IN1}" \
            --tx-out "${TX_OUT}" \
            --mir-proposal-file ${ROOT}/mir-proposal-example-multi-genesis1 \
            --out-file ${ROOT}/tx-multi1.txbody

# Create transaction for genesis1 proposal
cardano-cli transaction sign \
            --signing-key-file ${ROOT}/addresses/user1.skey \
            --signing-key-file ${ROOT}/delegate-keys/delegate1.skey \
            --testnet-magic 42 \
            --tx-body-file  ${ROOT}/tx-multi1.txbody \
            --out-file      ${ROOT}/tx-multi1.tx

# Submit genesis1 proposal transaction
echo "MIR proposal multi tx: publishing genesis1 MIR proposal..."
cardano-cli transaction submit --prototype-mode --tx-file ${ROOT}/tx-multi1.tx --testnet-magic 42

# Create transaction body for genesis2 proposal
# NB: this transaction will redeem the output created by the genesis1 transaction
TX_ID1="$(cardano-cli transaction txid --tx-body-file ${ROOT}/tx-multi1.txbody)"
cardano-cli transaction build-raw --prototype-era-two \
            --invalid-hereafter 100000 \
            --fee 0 \
            --tx-in "${TX_ID1}#0" \
            --tx-out "${TX_OUT}" \
            --mir-proposal-file ${ROOT}/mir-proposal-example-multi-genesis2 \
            --out-file ${ROOT}/tx-multi2.txbody

# Create transaction for genesis2 proposal
cardano-cli transaction sign \
            --signing-key-file ${ROOT}/addresses/user1.skey \
            --signing-key-file ${ROOT}/delegate-keys/delegate2.skey \
            --testnet-magic 42 \
            --tx-body-file  ${ROOT}/tx-multi2.txbody \
            --out-file      ${ROOT}/tx-multi2.tx

# Wait for the output of the first transaction to appear in the UTXO set
COUNT=0
echo "MIR proposal multi tx: waiting for first tx to be included in block..."
while (! cardano-cli query utxo --prototype-mode --testnet-magic 42 |grep "${TX_ID1}")
do
if [ $COUNT -gt 25 ]; then
    echo "ERROR: output did not appear in UTXO set"
    exit 1
fi
COUNT=$((COUNT+1))
sleep 5
done

# Submit genesis2 proposal transaction
echo "MIR proposal multi tx: publishing genesis2 MIR proposal..."
cardano-cli transaction submit --prototype-mode --tx-file ${ROOT}/tx-multi2.tx --testnet-magic 42
