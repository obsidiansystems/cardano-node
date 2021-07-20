#!/usr/bin/env bash

# Usage:
#
# This scrips needs to be run from the repository root, like so:
#   scripts/voltaire-prototype/demo.sh
#
# Before running this script you should, also from the repository root,
# run either:
#
#    (a) scripts/voltaire-prototype/mkfiles.sh
#       * and then start the nodes manually (as per the printed instructions), or
#
#    (b) scripts/voltaire-prototype/mkfiles.sh test
#       * which automatically starts the nodes (and you need to kill them when you're done)
#

set -e

ROOT=example

source scripts/voltaire-prototype/_windows_socket-path.sh
export CARDANO_NODE_SOCKET_PATH="${WINDOWS_SOCKET_PREFIX}${ROOT}/node-bft1/node.sock"


function assert_nonempty_proposals () {
  echo -n "Verifying that MIR proposal has been registered... "
  sleep 10 # ideally we would wait until the "MIR proposal" transaction has been included in a block
  if ! PROPOSALS=$(cardano-cli query ledger-state --prototype-mode --testnet-magic 42 | jq .stateBefore.esLState.utxoState.ppups.proposals); then exit 1; fi
  if [ "$PROPOSALS" = "{}" ]; then echo "ERROR: no proposals registered"; exit 1; else echo "OK"; fi
}

# Get the reward balance for a given address
#
# Usage: reward_balance <stake_address>
function reward_balance () {
  cardano-cli query stake-address-info --prototype-mode --address "$1" --testnet-magic 42 | jq '.[0].rewardAccountBalance'
}

# Wait until the given reward address has the given balance
#
# Usage: wait_for_reward_balance <stake_address> <balance>
function wait_for_reward_balance () {
  STAKE_ADDRESS="$1"
  TARGET_BALANCE="$2"
  REWARD_BALANCE="null"
  echo -n "Reward balance for address $STAKE_ADDRESS:"
  while [ "$REWARD_BALANCE" != "$TARGET_BALANCE" ]
  do
    if ! REWARD_BALANCE=$(reward_balance "$STAKE_ADDRESS"); then exit 1; fi
    echo -n " $REWARD_BALANCE"
    sleep 5
  done
  echo ""
}

function wait_for_era () {
  echo "Waiting for nodes to upgrade to $1..."
  echo -n "Current era:"
  while [ "$ERA_NAME" != "$1" ]
  do
    sleep 5
    if ! ERA_NAME=$(cardano-cli query tip --prototype-mode --testnet-magic 42| jq -r .era); then exit 1; fi
    echo -n " $ERA_NAME"
  done
  echo
  echo "Hard fork to $1 completed."
}

function wait_for_node_connect () {
  # Windows error output example:
  #   cardano-cli.exe: CreateFile "example/node-bft1/node.sock": does not exist (The system cannot find the file specified.)
  # UNIX error output example:
  #   cardano-cli: Network.Socket.connect: <socket: 13>: does not exist (No such file or directory)
  #   cardano-cli: Network.Socket.connect: <socket: 13>: does not exist (Connection refused)
  local REGEX='(CreateFile|Network\.Socket\.connect).*does not exist'
  COUNT=0
  echo "Waiting for successful connection to node..."
  while (cardano-cli query tip --prototype-mode --testnet-magic 42 2>&1 |grep -E "$REGEX")
  do
    if [ $COUNT -gt 25 ]; then
      echo "ERROR: failed to connect to node"
      exit 1
    fi
    COUNT=$((COUNT+1))
    sleep 5
  done
  echo "Connected to node."
}

wait_for_node_connect

echo "Waiting for node to produce blocks..."
BLOCK_NO=0
while [ $BLOCK_NO -le 0 ]
do
  sleep 5
  if ! BLOCK_NO=$(cardano-cli query tip --prototype-mode --testnet-magic 42 |jq .block); then exit 1; fi
done
echo "First block produced."

echo "Setting up staking..."
scripts/voltaire-prototype/stake-pool_setup.sh

sleep 10 # ideally we would wait until the "stake pool" transaction has been included in a block
echo "Initiating PrototypeOne hard fork..."
scripts/voltaire-prototype/update.sh 1 shelley-era

wait_for_era "VoltairePrototypeOne"
echo "Initiating PrototypeTwo hard fork..."
scripts/voltaire-prototype/update.sh 2 prototype-era-one

wait_for_era "VoltairePrototypeTwo"
echo "Publishing MIR proposal..."
scripts/voltaire-prototype/mir-proposal.sh

assert_nonempty_proposals
echo "Waiting to receive MIR transfer..."
wait_for_reward_balance "$(cat ${ROOT}/addresses/user1-stake.addr)" "250000000"

ADDR_2="$(cat ${ROOT}/addresses/pool-owner1-stake.addr)"
ADDR_AMOUNT_2=50000000
echo "Publishing second MIR proposal..."
scripts/voltaire-prototype/mir-proposal_two-addrs.sh "$ADDR_2" "$ADDR_AMOUNT_2"

assert_nonempty_proposals
echo "Waiting to receive both MIR transfers..."
wait_for_reward_balance "$(cat ${ROOT}/addresses/user1-stake.addr)" "350000000"
wait_for_reward_balance "$ADDR_2" "$ADDR_AMOUNT_2"

# Test 3: each genesis delegate votes on the same proposal in a separate transaction
AMOUNT_SEP=345678
scripts/voltaire-prototype/mir-proposal_separate-txs.sh "$AMOUNT_SEP"
wait_for_reward_balance "$(cat ${ROOT}/addresses/user1-stake.addr)" $((350000000+AMOUNT_SEP))

echo "Done!"
