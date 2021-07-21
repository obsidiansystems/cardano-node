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
  COUNT=0
  echo -n "Reward balance for address $STAKE_ADDRESS:"
  while [ "$REWARD_BALANCE" != "$TARGET_BALANCE" ]
  do
    if ! REWARD_BALANCE=$(reward_balance "$STAKE_ADDRESS"); then exit 1; fi
    echo -n " $REWARD_BALANCE"
    sleep 5
    if [ $COUNT -gt 70 ]; then echo "ERROR: waited too long for reward balance"; exit 1; fi
    COUNT=$((COUNT+1))
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

function wait_for_slot () {
  TARGET_SLOT_NUMBER=$1
  SLOT_NUMBER=0
  echo "Waiting for slot number $TARGET_SLOT_NUMBER..."
  echo -n "Current slot number:"
  while [ "$SLOT_NUMBER" -lt "$TARGET_SLOT_NUMBER" ]
  do
    sleep 5
    if ! SLOT_NUMBER=$(cardano-cli query tip --prototype-mode --testnet-magic 42 |jq -r .slot); then exit 1; fi
    echo -n " $SLOT_NUMBER"
  done
  echo
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

########
# Test 1: create and publish a MIR, and verify that funds are received.
wait_for_era "VoltairePrototypeTwo"
echo "Publishing MIR proposal..."
ADDRESS="$(cat ${ROOT}/addresses/user1-stake.addr)"
AMOUNT=250000000
scripts/voltaire-prototype/mir-proposal.sh "$ADDRESS" "$AMOUNT"
assert_nonempty_proposals
echo "Waiting to receive MIR transfer..."
wait_for_reward_balance "$ADDRESS" "$AMOUNT"

########
# Test 2: create and publish a MIR containing two recipients,
#         and verify that funds are received by both recipients.
ADDR_2="$(cat ${ROOT}/addresses/pool-owner1-stake.addr)"
ADDR_AMOUNT_2=50000000
echo "Publishing second MIR proposal..."
scripts/voltaire-prototype/mir-proposal_two-addrs.sh "$ADDR_2" "$ADDR_AMOUNT_2"
assert_nonempty_proposals
echo "Waiting to receive both MIR transfers..."
wait_for_reward_balance "$(cat ${ROOT}/addresses/user1-stake.addr)" "350000000"
wait_for_reward_balance "$ADDR_2" "$ADDR_AMOUNT_2"

########
# Test 3: create and publish two identical MIRs in two separate transactions.
#         the first transaction is signed only by the first delegate, the second only by the second delegate.
#         verify that the funds are received.
AMOUNT_SEP=345678
EXPECTED_BALANCE_SEP=$((350000000+AMOUNT_SEP))
scripts/voltaire-prototype/mir-proposal_separate-txs.sh "$AMOUNT_SEP"
wait_for_reward_balance "$(cat ${ROOT}/addresses/user1-stake.addr)" $EXPECTED_BALANCE_SEP

########
# Test 4: create and publish one MIR, which has sufficient votes.
#         then publish a different MIR, also with sufficient votes.
#         verify that only the funds of the second MIR are received.
echo "Two MIRs: publishing first MIR proposal..."
ADDRESS1="$(cat ${ROOT}/addresses/user1-stake.addr)"
AMOUNT1=135790
scripts/voltaire-prototype/mir-proposal.sh "$ADDRESS1" $AMOUNT1
assert_nonempty_proposals
sleep 30 # wait a bit
echo "Two MIRs: publishing second MIR proposal..."
ADDRESS2="$(cat ${ROOT}/addresses/pool-owner1-stake.addr)"
AMOUNT2=567890
scripts/voltaire-prototype/mir-proposal.sh "$ADDRESS2" "$AMOUNT2"
echo "Two MIRs: waiting to receive MIR transfer..."
wait_for_reward_balance "$ADDRESS2" "$((AMOUNT2+ADDR_AMOUNT_2))"
sleep 20 # wait a bit
# assert that the balance of the other address _has not_ changed
if [ "$(reward_balance "$ADDRESS1")" != "$EXPECTED_BALANCE_SEP" ]; then
  echo "Two MIRs: FAILURE: First MIR proposal enacted. Balance of $ADDRESS1: $(reward_balance "$ADDRESS1")"
  exit 1;
fi

########
# Test 5: verify that nodes respect the stability window for MIR proposals
EPOCH_LENGTH=1500
ACTIVE_SLOTS_COEFF=0.1
SECURITY_PARAM=10
STABILITY_WINDOW=$((3*SECURITY_PARAM/ACTIVE_SLOTS_COEFF))
if ! EPOCH_NUMBER=$(cardano-cli query tip --prototype-mode --testnet-magic 42 |jq -r .epoch); then exit 1; fi
FIRST_SLOT_NEXT_EPOCH=$(((EPOCH_NUMBER+1)*EPOCH_LENGTH))
# wait until there are at most 2*STABILITY_WINDOW slots until the next epoch
wait_for_slot $((FIRST_SLOT_NEXT_EPOCH-2*STABILITY_WINDOW))
if scripts/voltaire-prototype/mir-proposal.sh "$ADDRESS1" $AMOUNT1; then
  echo "SUCCESS: late MIR proposal rejected"
else
  echo "FAIL: late MIR proposal accepted"
  exit 1
fi

echo "Done!"
