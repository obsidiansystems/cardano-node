#!/usr/bin/env bash

set -e

ROOT=example

source scripts/voltaire-prototype/_windows_socket-path.sh
export CARDANO_NODE_SOCKET_PATH="${WINDOWS_SOCKET_PREFIX}${ROOT}/node-bft1/node.sock"

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

echo -n "Verifying that MIR proposal has been registered... "
sleep 10 # ideally we would wait until the "MIR proposal" transaction has been included in a block
if ! PROPOSALS=$(cardano-cli query ledger-state --prototype-mode --testnet-magic 42 | jq .stateBefore.esLState.utxoState.ppups.proposals); then exit 1; fi
if [ "$PROPOSALS" = "{}" ]; then echo "ERROR: MIR proposal not registered"; exit 1; else echo "OK"; fi

echo "Waiting to receive MIR transfer..."
STAKER_BALANCE="null"
echo -n "Stake balance for MIR target address:"
while [ "$STAKER_BALANCE" != "250000000" ]
do
  if ! STAKER_BALANCE=$(cardano-cli query stake-address-info --prototype-mode --address "$(cat ${ROOT}/addresses/user1-stake.addr)" --testnet-magic 42 | jq '.[0].rewardAccountBalance'); then exit 1; fi
  echo -n " $STAKER_BALANCE"
  sleep 5
done
echo ""

echo "Done!"
