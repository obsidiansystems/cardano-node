# Voltaire prototype demo

1. Run the below command to create the files needed to run the network

```
scripts/voltaire-prototype/mkfiles.sh
```

2. Follow the instructions printed by the above script to start the nodes.
3. Set up a stake pool by running the command below

```bash
scripts/voltaire-prototype/stake-pool_setup.sh
```

4. Fork to the Prototype One era by running the command below

```bash
scripts/voltaire-prototype/update.sh 1 shelley-era
```

5. Wait until the end of the current epoch for the fork to take effect
6. Fork to the Prototype Two era by running the command below

```bash
scripts/voltaire-prototype/update.sh 2 prototype-era-one
```

7. Wait until the end of the current epoch for the fork to take effect

8. Create and publish an MIR proposal by running the command below

```bash
scripts/voltaire-prototype/mir-proposal.sh
```

9. Verify that the nodes have included the pending proposal in their ledger state by running the commands below, and verifying that the output of the last command is not an empty object (`{}`)

```bash
source scripts/voltaire-prototype/_windows_socket-path.sh
export CARDANO_NODE_SOCKET_PATH="${WINDOWS_SOCKET_PREFIX}example/node-bft1/node.sock"
cardano-cli query ledger-state --prototype-mode --testnet-magic 42 | jq .stateBefore.esLState.utxoState.ppups.proposals
```

10. Wait until the end of the current epoch for the MIR proposal to be enacted
11. Verify that the amount specified in the `mir-proposal.sh` script (250,000,000 ADA) is transferred to the given reward address by running the command below, and verifying that it outputs `250000000`

```bash
cardano-cli query stake-address-info --prototype-mode --address "$(cat ${ROOT}/addresses/user1-stake.addr)" --testnet-magic 42 | jq '.[0].rewardAccountBalance'
```



# Running the automated demo script

Instead of running the above commands manually, you can you a demo script that does all of the above in an automated fashion. At the repository root, run the below command.

```
scripts/voltaire-prototype/mkfiles.sh test && scripts/voltaire-prototype/demo.sh
```

The above command will:

1. Create necessary files (e.g. keys, config files)
2. Start nodes
3. Set up a stake pool
4. Fork to v1.0 (Prototype One)
5. Fork to v2.0 (Prototype Two)
6. Publish MIR proposal
7. Verify that MIR has been registered by node
8. Verify that MIR transfer is enacted

