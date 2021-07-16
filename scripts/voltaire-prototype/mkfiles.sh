#!/usr/bin/env bash

set -e
#set -x

ROOT=example
SUPPLY=1500000000
INITIAL_FUNDS=1000000000

BFT_NODES="node-bft1 node-bft2"
BFT_NODES_N="1 2"
NUM_BFT_NODES=2

POOL_NODES="node-pool1"

ALL_NODES="${BFT_NODES} ${POOL_NODES}"

if [ "$1" = "test" ]; then TEST=true; fi

if ! mkdir "${ROOT}"; then
  echo "The ${ROOT} directory already exists, please move or remove it"
  exit
fi

# copy and tweak the configuration
cp configuration/defaults/byron-mainnet/configuration.yaml ${ROOT}/

sed -i ${ROOT}/configuration.yaml \
    -e 's/Protocol: RealPBFT/Protocol: Voltaire/' \
    -e 's/minSeverity: Info/minSeverity: Warning/'\
    -e 's/LastKnownBlockVersion-Minor: 2/LastKnownBlockVersion-Minor: 0/'

# Set up our template
cardano-cli genesis create --testnet-magic 42 --genesis-dir ${ROOT}

# Then edit the genesis.spec.json ...

# We're going to use really quick epochs (300 seconds), by using short slots 0.2s
# and K=10, but we'll keep long KES periods so we don't have to bother
# cycling KES keys
sed -i ${ROOT}/genesis.spec.json \
    -e 's/"slotLength": 1/"slotLength": 0.2/' \
    -e 's/"activeSlotsCoeff": 5.0e-2/"activeSlotsCoeff": 0.1/' \
    -e 's/"securityParam": 2160/"securityParam": 10/' \
    -e 's/"epochLength": 432000/"epochLength": 1500/' \
    -e 's/"decentralisationParam": 1/"decentralisationParam": 0.7/' \
    -e 's/"updateQuorum": 5/"updateQuorum": 2/'

# Now generate for real:

cardano-cli genesis create \
    --testnet-magic 42 \
    --genesis-dir ${ROOT}/ \
    --gen-genesis-keys ${NUM_BFT_NODES} \
    --gen-utxo-keys 1 \
    --supply "$INITIAL_FUNDS"

# Increase the total supply, so that there's some funds left in the reserves
sed -i ${ROOT}/genesis.json \
    -e "s/\"maxLovelaceSupply\": $INITIAL_FUNDS/\"maxLovelaceSupply\": $SUPPLY/"

pushd ${ROOT}

echo "====================================================================="
echo "Generated genesis keys and genesis files:"
echo
ls -1 *
echo "====================================================================="

echo "Generated genesis.json:"
echo
cat genesis.json
echo
echo "====================================================================="

mkdir ${ALL_NODES}

# Make the pool operator cold keys
# This was done already for the BFT nodes as part of the genesis creation

for NODE in ${POOL_NODES}; do

  cardano-cli node key-gen \
      --cold-verification-key-file                 ${NODE}/operator.vkey \
      --cold-signing-key-file                      ${NODE}/operator.skey \
      --operational-certificate-issue-counter-file ${NODE}/operator.counter

  cardano-cli node key-gen-VRF \
      --verification-key-file ${NODE}/vrf.vkey \
      --signing-key-file      ${NODE}/vrf.skey

  # Set permissions for the vrf private key file: read for owner only
  chmod gou-rwx "${NODE}/vrf.skey"
  chmod u+r "${NODE}/vrf.skey"

done

# Symlink the BFT operator keys from the genesis delegates, for uniformity

for N in ${BFT_NODES_N}; do

  ln -s ../delegate-keys/delegate${N}.skey node-bft${N}/operator.skey
  ln -s ../delegate-keys/delegate${N}.vkey node-bft${N}/operator.vkey
  ln -s ../delegate-keys/delegate${N}.counter node-bft${N}/operator.counter
  ln -s ../delegate-keys/delegate${N}.vrf.vkey node-bft${N}/vrf.vkey
  ln -s ../delegate-keys/delegate${N}.vrf.skey node-bft${N}/vrf.skey

done


# Make hot keys and for all nodes

for NODE in ${ALL_NODES}; do

  cardano-cli node key-gen-KES \
      --verification-key-file ${NODE}/kes.vkey \
      --signing-key-file      ${NODE}/kes.skey

  cardano-cli node issue-op-cert \
      --kes-period 0 \
      --kes-verification-key-file                  ${NODE}/kes.vkey \
      --cold-signing-key-file                      ${NODE}/operator.skey \
      --operational-certificate-issue-counter-file ${NODE}/operator.counter \
      --out-file                                   ${NODE}/node.cert

done

# Make topology files
#TODO generalise this over the N BFT nodes and pool nodes
(cat <<TOPOLOGY_FILE
{
   "Producers": [
     {
       "addr": "127.0.0.1",
       "port": 3002,
       "valency": 1
     }
   , {
       "addr": "127.0.0.1",
       "port": 3003,
       "valency": 1
     }
   ]
 }
TOPOLOGY_FILE
) > node-bft1/topology.json
echo 3001 > node-bft1/port

(cat <<TOPOLOGY_FILE
{
   "Producers": [
     {
       "addr": "127.0.0.1",
       "port": 3001,
       "valency": 1
     }
   , {
       "addr": "127.0.0.1",
       "port": 3003,
       "valency": 1
     }
   ]
 }
TOPOLOGY_FILE
) > node-bft2/topology.json
echo 3002 > node-bft2/port

(cat <<TOPOLOGY_FILE
{
   "Producers": [
     {
       "addr": "127.0.0.1",
       "port": 3001,
       "valency": 1
     }
   , {
       "addr": "127.0.0.1",
       "port": 3002,
       "valency": 1
     }
   ]
 }
TOPOLOGY_FILE
) > node-pool1/topology.json
echo 3003 > node-pool1/port


echo "Generated node operator keys (cold, hot) and operational certs:"
echo
ls -1 ${ALL_NODES}
echo "====================================================================="


# Make some payment and stake addresses
# user1..n:       will own all the funds in the system, we'll set this up from
#                 initial utxo the
# pool-owner1..n: will be the owner of the pools and we'll use their reward
#                 account for pool rewards

USER_ADDRS="user1"
POOL_ADDRS="pool-owner1"

ADDRS="${USER_ADDRS} ${POOL_ADDRS}"

mkdir addresses

for ADDR in ${ADDRS}; do

  # Payment address keys
  cardano-cli address key-gen \
      --verification-key-file addresses/${ADDR}.vkey \
      --signing-key-file      addresses/${ADDR}.skey

  # Stake address keys
  cardano-cli stake-address key-gen \
      --verification-key-file addresses/${ADDR}-stake.vkey \
      --signing-key-file      addresses/${ADDR}-stake.skey

  # Payment addresses
  cardano-cli address build \
      --payment-verification-key-file addresses/${ADDR}.vkey \
      --stake-verification-key-file addresses/${ADDR}-stake.vkey \
      --testnet-magic 42 \
      --out-file addresses/${ADDR}.addr

  # Stake addresses
  cardano-cli stake-address build \
      --stake-verification-key-file addresses/${ADDR}-stake.vkey \
      --testnet-magic 42 \
      --out-file addresses/${ADDR}-stake.addr

  # Stake addresses registration certs
  cardano-cli stake-address registration-certificate \
      --stake-verification-key-file addresses/${ADDR}-stake.vkey \
      --out-file addresses/${ADDR}-stake.reg.cert

done

# user N will delegate to pool N
USER_POOL_N="1"

for N in ${USER_POOL_N}; do

  # Stake address delegation certs
  cardano-cli stake-address delegation-certificate \
      --stake-verification-key-file addresses/user${N}-stake.vkey \
      --cold-verification-key-file  node-pool${N}/operator.vkey \
      --out-file addresses/user${N}-stake.deleg.cert

  ln -s ../addresses/pool-owner${N}-stake.vkey node-pool${N}/owner.vkey
  ln -s ../addresses/pool-owner${N}-stake.skey node-pool${N}/owner.skey

done

echo "Generated payment address keys, stake address keys,"
echo "stake address regitration certs, and stake address delegatation certs"
echo
ls -1 addresses/
echo "====================================================================="


# Next is to make the stake pool registration cert

for NODE in ${POOL_NODES}; do

  cardano-cli stake-pool registration-certificate \
    --testnet-magic 42 \
    --pool-pledge 0 --pool-cost 0 --pool-margin 0 \
    --cold-verification-key-file             ${NODE}/operator.vkey \
    --vrf-verification-key-file              ${NODE}/vrf.vkey \
    --reward-account-verification-key-file   ${NODE}/owner.vkey \
    --pool-owner-stake-verification-key-file ${NODE}/owner.vkey \
    --out-file                               ${NODE}/registration.cert
done

echo "Generated stake pool registration certs:"
ls -1 node-*/registration.cert
echo "====================================================================="

if [ ! $TEST ]; then
  echo "To start the nodes, in separate terminals use:"
  echo
fi

for NODE in ${ALL_NODES}; do

COMMAND=$(cat <<-END
cardano-node run \\
	--config                          ${ROOT}/configuration.yaml \\
	--topology                        ${ROOT}/${NODE}/topology.json \\
	--database-path                   ${ROOT}/${NODE}/db \\
	--socket-path                     ${ROOT}/${NODE}/node.sock \\
	--shelley-kes-key                 ${ROOT}/${NODE}/kes.skey \\
	--shelley-vrf-key                 ${ROOT}/${NODE}/vrf.skey \\
	--shelley-operational-certificate ${ROOT}/${NODE}/node.cert \\
	--port                            $(cat "${NODE}"/port)
END
)

if [ ! "$TEST" ]; then
  echo "$COMMAND"
else
  eval "(cd .. ; $COMMAND) &"
fi

done
echo "In order to do the protocol updates, proceed as follows:"
echo
echo "  0. wait for the nodes to start producing blocks"
echo "  1. invoke ./scripts/voltaire-prototype/update.sh <N>"
echo "     Here, <N> the current epoch (2 if you're quick)."
echo "     If you provide the wrong epoch, you will see an error"
echo "     that will tell you the current epoch, and can run"
echo "     the script again."
echo "  2. restart the nodes"
echo "     wait for the next epoch for the update to take effect"

echo "The update script also delegates to a mining pool"
echo "You can query the stake distribution, and see if the pool node creates blocks"
echo
echo "CARDANO_NODE_SOCKET_PATH=example/node-bft1/node.sock \\"
echo "  cardano-cli query stake-distribution --prototype-mode --testnet-magic 42"
echo

popd
