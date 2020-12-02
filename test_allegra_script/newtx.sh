#!/usr/bin/env bash

# Send ada to destination script address

# Key that can spend from the destination script address
# cardano-cli address key-hash --payment-verification-key-file test_allegra_script/destination.vkey

# Build destination script address
cardano-cli address build-script \
  --script-file test_allegra_script/testallegra.script \
  --testnet-magic 42 \
  --out-file test_allegra_script/testallegrascript.addr

#tx body
# Get tx in from exisiting UTxO
# Existing UTxO address = $(cat example/addresses/user1.addr)
# cardano-cli query utxo --address addr_test1qq6ueyytlzwqfvasc7stfpwezg4am2ysdqqht3wzkud79qs4rfjn9p2hau7q89xqckahap8yf273l9ct24t97fzsp3fsf577ec --allegra-era --testnet-magic 42

# Create tx body to send ADA to script address
cardano-cli transaction build-raw \
            --fee 0 \
            --allegra-era \
            --tx-in EXISTING-TXIN#0\
            --tx-out SCRIPTADDRESS+450000000\
            --out-file destination_script_address_tx_body

#Tx witness
cardano-cli transaction witness \
  --tx-body-file destination_script_address_tx_body \
  --signing-key-file example/addresses/user1.skey \
  --testnet-magic 42 \
  --out-file txwitness

#Assemble
cardano-cli transaction assemble \
  --tx-body-file destination_script_address_tx_body \
  --witness-file txwitness \
  --out-file send_to_script_address_tx

#submit
#cardano-cli transaction submit \
#   --cardano-mode \
#   --testnet-magic 42 \
#   --tx-file send_to_script_address_tx


# Spend from script address

# Construct tx body
# Send it back to original existing  address = $(cat example/addresses/user1.addr)

#tx-out Needs to change when restarting: INITIAL_UTXO_ADDRESS=$(cat example/addresses/user1.addr)
# setting a lower-bound or upper-bound depends on the type of script. See: https://github.com/input-output-hk/cardano-node/pull/2165
cardano-cli transaction build-raw \
    --allegra-era \
    --lower-bound 100000 \
    --fee 0 \
    --tx-in SCRIPT-ADDRESS-TXIN#0 \
    --tx-out $(cat example/addresses/user1.addr)+450000000\
    --out-file spendScriptTxBody

# Construct required witness
# Signing key witness test_allegra_script/destination.skey (this is the required signature on the script you created)

#TODO: Improve error message for transaction witness when you have built
# a txbody with the incorrect era

#Signing key witness
cardano-cli transaction witness \
  --tx-body-file spendScriptTxBody \
  --signing-key-file test_allegra_script/destination.skey \
  --testnet-magic 42 \
  --out-file key_witness

#Script witness
cardano-cli transaction witness \
  --tx-body-file spendScriptTxBody \
  --script-file test_allegra_script/testallegra.script \
  --testnet-magic 42 \
  --out-file script_witness

cardano-cli transaction assemble \
  --tx-body-file spendScriptTxBody \
  --witness-file key_witness \
  --witness-file script_witness \
  --out-file spendMultiSig

#cardano-cli transaction submit \
#   --cardano-mode \
#   --testnet-magic 42 \
#   --tx-file spendMultiSig
