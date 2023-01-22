#!/bin/bash
# mixaxim@mixaxim: ~$ chmod +x createDistroContractRefUTxO.sh; ./createDistroContractRefUTxO.sh

# Environment Setting
. ./env.conf
reset
clear
set -e

# Logo
printf "\n"
printf "$BLUE%b" "                                                                                             .@.      "
printf "$BLUE%b" "                                                                                            .@@.      "
printf "$BLUE%b" " .#############################.            .########.                      .@@@@@@@@@@@@@@@@@.       "
printf "$BLUE%b" "  .############################.          .########.                       .@@@.           .@@@.      "
printf "$BLUE%b" "   .#######.           .#######.        .########.                        .@@@.              .@@.     "
printf "$BLUE%b" "    .#######.          .#######.      .########.                         .@@@.                .@@.    "
printf "$BLUE%b" "     .#######.         .#######.    .########.                          .@@@.                  .@@.   "
printf "$BLUE%b" "      .#######.                   .########.        .    ..    .       .@@@.                   .@@@.  "
printf "$BLUE%b" "       .#######.                .########.           .        .         .@@@.                  .@@.   "
printf "$BLUE%b" "        .#######.             .########.             ..  ::  ..          .@@@.                 .@.    "
printf "$BLUE%b" "         .#######.          .########.          .. .. .::..::  .. ..      .@@@@.                      "
printf "$BLUE%b" "          .#######.       .########.               ....::..::....          .@@@@@.          .@@.      "
printf "$CYAN%b" "          .########.    .########.           .  :  .  :  ..  :  .  :  .     .@@@@@@@@@@@@@@@@@.       "
printf "$CYAN%b" "          .#######.       .########.               ....::..::....            .@@@@@@@@@@@@@@@@@.      "
printf "$CYAN%b" "         .#######.          .########.          .. ..  ::..::. .. ..                      .@@@@@.     "
printf "$CYAN%b" "        .#######.             .########.             ..  ::  ..                             .@@@@.    "
printf "$CYAN%b" "       .#######.                .########.           .        .                               .@@@.   "
printf "$CYAN%b" "      .#######.                   .########.        .    ..    .                               .@@@.  "
printf "$CYAN%b" "     .#######.         .#######.    .########.                          .@.                     .@@@. "
printf "$CYAN%b" "    .#######.          .#######.      .########.                         .@.                   .@@@.  "
printf "$CYAN%b" "   .#######.           .#######.        .########.                        .@@.                .@@@.   "
printf "$CYAN%b" "  .############################.          .########.                       .@@.              .@@@.    "
printf "$CYAN%b" " .#############################.            .########.                      .@@@.           .@@@.     "
printf "$CYAN%b" "                                                                             .@@@@@@@@@@@@@@@@@.      "
printf "\n"
printf "$YELLOW%b" "                     E K I V A L                                             G I M B A L A B L S    "
printf "$GREEN%b" "                  https://ekival.com                                        https://gimbalabs.com    "
printf "\n"
printf "$RED%b" "<------------------------------- Create Distro Contract Reference UTxO ------------------------------->"
printf "\n\n"

# Check Cardano Node is Running
check_process cardano-node

sleep 1

# Get Wallets UTxOs
if [[ -f "/tmp/regulator_contract_ref.json" ]]; then
    rm /tmp/regulator_contract_ref.json
fi
printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Getting Contract Reference Address UTxOs ..."
cardano-cli query utxo \
    --address "$CONTRACT_REFERENCES_ADDRESS" \
    --testnet-magic "$MAGIC_TESTNET_NUMBER" \
    --out-file /tmp/regulator_contract_ref.json
if [[ $(grep -q >/dev/null 2>&1) == $(grep '[^[:space:]]' /tmp/regulator_contract_ref.json) && -f "/tmp/regulator_contract_ref.json" ]]; then
    printf "\n$RED%b\n\n" "[-] ERROR: NO Any UTxOs Found At Contract Reference Address"
    exit
fi
TXNS=$(jq length /tmp/regulator_contract_ref.json)
if [ "$TXNS" -eq "0" ]; then
    printf "\n$RED%b\n\n" "[-] ERROR: NO Any UTxOs Found At Contract Reference Address"
    exit
fi
contractRefTxIN=$(get_address_biggest_lovelace "$CONTRACT_REFERENCES_ADDRESS")

sleep 1

# Get Minimum Amount
printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Getting Minimum Amount For Contract Reference ..."
sleep 1
cardano-cli transaction build \
    --babbage-era \
    --protocol-params-file "$WALLET_PATH"/protocol.json \
    --out-file "$TX_PATH"/distro_contract_ref.body \
    --change-address "$CONTRACT_REFERENCES_ADDRESS" \
    --tx-in "${contractRefTxIN}" \
    --tx-out="${CONTRACT_REFERENCES_ADDRESS} + 1000000" \
    --tx-out-reference-script-file "$SPENDING_CONTRACT" \
    --testnet-magic "$MAGIC_TESTNET_NUMBER" \
    2>&1 | tee tmp >/dev/null 2>&1
minAmount=$(sed -n '2p' tmp)
IFS=' ' read -ra outputs <<<"$minAmount"
IFS=' ' read -ra minAmount <<<"${outputs[4]}"
echo -e "\033[1;32m[+] Minimum ADA Needed for The Script:\033[0m" "${minAmount[0]}"
rm tmp

sleep 1

# Tx Output
contractRefOutput="$CONTRACT_REFERENCES_ADDRESS + ${minAmount[0]}"
echo -e "\033[1;32m[+] Tx Output to Contract Reference Address: \033[0m" "$contractRefOutput"

sleep 1

# Contract Address
echo -e "\033[1;32m[+] The Contract Address: \033[0m" "$SPENDING_CONTRACT_ADDRESS"

sleep 1

# Build Tx
printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Building Tx ..."
commandOutput=$(cardano-cli transaction build \
    --babbage-era \
    --protocol-params-file "$WALLET_PATH"/protocol.json \
    --out-file "$TX_PATH"/distro_contract_ref.body \
    --change-address "$CONTRACT_REFERENCES_ADDRESS" \
    --tx-in "${contractRefTxIN}" \
    --tx-out "$contractRefOutput" \
    --tx-out-reference-script-file "$SPENDING_CONTRACT" \
    --testnet-magic "$MAGIC_TESTNET_NUMBER")
IFS=':' read -ra outputs <<<"$commandOutput"
IFS=' ' read -ra fee <<<"${outputs[1]}"
echo -e "\033[1;32m[+] Tx Fee:\033[0m" "${fee[1]}"

sleep 1

# Signing Tx
printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Signing Tx ..."
cardano-cli transaction sign \
    --signing-key-file "$CONTRACT_REF_PAYMENT_SKEY" \
    --tx-body-file "$TX_PATH"/distro_contract_ref.body \
    --out-file "$TX_PATH"/distro_contract_ref.signed \
    --testnet-magic 1

sleep 1

# Submit Tx
printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Submitting Tx ..."
commandOutput=$(cardano-cli transaction submit \
    --testnet-magic "$MAGIC_TESTNET_NUMBER" \
    --tx-file "$TX_PATH"/distro_contract_ref.signed)
sleep 1
echo -e "\033[1;36m[$(date +%Y-%m-%d\ %H:%M:%S)]" "${commandOutput}"

sleep 1

# Tx ID
TXID=$(cardano-cli transaction txid --tx-file "$TX_PATH"/distro_contract_ref.signed)
echo -e "\033[1;32m[+] Transaction ID:\033[0m" "$TXID"

sleep 1

printf "\n$RED%b\n\n" "<----------------------------------------------- DONE ------------------------------------------------>"

#
# Exit
#
