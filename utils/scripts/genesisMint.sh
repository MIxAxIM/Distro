#!/bin/bash
# mixaxim@mixaxim:~$ chmod +x genesiMint.sh;./genesiMint.sh

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
printf "$RED%b" "<------------------------------------------ Genesis Mint --------------------------------------------->"
printf "\n\n"

# Check Cardano Node is Running
check_process cardano-node

sleep 1

## Collateral Wallet
if [[ -f "/tmp/collat_utxo.json" ]]; then
    rm /tmp/collat_utxo.json
fi
printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Getting Collatral Wallet UTxOs ..."
cardano-cli query utxo \
    --address "$COLLATERAL_ADDRESS" \
    --testnet-magic "$MAGIC_TESTNET_NUMBER" \
    --out-file /tmp/collat_utxo.json
if [[ $(grep -q >/dev/null 2>&1) == $(grep '[^[:space:]]' /tmp/collat_utxo.json) && -f "/tmp/collat_utxo.json" ]]; then
    printf "\n$RED%b\n\n" "[-] ERROR: No Any UTxO Found At Collateral Address"
    exit 1
fi
txINLength=$(jq length /tmp/collat_utxo.json)
if [ "$txINLength" -eq "0" ]; then
    printf "\n$RED%b\n\n" "[-] ERROR: No Any UTxO Found At Collateral Address"
    exit 1
fi
txIn=$(jq -r --arg allTxIn "" 'keys[] | . + $allTxIn + " --tx-in"' /tmp/collat_utxo.json)
collatTxIn=${txIn::-8}

sleep 1

# Get Contract Reference UTxO
printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Getting Regulator Contract Reference UTxO ..."
if [[ $(grep -q >/dev/null 2>&1) == $(grep '[^[:space:]]' "$TX_PATH"/regulator_contract_ref.signed) && -f "$TX_PATH/regulator_contract_ref.signed" ]]; then
    printf "\n$RED%b\n\n" "[-] ERROR: There Is No Contract Reference UTxO"
    exit 1
fi
mintingContractRefUTxO=$(cardano-cli transaction txid --tx-file "$TX_PATH"/regulator_contract_ref.signed)

sleep 1

# Get First Developer Wallet
printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Getting First Developer Address UTxOs ..."
sleep 1
echo ""
cardano-cli query utxo \
    --address "$FIRST_DEVELOPER_ADDRESS" \
    --testnet-magic "$MAGIC_TESTNET_NUMBER"
firstDevTxIn=$(get_address_biggest_lovelace "$FIRST_DEVELOPER_ADDRESS")

sleep 1

# Tx Output
echo ""
distroContractOutput="$SPENDING_CONTRACT_ADDRESS + 2000000 + 1 $HAPPY_TOKEN"
echo -e "\033[1;32m[+] Tx Output to Distro Contract: \033[0m" "$distroContractOutput"

sleep 1

# Build Tx
printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Building Tx ..."
commandOutput=$(
    cardano-cli transaction build \
        --babbage-era \
        --protocol-params-file "$WALLET_PATH"/protocol.json \
        --testnet-magic "$MAGIC_TESTNET_NUMBER" \
        --out-file "$TX_PATH"/genesis_mint_tx.body \
        --change-address "$FIRST_DEVELOPER_ADDRESS" \
        --tx-in-collateral="$collatTxIn" \
        --tx-in="$firstDevTxIn" \
        --tx-out="$distroContractOutput" \
        --tx-out-inline-datum-file "$GENESIS_MINT_INLINE_DATUM" \
        --mint="1 $HAPPY_TOKEN" \
        --mint-tx-in-reference "$mintingContractRefUTxO#1" \
        --mint-plutus-script-v2 \
        --mint-reference-tx-in-redeemer-file "$GENESIS_MINT_ACTION_REDEEMER" \
        --policy-id="${HAPPY_TOKEN_POLICY_ID}"
    # --mint-script-file="$MINTING_CONTRACT" \
    # --mint-redeemer-file="$GENESIS_MINT_ACTION_REDEEMER"
)
IFS=':' read -ra output <<<"$commandOutput"
IFS=' ' read -ra fee <<<"${output[1]}"
echo -e "\033[1;32m[+] Tx Fee:\033[0m" "${fee[1]}"

sleep 1

# Signing Tx
printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Signing Tx ..."
cardano-cli transaction sign \
    --signing-key-file "$FIRST_DEVELOPER_PAYMENT_SKEY" \
    --signing-key-file "$COLLATERAL_PAYMENT_SKEY" \
    --tx-body-file "$TX_PATH"/genesis_mint_tx.body \
    --out-file "$TX_PATH"/genesis_mint_tx.signed \
    --testnet-magic "$MAGIC_TESTNET_NUMBER"

sleep 1

# Submit Tx
printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Submitting Tx ..."
commandOutput=$(cardano-cli transaction submit \
    --testnet-magic "$MAGIC_TESTNET_NUMBER" \
    --tx-file "$TX_PATH"/genesis_mint_tx.signed)
sleep 1
echo -e "\033[1;36m[$(date +%Y-%m-%d\ %H:%M:%S)]" "${commandOutput}"

sleep 1

# Tx ID
TXID=$(cardano-cli transaction txid --tx-file "$TX_PATH"/genesis_mint_tx.signed)
echo -e "\033[1;32m[+] Transaction ID:\033[0m" "$TXID"

sleep 1

printf "\n$RED%b\n\n" "<----------------------------------------------- DONE ------------------------------------------------>"

#
# Exit
#
