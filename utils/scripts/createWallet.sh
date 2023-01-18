#!/bin/bash

# For understanding the commands and output of commands please visit this link
# https://github.com/input-output-hk/cardano-addresses

# Make script executable and run it
# mixaxim@mixaxim:~$ chmod +x createWallet.sh;./createWallet.sh

# Environment Setting
RED='\e[1;31m%s\e[0m'
GREEN='\e[1;32m%s\e[0m\n'
YELLOW='\e[1;33m%s\e[0m\n'
BLUE='\e[1;34m%s\e[0m\n'
CYAN='\e[1;36m%s\e[0m\n'

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
printf "$RED%b" "<--------------- Create Cardano Wallet Compatible With Browser Wallet And Cardano_cli --------------->"

printf "\n"

echo ""
printf "$YELLOW%b" "[+] Please Enter The Name of The Wallet: "
read -r walletName

printf "$YELLOW%b" "[+] Please Enter The Number of Menomic Words for Recovery Phrase. Expected one of: 9, 12, 15, 18, 21 or 24"
read -r no

cardano-address recovery-phrase generate --size "$no" |
    tee "$(pwd)"/"$walletName".PrivateRecoveryPhrase.txt |
    cardano-address key from-recovery-phrase Shelley \
        >"$(pwd)"/"$walletName".Root.xsk

recoveryPhrases=$(cat "$(pwd)"/"$walletName".PrivateRecoveryPhrase.txt)
echo -e "\033[1;32m[+] Recovery Phrases:\033[0m" "${recoveryPhrases}"

sleep 1

rootXSK=$(cat "$(pwd)"/"$walletName".Root.xsk)
echo -e "\033[1;32m[+] Root(Parent) Extended Sigining Key:\033[0m" "${rootXSK}"

sleep 1

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Payment Address Section Start ..."

sleep 1

# Comment these 2 lines out for creating different accounts
# for PAYMENT addresses and comment `accountIndex=0` line.
# printf "$YELLOW%b" "[+] Please Enter The Account Index: "
# read -r accountIndex
accountIndex=0

# Comment these 2 lines out for creating different roles and comment `roleIndex=0` line.
# printf "$YELLOW%b" "[+] Please Enter The Role Index ['0' External Chain (Payment), '1' Internal Chain, '2' Staking]: "
# read -r roleIndex
roleIndex=0

printf "$YELLOW%b" "[+] Please Enter The Payment Address Index: "
read -r paymentAddressIndex

cardano-address key child 1852H/1815H/"$accountIndex"H/"$roleIndex"/"$paymentAddressIndex" \
    <"$(pwd)"/"$walletName".Root.xsk \
    >"$(pwd)"/"$walletName"."$paymentAddressIndex".PrivatePaymentAddress.xsk

privatePaymentAddressXVK=$(cat "$(pwd)"/"$walletName"."$paymentAddressIndex".PrivatePaymentAddress.xsk)
echo -e "\033[1;32m[+] Private(Child) Payment Address Extended Signing Key:\033[0m" "${privatePaymentAddressXVK}"

sleep 1

cardano-address key public --with-chain-code \
    <"$(pwd)"/"$walletName"."$paymentAddressIndex".PrivatePaymentAddress.xsk \
    >"$(pwd)"/"$walletName"."$paymentAddressIndex".PublicPaymentAddress.xvk

publicPaymentAddressXVK=$(cat "$(pwd)"/"$walletName"."$paymentAddressIndex".PublicPaymentAddress.xvk)
echo -e "\033[1;32m[+] Public Payment Address Extended Signing Key:\033[0m" "${publicPaymentAddressXVK}"

sleep 1

# Comment these 2 lines out for creating PAYMENT address for different network
# default is `Pre Production` testnet. For different network comment `networkTag=preprod` line.
# printf "$YELLOW%b" "[+] Please Enter The Network Tag [mainnet, testnet, preview, preprod]: "
# read -r networkTag
networkTag=preprod

cardano-address address payment --network-tag "$networkTag" \
    <"$(pwd)"/"$walletName"."$paymentAddressIndex".PublicPaymentAddress.xvk \
    >"$(pwd)"/"$walletName"."$paymentAddressIndex".TestnetPayment.addr

testnetPaymentAddress=$(cat "$(pwd)"/"$walletName"."$paymentAddressIndex".TestnetPayment.addr)
echo -e "\033[1;32m[+] Testnet Payment Address:\033[0m" "${testnetPaymentAddress}"

sleep 1

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating Sigingin Key, Verification Key and Public Key Hash ..."

cardano-cli key convert-cardano-address-key --shelley-payment-key \
    --signing-key-file "$(pwd)"/"$walletName"."$paymentAddressIndex".PrivatePaymentAddress.xsk \
    --out-file "$(pwd)"/"$walletName"."$paymentAddressIndex".Payment.xsk

cardano-cli key verification-key \
    --signing-key-file "$(pwd)"/"$walletName"."$paymentAddressIndex".Payment.xsk \
    --verification-key-file "$(pwd)"/"$walletName"."$paymentAddressIndex".Payment.xvk

cardano-cli address key-hash \
    --payment-verification-key-file "$(pwd)"/"$walletName"."$paymentAddressIndex".Payment.xvk \
    --out-file "$(pwd)"/"$walletName"."$paymentAddressIndex".Payment.pkh

sleep 1

echo -e "\033[1;32m[+] Payment Sigingin Key:\033[0m" "$(cat "$(pwd)"/"$walletName"."$paymentAddressIndex".Payment.xsk)"

sleep 1

echo -e "\033[1;32m[+] Payment Verification Key:\033[0m" "$(cat "$(pwd)"/"$walletName"."$paymentAddressIndex".Payment.xvk)"

sleep 1

echo -e "\033[1;32m[+] Payment Public Key Hash:\033[0m" "$(cat "$(pwd)"/"$walletName"."$paymentAddressIndex".Payment.pkh)"

sleep 1

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Staking Address Section ..."

# Comment these 2 lines out for creating different accounts
# for STAKE addresses and comment `accountIndex=0` line.
# printf "$YELLOW%b" "[+] Please Enter The Account Index: "
# read -r accountIndex
accountIndex=0

printf "$YELLOW%b" "[+] Please Enter The Stake Address Index: "
read -r stakeAddressIndex

cardano-address key child 1852H/1815H/"$accountIndex"H/2/"$stakeAddressIndex" \
    <"$(pwd)"/"$walletName".Root.xsk \
    >"$(pwd)"/"$walletName"."$stakeAddressIndex".PrivateStakeAddress.xsk

privateStakeAddressXVK=$(cat "$(pwd)"/"$walletName"."$stakeAddressIndex".PrivateStakeAddress.xsk)
echo -e "\033[1;32m[+] Private(Child) Stake Address Extended Signing Key:\033[0m" "${privateStakeAddressXVK}"

sleep 1

cardano-address key public --with-chain-code \
    <"$(pwd)"/"$walletName"."$stakeAddressIndex".PrivateStakeAddress.xsk \
    >"$(pwd)"/"$walletName"."$stakeAddressIndex".PublicStakeAddress.xvk

publicStakeAddressXVK=$(cat "$(pwd)"/"$walletName"."$stakeAddressIndex".PublicStakeAddress.xvk)
echo -e "\033[1;32m[+] Public Stake Address Extended Signing Key:\033[0m" "${publicStakeAddressXVK}"

sleep 1

# Comment these 2 lines out for creating STAKE address for different network
# default is `Pre Production` testnet. For different network comment `networkTag=preprod` line.
# printf "$YELLOW%b" "[+] Please Enter The Network Tag [mainnet, testnet, preview, preprod]: "
# read -r networkTag
networkTag=preprod

cardano-address address stake --network-tag "$networkTag" \
    <"$(pwd)"/"$walletName"."$stakeAddressIndex".PublicStakeAddress.xvk \
    >"$(pwd)"/"$walletName"."$stakeAddressIndex".TestnetStake.addr

testnetStakeAddress=$(cat "$walletName"."$stakeAddressIndex".TestnetStake.addr)
echo -e "\033[1;32m[+] Testnet Stake Address:\033[0m" "${testnetStakeAddress}"

sleep 1

printf "$CYAN%b" "[$(date +%Y-%m-%d\ %H:%M:%S)] Creating Stake Signing Key, Stake Verification Key and Stake Public Key Hash ..."

cardano-cli key convert-cardano-address-key --shelley-stake-key \
    --signing-key-file "$(pwd)"/"$walletName"."$stakeAddressIndex".PrivateStakeAddress.xsk \
    --out-file "$(pwd)"/"$walletName"."$stakeAddressIndex".Stake.xsk

cardano-cli key verification-key \
    --signing-key-file "$(pwd)"/"$walletName"."$stakeAddressIndex".Stake.xsk \
    --verification-key-file "$(pwd)"/"$walletName"."$stakeAddressIndex".Stake.xvk

cardano-address key hash --hex \
    <"$(pwd)"/"$walletName"."$stakeAddressIndex".PublicStakeAddress.xvk \
    >"$(pwd)"/"$walletName"."$stakeAddressIndex".Stake.pkh

sleep 1

echo -e "\033[1;32m[+] Stake Sigingin Key:\033[0m" "$(cat "$(pwd)"/"$walletName"."$stakeAddressIndex".Stake.xsk)"

sleep 1

echo -e "\033[1;32m[+] Stake Verification Key:\033[0m" "$(cat "$(pwd)"/"$walletName"."$stakeAddressIndex".Stake.xvk)"

sleep 1

echo -e "\033[1;32m[+] Stake Public Key Hash:\033[0m" "$(cat "$(pwd)"/"$walletName"."$stakeAddressIndex".Stake.pkh)"

sleep 1

cardano-address address delegation "$(cat "$(pwd)"/"$walletName"."$stakeAddressIndex".PublicStakeAddress.xvk)" \
    <"$(pwd)"/"$walletName"."$paymentAddressIndex".TestnetPayment.addr \
    >"$(pwd)"/"$walletName"."$paymentAddressIndex"."$stakeAddressIndex".TestnetDelegation.addr

testnetDelegationAddress=$(cat "$(pwd)"/"$walletName"."$paymentAddressIndex"."$stakeAddressIndex".TestnetDelegation.addr)
echo -e "\033[1;32m[+] Testnet Delegation Address:\033[0m" "${testnetDelegationAddress}"

sleep 1

printf "\n$RED%b\n\n" "<----------------------------------------------- DONE ------------------------------------------------>"

#
# Exit
#
