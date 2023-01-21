<p align="center" style="padding-bottom:-10px">
        <img src="./dist/resources/images/smart_contract_distro.png" width="270" height="230" alt="Distro">
</p>

<div align="center">

![version](https://img.shields.io/badge/version-1.0.0-blue.svg)
![plutus](https://img.shields.io/badge/plutus-V2-red)
![license](https://img.shields.io/badge/license-Apache-blue.svg)

</div>

---

## Table of Contents

[Description](#description)

[Installation and Compilation](#installation-and-compilation )

[Usage](#usage)

[Use Case](#use-case)

## Description

This is token distribution design pattern proof of concept named as Distro, where by interlinking `spending` contract with `minting` contract, we have achieved greater use case for smart contracts interaction inside transaction, mainly minting contract where minting contract can have access to Datum of input UTxO.

There are different approaches to make a concept called "Shared Validation" possible and shared validation means several contracts with their unique validation will create a set of validations and together will validate whole transaction. Here by using `txInfoRedeemer` and "inline datum" we've achieve shared validation.

In the case of two smart contracts, spending contract name is **Distro** and the minting contract name is **Regulator** which both will validate minting of "HAPPY_TOKEN".

The scenarios of PoC is about two developers use these two contracts to mint their share of project tokenomic tokens in two different phases. So after deciding what their share are from the total amount of project tokenomic and when they are going to mint, they will put all those information into **inline datum** and attached it to the UTxO that is going to be send to Distro contract, so when the time of minting comes, Distro(`spending`) will use input UTxO inline datum and redeemer of Regulator(`minting`) contract to validate all the minting conditions and cases.

<p align="center" style="padding:5px">
        <img src="./docs/diagrams/Design_Pattern.drawio.svg" alt="Design_Pattern">
</p>

1. The Regulator smart contract aims to handle the following:

    1.1. Minting genesis token, which will be sent to Distro contract with inline datum and specify how, when and who is able to mint "HAPPY_TOKEN".
      - Genesis token roles is to proof authenticity of the UTXO that its inline datum is going to be used as reference for minting.

    1.2. All the information that going to be inside inline datum will be contract parameter of Regulator contract as well, and inside contract, the validator will check all the fields of inline datum of output UTxO authenticity and integrity against those parameters.

    1.3. Checks whether first or second developer has signed the transaction or not.
      - Since there are different versions of same contract for educational purposes, in one version both Regulator and Distro are going to check developer address, but one at Regulator contract is enough in the scenario of this design pattern.
  
2. The Distro smart contract aims to handle the following:

    2.1. By accessing to Regulator contract redeemer, handle different cases of developer claiming tokens on each phases.

    2.3 By using information stored as inline datum of input UTxO that must holds genesis token, the contract will validate the minting process.

    2.3 Update inline datum based on the action developer has taken.

---

## Installation and Compilation  

1. Checkout `plutus-apps` repo to `v1.0.0` tag.
2. Run `cabal update` and then `cabal repl`.
3. Call `writeScript` function to compile Distro contract.
4. load `Regulator.Compiler` then call `writScript` again to compile Regulator contract.
5. Take the script hash of Regulator contract from the output and substitute Happy token currency symbol variable value inside `Regulator.ToJSON` file.
6. load `Regulator.ToJSON` and call `main` function to create Datum and Redeemer files for Regulator.
7. load `Distro.ToJSON` and call `main` function to create Datum and Redeemer for Distro Contract.

---

## Usage

In order to run the scenario:

1. Make all script inside `./utils/scripts` folder executable.

2. Create `wallets` folder inside `utils` folder and add or create`protocol.json` file inside of it.

    ```bash
    $   cd ./utils; mkdir wallets; cd ./wallets
    $   cardano-cli query protocol-parameters --testnet-magic 1 --out-file protocol.json
    ```

3. Create following wallets folders inside `wallets` folder and use the folder name as wallet name. Run `createWallet.sh` script and then fund `... .TestnetDelegation.add` address of wallets.
    2.1 `collateral` which will be used for collateral inside transactions.
    2.2 `contract_reference` which will be used for contract reference UTxO for both Distro and Regulator contracts.
    2.3 `first_dev` which will be used as First developer wallet to mint Happy token.
    2.4 `second_dev` which will be used as Second developer wallet to mint Happy token.
    2.5 `debugger` which will be used for debugging purpose.
4. Change values of variables inside both `Regulator.ToJSON` and `Distro.ToJSON` based on the wallets info, time/date of minting and the amount each developer will mint.
5. After contracts compilation, and creating Datum and redeemer check `env.conf` file inside `scripts` folder, environment variable and set them based on your system and environment configuration.
6. Run `createDistroContractRefUTxO.sh` to create Distro contract reference script UTxO.
7. Wait for awhile and then run `createRegulatorContractRefUTxO.sh` to create Regulator reference script UTxO.
8. Run `genesisMint.sh` to mint genesis token and sending it with the inline datum that is going to be used as reference for minting phases.
9. Run `firstDevClaimAtPhaseOne.sh` at the time of phase one.
10. Based on the time you have set for the scenario phase two, run `firstDevClaimAtPhaseTwo.sh`, The script can  run without running the first phase script and it will mint total phases at once.

---

## Use Case

This PoC was done in both Typed and Untyped validation for demonstrating different cost and issues of executing this design pattern, but with correct optimization and compilation, this PoC is effective and possible to implement within a Dapp.

Beside the usage described already, an NFT project can use this PoC and design pattern to allow its users mint NFT safely and secure without the need for the project minting those by themselves and then sending them to users. The minting process would be an internal transfer, therefore fee and cost for minting will be reduced.

---

## Collaboration & Acknowledgment

Gimbalabs, Ekival, James, Genty, Adrian

---
