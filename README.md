# belgian-structured-communication

[![Build Status of the package by GitHub actions](https://github.com/hapytex/belgian-structured-communication/actions/workflows/build-ci.yml/badge.svg)](https://github.com/hapytex/tuple-append/actions/workflows/build-ci.yml)
[![Hackage version badge](https://img.shields.io/hackage/v/belgian-structured-communication.svg)](https://hackage.haskell.org/package/tuple-append)

Belgium introduced a system of structured communication for bank transfers (Dutch: *gestructureerde mededeling (GM)*; French: *Communication structurée (CS)*). The format is three digits, four digits and five digits separated by slashes (`/`), and preceded and succeeded by three plusses (`+`) or asterisks (`*`). The two last digits are a checksum of all the previous digits. An example and the lowest representable structured communication is `+++000/0000/00097+++`.

This package provides a data type `StructuredCommunication` to parse, validate, manipulate and render structured communication.

# Structured communication and friends

Belgian bank account numbers also have twelve digits, except that one does not write slashes and plusses or asterisks, but the same checksum algorithm is used. The digits are grouped in three groups of four digits. Therefore the lowest Belgian banking account number is `0000 0000 0097`. In order to convert this to an International Bank Account Number (IBAN), a prefix `BE` is added and two extra checksum digits. The IBAN variant of the lowest Belgian account number is thus `BE54 0000 0000 0097`.

Belgian VAT numbers use ten numbers, often defined in one group of four digits, and two groups of three digits. For transactions related this VAT number, one often adds the checksum to the VAT number to turn it into a structured communication. If the VAT number is thus `0000.000.000`, then the structured communcation to pay VAT in advance to the federal government is `+++000/0000/00097+++`.
