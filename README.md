# belgian-structured-communication

[![Build Status of the package by GitHub actions](https://github.com/hapytex/belgian-structured-communication/actions/workflows/build-ci.yml/badge.svg)](https://github.com/hapytex/tuple-append/actions/workflows/build-ci.yml)
[![Hackage version badge](https://img.shields.io/hackage/v/belgian-structured-communication.svg)](https://hackage.haskell.org/package/tuple-append)

Belgium introduced a system of structured communication for bank transfers (Dutch: *gestructureerde mededeling (GM)*; French: *Communication structurée (CS)*). The format is three digits, four digits and five digits separated by slashes (`/`), and preceded and succeeded by three plusses (`+`) or asterisks (`*`). The two last digits are a checksum of all the previous digits. An example and the lowest representable structured communication is `+++000/0000/00097+++`.

# Package overview

This package provides a data type `StructuredCommunication` to parse, validate, manipulate and render structured communication. The package aims to prevent constructing 

## Checksum

The algorithm of the checksum sees the first ten digits as a whole number and determines the result of that number modulo 97. If that result is zero, the checksum is 97, otherwise it is the result of the modulo operation. So for <code>+++123/4567/890&hellip;&hellip;+++</code>, the number is `1234567890`, the result of the modulo operation is `2`, so that means that the result with checksum is `+++123/4567/89002+++`.

# Structured communication and friends

Belgian bank account numbers also have twelve digits, except that one does not write slashes and plusses or asterisks, but the same checksum algorithm is used. The digits are grouped in three groups of four digits. Therefore the lowest Belgian banking account number is `0000 0000 0097`. In order to convert this to an International Bank Account Number (IBAN), a prefix `BE` is added and two extra checksum digits. The IBAN variant of the lowest Belgian account number is thus `BE54 0000 0000 0097`.

Belgian VAT numbers use ten numbers, often defined in one group of four digits, and two groups of three digits. For transactions related this VAT number, one often adds the checksum to the VAT number to turn it into a structured communication. If the VAT number is thus `0000.000.000`, then the structured communcation to pay VAT in advance to the federal government is `+++000/0000/00097+++`. Beware that this is *not* the case for *all* banking transfers regarding the (VAT of) a company.

Some portals, like *Bolero* for example also use the client number as prefix, and then add the checksum to construct structured messages for bank transfers to the corresponding account(s).
