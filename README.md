# General purpose library for encoding values to LEB128 encoded byte sequences.

This library provides a generic interface to the core (S)LEB128 encoding
algorithm.

## Provided specific interfaces:

* A specializations based on `bytestring` in "Codec.LEB128".
* A specialization over lists in "Codec.LEB128.List".
* Other implementations should be easy to derive from the interface provided
in "Codec.LEB128.Generic"

## Alternative implementations

The package `leb128-cereal` provides a way to decode using a
cereal parser using the same algorithm.
