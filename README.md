# General purpose library for encoding values to LEB128 encoded byte sequences.

This library provides a generic interface to the core (S)LEB128 encoding
algorithm.

Specializations based on `bytestring` and `[]` are provided in
Codec.LEB128 and Codec.LEB128.List respectively.

The package `leb128-cereal` provides a way to decode using a
cereal parser using the same algorithm.

If in doubt use the signed encoding everywhere. The difference in
performance is unlikely to be relevant and errors caused by encoding
missmatch are heard to detect.