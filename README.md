bencode
=======

F# [Bencode](https://wiki.theory.org/BitTorrentSpecification#Bencoding) parser.

Parses a Bencode string and returns an object representation using discriminated unions.

Internally, the parser users Active patterns to match Bencode elements.
