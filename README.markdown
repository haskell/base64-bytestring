# Fast base64 support [![Hackage version](https://img.shields.io/hackage/v/base64-bytestring.svg?label=Hackage)](https://hackage.haskell.org/package/base64-bytestring) [![Stackage version](https://www.stackage.org/package/base64-bytestring/badge/lts?label=Stackage)](https://www.stackage.org/package/base64-bytestring) [![Build Status](https://secure.travis-ci.org/haskell/base64-bytestring.svg?branch=master)](http://travis-ci.org/haskell/base64-bytestring)

This package provides a Haskell library for working with base64-encoded
data quickly and efficiently, using the ByteString type.


# Performance

This library is written in pure Haskell, and it's fast:

* 250 MB/sec encoding

* 200 MB/sec strict decoding (per RFC 4648)

* 100 MB/sec lenient decoding


# Get involved!

Please report bugs via the
[github issue tracker](https://github.com/haskell/base64-bytestring).

Master [git repository](https://github.com/haskell/base64-bytestring):

* `git clone git://github.com/haskell/base64-bytestring.git`


# Authors

This library is written by Bryan O'Sullivan, <bos@serpentine.com>. It
is maintained by Herbert Valerio Riedel, <hvr@gnu.org> and Mikhail
Glushenkov, <mikhail.glushenkov@gmail.com>.
