# Fast base64 support

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
