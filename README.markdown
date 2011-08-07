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
[github issue tracker](https://github.com/bos/base64-bytestring).

Master [git repository](https://github.com/bos/base64-bytestring):

* `git clone git://github.com/bos/base64-bytestring.git`

And a [Mercurial mirror](https://bitbucket.org/bos/base64-bytestring):

* `hg clone https://bitbucket.org/bos/base64-bytestring`

(You can create and contribute changes using either Mercurial or git.)


# Authors

This library is written and maintained by Bryan O'Sullivan,
<bos@serpentine.com>.
