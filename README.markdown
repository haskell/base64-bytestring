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
[bitbucket issue tracker](http://bitbucket.org/bos/base64-bytestring).

Master [Mercurial repository](http://bitbucket.org/bos/base64-bytestring):

* `hg clone http://bitbucket.org/bos/base64-bytestring`

There's also a [git mirror](http://github.com/bos/base64-bytestring):

* `git clone git://github.com/bos/base64-bytestring.git`

(You can create and contribute changes using either Mercurial or git.)


# Authors

This library is written and maintained by Bryan O'Sullivan,
<bos@serpentine.com>.
