import Data.ByteString.Base64
import Data.ByteString.Char8 (pack)
import Criterion.Main

main :: IO ()
main = defaultMain
   [ bench "encode" $ whnf encode $ pack "qwerty"
   ]
