import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
-- $ cabal install http-conduit
-- $ runhaskell http 


main = simpleHttp "http://www.haskell.org/" >>= L.putStr
