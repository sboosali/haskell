#!/usr/bin/env runhaskell
import HSH



main = do
 runIO $ "ls" -|- "wc"

{-

class Monad m =>  MonadError e m  | m -> e  where
 throwError :: e -> m a
 catchError :: m a -> (e -> m a) -> m a

instance MonadError IOException IO where
    throwError = ioError
    catchError = catch

instance (Error e) => MonadError e (Either e) where
    throwError             = Left
    Left  l `catchError` h = h l
    Right r `catchError` _ = Right r


-}

{- sysctls API

get :: String -> IO Integer
set :: String -> Integer -> Priv ()
modify :: String-> (Integer -> Integer)-> IO (Integer, Integer)
-}


{-

toggle v
| v == 100 = 0
| otherwise= 100

main= do
 (old, new) <- modify "hw.setperf" toggle
 clock <- get "hw.cpuspeed"
 printf "cpu: %d -> %d\n" old new
 printf "clock: %f Ghz\n" (clock / 1000)


newtype Shell a = Shell { runShell :: ErrorT String IO a }

instance MonadError String Shell where
 throwError=error . ("Shell failed: " ++)

get s = do
 v <-run $"sysctl" <+> s
 readM (parse v)
where
 parse = tail . dropWhile (/= '=') . init

readM s
 | [x] <-parse= return x
 | otherwise= throwError (show s)
where
 parse = [ x | (x,t) <-reads s ]

set s v = run $
 printf "sysctl -w %s=%s" s (show v)

modify s f = do
 v <-get s
 let u = f v
 v <-set s u
 return(v,u)


instance MonadError String Priv where
 throwError=error . ("Priv failed: " ++)

runPriv :: String -> Priv String
runPriv s = Priv $run ("/usr/bin/sudo" <+> s)

set :: String -> Integer -> Priv String
set s v =runPriv$printf "sysctl -w %s=%s\n" s (show v)

priv (set s u)

-}
