{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod

-- http://www.yesodweb.com/book/basics

{- $

cabal sandbox init
time cabal install  yesod

cabal exec runhaskell  server.hs
open http://localhost:3000

-}


mkYesod "Site" [parseRoutes|
/     HomeR GET
/page PageR GET
|]
-- Route Resource Method

-- "R" means resource

-- parseRoutes :: Language.Haskell.TH.Quote.QuasiQuoter
-- parseRoutes is a QuasiQuoter

-- mkYesod :: String
--         -> [yesod-routes-1.2.0.7:Yesod.Routes.TH.Types.ResourceTree String]
--         -> Language.Haskell.TH.Syntax.Q [Language.Haskell.TH.Syntax.Dec]
-- mkYesod is a Template Haskell function

{- ghc -XTemplateHaskell -ddump-splices server.hs  ^&1

-- (the splices are elided and simplified and cleaned up)

instance ParseRoute Site where
 parseRoute ([], [])       = Just HomeR
 parseRoute (["page"], []) = Just PageR
 parseRoute _              = Nothing

instance RenderRoute Site where
 data instance Route Site  = HomeR | PageR  deriving (Show, Eq, Read)
 renderRoute HomeR = ([], [])
 renderRoute PageR = (["page"], [])

-- parseRoute and renderRoute are inverses

instance YesodDispatch Site where
 yesodDispatch env req = yesodRunner handler env mroute req
  where
   mroute = parseRoute (pathInfo req, textQueryString req)
   handler = handler' mroute req
   handler' Nothing      _                        = notFound
   handler' (Just HomeR) (requestMethod -> "GET") = getHomeR
   handler' (Just PageR) (requestMethod -> "GET") = getPageR
   handler' _            _                        = badMethod

type Handler = HandlerT Site IO

-}

data Site = Site
instance Yesod Site
-- "Yesod" means "foundation" in Hebrew
-- the instance declaration makes Site our site

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|<a href=@{PageR}>Go to the page!|]
getPageR = defaultLayout [whamlet|<a href=@{HomeR}>Go back home!|]
-- "whamlet" means "widget Hamlet"
-- whamlet type-safely embeds Haskell in HTML
-- @{PageR} becomes "/page" via renderRoute

-- defaultLayout :: Yesod site =>  WidgetT site IO () -> HandlerT site IO Html


main = warp 3000 Site
