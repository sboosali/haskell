{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

 {-

paginate pageNumber = do
    let resultsPerPage = 10
    selectList
        (   [PersonAge >. 25, PersonAge <=. 30]
        ||. [PersonFirstName /<-. ["Adam", "Bonny"]]
        )
        [ Desc PersonAge
        , Asc PersonLastName
        , Asc PersonFirstName
        , LimitTo resultsPerPage
        , OffsetBy $ (pageNumber - 1) * resultsPe FrPage
        ]

type PersonId = Int64

newtype Key entity = Key Int64
type PersonId = Key Person

data Key entity = KeyInt Int64 | KeyByteString ByteString


newtype Key entity = Key PersistValue

newtype KeyBackend backend entity = Key { unKey :: PersistValue }
type Key val = KeyBackend (PersistEntityBackend val) val



-}

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    johnId <- insert $ Person "John Doe" $ Just 35
    janeId <- insert $ Person "Jane Doe" Nothing

    insert $ BlogPost "My fr1st p0st" johnId
    insert $ BlogPost "One more for good measure" johnId

    oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity BlogPost])

    john <- get johnId
    liftIO $ print (john :: Maybe Person)

    delete janeId
    deleteWhere [BlogPostAuthorId ==. johnId]

