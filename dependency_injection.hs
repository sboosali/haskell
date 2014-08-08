{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- http://mikehadlow.blogspot.com/2011/05/dependency-injection-haskell-style.html


----------------------------------
-- some services

data Report = Report Int String
 deriving (Show)

-- this could talk to a database
type GetReport = Int -> IO Report
-- this could talk to an email API
type SendReport = Report -> IO ()
-- this takes a report id and does something with it
type ProcessReport = Int -> IO ()


----------------------------------
-- some implementations

-- getReport simply creates a new report with the given id
getReport :: GetReport 
getReport id =
    return $ Report id "Hello"

-- sendReport simply prints the report
sendReport :: SendReport 
sendReport report = putStr $ show report

-- processReport uses a GetReport and a SendReport to process a report
processReport :: GetReport -> SendReport -> ProcessReport 
processReport get send id = do
    r <- get id
    send r


----------------------------------
-- dependency injection

class Service a where
    inject :: a

instance Service GetReport where
    inject = getReport

instance Service SendReport where
    inject = sendReport

instance Service ProcessReport where
    inject = processReport inject inject


-- PROBLEM: only one instance per typeclass
-- BENEFIT: dependency graph creation is statically-checked by type-system
-- PROBLEM: dependency graph creation is not reified (e.g. we can't control the order of evaluation of dependencies, or cash references)


----------------------------------
main = do
 let process = inject :: ProcessReport
 process 1
