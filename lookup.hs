import Prelude hiding (lookup)
import Data.Map

employeeDept = fromList([("John","Sales"), ("Bob","IT")])
deptCountry = fromList([("IT","USA"), ("Sales","France")])
countryCurrency = fromList([("USA", "Dollar"), ("France", "Euro")])

employeeCurrency :: String -> Maybe String
employeeCurrency name =
    let
        dept = lookup name employeeDept
        country = lookup dept deptCountry
    in lookup country countryCurrency

main = do
    putStrLn $ "John's currency: " ++ (show (employeeCurrency "John"))
    putStrLn $ "Pete's currency: " ++ (show (employeeCurrency "Pete"))
