import Data.Char
import Data.List

database :: [(String,Int,Char,String)]
-- (Name, Age, Gender (M/F), Place of residence)
database = [("Rien",23,'M',"Enschede"),("Test",11,'F',"Groningen"),("Rien",23,'M',"Enschede")]

name :: (String,Int,Char,String) -> String
name (a,b,c,d) = a

age :: (String,Int,Char,String) -> Int
age (a,b,c,d) = b

gender :: (String,Int,Char,String) -> Char
gender (a,b,c,d) = c

place_of_residence :: (String,Int,Char,String) -> String
place_of_residence (a,b,c,d) = d

incAgeRecur :: [(String,Int,Char,String)] -> Int -> [(String,Int,Char,String)]
incAgeRecur (p:persons) inc | length persons == 0 = [(name p,age p+inc, gender p, place_of_residence p)]
                            | otherwise = incAgeRecur [p] inc ++ (incAgeRecur persons inc)

incAgeSingle :: Int -> (String,Int,Char,String) -> (String,Int,Char,String)
incAgeSingle inc (a,b,c,d) = (a,b+inc,c,d)

incAgeHigh :: [(String,Int,Char,String)] -> Int -> [(String,Int,Char,String)]
incAgeHigh persons inc = map (incAgeSingle inc) persons

incAgeComp :: [(String,Int,Char,String)] -> Int -> [(String,Int,Char,String)]
incAgeComp persons inc = [(a,b+inc,c,d) | (a,b,c,d) <- persons]

namesRecur :: [(String,Int,Char,String)] -> [String]
namesRecur (p:persons)      | null persons && age p >= 30 && age p <= 40 && gender p == 'F' = [name p]
                            | null persons = []
                            | age p >= 30 && age p <= 40 && gender p == 'F' = name p : namesRecur persons
                            | otherwise = namesRecur persons

namesSingle :: (String,Int,Char,String) -> Bool
namesSingle p               | age p >= 30 && age p <= 40 && gender p == 'F' = True
                            | otherwise = False

namesHigh :: [(String,Int,Char,String)] -> [String]
namesHigh persons = map name (filter namesSingle persons)

namesComp :: [(String,Int,Char,String)] -> [String]
namesComp persons = [name person | person <- persons , namesSingle person]

getAgebyName :: String -> [(String,Int,Char,String)] -> Int
getAgebyName n persons = head [age person | person <- persons, map toLower (name person) == map toLower n]

switchOneTwo :: (a,b,c,d) -> (b,a,c,d)
switchOneTwo (a,b,c,d) = (b,a,c,d)

sortAge :: [(String,Int,Char,String)] -> [(String,Int,Char,String)]
sortAge persons = map switchOneTwo (sort (map switchOneTwo persons))
-- use reverse if high to low sort
