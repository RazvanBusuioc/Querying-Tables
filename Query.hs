module Query where

import UserInfo
import Rating
import Movie
import Data.List.Split
import Data.List
import Data.Ord


type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char


-- TODO 1
charToString :: Char -> String
charToString c = [c]

--separates the [string] representing the entry lines into list of list of String aka [Entry]
separate_multiple_lines :: ColSeparator -> [String] -> [Entry]
separate_multiple_lines c [""] = []
separate_multiple_lines c list = (splitOn (charToString c) (head list)):(separate_multiple_lines c (tail list))

read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table c l text = Table (splitOn (charToString c) (head (splitOn (charToString l) text))) 
                            (separate_multiple_lines c (tail (splitOn (charToString l) text)))


-- TODO 2
--Calculates the length of the longest String in a list of Strings
max_len_string :: [String] -> Integer
max_len_string [] = 0
max_len_string (x:[]) = toInteger (length x)
max_len_string (x:y:xs) = if length x > length y then max_len_string (x:xs) else max_len_string (y:xs)

--Calculates the maximum length of a String in a column
calculate_maximum_len :: [Entry] -> [Integer]
calculate_maximum_len entries |(length (head entries)) == 0 = []
                              |otherwise = (max_len_string (map head entries)):(calculate_maximum_len (map tail entries))

--Retursn a list containing the maximum length for each column
calculate_maximum_len_in_table :: Table -> [Integer]
calculate_maximum_len_in_table (Table ts entries) = calculate_maximum_len (ts:entries)

--Padding a string with n whitespaces
string_padding :: String -> Integer -> String
string_padding s n | n > 0 = string_padding (s ++ " ") (n - 1)
                   | otherwise = s

--Padding every String from an Entry with the necessary number of whitespaces
list_padding :: Entry -> [Integer] -> Entry
list_padding [] [] = []
list_padding entry list = (string_padding (head entry) ((head list) - toInteger (length (head entry)))) : 
                                                                  (list_padding (tail entry) (tail list))

--Padding all the Entries
entry_padding :: [Entry] -> [Integer] -> [Entry]
entry_padding [] list = []
entry_padding entries list = (list_padding (head entries) list) : (entry_padding (tail entries) list)

--Padding the hole table 
table_padding :: Table -> Table
table_padding (Table ts entries) = Table (list_padding ts x) (entry_padding entries x)
                                   where x = calculate_maximum_len_in_table (Table ts entries)

--The function returns the first and last string(-------------) in order to print a table
print_antet :: [Integer] -> String
print_antet [] = "-\n"
print_antet (x:xs) | x >= 0 = "-" ++ print_antet ((x-1):xs)
                   | otherwise = print_antet xs

--Prints a line/entry from the table
print_entry :: Entry -> String
print_entry [] = "|\n"
print_entry (x:xs) = "|" ++  x ++ (print_entry xs)

--Prints all the entries
print_entries :: [Entry] -> String
print_entries [] = ""
print_entries (x:xs) = (print_entry x) ++ (print_entries xs)


instance Show Table where
    show (Table header entries) = (print_antet x) ++ (print_entry (list_padding header x)) ++ (print_antet x) ++
                                  (print_entries (entry_padding entries x)) ++ (print_antet x)
                                       where x = calculate_maximum_len_in_table(Table header entries)

user_info = read_table '|' '\n' user_info_str
movie = read_table '|' '\n' movie_str
rating = read_table ' ' '\n' rating_str

--Functions used for Select

--Returns the index of a string elem in a string list
index_of_elem :: String -> [String] -> Integer
index_of_elem s xs = aux_index_of_elem s 1 xs

--Returns the string element at a given index
elem_at_index :: TableSchema -> Integer -> String
elem_at_index [] x = ""
elem_at_index ts x = if x == 1 then (head ts) else elem_at_index (tail ts) (x - 1)

--aux function for 'elem_at_index'
aux_index_of_elem :: String -> Integer -> [String] -> Integer
aux_index_of_elem s n [] = -1
aux_index_of_elem s n (x:xs) = if s == x then n else aux_index_of_elem s (n + 1) xs

{-Returns a list of Integers holding the numbers of the columns that match the strings given
	De ex. ["age","sex"] in headerul tabele user_info va returna [2,3]
-}
count_table_headers :: TableSchema -> [String] -> [Integer]
count_table_headers ts s | s == [] = []
                         | elem (head s) ts = ( toInteger (index_of_elem (head s) ts)) : count_table_headers ts (tail s)
                         | otherwise = (count_table_headers ts (tail s))

--Given a list of Integers, this function extracts only the strings at the positions given in the list
extract_headers:: TableSchema -> [Integer] -> TableSchema
extract_headers ts [] = []
extract_headers ts (x:xs) = (elem_at_index ts x) : (extract_headers ts xs)

--Given a list of Integers, this function extracts only the columns at the positions given in the list
extract_columns:: [Entry] -> [Integer] -> [Entry]
extract_columns [] list = []
extract_columns (x:xs) list = (extract_headers x list):(extract_columns xs list)

--Selects the columns given in the list of Strings
select_columns :: Table -> [String] -> Table
select_columns (Table ts entries) fields = Table (extract_headers ts list) (extract_columns entries list)
                                      where list = count_table_headers ts fields



--Functions used for SelectLImit:

--Given an Integer n and a list of entries, this functions returns the first n entries
first_n :: Integer -> [Entry] -> [Entry]
first_n 0 entries = []
first_n n entries = (head entries) : (first_n (n - 1) (tail  entries))

--Selects the column given in the list of Strings and selects the first n entries
select_nr_columns :: Table -> [String] -> Integer -> Table
select_nr_columns (Table ts entries) fields nr = Table (extract_headers ts list) (first_n nr (extract_columns entries list))
                                                  where list = count_table_headers ts fields


--Functions used for Filter

--Checks if a list of strings contains a given string
is_in_list :: String -> [String] -> Bool
is_in_list s [] = False
is_in_list s (x:xs) = if x == s then True else is_in_list s xs

--Selects only the entries that match the filter condition
select_entries :: Table -> FilterCondition -> [Entry]
select_entries (Table ts []) fc = []
select_entries (Table ts (x:xs)) fc = if ((getFilter fc ts) x) == True then x:(select_entries (Table ts xs) fc) 
                                                                       else select_entries (Table ts xs) fc

--returns a table that has its entries filtered by the filter codition
filter_table :: Table -> FilterCondition -> Table
filter_table (Table ts entries) fc = Table ts (select_entries (Table ts entries) fc)

data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

-- TODO 3
getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter (Lt field int) ts = (\entry -> if ((read (elem_at_index entry idx)) :: Integer) < int then True else False)
                             where idx = index_of_elem field ts
getFilter (Eq field str) ts = (\entry -> if (elem_at_index entry idx) == str then True else False)
                             where idx = index_of_elem field ts
getFilter (In field strings) ts = (\entry -> if (is_in_list (elem_at_index entry idx) strings) == True then True else False)
                             where idx = index_of_elem field ts
getFilter (Not fc) ts = (\entry -> if (getFilter fc ts) entry == True then False else True)


--Functions used for :||

--Reunion of 2 tables with the same TableSchema
reunite_tables:: Table -> Table -> Table
reunite_tables (Table ts1 e1) (Table ts2 e2) = Table ts1 (reunite_entries e1 e2)

reunite_entries:: [Entry] -> [Entry] -> [Entry]
reunite_entries entries [] = entries
reunite_entries entries (x:xs) | elem x entries == True = reunite_entries entries xs
                               | otherwise = reunite_entries (entries ++ [x]) xs
{-
eliminate_duplicates_entry:: [Entry] -> [Entry]
eliminate_duplicates_entry [x] = [x]
eliminate_duplicates_entry (x:y:xs) = if x == y then (x:xs) else x:(eliminate_duplicates_entry (y:xs))

eliminate_duplicates_table :: Table -> Table
eliminate_duplicates_table (Table ts e)  = Table ts (eliminate_duplicates_entry e)
-}

-- TODO 4
data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

eval :: Query -> Table
eval (Atom table) = table

eval (Select fields (Atom table)) = select_columns table fields
eval (Select fields q) = eval $ Select fields $  Atom $ eval q

eval (SelectLimit fields nr (Atom table)) = select_nr_columns table fields nr
eval (SelectLimit fields nr q) = eval $ SelectLimit fields nr $ Atom $ eval q

eval (Filter fc (Atom table)) = filter_table table fc
eval (Filter fc q) = eval $ Filter fc $ Atom $ eval q

eval ((Atom table1) :|| (Atom table2)) =  reunite_tables table1 table2
eval (q1 :|| q2) = eval ((Atom (eval q1)) :|| (Atom (eval q2)))

-- TODO 5

z = eval $ table1 :|| table2
  where
    table1 = Filter (Lt "user_id" 25) $ Select ["user_id", "occupation"] $ Atom user_info
    table2 = Filter (Not (Lt "user_id" 45)) $ Select ["occupation", "user_id"] $ Atom user_info

    
--Functions TODO 5.1

--returns a table containing the entries with the id given as paramtere
table_with_id :: String -> Table -> Table
table_with_id id table  = eval $ Filter (Eq "user_id" id) $ Atom table

--Returns the n`th value in a table entrie
get_field_value :: Table -> Integer -> String
get_field_value (Table ts entries) n = elem_at_index (head entries) n

--TODO 5.1
same_zone :: String -> Query
same_zone id = Atom $ eval $ Select ["user_id", "occupation"] $ Atom table_with_same_zone_without_source_id 
                where table_with_same_zone_without_source_id = eval $ Filter (Not (Eq "user_id" id)) $ Atom table_with_same_zone
                      table_with_same_zone =  eval $ Filter (Eq "zone" zone) $ Atom user_info
                      zone = get_zone_value (table_with_id id user_info)
                      get_zone_value table = get_field_value table 5

--TODO 5.2
male_within_age :: Integer -> Integer -> Query
male_within_age a b = Atom $ eval $ Select ["occupation", "zone"] $ Atom male_table_withing_age
                  where male_table_withing_age = eval $ Filter (Eq "sex" "M") $ Atom table_withing_age
                        table_withing_age = eval $ Filter (Not (Eq "age" (show a))) $ Atom table_over_and_equal_inferior_age
                        table_over_and_equal_inferior_age = eval $ Filter (Not(Lt "age" a)) $ Atom table_under_superior_age
                        table_under_superior_age =  eval $ Filter (Lt "age" b) $ Atom user_info

--Functions for TODO 5.3

--returns a table that contains entries matching the zones in the list given
table_within_zone_list :: Table -> [String] -> Query
table_within_zone_list table list | (length list) == 1 = Atom $  eval $ Filter (Eq "zone" (head list)) $ Atom $ table
                                  | otherwise = Atom (eval $ Filter ( Eq "zone" (head list) ) $ Atom $ table) :|| 
                                      (table_within_zone_list table (tail list))

--returns a table that contains entries matching the occupations in the list given
table_within_occupation_list :: Table -> [String] -> Query
table_within_occupation_list table list |  (length list) == 1 = Atom $  eval $ Filter (Eq "occupation" (head list)) $ Atom $ table
                                        | otherwise = Atom (eval $ Filter ( Eq "occupation" (head list) ) $ Atom $ table) :|| 
                                            (table_within_occupation_list table (tail list))

--TODO 5.3
mixed :: [String] -> [String] -> Integer -> Query
mixed s1 s2 nr = Select ["user_id"] $ table_with_occ_and_zone
                where table_with_occ_and_zone = table_within_occupation_list (eval (table_within_zone_list table_under_age s1)) s2
                      table_under_age = eval $ Filter (Lt "age" nr) $ Atom user_info



