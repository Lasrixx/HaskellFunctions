--1a.
{-
Creates a new data type called Dog which is a 
tuple of a String and an Int.
-}
type Dog = (String,Int)

--1b.
{-
Takes in a list of strings and a list of ints and pairs
them together to create a list of dogs with name and
height attributes, returning this new list of Dogs.
-}
create_dog_list :: [String] -> [Int] -> [Dog]
create_dog_list xs ys = [(x,y) | (x,y) <- zip xs ys ]

--1c.
{-
Takes in a list of dogs, sorts them into ascending height
order using quick sort and returns the new sorted list.
It must use the snd function as Dog is a 2-ary tuple, and 
we want to sort in order of height which is the second
element of the tuple.
-}
sort_dog_list :: [Dog] -> [Dog]
sort_dog_list [] = []
sort_dog_list (x:xs) = 
    sort_dog_list[y | y<-xs, snd y<=snd x] ++ [x] ++ sort_dog_list[y | y<- xs, snd y>snd x]

--1d.
{-
Takes in an int n and a list of dogs and removes the n amount
of smallest dogs in the list, returning this new list. 
This requires having a sorted list (ascending).
-}
remove_smallest_dogs :: Int -> [Dog] -> [Dog]
remove_smallest_dogs n xs = drop n (sort_dog_list xs)

--1e.
{-
Takes in a list of dogs and removes all dogs that are taller than
80cm, returning a modified list of dogs.
-}
remove_tall_dogs :: [Dog] -> [Dog]
remove_tall_dogs xs = [x | x <- xs, snd x <= 80]



---- Part 2 ----
--2a.
{-
Takes in 3 Ints, m,n, and p, that returns a string of p steps, 
of m height, and n+n width, then repeats in the opposite direction.
Forward steps deals with the first half of the output; imagine:
-
--
---
Backward steps deals with the last half of the output:
---
--
-
-}
steps :: Int -> Int -> Int -> String
steps m n p 
    | m < 1 || n < 1 || p < 1 = "Invalid input \n" 
    | otherwise = forward_steps m n p n ++ backward_steps m n p
 where
     forward_steps :: Int -> Int -> Int -> Int -> String
     forward_steps m n 0 q = ""
     forward_steps m n p q = step m n ++ forward_steps m (n+q) (p-1) q

     backward_steps :: Int -> Int -> Int -> String
     backward_steps m n 0 = ""
     backward_steps m n p = step m (p*n) ++ backward_steps m n (p-1)  

{-
Takes in the m and n inputs to return a single step in the overall
program. This can then be used repeatedly to build up all steps.
This uses the line function.
-}
step :: Int -> Int -> String
step m n
    | m == 0 = ""
    | otherwise = line n ++ "\n" ++ step (m-1) n

{-
Takes in the n input and creates a single line of spaces and asterisks,
concatenating them to return a single line in the overall pattern. The
function is broken down to build a string of n spaces, and a string of
n asterisks before the two strings are concatenated together.
-}
line :: Int -> String
line n = line_blank_a n ++ line_star_a n
 where
    line_blank_a :: Int -> String
    line_blank_a n 
        | n == 0 = ""
        | otherwise = " "++line_blank_a (n-1)

    line_star_a :: Int -> String
    line_star_a n
        | n == 0 =""
        | otherwise = "*"++line_star_a (n-1)

--Part 2b.
{-
Takes in a flag width Int, n, and a flag quantity Int, m. It returns a 
String of m flags of n width, each with an 'X' pattern in them.
This contains sub-functions, called flag, which builds a single flag
pattern using sequences of asterisks, spaces, and plus.
-}
flagpattern :: Int -> Int -> String
flagpattern n m
    | n < 5 || m < 1 = "Invalid input"
    | m == 0 = ""
    | otherwise = flag n 1 n (n`mod`2==0) ++ flagpattern n (m-1)
 where
    flag :: Int -> Int -> Int -> Bool -> String
    flag n o max even
        | n == 0 = ""
        | n == max = line_star_b max++"\n"++flag (n-1) (o+1) max even
        | n == 1 = line_star_b max++"\n"++flag (n-1) (o+1) max even
        | n == max`div`2+1 && even = "*"++line_blank_b (max-n-1)++"++"++line_blank_b (max-n-1)++"*"++"\n"++flag(n-1) (o+1) max even
        | n > (max`div`2)+1 = "*"++line_blank_b (max-n-1)++"+"++line_blank_b (-max+2*n-2)++"+"++line_blank_b (max-n-1)++"*"++"\n"++ flag (n-1) (o+1) max even
        | n == (max`div`2)+1 = "*"++line_blank_b(max-n-1)++"+"++line_blank_b(max-n-1)++"*"++"\n"++flag (n-1) (o+1) max even
        | otherwise = "*"++line_blank_b (max-o-1)++"+"++line_blank_b (-max+2*o-2)++"+"++line_blank_b (max-o-1)++"*"++"\n"++flag (n-1) (o+1) max even
         where
            line_blank_b :: Int -> String
            line_blank_b n 
                | n == 0 = ""
                | otherwise = " "++line_blank_b (n-1)

            line_star_b :: Int -> String
            line_star_b n
                | n == 0 = ""
                | otherwise = "*"++line_star_b (n-1)



---- Part 3 ----
{-
Takes in two strings, representing the names of two people. Overall,
the function returns whether the people like, hate, admire, or are 
indifferent to each other. Breaking this down, first, it removes
all pairs of common characters from the two names. Then, it assigns the 
characters 'l', 'a', 'h', and 'i' in that order to each of the remaining
chracters. Finally, the last character assigned to the name signifies
what relationship the people have.
-}

compatibility :: String -> String -> String
compatibility f s = f++interpret_lahi(apply_lahi(cycle_string s f) 0)++s++" and "++s++interpret_lahi(apply_lahi(cycle_string f s) 0)++f

{-
Takes in two strings and removes characters from the 
second name that are found to be in the first name. It outputs an
augmented version of the second name, where all pairs of like characters found in the
first name have been changed to "*". This function will be called twice,
one to change the first name, and two to change the second name.
-}

cycle_string :: String -> String -> String
cycle_string [] s = s
cycle_string (x:xs) s = cycle_string xs (replace_char s x)
 where
    replace_char :: String -> Char -> String
    replace_char [] c = []
    replace_char (x:xs) c
        | x == ' ' = x:replace_char xs c
        | x == c = '*':xs
        | otherwise = x:replace_char xs c

{-
Takes in the string of the augmented name (like characters replaced with
'*') and applies 'lahi' to each of the remaining alphanumeric characters.
This means that the first character is assigned 'l', second character is
assigned 'a', and so on... fifth character is assigned 'l' again. The output
is the combination of 'lahi' applied to the given string.
-}
 
apply_lahi :: String -> Int -> String
apply_lahi [] c = []
apply_lahi (x:xs) c
    | x == '*' || x == ' ' = ' ' : apply_lahi xs c
    | otherwise = determine_char c : apply_lahi xs (c+1)
    where        
        determine_char :: Int -> Char
        determine_char c 
            | c `mod` 4 == 0 = 'l'
            | c `mod` 4 == 1 = 'a'
            | c `mod` 4 == 2 = 'h'
            | c `mod` 4 == 3 = 'i'
            | otherwise = 'l'

{-
Takes in the string of 'lahi' that has previously been generated,
and inspects the final character (that is not whitespace), using it
to define the relationship between the two names given in compatibility.
It returns a string representing the relationship. 
-}

interpret_lahi :: String -> String
interpret_lahi s 
    | s == [] = " is indifferent to "
    | last s == ' ' = interpret_lahi (init s)
    | last s == 'l' = " likes "
    | last s == 'a' = " admires "
    | last s == 'h' = " hates "
    | last s == 'i' = " is indifferent to "
    | otherwise = " likes"



---- Part 4 ----
{-
Polymorphic function that takes in a list and singe value, and returns
a list of Ints. The list is split at each occurence of the single value and
the lengths of the sublists is the value put into the output list.
-}
nsplit :: (Eq a) => [a] -> a -> [Int]
nsplit xs n = sublist_lengths (split_list xs n)

{-
Takes in a list and a target value. At each occurence of the target value,
the list is split into sublists, generating the output (a list of lists).
This function is polymorphic; it can take a list of any data type.
-}
split_list :: (Eq a) => [a] -> a -> [[a]]
split_list [] n = []
split_list xs n
    | head xs == n = split_list (tail xs) n
    | otherwise = takeWhile (/=n) xs : split_list (drop 1(dropWhile (/=n) xs)) n

{-
Takes in a list of lists of any data type. A new list is created, 
which holds the values of the lengths of each of the sub-lists.
This new list is returned.
-}
sublist_lengths :: [[a]] -> [Int]
sublist_lengths xs = [length x | x <- xs]