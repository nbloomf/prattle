module Main where

import Test.QuickCheck

main :: IO ()
main = putStrLn "some junk"

data WP_Post = WP_Post
  { wp_post_title :: String
  , wp_post_content :: String
  }

paragraph :: Gen String
paragraph = do
  k <- paragraph_length
  ss <- vectorOf k sentence
  return $ unwords ss

sentence :: Gen String
sentence = do
  k <- sentence_length
  ws <- vectorOf k word
  p <- end_punct
  return $ (unwords ws) ++ p

word :: Gen String
word = do
  k <- word_length
  c <- first_letter
  cs <- vectorOf (k-1) any_letter
  return (c:cs)


{- Frequency data created from whole cloth -}
end_punct :: Gen String
end_punct = frequency
  [ (80, return ".")
  , (10, return "!")
  , (10, return "?")
  , (5, return "!?")
  ]


{- Frequency data pulled from thin air -}
paragraph_length :: Gen Int
paragraph_length = frequency
  [ (5, return 1)
  , (8, return 2)
  , (20, return 3)
  , (40, return 4)
  , (50, return 5)
  , (45, return 6)
  , (30, return 7)
  , (15, return 8)
  ]


{- Frequency data made up on the spot -}
sentence_length :: Gen Int
sentence_length = frequency
  [ (5, return 1)
  , (8, return 2)
  , (10, return 3)
  , (15, return 4)
  , (25, return 5)
  , (35, return 6)
  , (45, return 7)
  , (50, return 8)
  , (55, return 9)
  , (60, return 10)
  , (65, return 11)
  , (60, return 12)
  , (55, return 13)
  , (50, return 14)
  , (30, return 15)
  , (22, return 16)
  , (20, return 17)
  , (15, return 18)
  , (12, return 19)
  , (10, return 20)
  ]


{- Frequency data (roughly) from
  Length-Frequency Statistics for Written English 
  Miller, Newman, and Friedman
  in Information and Control, v. 1, pp. 370--389 (1958)
-}
word_length :: Gen Int
word_length = frequency
  [ (10, return 1)
  , (70, return 2)
  , (80, return 3)
  , (60, return 4)
  , (40, return 5)
  , (30, return 6)
  , (28, return 7)
  , (20, return 8)
  , (17, return 9)
  , (10, return 10)
  , (5, return 11)
  , (4, return 12)
  , (2, return 13)
  , (1, return 14)
  ]


{- Frequency data taken from Wikipedia -}
first_letter :: Gen Char
first_letter = frequency
  [ (11602, return 'a')
  , (4702, return 'b')
  , (3511, return 'c')
  , (2670, return 'd')
  , (2007, return 'e')
  , (3779, return 'f')
  , (1950, return 'g')
  , (7232, return 'h')
  , (6286, return 'i')
  , (597, return 'j')
  , (590, return 'k')
  , (2705, return 'l')
  , (4383, return 'm')
  , (2365, return 'n')
  , (6264, return 'o')
  , (2545, return 'p')
  , (173, return 'q')
  , (1653, return 'r')
  , (7755, return 's')
  , (16671, return 't')
  , (1487, return 'u')
  , (649, return 'v')
  , (6753, return 'w')
  , (17, return 'x')
  , (1620, return 'y')
  , (34, return 'z')
  ]

{- Frequency data taken from Wikipedia -}
any_letter :: Gen Char
any_letter = frequency
  [ (8167, return 'a')
  , (1492, return 'b')
  , (2782, return 'c')
  , (4253, return 'd')
  , (12702, return 'e')
  , (2228, return 'f')
  , (2015, return 'g')
  , (6094, return 'h')
  , (6966, return 'i')
  , (153, return 'j')
  , (772, return 'k')
  , (4025, return 'l')
  , (2406, return 'm')
  , (6749, return 'n')
  , (7507, return 'o')
  , (1929, return 'p')
  , (95, return 'q')
  , (5987, return 'r')
  , (6327, return 's')
  , (9056, return 't')
  , (2758, return 'u')
  , (978, return 'v')
  , (2360, return 'w')
  , (150, return 'x')
  , (1974, return 'y')
  , (74, return 'z')
  ]
