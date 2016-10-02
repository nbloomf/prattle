module Main where

import Test.QuickCheck
import Data.Time
import Data.List
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  n <- case args of
         [x] -> return (read x)
         _   -> return 0
  blog <- wp_blog n
  putStr (to_xml blog)
  return ()

class ToXML t where
  to_xml :: t -> String

data WP_Blog = WP_Blog
  {  wp_blog_posts :: [WP_Post]
  } deriving Show

wp_blog :: Int -> IO WP_Blog
wp_blog n = do
  the_posts <- wp_posts n
  return WP_Blog
    { wp_blog_posts = the_posts
    }

instance ToXML WP_Blog where
  to_xml x = unlines
    [ header
    , "<channel>"
    , "<wp:wxr_version>1.2</wp:wxr_version>"
    , concatMap to_xml (wp_blog_posts x)
    , "</channel>"
    , "</rss>"
    ]

data WP_Post = WP_Post
  { wp_post_title :: String
  , wp_post_link :: String
  , wp_post_pubDate :: String
  , wp_post_creator :: String
  , wp_post_guid :: String
  , wp_post_description :: String
  , wp_post_content_encoded :: String
  , wp_post_excerpt_encoded :: String
  , wp_post_post_id :: String
  , wp_post_date :: String
  , wp_post_date_gmt :: String
  , wp_post_comment_status :: String
  , wp_post_name :: String
  , wp_post_status :: String
  , wp_post_menu_order :: String
  , wp_post_type :: String
  , wp_post_password :: String
  , wp_post_is_sticky :: String
  , wp_post_category :: String
  , wp_post_comments :: [WP_Comment]
  } deriving Show

instance ToXML WP_Post where
  to_xml x = unlines
    [ "<item>"
    , "  <title>" ++ (wp_post_title x) ++ "</title>"
    , "  <link>" ++ (wp_post_link x) ++ "</link>"
    , "  <pubDate>" ++ (wp_post_pubDate x) ++ "</pubDate>"
    , "  <dc:creator>" ++ cdata (wp_post_creator x) ++ "</dc:creator>"
    , "  <content:encoded>" ++ cdata (wp_post_content_encoded x) ++ "</content:encoded>"
    , "  <wp:post_id>" ++ (wp_post_post_id x) ++ "</wp:post_id>"
    , "  <wp:post_date>" ++ cdata (wp_post_date x) ++ "</wp:post_date>"
    , concatMap to_xml (wp_post_comments x)
    , "</item>"
    ]

data WP_Comment = WP_Comment
  { wp_comment_id :: String
  , wp_comment_author :: String
  , wp_comment_author_email :: String
  , wp_comment_author_IP :: String
  , wp_comment_date :: String
  , wp_comment_date_gmt :: String
  , wp_comment_content :: String
  , wp_comment_approved :: String
  , wp_comment_type :: String
  , wp_comment_parent :: String
  , wp_comment_user_id :: String
  , wp_comment_metadata :: String
  } deriving Show

instance ToXML WP_Comment where
  to_xml x = unlines
    [ "  <wp:comment>"
    , "    <wp:comment_id>" ++ wp_comment_id x ++ "</wp:comment_id>"
    , "    <wp_comment_author>" ++ cdata (wp_comment_author x) ++ "</wp:comment_author>"
    , "    <wp_comment_author_email>" ++ cdata (wp_comment_author_email x) ++ "</wp:comment_author_email>"
    , "    <wp_comment_author_IP>" ++ cdata (wp_comment_author_IP x) ++ "</wp:comment_author_IP>"
    , "    <wp_comment_date>" ++ cdata (wp_comment_date x) ++ "</wp:comment_date>"
    , "    <wp_comment_date_gmt>" ++ cdata (wp_comment_date_gmt x) ++ "</wp:comment_date_gmt>"
    , "    <wp_comment_content>" ++ cdata (wp_comment_content x) ++ "</wp:comment_content>"
    , "    <wp_comment_approved>" ++ cdata (wp_comment_approved x) ++ "</wp:comment_approved>"
    , "    <wp_comment_type>" ++ cdata (wp_comment_type x) ++ "</wp:comment_type>"
    , "    <wp_comment_parent>" ++ (wp_comment_parent x) ++ "</wp:comment_parent>"
    , "    <wp_comment_user_id>" ++ (wp_comment_user_id x) ++ "</wp:comment_user_id>"
    , "  </wp:comment>"
    ]

wp_posts :: Int -> IO [WP_Post]
wp_posts n = sequence $ map wp_post [1..n]

wp_post :: Int -> IO WP_Post
wp_post k = do
  the_title <- generate sentence
  the_date <- generate date
  the_content <- generate paragraph
  n <- generate $ choose (0::Int,50)
  the_comments <- wp_comments n
  return WP_Post
    { wp_post_title = the_title
    , wp_post_link = ""
    , wp_post_pubDate = the_date
    , wp_post_creator = ""
    , wp_post_guid = ""
    , wp_post_description = ""
    , wp_post_content_encoded = the_content
    , wp_post_excerpt_encoded = ""
    , wp_post_post_id = show k
    , wp_post_date = the_date
    , wp_post_date_gmt = the_date
    , wp_post_comment_status = ""
    , wp_post_name = ""
    , wp_post_status = ""
    , wp_post_menu_order = ""
    , wp_post_type = ""
    , wp_post_password = ""
    , wp_post_is_sticky = ""
    , wp_post_category = ""
    , wp_post_comments = the_comments
    }

wp_comments :: Int -> IO [WP_Comment]
wp_comments n = sequence $ map wp_comment [1..n]

wp_comment :: Int -> IO WP_Comment
wp_comment k = do
  the_author <- generate word
  the_author_email <- generate email
  the_author_IP <- generate ip_address
  the_date <- generate date
  the_content <- generate paragraph
  the_approved <- generate zero_or_one
  the_type <- generate comment_type
  the_parent <- generate $ fmap show $ choose (0,k-1)
  return WP_Comment
    { wp_comment_id = show k
    , wp_comment_author = the_author
    , wp_comment_author_email = the_author_email
    , wp_comment_author_IP = the_author_IP
    , wp_comment_date = the_date
    , wp_comment_date_gmt = the_date
    , wp_comment_content = the_content
    , wp_comment_approved = the_approved
    , wp_comment_type = the_type
    , wp_comment_parent = the_parent
    , wp_comment_user_id = ""
    , wp_comment_metadata = ""
    }

comment_type :: Gen String
comment_type = frequency
  [ (98, return "")
  , (1, return "pingback")
  , (1, return "trackback")
  ]

date :: Gen String
date = return ""

ip_address :: Gen String
ip_address = do
  oa <- fmap show $ choose (0::Int,255)
  ob <- fmap show $ choose (0::Int,255)
  oc <- fmap show $ choose (0::Int,255)
  od <- fmap show $ choose (0::Int,255)
  return $ concat
    [oa, ".", ob, ".", oc, ".", od]

zero_or_one :: Gen String
zero_or_one = elements ["0", "1"]

email :: Gen String
email = do
  name <- word
  return $ name ++ "@example.com"

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

cdata :: String -> String
cdata str = "<![CDATA[" ++ str ++ "]]>"

header :: String
header = unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
  , "<rss version=\"2.0\""
  , "  xmlns:excerpt=\"http://wordpress.org/export/1.2/excerpt/\""
  , "  xmlns:content=\"http://purl.org/rss/1.0/modules/content/\""
  , "  xmlns:wfw=\"http://wellformedweb.org/CommentAPI/\""
  , "  xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
  , "  xmlns:wp=\"http://wordpress.org/export/1.2/\""
  , ">\n"
  ]
