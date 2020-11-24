module Lib where

import           Data.List.Split                ( splitOn )
import           Data.List                      ( find )
import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                )
import           System.Directory

categories :: [(String, String)]
categories =
    [ ("txt" , "Docs")
    , ("7z"  , "Compress")
    , ("doc" , "Docs")
    , ("zip" , "Compress")
    , ("rar" , "Compress")
    , ("docx", "Docs")
    , ("ppt" , "Docs")
    , ("pptx", "Docs")
    , ("xls" , "Docs")
    , ("xlsx", "Docs")
    ]

categorizeItems :: FilePath -> IO ()
categorizeItems filesrc = associated_with_dst >>= mapM_
    (\(filename, dst) ->
        renameFile (filesrc ++ "/" ++ filename) (dst ++ "/" ++ filename)
    )
  where
    items = map (\x -> (x, last $ splitOn "." x)) <$> listDirectory filesrc
    identified_items =
        filter (\(_, ext) -> isJust (find (\(h, _) -> h == ext) categories))
            <$> items
    associated_with_dst =
        map (\(x, ext) -> (x, fromMaybe undefined $ lookup ext categories))
            <$> identified_items
