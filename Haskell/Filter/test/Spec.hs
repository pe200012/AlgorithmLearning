
import           Lib
import           System.Directory
import           Test.HUnit
import           System.Random                  ( randomRs
                                                , getStdGen
                                                )
import           Data.List                      ( unfoldr
                                                , nub
                                                , zipWith
                                                , sort
                                                , find
                                                )
import           Lens.Micro
import           Control.Monad                  ( liftM2 )
import           Data.Maybe                     ( fromMaybe )
import Control.Concurrent (threadDelay)

test1 = TestCase
    (do
        some_random_files <- take 100 <$> generateRandomFilename
        createDirectory "test_dir"
        setCurrentDirectory "test_dir"
        createDirectory "test_download_dir"
        mapM_ createDirectory categories_dir
        mapM_
            (\name ->
                writeFile ("./test_download_dir/" ++ name) "some test content"
            )
            some_random_files
        categorizeItems "test_download_dir"
        mapM_
            (\cat -> do
                files <- listDirectory cat
                assertEqual "files should be equal" files
                    <$> maybe
                            undefined
                            snd
                            (find ((== cat) . fst) $ filesThatShouldBeThere 100)
            )
            categories_dir
        assertEqual "no files are remained" 0 . length <$> listDirectory
            "test_download_dir"
        setCurrentDirectory ".."
        threadDelay 10000000
        removeDirectoryRecursive "test_dir"
    )
  where
    generateRandomSuffixes =
        map (suffixes_list !!)
            .   randomRs (0, length suffixes_list - 1)
            <$> getStdGen
        where suffixes_list = map fst categories
    generateRandomStrings =
        unfoldr (return . splitAt 10) . randomRs ('a', 'z') <$> getStdGen
    generateRandomFileAndSuffixPairs =
        liftM2 zip generateRandomStrings generateRandomSuffixes
    generateRandomFilename =
        map (\(a, b) -> a ++ "." ++ b) <$> generateRandomFileAndSuffixPairs
    filesThatShouldBeThere nums = map
        (\cat ->
            ( cat
            , map (uncurry (++))
                .   filter
                        ( (== cat)
                        . fromMaybe undefined
                        . flip lookup categories
                        . snd
                        )
                .   take nums
                <$> generateRandomFileAndSuffixPairs
            )
        )
        categories_dir
    categories_dir  = nub $ map snd categories
    categories_list = nub $ map fst categories

main :: IO Counts
main = runTestTT $ TestList [TestLabel "categorize items test" test1]
