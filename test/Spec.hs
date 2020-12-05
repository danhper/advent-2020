import Data.List (sortOn, stripPrefix)
import Data.Maybe (fromJust)
import Runner (run)
import System.Directory (listDirectory)
import System.FilePath (dropExtension, takeFileName, (</>))

getProblemNumber :: FilePath -> Int
getProblemNumber filepath = read . fromJust $ stripPrefix "day" filename
  where
    filename = dropExtension $ takeFileName filepath

testSolution :: FilePath -> IO ()
testSolution solutionPath = do
    expected <- readFile solutionPath
    let problem = getProblemNumber solutionPath
    output <- run problem Nothing
    if expected == output
        then putStrLn ("\x1b[0;32m✔ Day " ++ show problem ++ " passed\x1b[0m")
        else
            putStrLn
                ( "\x1b[0;31m✗ Day " ++ show problem ++ " failed:\n"
                    ++ output
                    ++ "!=\n"
                    ++ expected
                    ++ "\x1b[0m"
                )

main :: IO ()
main = do
    let solutionsDir = "data/solutions"
    solutions <- sortOn getProblemNumber <$> listDirectory solutionsDir
    mapM_ (testSolution . (solutionsDir </>)) solutions
