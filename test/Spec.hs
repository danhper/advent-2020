import Data.List (stripPrefix)
import Runner (run)
import System.Directory (listDirectory)
import System.FilePath (dropExtension, takeFileName, (</>))

testSolution :: FilePath -> IO ()
testSolution solutionPath = do
  expected <- readFile solutionPath
  let (Just problem) = stripPrefix "day" (dropExtension (takeFileName solutionPath))
  output <- run (read problem)
  putStrLn ""
  if expected == output
    then putStrLn ("\x1b[0;32m✔ Day " ++ problem ++ " passed\x1b[0m\n")
    else
      putStrLn
        ( "\x1b[0;31m✗ Day " ++ problem ++ " failed:\n"
            ++ output
            ++ "!=\n"
            ++ expected
            ++ "\x1b[0m\n"
        )
main :: IO ()
main = do
  let solutionsDir = "data/solutions"
  solutions <- listDirectory solutionsDir
  mapM_ (testSolution . (solutionsDir </>)) solutions
