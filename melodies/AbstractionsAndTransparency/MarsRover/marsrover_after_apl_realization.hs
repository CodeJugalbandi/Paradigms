import Data.List (elemIndices)

main :: IO ()
main = 
  do
    let movements = [[0,1], [1,0], [0,-1], [-1,0]]
    -- 'LMR'⍳commands
    -- 2 2 3 2 2 3 2 1 2 2
    let cmds = "MRRMMLRM"
    -- print [elemIndices c cmds | c <- "LMR"]
    print $ zipWith (\idx c -> (idx, c)) [1,2,3] cmds
    print "Done"
