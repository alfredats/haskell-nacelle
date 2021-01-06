import SVparse

main :: IO ()
main = do
  putStrLn "-----------------------------------------------------"
  putStrLn "TEST : psv"
  putStrLn "-----------------------------------------------------"
  putStrLn "psv \"2.1.1\" == Success (SemVer 2 1 1 [] [])"
  psv "2.1.1"
  putStrLn ""
  putStrLn "psv \"1.0.0-x.7.z.92\" == Success (SemVer 1 0 0 [NOSS \"x\", NOSI 7, NOSS \"z\", NOSI 92] [])"
  psv "1.0.0-x.7.z.92"
  putStrLn ""
  putStrLn "psv \"1.0.0-gamma+002\" == Success (SemVer 1 0 0 [NOSS \"gamma\"] [NOSI 2])"
  psv "1.0.0-gamma+002"
  putStrLn ""
  putStrLn "psv \"1.0.0-beta+oof.sha.41af286\" == Success (SemVer 1 0 0 [NOSS \"beta\"] [NOSS \"oof\", NOSS \"sha\", NOSS \"41af286\"])"
  psv "1.0.0-gamma+002"
  putStrLn ""
  putStrLn "(SemVer 2 1 1 [] []) > (SemVer 2 1 0 [] []) == True"
  print $ SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []
  putStrLn "end"
