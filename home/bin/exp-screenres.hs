import System.Environment

determineRes :: String -> String
determineRes "mlaptop" = "1440x900"
determineRes "house_pc" = "1280x1024"
determineRes "maths_laptop" = "1280x800"
determineRes "house_laptop" = "1366x768"
determineRes "home_pc" = "1680x1050"
determineRes "small" = "650x650"
determineRes "art_laptop" = "1536x864"
determineRes "library_pc" = "1024x768"
determineRes "claptop" = "1280x800"

determineRes "" = "err: empty"
determineRes x = x ++ " given, err"


main = do
  remoteHostname <- getEnv "REMOTE_HOSTNAME"
  putStr $ determineRes remoteHostname
