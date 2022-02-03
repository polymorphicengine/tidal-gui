let input :: Note -> (ControlPattern -> ControlPattern) -> IO ()
    input x fx = (streamReplace tidal) (ID $ "input" ++ (show $ floor $ unNote x)) $ fx $ s "in1" # n (pure x)
