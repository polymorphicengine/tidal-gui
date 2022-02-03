let wrandbool :: Double -> Pattern Time -> Pattern Bool
    wrandbool x n = segment n $ wchoose [(True,x),(False,1-x)]
    randbool = wrandbool 0.5
    wrLoop w cycs dens shift = timeLoop cycs $ shift ~> wrandbool w dens
    rLoop = wrLoop 0.5
