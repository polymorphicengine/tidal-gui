let genrecv name busid = pI ("^" ++ name) busid
    genbusF name busid pat = pF name pat # pI ("^" ++ name) busid
    genbusI :: String -> Pattern Int -> Pattern Double -> ControlPattern
    genbusI name busid pat = pI name (fmap floor pat) # pI ("^" ++ name) busid
    controlbus busname num pat = streamReplace tidal (Sound.Tidal.ID.ID $ busname ++ "control" ++ (show num)) $ struct "t*128" $ p
                        where p = case busname of
                                        "amp" -> ampbus (pure num) pat
                                        "pan" -> panbus (pure num) pat
                                        "accelerate" -> acceleratebus (pure num) pat
                                        "lpf" -> lpfbus (pure num) pat
                                        "lpq" -> lpqbus (pure num) pat
                                        "hpf" -> hpfbus (pure num) pat
                                        "hpq" -> hpqbus (pure num) pat
                                        "shape" -> shapebus (pure num) pat
                                        "room" -> roombus (pure num) pat
                                        "size" -> sizebus (pure num) pat
                                        "delayt" -> delaybus (pure num) pat
                                        "delayfb" -> delayfbbus (pure num) pat
                                        name -> genbusF name (pure num) pat
    recv name num = case name of
          		"lpq" -> genrecv "resonance" num
          		"lpf" -> genrecv "cutoff" num
          		_ -> genrecv name num
    -- some predefined recv shortcuts
    cb = controlbus
    elementrecv num pat = pat
                      # genrecv "elementspitch" (num)
                      # genrecv "elementsstrength" (num + 1)
                      # genrecv "elementscontour" (num + 2)
                      # genrecv "elementsbowlevel" (num + 3)
                      # genrecv "elementsblowlevel" (num + 4)
                      # genrecv "elementsstrikelevel" (num + 5)
                      # genrecv "elementsflow" (num + 6)
                      # genrecv "elementsmallet" (num + 7)
                      # genrecv "elementsbowtimb" (num + 8)
                      # genrecv "elementsblowtimb" (num + 9)
                      # genrecv "elementsstriketimb" (num + 10)
                      # genrecv "elementsgeom" (num + 11)
                      # genrecv "elementsbright" (num + 12)
                      # genrecv "elementsdamp" (num + 13)
                      # genrecv "elementspos" (num + 14)
                      # genrecv "elementsspace" (num + 15)
                      # genrecv "elementsmodel" (num + 16)
                      # genrecv "elementseasteregg" (num + 17)
    murecv num pat = pat
                      # genrecv "mu" num
    ringsrecv num pat = pat
                      # genrecv "ringsfreq" (num)
                      # genrecv "ringsstruct" (num + 1)
                      # genrecv "ringsbright" (num + 2)
                      # genrecv "ringsdamp" (num + 3)
                      # genrecv "ringspos" (num + 4)
                      # genrecv "ringsmodel" (num + 5)
                      # genrecv "ringspoly" (num + 6)
                      # genrecv "ringsinternal" (num + 7)
                      # genrecv "ringseasteregg" (num + 8)
    ripplesrecv num pat = pat
                      # genrecv "ripplescf" (num)
                      # genrecv "ripplesreson" (num + 1)
                      # genrecv "ripplesdrive" (num + 2)
    verbrecv num pat = pat
                      # genrecv "verbgain" (num)
                      # genrecv "verbwet" (num + 1)
                      # genrecv "verbtime" (num + 2)
                      # genrecv "verbdamp" (num + 3)
                      # genrecv "verbhp" (num + 4)
                      # genrecv "verbfreeze" (num + 5)
                      # genrecv "verbdiff" (num + 6)
    warpsrecv num pat = pat
                      # genrecv "warpsalgo" (num)
                      # genrecv "warpstimb" (num + 1)
                      # genrecv "warpsosc" (num + 2)
                      # genrecv "warpsfreq" (num + 3)
                      # genrecv "warpsvgain" (num + 4)
                      # genrecv "warpseasteregg" (num + 5)
    cloudsrecv num pat = pat
                      # genrecv "cloudspitch" (num)
                      # genrecv "cloudspos" (num + 1)
                      # genrecv "cloudssize" (num + 2)
                      # genrecv "cloudsdens" (num + 3)
                      # genrecv "cloudstex" (num + 4)
                      # genrecv "cloudswet" (num + 5)
                      # genrecv "cloudsgain" (num + 6)
                      # genrecv "cloudsspread" (num + 7)
                      # genrecv "cloudsrvb" (num + 8)
                      # genrecv "cloudsfb" (num + 9)
                      # genrecv "cloudsfreeze" (num + 10)
