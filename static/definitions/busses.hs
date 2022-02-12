let fxs = ["amp"
       ,"pan"
       ,"accelerate"
       ,"lpf"
       ,"lpq"
       ,"hpf"
       ,"hpq"
       ,"shape"
       ,"room"
       ,"size"
       ,"delayt"
       ,"delayfb"
       ,"elementspitch"
       ,"elementsstrength"
       ,"elementscontour"
       ,"elementsbowlevel"
       ,"elementsblowlevel"
       ,"elementsstrikelevel"
       ,"elementsflow"
       ,"elementsmallet"
       ,"elementsbowtimb"
       ,"elementsblowtimb"
       ,"elementsstriketimb"
       ,"elementsgeom"
       ,"elementsbright"
       ,"elementsdamp"
       ,"elementspos"
       ,"elementsspace"
       ,"elementsmodel"
       ,"elementseasteregg"
       ,"mu"
       ,"ringsfreq"
       ,"ringsstruct"
       ,"ringsbright"
       ,"ringsdamp"
       ,"ringspos"
       ,"ringsmodel"
       ,"ringspoly"
       ,"ringsinternal"
       ,"ringseasteregg"
       ,"ripplescf"
       ,"ripplesreson"
       ,"ripplesdrive"
       ,"verbgain"
       ,"verbwet"
       ,"verbtime"
       ,"verbdamp"
       ,"verbhp"
       ,"verbfreeze"
       ,"verbdiff"
       ,"warpsalgo"
       ,"warpstimb"
       ,"warpsosc"
       ,"warpsfreq"
       ,"warpsvgain"
       ,"warpseasteregg"
       ,"cloudspitch"
       ,"cloudspos"
       ,"cloudssize"
       ,"cloudsdens"
       ,"cloudstex"
       ,"cloudswet"
       ,"cloudsgain"
       ,"cloudsspread"
       ,"cloudsrvb"
       ,"cloudsfb"
       ,"cloudsfreeze"
       ,"cloudsmode"
       ,"cloudslofi"
       ,"enhance"
       ,"imag"
       ,"real"
       ,"binshift"
       ,"comb"
       ,"freeze"
       ,"hbrick"
       ,"lbrick"
       ,"scram"
       ,"smear"
       ,"freq"
       ,"sup"
       ]
    busmap :: Map String Int
    busmap = Data.Map.fromList $ zip fxs [0..]
    maxBusses = pure $ length fxs
    genrecv name busid = pI ("^" ++ name) busid
    genbusF name busid pat = pF name pat # pI ("^" ++ name) busid
    genbusI :: String -> Pattern Int -> Pattern Double -> ControlPattern
    genbusI name busid pat = pI name (fmap floor pat) # pI ("^" ++ name) busid
    controlbus busname shift pat = streamReplace tidal (Sound.Tidal.ID.ID $ busname ++ "control" ++ (show shift)) $ struct "t*128" $ p
                        where totalShift = (pure shift)*maxBusses
                              p = case busname of
                                        "amp" -> ampbus (0 + totalShift) pat
                                        "pan" -> panbus (1 + totalShift) pat
                                        "accelerate" -> acceleratebus (2 + totalShift) pat
                                        "lpf" -> lpfbus (3 + totalShift) pat
                                        "lpq" -> lpqbus (4 + totalShift) pat
                                        "hpf" -> hpfbus (5 + totalShift) pat
                                        "hpq" -> hpqbus (6 + totalShift) pat
                                        "shape" -> shapebus (7 + totalShift) pat
                                        "room" -> roombus (8 + totalShift) pat
                                        "size" -> sizebus (9 + totalShift) pat
                                        "delayt" -> delaybus (10 + totalShift) pat
                                        "delayfb" -> delayfbbus (11 + totalShift) pat
                                        "elementspitch" -> genbusF "elementspitch" (12 + totalShift) pat
                                        "elementsstrength" -> genbusF "elementsstrength" (13 + totalShift) pat
                                        "elementscontour" -> genbusF "elementscontour" (14 + totalShift) pat
                                        "elementsbowlevel" -> genbusF "elementsbowlevel" (15 + totalShift) pat
                                        "elementsblowlevel" -> genbusF "elementsblowlevel" (16 + totalShift) pat
                                        "elementsstrikelevel" -> genbusF "elementsstrikelevel" (17 + totalShift) pat
                                        "elementsflow" -> genbusF "elementsflow" (18 + totalShift) pat
                                        "elementsmallet" -> genbusF "elementsmallet" (19 + totalShift) pat
                                        "elementsbowtimb" -> genbusF "elementsbowtimb" (20 + totalShift) pat
                                        "elementsblowtimb" -> genbusF "elementsblowtimb" (21 + totalShift) pat
                                        "elementsstriketimb" -> genbusF "elementsstriketimb" (22 + totalShift) pat
                                        "elementsgeom" -> genbusF "elementsgeom" (23 + totalShift) pat
                                        "elementsbright" -> genbusF "elementsbright" (24 + totalShift) pat
                                        "elementsdamp" -> genbusF "elementsdamp" (25 + totalShift) pat
                                        "elementspos" -> genbusF "elementspos" (26 + totalShift) pat
                                        "elementsspace" -> genbusF "elementsspace" (27 + totalShift) pat
                                        "elementsmodel" -> genbusI "elementsmodel" (28 + totalShift) pat
                                        "elementseasteregg" -> genbusI "elementseasteregg" (29 + totalShift) pat
                                        "mu" -> genbusF "mu" (30 + totalShift) pat
                                        "ringsfreq" -> genbusF "ringsfreq" (31 + totalShift) pat
                                        "ringsstruct" -> genbusF "ringsstruct" (32 + totalShift) pat
                                        "ringsbright" -> genbusF "ringsbright" (33 + totalShift) pat
                                        "ringsdamp" -> genbusF "ringsdamp" (34 + totalShift) pat
                                        "ringspos" -> genbusF "ringspos" (35 + totalShift) pat
                                        "ringsmodel" -> genbusF "ringsmodel" (36 + totalShift) pat
                                        "ringspoly" -> genbusI "ringspoly" (37 + totalShift) pat
                                        "ringsinternal" -> genbusI "ringsinternal" (38 + totalShift) pat
                                        "ringseasteregg" -> genbusI "ringseasteregg" (39 + totalShift) pat
                                        "ripplescf" -> genbusF "ripplescf" (40 + totalShift) pat
                                        "ripplesreson" -> genbusF "ripplesreson" (41 + totalShift) pat
                                        "ripplesdrive" -> genbusF "ripplesdrive" (42 + totalShift) pat
                                        "verbgain" -> genbusF "verbgain" (43 + totalShift) pat
                                        "verbwet" -> genbusF "verbwet" (44 + totalShift) pat
                                        "verbtime" -> genbusF "verbtime" (45 + totalShift) pat
                                        "verbdamp" -> genbusF "verbdamp" (46 + totalShift) pat
                                        "verbhp" -> genbusF "verbhp" (47 + totalShift) pat
                                        "verbfreeze" -> genbusI "verbfreeze" (48 + totalShift) pat
                                        "verbdiff" -> genbusF "verbdiff" (49 + totalShift) pat
                                        "warpsalgo" -> genbusI "warpsalgo" (50 + totalShift) pat
                                        "warpstimb" -> genbusF "warpstimb" (51 + totalShift) pat
                                        "warpsosc" -> genbusI "warpsosc" (52 + totalShift) pat
                                        "warpsfreq" -> genbusF "warpsfreq" (53 + totalShift) pat
                                        "warpsvgain" -> genbusF "warpsvgain" (54 + totalShift) pat
                                        "warpseasteregg" -> genbusI "warpseasteregg" (55 + totalShift) pat
                                        "cloudspitch" -> genbusF "cloudspitch" (56 + totalShift) pat
                                        "cloudspos" -> genbusF "cloudspos" (57 + totalShift) pat
                                        "cloudssize" -> genbusF "cloudssize" (58 + totalShift) pat
                                        "cloudsdens" -> genbusF "cloudsdens" (59 + totalShift) pat
                                        "cloudstex" -> genbusF "cloudstex" (60 + totalShift) pat
                                        "cloudswet" -> genbusF "cloudswet" (61 + totalShift) pat
                                        "cloudsgain" -> genbusF "cloudsgain" (62 + totalShift) pat
                                        "cloudsspread" -> genbusF "cloudsspread" (63 + totalShift) pat
                                        "cloudsrvb" -> genbusF "cloudsrvb" (64 + totalShift) pat
                                        "cloudsfb" -> genbusF "cloudsfb" (65 + totalShift) pat
                                        "cloudsfreeze" -> genbusF "cloudsfreeze" (66 + totalShift) pat
                                        "cloudsmode" -> genbusF "cloudsmode" (67 + totalShift) pat
                                        "cloudslofi" -> genbusF "cloudslofi" (68 + totalShift) pat
                                        "enhance" -> genbusF "enhance" (69 + totalShift) pat
                                        "imag" -> genbusF "imag" (70 + totalShift) pat
                                        "real" -> genbusF "real" (71 + totalShift) pat
                                        "binshift" -> genbusF "binshift" (72 + totalShift) pat
                                        "comb" -> genbusF "comb" (73 + totalShift) pat
                                        "freeze" -> genbusF "freeze" (74 + totalShift) pat
                                        "hbrick" -> genbusF "hbrick" (75 + totalShift) pat
                                        "lbrick" -> genbusF "lbrick" (76 + totalShift) pat
                                        "scram" -> genbusF "scram" (77 + totalShift) pat
                                        "smear" -> genbusF "smear" (78 + totalShift) pat
                                        "freq" -> genbusF "freq" (79 + totalShift) pat
                                        "sup" -> genbusF "sup" (80 + totalShift) pat
    recv name shift = case Data.Map.lookup name busmap of
                          Nothing -> error "Bus not found"
                          Just i -> case name of
                          		"lpq" -> genrecv "resonance" ((pure i) + totalShift)
                          		"lpf" -> genrecv "cutoff" ((pure i) + totalShift)
                          		_ -> genrecv name ((pure i) + totalShift)
                          where totalShift = (pure shift)*maxBusses
    -- some predefined recv shortcuts
    cb = controlbus
    elementrecv shift pat = pat
                      # genrecv "elementspitch" (12 + totalShift)
                      # genrecv "elementsstrength" (13 + totalShift)
                      # genrecv "elementscontour" (14 + totalShift)
                      # genrecv "elementsbowlevel" (15 + totalShift)
                      # genrecv "elementsblowlevel" (16 + totalShift)
                      # genrecv "elementsstrikelevel" (17 + totalShift)
                      # genrecv "elementsflow" (18 + totalShift)
                      # genrecv "elementsmallet" (19 + totalShift)
                      # genrecv "elementsbowtimb" (20 + totalShift)
                      # genrecv "elementsblowtimb" (21 + totalShift)
                      # genrecv "elementsstriketimb" (22 + totalShift)
                      # genrecv "elementsgeom" (23 + totalShift)
                      # genrecv "elementsbright" (24 + totalShift)
                      # genrecv "elementsdamp" (25 + totalShift)
                      # genrecv "elementspos" (26 + totalShift)
                      # genrecv "elementsspace" (27 + totalShift)
                      # genrecv "elementsmodel" (28 + totalShift)
                      # genrecv "elementseasteregg" (29 + totalShift)
                      where totalShift = (pure shift)*maxBusses
    murecv shift pat = pat
                      # genrecv "mu" (30 + totalShift)
                      where totalShift = (pure shift)*maxBusses
    ringsrecv shift pat = pat
                      # genrecv "ringsfreq" (31 + totalShift)
                      # genrecv "ringsstruct" (32 + totalShift)
                      # genrecv "ringsbright" (33 + totalShift)
                      # genrecv "ringsdamp" (34 + totalShift)
                      # genrecv "ringspos" (35 + totalShift)
                      # genrecv "ringsmodel" (36 + totalShift)
                      # genrecv "ringspoly" (37 + totalShift)
                      # genrecv "ringsinternal" (38 + totalShift)
                      # genrecv "ringseasteregg" (39 + totalShift)
                      where totalShift = (pure shift)*maxBusses
    ripplesrecv shift pat = pat
                      # genrecv "ripplescf" (40 + totalShift)
                      # genrecv "ripplesreson" (41 + totalShift)
                      # genrecv "ripplesdrive" (42 + totalShift)
                      where totalShift = (pure shift)*maxBusses
    verbrecv shift pat = pat
                      # genrecv "verbgain" (43 + totalShift)
                      # genrecv "verbwet" (44 + totalShift)
                      # genrecv "verbtime" (45 + totalShift)
                      # genrecv "verbdamp" (46 + totalShift)
                      # genrecv "verbhp" (47 + totalShift)
                      # genrecv "verbfreeze" (48 + totalShift)
                      # genrecv "verbdiff" (49 + totalShift)
                      where totalShift = (pure shift)*maxBusses
    warpsrecv shift pat = pat
                      # genrecv "warpsalgo" (50 + totalShift)
                      # genrecv "warpstimb" (51 + totalShift)
                      # genrecv "warpsosc" (52 + totalShift)
                      # genrecv "warpsfreq" (53 + totalShift)
                      # genrecv "warpsvgain" (54 + totalShift)
                      # genrecv "warpseasteregg" (55 + totalShift)
                      where totalShift = (pure shift)*maxBusses
    cloudsrecv shift pat = pat
                      # genrecv "cloudspitch" (56 + totalShift)
                      # genrecv "cloudspos" (57 + totalShift)
                      # genrecv "cloudssize" (58 + totalShift)
                      # genrecv "cloudsdens" (59 + totalShift)
                      # genrecv "cloudstex" (60 + totalShift)
                      # genrecv "cloudswet" (61 + totalShift)
                      # genrecv "cloudsgain" (62 + totalShift)
                      # genrecv "cloudsspread" (63 + totalShift)
                      # genrecv "cloudsrvb" (64 + totalShift)
                      # genrecv "cloudsfb" (65 + totalShift)
                      # genrecv "cloudsfreeze" (66 + totalShift)
                      # genrecv "cloudsmode" (67 + totalShift)
                      # genrecv "cloudslofi" (68 + totalShift)
                      where totalShift = (pure shift)*maxBusses
