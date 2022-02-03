let
add :: Pattern a -> Time -> Pattern a -> Pattern a
add value t pat = slow (pure $ 1+t) $ timeCat [(shift,pat),(1-shift, value)]
				where shift = 1 / (t + 1)


