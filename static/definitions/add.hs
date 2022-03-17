let
_add :: Time -> Pattern a -> Pattern a -> Pattern a
_add t value pat = slow (pure $ 1+t) $ timeCat [(shift,pat),(1-shift, value)]
				where shift = 1 / (t + 1)
add :: Pattern Time -> Pattern a -> Pattern a -> Pattern a
add pt x y = innerJoin $ fmap (\t -> _add t x y) pt


