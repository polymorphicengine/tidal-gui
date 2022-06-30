let
vec :: Eq a => (a -> a -> a) -> Pattern a -> Pattern a -> Pattern a
vec f p1 p2 = uncollect $ do
                     xs <- collect p1
                     ys <- collect p2
                     return $ zipWith f xs ys
--
(#>) :: (Unionable a, Eq a) => Pattern a -> Pattern a -> Pattern a
(#>) = vec Sound.Tidal.Context.union
