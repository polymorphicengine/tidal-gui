let
sameDur :: Event a -> Event a -> Bool
sameDur e1 e2 = (whole e1 == whole e2) && (part e1 == part e2)
--
groupEventsBy :: Eq a => (Event a -> Event a -> Bool) -> [Event a] -> [[Event a]]
groupEventsBy f [] = []
groupEventsBy f (e:es) = eqs:(groupEventsBy f (es Data.List.\\ eqs))
                   where eqs = e:[x | x <- es, f e x]
--
-- assumes that all events in the list have same whole/part
collectEvent :: [Event a] -> Maybe (Event [a])
collectEvent [] = Nothing
collectEvent l@(e:es) = Just $ e {context = con, value = vs}
                      where con = unionC $ Data.List.map context l
                            vs = Data.List.map value l
                            unionC [] = Context []
                            unionC ((Context is):cs) = Context (is ++ iss)
                            						 where Context iss = unionC cs
--
collectEventsBy :: Eq a => (Event a -> Event a -> Bool) -> [Event a] -> [Event [a]]
collectEventsBy f es = remNo $ Data.List.map collectEvent (groupEventsBy f es)
                     where 
                     remNo [] = []
                     remNo (Nothing:cs) = remNo cs
                     remNo ((Just c):cs) = c : (remNo cs)
--
collectBy :: Eq a => (Event a -> Event a -> Bool) -> Pattern a -> Pattern [a]
collectBy f = withEvents (collectEventsBy f)
--
collect :: Eq a => Pattern a -> Pattern [a]
collect = collectBy sameDur
--
uncollectEvent :: Event [a] -> [Event a]
uncollectEvent e = [e {value = x} | x <- value e]
--
uncollectEvents :: [Event [a]] -> [Event a]
uncollectEvents = Data.List.concatMap uncollectEvent
--
uncollect :: Pattern [a] -> Pattern a
uncollect = withEvents uncollectEvents
