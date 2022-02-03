let ncat :: [(Time, Pattern a)] -> Pattern a
    ncat = seqPLoop . go 0
        where
          go _     []          = []
          go t_acc ((0, _):ps) = go t_acc ps
          go t_acc ((t, p):ps) = (t_acc, t', p) : go t' ps
            where
              t' = t_acc + t
