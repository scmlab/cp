-- putName = u[] . end
-- compute = u() . v() . w[] . end

-- test = \ q . (a() . end | b[] . end)
test = x[y] . (y() . end | x[] . end)
