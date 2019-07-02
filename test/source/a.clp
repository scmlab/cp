-- putName = u[] . end
-- compute = u() . v() . w[] . end

compose0 = \x . (x[] . end | x() . end)
output0 = x[y] . (y[] . end | x() . end)
input = x(y) . x() . y() . end
