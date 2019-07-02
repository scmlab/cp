-- putName = u[] . end
-- compute = u() . v() . w[] . end

-- test = \ q . (a() . end | b[] . end)
sub = x[y] . (y() . end | x[] . end)
test = w(z) . z(x) . sub
