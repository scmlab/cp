-- a = x[putName] . ( putName[].end | x[putName] . (putName[].end | x(getReceipt) . x().getReceipt[].end))
-- b = \ chan : 1 . ( chan[x].( x[].end | chan[].end ) | chan(x) . x(). x[].end )
-- c = \ chan : 1 . ( chan(a) . chan(b) . a <-> b | chan[a] . ( a[] . end | chan[b] . (b[] . end | a <-> b ) ) )

run = x[y] . (u[].end | x[].end)

-- run = \ x: 1 . (x[y] . (y[].end | x[].end) | x(w). x().x[].end)

-- close = x().y[].end

-- zero = x(X) . x(s) . x(z) . z <-> x
