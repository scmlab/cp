-- a : 1 * 1 * 1
-- a = x[putName] . ( putName[].end | x[putName] . (putName[].end | x(getReceipt) . x().getReceipt[].end))

-- b : 1
-- b = \ chan . ( chan[x].( x[].end | chan[].end ) | chan(x) . x(). x[].end )

zero : 1
zero = \ chan . ( chan(a) . chan(b) . a <-> b | chan[a] . ( a[] . end | chan[b] . (b[] . end | a <-> b ) ) )
