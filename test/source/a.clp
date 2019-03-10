-- a = x[putName] . ( putName[].end | x[putName] . (putName[].end | x(getReceipt) . x().getReceipt[].end))
-- b = \ chan : 1 . ( chan[x].( x[].end | chan[].end ) | chan(x) . x(). x[].end )
-- c = \ chan : 1 . ( chan(a) . chan(b) . a <-> b | chan[a] . ( a[] . end | chan[b] . (b[] . end | a <-> b ) ) )

-- output = x[y] . (y[].end| x[].end)

-- input = x(y) . x() . y() . y[].end

-- run = x[].end


-- run = \ x: 1 . (x[y] . (y[].end | x[].end) | x(w).x().w[].end)

-- link = \ x : 1 . (x <-> w | x[].end)

-- acc = !x(y) . y[] . end

-- req = ?y[z] . z[] .end

-- acc_req_cut = \ x : 1 . (!x(y) . y[] . end | ?x[y] . y() . z[]. end)


-- zero = x($1) . x(s) . x(z) . z <-> x
-- ping = x[1] . x[s] . (!s(f).f(a).a().?y[u].u().f[].end | x[z].(z[].end | x().w[]. end))

-- zero_ping = \ x : 1 . (x($1) . x(s) . x(z) . z <-> x | x[1] . x[s] . (!s(f).f(a).a().?y[u].u().f[].end | x[z].(z[].end | x().w[]. end)))

-- ex1 = x[1] . x[].end
-- forallBot = x($1) . x() . y[] .end

-- test = \ x : 1 . (x[1] . x[].end | x($1) . x() . y[] .end)


x(s) . x(z) . z <-> x
x[s] . (!s(f).f(a).a().?y[u].u().f[].end | x[z].(z[].end | x().w[]. end))
