zero = x(X).x(s).x(z).z<->x
one = x(X).x(s).x(z).?s[f].f[a].(a<->z|f<->x)


server = ?x[y] . y[] . end
client = !x(y) . y() . end

run = \x . (server | client)

-- sell = x(u) . x(v) . x[w] . (u() . v() . w[] . end | x[] . end)
