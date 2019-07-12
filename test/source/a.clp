zero : { x : forall X . ? (X * ^X) % (^X % X) }
zero = x(X).x(s).x(z).z<->x

one : { x : forall X . ? (X * ^X) % (^X % X) }
one = x(X).x(s).x(z).?s[f].f[a].(a<->z|f<->x)

count :
  { x : ^ (forall X . ? (X * ^X) % (^X % X))
  }

-- server = ?x[y] . y[] . end
-- client = !x(y) . y() . end

-- run = \x . (server | client)

-- sell = x(u) . x(v) . x[w] . (u() . v() . w[] . end | x[] . end)
