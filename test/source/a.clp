zero : { x : forall X . ? (X * ^X) % (^X % X) }
zero = x(X).x(s).x(z).z<->x

one : { x : forall X . ? (X * ^X) % (^X % X) }
one = x(X).x(s).x(z).?s[f].f[a].(a<->z|f<->x)

incr :
  { a : 0
  ; f : ? Top
  }

nought :
  { z : ? Top
  }

a = x[s] . (!s(f) . f(a) . incr | x[z] . (nought | x <-> y))

count :
  { x : ^ (forall X . ? (X * ^X) % (^X % X))
  ; y : ? Top
  }
count = x[? Top] . x[s] . (!s(f) . f(a) . incr | x[z] . (nought | x <-> y))

countZero = \x . (zero | count)
countOne = \x . (one | count)
