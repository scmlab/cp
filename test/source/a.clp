zero : { x : forall X . ? (X * ^X) % (^X % X) }
zero = x(X).x(s).x(z).z<->x

one : { x : forall X . ? (X * ^X) % (^X % X) }
one = x(X).x(s).x(z).?s[f].f[a].(a<->z|f<->x)

two : { x : forall X . ? (X * ^X) % (^X % X) }
two = x(X).x(s).x(z).?s[f].f[a].(a<->z|?s[g].g[b].(f<->b|g<->x))


incr :
  { a : 0
  ; f : ? Top
  }

nought :
  { z : ? Top
  }

count :
  { x : ^ (forall X . ? (X * ^X) % (^X % X))
  ; y : ? Top
  }
count = x[? Top] . x[s] . (!s(f) . f(a) . incr | x[z] . (nought | x <-> y))

a = \x . (zero | count)
b = \x . (one | count)
d = \x . (two | count)


c = \x .
    ( \y .
      ( x[Top] . y() . end
      | y[] . end
      )
    | \z .
      ( x(X) . z() . end
      | z[] . end
      )
    )
