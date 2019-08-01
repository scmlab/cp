# Reducing expressions in CP

## Background

CP is a process calculus presented by Philip Wadler in his paper "Propositions as sessions" (https://dl.acm.org/citation.cfm?id=2364568), as an attempt to bridge *π-calculus*
with *Linear logic*.

Process calculus (like CP) provides a mean of modelling and describing the interactions and communications between a bunch of independent processes.
The interaction between these processes are often described with some reduction
rules.

To better illustrate this, say, we have 3 process, `P`, `Q` and `R`.
They are *parallelly composed* with the binary operator `|`:

```
(P | Q) | R
```

Since `|` is associative and commutative, the order or the sequence of
composition doesn't matter. We can omit the parentheses, and place them however
we like. What we have is basically a soup of processes:

```
P | Q | R
```

Moreover, these processes may *react* with each other.
Suppose there's a reduction rule like this:

```
P | Q => P' | Q'
```

After applying the rule (along with some other structural rules), the soup would become like this:

```
P | Q | R => P' | Q' | R
```

This is basically how processes interacts with each other.
However, things are a bit more tricky in CP.

## Parallel composition in CP

The parallel composition `|` in CP is different, in that the operator is now
indexed with some *channels*.

```
P <=x=> Q
```

Moreover, the processes also specify which channel they are communicating with.

```
[x]P <=x=> [x]Q
```

In order for the interaction to take place, both the processes and the
composition operator must all have matching channels (in this example, `x`).

An because structural properties like associativity and commutativity
are also different. We can't think of them as a soup of processes anymore,
but *a tree of processes*.

It's also not so obvious how reductions may take place anymore,
because the processes are now in a tree.
The matching process (of which the reduction will happen) may be placed at
different subtrees. Keeping them from interacting with each other:

```
[x]P <=x=> ( [x]Q <=y=> [y]R )
```

To put the matching processes in the same subtree, we must rotate the tree (by
applying some structural rules):

```
[x]P <=x=> ( [x]Q <=y=> [y]R )
  => ( [x]P <=x=> [x]Q ) <=y=> [y]R
```

## Sequence of tree rotations

*to be continued*
