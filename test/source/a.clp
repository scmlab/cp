putName = u[] . end
putCredit = v[] . end
getReceipt = w() . end
compute = u() . v() . w[] . end

buy = x[u] . (putName | x[v] . (putCredit | x(w) . x() . getReceipt))
sell = x(u) . x(v) . x[w] . (compute | x[] . end)
runBuy = \ x . (buy | sell)

a = x() . end
b = x[] . end
main = \x . (x() . end | y[] . end)
