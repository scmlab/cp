putName = u[] . end
putCredit = v[] . end
getReceipt = w() . end
compute = u() . v() . w[] . end

buy = x[u] . (putName | x[v] . (putCredit | x(w) . x() . getReceipt))
sell = x(u) . x(v) . x[w] . (compute | x[] . end)

buySell = \ x . (buy | sell)

getPrice = v() . end
lookup = u() . v[] . end

shop = x[u] . (putName | x(v) . getPrice)
quote = x(u) . x[v] . (lookup | x[] . end)
