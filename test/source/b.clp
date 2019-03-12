zero = x(A).x(s).x(z).z<->x

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

selectBuy = x[inl] . buy
selectShop = x[inr] . shop
choice = x.case(sell, quote)

runBuy = \ x . (selectBuy | choice)
runShop = \ x . (selectShop | choice)

-- client = ?a[y] . selectBuy | ?a[y] . selectShop
-- server = !a(y) . choice