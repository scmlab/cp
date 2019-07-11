zero = x(X).x(s).x(z).z<->x
one = x(X).x(s).x(z).?s[f].f[a].(a<->z|f<->x)


putName = u[] . end
putCredit = v[] . end
getReceipt = w() . end
compute = u() . v() . w[] . end

buy1 = x() . getReceipt
buy2 = x(w) . x() . getReceipt
buy3 = x[v] . (putCredit | x(w) . x() . getReceipt)
buy = x[u] . (putName | x[v] . (putCredit | x(w) . x() . getReceipt))
sell = x(u) . x(v) . x[w] . (compute | x[] . end)
-- runBuy = \ x . (buy | sell)


getPrice = v() . x() . end
lookup = u() . v[] . end

shop = x[u] . (putName | x(v) . getPrice)
quote = x(u) . x[v] . (lookup | x[] . end)
runShopQuote = \x . (shop | quote)


selectBuy = x[inl] . buy
selectShop = x[inr] . shop
choice = x.case(sell, quote)


-- runSelectBuyChoice = \x . (selectBuy | choice)
-- runSelectShopChoice = \x . (selectShop | choice)


-- a = ?z[y] . selectBuy
-- b = ?z[y] . selectShop
-- client = a | b
