load("booktrade_network.rda")

wt = walktrap.community(g)
top_communities = sort(table(wt$membership), decreasing = T)[1:5]
top_communities = names(top_communities)
grep("Shakespeare", wt$names)

d = degree(g)
btw = betweenness(g)
hits = which(wt$membership == top_communities[3])
sort(d[hits], decreasing = T)[1:10]
subg = induced_subgraph(g, hits)
plot(subg)
ids = E(subg)$id
ids = unique(ids)

most_similar(E, "husband")
most_similar(E[,ids], "husband")

shakes = ego(g, nodes = 1482)
vids = shakes[[1]]
vids = vids[2:length(vids)]
subg = induced_subgraph(g, vids = vids)
plot(subg)

g_simple = simplify(subg, edge.attr.comb = list(weight = "sum"))
widths = E(g_simple)$weight / 10
plot(g_simple, edge.arrow.size = 0, edge.width = widths)

ids = E(subg)$id
ids = unique(ids)
most_similar(E[,ids], "husband")

lsa = svd(E[,ids], nu = 50)
US = lsa$u %*% diag(lsa$d[1:50])
rownames(US) = rownames(E)
