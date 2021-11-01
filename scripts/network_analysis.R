#### Working with network by speech ####
# Load data and source functions
library(igraph)
library(Matrix)
load("~/Desktop/network_by_speech.rda")
load("~/Desktop/character_metadata.rda")
load("~/Downloads/litmath/data/speech_metadata.rda")
load("~/Downloads/litmath/data/play_metadata.rda")
load("~/Downloads/litmath/data/S.rda")
source("~/Downloads/litmath/scripts/stats_functions.R")

##### Selecting a play and inducing a subgraph #####
ids = character_metadata$Character.ID[character_metadata$Play.ID == "H5"]
hits = which(V(g)$id %in% ids)
subg = induced_subgraph(g, vids = hits)

#### Plotting the graph by speeches ####
g_simple = simplify(subg, edge.attr.comb = list(weight = "sum"))
widths = E(g_simple)$weight / 10
plot(g_simple, edge.arrow.size = 0, edge.width = widths)

#### Community Detection ####
wt = walktrap.community(subg)
V(g_simple)$color = as.factor(wt$membership)
plot(g_simple, edge.arrow.size = 0, edge.width = widths)

# To analyze speeches by community
m = matrix(0,nrow(S),max(wt$membership))
rownames(m) = rownames(S)
for (i in 1:ncol(m)) {
  print(i)
  char_ids = V(subg)$id[wt$membership == i]
  hits = which(speech_metadata$CHARACTER %in% char_ids)
  totals = rowSums(S[, hits])
  m[,i] = totals
}
doc_freqs = apply(m, 1, function(x) length(x[x>0]))
m = m[which(doc_freqs > 1),]
Z = apply(m, 1, zscore)
Z = t(Z)

# Make table of top words by community
df = data.frame(1:25)
for (i in 1:ncol(Z)) {
  vec = sort(Z[,i], decreasing = T)[1:25]
  df = cbind(df, names(vec), vec)
  colnames(df)[(ncol(df)-1):ncol(df)] = i
}
View(df)

# Get incident edges for each node -- "in" example
# NOTE: This only works with network by speeches
m = matrix(0, nrow(S), vcount(subg))
colnames(m) = V(subg)$id
rownames(m) = rownames(S)
for (i in 1:ncol(m)) {
  edges = incident(subg, v = i, mode = c("in"))
  hits = as.numeric(E(subg)$speech_num[edges])
  totals = rowSums(S[, hits])
  m[ ,i] = totals
}
doc_freqs = apply(m, 1, function(x) length(x[x>0]))
m = m[which(doc_freqs > 1),]
Z = apply(m, 1, zscore)
Z = t(Z)

# Make table of top words by character
df = data.frame(1:25)
for (i in 1:ncol(Z)) {
  vec = sort(Z[,i], decreasing = T)[1:25]
  df = cbind(df, names(vec), vec)
  colnames(df)[(ncol(df)-1):ncol(df)] = colnames(Z)[i]
}
View(df)


##### Centrality Measures #####

# Degree
d = degree(subg, mode = "total")

# Betweenness #
btw = betweenness(subg, directed = F)

# Eigencentrality
evc = eigen_centrality(subg, directed = F)

# Pagerank
pr = page_rank(subg, directed = T)

# Eigencentrality over betweenness
toadies = evc$vector / d
names(toadies) = V(subg)$name
toadies = toadies[names(toadies) != ""]

# Betweenness over degree
bridges = btw / d
names(bridges) = V(subg)$name
bridges = bridges[names(bridges) != ""]

#### Clustering, Modularity, and Similarity ####

# Clustering
trans = transitivity(subg, type = "local") # For each node
names(trans) = V(subg)$id

trans = transitivity(subg, type = "global") # For the graph as a whole

# Modularity by gender
hits = which(V(subg)$gender == "")
subg_named = delete_vertices(subg, hits)
groups = as.factor(V(subg_named)$gender)
mod = modularity(as.undirected(subg_named, mode = "each"), membership = groups, weights = E(subg)$weight)

# Modularity by status
hits = which(V(subg)$status == "")
subg_named = delete_vertices(subg, hits)
groups = as.factor(V(subg_named)$status)
mod = modularity(as.undirected(subg_named), membership = groups, weights = E(subg)$weight)


# Similarity
A = get.adjacency(as.undirected(subg, mode = "each"))
A = A[rownames(A) != "",rownames(A) != ""]
most_similar(A, "Benedick")


##### Tips for visualization #####

# For comparing nodes, sorted barplots often work
d = degree(subg, mode = "total")
par(las = 2, mar=c(5,8,4,2)) # Orients labels and sizes the margins
barplot(sort(d), horiz = T)

# For comparing variables, use a scatterplot
d = degree(subg, mode = "total")
btw = betweenness(subg, directed = F)
hits = which(V(subg)$name != "")
plot(d[hits],btw[hits], cex = 0)
text(d[hits],btw[hits], labels = V(subg)$name[hits])

# For comparing groups, try boxplots
groups = as.factor(V(subg)$gender)
d = degree(subg, mode = "total")
boxplot(d ~ groups)

groups = wt$membership
boxplot(d ~ groups)

# To compile data across all plays
load("~/Desktop/network_by_speech.rda")
play_ids = levels(play_metadata$ID)
vec = c()
for (i in 1:length(play_ids)) {
  play = play_ids[i]
  ids = character_metadata$Character.ID[character_metadata$Play.ID == play]
  hits = which(V(g)$id %in% ids)
  subg = induced_subgraph(g, vids = hits)
  
  # Calculate degree (substitute whatever)
  #d = degree(subg, mode = "total")
  #mean_degree = mean(d)
  
  # Modularity by gender 
  hits = which(V(subg)$gender == "")
  subg_named = delete_vertices(subg, hits)
  groups = as.factor(V(subg_named)$gender)
  mod = modularity(as.undirected(subg_named), membership = groups, weights = E(subg)$weight)
  
  # trans = transitivity(subg, type = "global")
  
  # mixing = assortativity_degree(subg)
  
  # Compile into a list
  vec = c(vec, mod)
}
names(vec) = play_ids

par(las = 2, mar=c(3,4,0.5,1)) # Orients labels and sizes the margins
barplot(sort(vec), horiz = T)
