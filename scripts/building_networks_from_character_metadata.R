character_metadata = read.csv("~/Downloads/Character Metadata - Sheet1.csv", header = T, stringsAsFactors = F)
relationships = colnames(character_metadata)[c(9:13,16)]

SOURCE = c()
TARGET = c()
RELATIONSHIP = c()
PLAY = c()
for (i in 1:length(relationships)) {
  
  relationship = relationships[i]
  
  hits = which(character_metadata[,relationship] != "")

  selection = character_metadata[hits, c("Character.ID",relationship,"Play.ID")]
  SOURCE = c(SOURCE, selection[,1])
  TARGET = c(TARGET, selection[,2])
  PLAY = c(PLAY, selection[,3])
  RELATIONSHIP = c(RELATIONSHIP, rep(relationship, length(hits)))
}
edge_list = cbind(SOURCE, TARGET, PLAY, RELATIONSHIP)

hits = grep(";",edge_list[,2])
sub_edges = edge_list[hits,]
edge_list = edge_list[-hits,]
SOURCE = c()
TARGET = c()
RELATIONSHIP = c()
PLAY = c()
for (i in 1:nrow(sub_edges)) {
  targs = unlist(strsplit(sub_edges[i,2], split = ";"))
  
  SOURCE = c(SOURCE, rep(sub_edges[i,1], length(targs)))
  TARGET = c(TARGET, targs)
  RELATIONSHIP = c(RELATIONSHIP, rep(sub_edges[i,4], length(targs)))
  PLAY = c(PLAY, rep(sub_edges[i,3], length(targs)))
}
new_edges = cbind(SOURCE, TARGET, PLAY, RELATIONSHIP)
edge_list = rbind(edge_list, new_edges)

library(igraph)
g = graph_from_edgelist(el = edge_list[,1:2])
E(g)$play = edge_list[,3]
E(g)$relationship = edge_list[,4]

cdata = character_metadata[!duplicated(character_metadata$Character.ID),c("Character.ID","Name","Gender","Age","Status")]
rownames(cdata) = cdata$Character.ID
cdata$Gender[cdata$Gender == "Male"] = "M"

hit = which(V(g)$name %in% rownames(cdata) == F)
g = delete_vertices(g, hit)

V(g)$id = cdata[V(g)$name, "Character.ID"]
V(g)$name = cdata[V(g)$name, "Name"]
V(g)$gender = cdata[V(g)$id, "Gender"]
V(g)$status = cdata[V(g)$id, "Status"]
V(g)$color = as.factor(V(g)$gender)
V(g)$size = 5
save(g, file = "network_by_relationship.rda")


# By speech
scene_labels = paste(speech_metadata$PLAY, speech_metadata$ACT, speech_metadata$SCENE, sep = "_")
SOURCE = c()
TARGET = c()
SPEECH_NUM = c()
scenes = unique(scene_labels)
speech_metadata$CHARACTER = as.character(speech_metadata$CHARACTER)
for (i in 1:length(scenes)) {
  scene = scenes[i]
  hits = which(scene_labels == scene)
  if (length(hits) < 2) next
  source_rows = hits[1:(length(hits) - 1)]
  target_rows = hits[2:length(hits)]
  source_nodes = speech_metadata[source_rows, "CHARACTER"]
  target_nodes = speech_metadata[target_rows, "CHARACTER"]
  SOURCE = c(SOURCE, source_nodes)
  TARGET = c(TARGET, target_nodes)
  SPEECH_NUM = c(SPEECH_NUM, source_rows)
}
edge_list = cbind(SOURCE, TARGET, SPEECH_NUM)
edge_list = edge_list[!is.na(edge_list[,"SOURCE"]),]
edge_list = edge_list[!is.na(edge_list[,"TARGET"]),]

g = graph_from_edgelist(el = edge_list[,1:2])
V(g)$id = cdata[V(g)$name, "Character.ID"]
V(g)$name = cdata[V(g)$name, "Name"]
V(g)$gender = cdata[V(g)$id, "Gender"]
V(g)$status = cdata[V(g)$id, "Status"]
V(g)$color = as.factor(V(g)$gender)
V(g)$size = 5
E(g)$speech_num = edge_list[,"SPEECH_NUM"]
E(g)$weight = 1
save(g, file = "network_by_speeches.rda")
