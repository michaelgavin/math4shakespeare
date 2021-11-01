# Create term-document matrix
fnames = dir()
# Build vocab
TXT = c()
for (f in 1:length(fnames)) {
  play = read_html(fnames[f])
  nodes = xml_find_all(play, ".//*")
  ord = which(xml_name(nodes) == "sp")
  speeches = nodes[ord]
  
  for (i in 1:length(speeches)) {
    print(paste(f, "speech", i, "of", length(speeches)))
    speech = speeches[i]
    speech = xml_children(speech)
    ord = which(xml_name(speech) == "speaker")
    speech = speech[-ord]
    txt = paste(xml_text(xml_find_all(speech, "w")), sep = " ")
    txt = tolower(txt)
    TXT = c(TXT, txt)
  }
}
vocab = names(sort(table(TXT), decreasing = T))

P = matrix(0, length(vocab), length(fnames)) 
rownames(P) = names(vocab)
for (f in 1:length(fnames)) {
  print(f)
  play = read_html(fnames[f])
  nodes = xml_find_all(play, ".//*")
  ord = which(xml_name(nodes) == "sp")
  speeches = nodes[ord]
  
  for (i in 1:length(speeches)) {
    print(paste(f, "speech", i, "of", length(speeches)))
    speech = speeches[i]
    speech = xml_children(speech)
    ord = which(xml_name(speech) == "speaker")
    speech = speech[-ord]
    txt = paste(xml_text(xml_find_all(speech, "w")), sep = " ")
    txt = tolower(txt)
    freqs = table(txt)
    P[names(freqs), f] = P[names(freqs), f] + freqs
  }
}
save(P, file = "~/Desktop/litmath/P.rda")

# Now build word-speech matrix
library(Matrix)
PLAY = c()
ACT = c()
SCENE = c()
CHARACTER = c()
for (f in 1:length(fnames)) {
  play = read_html(fnames[f])
  nodes = xml_find_all(play, ".//*")
  ord = which(xml_name(nodes) == "sp")
  speeches = nodes[ord]
  m = Matrix(0, length(vocab), length(speeches))
  rownames(m) = vocab
  for (i in 1:length(speeches)) {
    print(paste(f, "speech", i, "of", length(speeches)))
    PLAY = c(PLAY, gsub(".xml","",fnames[f]))
    
    speech = speeches[i]
    
    CHARACTER = c(CHARACTER, xml_attr(speech, "who"))
    
    speech = xml_children(speech)
    ord = which(xml_name(speech) == "speaker")
    speech = speech[-ord]
    
    first_word = xml_find_first(speech, "w")
    n = xml_attr(first_word, attr = "n")[1]
    ACT = c(ACT, substring(n, 1, gregexpr("\\.", n)[[1]][1] - 1))
    SCENE = c(SCENE, substring(n, gregexpr("\\.", n)[[1]][1] + 1, gregexpr("\\.", n)[[1]][2]-1))
    
    txt = paste(xml_text(xml_find_all(speech, "w")), sep = " ")
    txt = tolower(txt)
    freqs = table(txt)
    
    vec = rep(0, length(vocab))
    names(vec) = vocab
    vec[names(freqs)] = freqs
    m[, i] = vec
  }
  
  if (f == 1) {
    S = m
  } else {
    print("... binding matrices ...")
    S = cbind(S, m)
  }
}  
save(S, file = "~/Desktop/litmath/S.rda")
