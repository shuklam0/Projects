library(words)
library(tidyverse)

`%!in%` <- Negate(`%in%`)
L=3 # min length

W = words::words
W$word = W$word %>% str_to_upper
W = W[W$word_length>=max(L,1),1] %>% unlist

L1 = c('E', 'T', 'R')
L2 = c('C', 'F', 'A')
L3 = c('K', 'N', 'M')
L4 = c('I', 'U', 'W')
L5 = c(L1,L2,L3,L4)
L6 = LETTERS[which(LETTERS %!in% L5)]

W = W[!grepl(paste0(L6, collapse = '|'), W, ignore.case = TRUE)]

W1 = c()
for (Wrd in W) {
  X = strsplit(Wrd,"") %>% unlist
  Y = TRUE
  Z = (match(X,L5)-1) %/% 3
  
  for (i in 2:length(Z)) {
    if (Z[i-1] == Z[i]) {
      Y = FALSE
      break
    }
  }
  
  if (Y) {
    W1 = c(W1,Wrd)
  }
}

W11 = rev(W1)[2]
L11 = L5[which(L5 %!in% (strsplit(W11,"") %>% unlist))]
L11 = c(str_sub(W11, start=-1), L11)
# L12 = LETTERS[which(LETTERS %!in% L11)]
W12 = W1[grepl(paste0(L11, collapse = '|'), W1, ignore.case = TRUE)]
W13 = W12
for (i in L11){
  W13 = W13[grepl(i, W12, ignore.case = TRUE)]
}
rev(W13)

# X = strsplit(W[3088],"") %>% unlist
X = strsplit(W1[558],"") %>% unlist
Z = (match(X,L5)-1) %/% 3
Z

LB1 = function(Wrd, Lx, Ly=3) {
  X = strsplit(Wrd,"") %>% unlist
  Y = TRUE
  Z = (match(X,Lx)-1) %/% Ly
  
  for (i in 2:length(Z)) {
    if (Z[i-1] == Z[i]) {
      Y = FALSE
      break
    }
  }
  
  Y
}

# Number of unique characters in word
Unq_chr = function(Wrd){
  W1 = str_split(Wrd, "") %>% unlist
  W2 = unique(W1)
  length(W2)
}
WL1 = lapply(W1, Unq_chr) %>% unlist
WL2 = lapply(W1, str_length) %>% unlist

Wx = data.table::data.table(W1, WL1, WL2)
Wx[, Wr := WL1/WL2]

Wx = Wx[order(WL1, Wr, decreasing = TRUE),]
Wx

