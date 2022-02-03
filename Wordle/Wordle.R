# library(qdapDictionaries)
# library(data.table)
library(tidyverse)
# library(arrangements)
library(words)
# library(stringr)

# "towns"  %in% DICTIONARY$word
# "towns" %in% words$word


# letters
# words
Wrds = as.vector(words[words$word_length==5,]$word)

Wordle_remove <- function(Wrds = words, l_rem){
  Wrds[!grepl(l_rem, Wrds, ignore.case = TRUE, )]
}

Wordle_keep <- function(Wrds = words, l_keep){
  ch = str_split(l_keep,'')[[1]]
  df = Wrds
  for (i in ch){
    df = df[grepl(i, df, ignore.case = TRUE)]
  }
  df
}

Wordle_pos <- function(Wrds = words, ...){
  x = list(...)
  i = 1
  while (i < length(x)){
    Wrds = Wrds[substr(Wrds, x[i+1], x[i+1])==x[i]]
    i=i+2
  }
  Wrds
}

Wordle_pos_not <- function(Wrds = words, ...){
  x = list(...)
  i = 1
  while (i < length(x)){
    Wrds = Wrds[substr(Wrds, x[i+1], x[i+1])!=x[i]]
    i=i+2
  }
  Wrds
}

W0 = Wrds
W0 = Wordle_keep(W0, 'a|s|r')
W0 = Wordle_remove(W0, 'b|o|u|t|p|i|e|c|f|k')
W0 = Wordle_pos_not(W0, 'a', 1)
W0 = Wordle_pos(W0, 's', 1, 'r', 4, 'a', 3, 'h', 2)
W0



################# Rough 

d_sum <- function(...){
  x <- list(...) # THIS WILL BE A LIST STORING EVERYTHING:
  print(length(x))       # Example of inbuilt function
}

d_sum(24,52,5,92)


Wordle_pos_yes <- function(Wrds, ...){
  x = list(...)
  i = 1
  while (i < length(x)){
    Wrds = Wrds[substr(Wrds, x[i+1], x[i+1])==x[i]]
    i=i+2
  }
  Wrds
}

W0 = Wordle_pos_yes(W0, 'a', 1, 'b', 2)


X = c('a','b','o','u','t')
paste0(X, collapse = '|')
