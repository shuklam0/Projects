library(words)
library(tidyverse)

`%!in%` <- Negate(`%in%`)

W = words::words
W$word = W$word %>% str_to_upper

L = 4       # min length of words
W = W[W$word_length>=L,]

X0 = LETTERS %>% str_to_upper
X1 = c('L', 'I', 'E', 'V', 'N', 'U') %>% str_to_upper
X2 = 'P' %>% str_to_upper
X3 = X0[which(LETTERS %!in% c(X1,X2))]

W = W[!grepl(paste0(X3, collapse = '|'), W$word, ignore.case = TRUE),]
W = W[grepl(X2, W$word, ignore.case = TRUE),]

W$word

spelling_bee <- function(L=4){
  `%!in%` <- Negate(`%in%`)
  
  W = words::words
  W$word = W$word %>% str_to_upper
  W = W[W$word_length>=L,]
  
  X0 = LETTERS %>% str_to_upper
  
  X1 = readline(prompt = "Enter Surrounding Letters: " ) %>% 
    str_replace_all("[^[:alnum:]]", "") %>% 
    str_split(pattern = "") %>% 
    unlist %>% 
    str_to_upper %>% 
    unique %>% 
    sort
  
  L1 = 6 - length(X1)
  while(L1>0) {
    x = readline(paste0(L1, " Letters missing, Enter ", L1, " more Letter(s): "))
    X1 = c(X1, x) %>% 
      str_replace_all("[^[:alnum:]]", "") %>% 
      str_split(pattern = "") %>% 
      unlist %>% 
      str_to_upper %>% 
      unique %>% 
      sort
    L1 = 6 - length(X1)
    }
     
  
  X2 = readline(prompt = "Enter Central Letter: " ) 
  
  
  X3 = LETTERS[which(LETTERS %!in% c(X1,X2))]
  
  W = W[!grepl(paste0(X3, collapse = '|'), W$word, ignore.case = TRUE),]
  W = W[grepl(X2, W$word, ignore.case = TRUE),]
  
  W$word
}
