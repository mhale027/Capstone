bolster <- function(start, end) {
      blogs1 <- blogs[start:end]
      news1 <- news[start:end]
      twitter1 <- twitter[start:end]
      
      sam <- c(blogs1, twitter1, news1)
      sam <- sam[!is.na(sam)]
      sam <- sentence(sam)
      sam <- gsub("[^a-zA-Z\ (^<s>)]", "", sam)
      sam <- gsub("\ +", " ", sam)
      sam <- tolower(sam)
      
      rm(twitter1, news1, blogs1)
      
      unigramizer <<- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=1, max=1))
      bigramizer <<- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=2, max=2))
      trigramizer <<- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=3, max=3))
      quadgramizer <<- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=4, max=4))
      
      
      uni <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = unigramizer, wordLengths=c(1,Inf)))
      load("~/Projects/Capstone/tdm.data/tot.RData")
      tots[[1]] <- c(tots[[1]] + uni)
      save(tots, file = "~/Projects/Capstone/tdm.data/tot.RData")
      rm(tots)
      gc()
      
      
      
      bi <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = bigramizer, wordLengths=c(1,Inf)))
      load("~/Projects/Capstone/tdm.data/tot.RData")
      tots[[2]] <- c(tots[[2]] + bi)
      save(tots, file = "~/Projects/Capstone/tdm.data/tot.RData")
      rm(tots)
      gc()
      
      
      
      tri <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = trigramizer,wordLengths=c(1,Inf)))
      load("~/Projects/Capstone/tdm.data/tot.RData")
      tots[[3]] <- c(tots[[3]] + tri)
      save(tots, file = "~/Projects/Capstone/tdm.data/tot.RData")
      rm(tots)
      gc()
      
      
      
      quad <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = quadgramizer, wordLengths=c(1,Inf)))
      load("~/Projects/Capstone/tdm.data/tot.RData")
      tots[[4]] <- c(tots[[4]] + quad)
      save(tots, file = "~/Projects/Capstone/tdm.data/tot.RData")
      rm(tots)
}