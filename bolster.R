bolster <- function(start, end) {
      
      if (!file.exists("~/Projects/Capstone/tdm.data/uni.csv")){
            uni.starter <- data.frame(terms = "<s>", sum = 0)
            write.table(uni.starter, file = "~/Projects/Capstone/tdm.data/uni.csv",
                        row.names = FALSE, sep = ",")
      }
      if (!file.exists("~/Projects/Capstone/tdm.data/bi.csv")){
            bi.starter <- data.frame(terms = "<s> <s>", sum = 0)
            write.table(bi.starter, file = "~/Projects/Capstone/tdm.data/bi.csv",
                      row.names = FALSE, sep = ",")
      }
      if (!file.exists("~/Projects/Capstone/tdm.data/tri.csv")){
            tri.starter <- data.frame(terms = "<s> <s> <s>", sum = 0)
            write.table(tri.starter, file = "~/Projects/Capstone/tdm.data/tri.csv",
                      row.names = FALSE, sep = ",")
      }
      if (!file.exists("~/Projects/Capstone/tdm.data/quad.csv")){
            quad.starter <- data.frame(terms = "<s> <s> <s> <s>", sum = 0)
            write.table(quad.starter, file = "~/Projects/Capstone/tdm.data/quad.csv",
                      row.names = FALSE, sep = ",")
      }
      
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
      uni <- as.data.frame(inspect(uni))
      uni$terms <- rownames(uni)
      rownames(uni) <- NULL
      names(uni) <- c("sum", "terms")
      
      uni1 <- read.table("~/Projects/Capstone/tdm.data/uni.csv", 
                         header = TRUE, sep = ",")
      uni <- merge(uni, uni1, all = TRUE, by = "terms")
      uni[is.na(uni)] <- 0
      uni <- mutate(uni, sum = sum.x + sum.y)
      uni <- select(uni, terms, sum)
      
      write.table(uni, file = "~/Projects/Capstone/tdm.data/uni.csv", sep = ",")
      
      gc()
      
      
      
      bi <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = bigramizer, wordLengths=c(1,Inf)))
      bi <- as.data.frame(inspect(bi))
      bi$terms <- rownames(bi)
      rownames(bi) <- NULL
      names(bi) <- c("sum", "terms")
      
      bi1 <- read.table("~/Projects/Capstone/tdm.data/bi.csv", 
                         header = TRUE, sep = ",")
      bi <- merge(bi, bi1, all = TRUE, by = "terms")
      bi[is.na(bi)] <- 0
      bi <- mutate(bi, sum = sum.x + sum.y)
      bi <- select(bi, terms, sum)
      
      write.table(bi, file = "~/Projects/Capstone/tdm.data/bi.csv", sep = ",")
      
      gc()
      
      
      
      tri <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = trigramizer,wordLengths=c(1,Inf)))
      tri <- as.data.frame(inspect(tri))
      tri$terms <- rownames(tri)
      rownames(tri) <- NULL
      names(tri) <- c("sum", "terms")
      
      tri1 <- read.table("~/Projects/Capstone/tdm.data/tri.csv", 
                         header = TRUE, sep = ",")
      tri <- merge(tri, tri1, all = TRUE, by = "terms")
      tri[is.na(tri)] <- 0
      tri <- mutate(tri, sum = sum.x + sum.y)
      tri <- select(tri, terms, sum)
      
      write.table(tri, file = "~/Projects/Capstone/tdm.data/tri.csv", sep = ",")
      
      gc()
      
      
      
      quad <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = quadgramizer, wordLengths=c(1,Inf)))
      quad <- as.data.frame(inspect(quad))
      quad$terms <- rownames(quad)
      rownames(quad) <- NULL
      names(quad) <- c("sum", "terms")
      
      quad1 <- read.table("~/Projects/Capstone/tdm.data/quad.csv", 
                         header = TRUE, sep = ",")
      quad <- merge(quad, quad1, all = TRUE, by = "terms")
      quad[is.na(quad)] <- 0
      quad <- mutate(quad, sum = sum.x + sum.y)
      quad <- select(quad, terms, sum)
      
      write.table(quad, file = "~/Projects/Capstone/tdm.data/quad.csv", sep = ",")
      
      gc()
}