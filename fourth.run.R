library(tm)
library(stringi)
library(dplyr)
library(rJava)
library(RWeka)
library(NLP)
library(openNLP)
library(qdap)
library(ggplot2)
library(doParallel)
registerDoParallel(cores = 4)



sentence <- function(corpus) {
      sentence.annotator <- Maxent_Sent_Token_Annotator(language = "en")
      corpus <- as.String(unlist(corpus))
      bounds <- NLP::annotate(corpus, sentence.annotator)
      corpus <- corpus[bounds]
      corpus <- gsub("[^a-zA-Z\ ]", "",corpus)
      corpus <- gsub("^", "<s> ", corpus)
      corpus <<- gsub("$", " <s>", corpus)
      return(corpus)
      #    vc <<- VCorpus(VectorSource(PlainTextDocument(corpus)))
}

clean.input <- function(input) {
      string <- gsub("^", "<s> ", input)
      string <- tolower(string)
      string <- unlist(stri_split_fixed(string, " "))
      string <- gsub("[^a-zA-Z\ (^<s>)]", "", string)
      string <- gsub("\ +", " ", string)
      input.string <<- string
}



gram.df <- function(){
      tokens <- names(uni.sum3)
      bi.tokens <- names(bi.sum3)
      tri.tokens <- names(tri.sum3)
      quad.tokens <- names(quad.sum3)
      
      
      
      bi.tok <<- lapply(bi.tokens, function(x) {stri_split_fixed(x, pattern= " ")})
      df.bi <<- data.frame(matrix(unlist(bi.tok), ncol = 2, byrow = TRUE))
      
      tri.tok <<- lapply(tri.tokens, function(x) {stri_split_fixed(x, pattern= " ")})
      df.tri <<- data.frame(matrix(unlist(tri.tok), ncol = 3, byrow = TRUE))
      
      quad.tok <<- lapply(quad.tokens, function(x) {stri_split_fixed(x, pattern= " ")})
      df.quad <<- data.frame(matrix(unlist(quad.tok), ncol = 4, byrow = TRUE))
      
}



prep <- function(input) {
      string <<- clean.input(input)
      num <<- length(string)
      delta <<- .5
      if (string[num] == "") {
            string <<- string[-num]
            num <<- length(string)
      }
      if (num >= 3) {
            w1 <<- string[num]
            w2 <<- string[num-1]
            w3 <<- string[num-2]
      } else if (num == 2) {
            w1 <<- string[num]
            w2 <<- string[num-1]
      } else if (num == 1) {
            w1 <<- string[num]
      } else {
            print("Please input 3 or more words")
            break 
      }
      quad.w <<- filter(t.quad, term1 == w3, term2 == w2, term3 == w1)
      tri.w <<- filter(t.tri, term1 == w2, term2 == w1)
      bi.w <<- filter(t.bi, term1 == w1)
      bi.choices <<- as.character(head(arrange(bi.w, desc(count))$term2, max(5, length(bi.w))))
      #      bi.choices <<- bi.choices[-grep("<s>", bi.choices)]
      tri.choices <<- c(as.character(head(arrange(tri.w, desc(count))$term3, max(6, length(tri.w)))), bi.choices)
      #      tri.choices <<- bi.choices[-grep("<s>", tri.choices)]
      quad.choices <<- c(as.character(head(arrange(quad.w, desc(count))$term4, max(10, length(quad.w)))), tri.choices)
      if ("<s>" %in% quad.choices) {
            quad.choices <<- quad.choices[-grep("<s>", quad.choices)]
      }
      #      choices <<- c(quad.choices, tri.choices, bi.choices)
}





pkn.bi <- function(input, word){
      #      string <<- unlist(clean.input(input))
      #      num <<- length(string)
      #      w1 <<- string[num]
      #      w2 <<- string[num-1]
      #      w3 <<- string[num-2]
      #      bi.w <<- filter(t.bi, term1 == w1)
      bi.wi <<- filter(t.bi, term2 == word)
      pc.wi <<- nrow(bi.wi)/nrow(t.bi)
      #      delta <<- .5
      lambda.bi <<- (delta / sum(bi.w$count)) * nrow(bi.w)
      if (nrow(bi.wi) == 0) {
            pkn.bi.wi <<- lambda.bi * pc.wi
      } else {
            pkn.bi.wi <<- (max(c(0, sum(filter(bi.wi, term1 == w1)$count)-delta))) / 
                  sum(bi.w$count) + lambda.bi * pc.wi
      }
      #      return(pkn.bi.wi)
}

pkn.tri <- function(input, word){
      #      string <- unlist(clean.input(input))
      #      num <- length(string)
      #      w1 <- string[num]
      #      w2 <- string[num-1]
      #      tri.w <<- filter(t.tri, term1 == w2, term2 == w1)
      tri.wi <<- filter(t.tri, term3 == word)
      #      delta <- .5
      lambda.tri <<- (delta / sum(tri.w$count))*nrow(tri.w)
      if (nrow(tri.wi) == 0) {
            pkn.tri.wi <<- lambda.tri * pkn.bi.wi
      } else {
            pkn.tri.wi <<- (max(c(0, sum(filter(tri.w, term3 == word)$count)-delta)))/ 
                  sum(tri.w$count) + 
                  lambda.tri*pkn.bi(input, word)
      }
      
      #      return(pkn.tri.wi)
}


pkn.quad <- function(input, word){
      #      string <- unlist(clean.input(input))
      #      num <- length(string)
      #      w1 <- string[num]
      #      w2 <- string[num-1]
      #      w3 <- string[num-2]
      #      quad.w <<- filter(t.quad, term1 == w3, term2 == w2, term3 == w1)
      quad.wi <<- filter(t.quad, term4 == word)
      #      delta <- .5
      
      lambda.quad <<- (delta / sum(quad.w$count))*nrow(quad.w)
      
      if (sum(filter(quad.w, term4 == word)$count) != 0 & sum(filter(quad.w, term4 == word)$count) !=0) {
            pkn.quad.wi <<- (max(c(0, sum(filter(quad.w, term4 == word)$count)-delta)))/ 
                  sum(quad.w$count) + 
                  lambda.quad*pkn.tri(input, word)
      } else if (is.na(pkn.tri.wi)) {
            pkn.quad.wi <<- lambda.quad*(pkn.bi.wi^2)
      } else {
            pkn.quad.wi <<- lambda.quad*pkn.tri.wi
      }
      
      return(pkn.quad.wi)
      
}


pk <- function(input) {
      prep(input)
      
      probs <- NULL
      if (num >= 4) {
            for (i in 1:length(quad.choices)) {
                  probs[i] <- pkn.quad(input, quad.choices[i])
            }
            prediction <<- quad.choices[which.max(probs)]
      } else if (num == 3) {
            for (i in 1:length(tri.choices)) {
                  probs[i] <- pkn.tri(input, tri.choices[i])
            }
            prediction <<- tri.choices[which.max(probs)]
      } else if (num == 2) {
            for (i in 1:length(bi.choices)) {
                  probs[i] <- pkn.bi(input, bi.choices[i])
            }
            prediction <<- bi.choices[which.max(probs)]
      } else {
            break
      }
      return(prediction)
}


set.seed(1111)

setwd("~/Projects/Capstone/en_US")

blog <- file("en_US.blogs.txt")
blogs <- iconv(readLines(blog, encoding = "UTF-8"), "UTF-8", "ASCII")
close(blog)
rm(blog)

twit <- file("en_US.twitter.txt")
twitter <- readLines(twit, encoding = "ASCII")
close(twit)
rm(twit)

new <- file("en_US.blogs.txt", "rb")
news <- readLines(new, encoding = "UTF-8")
close(new)
rm(new)


blogs1 <- blogs[15001:35000]
twitter1 <- twitter[15001:35000]
news1 <- news[15001:35000]

rm(twitter, news, blogs)

sam <- c(blogs1, twitter1, news1)
sam <- sam[!is.na(sam)]
sam <- sentence(sam)
sam <- gsub("[^a-zA-Z\ (^<s>)]", "", sam)
sam <- gsub("\ +", " ", sam)
sam <- tolower(sam)

rm(twitter1, news1, blogs1)
gc()

unigramizer <- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=1, max=1))
bigramizer <- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=2, max=2))
trigramizer <- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=3, max=3))
quadgramizer <- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=4, max=4))

uni3 <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = unigramizer, wordLengths=c(1,Inf)))
gc()
bi3 <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = bigramizer, wordLengths=c(1,Inf)))
gc()
tri3 <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = trigramizer,wordLengths=c(1,Inf)))
gc()
quad3 <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = quadgramizer, wordLengths=c(1,Inf)))
gc()

uni.sum3 <- apply(uni3, 1, sum)
bi.sum3 <- apply(bi3, 1, sum)
tri.sum3 <- apply(tri3, 1, sum)
quad.sum3 <- apply(quad3, 1, sum)

gc()

gram.df()

gc()

names(df.bi) <- c("term1", "term2")
names(df.tri) <- c("term1", "term2", "term3")
names(df.quad) <- c("term1", "term2", "term3", "term4")
t.bi <- select(df.bi, term1, term2)
t.bi <- mutate(t.bi, count = as.numeric(bi.sum3))

t.tri <- select(df.tri, term1, term2, term3)
t.tri <- mutate(t.tri, count = as.numeric(tri.sum3))

t.quad<- select(df.quad, term1, term2, term3, term4)
t.quad <- mutate(t.quad, count = as.numeric(quad.sum3))
