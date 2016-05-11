library(tm)
library(stringi)
library(dplyr)
library(RWeka)
library(NLP)
library(openNLP)
library(qdap)
library(ggplot2)

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


blogs1 <- blogs[as.logical(rbinom(length(blogs), 1, prob = .001))]
twitter1 <- twitter[as.logical(rbinom(length(twitter), 1, prob = .001))]
news1 <- news[as.logical(rbinom(length(news), 1, prob = .001))]


sam <- c(blogs1, twitter1, news1)
sam <- gsub("[^a-zA-Z\ ]", "", sam)
sam <- gsub("\ +", " ", sam)
sam <- tolower(sam)

vc <- VCorpus(VectorSource(sam))

vc <- tm_map(vc, FUN=stripWhitespace)
vc <- tm_map(vc, FUN=tolower)
vc <- tm_map(vc, FUN=removePunctuation)
vc <- tm_map(vc, FUN=PlainTextDocument)


unigramizer <- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=1, max=1))
bigramizer <- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=2, max=2))
trigramizer <- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=3, max=3))
quadgramizer <- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=4, max=4))

uni <- TermDocumentMatrix(as.Corpus(sam), control = list(tokenize = unigramizer, wordLengths=c(1,Inf)))
bi <- TermDocumentMatrix(as.Corpus(sam), control = list(tokenize = bigramizer, wordLengths=c(1,Inf)))
tri <- TermDocumentMatrix(as.Corpus(sam), control = list(tokenize = trigramizer,wordLengths=c(1,Inf)))
quad <- TermDocumentMatrix(as.Corpus(sam), control = list(tokenize = quadgramizer, wordLengths=c(1,Inf)))

uni.sum <- apply(uni, 1, sum)
bi.sum <- apply(bi, 1, sum)
tri.sum <- apply(tri, 1, sum)
quad.sum <- apply(quad, 1, sum)

tokens <- names(uni.sum)
bi.tokens <- names(bi.sum)
bi.count <- sum(as.numeric(bi.sum))
for (i in 1:length(bi.tokens)){
    b1[i] <- unlist(stri_split_fixed(bi.tokens[i], " "))[1]
    b2[i] <- unlist(stri_split_fixed(bi.tokens[i], " "))[2]
}
t.bi <- data.frame(term1 = b1, term2 = b2)
t.bi <- mutate(t.bi, count = as.numeric(bi.sum), prob = as.numeric(bi.sum)/bi.count)

tri.tokens <- names(tri.sum)
tri.count <- sum(as.numeric(tri.sum))
for (i in 1:length(tri.tokens)){
    t1[i] <- unlist(stri_split_fixed(tri.tokens[i], " "))[1]
    t2[i] <- unlist(stri_split_fixed(tri.tokens[i], " "))[2]
    t3[i] <- unlist(stri_split_fixed(tri.tokens[i], " "))[3]
}
t.tri <- data.frame(term1 = t1, term2 = t2, term3 = t3)
t.tri <- mutate(t.tri, count = as.numeric(tri.sum), prob = count / tri.count)

quad.tokens <- names(quad.sum)
quad.count <- sum(as.numeric(quad.sum))
for (i in 1:length(quad.tokens)){
    q1[i] <- unlist(stri_split_fixed(quad.tokens[i], " "))[1]
    q2[i] <- unlist(stri_split_fixed(quad.tokens[i], " "))[2]
    q3[i] <- unlist(stri_split_fixed(quad.tokens[i], " "))[3]
    q4[i] <- unlist(stri_split_fixed(quad.tokens[i], " "))[4]
}
t.quad <- data.frame(term1 = q1, term2 = q2, term3 = q3, term4 = q4)
t.quad <- mutate(t.quad, count = as.numeric(quad.sum), prob = count / quad.count)






V <- length(uni.sum)

unigrams <- cbind(tokens, mutate(data.frame(term = names(uni.sum)), count = as.numeric(uni.sum), prob = count/sum(as.numeric(uni.sum))))
bigrams <- data.frame(names(bi.sum), count = as.numeric(bi.sum), prob = count/sum(as.numeric(bi.sum)))
trigrams <- data.frame(names(tri.sum), count = as.numeric(tri.sum), prob = count/sum(as.numeric(tri.sum)))
quadgrams <- data.frame(names(quad.sum), count = as.numeric(quad.sum), prob = count/sum(as.numeric(quad.sum)))














sample.text <- function(blogs, twitter, news) {
    set.seed(1111)
    blogs.text <- blogs[as.logical(rbinom(length(blogs), 1, prob = .001))]
    twitter.text <- twitter[as.logical(rbinom(length(twitter), 1, prob = .001))]
    news.text <- news[as.logical(rbinom(length(news), 1, prob = .001))]
    sam <<- c(blogs.text, twitter.text, news.text)
    
}



clean.ngram <- function(input) {
    string <- gsub("^", "<s> ", input)
    string <- gsub("$", " <s>", string)
    string <- unlist(tolower(stri_split_fixed(string, " ")))
    string <- gsub("[^a-zA-Z\ ]", "", string)
    string <- gsub("\ +", " ", string)
    input.string <<- string
}

sentence <- function(corpus) {
    sentence.annotator <- Maxent_Sent_Token_Annotator(language = "en")
    corpus <- as.String(unlist(corpus))
    bounds <- NLP::annotate(corpus, sentence.annotator)
    corpus <- corpus[bounds]
    corpus <- tolower(gsub("[^a-zA-Z\ ]", "",corpus))
    corpus <- gsub("^", "<s> ", corpus)
    corpus <- gsub("$", " <s>", corpus)
    vc <<- VCorpus(VectorSource(PlainTextDocument(corpus)))
}

spell.check <- function(corpus) {
    sen <- as.character(sentence(corpus)[[1]])
    mis <- NULL
    for (i in 1:length(sen)) {
        mis[i] <- which_misspelled(sen[i], suggest = TRUE)
    }
    
    
}



