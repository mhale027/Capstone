library(tm)
library(stringi)
library(dplyr)
library(RWeka)
library(NLP)
library(openNLP)
library(ggplot2)

set.seed(1111)


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
sam <- gsub("[[:punct:]]|[[:digit:]]", " ", sam)vc <- VCorpus(VectorSource(sam))

vc <- tm_map(vc, FUN=stripWhitespace)
vc <- tm_map(vc, FUN=tolower)
vc <- tm_map(vc, FUN=removePunctuation)
vc <- tm_map(vc, PlainTextDocument)


unigramizer <- function(vc) NGramTokenizer(vc, Weka_control(min=1, max=1))
bigramizer <- function(vc) NGramTokenizer(vc, Weka_control(min=2, max=2))
trigramizer <- function(vc) NGramTokenizer(vc, Weka_control(min=3, max=3))
quadgramizer <- function(vc) NGramTokenizer(vc, Weka_control(min=4, max=4))

uni <- TermDocumentMatrix(vc, control = list(tokenize = unigramizer))
bi <- TermDocumentMatrix(vc, control = list(tokenize = bigramizer))
tri <- TermDocumentMatrix(vc, control = list(tokenize = trigramizer))
quad <- TermDocumentMatrix(vc, control = list(tokenize = quadgramizer))



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

sentence <- function(corp) {
    sentence.annotator <- Maxent_Sent_Token_Annotator(language = "en")
    corpus <- as.String(corp)
    bounds <- annotate(corpus, sentence.annotator)
    corpus <- corpus[bounds]
    corpus <- gsub("^", "<s> ", corpus)
    corpus <- gsub("$", " <s>", corpus)
}





