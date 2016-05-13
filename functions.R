library(tm)
library(stringi)
library(dplyr)
library(RWeka)
library(NLP)
library(openNLP)
library(qdap)
library(ggplot2)
library(doParallel)
registerDoParallel(cores = 4)

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
sam <- sam[!is.na(sam)]

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
b1 <- b2 <- t1 <- t2 <- t3 <- q1 <- q2 <- q3 <- q4 <- NULL
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




unigrams <- mutate(data.frame(tokens), count = as.numeric(uni.sum), prob = count/sum(as.numeric(uni.sum)))
bigrams <- mutate(data.frame(bi.tokens), count = as.numeric(bi.sum), prob = count/sum(as.numeric(bi.sum)))
trigrams <- mutate(data.frame(tri.tokens), count = as.numeric(tri.sum), prob = count/sum(as.numeric(tri.sum)))
quadgrams <- mutate(data.frame(quad.tokens), count = as.numeric(quad.sum), prob = count/sum(as.numeric(quad.sum)))

V.uni <- nrow(unigrams)
V.bi <- nrow(bigrams)
V.tri <- nrow(trigrams)
V.quad <- nrow(quadgrams)











sample.text <- function(blogs, twitter, news) {
    set.seed(1111)
    blogs.text <- blogs[as.logical(rbinom(length(blogs), 1, prob = .001))]
    twitter.text <- twitter[as.logical(rbinom(length(twitter), 1, prob = .001))]
    news.text <- news[as.logical(rbinom(length(news), 1, prob = .001))]
    sam <<- c(blogs.text, twitter.text, news.text)
    
}



clean.input <- function(input) {
    string <- gsub("^", "<s> ", input)
    string <- gsub("$", " <s>", string)
    string <- unlist(tolower(stri_split_fixed(string, " ")))
    string <- gsub("[^a-zA-Z\ <>]|", "", string)
    string <- gsub("\ +", " ", string)
    input.string <<- string
}

sentence <- function(lines) {
    sentence.annotator <- Maxent_Sent_Token_Annotator(language = "en")
    lines <- unlist(lines)
    bounds <- NLP::annotate(lines, sentence.annotator)
    lines <- lines[bounds]
    lines <- tolower(gsub("[^a-zA-Z\ ]", "",lines))
    lines <- gsub("^", "<s> ", lines)
    cleaned <<- gsub("$", " <s>", lines)
    #vc <<- VCorpus(VectorSource(PlainTextDocument(lines)))
}

spell.correct <- function(word, prev, options){
    pre <- stri_split_fixed(prev, " ")
    uni.prob <- unigrams[unigrams$term == word,]$prob
    df.bi <- bigrams[bigrams$term2 == word,]
    df.tri <- trigrams[trigrams$term3 == word,]
    df.quad <- quadgrams[quadgrams$term4 == word,]
}

spell.check <- function(corpus) {
    sen <- as.String(unlist(corpus))
    mis <- word <- position <- opt1 <- opt <- NULL
    for (i in 1:length(sen)) {
        sent <- sen[i]
        mis <- which_misspelled(sent, suggest = TRUE)
        
        
        for (j in 1:length(sent)){
            word[j] <- unlist(stri_split_fixed(sent, " "))[j]
            position[j] <- j
            
            for (k in 1:length(unlist(stri_split_fixed(mis[k,]$more.suggestions)))){
                opt1[k] <- mis[k,]$suggestion
                opt[k] <- unlist(stri_split_fixed(mis[k,]$more.suggestions))[k]
                df <- arrange(filter(t.tri, term1 == word[j-1], term == word[j+1]), count)
                if (opt1[k] %in% df$term2){
                    word[j] <- opt1[k]
                } else {
                    edit.dist[k] <- adist(word[k], opt[k])
                }
                
                
            }
            
            
        }
    }
    
    
}

spelling <- function() {
    words <- arrange(unigrams, desc(count))$tokens
#    sorted_words <- names(sort(table(strsplit(tolower(paste(readLines("http://www.norvig.com/big.txt"), collapse = " ")), "[^a-z]+")), decreasing = TRUE))
    correct <- function(word) { c(words[ adist(word, words) <= min(adist(word, words), 2)], word)[1] }
    
    
    
    
}




split_text <- strsplit(tolower(raw_text), "[^a-z]+")
# Count the number of different type of words.
word_count <- table(split_text)
# Sort the words and create an ordered vector with the most common type of words first.
sorted_words <- names(sort(word_count, decreasing = TRUE))





spell.check1 <- function(corpus) {
    corpus <- gsub("[^a-zA-Z\ ']", " ", corpus)
    corpus <- gsub("\ +", " ", corpus)
    corpus <- unlist(stri_split_fixed(as.String(corpus), " "))
        
    for (k in 1:length(corpus)){
        mis <- check_misspelled(corpus[k])
        opt1[k] <- mis[k,]$suggestion
        opt[k] <- unlist(stri_split_fixed(mis[k,]$more.suggestions))[k]
        df <- arrange(filter(t.tri, term1 == word[j-1], term == word[j+1]), count)
        if (opt1[k] %in% df$term2){
            word[j] <- opt1[k]
        } else {
            edit.dist[k] <- adist(word[k], opt[k])
        }
        
        
    }
    
    
}
    
    
    
