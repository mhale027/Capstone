starts <- seq(5001, 20000, 500)
ends <- seq(5500, 20000, 500)

for (i in 1:length(starts)) {
      bolster(starts[i], ends[i])
}





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



#blogs1 <- blogs[as.logical(rbinom(length(blogs), 1, prob = .01))]
#twitter1 <- twitter[as.logical(rbinom(length(twitter), 1, prob = .01))]
#news1 <- news[as.logical(rbinom(length(news), 1, prob = .01))]

blogs1 <- blogs[1:10000]
twitter1 <- twitter[1:10000]
news1 <- news[1:10000]

rm(twitter, blogs, news)

sam <- c(blogs1, twitter1, news1)
sam <- sam[!is.na(sam)]
sam <- sentence(sam)
sam <- gsub("[^a-zA-Z\ (^<s>)]", "", sam)
sam <- gsub("\ +", " ", sam)
sam <- tolower(sam)

rm(twitter1, blogs1, news1)



unigramizer <- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=1, max=1))
bigramizer <- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=2, max=2))
trigramizer <- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=3, max=3))
quadgramizer <- function(sam) NGramTokenizer(sam, RWeka::Weka_control(min=4, max=4))

uni <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = unigramizer, wordLengths=c(1,Inf)))
bi <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = bigramizer, wordLengths=c(1,Inf)))
tri <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = trigramizer,wordLengths=c(1,Inf)))
quad <- TermDocumentMatrix(as.Corpus(as.character(sam)), control = list(tokenize = quadgramizer, wordLengths=c(1,Inf)))

uni.sum <- apply(uni, 1, sum)
bi.sum <- apply(bi, 1, sum)
tri.sum <- apply(tri, 1, sum)
quad.sum <- apply(quad, 1, sum)

gram.df()

names(df.bi) <- c("term1", "term2")
names(df.tri) <- c("term1", "term2", "term3")
names(df.quad) <- c("term1", "term2", "term3", "term4")
t.bi <- select(df.bi, term1, term2)
t.bi <- mutate(t.bi, count = as.numeric(bi.sum))

t.tri <- select(df.tri, term1, term2, term3)
t.tri <- mutate(t.tri, count = as.numeric(tri.sum))

t.quad <- select(df.quad, term1, term2, term3, term4)
t.quad <- mutate(t.quad, count = as.numeric(quad.sum))




















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
#tri.tokens <- names(tri.sum)
tri.count <- sum(as.numeric(tri.sum))
for (i in 1:length(tri.tokens)){
    t1[i] <- unlist(stri_split_fixed(tri.tokens[i], " "))[1]
    t2[i] <- unlist(stri_split_fixed(tri.tokens[i], " "))[2]
    t3[i] <- unlist(stri_split_fixed(tri.tokens[i], " "))[3]
}
t.tri <- data.frame(term1 = t1, term2 = t2, term3 = t3)
t.tri <- mutate(t.tri, count = as.numeric(tri.sum), prob = count / tri.count)
#quad.tokens <- names(quad.sum)
quad.count <- sum(as.numeric(quad.sum))
for (i in 1:length(quad.tokens)){
    q1[i] <- unlist(stri_split_fixed(quad.tokens[i], " "))[1]
    q2[i] <- unlist(stri_split_fixed(quad.tokens[i], " "))[2]
    q3[i] <- unlist(stri_split_fixed(quad.tokens[i], " "))[3]
    q4[i] <- unlist(stri_split_fixed(quad.tokens[i], " "))[4]
}
t.quad <- data.frame(term1 = q1, term2 = q2, term3 = q3, term4 = q4)
t.quad <- mutate(t.quad, count = as.numeric(quad.sum), prob = count / quad.count)




#unigrams <- mutate(data.frame(tokens), count = as.numeric(uni.sum), prob = count/sum(as.numeric(uni.sum)))
#bigrams <- mutate(data.frame(bi.tokens), count = as.numeric(bi.sum), prob = count/sum(as.numeric(bi.sum)))
#trigrams <- mutate(data.frame(tri.tokens), count = as.numeric(tri.sum), prob = count/sum(as.numeric(tri.sum)))
#quadgrams <- mutate(data.frame(quad.tokens), count = as.numeric(quad.sum), prob = count/sum(as.numeric(quad.sum)))

#V.uni <- nrow(unigrams)
#V.bi <- nrow(bigrams)
#V.tri <- nrow(trigrams)
#V.quad <- nrow(quadgrams)

























tokens <- names(uni.sum)
bi.tokens <- names(bi.sum)
bi.count <- sum(as.numeric(bi.sum))























bi.length <- length(bi.tokens)
bi.remainder <- bi.length%%5000
bi.count <- round(bi.length/5000,0)

b1 <- b2 <- rep("NA", bi.length)
t1 <- t2 <- t3 <- rep("NA", tri.length)
q1 <- q2 <- q3 <- q4 <- rep("NA", quad.length)

for (i in 1:bi.count){
      for (j in 1:5000) {
            count <- (i-1) * 5000 + j
            b <- unlist(stri_split_fixed(bi.tokens[count], " "))
            b1[count] <- b[1]
            b2[count] <- b[2]
      }
}
for (k in 1:bi.remainder) {
      count <- bi.length - bi.remainder + k
      b <- unlist(stri_split_fixed(bi.tokens[count], " "))
      b1[count] <- b[1]
      b2[count] <- b[2]
}


t.bi <- data.frame(term1 = b1, term2 = b2)
t.bi <- mutate(t.bi, count = as.numeric(bi.sum), prob = as.numeric(bi.sum)/bi.count)




tri.tokens <- names(tri.sum)
tri.count <- sum(as.numeric(tri.sum))

tri.length <- length(tri.tokens)
tri.remainder <- tri.length%%5000
tri.count <- round(tri.length/5000,0)


for (i in 1:length(tri.length)){
      for (j in 1:5000) {
            count <- (i-1) * 5000 + j
            t <- unlist(stri_split_fixed(tri.tokens[count], " "))
            t1[count] <- t[1]
            t2[count] <- t[2]
            t3[count] <- t[3]
      }
}
for (k in 1:tri.remainder) {
      count <- tri.length - tri.remainder + k
      t <- unlist(stri_split_fixed(tri.tokens[count], " "))
      t1[count] <- t[1]
      t2[count] <- t[2]
      t3[count] <- t[3]
}


t.tri <- data.frame(term1 = t1, term2 = t2, term3 = t3)
t.tri <- mutate(t.tri, count = as.numeric(tri.sum), prob = as.numeric(count / tri.count))



quad.tokens <- names(quad.sum)
quad.count <- sum(as.numeric(quad.sum))

quad.length <- length(quad.tokens)
quad.remainder <- quad.length%%5000
quad.count <- round(quad.length/5000,0)


for (i in 1:length(quad.length)){
      for (j in 1:5000) {
            count <- (i-1) * 5000 + j
            q <- unlist(stri_split_fixed(quad.tokens[count], " "))
            q1[count] <- q[1]
            q2[count] <- q[2]
            q3[count] <- q[3]
            q4[count] <- q[4]
      }
}
for (k in 1:quad.remainder) {
      count <- quad.length - quad.remainder + k
      q <- unlist(stri_split_fixed(quad.tokens[count], " "))
      q1[count] <- q[1]
      q2[count] <- q[2]
      q3[count] <- q[3]
      q4[count] <- q[4]
}




t.quad <- data.frame(term1 = q1, term2 = q2, term3 = q3, term4 = q4)
t.quad <- mutate(t.quad, count = as.numeric(quad.sum), prob = as.numeric(count / quad.count))




pkn.bi <- function(input, word){
      string <- unlist(clean.input(input))
      num <- length(string)
      w1 <- string[num]
      bi.w <- filter(t.bi, term1 == w1)
      bi.wi <- filter(t.bi, term2 == word)
      pc.wi <- nrow(bi.wi)/nrow(t.bi)
      delta <- .5
      
      lambda.bi <- (delta / sum(bi.w$count)) * nrow(bi.w)
      
      pkn.bi.wi <- (max(c(0, sum(filter(bi.wi, term1 == w1)$count)-delta))) / 
            sum(bi.w$count) + lambda.bi * pc.wi
      return(pkn.bi.wi)
}

pkn.tri <- function(input, word){
      string <- unlist(clean.input(input))
      num <- length(string)
      w1 <- string[num]
      w2 <- string[num-1]
      tri.w <- filter(t.tri, term1 == w2, term2 == w1)
      tri.wi <- filter(t.tri, term3 == word)
      delta <- .5
      
      lambda.tri <- (delta / sum(tri.w$count))*nrow(tri.w)
      
      pkn.tri.wi <- (max(c(0, sum(filter(tri.w, term3 == word)$count)-delta)))/ 
            sum(filter(tri.w, term3 == word)$count) + 
            lambda.tri*pkn.bi(input, word)
      return(pkn.tri.wi)
}


pkn.quad <- function(input, word){
      string <- unlist(clean.input(input))
      num <- length(string)
      w1 <- string[num]
      w2 <- string[num-1]
      w3 <- string[num-2]
      quad.w <- filter(t.quad, term1 == w3, term2 == w2, term3 == w1)
      quad.wi <- filter(t.quad, term4 == word)
      delta <- .5
      
      lambda.quad <- (delta / sum(quad.w$count))*nrow(quad.w)
      if (sum(filter(quad.w, term4 == word)$count) != 0 & sum(filter(quad.w, term4 == word)$count) !=0) {
            pkn.quad.wi <- (max(c(0, sum(filter(quad.w, term4 == word)$count)-delta)))/ 
                  sum(filter(quad.w, term4 == word)$count) + 
                  lambda.quad*pkn.tri(input, word)
      } else if (is.na(pkn.tri.wi)) {
            pkn.quad.wi <- lambda.quad*pkn.bi.wi
      } else {
            pkn.quad.wi <- lambda.quad*pkn.tri.wi
      }
            
      return(pkn.quad.wi)
      
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


sample.text <- function(blogs, twitter, news) {
    set.seed(1111)
    blogs.text <- blogs[as.logical(rbinom(length(blogs), 1, prob = .001))]
    twitter.text <- twitter[as.logical(rbinom(length(twitter), 1, prob = .001))]
    news.text <- news[as.logical(rbinom(length(news), 1, prob = .001))]
    sam <<- c(blogs.text, twitter.text, news.text)
    
}



clean.input <- function(input) {
    string <- gsub("^", "<s> ", input)
    string <- tolower(string)
    string <- unlist(stri_split_fixed(string, " "))
    string <- gsub("[^a-zA-Z\ (^<s>)]", "", string)
    string <- gsub("\ +", " ", string)
    input.string <<- string
}

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

spell.correct <- function(word, prev, options){
    pre <- stri_split_fixed(prev, " ")
    uni.prob <- unigrams[unigrams$term == word,]$prob
    df.bi <- bigrams[bigrams$term2 == word,]
    df.tri <- trigrams[trigrams$term3 == word,]
    df.quad <- quadgrams[quadgrams$term4 == word,]
}
#sent <- function(corpus) {
#      sentence.count <- 0
#      corpus.sentences <<- NULL
#      corpus <<- corpus[!is.na(corpus)]
#      for (i in 1:length(corpus)) {
#            
#            c.sample <<- corpus[i]
#            c.sentences <<- sentence(c.sample)
#            
#            for (j in 1:length(c.sentences)) {
#                  sentence.count <<- sentence.count + 1
#                  corpus.sentences[sentence.count] <<- c.sentences[j]
#                  
#            }
#      
#      }
#      return(corpus.sentences)
#      
#}
#vc <- VCorpus(VectorSource(sam))

#vc <- tm_map(vc, FUN=stripWhitespace)
#vc <- tm_map(vc, FUN=tolower)
#vc <- tm_map(vc, FUN=removePunctuation)
#vc <- tm_map(vc, FUN=PlainTextDocument)

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
    
   






gram.df <- function(){
      tokens <- names(uni.sum)
      bi.tokens <- names(bi.sum)
      tri.tokens <- names(tri.sum)
      quad.tokens <- names(quad.sum)
      
      
      
      bi.tok <<- lapply(bi.tokens, function(x) {stri_split_fixed(x, pattern= " ")})
      df.bi <<- data.frame(matrix(unlist(bi.tok), ncol = 2, byrow = TRUE))
      
      tri.tok <<- lapply(tri.tokens, function(x) {stri_split_fixed(x, pattern= " ")})
      df.tri <<- data.frame(matrix(unlist(tri.tok), ncol = 3, byrow = TRUE))
      
      quad.tok <<- lapply(quad.tokens, function(x) {stri_split_fixed(x, pattern= " ")})
      df.quad <<- data.frame(matrix(unlist(quad.tok), ncol = 4, byrow = TRUE))
      
}





