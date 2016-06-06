library(shiny)
library(tm)
library(stringi)
library(dplyr)
library(rJava)
library(RWeka)
library(NLP)
library(openNLP)
library(qdap)
library(doParallel)
registerDoParallel(cores = 4)
library(qdap)
t.uni <- read.table("unigram.data.csv", sep = ",", header = TRUE)
t.bi <- read.table("bigram.data.csv", sep = ",", header = TRUE)
t.tri <- read.table("trigram.data.csv", sep = ",", header = TRUE)
t.quad <- read.table("quadgram.data.csv", sep = ",", header = TRUE)

prep <- function(input) {
      string <<- input;
      num <<- length(string);
      delta <<- .5;
      if (string[num] == "") {
            string <- string[-num];
            num <- length(string);
      }
      if (num >= 3) {
            w1 <<- string[num];
            w2 <<- string[num-1];
            w3 <<- string[num-2];
      } else if (num == 2) {
            w1 <<- string[num];
            w2 <<- string[num-1];
      } else {
            print("Please input 3 or more words");
            break ;
      }
      quad.w <<- filter(t.quad, term1 == w3, term2 == w2, term3 == w1);
      tri.w <<- filter(t.tri, term1 == w2, term2 == w1);
      bi.w <<- filter(t.bi, term1 == w1);
      
      bi.choices <<- as.character(head(arrange(bi.w, desc(count))$term2,4));
      
      tri.choices <<- as.character(head(arrange(tri.w, desc(count))$term3,4));
      
      quad.choices <<- as.character(head(arrange(quad.w, desc(count))$term4,4));
      
      choices <<- unique(c(quad.choices, tri.choices, bi.choices));
      if (length(quad.choices) == 0 ) { choices <<- unique(c(tri.choices, bi.choices)) }
      if (length(tri.choices) == 0 ) { choices <<- bi.choices }
      
      #Thanks to the contributors at Phorum for keeping me from writing out this list.
      #http://www.phorum.org/phorum5/read.php?16,114701,114701#msg-114701
      bads <- read.csv("bad_words.csv")[,5]
      
      choices <- unlist(rm_stopwords(choices, stopwords = bads))
      
      if ("<s>" %in% choices){
            choices <<- choices[-grep("<s>", choices)];
            return(choices);
      } else {
            choices <<- choices;
            return(choices);
      }
}


pkn.bi <- function(input, word){
      #      string <- unlist(clean.input(input))
      #      num <- length(string)
      #      w1 <- string[num]
      #      bi.w <- filter(t.bi, term1 == w1)
      bi.wi <<- filter(t.bi, term2 == word);
      pc.wi <<- nrow(bi.wi)/nrow(t.bi);
      #      delta <- .5
      
      lambda.bi <<- (delta / sum(bi.w$count)) * nrow(bi.w);
      
      pkn.bi.wi <<- (max(c(0, sum(filter(bi.wi, term1 == w1)$count)-delta))) / 
            sum(bi.w$count) + lambda.bi * pc.wi;
      return(pkn.bi.wi);
}

pkn.tri <- function(input, word){
      #      string <- unlist(clean.input(input))
      #      num <- length(string)
      #      w1 <- string[num]
      #      w2 <- string[num-1]
      #      tri.w <- filter(t.tri, term1 == w2, term2 == w1)
      tri.wi <<- filter(t.tri, term3 == word);
      #      delta <- .5
      
      lambda.tri <<- (delta / sum(tri.w$count))*nrow(tri.w);
      
      pkn.tri.wi <<- (max(c(0, sum(filter(tri.w, term3 == word)$count)-delta)))/ 
            sum(filter(tri.w, term3 == word)$count) + 
            lambda.tri*pkn.bi(input, word);
      return(pkn.tri.wi);
}


pkn.quad <- function(input, word){
      #      string <- unlist(clean.input(input))
      #      num <- length(string)
      #      w1 <- string[num]
      #      w2 <- string[num-1]
      #      w3 <- string[num-2]
      #      quad.w <- filter(t.quad, term1 == w3, term2 == w2, term3 == w1)
      quad.wi <<- filter(t.quad, term4 == word);
      #      delta <- .5
      
      lambda.quad <<- (delta / sum(quad.w$count))*nrow(quad.w);
      if (sum(filter(quad.w, term4 == word)$count) != 0 & sum(filter(quad.w, term4 == word)$count) !=0) {
            pkn.quad.wi <- (max(c(0, sum(filter(quad.w, term4 == word)$count)-delta)))/ 
                  sum(filter(quad.w, term4 == word)$count) + 
                  lambda.quad*pkn.tri(input, word);
      } else if (is.na(pkn.tri.wi)) {
            pkn.quad.wi <<- lambda.quad*pkn.bi.wi;
      } else {
            pkn.quad.wi <<- lambda.quad*pkn.tri.wi;
      }
      
      return(pkn.quad.wi);
      
}

pk <- function(input) {
      prep(input);
      
      probs <- NULL;
      if (num >= 4) {
            for (i in 1:length(choices)) {
                  probs[i] <- pkn.quad(input, choices[i]);
            }
            prediction <- choices[which.max(probs)];
      } else if (num == 3) {
            for (i in 1:length(choices)) {
                  probs[i] <- pkn.tri(input, tri.choices[i]);
            }
            prediction <- choices[which.max(probs)];
      } else if (num == 2) {
            for (i in 1:length(choices)) {
                  probs[i] <- pkn.bi(input, bi.choices[i]);
            }
            prediction <- choices[which.max(probs)];
      } else {
            break;
      }
      return(prediction);
}



clean.input <- function(input) {
      string <- gsub("[^a-zA-Z\ ]", "", input);
      string <- gsub("\ +", " ", string);
      string <- gsub("^", "<s> ", string);
      string <<- tolower(unlist(stri_split_fixed(string, " ")));
      
}




shinyServer(
      function(input, output) {
            
            cleaned <- reactive({ clean.input( input$input.string ) })
            output$clean <- renderText( cleaned() )
            prediction <- reactive({ pk( cleaned() ) })
            output$prediction <- renderText( prediction() )
            choices <- reactive({ prep(cleaned()) })
            output$other.choices <- renderText( choices() )
      }     
)