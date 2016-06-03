
#
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

