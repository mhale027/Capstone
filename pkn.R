



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
      
      lambda.quad <- (delta / sum(quad.wi$count))*nrow(quad.w)
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



