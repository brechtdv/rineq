## imp()
## Evaluating variable importance in rpart_ci() trees
## ------------------------------------------------------------------------#

imp <- 
function(object, surrogates = TRUE, competes = FALSE, ...) {

 all_vars <- colnames(attributes(object$terms)$factors)
 tmp <- rownames(object$splits)
 if(is.null(rownames(object$splits))) {
     out <- NULL
     zeros <- data.frame(x = rep(0, length(all_vars)),Variable = all_vars)
     out <- rbind(out, zeros)

  } else {

     rownames(object$splits) <- seq(nrow(object$splits))
     splits <- data.frame(object$splits)
     splits$var <- tmp
     splits$type <- ""
     frame <- as.data.frame(object$frame)
     index <- 0
     for(i in seq(nrow(frame))) {
        if(frame$var[i] != "<leaf>") {
            index <- index + 1
            splits$type[index] <- "primary"
                 if(frame$ncompete[i] > 0) {
                    for(j in seq(frame$ncompete[i])) {
                        index <- index + 1
                        splits$type[index] <- "competing"
                    }
                 }
                 if(frame$nsurrogate[i] > 0) {
                    for(j in seq(frame$nsurrogate[i])) {
                        index <- index + 1
                        splits$type[index] <- "surrogate"
                    }
                 }
        }
     }
     splits$var <- factor(as.character(splits$var))
     
     # Correcting the "splits" object: splits$improve isn't the "improve" 
     # but the "adj" for surrogate spits
     for (i in seq(nrow(splits))) {
          if (splits$type[i] == "primary") {
              splits$correcting[i] <- splits$improve[i] 
          } else { 
              splits$correcting[i] <- splits$correcting[i-1]
          }
     }
     splits <- within(splits, improve[splits$adj != 0] <- splits$correcting[splits$adj != 0] * splits$adj[splits$adj != 0])
     if(!surrogates) splits <- subset(splits, splits$type != "surrogate")
     if(!competes) splits <- subset(splits, splits$type != "competing")
     out <- aggregate(splits$improve, list(Variable = splits$var), sum, na.rm = TRUE)

     if(!all(all_vars %in% out$Variable)) {
        missing_vars <- all_vars[!(all_vars %in% out$Variable)]
        zeros <- data.frame(x = rep(0, length(missing_vars)), Variable = missing_vars)
        out <- rbind(out, zeros)
     }
 }
 
 out2 <-
 data.frame(overall_importance = out$x,
            relative_importance = out$x * 100 / max(out$x, na.rm = TRUE))
 rownames(out2) <- out$Variable
 out2 <- out2[order(-out2$relative_importance), ]
 out2
}
