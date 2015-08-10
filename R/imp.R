## imp()
## Evaluating variable importance in rpart_ci() trees
## last update: 07/07/2015
## ------------------------------------------------------------------------#



imp <- 
function(object, surrogates = TRUE, competes = FALSE, ...) {

 allVars <- colnames(attributes(object$terms)$factors)
 tmp <- rownames(object$splits)
 if(is.null(rownames(object$splits))) {
     out <- NULL
     zeros <- data.frame(x = rep(0, length(allVars)),Variable = allVars)
     out <- rbind(out, zeros)

  } else {

     rownames(object$splits) <- 1:nrow(object$splits)
     splits <- data.frame(object$splits)
     splits$var <- tmp
     splits$type <- ""
     frame <- as.data.frame(object$frame)
     index <- 0
     for(i in 1:nrow(frame)) {
        if(frame$var[i] != "<leaf>") {
            index <- index + 1
            splits$type[index] <- "primary"
                 if(frame$ncompete[i] > 0) {
                    for(j in 1:frame$ncompete[i]) {
                        index <- index + 1
                        splits$type[index] <- "competing"
                    }
                 }
                 if(frame$nsurrogate[i] > 0) {
                    for(j in 1:frame$nsurrogate[i]) {
                        index <- index + 1
                        splits$type[index] <- "surrogate"
                    }
                 }
        }
     }
     splits$var <- factor(as.character(splits$var))
     
     # Correcting the "splits" object: splits$improve isn't the "improve" 
     # but the "adj" for surrogate spits
     for (i in 1: nrow(splits)) {
          if (splits$type[i] == "primary") {
              splits$correcting[i] <- splits$improve[i] 
          } else { 
              splits$correcting[i] <- splits$correcting[i-1]
          }
     }
     splits <- within(splits, improve[adj != 0] <- correcting[adj != 0] * adj[adj != 0])
     if(!surrogates) splits <- subset(splits, type != "surrogate")
     if(!competes) splits <- subset(splits, type != "competing")
     out <- aggregate(splits$improve, list(Variable = splits$var), sum, na.rm = TRUE)

     if(!all(allVars %in% out$Variable)) {
        missingVars <- allVars[!(allVars %in% out$Variable)]
        zeros <- data.frame(x = rep(0, length(missingVars)),Variable = missingVars)
        out <- rbind(out, zeros)
     }
 }
 
 out2 <- data.frame(overall.importance = out$x, relative.importance= out$x * 100 / max(out$x, na.rm = TRUE))
 rownames(out2) <- out$Variable
 out2 <- out2[order(-out2$relative.importance),]
 out2
}
