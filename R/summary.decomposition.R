summary.decomposition <-
function(object, ...)
    {
    if (class(object) != "decomposition") stop("Object is not of class decomposition")
    cat("Overall CI:", concentrationIndex(object$overallci), "\n")
    cat("95% confidence interval:", confint(object$overallci),"\n")
    if (object$outcomeCorrected) cat("(based on a corrected value)\n")
    cat("\n")
    
    cat("Decomposition:\n")
    result <- data.frame(object$contribution)
    names(result) <- "Contribution (%)"
    result$"Concentration Index" <- c(NA, object$partialcis)
    result$"lower 5%" <- c(NA, object$confints[1,])
    result$"upper 5%" <- c(NA, object$confints[2,])
    result$Corrected <- ""
    result$Corrected[object$correctedCoefficients] <- "yes"
    result$Corrected[!object$correctedCoefficients] <- "no"
    (result)
    }

