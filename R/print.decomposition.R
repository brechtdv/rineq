print.decomposition <-
function(x, ...)
    {
    if (class(x) != "decomposition") stop("Object is not of class decomposition")
    cat("Overall CI:", concentrationIndex(x$overallci), "\n")
    if (x$outcomeCorrected) cat("(based on a corrected value)\n")
    cat("\n")
    
    cat("Decomposition:\n")
    result <- data.frame(x$contribution)
    names(result) <- "Contribution (%)"
    result$Corrected <- ""
    result$Corrected[x$correctedCoefficients] <- "yes"
    result$Corrected[!x$correctedCoefficients] <- "no"
    print(result)
    }

