summary.decomposition <-
function(object, ...) {
    if (class(object) != "decomposition") stop("Object is not of class decomposition")
    cat("Overall CI:", concentration_index(object$overall_ci), "\n")
    cat("95% confidence interval:", confint(object$overall_ci),"\n")
    if (object$outcome_corrected) cat("(based on a corrected value)\n")
    cat("\n")
    
    cat("Decomposition:\n")
    result <- data.frame(object$contribution)
    names(result) <- "Contribution (%)"
    result$"Concentration Index" <- c(NA, object$partial_cis)
    result$"lower 5%" <- c(NA, object$confints[1,])
    result$"upper 5%" <- c(NA, object$confints[2,])
    result$Corrected <- ""
    result$Corrected[object$corrected_coefficients] <- "yes"
    result$Corrected[!object$corrected_coefficients] <- "no"
    (result)
}
