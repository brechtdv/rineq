print.decomposition <-
function(x, ...) {
  if (class(x) != "decomposition") stop("Object is not of class decomposition")
  cat("Overall CI:", concentration_index(x$overall_ci), "\n")
  if (x$outcome_corrected) cat("(based on a corrected value)\n")
  cat("\n")
  
  cat("Decomposition:\n")
  result <- data.frame(x$contribution)
  names(result) <- "Contribution (%)"
  result$Corrected <- ""
  result$Corrected[x$corrected_coefficients] <- "yes"
  result$Corrected[!x$corrected_coefficients] <- "no"
  print(result)
}

