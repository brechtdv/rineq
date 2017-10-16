summary.hci <-
function(x, ...) {
    if (!any(class(x) == 'hci')) stop("Object is not of class hci")
    cat("Call:\n")
    print(x$call)
    cat("\nType of Concentration Index:\n", x$type,"\n")
    cat("\nHealth Concentration Index:\n", x$concentration_index,"\n\n")
    cat("Variance:\n", x$variance, "\n\n")
    cat("95% Confidence Interval:\n", confint(x), "\n")
}
