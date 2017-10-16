summary.hci <-
function(object, ...) {
    if (!any(class(object) == 'hci')) stop("Object is not of class hci")
    cat("Call:\n")
    print(object$call)
    cat("\nType of Concentration Index:\n", object$type,"\n")
    cat("\nHealth Concentration Index:\n", object$concentration_index,"\n\n")
    cat("Variance:\n", object$variance, "\n\n")
    cat("95% Confidence Interval:\n", confint(object), "\n")
}
