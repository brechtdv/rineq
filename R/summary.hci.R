summary.hci <-
function(object, ...)
    {
    if (!any(class(object) == 'hci')) stop("Object is not of class hci")
    cat("Call:\n")
    print(object$call)
    cat("\nHealth Concentration Index:\n", object$concentrationIndex,"\n\n")
    cat("Variance:\n",object$variance,"\n\n")
    cat("95% Confidence Interval:\n", confint(object), "\n")
    }

