confint.hci <-
function(object, parm = NULL, level = 0.95, ...)
    {
    if (!any(class(object) == 'hci')) stop("Object is not of class hci")
	parm <- NULL
    a <- (1 - level) / 2
    a <- c(a, 1 - a)
    pct <- stats:::format.perc(a, 3)
    fac <- qnorm(a)
    ci <- object$concentration_index + fac * sqrt(object$variance) 
    return(ci)
    }

