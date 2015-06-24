plot.decomposition <-
function(x,...)
    {
    if (class(x) != "decomposition") stop("Object is not of class decomposition")
    barplot(sort(x$contribution, decreasing = TRUE),...)
    }

