default.decomposition <-
function(object)
    {
    if (class(object) != "decomposition") stop("Object is not of class decomposition")
    unlist(object)
    }

