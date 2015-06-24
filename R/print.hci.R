print.hci <-
function(x, ...)
    {
    if (class(x) != "hci") stop("Object is not of class hci")
    print(x$concentrationIndex)
    }

