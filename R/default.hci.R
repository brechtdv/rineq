default.hci <-
function(object) {
    if (class(object) != "hci") stop("Object is not of class hci")
    unlist(object)
}

