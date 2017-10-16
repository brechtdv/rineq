correct_sign <-
function(x, shift = TRUE) {
    # we have to check for two things here:
    # 1/ sign(0) = 0, but this does not pose a problem, so leave them out of the test
    # 2/ NA should remain NA; since sign(NA) = NA we leave them out of the test
    # if the signs of the remaining vector are all the same, there will be only 1 unique value
    # if not, there will be two unique values.
	modified <- FALSE
	if (length(unique(sign(x[(sign(x) != 0) & (!is.na(x))]))) > 1)
	{
		if(isTRUE(shift))
		{
			# subtract the minimum, taking care about possible NAs in the vector...
			myOffset <- min(x, na.rm = TRUE)
			x <- x - myOffset
		} else {
			x[x < 0] <- 0
		}
		# throw a warning that the variable has been changed
		modified <- TRUE
	}
    # return the original vector if everything is ok, otherwise the modified one.
    return(list(correctedx = x, modified = modified))
}

