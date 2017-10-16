decomposition <-
function(outcome, betas, mm, ranker, wt, correction) {    
    # define an index vector for the rows that are actually used in the model, rownames are strings
    rows <- as.numeric(rownames(mm))

    # correct the sign for the partial outcomes when requested
    corrected <- rep(FALSE, ncol(mm))
    outcome_corrected <- FALSE
    if (correction) {
        temp <- apply(mm, 2, correct_sign)
        mm <- sapply(temp, corrected_value)
        corrected <- sapply(temp, is_corrected)
        temp <- correct_sign(outcome)
        outcome <- corrected_value(temp)
        outcome_corrected <- is_corrected(temp)
	}
    
    # calculate the partial CI for every variable in the model matrix, except the intercept
    # weights are given by the weights of the model
    # use only the observations of the ranking variable that are actually used in the model
    # the intermediate cis returns a list with objects of class hci
    # indices stores the values of the concentration index only
    cis <- apply(mm[,-1], 2, ci, x = ranker[rows], wt = wt)
    indices <- sapply(cis, concentration_index)
    confints <- sapply(cis, confint)
    
    # calculate the weighted average for every variable in the model matrix
    averages <- apply(mm[,-1], 2, weighted.mean, w = wt)
    
    # calculate the weighted mean of the outcome
    mu <- weighted.mean(outcome, w = wt)
    
    # calculate the sum off all partial contributions
    contributions <- betas * averages / mu * indices
    sumOfContributions <- sum(contributions)
    
    # calculate the residual CI: first, calculate the overal CI
    # between the overall wealth variable (using only the observations used in the model)
    # and the health outcome; use the sampling weights used in the model
    # then subtract the sum of all partial contributions
    CIoverall <- ci(ranker[rows], as.numeric(outcome), wt)
    CIresid <- concentration_index(CIoverall) - sumOfContributions
    
    # add the residual CI to the contributions vector and name it
    contributions <- c(CIresid, contributions)
    names(contributions)[1] <- "residual"
    
    # calculate the % contribution
    pctcontrib <- contributions/sum(contributions) * 100
    
    # return the result
    results <-
	list(betas = betas,
	     partial_cis = indices,
		 confints = confints,
		 averages = averages, 
		 contribution = pctcontrib,
		 overall_ci = CIoverall,
		 corrected_coefficients = corrected, 
		 outcome_corrected = outcome_corrected,
		 rows = rows)
    class(results) <- "decomposition"
    return(results)   
}
