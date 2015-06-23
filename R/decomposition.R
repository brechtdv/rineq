decomposition <-
function(outcome, betas, mm, ranker, wt, correction)
    {    
    # define an index vector for the rows that are actually used in the model, rownames are strings
    rows <- as.numeric(rownames(mm))

    # correct the sign for the partial outcomes when requested
    corrected <- rep(FALSE, ncol(mm))
    outcomeCorrected <- FALSE
    if (correction) 
        {
        temp <- apply(mm, 2, correctSign)
        mm <- sapply(temp, correctedValue)
        corrected <- sapply(temp, isCorrected)
        temp <- correctSign(outcome)
        outcome <- correctedValue(temp)
        outcomeCorrected <- isCorrected(temp)
        }
    
    # calculate the partial CI for every variable in the model matrix, except the intercept
    # weights are given by the weights of the model
    # use only the observations of the ranking variable that are actually used in the model
    # the intermediate cis returns a list with objects of class hci
    # indices stores the values of the concentration index only
    cis <- apply(mm[,-1], 2, RCI, x = ranker[rows], wt = wt)
    indices <- sapply(cis, concentrationIndex)
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
    CIoverall <- RCI(ranker[rows], as.numeric(outcome), wt)
    CIresid <- concentrationIndex(CIoverall) - sumOfContributions
    
    # add the residual CI to the contributions vector and name it
    contributions <- c(CIresid, contributions)
    names(contributions)[1] <- "residual"
    
    # calculate the % contribution
    pctcontrib <- contributions/sum(contributions) * 100
    
    # return the result
    results <- list(betas = betas, partialcis = indices, confints = confints, averages = averages, 
        contribution = pctcontrib, overallci = CIoverall, correctedCoefficients = corrected, 
        outcomeCorrected = outcomeCorrected, rows = rows)
    class(results) <- "decomposition"
    return(results)   
    }
