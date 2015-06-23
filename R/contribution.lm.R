contribution.lm <-
function(object, ranker, correction = TRUE)
    {
    # The ranking variable (wealth, income,...) should be given explicitely.
    # Throw an error if this is not a numeric one
    if (class(ranker) != "numeric") stop("Not a numeric ranking variable")
    
    # extract the outcome of the lm object
    outcome <- object$model[,1]
    
    # extract the model matrix of the lm object
    mm <- model.matrix(object)
    
    # extract the weights of the lm object
    if (!is.null(weights(object))) {
            wt <- weights(object) } else {
            wt <- rep(1, nrow(mm))
            }
        
    # retrieve the coefficients for all variables except the intercept from the model object
    betas <- coefficients(object)[-1]
    
    # call the backend decomposition function
    results <- decomposition(outcome, betas, mm, ranker, wt, correction)
    return(results)
    }

