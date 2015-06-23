contribution.coxph <-
function(object, ranker, correction = TRUE)
    {
    # The ranking variable (wealth, income,...) should be given explicitely.
    # Throw an error if this is not a numeric one
    if (class(ranker) != "numeric") stop("Not a numeric ranking variable")
    
    # extract the outcome of the coxph object
    outcome <- object$linear.predictor
    
    # extract the model matrix of the coxph object
    mm <- model.matrix(object)[,names(object$coefficients)]
    
    # extract the weights of the coxph object
    wt <- weights(object)
        
    # retrieve the coefficients for all variables except the intercept from the model object
    betas <- coefficients(object)[-1]
    
    # call the backend decomposition function
    results <- decomposition(outcome, betas, mm, ranker, wt, correction)
    return(results)
    }

