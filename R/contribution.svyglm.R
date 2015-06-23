contribution.svyglm <-
function(object, ranker, correction = TRUE)
    {
    # The ranking variable (wealth, income,...) should be given explicitely.
    # Throw an error if this is not a numeric one
    if (class(ranker) != "numeric") stop("Not a numeric ranking variable")
    
    # extract the outcome of the svyglm object
    outcome <- predict(object, newdata = object$model[,-1], vcov = FALSE)
    
    # extract the model matrix of the svyglm object
    mm <- model.matrix(object)
    
    # extract the weights of the svyglm object
    wt <- 1 / object$survey.design$prob
        
    # retrieve the coefficients for all variables except the intercept from the model object
    betas <- coefficients(object)[-1]
    
    # call the backend decomposition function
    results <- decomposition(outcome, betas, mm, ranker, wt, correction)
    return(results)
    }
