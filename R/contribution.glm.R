contribution.glm <-
function(object, ranker, correction = TRUE) {
    # The ranking variable (wealth, income,...) should be given explicitely.
    # Throw an error if this is not a numeric one
    if (class(ranker) != "numeric") stop("Not a numeric ranking variable")
    
    # extract the outcome of the glm object
    outcome <- predict(object)
    
    # extract the model matrix of the glm object
    mm <- model.matrix(object)
    
    # extract the weights of the glm object
    wt <- object$prior.weight
        
    # retrieve the coefficients for all variables except the intercept from the model object
    betas <- coefficients(object)[-1]
    
    # call the backend decomposition function
    results <- decomposition(outcome, betas, mm, ranker, wt, correction)
    return(results)
}
