## ci()
## A function to calculate the concentration index
## last update: 12/07/2015
## ------------------------------------------------------------------------#


ci <-
function(x, y, wt = NULL, type = NULL) {

	# x: income, wealth,... variable (required)
	# y: health variable (required)
	# type: type of concentration index (optional): 
	#	"CI" : relative concentration index (by default)
	#	"CIg": generalized concentration index                     		
	# "CIc": concentration index with Erreygers Correction
	# wt: sampling weights (optional)
	
	# if no wt argument is given (default), set all sampling weights to 1.
	# sum(weights) = 1, see Lerman & Yitzhaki (1989), par. 2
	if (is.null(wt)) wt <- rep(1, length(x))
	
	# drop all observations with missing values
	if (length(wt) == 1) wt <- rep(wt, length(x))
	mydf <- data.frame(x = x, y = y, wt = wt)
	mydf <- na.omit(mydf)
	mydf$wt <- mydf$wt / sum(mydf$wt)
	
	# calculate weighted average of the health variable
	ybar <- weighted.mean(mydf$y, w = mydf$wt)

	# calculate weighed rank of the wealth variable
	R <- rank_wt(x = mydf$x, wt = mydf$wt)

	# calculate average rank
	Rbar <- mean(R)
	n <- nrow(mydf)
	
	# calculate ci for weighted microcase data
	# see Lerman & Yitzhaki eq (3)
	concentrationIndex <- 2 * sum(mydf$wt * (mydf$y - ybar) * (R - Rbar) / ybar)
	
	# calculate individual's contribution to ci
	# this is a vectorized form of the previous
	myOrder <- order(R)
	mydf <- mydf[myOrder,]
	R <- R[myOrder]

	# calculate variance of the ci for weighted microcase data
	# see Kakwani et al (eq 14) with qi = qt, R = ft and y = x
	qi <- cumsum(mydf$y) / sum(mydf$y)
	qilag <- c(0, qi[-n])
	ai <- mydf$y / ybar * (2 * R - 1 - concentrationIndex) + 
			2 - qilag - qi
	varC <- (sum(ai^2) / n - (1 + concentrationIndex)^2) / n

	# calculate the different type of concentration index
	
      if (is.null(type)) {
                 concentrationIndex <- concentrationIndex
                 type = "CI"	
      } else if (type == "CI"){
                concentrationIndex <- concentrationIndex
      } else if (type == "CIg"){
                concentrationIndex <- concentrationIndex *  ybar
      } else if (type == "CIc"){
                concentrationIndex <- 4 * concentrationIndex *  ybar / 
                                      (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))

      }



	# return an object of class hci
	ci <- list(concentrationIndex = concentrationIndex,type = type, variance = varC, 
			fractionalRank = R, outcome = mydf$y, 'call' = match.call(), n = nrow(mydf))
	class(ci) <- "hci"
	return(ci)
}

