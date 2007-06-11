`cancel.fraction` <-
function(numerator,denominator){
	
	test <- is.whole(numerator)
	if(test==FALSE){
		msg <- cat("Please enter a whole numerator\r")
		return(msg)
		}

	test <- is.whole(denominator)
	if(test==FALSE){
		msg <- cat("Please enter a whole denominator\r")
		return(msg)
		}
	test <- is.prim(numerator)
	if(test==FALSE){
		ggT <- gcd(numerator, denominator)
		
		numerator <- numerator/ggT
		denominator <- denominator/ggT
		
		}
	 	
 	cat(numerator, "/", denominator)
 	#return(msg)
 	}

