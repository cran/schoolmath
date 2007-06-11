`prime.factor` <-
function(n){
	data(primlist)
	dummy<-2
	end <-0
	faclist <-0
	test <- is.prim(n)
	
	if(test==TRUE){
		msg <- cat(n,"is a prime!\r")
		return(msg)
		}
	while(end==0){
		prim <- primlist[dummy]
		if(prim>n){
			end <- 1
			}else{
			
			#cat(prim)
			test <- n/prim
			test2 <- is.whole(test)
			if(test2==TRUE){
				faclist <- c(faclist,prim)
				test3 <- is.prim(test)
				if(test3==TRUE){
					end <- 1
					faclist <- c(faclist, test)
					}else{
					n<-test
					dummy <- 1
					}
				}
			
			dummy <-dummy+1
			}
		}	
	faclist <- faclist[-1]
	return(faclist)
	}

