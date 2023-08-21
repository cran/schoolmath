####################################################
# In order to let roxygen2() create all man pages, #
# you must run the following command:              #
#       roxygen2::roxygenise()                     #
####################################################



#' cancel a fraction to the smallest numbers
#'
#' returns a frequency table with absolute and relative frequencies and cumulated frequencies
#' @param numerator the fraction's numerator
#' @param denominator the fraction's denominator
#' #'
#' @return Character string
#'
#' @examples
#' cancel.fraction(40,15)
#' cancel.fraction(42, 56)
#'
#' @export
#---------------------------------------------------------------

cancel.fraction <-
  function (numerator, denominator)
  {
    test <- is.whole(numerator)
    if (test == FALSE) {
      msg <- paste("Please enter a whole numerator\r")
      return(msg)
    }
    test <- is.whole(denominator)
    if (test == FALSE) {
      msg <- paste("Please enter a whole denominator\r")
      return(msg)
    }
    ## The following line was modified.
    test <- is.prim(denominator)  ## Initially, is.prim(numerator), actually, I am not sure this test is important!
    ## End of modifications
    if (test == FALSE) {
      ggT <- gcd(numerator, denominator)
      numerator <- numerator/ggT
      denominator <- denominator/ggT
    }
    msg <- paste(numerator, "/", denominator)
    return(msg)
  }
#---------------------------------------------------------------





#' checks if a number is decimal or integer
#'
#' @param x the number to check
#' #'
#' @return true or false
#'
#' @examples
#' is.decimal(40.15)
#' is.decimal(4015)
#'
#' @export
#---------------------------------------------------------------

is.decimal <-
  function(x){
    start <- 1
    end <- length(x)+1
    while(start<end){
      y <- x[start]

      test <- floor(y)
      if(y==test){
        if(start==1){
          result=FALSE
        }else{
          result<- c(result,FALSE)
        }

      }else{
        if(start==1){
          result=TRUE
        }else{
          result <- c(result,TRUE)
        }
      }
      start <- start+1
    }

    return(result)
  }
#---------------------------------------------------------------




#' check whether a vector contains numbers with decimal places
#'
#' @param x the number or vector to check
#' #'
#' @return true or false
#'
#' @examples
#' is.whole(3.12)  # this will return FALSE
#' is.whole(2)     # this will return TRUE
#'
#' x <- c(1, 2, 3, 4, 5.5, 6.03, 23.07)
#' is.whole(x)
#'
#' @export
#---------------------------------------------------------------

is.whole <-
  function(x){
    start <- 1
    end <- length(x)+1
    while(start<end){
      y <- x[start]

      test <- floor(y)
      if(y==test){
        if(start==1){
          result=TRUE
        }else{
          result<- c(result,TRUE)
        }

      }else{
        if(start==1){
          result=FALSE
        }else{
          result <- c(result,FALSE)
        }
      }
      start <- start+1
    }

    return(result)
  }


#---------------------------------------------------------------







#' checks if a number or vector is even
#'
#' @param x the number to check
#' #'
#' @return true or false
#'
#' @examples
#' is.even(45)
#' is.even(46)
#' x <- c(1,2,3,4,5, 6, 7)
#' is.even(x)
#'
#' @export
#---------------------------------------------------------------

is.even <-
  function(x){
    start <- 1
    end <- length(x)+1
    while(start<end){
      y <- x[start]

      test1 <- y/2
      test2 <- floor(test1)
      if(test1 == test2) {
        if(start==1){
          result=TRUE
        }else{
          result<- c(result,TRUE)
        }

      }else{
        if(start==1){
          result=FALSE
        }else{
          result <- c(result,FALSE)
        }

      }
      start <- start+1
    }
    return(result)
  }

#---------------------------------------------------------------


#' checks if a number or vector is odd
#'
#' @param x the number or vector to check
#' #'
#' @return true or false
#'
#' @examples
#' is.odd(45)
#' is.odd(46)
#' x <- c(1,2,3,4,5, 6, 7)
#' is.odd(x)
#'
#' @export
#---------------------------------------------------------------

is.odd <-
  function(x){
    start <- 1
    end <- length(x)+1
    while(start<end){
      y <- x[start]
      if(y==0){
        return("Please enter a number unequal to 0")
      }
      test1 <- y/2
      test2 <- floor(test1)
      if(test1 != test2) {

        if(start==1){
          result=TRUE
        }else{
          result<- c(result,TRUE)
        }

      }else{
        if(start==1){
          result=FALSE
        }else{
          result <- c(result,FALSE)
        }
      }
      start <- start+1
    }

    return(result)
  }

#---------------------------------------------------------------



#' check whether numbers of a vector are negative
#'
#' @param x the number or vector to check
#' #'
#' @return true or false
#'
#' @examples
#' is.negative(3)  # this will return FALSE
#' is.negative(-2)  # this will return TRUE
#'
#' x <- c(-1, -2, 3.02, 4, -5.2, 6, -7)
#' is.negative(x)
#'
#' @export
#---------------------------------------------------------------

is.negative <-
  function(x){
    start <- 1
    end <- length(x)+1
    while(start<end){
      y <- x[start]

      if(y<0) {
        if(start==1){
          result=TRUE
        }else{
          result<- c(result,TRUE)
        }

      }else{
        if(start==1){
          result=FALSE
        }else{
          result <- c(result,FALSE)
        }
      }
      start <- start+1
    }
    return(result)
  }



#---------------------------------------------------------------


#' check whether numbers of a vector are positive
#'
#' @param x the number or vector to check
#' #'
#' @return true or false
#'
#' @examples
#' is.positive(-3)   # this will return FALSE
#' is.positive(2)  # this will return TRUE
#'
#' x <- c(-1, -2, 3.02, 4, -5.2, 6, -7)
#' is.positive(x)
#'
#' @export
#---------------------------------------------------------------

is.positive <-
  function(x){
    start <- 1
    end <- length(x)+1
    while(start<end){
      y <- x[start]

      if(y>0) {
        if(start==1){
          result=TRUE
        }else{
          result<- c(result,TRUE)
        }

      }else{
        if(start==1){
          result=FALSE
        }else{
          result <- c(result,FALSE)
        }
      }
      start <- start+1
    }
    return(result)
  }




#---------------------------------------------------------------


#' check whether numbers of a vector are real positive. Real positive means, that zero is included as a positive number.
#'
#' @param x the number or vector to check
#' #'
#' @return true or false
#'
#' @examples
#' is.real.positive(-3)   # this will return FALSE
#' is.real.positive(0)    # this will return TRUE
#'
#' x <- c(0, -1, -2, 3.02, 4, -5.2, 6, -7)
#' is.real.positive(x)
#'
#' @export
#---------------------------------------------------------------

is.real.positive <-
  function(x){
    start <- 1
    end <- length(x)+1
    while(start<end){
      y <- x[start]

      if(y>=0) {
        if(start==1){
          result=TRUE
        }else{
          result<- c(result,TRUE)
        }

      }else{
        if(start==1){
          result=FALSE
        }else{
          result <- c(result,FALSE)
        }
      }
      start <- start+1
    }
    return(result)
  }

#---------------------------------------------------------------



#' check whether a vector contains prime-numbers
#'
#' @param y the number or vector to check
#'
#' @return true or false
#'
#' @examples
#' is.prim(8)  # this will return FALSE
#' is.prim(11) # this will return TRUE
#'
#' x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
#' is.prim(x)
#'
#' @export
#---------------------------------------------------------------

is.prim <-
  function(y){
    starten <- 1
    enden <- length(y)+1
    while(starten<enden){
      x <- y[starten]

      error <- 0

      test <- is.negative(x)
      if(test==TRUE){
        x <- x*(-1)
      }

      #test ob ganze Zahl
      test <- is.whole(x)
      if(test==FALSE) {
        #cat("keine ganze Zahl\r")
        error <- error+1
      }

      # 0 ist keine Primzahl
      if(x==0){
        ##cat("ist 0 \r")
        error <- error+1
      }

      #test ob gerade Zahle
      test <- is.even(x)
      if(test==TRUE & x!=2){
        #cat("ist gerade\r")
        error <- error+1
      }

      # test ob Quadratwurzel
      test <- is.whole(sqrt(x))
      if(test==TRUE & x!=1){
        #cat("ist wurzel\r")
        error <- error+1
      }

      #### los gehtz
      if(error>0){
        #cat("setze FALSE\r")
        if (starten==1){
          result=FALSE
        }else{
          result <- c(result, FALSE)
        }
      }else{
        ##### Teste, ob Primzahl
        if(x==1 |x==2|x==3|x==5|x==7){
          #cat("ist 2, setze TRUE\r")
          if (starten==1){
            result=TRUE
          }else{
            result <- c(result, TRUE)
          }
        }else{
          anfang <- 3
          ende <- ceiling(sqrt(x))
          #cat("potentiell\r")
          while(anfang<ende){

            test1 <- x/anfang
            test2 <- floor(test1)

            if(test1==test2){
              error <- error+1
            }

            anfang <- anfang+2
          }
          if(error==0){
            if (starten==1){
              result=TRUE
            }else{
              result <- c(result, TRUE)
            }
          } else{
            if (starten==1){
              result=FALSE
            }else{
              result <- c(result, FALSE)
            }
          }
        }

      }

      starten <- starten+1
    }
    return(result)
  }

#---------------------------------------------------------------






#' generate prime-numbers in a range from START to END
#'
#' @param start the number to start from
#' @param end the number to end
#' #'
#' @return a vector of prime numbers
#'
#' @examples
#' primes(12,150) # list prime-numbers between 12 and 150
#'
#' @export
#---------------------------------------------------------------

primes <-
  function(start=12, end=9999){
    dummy  <- 0
    dummyr <- 0
    first <- 0
    nixda <- 0
    test <- is.negative(start)
    if(test==TRUE) {start <- start*(-1)}
    test <- is.negative(end)
    if(test==TRUE){end <- end*(-1)}
    if(end<start) {
      xyz <- start
      start <- end
      end <- xyz
    }
    if(start<18){
      primzahlen <- c(1,2,3,5,7,11,13,17)
      first <- 1
      start <- 18
    }
    # befinde ich mich in der 3er-Reihe?
    test <- is.even(start)
    if(test==TRUE){start <- start+1}
    test <- start/3
    test2 <- floor(test)
    if(test==test2){
      dummy3 <-1
    }else{
      dummy3 <- 0
      dummy <- 1
    }
    ## Test auf Primzahl
    while(start<(end+1)){
      test1 <- 5
      test2 <- ceiling(sqrt(start))
      while(test1 < (test2+1)){

        primtest1 <- start/test1
        primtest2 <- floor(primtest1)
        if(primtest1==primtest2){
          nixda <- nixda + 1
        }

        if(dummyr==0){
          test1 <- test1 + 2
          dummyr <- 1
        }else{
          test1 <- test1 + 4
          dummyr <- 0
        }
      }

      if(nixda==0){
        #cat("Primzahl gefunden: ",start,"\r")
        if(first==0){
          primzahlen <- start
          first <- 1
        }else{
          primzahlen <- c(primzahlen, start)
        }

      }
      nixda <- 0
      ## 2er und 3er auslassen
      #cat (c(start, "\r"))
      if(dummy==1){
        start <- start+4
        dummy<-0
      }else{
        start <- start+2
        dummy <- 1
      }
      if(dummy3==1){
        dummy <- 0
        dummy3 <- 0
      }
    }

    return(primzahlen)
  }

#---------------------------------------------------------------


#' This function calculates the prime-factors of a number
#'
#' @param n the number to be checked
#' #'
#' @return a vector with the prime factors
#'
#' @examples
#' prime.factor(21)
#' prime.factor(100)
#'
#' @export
#---------------------------------------------------------------

prime.factor <-
  function (n)
  {
    primlist <- schoolmath::primlist
    dummy <- 2
    end <- 0
    faclist <- 0
    test <- is.prim(n)
    if (test == TRUE) {
      ## The two following lines were modified:
      msg <- paste(n, "is a prime!\n")  ## Initially \r instead of \n, why?
      return(msg)                       ## initially return(msg)
      ## End of modifications
    }
    while (end == 0) {
      prim <- primlist[dummy]
      if (prim > n) {
        end <- 1
      }
      else {
        test <- n/prim
        test2 <- is.whole(test)
        if (test2 == TRUE) {
          faclist <- c(faclist, prim)
          test3 <- is.prim(test)
          if (test3 == TRUE) {
            end <- 1
            faclist <- c(faclist, test)
          }
          else {
            n <- test
            dummy <- 1
          }
        }
        dummy <- dummy + 1
      }
    }
    faclist <- faclist[-1]
    return(faclist)
  }


#---------------------------------------------------------------














#' convert a decimal-number into fraction
#'
#' @param decimal the decimal number to be converted, given without an repeating ending
#' @param period if the decimal places have an repeating ending (period), set the period here. See examples.
#' #'
#' @return a character string with the fraction.
#'
#' @examples
#' ## converting 23.4323
#' decimal2fraction(23.4323)
#'
#' ## converting a number with decimal period, e.g. 12.12344444444444444444
#' decimal2fraction(12.123, 4)
#'
#' @export
#---------------------------------------------------------------

decimal2fraction <-
  function(decimal, period=0){
    dummy<-0
    ganze <- floor(decimal)
    anzahl.ganze <- nchar(ganze)
    nuller <- nchar(decimal) - (anzahl.ganze+1) # wird abgezogen
    nenner1 <- 10^nuller
    zaehler1 <- floor(decimal*nenner1)
    if(period<0) period <- period*(-1)
    if(period>0){
      test <- is.whole(period)
      if(test==FALSE){
        msg <- "period must be a whole number\r"
        return(msg)
      }
      dummy <-1
      zaehler2 <- period
      nenner2 <- nenner1*9

      zaehler1 <- zaehler1*9
      nenner1 <- nenner1*9

      zaehler3 <- zaehler1 + zaehler2
      nenner3 <- nenner2
    }

    if(dummy==0) {
      zaehler3 <- zaehler1
      nenner3 <- nenner1
    }
    test1 <- is.prim(zaehler3)
    if(test1==FALSE){
      ggT <- gcd(zaehler3,nenner3)
      zaehler3 <- zaehler3/ggT
      nenner3 <- nenner3/ggT

    }

    #cat("\r--------------\r")
    msg <- paste(zaehler3,"/",nenner3)
    return(msg)
  }

#---------------------------------------------------------------




#' Greatest common divisor of two numbers
#'
#' @param x first number
#' @param y second number
#' #'
#' @return numeric greatest common divisor
#'
#' @examples
#' gcd(42, 56)
#'
#' @export
#---------------------------------------------------------------

gcd <-
  function(x,y){
    gcd <- 1
    liste <- NULL
    a <- prime.factor(x)
    b <- prime.factor(y)
    a1 <- unique(a)
    b1 <- unique(b)
    c <- length(a1)
    d <- length(b1)
    if(c<d){
      gemeinsam <- is.element(a1,b1)
      switch <- a1
    }else{
      gemeinsam <- is.element(b1,a1)
      switch <- b1
    }
    gem.l <- length(gemeinsam)
    start <- 1
    end <- gem.l+1
    while(start<end){
      if(gemeinsam[start]==TRUE){
        liste <- c(liste, switch[start])
      }
      start <- start+1
    }
    liste.l <- length(liste)
    start <-1
    end <- liste.l+1
    while(start<end){
      dieser <- liste[start]
      test1 <- sum(as.integer(is.element(a, dieser)))
      test2 <- sum(as.integer(is.element(b, dieser)))
      if(test1<test2){
        zwischen <- liste[start]^test1
      }else{
        zwischen <- liste[start]^test2
      }
      gcd <- gcd*zwischen
      start <- start+1
    }
    return(gcd)
  }

#---------------------------------------------------------------




#' calculating the smallest common multiple of two numbers
#'
#' @param x first number
#' @param y second number
#' #'
#' @return numeric least common multiple
#'
#' @examples
#' scm(3528, 3780)
#'
#' @export
#---------------------------------------------------------------

scm <-
  function(x,y){
    scm <- 1
    liste <- NULL
    a <- prime.factor(x)
    b <- prime.factor(y)
    aa <-a
    a1 <- unique(a)
    bb <-b
    b1 <- unique(b)
    c <- length(a1)
    d <- length(b1)
    if(c<d){
      gemeinsam <- is.element(a1,b1)
      switch <- a1
    }else{
      gemeinsam <- is.element(b1,a1)
      switch <- b1
    }
    gem.l <- length(gemeinsam)
    start <- 1
    end <- gem.l+1
    while(start<end){
      if(gemeinsam[start]==TRUE){
        liste <- c(liste, switch[start])
      }
      start <- start+1
    }
    liste.l <- length(liste)
    start <-1
    end <- liste.l+1
    while(start<end){
      dieser <- liste[start]
      test1 <- sum(as.integer(is.element(a, dieser)))
      test2 <- sum(as.integer(is.element(b, dieser)))
      aa <- aa[aa!=dieser]
      bb <- bb[bb!=dieser]
      if(test1>test2){
        zwischen <- liste[start]^test1
      }else{
        zwischen <- liste[start]^test2
      }
      scm <- scm*zwischen
      start <- start+1
    }
    rest <- c(aa,bb)
    rest.ende <- length(rest)+1
    start<-1
    while(start<rest.ende){
      scm <- scm*rest[start]
      start <- start+1
    }
    return(scm)
  }
#---------------------------------------------------------------


