pow <- function(x, y) { 
    # function to print x raised to the power y 
    result <-x^y 
    print(result) 
    } 
pow(8,2) 
# [1] 64
pow(10,2) 
# [1] 100

##################################################

check <-function(x) { 
    if (x > 0) { 
      result <-"Positive" 
    } else if (x<0) { 
      result <-"Negative" 
    } else { 
      result <-"Zero" 
    } 
    return(result) 
    } 
check(1) 
# [1] "Positive"
check(-110) 
# [1] "Negative"
check(0) 
# [1] "Zero"

############################################################

multi_return = function(){
  
  my_list = list("color"= "red", "size" = 20, "shape" = "round")
  return(my_list)
}
a = multi_return()
a$color
# [1] "red"
a$shape
# [1] "round"

#######################################################
recursive.factorial = function(x){
  
  if (x==0) return (1)
  else return (x*recursive.factorial(x-1))
}
recursive.factorial(5) 
# [1] 120
recursive.factorial(7) 
#[1] 5040
recursive.factorial(120) 
#[1] 6.689503e+198
recursive.factorial(1)
# [1] 0
recursive.factorial(0) 
# [1] 0

#######################################################
'%divisible%' = function (x,y){
  
  if (x%%y ==0) return (TRUE) 
  else return (FALSE) 
}
0 %divisible% 3 
# [1] TRUE
10 %divisible% 2 
# [1] TRUE
`%divisible%`(10,5) 
# [1] TRUE
10 %divisible% 3 
# [1] FALSE


############################################
## Write a function which find sum of two matrices and return the sum as matrix. (Check the size compatibility of matrix). 

matrix.sum = function(x,y){
  if (length(x)==length(y)){
    sum.matrix = x+y
    return(sum.matrix)
  } else {
    print("Matrix are not of same size")
  }
}

##########################################
#7. Write a function which returns nth power of a number x. 

power.number = function(x,n) {
  
  value = x^n
  return (value)
  
}

################################
#Create a function ‘summary’ which calculates minimum, maximum and sum of a matrix using ‘switch’ statement.

summary = function (x, type) {
  switch (type,
          minium = min(x),
          maximum = max(x),
          sumofmatrix = sum(x))
  }

#################################################

#User defined infix operator
x = matrix(c(11, 13, 14, 20, 40, 91), ncol = 3)
y = matrix(c(91, 12, 41, 11, 10, 21), ncol = 3)

'%common%' = function (x,y) {
  count = 0
  for (i in 1:nrow(x)){
    for (j in 1:ncol(x)){
      for (a in 1:nrow(y)){
        for (b in 1:ncol(y)){ 
          if (x[i,j] - y[a,b] == 0) {
            count = count +1
            }
    }
      }
    }
  }
  return(count)
}

x%common%y
