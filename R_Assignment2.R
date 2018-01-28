
## Answer 1
x.list = list (a=1:10, b = letters [1:3], b= matrix(1:10,ncol=2))

## point 1:
## > x.list$a
## [1]  1  2  3  4  5  6  7  8  9 10

## point 2:
##  > x.list[[2]]
## [1] "a" "b" "c"

## Point 3:
## > x.list[[3]][,1]
## [1] 1 2 3 4 5

##------------------------------------------------------------------------------

## Print numbers from 1 to 5 using repeat and break

a = 1
repeat
{
  
  print(a)
  a = a+1
  if (a > 5)
    break
}

##------------------------------------------------------------------------------

## Check a number is positive or negative using if ... else statement. This will check if the user entered number is positive or negative

{
user_number = readline(prompt = "Enter Positive or Negative Number: ") ## Takes input from user
abc = as.integer(user_number) ## converts entered string to integer

  if (abc > 0) {
   print ("The user entered number is Positive")
   } else if (abc == 0) {
     print ("The user entered number is Zero")
    } else {
      print("The user entered number is Negative")
     }
}
 
##------------------------------------------------------------------------------

## Print numbers from 1 to 10 using while loop

i = 1
while (i <=10)
  {
  print (i)
  i = i+1
}
 
##------------------------------------------------------------------------------

## Create a vector with ‘N’ elements and find the sum of N numbers using while loop

a <- c(1:10)

i=1
x=0
while (i <= length(a))
{
  
  x = a[i]+x
  i = i+1
}
print (paste("Sum of all elements in vector: ", x))

##------------------------------------------------------------------------------

## Create a vector with ‘N’ elements and find the sum of N numbers using  for  loop

a <- c(1:10)

x=0
for (i in 1:length(a)){
  
  x = a[i]+x

}
print (paste("Sum of elements invector: " , x))

##------------------------------------------------------------------------------

## Create a vector with ‘N’ elements and count number of even numbers in the vector

a <- c(1:15)

x=0
for (i in 1:length(a)){
  if (a[i]%%2 == 0){
    x=x+1
  }
    
}
print (paste("Even Numbers in vector: ", x))

##------------------------------------------------------------------------------

## Create a 3× 4 matrix. Find the min and max value from the matrix using Loops 

mymatrix = matrix(c(23,56,47,98,100,435,6789,89,34,12,54,17), ncol= 3,nrow=4)

min = mymatrix[1,1]
max = mymatrix[1.1]

for (i in 1:nrow(mymatrix)) {
  for (j in 1:ncol(mymatrix)){
    
    if (mymatrix[i,j] < min) {
      min = mymatrix[i,j]
      
    }
    if (mymatrix[i,j] > max){
      max = mymatrix[i,j]
    }
  }
}
print (paste (max,min))


