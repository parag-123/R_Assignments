# Write a function  to find the standard deviation of a matrix, using apply function
std.dev.matrix = function (x){
    
    my.mean =  apply(x, 1, mean)
    my.mean1 =  apply(x, 2, mean)
    main.mean = mean(my.mean,my.mean1, trim = .0, na.rm=TRUE)
    
    square = apply(my.matrix,c(1,2), function (x) (x-main.mean)^2)
    square.mean =  apply(square, 1, mean)
     square.mean1 =  apply(square, 2, mean)
     square.mean2 = mean(square.mean,square.mean1, trim =.0, na.rm=TRUE)
    std.dev = round(sqrt(square.mean2))
    print (paste( "Standard Deviation of matrix = ", std.dev))
}
SD_matrix = my.matrix = matrix(1:10,5,2)
std.dev.matrix(SD_matrix)

# Solution 2

plot(1:10,1:10,type="n")
for(i in 1:10){ 
  lines(c(i,i),
        c(1,20)) 
} 
for(j in 1:20){
  lines(c(1,10),c(j,j)) 
}

# Solution 3

plot(1:10,1:10) 
for(i in 1:10){ 
  for(j in 1:20){ 
    points(i,j) 
  }
}

#Solution 4

plot(1:10,1:10) 
for(j in 1:20){
  color <-
    if( j %% 2 ==0 ){ "blue" } else { "red"}
  lines( c(1,10), c(j,j), col=color )
}

#Can you make line color alternate between ”red”, ”blue”, and ”green”?
plot(1:10,1:10) 
for(j in 1:20){
  color <-
    if (j %% 3 == 1 ) {"red"}
    else if (j %% 3 == 2 ) {"blue"}
      else {"green"}
  
  lines( c(1,10), c(j,j), col=color )
}



