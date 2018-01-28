#1
numeric_vector = c(1,2,3,4,5,6,7,8)

#2
char_vec = as.character(numeric_vector)

#3
name = c("sai", "prasad", "rahul")
length_name = length(name)
name[c(1,2)]
name[c(2,3)]

#4
vec1 = c(rep("a",2), 1:9)

#5
vec2 = c(1,2,NA,4)
vec3 = vec2[!is.na(vec2)]

#6
mySample = sample(1:100, 50, replace = TRUE)
  
#7
class(mySample)

#8
class(iris)
summary(iris)

#Top 6 fields
top6 = head(iris)
spreadsheet = write.csv(top6, "top6.csv")

#Class of columns
class(iris$Sepal.Width)
class(iris$Sepal.Length)
class(iris$SPetal.Length)
class(iris$Petal.Width)
class(iris$Species)

#Col/row names and number
colum = colnames(iris)
row = rownames(iris)
Rows = nrow(iris)
ncol(iris)

#Last 2 rows in last 2 columns
iris[c(149,150), c("Petal.Width", "Species")]

#Get rows with sepal.width > 3.5
subset(iris, iris$Sepal.Width>3.5)
iris[which(iris[,"Sepal.Width"]>3.5), ]

#9

charvec = character(nrow(iris))
for (i in 1:nrow(iris)){
  if (iris$Sepal.Length[i] > 5){
    
    charvec[i] <- "greater than 5"

  } else {
    charvec[i] <- "lesser than 5"
  }
}
  


