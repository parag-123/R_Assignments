library("class")
getwd()
setwd("/Users/paragmehta/Desktop/R_Coursera")
candata = read.csv("chin.csv")
str(candata)
summary(candata)

#Scaling for continous or binary data columns
minmax=function(x){
  xnew = (x-min(x))/max(x)
}
condata[,1]= minmax(candata[,1])
condata[,9]= minmax(candata[,9])
condata[,10]= minmax(candata[,10])

class1 = subset(candata,grade ==1) 
class0= subset(candata, grade ==0)

smpsize1 = floor(0.70 * nrow(class1))
smpsize2 = floor(0.70* nrow(class0))

train_ind1 = sample((seq_len((nrow(class1)))), size = smpsize1)
train_ind0 = sample((seq_len((nrow(class0)))), size = smpsize2)

train0 = class0[train_ind0, ]
train1 = class1[train_ind1, ]

test0 = class0[-train_ind0, ]
test1 = class1[-train_ind1, ]

train = rbind(train0,train1)
test = rbind(test0,test1)

acc = c(0)

for (i in seq(1,20,2)){
di = knn.cv(train[-11], factor(train[,11]), k = i, use.all = TRUE)
count =0
for (j in 1:length (di)){
  
  if (train[j,11]==di[j]){count = count +1}
}
acc = c(acc, count)
}
plot(acc/79)

# Aply value of k on test data
dd = knn(train[,-11], test[,11], factor(train[,11]), k = 4, use.all = TRUE)
  count = 0
#compare train and test data
for (j in 1:length (di)){
  
  if (test[j,11]==di[j]){count = count +1}
}
  
