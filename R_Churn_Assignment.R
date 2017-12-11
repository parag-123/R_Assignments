setwd("D:\\Project\\parag Personal\\aegis")
getwd()
churn_data = read.csv("Churn .csv")
str(churn_data)
summary(churn_data)
names(churn_data)
head(churn_data)

class1 = subset(churn_data,Churn ==1) 
class0= subset(churn_data, Churn ==0)

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

str(train)
str(test)

library(rpart)
churn_mod = rpart(Churn ~ CustServ.Calls+Eve.Charge+Intl.Charge+Night.Charge+Day.Charge, data = churn_data,  method = "class", minsplit = 30 )
churn_mod1 = prune(churn_mod,cp=0.010)
printcp(churn_mod1) # display the results
plotcp(churn_mod1) # visualize cross-validation results
summary(churn_mod1) # detailed summary of splits
str(churn_mod1)

churnpredict = predict(churn_mod1,test, type ="class")
str(churnpredict)

library("caret")
library("e1071")
confusionMatrix(churnpredict, (test$Churn))


library(rpart)
f = rpart(Churn~CustServ.Calls+Eve.Calls+Intl.Calls+Night.Calls
        +Day.Calls,method="class", data=churn_data)
plot(f, uniform=TRUE,main="Classification Tree for Churn")
text(f, use.n=TRUE, all=TRUE, cex=.7)
plotcp(f,lty=4,col="red")

########################################################################
# plot trees
plot(churn_mod1, uniform=TRUE,
     main="Classification Tree for Churn")
text(churn_mod1, use.n=TRUE, all=TRUE, cex=.8)




