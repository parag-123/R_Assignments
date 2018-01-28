getwd()
setwd("/Users/paragmehta/Desktop/R_Coursera")
library("rpart")
data1 = read.csv("chin.csv")
class1 = subset(data1,grade = "1")
class0 = subset(data1,grade = "0")

id1 = sample(nrow(class1), round (nrow(class1)*0.8))
id0 = sample(nrow(class0), round (nrow(class0)*0.8))

train1 =  class1[id1,]
test1 = class1[-id1, ]

train0 = class0[id0, ]
test0 = class0[-id0, ]

train = rbind (train1, train0)
test = rbind (test1,test0)
str(test)

fit = rpart(grade ~ ., train, method = "class", minsplit = 2 )
fit2 = rpart( grade~  age+ factor(ethnicity)+ factor(ER) +factor(PR)+ factor(RT)+ factor(CT)+ factor(HT)+ factor(N) +factor(tumorStage)+ tumorSize
, train, method = "class", minsplit = 30 )
fit3 = rpart( grade~  age+ factor(ethnicity)+ factor(ER) +factor(PR)+ factor(RT)+ factor(CT)+ factor(HT)+ factor(N) +factor(tumorStage)+ tumorSize
              , train, method = "class", minsplit = 30, cp = 0.125 )
fit4 = prune(fit2,cp=0.125)

canpredict = predict(fit4, test, type = "class")
len = length(which(test[,11] == canpredict))
len/nrow(test)
str(canpredict)

library("caret")
confusionMatrix(canpredict,test[,11])

################################
#randomForest
library("randomForest")
fit5 = randomForest(train[, 1:10], factor(train [,11]))
rpredict = predict(fit5, test[,1:10])
confusionMatrix(rpredict,test[,11])
importance(fit5) # find importance of variables.




library("pROC")
roc(fit4$grade, train$grade, plot = "true")




fit$cptable