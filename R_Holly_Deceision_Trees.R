getwd()
setwd("D://Project//parag Personal//aegis")

hollydata = read.csv("t1.csv")
summary(hollydata)
str(hollydata)
head(hollydata)

library("rpart")
samplesize = floor(0.7 * nrow(hollydata))
sampleset = sample (1:nrow(hollydata),samplesize)
hollytrain1 = hollydata[sampleset,]
hollytest1 = hollydata[-sampleset,]

str(hollytrain1)
str(hollytest1)


hollymod1 = rpart(factor(Category) ~ movie_sequel + factor(creative_type) + factor(Source) + 
                    factor(production_method)+ factor(genre), data = hollytrain1,  method = "class", minsplit = 30 )

# hollymod2 = rpart(Category ~ . , data = hollytrain1, method = "class", minsplit = 30 )
hollymod3 = prune(hollymod1,cp=0.010)


printcp(hollymod1) # display the results
plotcp(hollymod1) # visualize cross-validation results
summary(hollymod3) # detailed summary of splits
str(hollymod3)

hollypredict = predict(hollymod3,hollytest1, type ="vector")
#hollypredict = predict(hollymod1, hollytest1, type ="class")


#hollytest1$newtotal = hollypredict

#write.csv(hollytest1, "hollytest2.csv")

str(hollypredict)
str(hollytest1[,14])
str(hollytest1$Category)

#levels(hollypredict)
#levels(factor(hollytest1[,14]))

#table(hollypredict)
#table(hollytest1[,14])
#str(hollytest1)


#len = length(which(hollytest1[,14] == hollypredict))
#Acc= (len/nrow(hollytest1)) * 100

library("caret")
confusionMatrix(hollypredict,factor(hollytest1[,14]))


# plot trees
plot(hollymod1, uniform=TRUE,
     main="Classification Tree for HollyData")
text(hollymod1, use.n=TRUE, all=TRUE, cex=.8)
print(importance(hollymod2,type = 2)) 


library(randomForest)
hollymod2 = randomForest((Category) ~ movie_sequel + (creative_type) + (Source) + 
                    (production_method)+ (genre), data = hollytrain1,  ntree=500 )

hollypredict2 = predict(hollymod2,hollytest1)
len = length(which(hollytest1[,14] == hollypredict2))
Acc= (len/nrow(hollytest1)) * 100

print(hollymod2)
print(importance(hollymod2,type = 2)) 

