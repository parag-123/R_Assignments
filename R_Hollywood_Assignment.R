getwd()
hollydata = read.csv("11.csv")
summary(hollydata)
str(hollydata)


samplesize = floor(0.7 * nrow(hollydata))
sampleset = sample (1:nrow(hollydata),samplesize)
hollytrain1 = hollydata[sampleset,]
hollytest1 = hollydata[-sampleset,]

str(hollytrain1)
str(hollytest1)

hollymod1 = lm(total ~ movie_sequel + factor(creative_type)+ factor(source) +  factor(production_method)+ factor(genre) + factor(language)
                ,
               data = hollytrain1)
summary(hollymod1)
library(car)
vif(hollymod1)
hollymod1 = step(hollymod1)
hollymod1_new = lm(formula = total ~ movie_sequel + factor(creative_type) + factor(source) + 
                     factor(production_method), data = hollytrain1)

hollypredict = predict(hollymod1_new, newdata = hollytest1)
summary(hollypredict)
hollytest1$newtotal = hollypredict

write.csv(hollytest1, "hollytest1.csv")



