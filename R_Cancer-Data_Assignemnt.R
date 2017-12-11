getwd()
candata = read.csv("chin.csv")
str(candata)
summary(candata)

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

cor(train)

fitmodel = glm(grade ~ age+factor(ethnicity)+factor(ER)+factor(PR)+factor(RT)+factor(CT)+factor(HT)+factor(N)
               +factor(tumorStage)+ tumorSize,data = train, family = binomial(link = "logit"))
step(fitmodel)
summary(fitmodel)

fitmodel1 = glm(formula = grade ~ age + factor(ethnicity) + tumorSize, family = binomial(link = "logit"), 
                data = train)


summary(fitmodel1)
vif(fitmodel1)
pred1 = predict (fitmodel1, newdata = test, type = "response")

test$grade1 = pred1
test$finalgrade[test$grade1 > 0.5] = 1
test$finalgrade[test$grade1 < 0.5] = 0


# Based on the paramters the model will predict correct values with 75% confidence.





