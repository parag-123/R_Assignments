getwd()
# ML ALGO USED: DECEISION TREES & Logistic Regression

setwd("/Users/paragmehta/Desktop/R_coursera/")
library(rpart)

# Read the data set
spam_data= read.csv("spam data.csv")

#Structure and details about the data
str(spam_data)
summary(spam_data)
head(spam_data)
spam_data$spambase_data

#Check any corelation between the data variabled
cor(spam_data) # It is found that no variables are co-related

#Check Class of each variable
sapply(spam_data[1, ], class)

#Check for NA values, if any
sum(is.na(spam_data))

#Number of NON SPAM EMAILS in dataset
output <- table(spam_data$spambase_data)
total_email_Data <- output["0"]
print (paste("total NON SPAM EMAILS in dataset are: " , total_email_Data))
print(paste("Percentage of NON SPAM EMAILS in dataset: ", round((total_email_Data/nrow(spam_data)) * 100, 2), "%"))

#Number of SPAM EMAILS in Dataset
output1 <- table(spam_data$spambase_data)
total_spam_dataset <- output1["1"]
print (paste("total SPAM EMAILS in dataset are: " , total_spam_dataset))

print(paste("Percentage of SPAM EMAILS in dataset: ", round((total_spam_dataset/nrow(spam_data)) * 100, 2), "%"))

# Create Test & Train data

#Dividing dataset base on class
class1 = subset(spam_data,spambase_data ==1) 
class0= subset(spam_data, spambase_data ==0)

smpsize1 = floor(0.70 * nrow(class1))
smpsize2 = floor(0.70* nrow(class0))

train_ind1 = sample((seq_len((nrow(class1)))), size = smpsize1)
train_ind0 = sample((seq_len((nrow(class0)))), size = smpsize2)

train0 = class0[train_ind0, ]
train1 = class1[train_ind1, ]

test0 = class0[-train_ind0, ]
test1 = class1[-train_ind1, ]

spam_train = rbind(train0,train1)
spam_test = rbind(test0,test1)

#Detailing train and test dataset
str(spam_test)
str(spam_train)
spam_train$spambase_data
spam_test$spambase_data

#Number of NON SPAM EMAILS in Train Data
output2 <- table(spam_train$spambase_data)
total_email_traindata <- output2["0"]
print (paste("total  NON SPAM EMAILS in train dataset are: " , total_email_traindata))
print(paste("Percentage of NON SPAM EMAILS in train dataset: ", round((total_email_traindata/nrow(spam_train)) * 100, 2), "%"))

#Number of SPAM EMAILS in Train Data
output3 <- table(spam_train$spambase_data)
total_spam_traindata <- output3["1"]
print (paste("total SPAM EMAILS in train dataset are: " , total_spam_traindata))
print(paste0("Percentage of SPAM EMAILS in train dataset: ", round((total_spam_traindata/nrow(spam_train)) * 100, 2), "%"))

#Number of NON SPAM EMAILS in Test Data
output5 <- table(spam_test$spambase_data)
total_email_testdata <- output5["0"]
print (paste("total  NON SPAM EMAILS in test dataset are: " , total_email_testdata))
print(paste("Percentage of NON SPAM EMAILS in test dataset: ", round((total_email_testdata/nrow(spam_test)) * 100, 2), "%"))

#Number of SPAM EMAILS in Test Data
output4 <- table(spam_test$spambase_data)
total_spam_testdata <- output4["1"]
print (paste("total SPAM EMAILS in test dataset are: " , total_spam_testdata))
print(paste("Percentage of SPAM EMAILS in test dataset: ", round((total_spam_testdata/nrow(spam_test)) * 100, 2), "%"))


#Using deceision trees with all features to generate model
spam_mod1 = rpart(spambase_data ~ ., data = spam_train,  method = "class", minsplit = 30)

#View the output of model
str(spam_mod1)
summary(spam_mod1)

#Decide on the CP values to Prune the tree for efficiency
printcp(spam_mod1) # display the results
plotcp(spam_mod1) # visualize cross-validation results

#Making another model based on CP where error is minimum, i.e 0.010
spam_mod2 = prune(spam_mod1,cp=0.010)

#Predicting the spam emails on the test data set
spam_predict = predict(spam_mod2,spam_test, type ="class")

#View the prediction details
str(spam_predict)

#Create confusion matrix to highlight accuracy of model
library("caret")
confusionMatrix(spam_predict,spam_test$spambase_data)

#Plot Tree
plot(spam_mod2, uniform=TRUE, main="Classification Tree for Spam Emails")
text(spam_mod2)


###############################################################
# Using Logistic regression

spam_log_mod = glm(spambase_data ~ .,data = spam_train, family = binomial(link = "logit"))
summary(spam_log_mod)
step(spam_log_mod)

spam_log_mod1 = glm(spambase_data ~ word_freq_1 + word_freq_2 + word_freq_3 + word_freq_4 + 
                      word_freq_5 + word_freq_6 + word_freq_7 + word_freq_8 + word_freq_9 + 
                      word_freq_10 + word_freq_11 + word_freq_12 + word_freq_13 + 
                      word_freq_14 + word_freq_15 + word_freq_16 + word_freq_17 + 
                      word_freq_18 + word_freq_20 + word_freq_21 + word_freq_22 + 
                      word_freq_23 + word_freq_24 + word_freq_25 + word_freq_26 + 
                      word_freq_27 + word_freq_28 + word_freq_29 + word_freq_30 + 
                      word_freq_31 + word_freq_32 + word_freq_33 + word_freq_34 + 
                      word_freq_35 + word_freq_36 + word_freq_37 + word_freq_39 + 
                      word_freq_41 + word_freq_42 + word_freq_43 + word_freq_44 + 
                      word_freq_45 + word_freq_46 + word_freq_47 + word_freq_48 + 
                      char_freq_1 + char_freq_2 + char_freq_3 + char_freq_4 + char_freq_5 + 
                      char_freq_6 + capital_run_avr + capital_run_long + capital_run_total, family = binomial(link = "logit"), 
                data = spam_train)


summary(spam_log_mod)

pred1 = predict (spam_log_mod1, newdata = spam_test, type = "response")

len = length(which(spam_test[,58] == round(pred1)))
Acc= (len/nrow(spam_test)) * 100

table(spam_test[,58],round(pred1))





#####################################################################

# Output Parameters: (Values keeps on changing on each run, but values are approx same)
#  Confusion Matrix and Statistics

# Reference
# Prediction   0   1
#           0 789  91
#           1  48 453

# Accuracy : 0.8993          
# 95% CI : (0.8823, 0.9147)
# No Information Rate : 0.6061          
# P-Value [Acc > NIR] : < 2.2e-16       

# Kappa : 0.7863          
# Mcnemar's Test P-Value : 0.0003675       

# Sensitivity : 0.9427          
# Specificity : 0.8327          
# Pos Pred Value : 0.8966          
# Neg Pred Value : 0.9042          
# Prevalence : 0.6061          
# Detection Rate : 0.5713          
# Detection Prevalence : 0.6372          
# Balanced Accuracy : 0.8877          

# 'Positive' Class : 0        

#[1] "total NON SPAM EMAILS in dataset are:  2788"
#[1] "Percentage of NON SPAM EMAILS in dataset:  60.6 %"

#[1] "total SPAM EMAILS in dataset are:  1813"
#[1] "Percentage of SPAM EMAILS in dataset: 39.4%"

#[1] "total  NON SPAM EMAILS in train dataset are:  1951"
#[1] "Percentage of NON SPAM EMAILS in train dataset: 60.59%"

#[1] "total SPAM EMAILS in train dataset are:  1269"
#[1] "Percentage of SPAM EMAILS in train dataset: 39.41%"

#[1] "total  NON SPAM EMAILS in test dataset are:  837"
#[1] "Percentage of NON SPAM EMAILS in test dataset:  60.61 %"

#[1] "total SPAM EMAILS in test dataset are:  544"
#[1] "Percentage of SPAM EMAILS in test dataset: 39.39%"

##############
# Using Logistic Reg.
#  Accuracy
#  [1] 92.83128

# Confusion Matrix:

#         0   1
#     0 803  34
#     1  65 479







