install.packages("RMySQL")
library(RMySQL)
mydb <- DBI::dbConnect(MySQL(), dbname='bi', user='root', password='P@ntera6', host='127.0.0.1')
dbListTables(mydb)

#import data set
table = dbSendQuery(mydb, "select * from germanloans")
germanloans = fetch(table, n=-1)

head(germanloans, 20)
summary(germanloans)

str(germanloans)

#converting data types across multiple columns to factorised style  
germanloans[c("checking_balance", "credit_history", "purpose","savings_balance", "employment_duration", 
         "other_credit", "housing", "job", "phone","defaulted")] <- lapply(germanloans[c("checking_balance", "credit_history", "purpose","savings_balance", "employment_duration", 
                                                                                    "other_credit", "housing", "job", "phone","defaulted")], factor)
# Verify the data frame 
str(germanloans)
summary(germanloans)

#FREQUENCY AND PROPORTION
#checking balance
cb <- table(germanloans$checking_balance)
prop.table(cb)

#credit history
ch <- table(germanloans$credit_history)
prop.table(ch)

#purpose
p<- table(germanloans$purpose)
prop.table(p)

#saving balance
sb <- table(germanloans$savings_balance)
prop.table(sb)

#employment duration
ed <- table(germanloans$employment_duration)
prop.table(ed)

#percent of income
pi <- table(germanloans$percent_of_income)
prop.table(pi)

#years at residence
yr <-table(germanloans$years_at_residence)
prop.table(yr)

#other credit
oc <- table(germanloans$other_credit)
prop.table(oc)

#housing
h <-table(germanloans$housing)
prop.table(h)

#existing loans count
elc <-table(germanloans$existing_loans_count)
prop.table(elc)

#job
 j<-table(germanloans$job)
 prop.table(j)
 
#dependents
 d <- table(germanloans$dependents)
 prop.table(d)
 
#phone
ph <- table(germanloans$phone)
prop.table(ph)

#defaulted
def <- table(germanloans$defaulted)
prop.table(def)

install.packages("ggplot2")
library(ggplot2)

cross<-table(germanloans$months_loan_duration,germanloans$defaulted)
addmargins(cross)

round(prop.table(cross,2)*100,digits=0)

#basic histogram
par(mfrow=c(1,2))
hist(germanloans$months_loan_duration[germanloans$defaulted == "no"])
hist(germanloans$months_loan_duration[germanloans$defaulted == "yes"])

#ggplot(germanloans,aes(x=months_loan_duration))+geom_histogram()+facet_grid(~defaulted)+theme_bw()

#coloured histogram with classes next to each other
ggplot(germanloans,aes(x=months_loan_duration,group=defaulted,fill=defaulted))+
  geom_histogram(position="dodge",binwidth=2)+theme_bw()

#3D SCATTER PLOT
library(plotly)

p <- plot_ly(germanloans, x = ~amount, y = ~age, z = ~months_loan_duration, color = ~defaulted, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Amount'),
                      yaxis = list(title = 'Age'),
                      zaxis = list(title = 'Months loan duration')))
p

#scatter plot
ggplot(germanloans, aes(x =age , y = amount, colour = defaulted)) + geom_point()

head(germanloans, 20)

#randomize the data rows
germanloans_randomised <- germanloans[sample(1:nrow(germanloans)), ]
head(germanloans_randomised, 20)


install.packages("rpart")
install.packages(c("caret", "rpart.plot"))
library(rpart)
library(rpart.plot)
library(caret)

#split data into training and testing sets
set.seed(1234)
intrain <- createDataPartition(y = germanloans_randomised$defaulted, p= 0.8, list = FALSE)
training <- germanloans_randomised[intrain,]
testing <- germanloans_randomised[-intrain,]

str(training)
str(testing)
dim(training)
dim(testing)

anyNA(germanloans_randomised)



#C5.0 algoritm
install.packages("C50")
library(C50)
install.packages("printr")
library(printr)

#Train decision tree
c50_tree_model <- C5.0(defaulted ~., data=training)

#test
c50_tree_testing <- predict(object=c50_tree_model, newdata=testing, type="class")

summary(c50_tree_model)
plot(c50_tree_model)
fancyRpartPlot(c50_tree_model)

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)


#confusion matrix
confusionMatrix(c50_tree_testing, testing$defaulted)

#AUC
#training$predictscore<-predict(c50_tree_model,training,type = "class")
#training$predictscore<-predict(c50_tree_model,training)
#testing$predictscore<-predict(c50_tree_model,testing)
#dim(training$predictscore)
#summary(training$predictscore)

#test_score <-training$predictscore
#c50_tree_scores<-prediction(test_score[,2],training$defaulted)


install.packages("ROCR")
library(ROCR)
#AUC
install.packages("pROC")
library(pROC)
c50_tree_testing
#the predictor must be numeric
numeric_label <- as.numeric(testing$defaulted)
auc(c50_tree_testing, numeric_label)

#F-1 score
install.packages("MLmetrics")
library(MLmetrics)
#data fram is needed
f1 <- data.frame(c50_tree_testing, testing$defaulted)
f1
F1_Score(f1$c50_tree_testing, f1$testing.defaulted, positive ="no")


#CART DT
library(rpart)
library(rpart.plot)
library(caret)

#cart_tree_model <- rpart(defaulted~.,data=training,method = 'class')

testing[17]
cart_prediction <- predict(cart_tree_model,newdata=testing[-17],type = 'class')

#CONFUSION MATRIX
confusionMatrix(cart_prediction, testing$defaulted)

#AUC
auc(cart_prediction,numeric_label)

#F-1 Score
f1 <- data.frame(cart_prediction, testing$defaulted)
f1

#CART TREE V2
trctrl <- trainControl(method = "repeatedcv", number = 17, repeats = 3)
set.seed(3333)
cart_tree_model <- train(defaulted ~., data = training, method = "rpart",
                         parms = list(split = "information"),
                         trControl=trctrl,
                         tuneLength = 10)
cart_tree_model


print(cart_tree_model)
testing
cart_prediction <- predict(cart_tree_model, newdata = testing)
confusionMatrix(cart_prediction, testing$defaulted )  #check accuracy

auc(cart_prediction, numeric_label)

f1_cart <- data.frame(cart_prediction, testing$defaulted)
f1_cart
F1_Score(f1_cart$cart_prediction, f1_cart$testing.defaulted, positive ="no")



###
#FIXED
###


#C5.0 algoritm
install.packages("C50")
library(C50)
install.packages("printr")
library(printr)

#Train decision tree
c50_tree_model <- C5.0(defaulted ~., data=training)
c50_tree_model
#test
c50_tree_testing <- predict(object=c50_tree_model, newdata=testing[-17], type="class")

summary(c50_tree_model)

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

training
#confusion matrix
confusionMatrix(c50_tree_testing, testing$defaulted)

#AUC

install.packages("ROCR")
library(ROCR)
#AUC
install.packages("pROC")
library(pROC)
c50_tree_testing
#the predictor must be numeric
numeric_label <- as.numeric(testing$defaulted)

auc(c50_tree_testing, actualValue)

#F-1 score
install.packages("MLmetrics")
library(MLmetrics)
#data fram is needed
f1 <- data.frame(c50_tree_testing, testing$defaulted)
f1
F1_Score(f1$c50_tree_testing, f1$testing.defaulted, positive ="no")



#CART TREE V2
?trainControl
trctrl <- trainControl(method = "repeatedcv", number = 17, repeats = 3)
set.seed(3333)
cart_tree_model <- train(defaulted ~., data = training, method = "rpart",
                         parms = list(split = "information"),
                         trControl=trctrl,
                         tuneLength = 10)
cart_tree_model



testing
cart_prediction <- predict(cart_tree_model, newdata = testing[-17])
confusionMatrix(cart_prediction, testing$defaulted )

auc(cart_prediction, actualValue)

f1_cart <- data.frame(cart_prediction, testing$defaulted)
f1_cart
F1_Score(f1_cart$cart_prediction, f1_cart$testing.defaulted, positive ="no")


#Improved CART Model
set.seed(3333)
cart_tree_model_gini <- train(defaulted~., data = training_new, method = "rpart",
                              parms = list(split = "gini"),
                              trControl=trctrl,
                              tuneLength = 37)
cart_tree_model_gini$bestTune

cart_tree_model_gini

cart_gini_prediction <- predict(cart_tree_model_gini, newdata = testing[-17])
cart_gini_prediction
confusionMatrix(cart_gini_prediction, testing$defaulted)

#pruning
training_old <- training
testing_old <- testing
training <- training[-18]
testing <- testing[-18]

pruned_tree <- rpart(defaulted~.,data=training,method = 'class')
printcp(pruned_tree)

opt  <-  which.min(pruned_tree$cptable[,'xerror'])
cp <-  pruned_tree$cptable[opt, 'CP']

pruned_model <-  prune(pruned_tree,cp)
rpart.plot(pruned_model, box.col=c("blue", "yellow"))

rpart_pruned_predict <- predict(pruned_model, newdata=testing[-17],type = 'class')

confusionMatrix(rpart_pruned_predict,testing$defaulted)


actualValue <- as.numeric(testing$defaulted)

auc(rpart_pruned_predict, actualValue)

f1_cart <- data.frame(rpart_pruned_predict, testing$defaulted)
f1_cart
F1_Score(f1_cart$rpart_pruned_predict, f1_cart$testing.defaulted, positive ="no")




