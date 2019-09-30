tele = read.csv("telemarketing.csv")
#split 80/20
set.seed(1)
train.index = sample(1:nrow(tele),nrow(tele)*0.8)
train = tele[train.index,]
test = tele[-train.index,]

#2A. Model Buiding
model1 = glm(y~.,data = train, family = binomial)
summary(model1)
#age, housingyes,contactunknown,duration,campaign are important significant
#2B. Interpretting coefficients
#convert the log-odds to log ratio
exp(coef(model1))
#2. Per unit of the duration increases, we do expect the odds of the person 
#who subscribes to increase by the factor of 1.0038 
#Because the coef is 1.003, which is close to 1
#3. If the person has housing loan, the odds of the person who subcribes is also
#expected to decrease by the factor of 0.5756, because the odds ratio of housing
#yes is lower than 1
prop.table(table(tele$y))
#2C. Model Comparison
#reduced model

model2 = glm(y ~ duration+contact+campaign,data = train, family = binomial)
summary(model2)

data.frame(full.model = AIC(model1),reduced.model = AIC(model2))
#full model is a better fit model because the AIC number is lower.

#2D. Predictions
#make prediction on test set
pred.prob = predict(model1,test,type = "response") #probability of subsciptions
pred.prob[1:10] #View first 10 predictions
#convert these prob to binary "Yes" and "No". threshold of 50%
pred.class = pred.prob
pred.class[pred.prob>0.5] = "Yes"
pred.class[!pred.prob>0.5] = "No"
pred.class[1:10]
table(pred.class)
#create confusion matrix
c.matrix = table(test$y,pred.class)
c.matrix
acc = (c.matrix[1]+c.matrix[4])/(c.matrix[1]+c.matrix[2]+c.matrix[3]+c.matrix[4])
sens.yes = c.matrix[4]/(c.matrix[2]+c.matrix[4])
prec.yes = c.matrix[4]/(c.matrix[3]+c.matrix[4])
data.frame(acc,sens.yes,prec.yes)
#accuracy is 89.17%, sens.yes is 18.69%, prec.yes is 64.51%

#change threshold to 0.8
pred.class = pred.prob
pred.class[pred.prob>0.8] = "Yes"
pred.class[!pred.prob>0.8] = "No"
pred.class[1:10]

#create confusion matrix
c.matrix = table(actual=test$y,pred.class)
c.matrix
acc = (c.matrix[1]+c.matrix[4])/(c.matrix[1]+c.matrix[2]+c.matrix[3]+c.matrix[4])
sens.yes = c.matrix[4]/(c.matrix[2]+c.matrix[4])
prec.yes = c.matrix[4]/(c.matrix[3]+c.matrix[4])
data.frame(acc,sens.yes,prec.yes)
#acc is 88%, sens.yes is 2.8%, prec.yes is 37.5%
#the company should keep the threshold at 50% since the acc and sesns and prec
#are all higher. It would be better to decrease the threshold for yes

#3. Random Forest
set.seed(1)
install.packages("randomForest")
library(randomForest)
reg.model = randomForest(y~.,data=train)
reg.model
# 500 trees were built. 2 variables were tried at each split
#the OOB estimated test error is 11.64%
#make prediction on test set
pred.y = predict(reg.model,test)
c.matrix = table(actual = test$y,pred.y)
c.matrix
acc = mean(test$y == pred.y)
sens.yes = c.matrix[4]/(c.matrix[2]+c.matrix[4])
prec.yes = c.matrix[4]/(c.matrix[3]+c.matrix[4])
data.frame(acc,sens.yes,prec.yes)
#accuracy is 88.62%, sens is 23.36%, prec is 54.35% compared to 89.17%, 18.7%,
# 64.51%. Compared to the logistic regression, the random forest gave out about
#the same result, almost the same accuracy, better at sensitivity but lower at 
#precision. 
varImpPlot(reg.model,main = "Variance Importance Plot")

#duration is the most important factor in predicting whether the customer will
#subscribe to the product after getting contacted by the marketing team. 











































