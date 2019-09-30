install.packages("tree")
library(tree)
library(class)
tele = read.csv("telemarketing.csv")
set.seed(1)

#1.The data
#check the distribution outcome of the variable y
prop.table(table(tele$y))
#baseline performance: accuracy would be 88.5% if predicting all nos and 11.52%
#for all yeses
#split 80/20 (training/test set)
train.index = sample(1:nrow(tele),nrow(tele)*0.8,replace = FALSE)
train = tele[train.index,]
test = tele[-train.index,]

#2. Building a decision tree

model = tree(y~.,data = train)
summary(model)

#a,plot the tree
plot(model)
text(model)

#7 nodes, 6/7 nodes are no because 88.5% of our data is "no"

#b, use 10-fold cross-validation to find the optimal tree size

best.tree = cv.tree(model,K=10) #k=10 specifiying 10-fold cross validation
best.tree

x = best.tree$size
y = best.tree$dev
plot(x,y,xlab = "tree size",ylab = "deviance", type= "b",pch = 20, col = "blue")

 # tree with 7 terminal nodes seem to have the lowest deviance
#prune the tree and plot it again
model.pruned = prune.tree(model,best = 7)
plot(model.pruned)
text(model.pruned)
# duration (length of contact) is the most important variable in predicting y.
#We can expect a higher chance that the clients subscribe to the term of 
#deposit when the duration of the call is greater than 758.5 seconds

#d, Use pruned tree, make prediction on test set
pred.class = predict(model.pruned,test,type = "class") #class gives label prediction

#Show confustion matrix on test set
c.matrix = table(test$y,pred.class)
c.matrix

#calculate accuracy
acc = mean(test$y == pred.class)
acc
#calculate sensitivity and precision for "yes"
sens.yes = c.matrix[4]/(c.matrix[2]+c.matrix[4])
prec.yes = c.matrix[4]/(c.matrix[3]+c.matrix[4])
data.frame(acc,sens.yes,prec.yes)

#Our accuracy is 89.17%, of actual "yes", we predicted 26% correct
#of our predicted "yes", 59% was correct

#This model returns a low rate of correctly predicted "yes" compared to actual

#3. KNN
#3a,Data intepretation
#normalized values
normalize = function (x) {
  return((x-min(x))/(max(x)-min(x)))
}
#normalize age, balance and duration
norm.tele = as.data.frame(lapply(tele[,-c(3,4,7)],normalize))
summary(norm.tele)

#combine them into one df
tele = as.data.frame(cbind(norm.tele,tele[,c(3,4,7)]))
summary(tele)
#categorical predictors -> convert to a dummy variable
tele$housing = ifelse(tele$housing == "yes",1,0)
tele$cell = ifelse(tele$contact == "cellular",1,0)
tele$tel = ifelse(tele$contact == "telephone",1,0)
tele$unknown = ifelse(tele$contact == "unknown",1,0) 
tele[,"contact"] = NULL
summary(tele)

#3B. Model building and comparison
#split data into train and test set
train = tele[train.index,]
test = tele[-train.index,]

train.x = train[,-6] #train data for predictors
test.x = test[,-6] #test data for predictors
train.cl = train[,6] #class label in training data (y)
#set the stage for 2 different sets of metrics for k=3 and k=5
rep = seq(3,5,2)
rep.acc = rep
rep.sens = rep
rep.prec = rep
rep.fp = rep
rep.fn = rep

set.seed(1)
#create index for 5-fold cross validation
k=5
fold = sample(1:k,nrow(train.x), replace = TRUE)
#Nested for loop
#Outer loop for KNN modesl with different K
## Inner loop for k-fold cross validation
iter = 1#index for rep iteration

for (K in rep){
  #Space to store metrics from each iteration of k-fold cv

 kfold.acc = 1:k
 kfold.sens = 1:k
 kfold.prec = 1:k
 kfold.fp = 1:k
 kfold.fn = 1:k
 
  for (i in 1:k) {
    #data for test and training sets
    test.kfold = train.x[fold ==i,]
    train.kfold = train.x[fold!=i,]
    #class labels for test and training sets
    test.cl.actual = train.cl[fold==i]
    train.cl.actual = train.cl[fold!=i]
    #make predictions on class labels for test set
    pred.class = knn(train.kfold,test.kfold,train.cl.actual,k=K)
    #evaluation metrics: acc, sens and prec for "yes"
    c.matrix = table(test.cl.actual,pred.class)
    acc = mean(pred.class == test.cl.actual)
    sens.yes = c.matrix[4]/(c.matrix[2]+c.matrix[4])
    prec.yes = c.matrix[4]/(c.matrix[3]+c.matrix[4])
    fp = c.matrix[3]
    fn = c.matrix[2]
    
    #Store results for each k-fold iteration
    kfold.acc[i] = acc
    kfold.sens[i] = sens.yes
    kfold.prec[i] = prec.yes
    kfold.fp[i] = fp
    kfold.fn[i] = fn
    
    }
    #store average k-fold performance for each KNN model
    rep.acc[iter] = mean(kfold.acc)
    rep.sens[iter] = mean(kfold.sens)
    rep.prec[iter] = mean(kfold.prec)
    rep.fp[iter] = mean(fp)
    rep.fn[iter] = mean(fn)
    iter = iter+1
}

results = as.data.frame(cbind(rep,rep.acc,rep.sens,rep.prec,rep.fp,rep.fn))
names(results) = c("K","accuracy","sensitivity","precision","FP","FN")

results

#k goes up, accuracy goes up,precision increases but sensitivity goes down
#FP is greater than FN

#3C. Predictions
#make prediction on the test set
pred.class = knn(train.x,test.x,train.cl,k=5)
c.matrix =table(test$y,pred.class)
c.matrix
acc = mean(pred.class == test$y)
sens.yes = c.matrix[4]/(c.matrix[2]+c.matrix[4])
prec.yes = c.matrix[4]/(c.matrix[3]+c.matrix[4])
as.data.frame(cbind(acc,sens.yes,prec.yes))

#on the test set, our accuracy is 88%, when predicting the customers would say
#yes, our model was only correct 45%. out of the actual yes, our model was 
#only 18% correct. 













