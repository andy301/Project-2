# Project 2
I've posted the code for Project 2
```{r}
require(mlbench)

# load the data set
data(BreastCancer)

#write.csv(BreastCancer,"BreastCancer.csv"). Remove NAs and ID column then save as BS1.csv.

#upload the BS1 data set to R
BS1 <- read.csv("BS1.csv")

```


```{r}
library(e1071)

#make a model using the BS1 data
mysvm <- svm(Classf ~ ., BS1)
mysvm.pred <- predict(mysvm, BS1)
table(mysvm.pred,BS1$Classf)
summary(mysvm.pred)


#make a matrix of predictions
mysvm.matrix <- matrix(mysvm.pred)

#look at matrix information
summary(mysvm.matrix)
typeof(mysvm.matrix)
str(mysvm.matrix)

#export csv and combine in ensemble table 
  #write.csv(mysvm.matrix, "mysvmmatrix.csv")

```


```{r}

#install.packages("klaR")

library(klaR)

mynb <- NaiveBayes(Classf ~ ., BS1)
mynb.pred <- predict(mynb,BS1)

table(mynb.pred$class,BS1$Classf)

#make a matrix of predictions
mynb.matrix <- matrix(mynb.pred$class)

summary(mynb.matrix)
typeof(mynb.matrix)
str(mynb.matrix)


#export csv and combine in ensemble table 
    #write.csv(mynb.matrix, "mynbmatrix.csv")

```


```{r}
library(nnet)
mynnet <- nnet(Classf ~ ., BS1, size=1)
mynnet.pred <- predict(mynnet,BS1,type="class")
table(mynnet.pred,BS1$Classf)


library(MASS)

#make a matrix of predictions
mynnet.matrix <- matrix(mynnet.pred)


summary(mynnet.matrix)
typeof(mynnet.matrix)
str(mynnet.matrix)

#export csv and combine in ensemble table 
  #write.csv(mynnet.matrix, "mynnetmatrix.csv")

```


```{r}
#Decision trees
library(rpart)
mytree <- rpart(Classf ~ ., BS1)
plot(mytree); text(mytree) # in "BS1_tree.ps"
summary(mytree)
mytree.pred <- predict(mytree,BS1,type="class")
table(mytree.pred,BS1$Classf)

# Leave-1-Out Cross Validation (LOOCV)
#ans <- numeric(length(BS1[,1]))
#for (i in 1:length(BS1[,1])) {
 # mytree <- rpart(Classf ~ ., BS1[-i,])
 # mytree.pred <- predict(mytree,BS1[i,],type="class")
 # ans[i] <- mytree.pred
#}
#ans <- factor(ans,labels=levels(BS1$Classf))
#table(ans,BS1$Classf)


#make a matrix of predictions
mytree.matrix <- matrix(mytree.pred)

summary(mytree.matrix)
typeof(mytree.matrix)
str(mytree.matrix)

#export csv and combine in ensemble table 
  #write.csv(mytree.matrix, "mytreematrix.csv")

```


```{r}
#Quadratic Discriminant Analysis
library(MASS)
myqda <- qda(Classf ~ ., BS1)
myqda.pred <- predict(myqda, BS1)
table(myqda.pred$class,BS1$Classf)


#make a matrix of predictions
myqda.matrix <- matrix(myqda.pred$class)

summary(myqda.matrix)
typeof(myqda.matrix)
str(myqda.matrix)

#export csv and combine in ensemble table 
  #write.csv(myqda.matrix, "myqdamatrix.csv")

```


```{r}
#Regularised Discriminant Analysis
library(klaR)
myrda <- rda(Classf ~ ., BS1)
myrda.pred <- predict(myrda, BS1)
table(myrda.pred$class,BS1$Classf)

#make a matrix of predictions 

myrda.matrix <- matrix(myrda.pred$class)

summary(myrda.matrix)
typeof(myrda.matrix)
str(myrda.matrix)


#export csv and combine in ensemble table
    #write.csv(myrda.matrix, "myrdamatrix.csv")

```


```{r}
#Random Forests
library(randomForest)
myrf <- randomForest(Classf ~ ., BS1)
myrf.pred <- predict(myrf, BS1)
table(myrf.pred, BS1$Classf)

#make a matrix of predictions
myrf.matrix <- matrix(myrf.pred)

summary(myrf.matrix)
typeof(myrf.matrix)
str(myrf.matrix)

#export csv and combine in ensemble table
    #write.csv(myrf.matrix,"myrfmatrix.csv")

```




```{r}

#make a data frame that sums the predictions from the classifers and chooses the majority classification for each prediction


df.ensemble1 <- cbind(mysvm.matrix,mynb.matrix,mynnet.matrix,mytree.matrix,myqda.matrix,myrda.matrix,myrf.matrix)

colnames(df.ensemble1) <- c("SVM","NB","NNet","Tree","QDA","RDA","RF")


head(df.ensemble1)


```

```{r}
Benigns <- rowSums(df.ensemble1[,1:7] == "benign") 
Malignants <- rowSums(df.ensemble1[,1:7] == "malignant")


ensemble.totals <- cbind(df.ensemble1, Benigns, Malignants)

head(ensemble.totals)


#convert to data frame
ensemble.totals.df <- as.data.frame(ensemble.totals)

dim(ensemble.totals.df)
  
head(ensemble.totals.df)

```


```{r}
#convert benign and malignant columns to integers

b.num <- c(ensemble.totals.df$Benigns)
m.num <- c(ensemble.totals.df$Malignants)

b.num <- b.num-1

m.num <- m.num-1

Etotals1 <- cbind(ensemble.totals.df,b.num,m.num)

dim(Etotals1)



```


```{r}
#Loop to take the highest count of benign or maliginant from each row and put into new vector 

Majority2 <- c()

for(i in 1:length(Etotals1[,1])){
  if(Etotals1$b.num[i] > Etotals1$m.num[i]){
    Majority2 <- append(Majority2, 'Benign')
  }
  else{
    Majority2 <- append(Majority2, 'Malignant')
  }
}



```





```{r}
#Clean table up

PredictionTable <- cbind(Etotals1,Majority2)
PredictionTable2 <- subset(PredictionTable, select = -c(8:10))


#colnames(PredictionTable2) <- c("SVM","NB","NNet","Tree","QDA","RDA","RF", "Total Benigns", "Total Malignants", "Majority"). Commeting this portion out. 

head(PredictionTable2)

#Write the tabel to csv to check 
  #write.csv(PredictionTable2,"PredictionTable2.csv")

```






