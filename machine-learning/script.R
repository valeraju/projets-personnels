data <- read.csv("data.csv",encoding="UTF-8", stringsAsFactors=FALSE)

head(data)

##Suppression de la colonne history_segment
data <- subset(data, select=-history_segment)

##Transformation des variables catégorielles en variables binaires
install.packages("dummies")
library(dummies)

col_categorial = c("zip_code","channel","segment")
for(c in col_categorial) {
  # print(unique(data[c]))
  data <- cbind(data,get.dummy(dummy.data.frame(data,sep="."),c))
  cat(c," [ ")
  cat(unique(data[[c]]),sep=", ")
  cat(" ]\n")
}
colnames(data) <- gsub(".*\\.","",colnames(data))

head(get.dummy(dummy.data.frame(data,sep="."),"channel"))

##Suppression des anciennes colonnnes catégorielles
data <- data[,!(names(data) %in% col_categorial)]
head(data)

##Definition des données du modèle
y <- subset(data,select=c('visit'))
x_var <- c('recency', 'history', 'mens', 'womens', 'newbie',
           'Rural', 'Surburban', 'Urban', 'Multichannel', 'Phone', 'Web',
           'Mens E-Mail', 'No E-Mail', 'Womens E-Mail')
x <- subset(data,select=c('recency', 'history', 'mens', 'womens', 'newbie',
                          'Rural', 'Surburban', 'Urban', 'Multichannel', 'Phone', 'Web',
                          'Mens E-Mail', 'No E-Mail', 'Womens E-Mail'))
xy <- cbind(x,y)
##Histogrammes x
par(mfrow=c(4,4), oma = c(2,2,2,2), mar = c(2,2,2,2))
for(c in x_var) {
  hist(x[[c]],
      col="blue",
      las=1,
      main=c)
}

##Histogrammes y
par(mfrow=c(1,1), oma = c(2,2,2,2), mar = c(2,2,2,2))
hist(y[['visit']],col="blue",las=1,main="visit")

##Sépration des données en 2 groupes
set.seed(1)
train_index <- sample(nrow(xy),0.33*nrow(xy))
# test_index <- sample(nrow(data),train_index)

data_train <- xy[train_index,]
data_test <- xy[-train_index,]
table(unlist(data_train$visit))

##Entrainement de l'arbre de décision avec les données (x_train,y_train)
library(rpart)
library(rpart.plot)
train_fit <- rpart(visit~.,data=data_train,method="class",
                   control=rpart.control(maxdepth=30,minsplit=0,cp=0))

table(data_train$visit)
plotcp(train_fit)
train_fit
cpOptimal <- train_fit$cptable[which.min(train_fit$cptable[,4]),1]
arbreOptimal <- prune(train_fit,cp=0.00039215686)
prp(arbreOptimal,extra=1)
predicted <- predict(arbreOptimal, data_test,type="class")
actual <- data_test$visit
cm <- table(actual=actual,predicted=predicted)


##Apprentissage
table(unlist(data_train$visit))
pourcentage(arbreOptimal,data_train)

##Test
table(unlist(data_test$visit))
pourcentage(arbreOptimal,data_test)

pourcentage <- function(arbre,data) {
  predicted <- predict(arbre, data, type="class")
  actual <- data$visit
  cm <- table(actual=actual,predicted=predicted)
  cat((((cm[1,1]/sum(cm[1,]))+(cm[2,2]/sum(cm[2,])))/2)*100,"%")
  return(cm)
}

predicted <- as.data.frame(predicted)
actual <- subset(data_test,select=c('visit'))
predicted <- predict(train_fit, data_test, "class")
actual <- data_test$visit
cm <- as.matrix(table(Actual = actual, Predicted = predicted))

table(data_test$visit)

