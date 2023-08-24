rm(list = ls())

library(kernlab)
library(MASS) 
library(randomForest) 
library(rpart)
library (e1071)
library(class)
library(ROCR) 

library(xlsx)
#importation des données heart.xlsx
heart <- read.xlsx(file.choose(), sheetIndex = 1, header = T, stringsAsFactors = TRUE)

################ important changement des facteur en numerique
XX <- model.matrix(coeur ~., data = heart)[,-1]
data.heart <- cbind(as.data.frame(XX), coeur = as.factor(heart[,"coeur"]))

################
str(data.heart)

dim(data.heart) # 270   8

#On partage la base en deux parties : train.set et test.setsample()
index <- 1:nrow(data.heart)

l <- 3 #(l-1)/l d'observations pour l'apprentissage et 1/l d'observations pour le test
set.seed(12)
test.set.index <- sample(x = index, size = trunc(length(index)/l), replace = FALSE)
heart.test.set <- data.heart[test.set.index, ] #ensemble de test
heart.train.set <- data.heart[- test.set.index, ] #ensemble d'apprentissage


#On compare les scores construits par RegLog, lda, qda, forêts aléatoires,
#arbres de décision et svm (variable reponse: type)
modele.logit <- glm(formula = coeur ~ ., data = heart.train.set, family = binomial)

require(MASS)
modele.lda <- lda(formula = coeur ~., data = heart.train.set)
modele.qda <- qda(formula = coeur ~., data = heart.train.set)

require(randomForest)
modele.RF <- randomForest(formula = coeur ~., data = heart.train.set)

require(rpart)
modele.arbre <- rpart(formula = coeur ~., data = heart.train.set)

require(e1071)
tune.out <- tune(svm, coeur ~ ., data = heart.train.set, kernel = "linear", scale = TRUE,
                 ranges = list(cost = c(2^(-2) ,2^(-1), 1, 2^2, 2^3, 2^4)))
modele.svm <- tune.out$best.model

#On calcule ensuite pour chaque modèle le score des individus de l'échantillon de test
Score.logit <- predict(modele.logit, newdata = heart.test.set, type = "response")
Score.lda <- predict(modele.lda, newdata = heart.test.set, type = "prob")$posterior[, 2] #proba posteri et on prend la deuxieume(cad 1er y=0 2nd y=1 un spam)
Score.qda <- predict(modele.qda, newdata = heart.test.set, type = "prob")$posterior[, 2]
Score.RF <- predict(modele.RF, newdata = heart.test.set, type = "prob")[, 2] # 
Score.arbre <- predict(modele.arbre, newdata = heart.test.set, type = "prob")[, 2]
Score.svm <- attributes(predict(modele.svm, newdata = heart.test.set, scale = TRUE,  
                                decision.values = TRUE))$decision.values

#On trace maintenant les 6 courbes ROC 
require(ROCR)
S1.pred <- prediction(Score.logit, heart.test.set$coeur)
S2.pred <- prediction(Score.lda, heart.test.set$coeur)
S3.pred <- prediction(Score.qda, heart.test.set$coeur)
S4.pred <- prediction(Score.RF, heart.test.set$coeur)
S5.pred <- prediction(Score.arbre, heart.test.set$coeur) # errror
S6.pred <- prediction(Score.svm, heart.test.set$coeur)

roc1 <- performance(S1.pred, measure = "tpr", x.measure = "fpr") # tpr: true positiv
roc2 <- performance(S2.pred, measure = "tpr", x.measure = "fpr")
roc3 <- performance(S3.pred, measure = "tpr", x.measure = "fpr")
roc4 <- performance(S4.pred, measure = "tpr", x.measure = "fpr")
roc5 <- performance(S5.pred, measure = "tpr", x.measure = "fpr")
roc6 <- performance(S6.pred, measure = "tpr", x.measure = "fpr")


par(mfrow = c(1,1))

#Tracer les courbes ROC des scores
plot(roc1, col = "black", lwd = 2, main = "Courbes ROC")
plot(roc2, add = TRUE, col = "red", lwd = 2)
plot(roc3, add = TRUE, col = "blue", lwd = 2)
plot(roc4, add = TRUE, col = "green", lwd = 2)
plot(roc5, add = TRUE, col = "yellow", lwd = 2)
plot(roc6, add = TRUE, col = "orange", lwd = 2)
bissect <- function(x) x
curve(bissect(x),  col = "black", lty = 2, lwd = 2, add = TRUE)
legend("bottomright", legend = c("logit", "lda", "qda", "RF", "arbre", "svm"),
       col = c("black", "red", "blue", "green", "yellow", "orange"), lty = 1, lwd = 2)
