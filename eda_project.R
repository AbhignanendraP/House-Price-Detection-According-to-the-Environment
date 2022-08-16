library(ggplot2)
library(dummies)
library(descr)
library(gridExtra)
library(ggpubr)
library(ggcorrplot)
library(caret)
library(party)
library(randomForest)
library(e1071)

data<-read.csv("C:/Abhi notes/class3-2/eda/project/House_Price_Analysis1.csv",header=TRUE)
dataset<-data
head(dataset)
View(dataset)
summary(dataset)

quantile(dataset[,11],0.99)
uv<-quantile(dataset[,11],0.99)
dataset[,11][dataset[,11]>uv]<-uv
summary(dataset[,11])


dataset[,39][1:length(dataset[,39])]

mean(dataset[,39],na.rm = TRUE)
dataset[,39][is.na(dataset[,39])] <- mean(dataset[,39],na.rm = TRUE)
print(sum(is.na(dataset[,39][1:length(dataset[,39])])))

dataset[,10][1:length(dataset[,10])]

mean(dataset[,10],na.rm = TRUE)
dataset[,10][is.na(dataset[,10])] <- mean(dataset[,10],na.rm = TRUE)
print(sum(is.na(dataset[,10][1:length(dataset[,10])])))

summary(dataset[,39])
quantile(dataset[,39],0.99)
uv<-quantile(dataset[,39],0.99)
dataset[,39][dataset[,39]>uv]<-uv
summary(dataset[,39])

summary(dataset[,40])
quantile(dataset[,40],0.99)
uv<-quantile(dataset[,40],0.99)
dataset[,40][dataset[,40]>uv]<-uv
summary(dataset[,40])

summary(dataset)

ggplot((dataset),aes(house.type))+geom_bar(aes(fill = house.type))+scale_fill_brewer(palette = "Blues") +labs(y="freq", title="bar Chart of Airport")

ggplot((dataset),aes(house.type))+geom_bar(aes(fill = from.main.road))+scale_fill_brewer(palette = "Reds") +labs(y="freq", title="bar Chart of type and near main road")

ggplot((dataset),aes(house.type))+geom_bar(aes(fill = from.a.Grocery))+scale_fill_brewer(palette = "Greens") +labs(y="freq", title="bar Chart of type and near gorcery")

dataframe2=dataset
dataframe2=dataframe2[,-1]
dataframe2=dataframe2[,-1]
dataframe2=dataframe2[,-1]
for(j in 1:ncol(dataframe2)){
  for(i in 1:nrow(dataframe2)){
    if(is.na(dataframe2[i,j]) || dataframe2[i,j]== ""){
      dataframe2[i,j]=NA
    }
  }
}
for(i in 1:nrow(dataframe2)){
  if(is.na(dataframe2[i,4])){
    dataframe2[i,4]="No"
  }
}
for(i in 1:nrow(dataframe2)){
  if(is.na(dataframe2[i,5])){
    dataframe2[i,5]="No"
  }
}
str(dataframe2)
dataframe2 <-dummy.data.frame(dataframe2)
summary(dataframe2)
View(dataframe2)

dataframe2=dataframe2[,-120]
dataframe2=dataframe2[,-115]
dataframe2=dataframe2[,-110]
dataframe2=dataframe2[,-105]
dataframe2=dataframe2[,-101]
dataframe2=dataframe2[,-96]
dataframe2=dataframe2[,-91]
dataframe2=dataframe2[,-87]
dataframe2=dataframe2[,-82]
dataframe2=dataframe2[,-78]
dataframe2=dataframe2[,-74]
dataframe2=dataframe2[,-71]
dataframe2=dataframe2[,-66]
dataframe2=dataframe2[,-62]
dataframe2=dataframe2[,-58]
dataframe2=dataframe2[,-53]
dataframe2=dataframe2[,-49]
dataframe2=dataframe2[,-45]
dataframe2=dataframe2[,-40]
dataframe2=dataframe2[,-36]
dataframe2=dataframe2[,-32]
dataframe2=dataframe2[,-28]
dataframe2=dataframe2[,-20]
dataframe2=dataframe2[,-7]
View(dataframe2)
png("C:/Abhi notes/class3-2/EDA/project/co-realtion.png")
cor(dataframe2,dataframe2$Price.of.House)
ggcorrplot(round(cor(dataframe2),2))+theme(axis.text.x = element_text(size = 4),axis.text.y = element_text(size = 4))
dev.off()
ggcorrplot(round(cor(dataframe2),2))+theme(axis.text.x = element_text(size = 4),axis.text.y = element_text(size = 4))

df<-sort(sample(nrow(dataframe2),nrow(dataframe2)*0.7))
train_data<-dataframe2[df,]
test_data<-dataframe2[-df,]

#Price of the house
lmvalues<-lm(Price.of.House~.,data=train_data)
summary(lmvalues)
predict_lmvalues<-predict(lmvalues,test_data)
View(test_data)
x = 1:length(test_data[,98])

RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}
MAPE = function(y_actual,y_predict){
  mean(abs((y_actual-y_predict)/y_actual))*100
}

dataframe3=dataframe2[,c(1,8,9,11,13,14,15,16,17,18,20,21,22,24,25,28,31,53,58,60,62,63,65,68,98)]
#df<-sort(sample(nrow(dataframe3),nrow(dataframe3)*0.7))
#View(dataframe3)
train_data1<-dataframe3[1:444,]
test_data1<-dataframe3[444:624,]

lmvalues<-lm(Price.of.House~.,data=train_data1)
summary(lmvalues)
predict_lmvalues<-predict(lmvalues,test_data1)
#View(test_data1)
x = 1:length(test_data1[,25])
plot(x, test_data1[,25], col = "red", type = "l", lwd=2, main = "Linear regression for Price of house")
lines(x, predict_lmvalues, col = "blue", lwd=2)
legend("topright",  legend = c("original values", "predicted values"), fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()

LR_MAPE_lm = MAPE(test_data1[,25],predict_lmvalues) 
LR_R_lm = RSQUARE(test_data1[,25],predict_lmvalues) 
Accuracy_lm = 100 - LR_MAPE_lm
print("R-Square: ")
print(LR_R_lm)
print('Accuracy of Linear Regression: ')
print(Accuracy_lm)





modelsvm = svm(Price.of.House~.,train_data1)
summary(modelsvm)
predict_svmvalues = predict(modelsvm, test_data1)

plot(x, test_data1[,25], col = "red", type = "l", lwd=2, main = "SVM model's for Price of House")
lines(x, predict_svmvalues, col = "blue", lwd=2)
legend("topright",  legend = c("original values", "predicted values"), fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()

LR_MAPE_svm = MAPE(test_data1[,25],predict_svmvalues) 
LR_R_svm = RSQUARE(test_data1[,25],predict_svmvalues) 
Accuracy_svm = 100 - LR_MAPE_svm
print("R-Square: ")
print(LR_R_svm)
print('Accuracy of SVM: ')
print(Accuracy_svm)





knnmodel = knnreg(train_data1[,-25],train_data1[,25])
str(knnmodel)
predict_knnvalues = predict(knnmodel, test_data1[,-25])
mse = mean((test_data1[,25] - predict_knnvalues)^2)
mae = caret::MAE(test_data1[,25], predict_knnvalues)
rmse = caret::RMSE(test_data1[,25], predict_knnvalues)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

x = 1:length(test_data1[,25])

plot(x, test_data1[,25], col = "red", type = "l", lwd=2, main = "KNN Algorithm for Price of House")
lines(x, predict_knnvalues, col = "blue", lwd=2)
legend("topright",  legend = c("original values", "predicted values"), fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()

LR_MAPE_knn = MAPE(test_data1[,25],predict_knnvalues) 
LR_R_knn = RSQUARE(test_data1[,25],predict_knnvalues) 
Accuracy_knn = 100 - LR_MAPE_knn
print("R-Square: ")
print(LR_R_knn)
print('Accuracy of KNN: ')
print(Accuracy_knn)






png(file = "decision_tree.png",width = 1000, height = 1000)
dataframe4=dataframe3[1:100,]
output.tree <- ctree(Price.of.House ~., data = train_data1)
plot(output.tree)
dev.off()
predict_dtreevalues<-predict(output.tree, test_data1[,-25])

plot(x, test_data1[,25], col = "red", type = "l", lwd=2, main = "Decision Tree for Price of House")
lines(x, predict_dtreevalues, col = "blue", lwd=2)
legend("topright",  legend = c("original values", "predicted values"), fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()

LR_MAPE_dtree = MAPE(test_data1[,25],predict_dtreevalues) 
LR_R_dtree = RSQUARE(test_data1[,25],predict_dtreevalues) 
Accuracy_dtree = 100 - LR_MAPE_dtree
print("R-Square: ")
print(LR_R_dtree)
print('Accuracy of Decision tree: ')
print(Accuracy_dtree)




predict_linear = predict(lmvalues, test_data1[40,-25])
predict_Linear
test_data1[40,25]

predict_SVM = predict(modelsvm, test_data1[40,-25])
predict_SVM
test_data1[40,25]

predict_knn = predict(knnmodel, test_data1[120,-25])
predict_knn
test_data1[120,25]

predict_Decision = predict(output.tree, test_data1[120,-25])
predict_Decision
test_data1[120,25]


#Rent of the house




lmvalues<-lm(Rent.of.House~.,data=train_data)
summary(lmvalues)
predict_lmvalues<-predict(lmvalues,test_data)
View(test_data)
x = 1:length(test_data[,98])


dataframe4=dataframe2[,c(1,8,9,11,13,14,15,16,17,18,20,21,22,24,25,28,31,53,58,60,62,63,65,68,97)]
#df<-sort(sample(nrow(dataframe3),nrow(dataframe3)*0.7))
#View(dataframe3)
train_data2<-dataframe4[1:444,]
test_data2<-dataframe4[444:624,]

lmvalues<-lm(Rent.of.House~.,data=train_data2)
summary(lmvalues)
predict_lmvalues<-predict(lmvalues,test_data2)
#View(test_data1)
x = 1:length(test_data2[,25])
plot(x, test_data2[,25], col = "red", type = "l", lwd=2, main = "Linear regression for Rent of House")
lines(x, predict_lmvalues, col = "blue", lwd=2)
legend("topright",  legend = c("original values", "predicted values"), fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()

LR_MAPE_lm = MAPE(test_data2[,25],predict_lmvalues) 
LR_R_lm = RSQUARE(test_data2[,25],predict_lmvalues) 
Accuracy_lm = 100 - LR_MAPE_lm
print("R-Square: ")
print(LR_R_lm)
print('Accuracy of Linear Regression: ')
print(Accuracy_lm)





modelsvm = svm(Rent.of.House~.,train_data2)
summary(modelsvm)
predict_svmvalues = predict(modelsvm, test_data2)

plot(x, test_data2[,25], col = "red", type = "l", lwd=2, main = "SVM model's for Rent of House")
lines(x, predict_svmvalues, col = "blue", lwd=2)
legend("topright",  legend = c("original values", "predicted values"), fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()

LR_MAPE_svm = MAPE(test_data2[,25],predict_svmvalues) 
LR_R_svm = RSQUARE(test_data2[,25],predict_svmvalues) 
Accuracy_svm = 100 - LR_MAPE_svm
print("R-Square: ")
print(LR_R_svm)
print('Accuracy of SVM: ')
print(Accuracy_svm)





knnmodel = knnreg(train_data2[,-25],train_data2[,25])
str(knnmodel)
predict_knnvalues = predict(knnmodel, test_data2[,-25])
mse = mean((test_data2[,25] - predict_knnvalues)^2)
mae = caret::MAE(test_data2[,25], predict_knnvalues)
rmse = caret::RMSE(test_data2[,25], predict_knnvalues)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

x = 1:length(test_data2[,25])

plot(x, test_data2[,25], col = "red", type = "l", lwd=2, main = "KNN Algorithm for Rent of House")
lines(x, predict_knnvalues, col = "blue", lwd=2)
legend("topright",  legend = c("original values", "predicted values"), fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()

LR_MAPE_knn = MAPE(test_data2[,25],predict_knnvalues) 
LR_R_knn = RSQUARE(test_data2[,25],predict_knnvalues) 
Accuracy_knn = 100 - LR_MAPE_knn
print("R-Square: ")
print(LR_R_knn)
print('Accuracy of KNN: ')
print(Accuracy_knn)






png(file = "decision_tree.png",width = 1000, height = 1000)
dataframe5=dataframe4[1:100,]
output.tree <- ctree(Rent.of.House ~., data = train_data2)
plot(output.tree)
dev.off()
predict_dtreevalues<-predict(output.tree, test_data2[,-25])
View(test_data2)
plot(x, test_data2[,25], col = "red", type = "l", lwd=2, main = "Decision Tree for Rent of House")
lines(x, predict_dtreevalues, col = "blue", lwd=2)
legend("topright",  legend = c("original values", "predicted values"), fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()

LR_MAPE_dtree = MAPE(test_data2[,25],predict_dtreevalues) 
LR_R_dtree = RSQUARE(test_data2[,25],predict_dtreevalues) 
Accuracy_dtree = 100 - LR_MAPE_dtree
print("R-Square: ")
print(LR_R_dtree)
print('Accuracy of Decision tree: ')
print(Accuracy_dtree)




predict_linear = predict(lmvalues, test_data2[40,-25])
predict_Linear
test_data2[40,25]

predict_SVM = predict(modelsvm, test_data2[40,-25])
predict_SVM
test_data2[40,25]

predict_knn = predict(knnmodel, test_data2[120,-25])
predict_knn
test_data2[120,25]

predict_Decision = predict(output.tree, test_data2[40,-25])
predict_Decision
test_data2[40,25]
