library(readxl);library(caret);library(glmnet);library(corrplot)
library(Metrics);library(ggplot2)
### data
setwd("C:/Users/Administrator/Desktop/lasso_data/") #数据路径
filename = "female_lasso_data.csv" # (male or female)_lasso_data.csv
class = strsplit(filename, "_")[[1]][1] # male or female
lasso_data <- read.csv(filename) #(male or female)_lasso_data.csv
set.seed(123)
trainIndex <- sample(1:nrow(lasso_data),0.75*nrow(lasso_data))
lasso_training <- lasso_data[trainIndex,]
lasso_validation <- lasso_data[-trainIndex,]



### lasso regression
lambdas <- seq(0,2, length.out = 100)
# 这个列索引?
# X <- as.matrix(female_lasso_training[,2:3641])
X <- as.matrix(lasso_training[,2:ncol(lasso_training)])
Y <- lasso_training[,1]
#set.seed(1245)
lasso_model <- cv.glmnet(X,Y,alpha = 1,lambda = lambdas,nfolds = 10)
plot(lasso_model)
plot(lasso_model$glmnet.fit, "lambda", label = T)
# write.csv(lasso_model$glmnet.fit),这个我运不了
lasso_coef_name = paste(class, "lasso_coef.csv", sep = "_")
write.csv(as.matrix(coef(lasso_model$glmnet.fit)), lasso_coef_name)



### lasso min or lasso 1se
lasso_best <- glmnet(X,Y,alpha = 1,lambda = lasso_model$lambda.min)
coef(lasso_best)
nonzero_coef1 <- sum(coef(lasso_best) != 0)
print(nonzero_coef1) # 多少个变量
lasso_se <- glmnet(X,Y,alpha = 1,lambda = lasso_model$lambda.1se)
nonzero_coef2 <- sum(coef(lasso_se) != 0)
print(nonzero_coef2) # 多少个变量
# choose which lambda: 1se  min也可以，看接不接受
lasso_best <- glmnet(X,Y,alpha = 1,lambda = lasso_model$lambda.1se)
lassobestbeta<-lasso_best$beta
lasso_training_name = paste(class, "lasso_training.csv", sep = "_")
write.csv(as.matrix(lassobestbeta),lasso_training_name)


### prediction
predict_training_filename = paste(class, "predict_training.csv", sep = "_")
predict_validation_filename = paste(class, "predict_validation.csv", sep = "_")
# predict_training <- predict(lasso_best,as.matrix(female_lasso_training[,2:3641]))
predict_training <- predict(lasso_best,as.matrix(lasso_training[,2:ncol(lasso_training)]))
write.csv(predict_training,predict_training_filename)
#predict_validation<- predict(lasso_best,as.matrix(female_lasso_validation[,2:3641]))
predict_validation<- predict(lasso_best,as.matrix(lasso_validation[,2:ncol(lasso_training)]))
write.csv(predict_validation,predict_validation_filename)



### 性能指标
#sprintf("??׼????ƽ??????????Ϊ: %f",mae(female_lasso_validation$age,predict_validation))
train_mse <- mean((predict_training - lasso_training$Age)^2)  # 计算均方误差(小)
sprintf("model train mse: %f",train_mse)
sprintf("model train rmse: %f",sqrt(train_mse))# 计算均方根误差(小)
sprintf("model train mae: %f",mae(lasso_training$Age,predict_training))# 平均绝对误差(小)
sprintf("model train cor: %f",cor(lasso_training$Age,predict_training))# 相关系数(大)
sprintf("model train r2: %f",R2(predict_training, lasso_training$Age))# 计算决定系数(大)
sprintf("model test mae: %f",mae(lasso_validation$Age,predict_validation))
sprintf("model test cor: %f",cor(lasso_validation$Age,predict_validation))
sprintf("model train r2: %f",R2(lasso_validation$Age,predict_validation))# 计算决定系数





