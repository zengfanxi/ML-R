# 朴素贝叶斯算法
library(caret)
library(klaR)
library(pROC)
library(e1071)
library(randomForest)
mydata <- read.csv("C:/Users/Administrator/Desktop/ML/朴素贝叶斯-毒蘑菇检测/data.csv",sep=",",header=T)
str(mydata)
summary(mydata)

set.seed(1000)
index <- sample(1:nrow(mydata),0.75*nrow(mydata))
train_data <- mydata[index,]
test_data <- mydata[-index,]

prop.table(table(mydata$type))
prop.table(table(train_data$type))
prop.table(table(test_data$type))
# 借助caret包实现特征选择
##构建rfe函数的控制参数（使用随机森林函数和10折交叉验证，抽取5组样本）
rfeControl_rf <- rfeControl(
  functions = rfFuncs,
  method = "cv",
  repeats = 5
)
##使用rfe函数进行特征选择
fs_nb <- rfe(x=train_data[,-1],
             y=train_data[,1],
             sizes = seq(4,21,2),
             rfeControl = rfeControl_rf)
fs_nb
plot(fs_nb,type=c("g","o"))
fs_nb$optVariables #输出所需要选入的６个变量

# 借助klrR包中的naivebayes函数来预测
vars <- c("type",fs_nb$optVariables)
fit <- NaiveBayes(type ~.,data=train_data[,vars]) #建模
pred <- predict(fit,test_data[,vars][,-1]) #预测
## 构建混淆矩阵
freq <- table(pred$class,test_data[,1])
freq
## 模型准确率
accuracy <- sum(diag(freq))/sum(freq)
accuracy

## 模型AUC值
fit_auc <- roc(as.integer(test_data[,1]),
               as.integer(factor(pred$class)))
## 输出roc曲线
plot(fit_auc,print.auc=T,auc.polygon=T,
     grid = c(0.1,0.2),grid.col=c("green","red"),
     max.auc.polygon=T,auc.polygon.col="steelblue")