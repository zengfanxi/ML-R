# caret学习测试使用
library(caret)
library(AppliedPredictiveModeling)
library(ggplot2)
library(reshape2)
transparentTheme(trans = .4)
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

View(iris)

featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 3))

transparentTheme(trans = .9)
featurePlot(x = iris[, 1:4], 
            y = iris$Species,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(4, 1), 
            auto.key = list(columns = 3))

featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
            auto.key = list(columns = 2))

# 回归场景中，可以绘制每个连续性输入变量与因变量的散点图关系分布
library(mlbench)
data(BostonHousing)
regVar <- c("age", "lstat", "tax")
str(BostonHousing[, regVar])
featurePlot(x = BostonHousing[, regVar], 
            y = BostonHousing$medv, 
            plot = "scatter", 
            layout = c(3, 1))
featurePlot(x = BostonHousing[, regVar], 
            y = BostonHousing$medv, 
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(3, 1))
# 哑变量的转化
## 传统方式
### 借用earth包中etitanic数据集
library(earth)
data(etitanic)
View(model.matrix(survived~.,data=etitanic))
### caret 包中的dummyVars方法
dumm <- dummyVars(survived~.,data=etitanic)
View(predict(dumm,etitanic))
# 零/近零方差变量的处理
## 采用mdrr数据集
data(mdrr)
data.frame(table(mdrrDescr$nR11)) #加上data.frame数据结果会加上Var1，和Freq

nzv <- nearZeroVar(mdrrDescr,saveMetrics = T)
nzv[nzv$nzv,][1:10,]
nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]
dim(filteredDescr)
# 识别高度线性相关变量
descrCor <- cor(filteredDescr)
highcorr <- sum(abs(descrCor[upper.tri(descrCor)])>0.99)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredDescr <- filteredDescr[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])
# 线性相关性
library(caret)
ltfrDesign <- matrix(0, nrow=6, ncol=6)
ltfrDesign[,1] <- c(1, 1, 1, 1, 1, 1)
ltfrDesign[,2] <- c(1, 1, 1, 0, 0, 0)
ltfrDesign[,3] <- c(0, 0, 0, 1, 1, 1)
ltfrDesign[,4] <- c(1, 0, 0, 1, 0, 0)
ltfrDesign[,5] <- c(0, 1, 0, 0, 1, 0)
ltfrDesign[,6] <- c(0, 0, 1, 0, 0, 1)

comboInfo <- findLinearCombos(ltfrDesign)
ltfrDesign[,-comboInfo$remove]
# 标准化
set.seed(100)
intrain <- sample(seq(along = mdrrClass), length(mdrrClass)/2)
training <- filteredDescr[intrain,]
test <- filteredDescr[-intrain,]
