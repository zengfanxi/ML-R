# caretѧϰ����ʹ��
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

# �ع鳡���У����Ի���ÿ������������������������ɢ��ͼ��ϵ�ֲ�
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
# �Ʊ�����ת��
## ��ͳ��ʽ
### ����earth����etitanic���ݼ�
library(earth)
data(etitanic)
View(model.matrix(survived~.,data=etitanic))
### caret ���е�dummyVars����
dumm <- dummyVars(survived~.,data=etitanic)
View(predict(dumm,etitanic))
# ��/���㷽������Ĵ���
## ����mdrr���ݼ�
data(mdrr)
data.frame(table(mdrrDescr$nR11)) #����data.frame���ݽ�������Var1����Freq

nzv <- nearZeroVar(mdrrDescr,saveMetrics = T)
nzv[nzv$nzv,][1:10,]
nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]
dim(filteredDescr)
# ʶ��߶�������ر���
descrCor <- cor(filteredDescr)
highcorr <- sum(abs(descrCor[upper.tri(descrCor)])>0.99)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredDescr <- filteredDescr[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])
# ���������
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
# ��׼��
set.seed(100)
intrain <- sample(seq(along = mdrrClass), length(mdrrClass)/2)
training <- filteredDescr[intrain,]
test <- filteredDescr[-intrain,]