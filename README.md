# ML-R
## 1. Naive Bayesian(朴素贝叶斯)
### 一句话概括
P(A∩B)=P(A)*P(B|A)=P(B)*P(A|B)
所以有：P(A|B)=P(B|A)*P(A)/P(B)
对于给出的待分类项，求解在此项出现的条件下各个目标类别出现的概率，哪个最大，就认为此待分类项属于哪个类别
### 工作原理
*  假设现在有样本x=(a1,a2,a3,…an)这个待分类项(并认为x里面的特征独立)
*  假设现在有分类目标Y={y1,y2,y3,y4..yn}
*  那么max(P(y1|x),P(y2|x),P(y3|x)..P(yn|x))中的最大者就是最终的分类类别
*  而P(yi|x)=p(x|yi)*P(yi)/P(x)
*  因为x对于每个分类目标来说都一样，所以就是求max(P(x|yi)*p(yi))
*  P(x|yi)*p(yi)=p(yi)*PI(P(ai|yi)) (PI表示连乘)
*  而具体的p(ai|yi)和p(yi)都是能从训练样本中统计出来
    p(ai|yi)表示该类别下该特征出现的概率
    p(yi)表示全部类别中这个这个类别出现的概率
## 2. Knn
## 3. Logistic Regression
