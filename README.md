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
*  而具体的p(ai|yi)和p(yi)都是能从训练样本中统计出来;
   p(ai|yi)表示该类别下该特征出现的概率;
   p(yi)表示全部类别中这个这个类别出现的概率;
### 工作流程
* **准备阶段**：确定特征属性，并对每个特征属性进行适当划分，然后由人工对一部分待分类项进行分类，形成训练样本
* **训练阶段**：计算每个类别在训练样本中的出现频率及每个特征属性划分对每个类别的条件概率估计
* **应用阶段**：使用分类器进行分类，输入是分类器和待分类样本，输出是样本属于的分类类别
### 优缺点
* **优点**：对小规模的数据表现很好，适合多分类任务，适合增量式训练
* **缺点**：对输入数据的表达形式很敏感（离散、连续，值极大极小之类的）
## 2. Knn(K-Nearest neighbors,K近邻)
### 一句话概括
给一个训练数据集和一个新的实例，在训练数据集中找出与这个新实例最近的k个训练实例，然后统计最近的k个训练实例中所属类别计数最多的那个类，就是新实例的类
### 三个要素
* K值的选择
> 1.  交叉验证法选择最优K值
> 2.  K值一般低于训练样本上的平方根
* 距离度量的计算方式
* 分类决策规则
## 3. Logistic Regression
