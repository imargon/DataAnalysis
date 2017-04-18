# 分类模型评价一般有以下几种方法：混淆矩阵(Confusion Matrix)、收益图(Gain Chart)、提升图(Lift Chart)、KS图(KS Chart)、接受者操作特性曲线(ROC Chart)。

##################混淆矩阵 ####################
# 混淆矩阵将分类预测结果与实际目标进行比较，并汇总成NXN列联表（N为分类类型数）。
# 它的本质就是一个N*N列联表（N为分类的类别数）,混淆矩阵是模型在总体样本的表现
# 混淆矩阵的缺点:
# 1.需要自己确定阀值,可以看到，混淆矩阵4个值的确定都依赖于最直线我们主观设定的0.5。如果只依靠混淆矩阵这种原始的方法，那么不经过繁琐的试错我们无法确认哪个阀值是最好的。

# 2.不平衡数据鲁棒性不好
# 一些positive事件发生概率极小的不平衡数据集(imbalanced data),混淆矩阵可能效果不好。
# 比如对信用卡交易是否异常做分类的情形，很可能1w笔交易中只有1笔交易是异常的。
# 一个将将所有交易都判定为正常的分类器，准确率是99.99%。这个数字虽然很高，但是没有任何现实意义。

# https://zhuanlan.zhihu.com/p/26064055

# TP:TRUE POSITIVE RATE,FN:False Negative RATE
# 其中FP和FN就是我们常说的第一类错误与第二类错误
#-----------------------------------------
#        |          预测结果
#真实情况|--------------------------------
#        |     正例    |     反例
#--------|-------------|------------------
#  正例  |  TP(真正例) |   FN(假反例)
#--------|-------------|------------------
#  反例  |  FP(假正例) |   TN(真反例)
#-----------------------------------------

# 训练精度:Accuracy = (TP+TN)/(TP+TN+FN+FP) 
library(caret)
confusionMatrix(iris_Freq_3, positive = "1")


####################收益图与提升图#########################

# RPP = (TP+FP)/(TP+TN+FN+FP)
# 指标就表示在所有预测中，预测为正的比例，我们把累积的RPP值作为收益图与提升图的公共横轴。
# 纵轴需要评价模型在预测为正的事件中，正确的概率

library(ROCR)
data("ROCR.simple")

# prediction需要分别制定预测概率与类别值,主要目的是将概率值与类别值转换为performance函数所要求的对象格式
pred <- prediction(ROCR.simple$predictions,ROCR.simple$labels)
par(mfrow = c(1,2))

# performance函数的三个参数分别为对象名,纵坐标,横坐标(该函数会生成众多测度指标以供使用)

gain <- performance(pred,"tpr","rpp")
lift <- performance(pred,"lift","rpp")

plot(gain,main="Gain Chart")
plot(lift,main="Lift Chart")

# 一般来讲，对于收益图，理想的情况应该是快速达到很高的累计收益值，并很快趋于100%；
# 而理想的提升图应该在很高的提升值上保持一段，或缓慢下降一段，然后迅速下降到1。

####################    ROC与AUC    图#########################
# ROC（Receiver Operating Characteristic）曲线和AUC常被用来评价一个二值分类器（binary classifier）的优劣
# ROC全称为“受试者工作特征”(Receiver Operating Characteristic)曲线，该曲线不仅仅是机器学习的专属，它最早起源与雷达信号分析技术，并且广泛应用在医学、心理学等领域中。
# 简单来讲，在机器学习领域内，ROC曲线和前面所学习的收益图提升图一样，
# 也是通过不同阀值的调整实现对混淆矩阵的优化改良。
# ROC曲线正是基于分类阈值对泛化性能的影响成为模型评估的有力工具。

# ROC曲线需要使用到两个指标，分别是TPR和FPR，其中TPR为纵坐标，FPR为假正例率（False Positive Rate），计算公式为FPR=FP/(TN+FP)，在ROC中为横坐标。
# ROC曲线还可用于不同学习器的比较，一般来讲，如果一个学习器的ROC把另一个学习器的曲线完全包住，那么就可以认为前者的性能要优于后者。
# 不过这种情况并不常见，更多的是不同曲线之间有所交叉，这样的话就无法对学习器性能有一个直观上的判别，为此我们可以转为比较ROC曲线下的面积，这就是接下来要学习的AUC(Area Under Curve)。

# AUC的求解非常简单，当我们得到ROC曲线的所有横纵坐标{ (x1,y1),(x2,y2),...(xn,yn)} 后，
# 面积计算公式为：AUC=

# 由于ROC曲线一般都处于y=x这条直线的上方，所以AUC的取值范围在0.5和1之间，数值越大，对应的分类器效果就越好。这就解决了ROC曲线的缺陷：当不同的分类器曲线有交叉而不好判断时，转而求解各自的线性面积图来进行分类器的取舍。







# https://zhuanlan.zhihu.com/p/26106488
# 选用AER包中的Affairs数据集来构建Logistics回归模型，这个数据集记录了一组婚外情数据，其中包括参与者性别、年龄、婚龄、是否有小孩、宗教信仰程度（5分制，1表示反对，5表示非常信仰）、学历、职业和婚姻的自我评分（5分制，1表示非常不幸福，5表示非常幸福）。


# Logistics 与 SVM 的对比

# 使用pROC包
library(pROC)
library(e1071)
library(ROCR)
library(AER)
data(Affairs,package="AER")
Affairs$ynaffairs[Affairs$affairs >0] <- 1
Affairs$ynaffairs[Affairs$affairs ==0] <- 0
Affairs$ynaffair <-  factor(Affairs$ynaffair,levels=c(0,1), labels=c("No","Yes"))

#构建Logistics模型
log_ml <- glm(ynaffair ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating,data = Affairs,family = binomial())
log_pre <- predict(log_ml,type="response")
pred <- prediction(log_pre,Affairs$ynaffair)
performance(pred,'auc')@y.values
perf <- performance(pred,'tpr','fpr')
plot(perf)

log_pre <- predict(log_ml,type="response")
log_ml_roc  <- roc(Affairs$ynaffair,log_pre)
plot(log_ml_roc, print.auc=TRUE, auc.polygon=TRUE,grid=c(0.1, 0.2), grid.col=c("green", "red"), max.auc.polygon=TRUE, auc.polygon.col="skyblue", print.thres=TRUE)

# 根据以上代码得知模型计算的AUC值为0.712，对此我们可以这样解释：从此模型预测的正例中随机选择一个，再从预测的反例中随机选择一个，那么有71.2%的机会前者的分数会比后者高，比随机模型多了21.2%的可能性。

svm_ml <- svm(ynaffair ~ gender + age + yearsmarried +children +religiousness+education+occupation+rating,data=Affairs)
svm_pre <- as.factor(svm_ml$decision.values)
svm_pre <- as.ordered(svm_pre)
svm_ml_roc <- roc(Affairs$ynaffair,svm_pre)

plot.roc(svm_ml_roc,add = TRUE,col = "green",print.thres = TRUE)
plot(log_ml_roc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE,col="blue")
plot.roc(svm_ml_roc,add = TRUE,col = "green",print.thres = TRUE)

# 从图中可以看到，两条ROC曲线有所交叉，无法直接判别模型谁好谁坏，因此我们使用AUC值来进行判断：
# Logistics模型的AUC为0.712，而SVM的AUC为0.775，所以我们可以认为在这个数据集上SVM的性能要优于Logistics模型。






































