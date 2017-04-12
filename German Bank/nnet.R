setwd("E:/Rfile/bank/")
# 神经网络
# AMORE用来构建前馈神经网络的函数是newff（）
library(AMORE)
library(Rcpp)
library(RSNNS)

bank_data <- read.table("E:/Rfile/bank/german.data-numeric.txt",header = F)
test<- read.table("E:/Rfile/bank/german.txt",header = F)

# 数据归一化
bank_data$V25[bank_data$V25 == 1] <- 0
bank_data$V25[bank_data$V25 == 2] <- 1

# 拆分数据集 test,predcit
test_data <- bank_data$V25
predict_data <- bank_data[,1:24]

# 把输入样本归一化,y=(x-MinValue)/(MaxValue-MinValue)来处理
# apply(X, MARGIN, FUN, ...) 
# X:数组、矩阵、数据框
# MARGIN: 按行计算或按按列计算，1表示按行，2表示按列

# 按列求最大值，最小值
maxP <- apply(predict_data,2,"max")
minP <- apply(predict_data,2,"min")

# t() function transposes a matrix or data.frame.矩阵或数据框转置
predict_data1 <- t(predict_data)
std_data <-(maxP - minP)
# 将除数为0的数据归一化为1
std_data[std_data == 0] <- 1
std_data <- (std_data-minP)/std_data
std_data <- t(std_data)

# 抽样300 作为测试集
testdat <- sample(1:nrow(bank_data),300,replace = F)
train_p <- predict_data[-testdat,]
train_t <- test_data[-testdat]

test_p <- predict_data[testdat,]
test_t <- test_data[testdat]


#创建网络
net <- newff(n.neurons=c(24,24,1), learning.rate.global=1e-4, momentum.global=0.01,error.criterium="LMS", Stao=NA, hidden.layer="tansig", output.layer="purelin", method="ADAPTgdwm")

# 批量注释 ctrl+shift+c
# learning.rate.global全局的学习率。
# momentum.global全局的动量值(貌似是步长）。
# error.criterium误差衡量算法，如用误差平方和，选“LMS”。
# hidden.layer隐藏层激活函数。
# output.layer输出层激活函数。
# method  学习方法，如梯度下降。


#训练
model <- train(net, train_p, train_t, error.criterium="LMS",report=TRUE, show.step=100, n.shows=10 )

#测试
testO <- sim(model$net, test_p)
testO[testO <0.5] <- 0
testO[testO >= 0.5] <- 1
table(testO,test_t)


############# RSNNS ###########

library(RSNNS)
library(Rcpp)
data("iris")

# #将数据顺序打乱
iris <- iris[sample(1:nrow(iris),length(1:nrow(iris))),1:ncol(iris)]

# 定义网络输入
iris_train <- iris[,1:4]

# 定义网络输出，并将数据进行格式转换
irisTargets <- decodeClassLabels(iris[,5])

# 划分训练集 和测试集
iris = splitForTrainingAndTest(iris_train,irisTargets,ratio = 0.15)

# 数据标准化
iris = normTrainingAndTestSet(iris)

# 利用MLP 命令执行前馈反向传播神经网络算法
model = mlp(iris$inputsTrain, iris$targetsTrain, size=5, learnFunc = "Quickprop", learnFuncParams = c(0.1,2.0,0.0001,0.1),maxit = 100,inputsTest = iris$inputsTest, targetsTest=iris$targetsTest)

# 利用上述模型进行预测

predictons = predict(model,iris$inputsTest)

# 生成混淆矩阵，观察预测精度
confusionMatrix(iris$targetsTest,predictons)


############## RSNNS German Bank 信用好坏 ###########


setwd("E:/Rfile/bank/")
library(RSNNS)
library(nnet)
#library(caret)
library(devtools) # 导入plot.nnet()
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
bank_data <- read.table("E:/Rfile/bank/german.data-numeric.txt",header = F)


# 数据归一化
bank_data$V25[bank_data$V25 == 1] <- 0
bank_data$V25[bank_data$V25 == 2] <- 1

bk_data_Train <- bank_data[,1:24]
bk_data_Targets <- decodeClassLabels(bank_data[,25])

bk_data = splitForTrainingAndTest(bk_data_Train,bk_data_Targets,ratio = 0.15)

# 数据标准化
bk_data = normTrainingAndTestSet(bk_data)


# 利用MLP 命令执行前馈反向传播神经网络算法
model = mlp(bk_data$inputsTrain, bk_data$targetsTrain, size=10, learnFunc = "Quickprop", learnFuncParams = c(0.1,2.0,0.0001,0.1),maxit = 100,inputsTest = bk_data$inputsTest, targetsTest=bk_data$targetsTest)
plotIterativeError(model) 

par(mar=numeric(4),family='serif')
plot.nnet(model)

# 利用上述模型进行预测

predictons = predict(model,bk_data$inputsTest)

# 生成混淆矩阵，观察预测精度，直接使用caret包中 confusionMatrix(),求得准确率
bank_Fre <- confusionMatrix(bk_data$targetsTest,predictons)

sum(diag(bank_Fre))/sum(bank_Fre)

#




















