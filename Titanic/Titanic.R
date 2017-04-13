setwd("E:/Rfile/Titanic")
library(gsubfn)
library(proto)
library(RSQLite)
library(sqldf)
library(ggplot2)
library(rpart)
library(MASS)
library(neuralnet)
library(dplyr)
library(randomForest)
library(xgboost)


#回归问题（Regression Problems）：我们想要预测的变量是数字（例如，房子的价格）
#分类问题（Classification Problems）：我们想要预测的变量是「是/否」的答案（例如，某一设备是否会经历设备故障）

#机器学习算法：线性模型（linear models）、树型模型（tree-based models）、和神经网络（neural networks）。

train <- read.csv("E:/Rfile/Titanic/train.csv")
test<- read.csv("E:/Rfile/Titanic/test.csv")
str(train)
table(train$Survived)

test$Survived <- rep(0,418)
submit <- data.frame(PassengerId= test$PassengerId,Survived=test$Survived)
write.csv(submit,file="alldie.csv",row.names = F)

# 获救与未获救之比
sum(train['Survived']==1)
sum(train['Survived']==0)

#船舱等级 与存活之比
sum(train['Pclass']==1)
sum(train['Pclass']==2)
sum(train['Pclass']==3)

#sqldf
dfp1 <- sqldf("select Pclass,count(Survived) from train t1 where t1.Survived='1' group by Pclass ",row.names=TRUE)
dfp2 <- sqldf("select Pclass,count(Survived) from train t1 where t1.Survived='0' group by Pclass ",row.names=TRUE)

dfsex <- sqldf("select Sex,count(Sex) from train t1  group by sex ",row.names=TRUE)
dfsex1 <- sqldf("select Sex,count(Survived) from train t1 where t1.Survived='1' group by sex ",row.names=TRUE)
dfsex2 <- sqldf("select Sex,count(Survived) from train t1 where t1.Survived='0' group by sex ",row.names=TRUE)

dfCabin1 <- sqldf("select Embarked ,count(Survived) from train t1 where t1.Survived='1' group by Embarked  ",row.names=TRUE)
dfCabin2 <- sqldf("select Embarked ,count(Survived) from train t1 where t1.Survived='0' group by Embarked  ",row.names=TRUE)


#船舱等级 与存活之比
par(mfrow=c(2,3))
opar <-par(no.readonly = True)
#par(lwd=2,cex=1.5,font.lab=2)

pclass_count <- table(train$Survived,train$Pclass)
barplot(pclass_count,main = "Pclass And Servived",
                     xlab = "Pclass",
                     col = c("darkblue","red"),
                     beside = TRUE)
legend("topleft",pch=c(15,16),legend=rownames(pclass_count),col = c("darkblue","red"),ncol=1,bg="aliceblue")

#性别与船舱等级
sp_count <- table(train$Sex,train$Pclass)
barplot(sp_count,main = "Survived And Pclass",xlab = "Pclass",
        col = c("darkblue","red"),
        beside = TRUE)

legend("topleft",pch=c(15,16),legend=rownames(sp_count),col = c("darkblue","red"),ncol=1,bg="aliceblue")

# 性别与存活率

age_count <- table(train$Survived,train$Sex)
barplot(age_count,main = "Age And Servived",
        xlab = "Age",
        col = c("darkblue","red"),
        beside = TRUE)
legend("topleft",pch=c(15,16),legend=rownames(age_count),col = c("darkblue","red"),ncol=1,bg="aliceblue")


# 登船港口
embarked_count <- table(train$Survived,train$Embarked)
barplot(embarked_count,main = "Embarked And Servived",
        xlab = "Embarked",
        col = c("darkblue","red"),
        beside = TRUE)
legend("topleft",pch=c(15,16),legend=rownames(embarked_count),col = c("darkblue","red"),ncol=1,bg="aliceblue")

# 年龄与存活率
dfage1 <- sqldf("select Age,count(Survived) as sum from train t1 where t1.Survived='1' and age is not null group by Age ",row.names=TRUE)
dfage2 <- sqldf("select Age,count(Survived) as sum from train t1 where t1.Survived='0' and age is not null group by Age ",row.names=TRUE)


xrange <- range(0,100,by=1)
yrange <- range(0,25,by=1)
#plot(xrange,yrange,xlab ="Age",ylab = "Sum")

plot(dfage1,type="b",pch=22,lty=3,col="darkblue")
plot(dfage2,type="b",pch=22,lty=3,col="red")

# 构建分类列联表

prop.table(table(train$Sex,train$Survived))

test$Survived <- 0

test$Survived[test$Sex=='female'] <- 1
submit<- data.frame(PassengerId = test$PassengerId,Survived = test$Survived)
write.csv(submit,file = "alldiebutfemale.csv",row.names = F)

# 绘制直方图
hist(train$Age)


# 老幼生还分布
train$oldchild <-0
train$oldchild[train$Age < 20 | train$Age > 60]<- 1
aggregate(Survived ~ oldchild + Sex,data = train,FUN = sum)

#model 1

test$Survived <- 0
# 考虑性别变量
test$Survived[test$Sex=='female'] <- 1
test$Survived[test$Sex=='female' & (test$Age<20|test$Age>60)] <-0
submit<- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit,file = "alldiebutfemalemiddleage.csv",row.names = F)

library(rpart)
rpart_ml <- rpart(Survived ~ Pclass+Sex+Age+Parch+Fare,data=train,method = "class")
plot(rpart_ml)
text(rpart_ml)
# 从结果上看:第一变量是性别：对男性而言，年龄是第二分割点；
# 对女性而言，仓位则成了第二分割点，所以决策树的分割相对还是比较精细的。

# 预测函数检验
predict_rp_ml <- predict(rpart_ml,test,type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = predict_rp_ml)
write.csv(submit,file = "rpart_tree.csv",row.names = FALSE)

# aprt_tree 补下age的缺失值
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare,data=train[!is.na(train$Age),], method="anova")
train$Age[is.na(train$Age)] <- predict(Agefit, train[is.na(train$Age),])

# 随机森林算法
library(randomForest)
rd_ml <- randomForest(as.factor(Survived) ~ Pclass + Age + Sex + SibSp + Parch + Fare, data=train, ntree = 2000)
rd_ml_predict <- as.numeric(predict(rd_ml, test))-1
submit0 <- data.frame(PassengerId = test$PassengerId, Survived = rd_ml_predict)
# 预测出的缺失值用决策树结果充数 这可看作模型嵌套
submit0[is.na(submit0$Survived),2] <- submit[is.na(submit0$Survived),2]
write.csv(submit, file = "randomforest.csv", row.names = FALSE)

# 逻辑回归
log_ml <- glm(Survived ~ Pclass + Age +SibSp+Parch + Fare ,data=train,family = binomial())
summary(log_ml)
coef(log_ml)

exp(coef(log_ml))
plot(log_ml)
log_ml_predict <- predict(log_ml,test,method="class")


############################################# 决策树############################################
#头衔特征抽取

train <- read.csv("E:/Rfile/Titanic/train.csv")
test<- read.csv("E:/Rfile/Titanic/test.csv")
str(train)
full <- bind_rows(train, test)
full$Title <- gsub('(.*,)|(\\..*)','',full$Name)
table(gsub('(.*,)|(\\..*)','',full$Name))
table(full$Sex,full$Title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

#把具有相近含义的title做个归一化
full$Title[full$Title == 'Mlle'] <- 'Miss'
full$Title[full$Title == 'Ms'  ] <- 'Miss'
full$Title[full$Title == 'Mme' ] <- 'Mrs'

table(full$Title,full$Survived)
mosaicplot(table(full$Sex, full$titleN), shade=TRUE)

# 家庭成员数目
full$familysize <- full$SibSp + full$Parch + 1

#aes(x = familysize, fill = factor(Survived))表示坐标轴的x轴我们取Fsize的值，这里的fill是指用什么变量填充统计值，
ggplot(full[1:891,], aes(x = familysize, fill = factor(Survived))) + geom_bar(stat='count', position='dodge')+ scale_x_continuous(breaks=c(1:12)) + labs(x = 'Family Size') + theme_few()

#家庭大小分类
full$FsizeD[full$familysize == 1] <- 'singleton'
full$FsizeD[full$familysize <5 & full$familysize >= 2] <- 'small'
full$FsizeD[full$familysize >=5] <- 'large'
table(full$FsizeD,full$Survived)

# 家庭大小与逃生
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

# 决策树
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Title','FsizeD')
full[factor_vars] <- lapply(full[factor_vars],function(x) as.factor(x))
train_data <- full[1:891,]
test_data <- full[892:1309,]

rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Embarked + Title + FsizeD, data = train_data)
print(rf_model)
summary(rf_model)
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# 计算特征重要程度
importance(rf_model)

# 数据预测
predict_rf_ml<- predict(rf_model,test_data)
rf_solution <- data.frame(PassengerID = test$PassengerId,Survived = predict_rf_ml)
write.csv(rf_solution,file = "rf_solution.csv",row.names = F)


############################################ 神经网络 ############################################
set.seed(500)
library(MASS)
library(neuralnet)
library(VIM)
library(mice) # 插补缺失值
library("ggplot2")
library(ggthemes)

# 确认数据是否有缺失值
train <- read.csv("E:/Rfile/Titanic/train.csv")
test<- read.csv("E:/Rfile/Titanic/test.csv")
str(train)
apply(train,2,function(x) sum(is.na(x)))

# 将male female 转化为0,1
train$Sex<-ifelse(train$Sex=='male',1,0)
#train$Sex<-(train$Sex=="male")*1
# 填补Age空值与删除Age空值

aggr(train, prop=FALSE, numbers=TRUE)
matrixplot(train)
marginplot(train[c("Pclass","Survived")], pch=c(20), col=c("darkgray", "red", "blue"))

# 删除空值
train_data <- na.omit(train)

#mydata$Young<-ifelse(mydata$Age<25,1,0)

as.matrix(train)
Titanic_net <- neuralnet(Survived ~ Pclass + Sex+Age+SibSp+Parch,train_data,err.fct="ce",linear.output=FALSE, likelihood=TRUE)
head(train)
plot(Titanic_net)
