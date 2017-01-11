# 1. Copyright statement comment  
# 5. Function definitions 
# 6. Executed statements, if applicable (e.g., print, plot)
# 2. Author comment 
# 3. File description comment, including purpose of program, inputs, and outputs 
# File-Name:       construction_model.R        
# Date:            2016-05-17                                
# Author:          haocheng.ma
# Email:           haocheng.ma@haohedata.com
# Purpose:         model of data_construction
# Data Used:       data_construction.csv
# Packages Used:   ggplot2
# Machine:         haocheng.ma's lenovo

if(FALSE){
# options(repos=structure(c(CRAN="http://mirror.bjtu.edu.cn/cran/")))
options(repos=structure(c(CRAN="http://cran.rstudio.com/")))
cran.packages <-  c("e1071",
					"ggplot2",  # visualization
					"glmnet",
					"Hmisc",
					"igraph",
					"lme4",
					"lubridate",
					"plyr",
					"RCurl",
					"reshape",
					"RJSONIO",
					"scales",
					"tm",
					"XML",
					"mlr",
					"scales",  # visualization
					"dplyr",  # data manipulation
					"mice",  # imputation
					"randomForest",  # classification algorithm
					"grid",
					"rattle",
					"ggthemes")  # visualization

for(p in cran.packages) {
if(!suppressWarnings(require(p, character.only = TRUE, quietly = TRUE))) {
	cat(paste(p, "missing, will attempt to install\n"))
	install.packages(p, depend = TRUE)
}
else {
	cat(paste(p, "installed OK\n"))
}
}

print("### All required packages installed ###")
}				 
library('ggplot2')  # visualization
library('ggthemes')  # visualization
library('scales')  # visualization
library('plyr')
library('dplyr')  # data manipulation
library('mice')  # imputation
library('randomForest')  # classification algorithm
library('grid')
library('rattle')
library('mlr')

# 4. source() and library() statements
# set working directory
setwd("E://insurance//construction_model")
data.file <- read.csv("data_construction.csv", as.is=TRUE)[, 1:85]
names.file <- read.csv("names_data.csv", as.is=TRUE)

names.issue <- names.file[, 1:2]
names.merge <- merge(names.file[, 3:4], names.issue, by.y="names_cn_issue",
				             by.x="names_cn_data", all.x=TRUE, all.y=TRUE)
names.merge <- names.merge[is.na(names.merge$xuhao) == FALSE, ]
names.merge <- names.merge[order(names.merge$xuhao), ]
write.table(names.merge, "names.merge", sep="\t", row.names=FALSE)
names(data.file) <- names.merge$names_en_issue

# 了解你的数据
str(data.file)      # 查看数据结构
dim(data.file)      # 查看数据集维度
names(data.file)    # 查看变量名
head(data.file)     # 查看数据集前几条样例
tail(data.file)     # 查看数据集后几条样例
data.file[sample(nrow(data.file), 6), ]  # 随机查看数据集记录
summary(data.file)  # 获得变量描述统计信息


# 商业理解-重要指标
insured.business <- names.merge[, 3][c(13, 14, 18, 22, 23, 26, 33, 38, 39, 40, 42, 43, 44, 45, 46)]
claim.business <- names.merge[, 3][c(75, 78, 79, 80, 81, 82)]
agent.business <- names.merge[, 3][c(54, 57, 58, 61, 62, 63, 64, 65)]
hospital.business <- names.merge[, 3][c(67, 68, 69, 70, 71, 72)]
target.business <- names.merge[, 3][83]
all.business <- names.merge[,3][c(13, 14, 18, 22, 23, 26, 33, 38, 39, 40, 42, 43, 44, 45, 46, 
								54, 57, 58, 61, 62, 63, 64, 65, 67, 68, 69, 70, 71, 72, 
								75, 78, 79, 80, 81, 82, 83)]

#summary target variable
print(sum(data.file$FalseFlag))


CalculateMissingValues <- function(vector) {
  # Calculate the missing values
  # 
  # Args:
  #   vector: One vector which is to be calculated.
  #
  # Returns:
  #   Number of the missing values.
	total.length <- length(vector)
	number.missing <- 0
	for(i in 1 : total.length) {
		if(is.na(vector[i])==TRUE | vector[i]=="")
      number.missing <- number.missing + 1
	}
  return(number.missing)
}

IntoFactor <- function(x) { 
  if (class(x) == "factor") {
    n <- length(x)
    data.fac <- data.frame(x = x, y = 1 : n)
    output <- model.matrix(y ~ x, data.fac)[, -1]
    # Convert factor into dummy variable matrix
  } else {
    output <- x
    # if x is numeric, output is x
  }
  return(output)
}

if (Flase) {
missing.number <- sort(apply(data.file, 2, CalculateMissingValues), decreasing=TRUE)
print(missing.number)
names.missing <- names(which(missing.number > 12000))
print(names.missing)
}


# 特征工程1，变量选择
# 基于商业理解，只使用业务方推荐的指标
data.modify <- data.file[all.business]
if (FALSE) {
missing.number <- sort(apply(data.modify, 2, CalculateMissingValues), decreasing=TRUE)
print(missing.number)
}
sapply(data.modify, function(x) sum(is.na(x)))
sapply(data.modify, class)
#Imputation for Missing Data
data.modify$HospitalTime[is.na(data.modify$HospitalTime)==TRUE] <- 0
data.modify$DailyAmnt[which(is.na(data.modify$DailyAmnt)==TRUE)] <- 0
data.modify$MedicalAmnt[which(is.na(data.modify$MedicalAmnt)==TRUE)] <- 0
data.modify$CriticalAmnt[which(is.na(data.modify$CriticalAmnt)==TRUE)] <- 0
#edit(data.modify[, c("HospitalTime", "DailyAmnt", "MedicalAmnt", "CriticalAmnt")])


names.single <- names(which(sapply(data.modify, function(x) length(unique(x))==1)))
if (!identical(names_single, character(0))) {
  cat('handle single variables!\n')
}

#Derived variable ExaggeratedDegree
data.modify$ExaggeratedDegree <- 0
proportion <- mean(data.modify$CriticalAmnt > 0) / mean(data.modify$DailyAmnt > 0)
data.modify$ExaggeratedDegree <- pmax(data.modify$CriticalAmnt / proportion, 
                                      data.modify$DailyAmnt)
#edit(data_modify_missing[, c("CriticalAmnt", "DailyAmnt", "ExaggeratedDegree")])


names.binomial <- names(which(sapply(data.modify, function(x) length(unique(x))==2)))
names.binomial <- setdiff(names.binomial, target.business)
names.binomial

names.multiple<-names(which(sapply(data.modify, function(x) length(unique(x)) > 2))) 
names.multiple

names.category<-c(names.binomial, "InsuredEducation", "InsuredOccupationType", 
                  "AgentClass", "InsuredProvince", "HospitalClass", "DiseaseClass", "HospitalArea")
print(names.category)
names.numeric<-c("DailyAmnt", "CriticalAmnt", "WorkYear", "HealthSumPrem", 
    "TreatmentPerHospital", "MedicalAmnt", 
		"InsuredAppage", "PastCase", "HospitalTime", "BusinesstoCase", 
    "PersistencyRate", "InsuredIncome", "ClaimAmnt",
    "AgentPayoutRatio", "FYC", "ExaggeratedDegree")
print(names.numeric)


#as.factor因子化names.category
for (i in c(names.category, target.business)) {
	data.modify[, i] <- as.factor(data.modify[, i])
}
sapply(data.modify, class)
#分类变量水平过多
manylvls <- names(which(sapply(data.modify[names.category], 
            function(x) length(levels(x)) > 20))) 
manylvls
#取值集中在某个值
centors <- names(which(sapply(data.modify[names.category], 
            function(x) max(table(x)) / length(x) > 0.95))) #取值集中在某个值
centors
names.category <- setdiff(setdiff(names.category, manylvls), centors)
#summary(data.modify[, names.category])
vplayout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
if (FALSE) {
#barplot 
library(grid)  
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 4)))

#for循环，未发现能区分目标变量的字段
for(i in 1 : length(names.category)) {
	p <- ggplot(data.modify, aes(data.modify[, names.category[i]], fill=factor(FalseFlag))) +
			 geom_bar(stat='count', position='dodge') + 
       labs(x=names.category[i]) 
	print(p, vp = vplayout(ceiling(i / 4), (i - (ceiling(i / 4) - 1) * 4)))
}
}
# chisq.test or summary()，不能有效区分目标变量的字段。
kftest.delete <- vector()
for(i in 1:length(names.category)) {
	kftest <- chisq.test(table(data.modify[, names.category[i]], data.modify[, "FalseFlag"]))
	if (kftest$p.value > 0.05) {
    kftest.delete <- c(kftest.delete, names.category[i])
  }
	print(kftest)
}
kftest.delete

names.category <- setdiff(names.category, kftest.delete)
#列联表
for(i in names.category) {
  cat(paste(i,"\n"))
	print(table(data.modify[, i], data.modify[, "FalseFlag"]))
}

#相关系数
cor(data.modify[names.numeric], use="pairwise", method="pearson") > 0.80

#去掉分项，保留衍生变量
names.delete <- c("CriticalAmnt", "DailyAmnt", "ClaimAmnt")
names.numeric <- setdiff(names.numeric, names.delete)
if(FALSE){
#直方图,分为10组。
ggplot(data.modify, aes(BusinesstoCase, fill = factor(FalseFlag) )) + 
		geom_histogram(bins=10) +  scale_fill_brewer(palette="Set1")

ggplot(data.modify[data.modify$HospitalTime > 0,], aes(HospitalTime, fill=factor(FalseFlag))) + 
		geom_histogram(bins=25) + scale_fill_manual(values=c("#F8766D", "#00BA38"))
ggplot(data.modify, aes(FYC, fill=factor(FalseFlag))) + 
		geom_histogram(bins=10, position="dodge") + scale_fill_manual(values=c("gray", "blue"))+ theme_dark()
ggplot(data.modify[data.modify$FYC <= 10000,], aes(FYC, fill = factor(FalseFlag))) + 
#geom_histogram(bins = 10, position="fill")
		geom_histogram(bins = 10, position="dodge")
ggplot(data.modify, aes(AgentPayoutRatio , fill = factor(FalseFlag))) + 
		geom_histogram(bins = 10)
ggplot(data.modify, aes(x=FalseFlag,y=FYC,fill="red")) + 
		geom_boxplot() 
	
	
grid.newpage()
pushViewport(viewport(layout=grid.layout(4, 4)))
for (i in 1 : length(names.numeric)) {
	p <- ggplot(data.modify, aes(x=FalseFlag, y=data.modify[, names.numeric[i]], fill=FalseFlag)) + 
			geom_boxplot() + ylab(names.numeric[i])+  scale_fill_brewer(palette="Set2")+
      guides(fill = guide_legend(title = "target", title.position = "left")) + theme_dark()
	#print(p)	
	print(p, vp = vplayout(ceiling(i / 4), (i - (ceiling(i / 4) - 1) * 4)))
}
}

#分箱操作
 

#通过IV值筛选变量
var.iv <- function(input, target) {
	if (is.numeric(input)) {
    input <- binning(input, bins=10, method="quantile")
  }
  # print(input)
	temp1 <- table(input, target)
  # print("temp1:")
  # print(temp1)
	temp2 <- as.data.frame(matrix(temp1, length(unique(input)), 2))
  # print(temp2)
	temp3 <- sapply(temp2, function(input) input / sum(input))
  # print(temp3)
	if(!is.matrix(temp3)) {
    iv=0
  } else {
		woe <- log(temp3[, 1] / temp3[, 2]) * 100
		iv <- sum((temp3[, 1] - temp3[, 2])[!is.infinite(woe)] * woe[!is.infinite(woe)])
	}
	return(iv) 
}
# var.iv(data.modify[, input[25]], target= data.modify$FalseFlag)
input <- c(names.category, names.numeric)
iv.value <- sapply(data.modify[input], var.iv, target= data.modify$FalseFlag)  #计算变量的IV值
input <- names(which(iv.value > 5))
input <- setdiff(input, c("InsuredProvince", "HospitalArea"))
#"PersistencyRate"  "AgentPayoutRatio"
if(FALSE){
#数据预处理示例
quan.PersistencyRate<-binning(data.modify$PersistencyRate, 10, method="quantile", ordered=FALSE)  #等频离散化
plot(quan.PersistencyRate)
table(quan.PersistencyRate)
equal.AgentPayoutRatio <- cut(data.modify$AgentPayoutRatio, 10)                                       #等宽离散化
plot(equal.AgentPayoutRatio)
table(equal.AgentPayoutRatio)
PersistencyRate.scale=scale(data.modify$PersistencyRate,scale=T,center=T)                         #变量标准化
mean(PersistencyRate.scale);sd(PersistencyRate.scale)
PersistencyRate.std=(data.modify$PersistencyRate-min(data.modify$PersistencyRate)) / (max(data.modify$PersistencyRate)-min(data.modify$PersistencyRate)) #变量归一化
summary(PersistencyRate.std)
#rainfall.log=log(data_modify_missing$rainfall) #变量数学变换
}

if(FALSE){
#样本分割抽样
set.seed(42)
nobs <- nrow(data.modify)
n_train <- sample(nobs, 0.7*nobs)
n_validate <- sample(setdiff(seq_len(nobs), n_train), 0.15*nobs)
n_test <- setdiff(setdiff(seq_len(nobs), n_train), n_validate)
vars <- union(input,target.business)
train <- data.modify[n_train, vars]         #训练集
validate <- data.modify[n_validate, vars]   #验证集
test <- data.modify[n_test, vars]           #测试集
nrow(train)
nrow(validate)
nrow(test)
	
	
#过抽样示例
table(train$FalseFlag) #获取正样本比例
train_new <- rbind(train[train$FalseFlag==1, vars], train[train$FalseFlag==1, ], train) #正样本扩充3倍
dim(train)
dim(train_new)
table(train$FalseFlag)
table(train_new$FalseFlag)
	
#构建决策树模型
library(rpart)
library(randomForest)
model <- rpart(FalseFlag ~ . , data=train_new, method="class", parms=list(split="information")) #建立模型
modelrf <- randomForest(FalseFlag ~ . , train_new,  ntree=500)
summary(modelrf)
predictrf <- predict(modelrf, test)
table(predictrf, test$FalseFlag)
printcp(model) #查看结果
asRules(model) #获得规则
print(model)
fancyRpartPlot(model, main="Decision Tree for FalseFlag") #得到决策树图
library(rpart.plot)
rpart.plot(model)
rpart.plot(model,type=4,fallen.leaves=T)

#模型打分
pr <- predict(model, test, type="class")
	
#模型评估
library(ROCR)
pre <- predict(model, test, type="prob")[, 2]
pred <- prediction(pre, test[target.business])

plot(performance(pred, "tpr", "fpr"), col='red', main="ROC curves") 

}
#use mlr packages
trainfun = function(data, target, args = list(center, scale)) {
	## Identify numerical features
	cns = colnames(data)
	nums = setdiff(cns[sapply(data, is.numeric)], target)
	## Extract numerical features from the data set and call scale
	x = as.matrix(data[, nums, drop = FALSE])
	x = scale(x, center = args$center, scale = args$scale)
	## Store the scaling parameters in control
	## These are needed to preprocess the data before prediction
	control = args
	if (is.logical(control$center) && control$center)
		control$center = attr(x, "scaled:center")
	if (is.logical(control$scale) && control$scale)
		control$scale = attr(x, "scaled:scale")
	## Recombine the data
	data = data[, setdiff(cns, nums), drop = FALSE]
	data = cbind(data, as.data.frame(x))
	return(list(data = data, control = control))
}


predictfun = function(data, target, args, control) {
	## Identify numerical features
	cns = colnames(data)
	nums = cns[sapply(data, is.numeric)]
	## Extract numerical features from the data set and call scale
	x = as.matrix(data[, nums, drop = FALSE])
	x = scale(x, center = control$center, scale = control$scale)
	## Recombine the data
	data = data[, setdiff(cns, nums), drop = FALSE]  
	data = cbind(data, as.data.frame(x))
	return(data)
}

lrns_list = listLearners("classif", properties = c("prob","twoclass"))
#data_modify_missing[,input[1:3]]<-as.numeric(data_modify_missing[,input[1:3]])
task = makeClassifTask(id = "insurance", data = data.modify[,c(input,target_business)], target = target_business)

#过抽样示例
task.over=oversample(task, rate = table(getTaskTargets(task))[1]/table(getTaskTargets(task))[2])
#edit(getTaskData(task.over))
#table(getTaskTargets(task));table(getTaskTargets(task.over));

#生成哑变量
task.over=createDummyFeatures(task.over)
#edit(getTaskData(task.over))


fv2 = generateFilterValuesData(task.over, method = c("information.gain","chi.squared"))
fv2$data
plotFilterValues(fv2)
#选择5个特征
filtered.task = filterFeatures(task.over, method = "information.gain", abs = 5)


#定义学习机
lrn1 = makeLearner("classif.logreg", predict.type = "prob")
lrn2 = makeLearner("classif.svm", predict.type = "prob")
#randomForest(Species ~ ., data=data,importance = TRUE, proximity = FALSE, ntree = 100)
lrn3 = makeLearner("classif.randomForest", predict.type = "prob",ntree=100,importance = TRUE)
lrn4 = makeLearner("classif.rpart", predict.type = "prob")
lrn5 = makeLearner("classif.xgboost", predict.type = "prob")
#数据标准化，中心化
lrn1= makePreprocWrapper(lrn1, train = trainfun, predict = predictfun,par.vals = list(center = TRUE, scale = TRUE))
lrn2= makePreprocWrapper(lrn2, train = trainfun, predict = predictfun,par.vals = list(center = TRUE, scale = TRUE))
lrn3= makePreprocWrapper(lrn3, train = trainfun, predict = predictfun,par.vals = list(center = TRUE, scale = TRUE))
lrn4= makePreprocWrapper(lrn4, train = trainfun, predict = predictfun,par.vals = list(center = TRUE, scale = TRUE))
lrn5= makePreprocWrapper(lrn5, train = trainfun, predict = predictfun,par.vals = list(center = TRUE, scale = TRUE))

lrns =list(lrn2,lrn3,lrn4,lrn5)

#使用三折交叉验证
rdesc = makeResampleDesc(method = "CV",  iters = 3L,stratify = TRUE)

#训练模型
bmr = benchmark(lrns, filtered.task, rdesc,show.info = FALSE)
bmr

df = generateThreshVsPerfData(bmr, measures = list(fpr, tpr, mmce), aggregate = FALSE)
plotROCCurves(df)

if(FALSE){
ingCurveData(learners = lrns, task = filtered.task,
		percs = seq(0.1, 1, by = 0.1), measures = list(tp, fp, tn, fn),resampling = rdesc, show.info = FALSE)
plotLearningCurve(lc)
}
if(FALSE){
lc = generateLearningCurveData(learners = lrns, task = filtered.task,
		percs = seq(0.1, 1, by = 0.1), measures = list(mmce, acc),resampling = rdesc, show.info = FALSE)
plotLearningCurve(lc)
}


if(FALSE){
getBMRPerformances(bmr)
getBMRAggrPerformances(bmr)
getBMRPerformances(bmr, as.df = TRUE)
getBMRAggrPerformances(bmr, as.df = TRUE)
getBMRPredictions(bmr)
head(getBMRPredictions(bmr, as.df = TRUE))
head(getBMRPredictions(bmr, learner.ids = "classif.rpart.preproc", as.df = TRUE))
getBMRTaskIds(bmr)
getBMRLearnerIds(bmr)
getBMRMeasureIds(bmr)
getBMRModels(bmr)
getBMRModels(bmr, learner.ids = "classif.randomForest.preproc")[[1]]$classif.randomForest.preproc[[1]]
	
getBMRLearners(bmr)
getBMRMeasures(bmr)
perf = getBMRPerformances(bmr, as.df = TRUE)
head(perf)
plotBMRBoxplots(bmr, measure = mmce)
plotBMRBoxplots(bmr, measure = mmce, style = "violin") +
		aes(color = learner.id)
plotBMRSummary(bmr)

perf = getBMRPerformances(bmr, as.df = TRUE)

## Density plots for two tasks
qplot(mmce, colour = learner.id, data = perf, geom = "density")
## Define the resampling strategy
df = reshape2::melt(perf, id.vars = c("task.id", "learner.id", "iter"))
df = df[df$variable != "ber",]
head(df)
qplot(variable, value, data = df, colour = learner.id, geom = "boxplot",
		xlab = "measure", ylab = "performance") 
}

if(FALSE){
#ROC curve
library(ROCR)
preds = getBMRPredictions(bmr)[[1]]
ROCRpreds = lapply(preds, asROCRPrediction)
ROCRperfs = lapply(ROCRpreds, function(x) ROCR::performance(x, "tpr", "fpr"))	
plot(ROCRperfs[[1]], col = "blue", avg = "vertical", spread.estimate = "stderror",
		show.spread.at = seq(0.1, 0.8, 0.1) , plotCI.col = "blue", plotCI.lwd = 2, lwd = 2)
plot(ROCRperfs[[1]], col = "blue", lty = 2, lwd = 0.25, add = TRUE)
plot(ROCRperfs[[2]], col = "red", avg = "vertical", spread.estimate = "stderror",
		show.spread.at = seq(0.1, 0.8, 0.1), plotCI.col = "red", plotCI.lwd = 2, lwd = 2, add = TRUE)
plot(ROCRperfs[[2]], col = "red", lty = 2, lwd = 0.25, add = TRUE)
plot(ROCRperfs[[3]], col = "yellow", avg = "vertical", spread.estimate = "stderror",
		show.spread.at = seq(0.1, 0.8, 0.1), plotCI.col = "yellow", plotCI.lwd = 2, lwd = 2, add = TRUE)
plot(ROCRperfs[[3]], col = "yellow", lty = 2, lwd = 0.25, add = TRUE)
plot(ROCRperfs[[4]], col = "gray", avg = "vertical", spread.estimate = "stderror",
		show.spread.at = seq(0.1, 0.8, 0.1), plotCI.col = "gray", plotCI.lwd = 2, lwd = 2, add = TRUE)
plot(ROCRperfs[[4]], col = "gray", lty = 2, lwd = 0.25, add = TRUE)
legend("bottomright", legend = getBMRLearnerIds(bmr), lty = 1, lwd = 2, col = c("blue", "red","yellow","gray"))
	
}



