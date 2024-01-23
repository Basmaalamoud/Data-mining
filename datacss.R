#The getwd() function tells you what the current working directory is
getwd()

#setwd function to change the directory you work on 
setwd("C:/Users/xlady/OneDrive/Documents")

################## Importing the dataset ##################
#read file named "Data.csv" and save it in dataframe named "dataset"
dataset = read.csv('C:/Users/xlady/OneDrive/Documents/data minig/smoke.csv')
View(dataset)
str(dataset)

##is.na(dataset)

sum(is.na(dataset))

#summary

summary(dataset)

# instal packages for outliers

install.packages("outliers")
library(outliers)

#Box plot 

boxplot(dataset$age)
boxplot(dataset$height.cm.)
boxplot(dataset$weight.kg.)
boxplot(dataset$waist.cm.)
boxplot(dataset$systolic)
boxplot(dataset$relaxation)
boxplot(dataset$fasting.blood.sugar)
boxplot(dataset$Cholesterol)
boxplot(dataset$triglyceride)
boxplot(dataset$HDL)
boxplot(dataset$LDL)
boxplot(dataset$hemoglobin)
boxplot(dataset$Urine.protein)
boxplot(dataset$serum.creatinine)
boxplot(dataset$AST)
boxplot(dataset$ALT)
boxplot(dataset$Gtp)

#Histgram 

hist(dataset$age)
hist(dataset$weight.kg.)

# catter 

plot(dataset$hemoglobin, dataset$Cholesterol)

#plot pie chart

install.packages("dplyr") # install only one time then put this command as comment after installation
library(dplyr)
tab <- dataset %>% table()
precentages <- tab %>% prop.table() %>% round(3) * 100 
txt <- paste0(names(tab), '\n', precentages, '%') # text on chart
pie(tab, labels=txt) # plot pie chart

# plot chart 

dataset$dental.caries %>% table() %>% barplot() #plot bar chart
dataset$dental.caries %>% table() %>% barplot() #plot bar chart
plot(dataset$fasting.blood.sugar, dataset$hemoglobin)



# number of outliers

x1 = boxplot.stats(dataset$age)$out
x2 = boxplot.stats(dataset$height.cm.)$out
x3= boxplot.stats(dataset$weight.kg.)$out
x4= boxplot.stats(dataset$waist.cm.)$out
x5= boxplot.stats(dataset$systolic)$out
x6= boxplot.stats(dataset$relaxation)$out
x7= boxplot.stats(dataset$fasting.blood.sugar)$out
x8= boxplot.stats(dataset$Cholesterol)$out
x9 = boxplot.stats(dataset$triglyceride)$out
x10= boxplot.stats(dataset$HDL)$out
x11= boxplot.stats(dataset$LDL)$out
x12= boxplot.stats(dataset$hemoglobin)$out
x13= boxplot.stats(dataset$Urine.protein)$out
x14= boxplot.stats(dataset$serum.creatinine)$out
x15= boxplot.stats(dataset$AST)$out
x16 = boxplot.stats(dataset$ALT)$out
x17 = boxplot.stats(dataset$Gtp)$out


#Data preprocessing:

##is.na(dataset)

sum(is.na(dataset))


#Encoding 
dataset$gender = factor(dataset$gender,levels = c("M","F"), labels = c(0,1))

dataset$tartar = factor(dataset$tartar,levels = c("N","Y"), labels = c(0,1))

dataset$oral = factor(dataset$oral,levels = c("N","Y"), labels = c(0,1))

#outliers
#After replacing and removing outliers: 


#age

boxplot.stats(dataset$age)$out   ###before
minVal <- boxplot.stats(dataset$age)$stats[1]
maxVal <- boxplot.stats(dataset$age)$stats [5]
myValue <- mean(dataset$age)
dataset [dataset$age < minVal | dataset$age > maxVal, "age"] <- myValue
boxplot(dataset$age)
boxplot.stats(dataset$age)$out ####after  
x1= boxplot.stats(dataset$age)$out ####after  
boxplot(dataset$age)

####################

#blood suger

boxplot.stats(dataset$fasting.blood.sugar)$out   ##before
minVal <- boxplot.stats(dataset$fasting.blood.sugar)$stats[1]
maxVal <- boxplot.stats(dataset$fasting.blood.sugar)$stats [5]
myValue <- mean(dataset$fasting.blood.sugar)
dataset [dataset$fasting.blood.sugar < minVal | dataset$fasting.blood.sugar > maxVal, "fasting.blood.sugar"] <- myValue
boxplot(dataset$fasting.blood.sugar)
boxplot.stats(dataset$fasting.blood.sugar)$out

outliers <- boxplot(dataset$fasting.blood.sugar, plot=FALSE)$out
dataset <- dataset[-which(dataset$fasting.blood.sugar %in% outliers),]
boxplot(dataset$fasting.blood.sugar)
boxplot.stats(dataset$fasting.blood.sugar)$out ###after
###########################################################

#height.cm.

boxplot.stats(dataset$height.cm.)$out   ##before
minVal <- boxplot.stats(dataset$height.cm.)$stats[1]
maxVal <- boxplot.stats(dataset$height.cm.)$stats [5]
myValue <- mean(dataset$height.cm.)
dataset [dataset$height.cm. < minVal | dataset$height.cm. > maxVal, "height.cm."] <- myValue
boxplot(dataset$height.cm.)
boxplot.stats(dataset$height.cm.)$out #after
########################################################

#weight.kg.

boxplot.stats(dataset$weight.kg.)$out   ##before
minVal <- boxplot.stats(dataset$weight.kg.)$stats[1]
maxVal <- boxplot.stats(dataset$weight.kg.)$stats [5]
myValue <- mean(dataset$weight.kg.)
dataset [dataset$weight.kg. < minVal | dataset$weight.kg. > maxVal, "weight.kg."] <- myValue
boxplot(dataset$weight.kg.)
boxplot.stats(dataset$weight.kg.)$out  #after
############################################################

#waist.cm.

boxplot.stats(dataset$waist.cm.)$out   ##before
minVal <- boxplot.stats(dataset$waist.cm.)$stats[1]
maxVal <- boxplot.stats(dataset$waist.cm.)$stats [5]
myValue <- mean(dataset$waist.cm.)
dataset [dataset$waist.cm. < minVal | dataset$waist.cm. > maxVal, "waist.cm."] <- myValue
boxplot(dataset$waist.cm.)
boxplot.stats(dataset$waist.cm.)$out  #after
##################################################

#systolic

boxplot.stats(dataset$systolic)$out   ##before
minVal <- boxplot.stats(dataset$systolic)$stats[1]
maxVal <- boxplot.stats(dataset$systolic)$stats [5]
myValue <- mean(dataset$systolic)
dataset [dataset$systolic < minVal | dataset$systolic > maxVal, "systolic"] <- myValue
boxplot(dataset$systolic)
boxplot.stats(dataset$systolic)$out  #after
################################################

#relaxation

boxplot.stats(dataset$relaxation)$out   ##before
minVal <- boxplot.stats(dataset$relaxation)$stats[1]
maxVal <- boxplot.stats(dataset$relaxation)$stats [5]
myValue <- mean(dataset$relaxation)
dataset [dataset$relaxation < minVal | dataset$relaxation > maxVal, "relaxation"] <- myValue
boxplot(dataset$relaxation)
boxplot.stats(dataset$relaxation)$out  

outliers <- boxplot(dataset$relaxation, plot=FALSE)$out
dataset <- dataset[-which(dataset$relaxation %in% outliers),]
boxplot(dataset$relaxation)
boxplot.stats(dataset$relaxation)$out ###after
##########################################################

#Cholesterol

boxplot.stats(dataset$Cholesterol)$out   ##before
minVal <- boxplot.stats(dataset$Cholesterol)$stats[1]
maxVal <- boxplot.stats(dataset$Cholesterol)$stats [5]
myValue <- mean(dataset$Cholesterol)
dataset [dataset$Cholesterol < minVal | dataset$Cholesterol > maxVal, "Cholesterol"] <- myValue
boxplot(dataset$Cholesterol)
boxplot.stats(dataset$Cholesterol)$out 

outliers <- boxplot(dataset$Cholesterol, plot=FALSE)$out
dataset <- dataset[-which(dataset$Cholesterol %in% outliers),]
boxplot(dataset$Cholesterol)
boxplot.stats(dataset$Cholesterol)$out ###after
#########################################################

#triglyceride

boxplot.stats(dataset$triglyceride)$out   ##before
minVal <- boxplot.stats(dataset$triglyceride)$stats[1]
maxVal <- boxplot.stats(dataset$triglyceride)$stats [5]
myValue <- mean(dataset$triglyceride)
dataset [dataset$triglyceride < minVal | dataset$triglyceride > maxVal, "triglyceride"] <- myValue
boxplot(dataset$triglyceride)
boxplot.stats(dataset$triglyceride)$out 

outliers <- boxplot(dataset$triglyceride, plot=FALSE)$out
dataset <- dataset[-which(dataset$Cholesterol %in% outliers),]
boxplot(dataset$Cholesterol)
boxplot.stats(dataset$Cholesterol)$out ###after
#####################################################

#HDL

boxplot.stats(dataset$HDL)$out   ##before
minVal <- boxplot.stats(dataset$HDL)$stats[1]
maxVal <- boxplot.stats(dataset$HDL)$stats [5]
myValue <- mean(dataset$HDL)
dataset [dataset$HDL < minVal | dataset$HDL > maxVal, "HDL"] <- myValue
boxplot(dataset$HDL)
boxplot.stats(dataset$HDL)$out 

outliers <- boxplot(dataset$HDL, plot=FALSE)$out
dataset <- dataset[-which(dataset$HDL %in% outliers),]
boxplot(dataset$HDL)
boxplot.stats(dataset$HDL)$out ###after
######################################

#LDL

boxplot.stats(dataset$LDL)$out   ##before
minVal <- boxplot.stats(dataset$LDL)$stats[1]
maxVal <- boxplot.stats(dataset$LDL)$stats [5]
myValue <- mean(dataset$LDL)
dataset [dataset$LDL < minVal | dataset$LDL > maxVal, "LDL"] <- myValue
boxplot(dataset$LDL)
boxplot.stats(dataset$LDL)$out 

outliers <- boxplot(dataset$LDL, plot=FALSE)$out
dataset <- dataset[-which(dataset$LDL %in% outliers),]
boxplot(dataset$LDL)
boxplot.stats(dataset$LDL)$out ###after
##############################################

#hemoglobin

boxplot.stats(dataset$hemoglobin)$out   ##before
minVal <- boxplot.stats(dataset$hemoglobin)$stats[1]
maxVal <- boxplot.stats(dataset$hemoglobin)$stats [5]
myValue <- mean(dataset$hemoglobin)
dataset [dataset$hemoglobin < minVal | dataset$hemoglobin > maxVal, "hemoglobin"] <- myValue
boxplot(dataset$hemoglobin)
boxplot.stats(dataset$hemoglobin)$out 

outliers <- boxplot(dataset$hemoglobin, plot=FALSE)$out
dataset <- dataset[-which(dataset$hemoglobin %in% outliers),]
boxplot(dataset$hemoglobin)
boxplot.stats(dataset$hemoglobin)$out ###after
#####################################

#Urine.protein

boxplot.stats(dataset$Urine.protein)$out   ##before
minVal <- boxplot.stats(dataset$Urine.protein)$stats[1]
maxVal <- boxplot.stats(dataset$Urine.protein)$stats [5]
myValue <- mean(dataset$Urine.protein)
dataset [dataset$Urine.protein < minVal | dataset$Urine.protein > maxVal, "Urine.protein"] <- myValue
boxplot(dataset$Urine.protein)
boxplot.stats(dataset$Urine.protein)$out 

outliers <- boxplot(dataset$Urine.protein, plot=FALSE)$out
dataset <- dataset[-which(dataset$Urine.protein %in% outliers),]
boxplot(dataset$Urine.protein)
boxplot.stats(dataset$Urine.protein)$out ###after
##########################################

#serum.creatinine

boxplot.stats(dataset$serum.creatinine)$out   ##before
minVal <- boxplot.stats(dataset$serum.creatinine)$stats[1]
maxVal <- boxplot.stats(dataset$serum.creatinine)$stats [5]
myValue <- mean(dataset$serum.creatinine)
dataset [dataset$serum.creatinine < minVal | dataset$serum.creatinine > maxVal, "serum.creatinine"] <- myValue
boxplot(dataset$serum.creatinine)
boxplot.stats(dataset$serum.creatinine)$out 
###################################################

#AST

boxplot.stats(dataset$AST)$out   ##before
minVal <- boxplot.stats(dataset$AST)$stats[1]
maxVal <- boxplot.stats(dataset$AST)$stats [5]
myValue <- mean(dataset$AST)
dataset [dataset$AST < minVal | dataset$AST > maxVal, "AST"] <- myValue
boxplot(dataset$AST)
boxplot.stats(dataset$AST)$out

outliers <- boxplot(dataset$AST, plot=FALSE)$out
dataset <- dataset[-which(dataset$AST %in% outliers),]
boxplot(dataset$AST)
boxplot.stats(dataset$AST)$out ###after
########################################################

#ALT

boxplot.stats(dataset$ALT)$out   ##before
minVal <- boxplot.stats(dataset$ALT)$stats[1]
maxVal <- boxplot.stats(dataset$ALT)$stats [5]
myValue <- mean(dataset$ALT)
dataset [dataset$ALT < minVal | dataset$ALT > maxVal, "ALT"] <- myValue
boxplot(dataset$ALT)
boxplot.stats(dataset$ALT)$out

outliers <- boxplot(dataset$ALT, plot=FALSE)$out
dataset <- dataset[-which(dataset$ALT %in% outliers),]
boxplot(dataset$ALT)
boxplot.stats(dataset$ALT)$out ###after
###########################################

#Gtp

boxplot.stats(dataset$Gtp)$out   ##before
minVal <- boxplot.stats(dataset$Gtp)$stats[1]
maxVal <- boxplot.stats(dataset$Gtp)$stats [5]
myValue <- mean(dataset$Gtp)
dataset [dataset$Gtp < minVal | dataset$Gtp > maxVal, "ALT"] <- myValue
boxplot(dataset$Gtp)
boxplot.stats(dataset$Gtp)$out

outliers <- boxplot(dataset$Gtp, plot=FALSE)$out
dataset <- dataset[-which(dataset$Gtp %in% outliers),]
boxplot(dataset$Gtp)
boxplot.stats(dataset$Gtp)$out
###############################

#box plot after removeing outliers 

boxplot.stats(dataset$age)$out
boxplot.stats(dataset$height.cm.)$out
boxplot.stats(dataset$weight.kg.)$out
boxplot.stats(dataset$waist.cm.)$out
boxplot.stats(dataset$systolic)$out
boxplot.stats(dataset$relaxation)$out
boxplot.stats(dataset$fasting.blood.sugar)$out
boxplot.stats(dataset$Cholesterol)$out
boxplot.stats(dataset$triglyceride)$out
boxplot.stats(dataset$HDL)$out
boxplot.stats(dataset$LDL)$out
boxplot.stats(dataset$hemoglobin)$out
boxplot.stats(dataset$Urine.protein)$out
boxplot.stats(dataset$serum.creatinine)$out
boxplot.stats(dataset$AST)$out
boxplot.stats(dataset$ALT)$out
boxplot.stats(dataset$Gtp)$out

#Correlation analysis 

cor(dataset$weight.kg.,dataset$hemoglobin)
cor(dataset$weight.kg.,dataset$Gtp)
cor(dataset$weight.kg.,dataset$Cholesterol)
cor(dataset$weight.kg.,dataset$HDL)
cor(dataset$weight.kg.,dataset$AST)
cor(dataset$weight.kg.,dataset$Urine.protein)

#Discretization
install.packages("arules")
library(arules)
x <- dataset[,2]
table(arules::discretize(x, breaks = 3))

#finish Data preprocessing 

View(dataset)
sum(is.null(dataset))

# instal pakages for classification

install.packages("party")
install.packages("e1071")
install.packages("caret")
install.packages("magrittr")
install.packages("AppliedPredictiveModeling")
install.packages("ggplot2")
install.packages("lattice")
library(party)
library(e1071)
library(ggplot2)
library(lattice)
library(caret)
library(magrittr)
library(AppliedPredictiveModeling)




#Import required library
##classification:
sum(is.null(dataset))

##1-Split data (70% - 30%) #MyFourmula and tables:
set.seed(1234)
ind <- sample(2,nrow(dataset),replace=TRUE,prob = c(0.7,0.3)) 
trainData <-dataset[ind==1,]
testData <-dataset[ind==2,]
myFormula <- smoking ~ gender	+age+	height.cm.	+weight.kg.+	waist.cm.	+eyesight.left.+	eyesight.right.+	hearing.left.+	hearing.right.+	systolic	+relaxation+	fasting.blood.sugar+	Cholesterol+	triglyceride+	HDL+	LDL+	hemoglobin+	Urine.protein+serum.creatinine+	AST+	ALT	+Gtp+	oral+	dental.caries+	tartar
dataset_ctree <-ctree(myFormula, data=trainData) 
table(predict(dataset_ctree), trainData$smoking) 
#Trees: 
print(dataset_ctree) 
plot(dataset_ctree)
plot(dataset_ctree, type ="simple" ) 
#Test: 
testPred <- predict(dataset_ctree, newdata=testData) 
##Evalute Model: 
table(testPred, testData$smoking)
dim(testPred)
x = as.factor(testData$smoking)
dim(x)
#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(testData$smoking, testPred)[1]

#create confusion matrix
confusionMatrix(testData$smoking, testPred)


coMa <-confusionMatrix(testPred,x)
 acc <-coMa$overallU["Accuracy"]*100 


 
 
 
 
 




