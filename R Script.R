
#Import csv - dataset comes from https://www.kaggle.com/datasets/alexteboul/heart-disease-health-indicators-dataset/download?datasetVersionNumber=3
#Import csv from my github directory how please see the above link which is where I downloaded the file from.
urlfile="https://raw.githubusercontent.com/suhayl123/EdxCapstone/main/heart_disease_health_indicators_BRFSS2015.csv"
hdhi_df<- read.csv(urlfile)

#Check if we have the required libraries, install them if required and then load them.


if (!require('dplyr')) install.packages('dplyr')
if (!require('Hmisc')) install.packages('Hmisc')
if (!require('caret')) install.packages('caret')
if (!require('scales')) install.packages('scales')
if (!require('ggstats')) install.packages('ggstats')
if (!require('ggplot2')) install.packages('ggplot2')
if (!require('fastDummies')) install.packages('fastDummies')
if (!require('cowplot')) install.packages('cowplot')


library(dplyr)
library(Hmisc)
library(caret)
library(scales)
library(ggstats)
library(ggplot2)
library('fastDummies')
library(cowplot)

#To remove scientific notation for number values. 
options(scipen=999)

#Check whether there are any missing values in our dataset i.e. there are none.
hdhi_df %>%summarise_all(funs(sum(is.na(.))))

# To find out the proportion of records where heart disease exits. 9.419% of records has been flagged as having heart disease i.e. there is an imbalance so when it comes to the modelling stage I will create another training dataset that will take all records where heart diesease or attack is present and will randomly under sample records where there is no heart disease to have a more balanced dataset.
summary(hdhi_df$HeartDiseaseorAttack)

#Create histograms of all variables.
hist1 <-hist(hdhi_df$HeartDiseaseorAttack,breaks=n_distinct(hdhi_df$HeartDiseaseorAttack),main ="Histogram of HeartDiseaseorAttack",labels = paste0(round(hist(hdhi_df$HeartDiseaseorAttack, plot = FALSE,breaks = n_distinct(hdhi_df$HeartDiseaseorAttack))$counts / length(hdhi_df$HeartDiseaseorAttack) * 100, 1),"%"))
hist2 <-hist(hdhi_df$HighBP,breaks=n_distinct(hdhi_df$HighBP),main ="Histogram of HighBP",labels = paste0(round(hist(hdhi_df$HighBP, plot = FALSE,breaks = n_distinct(hdhi_df$HighBP))$counts / length(hdhi_df$HighBP) * 100, 1),"%"))
hist3 <-hist(hdhi_df$HighChol,breaks=n_distinct(hdhi_df$HighChol),main ="Histogram of HighChol",labels = paste0(round(hist(hdhi_df$HighChol, plot = FALSE,breaks = n_distinct(hdhi_df$HighChol))$counts / length(hdhi_df$HighChol) * 100, 1),"%"))
hist4 <-hist(hdhi_df$CholCheck,breaks=n_distinct(hdhi_df$CholCheck),main ="Histogram of CholCheck",labels = paste0(round(hist(hdhi_df$CholCheck, plot = FALSE,breaks = n_distinct(hdhi_df$CholCheck))$counts / length(hdhi_df$CholCheck) * 100, 1),"%"))
hist5 <-hist(hdhi_df$Smoker,breaks=n_distinct(hdhi_df$Smoker),main ="Histogram of Smoker",labels = paste0(round(hist(hdhi_df$Smoker, plot = FALSE,breaks = n_distinct(hdhi_df$Smoker))$counts / length(hdhi_df$Smoker) * 100, 1),"%"))
hist6 <-hist(hdhi_df$Stroke,breaks=n_distinct(hdhi_df$Stroke),main ="Histogram of Stroke",labels = paste0(round(hist(hdhi_df$Stroke, plot = FALSE,breaks = n_distinct(hdhi_df$Stroke))$counts / length(hdhi_df$Stroke) * 100, 1),"%"))
hist7 <-hist(hdhi_df$Diabetes,breaks=n_distinct(hdhi_df$Diabetes),main ="Histogram of Diabetes",labels = paste0(round(hist(hdhi_df$Diabetes, plot = FALSE,breaks = n_distinct(hdhi_df$Diabetes))$counts / length(hdhi_df$Diabetes) * 100, 1),"%"))
hist8 <-hist(hdhi_df$PhysActivity,breaks=n_distinct(hdhi_df$PhysActivity),main ="Histogram of PhysActivity",labels = paste0(round(hist(hdhi_df$PhysActivity, plot = FALSE,breaks = n_distinct(hdhi_df$PhysActivity))$counts / length(hdhi_df$PhysActivity) * 100, 1),"%"))
hist9 <-hist(hdhi_df$Fruits,breaks=n_distinct(hdhi_df$Fruits),main ="Histogram of Fruits",labels = paste0(round(hist(hdhi_df$Fruits, plot = FALSE,breaks = n_distinct(hdhi_df$Fruits))$counts / length(hdhi_df$Fruits) * 100, 1),"%"))
hist10 <-hist(hdhi_df$Veggies,breaks=n_distinct(hdhi_df$Veggies),main ="Histogram of Veggies",labels = paste0(round(hist(hdhi_df$Veggies, plot = FALSE,breaks = n_distinct(hdhi_df$Veggies))$counts / length(hdhi_df$Veggies) * 100, 1),"%"))
hist11 <-hist(hdhi_df$HvyAlcoholConsump,breaks=n_distinct(hdhi_df$HvyAlcoholConsump),main ="Histogram of HvyAlcoholConsump",labels = paste0(round(hist(hdhi_df$HvyAlcoholConsump, plot = FALSE,breaks = n_distinct(hdhi_df$HvyAlcoholConsump))$counts / length(hdhi_df$HvyAlcoholConsump) * 100, 1),"%"))
hist12 <-hist(hdhi_df$AnyHealthcare,breaks=n_distinct(hdhi_df$AnyHealthcare),main ="Histogram of AnyHealthcare",labels = paste0(round(hist(hdhi_df$AnyHealthcare, plot = FALSE,breaks = n_distinct(hdhi_df$AnyHealthcare))$counts / length(hdhi_df$AnyHealthcare) * 100, 1),"%"))
hist13 <-hist(hdhi_df$NoDocbcCost,breaks=n_distinct(hdhi_df$NoDocbcCost),main ="Histogram of NoDocbcCost",labels = paste0(round(hist(hdhi_df$NoDocbcCost, plot = FALSE,breaks = n_distinct(hdhi_df$NoDocbcCost))$counts / length(hdhi_df$NoDocbcCost) * 100, 1),"%"))
hist14 <-hist(hdhi_df$GenHlth,breaks=n_distinct(hdhi_df$GenHlth),main ="Histogram of GenHlth",labels = paste0(round(hist(hdhi_df$GenHlth, plot = FALSE,breaks = n_distinct(hdhi_df$GenHlth))$counts / length(hdhi_df$GenHlth) * 100, 1),"%"))
hist15 <-hist(hdhi_df$DiffWalk,breaks=n_distinct(hdhi_df$DiffWalk),main ="Histogram of DiffWalk",labels = paste0(round(hist(hdhi_df$DiffWalk, plot = FALSE,breaks = n_distinct(hdhi_df$DiffWalk))$counts / length(hdhi_df$DiffWalk) * 100, 1),"%"))
hist16 <-hist(hdhi_df$Sex,breaks=n_distinct(hdhi_df$Sex),main ="Histogram of Sex",labels = paste0(round(hist(hdhi_df$Sex, plot = FALSE,breaks = n_distinct(hdhi_df$Sex))$counts / length(hdhi_df$Sex) * 100, 1),"%"))
hist17 <-hist(hdhi_df$BMI,breaks=n_distinct(hdhi_df$BMI),main ="Histogram of BMI",labels = paste0(round(hist(hdhi_df$BMI, plot = FALSE,breaks = n_distinct(hdhi_df$BMI))$counts / length(hdhi_df$BMI) * 100, 1),"%"))
hist18 <-hist(hdhi_df$MentHlth,breaks=n_distinct(hdhi_df$MentHlth),main ="Histogram of MentHlth",labels = paste0(round(hist(hdhi_df$MentHlth, plot = FALSE,breaks = n_distinct(hdhi_df$MentHlth))$counts / length(hdhi_df$MentHlth) * 100, 1),"%"))
hist19 <-hist(hdhi_df$PhysHlth,breaks=n_distinct(hdhi_df$PhysHlth),main ="Histogram of PhysHlth",labels = paste0(round(hist(hdhi_df$PhysHlth, plot = FALSE,breaks = n_distinct(hdhi_df$PhysHlth))$counts / length(hdhi_df$PhysHlth) * 100, 1),"%"))
hist20 <-hist(hdhi_df$Age,breaks=n_distinct(hdhi_df$Age),main ="Histogram of Age",labels = paste0(round(hist(hdhi_df$Age, plot = FALSE,breaks = n_distinct(hdhi_df$Age))$counts / length(hdhi_df$Age) * 100, 1),"%"))
hist21 <-hist(hdhi_df$Income,breaks=n_distinct(hdhi_df$Income),main ="Histogram of Income",labels = paste0(round(hist(hdhi_df$Income, plot = FALSE,breaks = n_distinct(hdhi_df$Income))$counts / length(hdhi_df$Income) * 100, 1),"%"))
hist22 <-hist(hdhi_df$Education,breaks=n_distinct(hdhi_df$Education),main ="Histogram of Education",labels = paste0(round(hist(hdhi_df$Education, plot = FALSE,breaks = n_distinct(hdhi_df$Education))$counts / length(hdhi_df$Education) * 100, 1),"%"))

#plot histograms:
par(mfrow = c(3, 3))
plot(hist1)
plot(hist2)
plot(hist3)
plot(hist4)
plot(hist5)
plot(hist6)
plot(hist7)
plot(hist8)
plot(hist9)
par(mfrow = c(3, 3))
plot(hist10)
plot(hist11)
plot(hist12)
plot(hist13)
plot(hist14)
plot(hist15)
plot(hist16)
par(mfrow = c(2, 3))
plot(hist17)
plot(hist18)
plot(hist19)
plot(hist20)
plot(hist21)
plot(hist22)

#Create factor level for HeartDiseaseorAttack.
hdhi_df <- hdhi_df %>% mutate(HDA = case_when(HeartDiseaseorAttack == 0  ~ "NoHDA", HeartDiseaseorAttack == 1  ~ "YesHDA"))
hdhi_df$HDA <- factor(hdhi_df$HDA ,levels =  c("NoHDA","YesHDA"))

#At this current time I will create 3 datasets which are:
#Train - A Training dataset that will be used for model building, this will represent 70% of the entire data
#Test - A test dataset that will be used to assess the perfomance of any model built using the training dataset where I expect to have simillar results in the training dataset, this will represent 15% of the entire data.
#Holdout - A dataset that will not be used for exploratory analysis or model building. This dataset will be used in the final stage to determine how well my final model performed on data that has not been used, this will represent 15% of the entire data
#Create holdout sample.
set.seed(1) 
hold_index <- createDataPartition(y = hdhi_df$HeartDiseaseorAttack, times = 1, p = 0.15, list = FALSE)
Holdout <- hdhi_df[hold_index,]
#Create combined train and test dataset.
TrainTest <- hdhi_df[-hold_index,]
#randomly split the training and test data.
test_index <- createDataPartition(y = TrainTest$HeartDiseaseorAttack, times = 1, p = 0.15/0.85, list = FALSE)
Train <- hdhi_df[-test_index,]
Test <- hdhi_df[test_index,]
#delete indexes and the combined training and test data.
rm( test_index,hold_index,TrainTest)

#Exploratory analysis on training and test data - from the charts it can be see that that someone who has HighBP is more likely to have Heart Disease or Attack.
#HeartDiseaseorAttack by HighBP plots - Train & Test. 
TrainPlot1HighBP<-ggplot(Train) + aes(x = HighBP, fill = factor(HeartDiseaseorAttack), by = factor(HighBP)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By HighBP Group', x = 'HighBP', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2HighBP<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = HighBP, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And HighBP', x = 'HighBP', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1HighBP<-ggplot(Test) + aes(x = HighBP, fill = factor(HeartDiseaseorAttack), by = factor(HighBP)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By HighBP Group', x = 'HighBP', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2HighBP<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = HighBP, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And HighBP', x = 'HighBP', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by HighChol plots - Train & Test - from the charts it can be see that that someone who has HighChol is more likely to have Heart Disease or Attack.
TrainPlot1HighChol<-ggplot(Train) + aes(x = HighChol, fill = factor(HeartDiseaseorAttack), by = factor(HighChol)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By HighChol Group', x = 'HighChol', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2HighChol<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = HighChol, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And HighChol', x = 'HighChol', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1HighChol<-ggplot(Test) + aes(x = HighChol, fill = factor(HeartDiseaseorAttack), by = factor(HighChol)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By HighChol Group', x = 'HighChol', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2HighChol<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = HighChol, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And HighChol', x = 'HighChol', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by CholCheck plots - Train & Test - from the charts it can be see that that someone who has CholCheck is more likely to have Heart Disease or Attack however 96% of records have CholCheck with the majority of observations having heart disease or attack falling in this group i.e. likely to not be a significant predictor.
TrainPlot1CholCheck<-ggplot(Train) + aes(x = CholCheck, fill = factor(HeartDiseaseorAttack), by = factor(CholCheck)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By CholCheck Group', x = 'CholCheck', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2CholCheck<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = CholCheck, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And CholCheck', x = 'CholCheck', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1CholCheck<-ggplot(Test) + aes(x = CholCheck, fill = factor(HeartDiseaseorAttack), by = factor(CholCheck)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By CholCheck Group', x = 'CholCheck', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2CholCheck<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = CholCheck, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And CholCheck', x = 'CholCheck', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by Smoker plots - Train & Test data. From the charts it can be see that that someone who has smoked is more likely to have Heart Disease or Attack.
TrainPlot1Smoker<-ggplot(Train) + aes(x = Smoker, fill = factor(HeartDiseaseorAttack), by = factor(Smoker)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By Smoker Group', x = 'Smoker', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2Smoker<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Smoker, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Smoker', x = 'Smoker', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1Smoker<-ggplot(Test) + aes(x = Smoker, fill = factor(HeartDiseaseorAttack), by = factor(Smoker)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By Smoker Group', x = 'Smoker', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2Smoker<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Smoker, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Smoker', x = 'Smoker', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by Stroke plots - Train & Test data. From the charts it can be see that that someone who has had a Stroke is significantly likely to have Heart Disease or Attack however this only represent 4% of the data but seems likely that this will be a significant predictor.
TrainPlot1Stroke<-ggplot(Train) + aes(x = Stroke, fill = factor(HeartDiseaseorAttack), by = factor(Stroke)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By Stroke Group', x = 'Stroke', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2Stroke<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Stroke, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Stroke', x = 'Stroke', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1Stroke<-ggplot(Test) + aes(x = Stroke, fill = factor(HeartDiseaseorAttack), by = factor(Stroke)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By Stroke Group', x = 'Stroke', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2Stroke<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Stroke, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Stroke', x = 'Stroke', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by Diabetes plots - Train & TestFrom the charts it can be see that that someone who has diabetes is more likely to have Heart Disease or Attack.
TrainPlot1Diabetes<-ggplot(Train) + aes(x = Diabetes, fill = factor(HeartDiseaseorAttack), by = factor(Diabetes)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By Diabetes Group', x = 'Diabetes', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2Diabetes<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Diabetes, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Diabetes', x = 'Diabetes', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1Diabetes<-ggplot(Test) + aes(x = Diabetes, fill = factor(HeartDiseaseorAttack), by = factor(Diabetes)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By Diabetes Group', x = 'Diabetes', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2Diabetes<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Diabetes, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Diabetes', x = 'Diabetes', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by PhysActivity plots - Train & Test data. From the charts it can be see that that someone who does Physical Activity on a regular basis is less likely to have Heart Disease or Attack.
TrainPlot1PhysActivity<-ggplot(Train) + aes(x = PhysActivity, fill = factor(HeartDiseaseorAttack), by = factor(PhysActivity)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By PhysActivity Group', x = 'PhysActivity', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2PhysActivity<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = PhysActivity, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And PhysActivity', x = 'PhysActivity', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1PhysActivity<-ggplot(Test) + aes(x = PhysActivity, fill = factor(HeartDiseaseorAttack), by = factor(PhysActivity)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By PhysActivity Group', x = 'PhysActivity', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2PhysActivity<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = PhysActivity, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And PhysActivity', x = 'PhysActivity', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by Fruits plots - Train & Test data. From the charts it can be see that that someone who fruit on a daily basis is slightly less likely to have Heart Disease or Attack.
TrainPlot1Fruits<-ggplot(Train) + aes(x = Fruits, fill = factor(HeartDiseaseorAttack), by = factor(Fruits)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By Fruits Group', x = 'Fruits', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2Fruits<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Fruits, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Fruits', x = 'Fruits', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1Fruits<-ggplot(Test) + aes(x = Fruits, fill = factor(HeartDiseaseorAttack), by = factor(Fruits)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By Fruits Group', x = 'Fruits', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2Fruits<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Fruits, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Fruits', x = 'Fruits', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by Veggies plots - Train & Test data. From the charts it can be see that that someone who fruit on a daily basis is less likely to have Heart Disease or Attack.
TrainPlot1Veggies<-ggplot(Train) + aes(x = Veggies, fill = factor(HeartDiseaseorAttack), by = factor(Veggies)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By Veggies Group', x = 'Veggies', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2Veggies<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Veggies, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Veggies', x = 'Veggies', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1Veggies<-ggplot(Test) + aes(x = Veggies, fill = factor(HeartDiseaseorAttack), by = factor(Veggies)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By Veggies Group', x = 'Veggies', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2Veggies<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Veggies, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Veggies', x = 'Veggies', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by HvyAlcoholConsump plots - Train & Test data. From the charts it can be see that that someone who is a heavy alcohol consumer basis is less likely to have Heart Disease or Attack however only 6% of data is classed as heavy consumer of alcohol.
TrainPlot1HvyAlcoholConsump<-ggplot(Train) + aes(x = HvyAlcoholConsump, fill = factor(HeartDiseaseorAttack), by = factor(HvyAlcoholConsump)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By HvyAlcoholConsump Group', x = 'HvyAlcoholConsump', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2HvyAlcoholConsump<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = HvyAlcoholConsump, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And HvyAlcoholConsump', x = 'HvyAlcoholConsump', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1HvyAlcoholConsump<-ggplot(Test) + aes(x = HvyAlcoholConsump, fill = factor(HeartDiseaseorAttack), by = factor(HvyAlcoholConsump)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By HvyAlcoholConsump Group', x = 'HvyAlcoholConsump', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2HvyAlcoholConsump<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = HvyAlcoholConsump, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And HvyAlcoholConsump', x = 'HvyAlcoholConsump', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by AnyHealthcare plots- Train & Test data. From the charts it can be see that that someone who has health care is more likely to have Heart Disease or Attack however 95 % of of records have health care.
TrainPlot1AnyHealthcare<-ggplot(Train) + aes(x = AnyHealthcare, fill = factor(HeartDiseaseorAttack), by = factor(AnyHealthcare)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By AnyHealthcare Group', x = 'AnyHealthcare', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2AnyHealthcare<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = AnyHealthcare, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And AnyHealthcare', x = 'AnyHealthcare', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1AnyHealthcare<-ggplot(Test) + aes(x = AnyHealthcare, fill = factor(HeartDiseaseorAttack), by = factor(AnyHealthcare)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By AnyHealthcare Group', x = 'AnyHealthcare', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2AnyHealthcare<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = AnyHealthcare, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And AnyHealthcare', x = 'AnyHealthcare', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by NoDocbcCost plots - Train & Test data. From the charts it can be see that that someone who could not see a doctor in the last 12 months due to cost is more likely to have Heart Disease or Attack however only 6% of data is classed as heavy consumer of alcohol.
TrainPlot1NoDocbcCost<-ggplot(Train) + aes(x = NoDocbcCost, fill = factor(HeartDiseaseorAttack), by = factor(NoDocbcCost)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By NoDocbcCost Group', x = 'NoDocbcCost', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2NoDocbcCost<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = NoDocbcCost, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And NoDocbcCost', x = 'NoDocbcCost', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1NoDocbcCost<-ggplot(Test) + aes(x = NoDocbcCost, fill = factor(HeartDiseaseorAttack), by = factor(NoDocbcCost)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By NoDocbcCost Group', x = 'NoDocbcCost', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2NoDocbcCost<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = NoDocbcCost, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And NoDocbcCost', x = 'NoDocbcCost', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by GenHlth plots - Train & Test data. From the charts it can be see that that someone who describes them self as having better health is less likely to have Heart Disease or Attack.
TrainPlot1GenHlth<-ggplot(Train) + aes(x = GenHlth, fill = factor(HeartDiseaseorAttack), by = factor(GenHlth)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By GenHlth Group', x = 'GenHlth', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2GenHlth<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = GenHlth, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And GenHlth', x = 'GenHlth', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1GenHlth<-ggplot(Test) + aes(x = GenHlth, fill = factor(HeartDiseaseorAttack), by = factor(GenHlth)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By GenHlth Group', x = 'GenHlth', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2GenHlth<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = GenHlth, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And GenHlth', x = 'GenHlth', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by DiffWalk plots - Train & Test data. From the charts it can be see that that someone who has difficulty walking or climbing stairs is more likely to have Heart Disease or Attack.
TrainPlot1DiffWalk<-ggplot(Train) + aes(x = DiffWalk, fill = factor(HeartDiseaseorAttack), by = factor(DiffWalk)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By DiffWalk Group', x = 'DiffWalk', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2DiffWalk<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = DiffWalk, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And DiffWalk', x = 'DiffWalk', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1DiffWalk<-ggplot(Test) + aes(x = DiffWalk, fill = factor(HeartDiseaseorAttack), by = factor(DiffWalk)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By DiffWalk Group', x = 'DiffWalk', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2DiffWalk<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = DiffWalk, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And DiffWalk', x = 'DiffWalk', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by Sex plots - Train & Test data. From the charts it can be see that that males are more likely to have Heart Disease or Attack.
TrainPlot1Sex<-ggplot(Train) + aes(x = Sex, fill = factor(HeartDiseaseorAttack), by = factor(Sex)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By Sex Group', x = 'Sex', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2Sex<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Sex, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Sex', x = 'Sex', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1Sex<-ggplot(Test) + aes(x = Sex, fill = factor(HeartDiseaseorAttack), by = factor(Sex)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By Sex Group', x = 'Sex', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2Sex<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Sex, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Sex', x = 'Sex', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by BMI plots - Train & Test data. From the charts it we can see that in general the higher a person BMI is the more likely they are to have Heart Disease or Attack howerver this not this does not seem true at the lower end of the BMI scale there I will create a new categorical variable called BMI2 that categorises BMI index into groups which is driven by the NHS (UK).
TrainPlot1BMI<-ggplot(Train) + aes(x = BMI, fill = factor(HeartDiseaseorAttack), by = factor(BMI)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By BMI Group', x = 'BMI', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2BMI<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = BMI, fill = factor(HeartDiseaseorAttack), by = 1)  + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And BMI', x = 'BMI', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1BMI<-ggplot(Test) + aes(x = BMI, fill = factor(HeartDiseaseorAttack), by = factor(BMI)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By BMI Group', x = 'BMI', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2BMI<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = BMI, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And BMI', x = 'BMI', y = 'n obs') + theme(text = element_text(size=7))


#Create BMI2 variable.
Train <- Train %>%
  mutate(BMI2 = case_when(BMI < 18.5  ~ "Underweight",
                          BMI >= 18.5 & BMI <= 24.9  ~ "Healthy", 
                          BMI >= 25 & BMI <= 29.9  ~ "Overweight", 
                          BMI >= 30 & BMI <= 39.9  ~ "Obese", 
                          BMI >= 40   ~ "SeverelyObese", 
                          TRUE ~ "Other"))

Test <- Test %>%
  mutate(BMI2 = case_when(BMI < 18.5  ~ "Underweight",
                          BMI >= 18.5 & BMI <= 24.9  ~ "Healthy", 
                          BMI >= 25 & BMI <= 29.9  ~ "Overweight", 
                          BMI >= 30 & BMI <= 39.9  ~ "Obese", 
                          BMI >= 40   ~ "SeverelyObese", 
                          TRUE ~ "Other"))
#create levels for BMI2 categories
Train$BMI2 <- factor(Train$BMI2,levels = c("Underweight", "Healthy", "Overweight", "Obese", "SeverelyObese"))
Test$BMI2 <- factor(Test$BMI2,levels = c("Underweight", "Healthy", "Overweight", "Obese", "SeverelyObese"))

#HeartDiseaseorAttack by BMI2 plots - Train & Test data. From the charts it can be seen clearly that the healthy category is the least likely to have Heart Disease or Attack and that all other categories are more likely. When it comes to modelling I will use the BMI2 variable over BMI.
TrainPlot1BMI2<-ggplot(Train) + aes(x = BMI2, fill = factor(HeartDiseaseorAttack), by = factor(BMI2)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By BMI2 Group', x = 'BMI2', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2BMI2<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = BMI2, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And BMI2', x = 'BMI2', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1BMI2<-ggplot(Test) + aes(x = BMI2, fill = factor(HeartDiseaseorAttack), by = factor(BMI2)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By BMI2 Group', x = 'BMI2', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2BMI2<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = BMI2, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And BMI2', x = 'BMI2', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by MentHlth plots - Train & Test data. The higher the value is (except 0) the more likely you are to have a Heart Disease or Attack however there is there are few observation between 1 and 30 therefore I will create a new variable called MentHlth2 that bins response between 1 and 30 based on visual inspection.
TrainPlot1MentHlth<-ggplot(Train) + aes(x = MentHlth, fill = factor(HeartDiseaseorAttack), by = factor(MentHlth)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By MentHlth Group', x = 'MentHlth', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2MentHlth<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = MentHlth, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And MentHlth', x = 'MentHlth', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1MentHlth<-ggplot(Test) + aes(x = MentHlth, fill = factor(HeartDiseaseorAttack), by = factor(MentHlth)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By MentHlth Group', x = 'MentHlth', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2MentHlth<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = MentHlth, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And MentHlth', x = 'MentHlth', y = 'n obs') + theme(text = element_text(size=7))



#MentHlth2 based on prior plots I've created a new variable:
Train <- Train %>%
  mutate(MentHlth2 = case_when(MentHlth == 0  ~ "0",
                               MentHlth > 0 & MentHlth <= 9  ~ "1to9", 
                               MentHlth > 9 & MentHlth <= 19  ~ "10to19", 
                               MentHlth > 19  ~ "20AndOver", 
                               TRUE ~ "Other"))
Test <- Test %>%
  mutate(MentHlth2 = case_when(MentHlth == 0  ~ "0",
                               MentHlth > 0 & MentHlth <= 9  ~ "1to9", 
                               MentHlth > 9 & MentHlth <= 19  ~ "10to19", 
                               MentHlth > 19  ~ "20AndOver", 
                               TRUE ~ "Other"))
#create levels for MentHlth2 
Train$MentHlth2 <- factor(Train$MentHlth2,levels = c("0", "1to9", "10to19", "20AndOver"))
Test$MentHlth2 <- factor(Test$MentHlth2,levels = c("0", "1to9", "10to19", "20AndOver"))

#HeartDiseaseorAttack by MentHlth2 plots - Train & Test data. From the charts it can be seen clearly that the 1 to 9 category is the least likely to have Heart Disease or Attack and that all other categories are more likely. When it comes to modelling I will use the MentHlth2 variable over MentHlth.
TrainPlot1MentHlth2<-ggplot(Train) + aes(x = MentHlth2, fill = factor(HeartDiseaseorAttack), by = factor(MentHlth2)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Train - Proportion With HDA By MentHlth2 Group', x = 'MentHlth2', y = 'Percent')  + theme(text = element_text(size=7))
TrainPlot2MentHlth2<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = MentHlth2, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And MentHlth2', x = 'MentHlth2', y = 'n obs')  + theme(text = element_text(size=7))
TestPlot1MentHlth2<-ggplot(Test) + aes(x = MentHlth2, fill = factor(HeartDiseaseorAttack), by = factor(MentHlth2)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Test - Proportion With HDA By MentHlth2 Group', x = 'MentHlth2', y = 'Percent')   + theme(text = element_text(size=7))
TestPlot2MentHlth2<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = MentHlth2, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And MentHlth2', x = 'MentHlth2', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by PhysHlth plots - Train & Test data. We have a similar trend as MentHlth variable except the trend is more prominent i.e. I will create a new categorical variable called PhysHlth2.
TrainPlot1PhysHlth<-ggplot(Train) + aes(x = PhysHlth, fill = factor(HeartDiseaseorAttack), by = factor(PhysHlth)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By PhysHlth Group', x = 'PhysHlth', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2PhysHlth<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = PhysHlth, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And PhysHlth', x = 'PhysHlth', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1PhysHlth<-ggplot(Test) + aes(x = PhysHlth, fill = factor(HeartDiseaseorAttack), by = factor(PhysHlth)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By PhysHlth Group', x = 'PhysHlth', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2PhysHlth<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = PhysHlth, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And PhysHlth', x = 'PhysHlth', y = 'n obs') + theme(text = element_text(size=7))


#based on plots I've created a new variable:
Train <- Train %>%
  mutate(PhysHlth2 = case_when(PhysHlth >= 0 & PhysHlth <= 2  ~ "0to2",
                               PhysHlth > 2 & PhysHlth <= 9  ~ "3to9", 
                               PhysHlth > 9 & PhysHlth <= 24  ~ "10to24", 
                               PhysHlth > 24 ~ "25AndOver", 
                               TRUE ~ "Other"))


Test <- Test %>%
  mutate(PhysHlth2 = case_when(PhysHlth >= 0 & PhysHlth <= 2  ~ "0to2",
                               PhysHlth > 2 & PhysHlth <= 9  ~ "3to9", 
                               PhysHlth > 9 & PhysHlth <= 24  ~ "10to24", 
                               PhysHlth > 24 ~ "25AndOver", 
                               TRUE ~ "Other"))
#create levels for PhysHlth2
Train$PhysHlth2 <- factor(Train$PhysHlth2,levels = c("0to2", "3to9", "10to24", "25AndOver"))
Test$PhysHlth2 <- factor(Test$PhysHlth2,levels = c("0to2", "3to9", "10to24", "25AndOver"))

#HeartDiseaseorAttack by PhysHlth2 plots - Train & Test data. From the charts it can be seen clearly that the lower category the observation is the least likely to have Heart Disease or Attack. When it comes to modelling I will use the PhysHlth2 variable over PhysHlth.
TrainPlot1PhysHlth2<-ggplot(Train) + aes(x = PhysHlth2, fill = factor(HeartDiseaseorAttack), by = factor(PhysHlth2)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Train - Proportion With HDA By PhysHlth2 Group', x = 'PhysHlth2', y = 'Percent')  + theme(text = element_text(size=7))
TrainPlot2PhysHlth2<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = PhysHlth2, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And PhysHlth2', x = 'PhysHlth2', y = 'n obs')  + theme(text = element_text(size=7))
TestPlot1PhysHlth2<-ggplot(Test) + aes(x = PhysHlth2, fill = factor(HeartDiseaseorAttack), by = factor(PhysHlth2)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Test - Proportion With HDA By PhysHlth2 Group', x = 'PhysHlth2', y = 'Percent')   + theme(text = element_text(size=7))
TestPlot2PhysHlth2<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = PhysHlth2, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And PhysHlth2', x = 'PhysHlth2', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by Age plots - Train & Test data. From the charts it can be see that the older a person is the more likely they are to have Heart Disease or Attack.
TrainPlot1Age<-ggplot(Train) + aes(x = Age, fill = factor(HeartDiseaseorAttack), by = factor(Age)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By Age Group', x = 'Age', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2Age<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Age, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Age', x = 'Age', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1Age<-ggplot(Test) + aes(x = Age, fill = factor(HeartDiseaseorAttack), by = factor(Age)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By Age Group', x = 'Age', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2Age<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Age, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Age', x = 'Age', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by Income plots - Train & Test data. From the charts it can be see that the more income a person has the less likely they are to have Heart Disease or Attack.
TrainPlot1Income<-ggplot(Train) + aes(x = Income, fill = factor(HeartDiseaseorAttack), by = factor(Income)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By Income Group', x = 'Income', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2Income<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Income, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Income', x = 'Income', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1Income<-ggplot(Test) + aes(x = Income, fill = factor(HeartDiseaseorAttack), by = factor(Income)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By Income Group', x = 'Income', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2Income<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Income, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Income', x = 'Income', y = 'n obs') + theme(text = element_text(size=7))

#HeartDiseaseorAttack by Education plots - Train & Test data. From the charts it can be see that the higher the persons education is the less likely they are to have Heart Disease or Attack.
TrainPlot1Education<-ggplot(Train) + aes(x = Education, fill = factor(HeartDiseaseorAttack), by = factor(Education)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Proportion With HDA By Education Group', x = 'Education', y = 'Percent') + theme(text = element_text(size=7))
TrainPlot2Education<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Education, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Education', x = 'Education', y = 'n obs') + theme(text = element_text(size=7))
TestPlot1Education<-ggplot(Test) + aes(x = Education, fill = factor(HeartDiseaseorAttack), by = factor(Education)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Proportion With HDA By Education Group', x = 'Education', y = 'Percent') + theme(text = element_text(size=7))
TestPlot2Education<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot() + aes(x = Education, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Education', x = 'Education', y = 'n obs') + theme(text = element_text(size=7))


#I have decided to create additional variables that combines the outcomes from variables. The first combines HighBP, HighChol, Stroke & Diabetes (1 and 2 will be classed together) to see if we get a different response with relation to Heart Disease or Attack. I feel it's logical to combine these variables as it will measure how severe the persons health is.
#Var 1
Train <- Train %>%
  mutate(Var1 = case_when(HighBP == 1 & HighChol ==1 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "All",
                          HighBP == 1 & HighChol ==0 & Stroke == 0 & Diabetes ==0 ~ "HighBP", 
                          HighBP == 0 & HighChol ==1 & Stroke == 0 & Diabetes ==0 ~ "HighChol", 
                          HighBP == 0 & HighChol ==0 & Stroke == 1 & Diabetes ==0 ~ "Stroke", 
                          HighBP == 0 & HighChol ==0 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "Diabetes", 
                          HighBP == 1 & HighChol ==1 & Stroke == 0 & Diabetes ==0 ~ "BP_Cho", 
                          HighBP == 1 & HighChol ==0 & Stroke == 1 & Diabetes ==0 ~ "BP_Str", 
                          HighBP == 1 & HighChol ==0 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "BP_Dia", 
                          HighBP == 0 & HighChol ==1 & Stroke == 1 & Diabetes ==0 ~ "Cho_Str", 
                          HighBP == 0 & HighChol ==1 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "Cho_Dia", 
                          HighBP == 0 & HighChol ==0 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "Str_Dia", 
                          HighBP == 1 & HighChol ==1 & Stroke == 1 & Diabetes ==0 ~ "BP_Cho_Str", 
                          HighBP == 1 & HighChol ==1 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "BP_Cho_Dia", 
                          HighBP == 1 & HighChol ==0 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "BP_Str_Dia", 
                          HighBP == 0 & HighChol ==1 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "Cho_Str_Dia", 
                          HighBP == 0 & HighChol ==0 & Stroke == 0 & Diabetes ==0 ~ "None", 
                          TRUE ~ "Other"))


Test <- Test %>%
  mutate(Var1 = case_when(HighBP == 1 & HighChol ==1 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "All",
                          HighBP == 1 & HighChol ==0 & Stroke == 0 & Diabetes ==0 ~ "HighBP", 
                          HighBP == 0 & HighChol ==1 & Stroke == 0 & Diabetes ==0 ~ "HighChol", 
                          HighBP == 0 & HighChol ==0 & Stroke == 1 & Diabetes ==0 ~ "Stroke", 
                          HighBP == 0 & HighChol ==0 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "Diabetes", 
                          HighBP == 1 & HighChol ==1 & Stroke == 0 & Diabetes ==0 ~ "BP_Cho", 
                          HighBP == 1 & HighChol ==0 & Stroke == 1 & Diabetes ==0 ~ "BP_Str", 
                          HighBP == 1 & HighChol ==0 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "BP_Dia", 
                          HighBP == 0 & HighChol ==1 & Stroke == 1 & Diabetes ==0 ~ "Cho_Str", 
                          HighBP == 0 & HighChol ==1 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "Cho_Dia", 
                          HighBP == 0 & HighChol ==0 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "Str_Dia", 
                          HighBP == 1 & HighChol ==1 & Stroke == 1 & Diabetes ==0 ~ "BP_Cho_Str", 
                          HighBP == 1 & HighChol ==1 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "BP_Cho_Dia", 
                          HighBP == 1 & HighChol ==0 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "BP_Str_Dia", 
                          HighBP == 0 & HighChol ==1 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "Cho_Str_Dia", 
                          HighBP == 0 & HighChol ==0 & Stroke == 0 & Diabetes ==0 ~ "None", 
                          TRUE ~ "Other"))

#create levels for Var1
Train$Var1 <- factor(Train$Var1,levels = c("All","BP_Cho_Str","BP_Cho_Dia","BP_Str_Dia","Cho_Str_Dia","Str_Dia","BP_Cho","BP_Str","BP_Dia","Cho_Str","Cho_Dia","Diabetes","HighChol","HighBP","Stroke","None"))
Test$Var1 <- factor(Test$Var1,levels = c("All","BP_Cho_Str","BP_Cho_Dia","BP_Str_Dia","Cho_Str_Dia","Str_Dia","BP_Cho","BP_Str","BP_Dia","Cho_Str","Cho_Dia","Diabetes","HighChol","HighBP","Stroke","None"))

#HeartDiseaseorAttack by Var1 plots - Train & Test data. From the charts it can be see that the more heath conditions a person has the more likely they are to have Heart Disease or Attack except it the person has had a stroke therefore I will create another variable that counts the number of conditions but factors in strokes.
TrainPlot1Var1<-ggplot(Train) + aes(x = Var1, fill = factor(HeartDiseaseorAttack), by = factor(Var1)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Train - Proportion With HDA By Var1 Group', x = 'Var1', y = 'Percent')  + theme(text = element_text(size=7))
TrainPlot2Var1<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = Var1, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Var1', x = 'Var1', y = 'n obs')  + theme(text = element_text(size=7))
TestPlot1Var1<-ggplot(Test) + aes(x = Var1, fill = factor(HeartDiseaseorAttack), by = factor(Var1)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Test - Proportion With HDA By Var1 Group', x = 'Var1', y = 'Percent')   + theme(text = element_text(size=7))
TestPlot2Var1<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = Var1, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Var1', x = 'Var1', y = 'n obs') + theme(text = element_text(size=7))


#I have created Var2 based on my findings from Var1.
#Var 2
Train <- Train %>%
  mutate(Var2 = case_when(HighBP == 1 & HighChol ==1 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "All4",
                          HighBP == 1 & HighChol ==0 & Stroke == 0 & Diabetes ==0 ~ "HighBP", 
                          HighBP == 0 & HighChol ==1 & Stroke == 0 & Diabetes ==0 ~ "HighChol", 
                          HighBP == 0 & HighChol ==0 & Stroke == 1 & Diabetes ==0 ~ "Stroke", 
                          HighBP == 0 & HighChol ==0 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "Diabetes", 
                          HighBP == 1 & HighChol ==1 & Stroke == 0 & Diabetes ==0 ~ "2ExcStroke", 
                          HighBP == 1 & HighChol ==0 & Stroke == 1 & Diabetes ==0 ~ "2IncStroke", 
                          HighBP == 1 & HighChol ==0 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "2ExcStroke", 
                          HighBP == 0 & HighChol ==1 & Stroke == 1 & Diabetes ==0 ~ "2IncStroke", 
                          HighBP == 0 & HighChol ==1 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "2ExcStroke", 
                          HighBP == 0 & HighChol ==0 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "2IncStroke", 
                          HighBP == 1 & HighChol ==1 & Stroke == 1 & Diabetes ==0 ~ "3IncStroke", 
                          HighBP == 1 & HighChol ==1 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "3ExcStroke", 
                          HighBP == 1 & HighChol ==0 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "3IncStroke", 
                          HighBP == 0 & HighChol ==1 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "3IncStroke", 
                          HighBP == 0 & HighChol ==0 & Stroke == 0 & Diabetes ==0 ~ "None", 
                          TRUE ~ "Other"))


Test <- Test %>%
  mutate(Var2 = case_when(HighBP == 1 & HighChol ==1 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "All4",
                          HighBP == 1 & HighChol ==0 & Stroke == 0 & Diabetes ==0 ~ "HighBP", 
                          HighBP == 0 & HighChol ==1 & Stroke == 0 & Diabetes ==0 ~ "HighChol", 
                          HighBP == 0 & HighChol ==0 & Stroke == 1 & Diabetes ==0 ~ "Stroke", 
                          HighBP == 0 & HighChol ==0 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "Diabetes", 
                          HighBP == 1 & HighChol ==1 & Stroke == 0 & Diabetes ==0 ~ "2ExcStroke", 
                          HighBP == 1 & HighChol ==0 & Stroke == 1 & Diabetes ==0 ~ "2IncStroke", 
                          HighBP == 1 & HighChol ==0 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "2ExcStroke", 
                          HighBP == 0 & HighChol ==1 & Stroke == 1 & Diabetes ==0 ~ "2IncStroke", 
                          HighBP == 0 & HighChol ==1 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "2ExcStroke", 
                          HighBP == 0 & HighChol ==0 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "2IncStroke", 
                          HighBP == 1 & HighChol ==1 & Stroke == 1 & Diabetes ==0 ~ "3IncStroke", 
                          HighBP == 1 & HighChol ==1 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "3ExcStroke", 
                          HighBP == 1 & HighChol ==0 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "3IncStroke", 
                          HighBP == 0 & HighChol ==1 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "3IncStroke", 
                          HighBP == 0 & HighChol ==0 & Stroke == 0 & Diabetes ==0 ~ "None", 
                          TRUE ~ "Other"))
#create levels for Var2
Train$Var2 <- factor(Train$Var2,levels = c("All4","3IncStroke","2IncStroke","Stroke","3ExcStroke","2ExcStroke","Diabetes","HighChol","HighBP","None"))
Test$Var2 <- factor(Test$Var2,levels = c("All4","3IncStroke","2IncStroke","Stroke","3ExcStroke","2ExcStroke","Diabetes","HighChol","HighBP","None"))

#HeartDiseaseorAttack by Var2 plots - Train & Test data. From the charts it can be proven that my findings from Var1 is justified (the more health conditions has a person has the more likely they are to have Heart Disease or Attack) except if the person has had a stroke only vs 3 conditions excluding a stroke. 
TrainPlot1Var2<-ggplot(Train) + aes(x = Var2, fill = factor(HeartDiseaseorAttack), by = factor(Var2)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Train - Proportion With HDA By Var2 Group', x = 'Var2', y = 'Percent')  + theme(text = element_text(size=7))
TrainPlot2Var2<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = Var2, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Var2', x = 'Var2', y = 'n obs')  + theme(text = element_text(size=7))
TestPlot1Var2<-ggplot(Test) + aes(x = Var2, fill = factor(HeartDiseaseorAttack), by = factor(Var2)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Test - Proportion With HDA By Var2 Group', x = 'Var2', y = 'Percent')   + theme(text = element_text(size=7))
TestPlot2Var2<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = Var2, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Var2', x = 'Var2', y = 'n obs') + theme(text = element_text(size=7))




#The 3rd variable I have created combines Veggies and Fruits variables together.
#Var3
Train <- Train %>%
  mutate(Var3 = case_when(Fruits == 1 & Veggies ==1 ~ "Fru_Veg",
                          Fruits == 1 & Veggies ==0 ~ "Fruits", 
                          Fruits == 0 & Veggies ==1 ~ "Veggies", 
                          Fruits == 0 & Veggies ==0 ~ "None", 
                          TRUE ~ "Other"))

Test <- Test %>%
  mutate(Var3 = case_when(Fruits == 1 & Veggies ==1 ~ "Fru_Veg",
                          Fruits == 1 & Veggies ==0 ~ "Fruits", 
                          Fruits == 0 & Veggies ==1 ~ "Veggies", 
                          Fruits == 0 & Veggies ==0 ~ "None", 
                          TRUE ~ "Other"))
#create levels for Var3
Train$Var3 <- factor(Train$Var3,levels = c("Fru_Veg","Fruits","Veggies","None"))
Test$Var3 <- factor(Test$Var3,levels = c("Fru_Veg","Fruits","Veggies","None"))

#HeartDiseaseorAttack by Var3 plots - Train & Test data. From the charts you can see that if a person has Fruits and Veggies everyday they are the least likely to have Heart Disease or Attack then by Veggies only, I have decided to group Fruits only and None together as they have a similar response to Heart Disease or Attack.
TrainPlot1Var3<-ggplot(Train) + aes(x = Var3, fill = factor(HeartDiseaseorAttack), by = factor(Var3)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Train - Proportion With HDA By Var3 Group', x = 'Var3', y = 'Percent')  + theme(text = element_text(size=7))
TrainPlot2Var3<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = Var3, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Var3', x = 'Var3', y = 'n obs')  + theme(text = element_text(size=7))
TestPlot1Var3<-ggplot(Test) + aes(x = Var3, fill = factor(HeartDiseaseorAttack), by = factor(Var3)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Test - Proportion With HDA By Var3 Group', x = 'Var3', y = 'Percent')   + theme(text = element_text(size=7))
TestPlot2Var3<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = Var3, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Var3', x = 'Var3', y = 'n obs') + theme(text = element_text(size=7))

#The 4th variable I have created is based on my learnings from Var3.
#Var4
Train <- Train %>%
  mutate(Var4 = case_when(Fruits == 1 & Veggies ==1 ~ "Fru_Veg",
                          Fruits == 1 & Veggies ==0 ~ "NoVegies", 
                          Fruits == 0 & Veggies ==1 ~ "Veggies", 
                          Fruits == 0 & Veggies ==0 ~ "NoVegies", 
                          TRUE ~ "Other"))

Test <- Test %>%
  mutate(Var4 = case_when(Fruits == 1 & Veggies ==1 ~ "Fru_Veg",
                          Fruits == 1 & Veggies ==0 ~ "NoVegies", 
                          Fruits == 0 & Veggies ==1 ~ "Veggies", 
                          Fruits == 0 & Veggies ==0 ~ "NoVegies", 
                          TRUE ~ "Other"))
#create levels for Var4
Train$Var4 <- factor(Train$Var4,levels = c("Fru_Veg","Veggies","NoVegies"))
Test$Var4 <- factor(Test$Var4,levels =  c("Fru_Veg","Veggies","NoVegies"))

#HeartDiseaseorAttack by Var4 plots - Train & Test data. From the charts you will see that it agrees with my findings from Var3. This will be used for modelling rather than Var3, Fruits & Veggies.
TrainPlot1Var4<-ggplot(Train) + aes(x = Var4, fill = factor(HeartDiseaseorAttack), by = factor(Var4)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Train - Proportion With HDA By Var4 Group', x = 'Var4', y = 'Percent')  + theme(text = element_text(size=7))
TrainPlot2Var4<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = Var4, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Var4', x = 'Var4', y = 'n obs')  + theme(text = element_text(size=7))
TestPlot1Var4<-ggplot(Test) + aes(x = Var4, fill = factor(HeartDiseaseorAttack), by = factor(Var4)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Test - Proportion With HDA By Var4 Group', x = 'Var4', y = 'Percent')   + theme(text = element_text(size=7))
TestPlot2Var4<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = Var4, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Var4', x = 'Var4', y = 'n obs') + theme(text = element_text(size=7))


#The 5th variable I have created combines HvyAlcoholConsump and Smoker variables together.
Train <- Train %>%
  mutate(Var5 = case_when(HvyAlcoholConsump == 1 & Smoker ==1 ~ "HAC_Smo",
                          HvyAlcoholConsump == 1 & Smoker ==0 ~ "HvyAlcoholConsump", 
                          HvyAlcoholConsump == 0 & Smoker ==1 ~ "Smoker",  
                          HvyAlcoholConsump == 0 & Smoker ==0 ~ "None", 
                          TRUE ~ "Other"))

Test <- Test %>%
  mutate(Var5 = case_when(HvyAlcoholConsump == 1 & Smoker ==1 ~ "HAC_Smo",
                          HvyAlcoholConsump == 1 & Smoker ==0 ~ "HvyAlcoholConsump", 
                          HvyAlcoholConsump == 0 & Smoker ==1 ~ "Smoker",  
                          HvyAlcoholConsump == 0 & Smoker ==0 ~ "None", 
                          TRUE ~ "Other"))
#create levels for Var5
Train$Var5 <- factor(Train$Var5,levels = c("HAC_Smo","HvyAlcoholConsump","None","Smoker"))
Test$Var5 <- factor(Test$Var5,levels =  c("HAC_Smo","HvyAlcoholConsump","None","Smoker"))

#HeartDiseaseorAttack by Var5 plots - Train & Test data. From the charts I can see that only smoking produces the highest risk to Heart Disease or Attack. Suprisingly heavy alcohol use produces the lowest however this could just mean that person suffers from other diseases and doesn't get a chance to get Heart Disease or Attack.
TrainPlot1Var5<-ggplot(Train) + aes(x = Var5, fill = factor(HeartDiseaseorAttack), by = factor(Var5)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Train - Proportion With HDA By Var5 Group', x = 'Var5', y = 'Percent')  + theme(text = element_text(size=7))
TrainPlot2Var5<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = Var5, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Var5', x = 'Var5', y = 'n obs')  + theme(text = element_text(size=7))
TestPlot1Var5<-ggplot(Test) + aes(x = Var5, fill = factor(HeartDiseaseorAttack), by = factor(Var5)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Test - Proportion With HDA By Var5 Group', x = 'Var5', y = 'Percent')   + theme(text = element_text(size=7))
TestPlot2Var5<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = Var5, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Var5', x = 'Var5', y = 'n obs') + theme(text = element_text(size=7))




#The 6th variable I have created combines education groups 1 and 3 together since there small proportion of these people in these groups and seem to be more at risk
Train <- Train %>%
  mutate(Education2 = case_when(Education >=1  & Education<=3 ~ "1_3", 
                                Education ==4  ~ "4",
                                Education ==5  ~ "5",
                                Education ==6  ~ "6",
                                TRUE ~ "Other"))

Test <- Test %>%
  mutate(Education2 = case_when(Education >=1  & Education<=3 ~ "1_3", 
                                Education ==4  ~ "4",
                                Education ==5  ~ "5",
                                Education ==6  ~ "6",
                                TRUE ~ "Other"))
#create levels for Education2
Train$Education2 <- factor(Train$Education2,levels = c("1_3","4","5","6"))
Test$Education2 <- factor(Test$Education2,levels =  c("1_3","4","5","6"))

#HeartDiseaseorAttack by Education2 plots - Train & Test data. From the charts I can see that only smoking produces the highest risk to Heart Disease or Attack. Suprisingly heavy alcohol use produces the lowest however this could just mean that person suffers from other diseases and doesn't get a chance to get Heart Disease or Attack.
TrainPlot1Education2<-ggplot(Train) + aes(x = Education2, fill = factor(HeartDiseaseorAttack), by = factor(Education2)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Train - Proportion With HDA By Education2 Group', x = 'Education2', y = 'Percent')  + theme(text = element_text(size=7))
TrainPlot2Education2<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = Education2, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Education2', x = 'Education2', y = 'n obs')  + theme(text = element_text(size=7))
TestPlot1Education2<-ggplot(Test) + aes(x = Education2, fill = factor(HeartDiseaseorAttack), by = factor(Education2)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Test - Proportion With HDA By Education2 Group', x = 'Education2', y = 'Percent')   + theme(text = element_text(size=7))
TestPlot2Education2<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = Education2, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Education2', x = 'Education2', y = 'n obs') + theme(text = element_text(size=7))


#The 7th variable I have created combines Age groups 1 to 4, 5 to 6, 7, 8 to 9, 10, 11 to 12 and 13 
Train <- Train %>%
  mutate(Age2 = case_when(Age >=1  & Age<=4 ~ "1_4", 
                          Age >=5  & Age<=6 ~ "5_6", 
                          Age ==7  ~ "7", 
                          Age >=8  & Age<=9 ~ "8_9", 
                          Age ==10  ~ "10", 
                          Age >=11  & Age<=12 ~ "11_12", 
                          Age ==13 ~ "13", 
                          TRUE ~ "Other"))

Test <- Test %>%
  mutate(Age2 = case_when(Age >=1  & Age<=4 ~ "1_4", 
                          Age >=5  & Age<=6 ~ "5_6", 
                          Age ==7  ~ "7", 
                          Age >=8  & Age<=9 ~ "8_9", 
                          Age ==10  ~ "10", 
                          Age >=11  & Age<=12 ~ "11_12", 
                          Age ==13 ~ "13", 
                          TRUE ~ "Other"))
#create levels for Age2
Train$Age2 <- factor(Train$Age2 ,levels = c("1_4","5_6","7","8_9","10","11_12","13"))
Test$Age2 <- factor(Test$Age2 ,levels =  c("1_4","5_6","7","8_9","10","11_12","13"))

#HeartDiseaseorAttack by Age2 plots - Train & Test data. From the charts I can see that only smoking produces the highest risk to Heart Disease or Attack. Suprisingly heavy alcohol use produces the lowest however this could just mean that person suffers from other diseases and doesn't get a chance to get Heart Disease or Attack.
TrainPlot1Age2<-ggplot(Train) + aes(x = Age2, fill = factor(HeartDiseaseorAttack), by = factor(Age2)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Train - Proportion With HDA By Age2 Group', x = 'Age2', y = 'Percent')  + theme(text = element_text(size=7))
TrainPlot2Age2<-Train %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = Age2, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Train - Total Proportion With HDA And Age2', x = 'Age2', y = 'n obs')  + theme(text = element_text(size=7))
TestPlot1Age2<-ggplot(Test) + aes(x = Age2, fill = factor(HeartDiseaseorAttack), by = factor(Age2)) + geom_bar(position = "fill") + geom_text(stat = "prop", position = position_fill(.5))  + labs(title = 'Test - Proportion With HDA By Age2 Group', x = 'Age2', y = 'Percent')   + theme(text = element_text(size=7))
TestPlot2Age2<-Test %>% filter(HeartDiseaseorAttack==1) %>% ggplot()  + aes(x = Age2, fill = factor(HeartDiseaseorAttack), by = 1) + geom_bar() +geom_text(aes(label = scales::percent(after_stat(prop), accuracy = 1)),stat = "prop", position = position_stack(.5)) + labs(fill='HeartDiseaseorAttack',  title = 'Test - Total Proportion With HDA And Age2', x = 'Age2', y = 'n obs') + theme(text = element_text(size=7))


#convert to factors
names <- c('Diabetes' ,'GenHlth','Income')
Train[,names] <- lapply(Train[,names] , factor)
Test[,names] <- lapply(Test[,names] , factor)


#Since we have a few observations where we have heart disease attack as occurring i.e. I have imbalanced data with regards to Heart Disease or Attack. I can balance the Heart Disease or Attack outcome by either adding in random samples of observations where heart disease is present or I can remove observations where heart disease isn't present. I have decided to under sample and will keep 20% of where records that do not contain HeartDiseaseorAttack for the fact that I will need less computing power to run any modelling. Once any model has been created I will assess the model against my test data and the original training data.

TrainKeep1 <- Train %>% filter(HeartDiseaseorAttack==1)
TrainKeep2 <- Train %>% filter(HeartDiseaseorAttack==0)
#Create a random index that will keep only 20% of data held in TrainKeep2 (HeartDiseaseorAttack = 0)
Train_index <- createDataPartition(y = TrainKeep2$BMI, times = 1, p = 0.2, list = FALSE)
set.seed(1)
TrainKeep2 <- TrainKeep2[Train_index,]
#Now combine TrainKeep1 & TrainKeep2 as Train2
Train2 <- rbind(TrainKeep1, TrainKeep2)
rm(Train_index,TrainKeep1, TrainKeep2)
#you can see that I now have a 34.13% response rate in my dataset that I will be using for model building rather than 9.39% i.e. the imbalance has been reduced.
summary(Train2$HeartDiseaseorAttack)


#I'm now going to create 4 models using the caret package (Random Forest, Logistic Regression and CART). Based on the best model I will then look to improve that model.
#cv set to 3 - computer power
trainControl <- trainControl(method="cv", number=3,summaryFunction = twoClassSummary,classProbs = TRUE)


# RF
set.seed(1)
fit.rf <- train(HDA~ HighBP+ HighChol+ CholCheck+ Smoker+ Stroke+  Diabetes+ PhysActivity+ HvyAlcoholConsump+ AnyHealthcare+ NoDocbcCost+ GenHlth+ DiffWalk+ Sex+ Age2+ Education2 + Income  + Var1 + Var2 + Var3 + Var4 + Var5+ BMI2  + MentHlth2 + PhysHlth2 , data=Train2, method = "rf", metric="ROC",trControl = trainControl,importance = TRUE)
#model summary results Train
y_hat_fit.rf_Train <- predict(fit.rf,Train,type="raw")
cm.rf_Train <- confusionMatrix(y_hat_fit.rf_Train, factor(Train$HDA))

#model summary results Test
y_hat_fit.rf_Test <- predict(fit.rf,Test,type="raw")
cm.rf_Test <- confusionMatrix(y_hat_fit.rf_Test, factor(Test$HDA))
cm.rf_Train
cm.rf_Test

#sommers d 
Train.rf.pr <- as.data.frame(y_hat_fit.rf_Train)
Train.rf.pr <- Train.rf.pr  %>% mutate(y = case_when(y_hat_fit.rf_Train  == 'YesHDA' ~ 1, y_hat_fit.rf_Train  == 'NoHDA'~ 0 ))
Test.rf.pr <- as.data.frame(y_hat_fit.rf_Test)
Test.rf.pr <- Test.rf.pr  %>% mutate(y = case_when(y_hat_fit.rf_Test  == 'YesHDA'  ~ 1, y_hat_fit.rf_Test == 'NoHDA'~ 0 ))
somers2(Train.rf.pr$y,  Train$HeartDiseaseorAttack)
somers2(Test.rf.pr$y,  Test$HeartDiseaseorAttack)


# LG - Logistic Regression
set.seed(1)
fit.glm <- train(HDA~ HighBP+ HighChol+ CholCheck+ Smoker+ Stroke+  Diabetes+ PhysActivity+ HvyAlcoholConsump+ AnyHealthcare+ NoDocbcCost+ GenHlth+ DiffWalk+ Sex+ Age2+ Education2 + Income  + Var1 + Var2 + Var3 + Var4 + Var5+ BMI2  + MentHlth2 + PhysHlth2 , data=Train2 , method="glmStepAIC",family="binomial", direction="forward", metric="ROC", trControl=trainControl)

#model summary results Train
y_hat_fit.glm_Train <- predict(fit.glm,Train,type="raw")
cm.glm_Train <- confusionMatrix(y_hat_fit.glm_Train, factor(Train$HDA))

#model summary results Test
y_hat_fit.glm_Test <- predict(fit.glm,Test,type="raw")
cm.glm_Test <- confusionMatrix(y_hat_fit.glm_Test, factor(Test$HDA))
cm.glm_Train
cm.glm_Test

#sommers d 
Train.glm.pr <- as.data.frame(y_hat_fit.glm_Train)
Train.glm.pr <- Train.glm.pr  %>% mutate(y = case_when(y_hat_fit.glm_Train  == 'YesHDA' ~ 1, y_hat_fit.glm_Train  == 'NoHDA'~ 0 ))
Test.glm.pr <- as.data.frame(y_hat_fit.glm_Test)
Test.glm.pr <- Test.glm.pr  %>% mutate(y = case_when(y_hat_fit.glm_Test  == 'YesHDA' ~ 1, y_hat_fit.glm_Test  == 'NoHDA'~ 0  ))
somers2(Train.glm.pr$y,  Train$HeartDiseaseorAttack)
somers2(Test.glm.pr$y,  Test$HeartDiseaseorAttack)


# CART - Classification and Regression Trees (CART), 
set.seed(1)
fit.cart <- train(HDA~ HighBP+ HighChol+ CholCheck+ Smoker+ Stroke+  Diabetes+ PhysActivity+ HvyAlcoholConsump+ AnyHealthcare+ NoDocbcCost+ GenHlth+ DiffWalk+ Sex+ Age2+ Education2 + Income  + Var1 + Var2 + Var3 + Var4 + Var5+ BMI2  + MentHlth2 + PhysHlth2, data=Train2, method="rpart", metric="ROC",trControl=trainControl)

#model summary results Train
y_hat_fit.cart_Train <- predict(fit.cart,Train,type="raw")
cm.cart_Train <- confusionMatrix(y_hat_fit.cart_Train, factor(Train$HDA))

#model summary results Test
y_hat_fit.cart_Test <- predict(fit.cart,Test,type="raw")
cm.cart_Test <- confusionMatrix(y_hat_fit.cart_Test, factor(Test$HDA))

#show results of confusion matrix
cm.cart_Train
cm.cart_Test

#sommers d 
Train.cart.pr <- as.data.frame(y_hat_fit.cart_Train)
Train.cart.pr <- Train.cart.pr  %>% mutate(y = case_when(y_hat_fit.cart_Train  == 'YesHDA' ~ 1, y_hat_fit.cart_Train  == 'NoHDA'~ 0 ))
Test.cart.pr <- as.data.frame(y_hat_fit.cart_Test)
Test.cart.pr <- Test.cart.pr  %>% mutate(y = case_when(y_hat_fit.cart_Test  == 'YesHDA' ~ 1, y_hat_fit.cart_Test  == 'NoHDA'~ 0 ))
somers2(Train.cart.pr$y,  Train$HeartDiseaseorAttack)
somers2(Test.cart.pr$y,  Test$HeartDiseaseorAttack)


#create a dataframe to compare Nvars, Accuracy,Sensitivity (note Sensitivity and Specificity are incorrectly populated in the table), Precision & Somers D

ModelResultsSummary <- data.frame(
  Model = c("RF","GLM","CART"),  
  NVars= c(length(fit.rf$finalModel$importance[,1]),length(row(varImp(fit.glm$finalModel)[1])),length(fit.cart$finalModel$variable.importance)),
  TrainAccuracy = c(cm.rf_Train$overall["Accuracy"],cm.glm_Train$overall["Accuracy"],cm.cart_Train$overall["Accuracy"]),
  TestAccuracy = c(cm.rf_Test$overall["Accuracy"],cm.glm_Test$overall["Accuracy"],cm.cart_Test$overall["Accuracy"]),
  TrainSensitivity = c(cm.rf_Train$byClass["Specificity"],cm.glm_Train$byClass["Specificity"],cm.cart_Train$byClass["Specificity"]),
  TestSensitivity = c(cm.rf_Test$byClass["Specificity"],cm.glm_Test$byClass["Specificity"],cm.cart_Test$byClass["Specificity"]),
  TrainPrecision = c(cm.rf_Train$byClass["Neg Pred Value"],cm.glm_Train$byClass["Neg Pred Value"],cm.cart_Train$byClass["Neg Pred Value"]),
  TestPrecision = c(cm.rf_Test$byClass["Neg Pred Value"],cm.glm_Test$byClass["Neg Pred Value"],cm.cart_Test$byClass["Neg Pred Value"]),
  TrainNcountTP = c(cm.rf_Train$table[4],cm.glm_Train$table[4],cm.cart_Train$table[4]),
  TestNcountTP = c(cm.rf_Test$table[4],cm.glm_Test$table[4],cm.cart_Test$table[4]),
  TrainSomersD = c(somers2(Train.rf.pr$y,  Train$HeartDiseaseorAttack)["Dxy"],somers2(Train.glm.pr$y,  Train$HeartDiseaseorAttack)["Dxy"],somers2(Train.cart.pr$y,  Train$HeartDiseaseorAttack)["Dxy"]),
  TestSomersD = c(somers2(Test.rf.pr$y,  Test$HeartDiseaseorAttack)["Dxy"],somers2(Test.glm.pr$y,  Test$HeartDiseaseorAttack)["Dxy"],somers2(Test.cart.pr$y,  Test$HeartDiseaseorAttack)["Dxy"])
)

#I have chosen to explore glm model further
#model summary 
summary(fit.glm)

#create dummy variables & train GLM using the all the variables that are present in my first model using forward selection.
Train <- dummy_cols(Train, select_columns = c('Var1','Var2', 'Var3','Var4','Var5','Diabetes','GenHlth','Education2','Income','BMI2','Age2','MentHlth2','PhysHlth2'), remove_selected_columns = FALSE)
Train2 <- dummy_cols(Train2, select_columns = c('Var1','Var2', 'Var3','Var4','Var5','Diabetes','GenHlth','Education2','Income','BMI2','Age2','MentHlth2','PhysHlth2'), remove_selected_columns = FALSE)
Test <- dummy_cols(Test, select_columns = c('Var1','Var2', 'Var3','Var4','Var5','Diabetes','GenHlth','Education2','Income','BMI2','Age2','MentHlth2','PhysHlth2'), remove_selected_columns = FALSE)

#increase cv to 7
trainControl <- trainControl(method="cv", number=7,summaryFunction = twoClassSummary,classProbs = TRUE)

# Now I will retrain but using the dummy variables where possbile based on the variables in the 1st GLM model
fit.glm2 <- train(HDA~ Var1_All+ Var1_BP_Cho_Str+ Var1_BP_Cho_Dia+ Var1_BP_Str_Dia+ Var1_Cho_Str_Dia+ Var1_Str_Dia+ Var1_BP_Cho+ Var1_BP_Str+ Var1_BP_Dia+ Var1_Cho_Str+ Var1_Cho_Dia+  Var1_Diabetes+ 
                    Var1_HighChol+ Var1_HighBP+ Var1_Stroke+  Var1_None+ DiffWalk+ Sex+ Stroke+ Age2_1_4+ Age2_5_6+  Age2_7+  Age2_8_9+  Age2_10+  Age2_11_12+ Age2_13+ GenHlth_1+ GenHlth_2+ 
                    GenHlth_3+ GenHlth_4+ GenHlth_5+ HighChol+ Var5_HAC_Smo+  Var5_HvyAlcoholConsump+  Var5_None+  Var5_Smoker+ Income_1+Income_2+Income_3+Income_4+ Income_5+Income_6+
                    Income_7+Income_8 + HighBP + NoDocbcCost+ CholCheck + Education2_1_3 + Education2_4 + Education2_5 + Education2_6 + PhysHlth2_0to2+ PhysHlth2_3to9+ PhysHlth2_10to24+ PhysHlth2_25AndOver +  BMI2_Healthy+  BMI2_Overweight+  BMI2_Obese+ 
                    BMI2_SeverelyObese+ MentHlth2_0+ MentHlth2_1to9+ MentHlth2_10to19+ MentHlth2_20AndOver + Var3_Fru_Veg+  Var3_Fruits+ Var3_None+  Var3_Veggies+ AnyHealthcare+ PhysActivity + Diabetes_0+ Diabetes_1+ Diabetes_2 ,
                  data=Train2 , method="glmStepAIC",family="binomial", direction="forward", metric="ROC", trControl=trainControl)

#model summary results Train
y_hat_fit.glm2_Train <- predict(fit.glm2,Train,type="raw")
cm.glm2_Train <- confusionMatrix(y_hat_fit.glm2_Train, factor(Train$HDA))

#model summary results Test
y_hat_fit.glm2_Test <- predict(fit.glm2,Test,type="raw")
cm.glm2_Test <- confusionMatrix(y_hat_fit.glm2_Test, factor(Test$HDA))

#show results of confusion matrix
cm.glm2_Train
cm.glm2_Test

#sommers d 
Train.glm2.pr <- as.data.frame(y_hat_fit.glm2_Train)
Train.glm2.pr <- Train.glm2.pr  %>% mutate(y = case_when(y_hat_fit.glm2_Train  == 'YesHDA' ~ 1, y_hat_fit.glm2_Train  == 'NoHDA'~ 0 ))
Test.glm2.pr <- as.data.frame(y_hat_fit.glm2_Test)
Test.glm2.pr <- Test.glm2.pr  %>% mutate(y = case_when(y_hat_fit.glm2_Test  == 'YesHDA' ~ 1, y_hat_fit.glm2_Test  == 'NoHDA'~ 0  ))
somers2(Train.glm2.pr$y,  Train$HeartDiseaseorAttack)
somers2(Test.glm2.pr$y,  Test$HeartDiseaseorAttack)

#model summary 
summary(fit.glm2)

#similar results produce, I will now create another model using the same variables except I will now stepwiseAIC procedure to determine which variables are most important in order and will use the dummy variables as opppsed to any categorical variable.
#update model results summaries
ModelResultsSummary <- ModelResultsSummary %>% add_row(Model = "GLM2", NVars=length(row(varImp(fit.glm2$finalModel)[1])),  TrainAccuracy = cm.glm2_Train$overall["Accuracy"], TestAccuracy =cm.glm2_Test$overall["Accuracy"] ,TrainSensitivity = cm.glm2_Train$byClass["Specificity"], TestSensitivity = cm.glm2_Test$byClass["Specificity"], TrainPrecision = cm.glm2_Train$byClass["Neg Pred Value"], TestPrecision = cm.glm2_Test$byClass["Neg Pred Value"] , TrainNcountTP = cm.glm2_Train$table[4] , TestNcountTP = cm.glm2_Test$table[4],TrainSomersD = somers2(Train.glm2.pr$y,  Train$HeartDiseaseorAttack)["Dxy"], TestSomersD =somers2(Test.glm2.pr$y,  Test$HeartDiseaseorAttack)["Dxy"] )

#Run backwards with the variable selected from the forward selection
fit.glm3 <- train(HDA~ Var1_None + DiffWalk + Sex + Age2_1_4 + Stroke + Age2_5_6 + GenHlth_1 + GenHlth_2 + GenHlth_3 + Age2_13 + Age2_11_12 + Var5_Smoker + HighChol + Var1_HighChol + Age2_10 + GenHlth_4 + Age2_7 + Var1_HighBP + Income_8 + HighBP + NoDocbcCost + CholCheck + PhysHlth2_10to24 +
                    Diabetes_2 + Income_2 + Var1_Stroke + Income_1 + PhysHlth2_0to2 + Education2_5 + Income_3 + Var1_Str_Dia + Var1_BP_Str_Dia + BMI2_Healthy + Income_4 + Var3_Fruits + Var1_All + AnyHealthcare + Var1_Cho_Dia + MentHlth2_20AndOver + PhysActivity + Var5_HvyAlcoholConsump,
                  data=Train2 , method="glmStepAIC",family="binomial", direction="backward", metric="ROC", trControl=trainControl)

#model summary results Train
y_hat_fit.glm3_Train <- predict(fit.glm3,Train,type="raw")
cm.glm3_Train <- confusionMatrix(y_hat_fit.glm3_Train, factor(Train$HDA))

#model summary results Test
y_hat_fit.glm3_Test <- predict(fit.glm3,Test,type="raw")
cm.glm3_Test <- confusionMatrix(y_hat_fit.glm3_Test, factor(Test$HDA))

#show results of confusion matrix
cm.glm3_Train
cm.glm3_Test

#sommers d 
Train.glm3.pr <- as.data.frame(y_hat_fit.glm3_Train)
Train.glm3.pr <- Train.glm3.pr  %>% mutate(y = case_when(y_hat_fit.glm3_Train  == 'YesHDA' ~ 1, y_hat_fit.glm3_Train  == 'NoHDA'~ 0 ))
Test.glm3.pr <- as.data.frame(y_hat_fit.glm3_Test)
Test.glm3.pr <- Test.glm3.pr  %>% mutate(y = case_when(y_hat_fit.glm3_Test  == 'YesHDA' ~ 1, y_hat_fit.glm3_Test  == 'NoHDA'~ 0  ))
somers2(Train.glm3.pr$y,  Train$HeartDiseaseorAttack)
somers2(Test.glm3.pr$y,  Test$HeartDiseaseorAttack)

#model summary - No Change compared to prior glm model using forward selection
summary(fit.glm3)

#update model results summaries
ModelResultsSummary <- ModelResultsSummary %>% add_row(Model = "GLM3", NVars=length(row(varImp(fit.glm3$finalModel)[1])),  TrainAccuracy = cm.glm3_Train$overall["Accuracy"], TestAccuracy =cm.glm3_Test$overall["Accuracy"] ,TrainSensitivity = cm.glm3_Train$byClass["Specificity"], TestSensitivity = cm.glm3_Test$byClass["Specificity"], TrainPrecision = cm.glm3_Train$byClass["Neg Pred Value"], TestPrecision = cm.glm3_Test$byClass["Neg Pred Value"] , TrainNcountTP = cm.glm3_Train$table[4] , TestNcountTP = cm.glm3_Test$table[4],TrainSomersD = somers2(Train.glm3.pr$y,  Train$HeartDiseaseorAttack)["Dxy"], TestSomersD =somers2(Test.glm3.pr$y,  Test$HeartDiseaseorAttack)["Dxy"] )

#StepAIC model
fit.glm4 <- train(HDA~ Var1_All+ Var1_BP_Cho_Str+ Var1_BP_Cho_Dia+ Var1_BP_Str_Dia+ Var1_Cho_Str_Dia+ Var1_Str_Dia+ Var1_BP_Cho+ Var1_BP_Str+ Var1_BP_Dia+ Var1_Cho_Str+ Var1_Cho_Dia+  Var1_Diabetes+ 
                    Var1_HighChol+ Var1_HighBP+ Var1_Stroke+  Var1_None+ DiffWalk+ Sex+ Stroke+ Age2_1_4+ Age2_5_6+  Age2_7+  Age2_8_9+  Age2_10+  Age2_11_12+ Age2_13+ GenHlth_1+ GenHlth_2+ 
                    GenHlth_3+ GenHlth_4+ GenHlth_5+ HighChol+ Var5_HAC_Smo+  Var5_HvyAlcoholConsump+  Var5_None+  Var5_Smoker+ Income_1+Income_2+Income_3+Income_4+ Income_5+Income_6+
                    Income_7+Income_8 + HighBP + NoDocbcCost+ CholCheck + Education2_1_3 + Education2_4 + Education2_5 + Education2_6 + PhysHlth2_0to2+ PhysHlth2_3to9+ PhysHlth2_10to24+ PhysHlth2_25AndOver +  BMI2_Healthy+  BMI2_Overweight+  BMI2_Obese+ 
                    BMI2_SeverelyObese+ MentHlth2_0+ MentHlth2_1to9+ MentHlth2_10to19+ MentHlth2_20AndOver + Var3_Fru_Veg+  Var3_Fruits+ Var3_None+  Var3_Veggies+ AnyHealthcare+ PhysActivity + Diabetes_0+ Diabetes_1+ Diabetes_2 ,
                  data=Train2 , method="glmStepAIC",family="binomial", direction="both", metric="ROC", trControl=trainControl)

#model summary results Train
y_hat_fit.glm4_Train <- predict(fit.glm4,Train,type="raw")
cm.glm4_Train <- confusionMatrix(y_hat_fit.glm4_Train, factor(Train$HDA))

#model summary results Test
y_hat_fit.glm4_Test <- predict(fit.glm4,Test,type="raw")
cm.glm4_Test <- confusionMatrix(y_hat_fit.glm4_Test, factor(Test$HDA))

#show results of confusion matrix
cm.glm4_Train
cm.glm4_Test

#sommers d 
Train.glm4.pr <- as.data.frame(y_hat_fit.glm4_Train)
Train.glm4.pr <- Train.glm4.pr  %>% mutate(y = case_when(y_hat_fit.glm4_Train  == 'YesHDA' ~ 1, y_hat_fit.glm4_Train  == 'NoHDA'~ 0 ))
Test.glm4.pr <- as.data.frame(y_hat_fit.glm4_Test)
Test.glm4.pr <- Test.glm4.pr  %>% mutate(y = case_when(y_hat_fit.glm4_Test  == 'YesHDA' ~ 1, y_hat_fit.glm4_Test  == 'NoHDA'~ 0  ))
somers2(Train.glm4.pr$y,  Train$HeartDiseaseorAttack)
somers2(Test.glm4.pr$y,  Test$HeartDiseaseorAttack)
summary(fit.glm4)

#update model results summaries
ModelResultsSummary <- ModelResultsSummary %>% add_row(Model = "GLM4", NVars=length(row(varImp(fit.glm4$finalModel)[1])),  TrainAccuracy = cm.glm4_Train$overall["Accuracy"], TestAccuracy =cm.glm4_Test$overall["Accuracy"] ,TrainSensitivity = cm.glm4_Train$byClass["Specificity"], TestSensitivity = cm.glm4_Test$byClass["Specificity"], TrainPrecision = cm.glm4_Train$byClass["Neg Pred Value"], TestPrecision = cm.glm4_Test$byClass["Neg Pred Value"] , TrainNcountTP = cm.glm4_Train$table[4] , TestNcountTP = cm.glm4_Test$table[4],TrainSomersD = somers2(Train.glm4.pr$y,  Train$HeartDiseaseorAttack)["Dxy"], TestSomersD =somers2(Test.glm4.pr$y,  Test$HeartDiseaseorAttack)["Dxy"] )

#model summary 
summary(fit.glm4$finalModel)

#The forward and backward models produced the best results however there are insigniifcant variables i.e. I will create a 5th model removing insignificant variables from model glm3.fit
fit.glm5 <- train(HDA~ Var1_None + DiffWalk + Sex + Age2_1_4 + Stroke + Age2_5_6 + GenHlth_1 + GenHlth_2 + GenHlth_3 + Age2_13 + Age2_11_12 + Var5_Smoker + HighChol + Var1_HighChol + Age2_10 + GenHlth_4 + Age2_7 + Var1_HighBP + Income_8 + HighBP + NoDocbcCost + CholCheck + PhysHlth2_10to24 + Diabetes_2 + Income_2 + Var1_Stroke + Income_1 + PhysHlth2_0to2 + Education2_5 + Income_3 + Var1_Str_Dia + Var1_BP_Str_Dia + BMI2_Healthy + Income_4 + Var3_Fruits + Var1_All ,
                  data=Train2 , method="glm",family="binomial", metric="ROC", trControl=trainControl)

#model summary results Train
y_hat_fit.glm5_Train <- predict(fit.glm5,Train,type="raw")
cm.glm5_Train <- confusionMatrix(y_hat_fit.glm5_Train, factor(Train$HDA))

#model summary results Test
y_hat_fit.glm5_Test <- predict(fit.glm5,Test,type="raw")
cm.glm5_Test <- confusionMatrix(y_hat_fit.glm5_Test, factor(Test$HDA))

#show results of confusion matrix
cm.glm5_Train
cm.glm5_Test

#sommers d 
Train.glm5.pr <- as.data.frame(y_hat_fit.glm5_Train)
Train.glm5.pr <- Train.glm5.pr  %>% mutate(y = case_when(y_hat_fit.glm5_Train  == 'YesHDA' ~ 1, y_hat_fit.glm5_Train  == 'NoHDA'~ 0 ))
Test.glm5.pr <- as.data.frame(y_hat_fit.glm5_Test)
Test.glm5.pr <- Test.glm5.pr  %>% mutate(y = case_when(y_hat_fit.glm5_Test  == 'YesHDA' ~ 1, y_hat_fit.glm5_Test  == 'NoHDA'~ 0  ))
somers2(Train.glm5.pr$y,  Train$HeartDiseaseorAttack)
somers2(Test.glm5.pr$y,  Test$HeartDiseaseorAttack)

#model summary 
summary(fit.glm5)

#update model results summaries
ModelResultsSummary <- ModelResultsSummary %>% add_row(Model = "GLM5", NVars=length(row(varImp(fit.glm5$finalModel)[1])),  TrainAccuracy = cm.glm5_Train$overall["Accuracy"], TestAccuracy =cm.glm5_Test$overall["Accuracy"] ,TrainSensitivity = cm.glm5_Train$byClass["Specificity"], TestSensitivity = cm.glm5_Test$byClass["Specificity"], TrainPrecision = cm.glm5_Train$byClass["Neg Pred Value"], TestPrecision = cm.glm5_Test$byClass["Neg Pred Value"] , TrainNcountTP = cm.glm5_Train$table[4] , TestNcountTP = cm.glm5_Test$table[4],TrainSomersD = somers2(Train.glm5.pr$y,  Train$HeartDiseaseorAttack)["Dxy"], TestSomersD =somers2(Test.glm5.pr$y,  Test$HeartDiseaseorAttack)["Dxy"] )
summary(fit.glm5$finalModel)

# Create a 6th model removing insignificant variables from model glm4.fit
#DO the same for Stepwise (model 4) 
fit.glm6 <- train(HDA~ Var1_All + Var1_BP_Cho_Str + Var1_BP_Cho_Dia + Var1_BP_Str_Dia + Var1_Cho_Str_Dia + Var1_Str_Dia + Var1_BP_Cho + Var1_BP_Str + Var1_BP_Dia + Var1_Cho_Str + Var1_Cho_Dia + Var1_Diabetes + Var1_HighChol + Var1_HighBP + Var1_Stroke + DiffWalk + Sex + Age2_1_4 + Age2_5_6 + Age2_7 + Age2_8_9 + Age2_10 + Age2_11_12 + GenHlth_1 + GenHlth_2 + GenHlth_3 + GenHlth_4 + Var5_HAC_Smo + Var5_HvyAlcoholConsump + Var5_None + Income_1 + Income_2 + Income_3 + Income_4 + Income_5 + Income_6 + Income_7 + NoDocbcCost + CholCheck + Education2_5 + PhysHlth2_0to2 + PhysHlth2_10to24 + BMI2_Healthy + Var3_Fruits + Diabetes_1,
                  data=Train2 , method="glm",family="binomial", metric="ROC", trControl=trainControl)

#model summary results Train
y_hat_fit.glm6_Train <- predict(fit.glm6,Train,type="raw")
cm.glm6_Train <- confusionMatrix(y_hat_fit.glm6_Train, factor(Train$HDA))

#model summary results Test
y_hat_fit.glm6_Test <- predict(fit.glm6,Test,type="raw")
cm.glm6_Test <- confusionMatrix(y_hat_fit.glm6_Test, factor(Test$HDA))

#show results of confusion matrix
cm.glm6_Train
cm.glm6_Test

#sommers d 
Train.glm6.pr <- as.data.frame(y_hat_fit.glm6_Train)
Train.glm6.pr <- Train.glm6.pr  %>% mutate(y = case_when(y_hat_fit.glm6_Train  == 'YesHDA' ~ 1, y_hat_fit.glm6_Train  == 'NoHDA'~ 0 ))
Test.glm6.pr <- as.data.frame(y_hat_fit.glm6_Test)
Test.glm6.pr <- Test.glm6.pr  %>% mutate(y = case_when(y_hat_fit.glm6_Test  == 'YesHDA' ~ 1, y_hat_fit.glm6_Test  == 'NoHDA'~ 0  ))
somers2(Train.glm6.pr$y,  Train$HeartDiseaseorAttack)
somers2(Test.glm6.pr$y,  Test$HeartDiseaseorAttack)

#model summary 
summary(fit.glm6)

#Update model results summary 
ModelResultsSummary <- ModelResultsSummary %>% add_row(Model = "GLM6", NVars=length(row(varImp(fit.glm6$finalModel)[1])),  TrainAccuracy = cm.glm6_Train$overall["Accuracy"], TestAccuracy =cm.glm6_Test$overall["Accuracy"] ,TrainSensitivity = cm.glm6_Train$byClass["Specificity"], TestSensitivity = cm.glm6_Test$byClass["Specificity"], TrainPrecision = cm.glm6_Train$byClass["Neg Pred Value"], TestPrecision = cm.glm6_Test$byClass["Neg Pred Value"] , TrainNcountTP = cm.glm6_Train$table[4] , TestNcountTP = cm.glm6_Test$table[4],TrainSomersD = somers2(Train.glm6.pr$y,  Train$HeartDiseaseorAttack)["Dxy"], TestSomersD =somers2(Test.glm6.pr$y,  Test$HeartDiseaseorAttack)["Dxy"] )

#I have decided that my final model will be the 5th GLM model.
summary(fit.glm5$finalModel)

#I will now discover what is my best probability score should be to determine the highest somers d statistic in my train and test data.
#Train
fit.glmF_P_Train <- predict(fit.glm5,Train,type="prob")
names(fit.glmF_P_Train)[1] <- "PscoreHDA0"
names(fit.glmF_P_Train)[2] <- "PscoreHDA1"
fit.glmF_P_Train<- data.frame(Train$HeartDiseaseorAttack, fit.glmF_P_Train$PscoreHDA1)
names(fit.glmF_P_Train)[1] <- "HeartDiseaseorAttack"
names(fit.glmF_P_Train)[2] <- "PscoreHDA1"
#Test
fit.glmF_P_Test <- predict(fit.glm5,Test,type="prob")
names(fit.glmF_P_Test)[1] <- "PscoreHDA0"
names(fit.glmF_P_Test)[2] <- "PscoreHDA1"
fit.glmF_P_Test<- data.frame(Test$HeartDiseaseorAttack, fit.glmF_P_Test$PscoreHDA1)
names(fit.glmF_P_Test)[1] <- "HeartDiseaseorAttack"
names(fit.glmF_P_Test)[2] <- "PscoreHDA1"

#Find best cutoff using Train
alphas <- seq(0.00, 1, 0.01)
cutoffsTrain <- sapply(alphas, function(alpha){
  fit.glmF_P_Train <- fit.glmF_P_Train %>% mutate(PredRes = case_when (PscoreHDA1 < alpha ~ 0, PscoreHDA1 >= alpha ~ 1)) 
  somers2(fit.glmF_P_Train$PredRes,  Train$HeartDiseaseorAttack)
})
cutoffsTrain <- as.data.frame(t(cutoffsTrain[,-1]))

#Same for Test
cutoffsTest <- sapply(alphas, function(alpha){
  fit.glmF_P_Test <- fit.glmF_P_Test %>% mutate(PredRes = case_when (PscoreHDA1 < alpha ~ 0, PscoreHDA1 >= alpha ~ 1)) 
  somers2(fit.glmF_P_Test$PredRes,  Test$HeartDiseaseorAttack)
})
cutoffsTest <- as.data.frame(t(cutoffsTest[,-1]))

# My cutoff values for my training and test are very similar
par(mfrow = c(1, 2))
plot(cutoffsTrain$Dxy)
plot(cutoffsTest$Dxy)
cutoffsTrain[which.max(cutoffsTrain$Dxy),]
cutoffsTest[which.max(cutoffsTest$Dxy),]

#I've decided to take the average best cut off values from my train & test result.
cutoffvalue <- (0.30 + 0.33) / 2 

# assess how well my cut off value performs using my new cut off value 0.31
y_hat_p_fit.glm5_Train <- predict(fit.glm5,Train,type="prob")
names(y_hat_p_fit.glm5_Train)[1] <- "PscoreHDA0GLM5"
names(y_hat_p_fit.glm5_Train)[2] <- "PscoreHDA1GLM5"
#Test
y_hat_p_fit.glm5_Test <- predict(fit.glm5,Test,type="prob")
names(y_hat_p_fit.glm5_Test)[1] <- "PscoreHDA0GLM5"
names(y_hat_p_fit.glm5_Test)[2] <- "PscoreHDA1GLM5"

#Train Summary
Train_Results <- Train[1]
Train_Results <- bind_cols(Train_Results, y_hat_p_fit.glm5_Train)
#based on my highest cuttoff value in my training 
Train_Results <- Train_Results %>% mutate(ResultHDA1GLM5 = case_when (PscoreHDA1GLM5 < cutoffvalue~ 0, PscoreHDA1GLM5 >= cutoffvalue~ 1))
#For graphical view
Train_Results <- Train_Results %>% mutate(PredCatGLM5 = case_when (PscoreHDA1GLM5 <= 0.1 ~ 1, PscoreHDA1GLM5 <= 0.2 ~ 2, PscoreHDA1GLM5 <= 0.3 ~ 3, PscoreHDA1GLM5 <= 0.4 ~ 4, PscoreHDA1GLM5 <= 0.5 ~ 5, PscoreHDA1GLM5 <= 0.6 ~ 6, PscoreHDA1GLM5 <= 0.7 ~ 7, PscoreHDA1GLM5 <= 0.8 ~ 8, PscoreHDA1GLM5 <= 0.9 ~ 9, PscoreHDA1GLM5 > 0.9 ~ 10))
GLM5SummaryResultsTrain<-Train_Results %>%group_by(PredCatGLM5) %>%summarise(Freq = n(),PredProb = mean(PscoreHDA1GLM5),Target = mean(HeartDiseaseorAttack))

#Test Summary
Test_Results <- Test[1]
Test_Results <- bind_cols(Test_Results, y_hat_p_fit.glm5_Test)
#based on my highest cuttoff value in my training 
Test_Results <- Test_Results %>% mutate(ResultHDA1GLM5 = case_when (PscoreHDA1GLM5 < cutoffvalue ~ 0, PscoreHDA1GLM5 >= cutoffvalue~ 1))
#For graphical view
Test_Results <- Test_Results %>% mutate(PredCatGLM5 = case_when (PscoreHDA1GLM5 <= 0.1 ~ 1, PscoreHDA1GLM5 <= 0.2 ~ 2, PscoreHDA1GLM5 <= 0.3 ~ 3, PscoreHDA1GLM5 <= 0.4 ~ 4, PscoreHDA1GLM5 <= 0.5 ~ 5, PscoreHDA1GLM5 <= 0.6 ~ 6, PscoreHDA1GLM5 <= 0.7 ~ 7, PscoreHDA1GLM5 <= 0.8 ~ 8, PscoreHDA1GLM5 <= 0.9 ~ 9, PscoreHDA1GLM5 > 0.9 ~ 10))
GLM5SummaryResultsTest<-Test_Results %>%group_by(PredCatGLM5) %>%summarise(Freq = n(),PredProb = mean(PscoreHDA1GLM5),Target = mean(HeartDiseaseorAttack))

#based on my cutoff value here is my confusion matrix 

#model summary results Train
cm.glm5_TrainOptimised <- confusionMatrix(factor(Train_Results$ResultHDA1GLM5), factor(Train_Results$HeartDiseaseorAttack))
#model summary results Test
cm.glm5_TestOptimised <- confusionMatrix(factor(Test_Results$ResultHDA1GLM5), factor(Test_Results$HeartDiseaseorAttack))
cm.glm5_TrainOptimised
cm.glm5_TestOptimised
#sommers d 
somers2(Train_Results$ResultHDA1GLM5,  Train$HeartDiseaseorAttack)
somers2(Test_Results$ResultHDA1GLM5,  Test$HeartDiseaseorAttack)

#Update model summary results, since I'm using HeartDiseaseorAttack in my confusion matrix rather than HDA variable Sensitivity and Precision are not marked the wrong way round.
ModelResultsSummary <- ModelResultsSummary %>% add_row(Model = "GLM5 Optimised", NVars=length(row(varImp(fit.glm5$finalModel)[1])),  TrainAccuracy = cm.glm5_TrainOptimised$overall["Accuracy"], TestAccuracy =cm.glm5_TestOptimised$overall["Accuracy"] ,TrainSensitivity = cm.glm5_TrainOptimised$byClass["Specificity"], TestSensitivity = cm.glm5_TestOptimised$byClass["Specificity"], TrainPrecision = cm.glm5_TrainOptimised$byClass["Neg Pred Value"], TestPrecision = cm.glm5_TestOptimised$byClass["Neg Pred Value"] , TrainNcountTP = cm.glm5_TrainOptimised$table[4] , TestNcountTP = cm.glm5_TestOptimised$table[4],TrainSomersD = somers2(Train_Results$ResultHDA1GLM5,  Train$HeartDiseaseorAttack)["Dxy"], TestSomersD =somers2(Test_Results$ResultHDA1GLM5,  Test$HeartDiseaseorAttack)["Dxy"] )

#Since I have decided to go with with my 5th GLM model that is optimised with regards to Somers D, I will now see how the model performs on the holdout data.
#I will now discover what is my best probability score should be to determine the highest somers d statistic in my train and test data.

#Train
fit.glmF_P_Train <- predict(fit.glm5,Train,type="prob")
names(fit.glmF_P_Train)[1] <- "PscoreHDA0"
names(fit.glmF_P_Train)[2] <- "PscoreHDA1"
fit.glmF_P_Train<- data.frame(Train$HeartDiseaseorAttack, fit.glmF_P_Train$PscoreHDA1)
names(fit.glmF_P_Train)[1] <- "HeartDiseaseorAttack"
names(fit.glmF_P_Train)[2] <- "PscoreHDA1"
#Test
fit.glmF_P_Test <- predict(fit.glm5,Test,type="prob")
names(fit.glmF_P_Test)[1] <- "PscoreHDA0"
names(fit.glmF_P_Test)[2] <- "PscoreHDA1"
fit.glmF_P_Test<- data.frame(Test$HeartDiseaseorAttack, fit.glmF_P_Test$PscoreHDA1)
names(fit.glmF_P_Test)[1] <- "HeartDiseaseorAttack"
names(fit.glmF_P_Test)[2] <- "PscoreHDA1"


#Prep my holdout data by adding all the variables I created in the Train and Test datasets
#Create BMI2 variable.
Holdout <- Holdout %>%
  mutate(BMI2 = case_when(BMI < 18.5  ~ "Underweight",
                          BMI >= 18.5 & BMI <= 24.9  ~ "Healthy", 
                          BMI >= 25 & BMI <= 29.9  ~ "Overweight", 
                          BMI >= 30 & BMI <= 39.9  ~ "Obese", 
                          BMI >= 40   ~ "SeverelyObese", 
                          TRUE ~ "Other"),
         MentHlth2 = case_when(MentHlth == 0  ~ "0",
                               MentHlth > 0 & MentHlth <= 9  ~ "1to9", 
                               MentHlth > 9 & MentHlth <= 19  ~ "10to19", 
                               MentHlth > 19  ~ "20AndOver", 
                               TRUE ~ "Other"),
         Var1 = case_when(HighBP == 1 & HighChol ==1 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "All",
                          HighBP == 1 & HighChol ==0 & Stroke == 0 & Diabetes ==0 ~ "HighBP", 
                          HighBP == 0 & HighChol ==1 & Stroke == 0 & Diabetes ==0 ~ "HighChol", 
                          HighBP == 0 & HighChol ==0 & Stroke == 1 & Diabetes ==0 ~ "Stroke", 
                          HighBP == 0 & HighChol ==0 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "Diabetes", 
                          HighBP == 1 & HighChol ==1 & Stroke == 0 & Diabetes ==0 ~ "BP_Cho", 
                          HighBP == 1 & HighChol ==0 & Stroke == 1 & Diabetes ==0 ~ "BP_Str", 
                          HighBP == 1 & HighChol ==0 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "BP_Dia", 
                          HighBP == 0 & HighChol ==1 & Stroke == 1 & Diabetes ==0 ~ "Cho_Str", 
                          HighBP == 0 & HighChol ==1 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "Cho_Dia", 
                          HighBP == 0 & HighChol ==0 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "Str_Dia", 
                          HighBP == 1 & HighChol ==1 & Stroke == 1 & Diabetes ==0 ~ "BP_Cho_Str", 
                          HighBP == 1 & HighChol ==1 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "BP_Cho_Dia", 
                          HighBP == 1 & HighChol ==0 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "BP_Str_Dia", 
                          HighBP == 0 & HighChol ==1 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "Cho_Str_Dia", 
                          HighBP == 0 & HighChol ==0 & Stroke == 0 & Diabetes ==0 ~ "None", 
                          TRUE ~ "Other"),
         Var2 = case_when(HighBP == 1 & HighChol ==1 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "All4",
                          HighBP == 1 & HighChol ==0 & Stroke == 0 & Diabetes ==0 ~ "HighBP", 
                          HighBP == 0 & HighChol ==1 & Stroke == 0 & Diabetes ==0 ~ "HighChol", 
                          HighBP == 0 & HighChol ==0 & Stroke == 1 & Diabetes ==0 ~ "Stroke", 
                          HighBP == 0 & HighChol ==0 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "Diabetes", 
                          HighBP == 1 & HighChol ==1 & Stroke == 0 & Diabetes ==0 ~ "2ExcStroke", 
                          HighBP == 1 & HighChol ==0 & Stroke == 1 & Diabetes ==0 ~ "2IncStroke", 
                          HighBP == 1 & HighChol ==0 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "2ExcStroke", 
                          HighBP == 0 & HighChol ==1 & Stroke == 1 & Diabetes ==0 ~ "2IncStroke", 
                          HighBP == 0 & HighChol ==1 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "2ExcStroke", 
                          HighBP == 0 & HighChol ==0 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "2IncStroke", 
                          HighBP == 1 & HighChol ==1 & Stroke == 1 & Diabetes ==0 ~ "3IncStroke", 
                          HighBP == 1 & HighChol ==1 & Stroke == 0 & (Diabetes ==1 | Diabetes ==2) ~ "3ExcStroke", 
                          HighBP == 1 & HighChol ==0 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "3IncStroke", 
                          HighBP == 0 & HighChol ==1 & Stroke == 1 & (Diabetes ==1 | Diabetes ==2) ~ "3IncStroke", 
                          HighBP == 0 & HighChol ==0 & Stroke == 0 & Diabetes ==0 ~ "None", 
                          TRUE ~ "Other"),
         Var3 = case_when(Fruits == 1 & Veggies ==1 ~ "Fru_Veg",
                          Fruits == 1 & Veggies ==0 ~ "Fruits", 
                          Fruits == 0 & Veggies ==1 ~ "Veggies", 
                          Fruits == 0 & Veggies ==0 ~ "None", 
                          TRUE ~ "Other"),
         Var4 = case_when(Fruits == 1 & Veggies ==1 ~ "Fru_Veg",
                          Fruits == 1 & Veggies ==0 ~ "NoVegies", 
                          Fruits == 0 & Veggies ==1 ~ "Veggies", 
                          Fruits == 0 & Veggies ==0 ~ "NoVegies", 
                          TRUE ~ "Other"),
         Var5 = case_when(HvyAlcoholConsump == 1 & Smoker ==1 ~ "HAC_Smo",
                          HvyAlcoholConsump == 1 & Smoker ==0 ~ "HvyAlcoholConsump", 
                          HvyAlcoholConsump == 0 & Smoker ==1 ~ "Smoker",  
                          HvyAlcoholConsump == 0 & Smoker ==0 ~ "None", 
                          TRUE ~ "Other"),
         Education2 = case_when(Education >=1  & Education<=3 ~ "1_3", 
                                Education ==4  ~ "4",
                                Education ==5  ~ "5",
                                Education ==6  ~ "6",
                                TRUE ~ "Other"),
         Age2 = case_when(Age >=1  & Age<=4 ~ "1_4", 
                          Age >=5  & Age<=6 ~ "5_6", 
                          Age ==7  ~ "7", 
                          Age >=8  & Age<=9 ~ "8_9", 
                          Age ==10  ~ "10", 
                          Age >=11  & Age<=12 ~ "11_12", 
                          Age ==13 ~ "13", 
                          TRUE ~ "Other")	,
         PhysHlth2 = case_when(PhysHlth >= 0 & PhysHlth <= 2  ~ "0to2",
                               PhysHlth > 2 & PhysHlth <= 9  ~ "3to9", 
                               PhysHlth > 9 & PhysHlth <= 24  ~ "10to24", 
                               PhysHlth > 24 ~ "25AndOver" ,
                               TRUE ~ "Other")
  )

#Create factor levels for the holdout
Holdout$BMI2 <- factor(Holdout$BMI2,levels = c("Underweight", "Healthy", "Overweight", "Obese", "SeverelyObese"))
Holdout$MentHlth2 <- factor(Holdout$MentHlth2,levels = c("0", "1to9", "10to19", "20AndOver"))
Holdout$PhysHlth2 <- factor(Holdout$PhysHlth2,levels = c("0to2", "3to9", "10to24", "25AndOver"))
Holdout$Var1 <- factor(Holdout$Var1,levels = c("All","BP_Cho_Str","BP_Cho_Dia","BP_Str_Dia","Cho_Str_Dia","Str_Dia","BP_Cho","BP_Str","BP_Dia","Cho_Str","Cho_Dia","Diabetes","HighChol","HighBP","Stroke","None"))
Holdout$Var2 <- factor(Holdout$Var2,levels = c("All4","3IncStroke","2IncStroke","Stroke","3ExcStroke","2ExcStroke","Diabetes","HighChol","HighBP","None"))
Holdout$Var3 <- factor(Holdout$Var3,levels = c("Fru_Veg","Fruits","Veggies","None"))
Holdout$Var4 <- factor(Holdout$Var4,levels = c("Fru_Veg","Veggies","NoVegies"))
Holdout$Var5 <- factor(Holdout$Var5,levels = c("HAC_Smo","HvyAlcoholConsump","None","Smoker"))
Holdout$Education2 <- factor(Holdout$Education2,levels = c("1_3","4","5","6"))
Holdout$Age2 <- factor(Holdout$Age2 ,levels = c("1_4","5_6","7","8_9","10","11_12","13"))

#dummy vars for my holdout
Holdout <- dummy_cols(Holdout, select_columns =c('Var1','Var2', 'Var3','Var4','Var5','Diabetes','GenHlth','Education2','Income','BMI2','Age2','MentHlth2','PhysHlth2'), remove_selected_columns = FALSE)

#using the holdout data get use fit.GLM5 model to predict the probability of whether observations will have heart disease or attack.
y_hat_p_fit.glm5_Holdout <- predict(fit.glm5,Holdout,type="prob")
names(y_hat_p_fit.glm5_Holdout)[1] <- "PscoreHDA0GLM5"
names(y_hat_p_fit.glm5_Holdout)[2] <- "PscoreHDA1GLM5"

#create Holdout Summary
Holdout_Results <- Holdout[1]
Holdout_Results <- bind_cols(Holdout_Results, y_hat_p_fit.glm5_Holdout)
#based on my cuttoff value 
Holdout_Results <- Holdout_Results %>% mutate(ResultHDA1GLM5 = case_when (PscoreHDA1GLM5 < cutoffvalue ~ 0, PscoreHDA1GLM5 >= cutoffvalue ~ 1))

# create 10 bins for predicted probability score
Holdout_Results <- Holdout_Results %>% mutate(PredCatGLM5 = case_when (PscoreHDA1GLM5 <= 0.1 ~ 1, PscoreHDA1GLM5 <= 0.2 ~ 2, PscoreHDA1GLM5 <= 0.3 ~ 3, PscoreHDA1GLM5 <= 0.4 ~ 4, PscoreHDA1GLM5 <= 0.5 ~ 5, PscoreHDA1GLM5 <= 0.6 ~ 6, PscoreHDA1GLM5 <= 0.7 ~ 7, PscoreHDA1GLM5 <= 0.8 ~ 8, PscoreHDA1GLM5 <= 0.9 ~ 9, PscoreHDA1GLM5 > 0.9 ~ 10))
GLM5SummaryResultsHoldout<-Holdout_Results %>%group_by(PredCatGLM5) %>%summarise(Freq = n(),PredProb = mean(PscoreHDA1GLM5),Target = mean(HeartDiseaseorAttack))

#calculate confusionMatrix of Holdout
cm.glm5_HoldoutOptimised <- confusionMatrix(factor(Holdout_Results$ResultHDA1GLM5), factor(Holdout_Results$HeartDiseaseorAttack))
cm.glm5_HoldoutOptimised

#get sommers d 
somers2(Holdout_Results$ResultHDA1GLM5,  Holdout$HeartDiseaseorAttack)


