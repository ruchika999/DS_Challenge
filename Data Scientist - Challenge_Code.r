#set current directory
setwd("C:/Users/ruchika.kumari/Desktop/test/")
getwd()

#setting the Java heap size
options(java.parameters = "-Xmx4048m")
memory.size()
memory.limit()


# disable scientific notation
options(scipen = 999)


library("psych")
require("xlsx")
require("InformationValue")
require(dplyr)
require(sqldf)
##############################read the table data_train####################
data_train<-read.csv("file:///C:/Users/ruchika.kumari/Desktop/test/ds_data/data_train.csv")
data_train$id<-NULL   #dropping customer ID

View(data_train)

#Understand distribution of the data

summary1 <- describe(data_train)
View(summary1)
write.csv(summary1,"summary.csv")
###################################list of numerical variable##################

numerical_vars<-c("num1","num2","num3","num4","num5","num6","num7","num8","num9","num10","num11","num12","num13","num14","num15","num16","num17","num18","num19","num20","num21","num22","num23","der1","der2","der3","der4","der5","der6","der7","der8","der9","der10","der11","der12","der13","der14","der15","der16","der17","der18","der19")


###################################List of categorical variables###############

categorical_vars<-c("cat1","cat2","cat3","cat4","cat5","cat6","cat7","cat8","cat9","cat10","cat11","cat12","cat13","cat14")


#as all character vars are in number format so we will change it to factor

data_train[,categorical_vars]=sapply(data_train[categorical_vars],FUN = factor)


##########################Summary(univariate)######################################

#user defined function for descriptive analysis

mysummary<-function(x)
{
  n=length(x)
  nmiss=sum(is.na(x))
  misspct=mean(is.na(x))
  mean=mean(x,na.rm = T)
  median=median(x,na.rm = T)
  std=sd(x,na.rm = T)
  var=var(x,na.rm = T)
  min=min(x,na.rm = T)
  max=max(x,na.rm = T)
  range=max-min
  pctl=quantile(x,p=c(0,.01,.05,.1,.25,.5,.75,.90,.95,.99,1),na.rm = T)
  return(c(N=n, Nmiss =nmiss, Nmisspct = misspct, avg=mean, meidan=median, 
           std=std, var=var, range=range, min=min,max=max,pctl=pctl))
  
}



#######summary for numeric vars################################################################
full_stats_numerical<-t(data.frame(apply(data_train[numerical_vars],2,FUN = mysummary)))
View(full_stats_numerical)

######################categorical vars summary################################################################

categorica_summary=function(x)
{
  Var_Type=class(x)
  n<-length(x)
  nmiss<-sum(is.na(x))
  fre<-table(x)
  prop<-prop.table(table(x))
  return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
}


#this cat_sum will come in the form of list and have applied user defined "categorica_summary" function
cat_sum=sapply(data_train[categorical_vars],FUN = categorica_summary)
View(cat_sum)


#################################Outlier treatment#########################################################################

#user defined outlier function

outlier<-function(x)
{
  
  uc<-quantile(x,p=.95,na.rm = T)
  lc<-quantile(x,p=.05,na.rm = T)
  #we have taken UC as .95tile bcoz after seeing "full_stats" there is large 
  # difference between 95%tile and 99%tile  while 90%tile is closer to 95%tile.
  
  x<-ifelse(x>uc,uc,x)
  x<-ifelse(x<lc,lc,x)
  return(x)
}

#-------------------------------------------------------------------------------------------------------------
data_train[,numerical_vars]<-data.frame(apply(data_train[numerical_vars],2,FUN = outlier))
#-------------------------------------------------------------------------------------------------------------
#for categorical
data_train[,categorical_vars] <- apply(data.frame(data_train[,categorical_vars]), 2,
                                            function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})
#-------------------------------------------------------------------------------------------------------------
summary_numerical<-t(sapply(data_train[numerical_vars],FUN = mysummary))
summary_categorical=sapply(data_train[categorical_vars],FUN = categorica_summary)
View(summary_numerical)
View(summary_categorical)

#-------------------------------------------------------------------------------------------------------------
#########missing value treatment###########################################################################################


missing_value_treat<-function(x){
  x[is.na(x)]=mean(x,na.rm = TRUE)
  return(x)
}
#-------------------------------------------------------------------------------------------------------------
data_train[,numerical_vars]<-data.frame(sapply(data_train[numerical_vars],FUN = missing_value_treat))
data_train[,categorical_vars] <- apply(data.frame(data_train[,categorical_vars]), 2, 
                                            function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})
#-------------------------------------------------------------------------------------------------------------
summary_numerical<-t(sapply(data_train[numerical_vars],FUN = mysummary))
summary_categorical=sapply(data_train[categorical_vars],FUN = categorica_summary)
View(summary_numerical)
View(summary_categorical)
View(data_train)
#-------------------------------------------------Variable Reduction------------------------------------------------------------
############################Chi Sq Test##################################################################################

#chi-square test (between target and each categorical variable)
chisq=function(x){
  chisq.test(data_train$target,x)
}
#-------------------------------------------------------------------------------------------------------------
chisq_val=t(sapply(data_train[categorical_vars],FUN = chisq))
View(chisq_val)
getwd()

write.csv(chisq_val,"chisq.csv")
#-------------------------------------------------------------------------------------------------------------
#after analyzing chi square value we found "cat14","cat3","cat7","cat9","cat10","cat4","cat6","cat5","cat12","cat8","cat11"as important
final_char_vars=c("cat14","cat3","cat7","cat9","cat10","cat4")

#-------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
#user defined anova check function
anova_check=function(x){
  test=aov(target~x,data=data_train)  
  return(summary(test)[[1]][["F value"]])#returns f value for each vriables
}

anovaF=t(data.frame(sapply(data_train[numerical_vars],FUN = anova_check)))
View(anovaF)

# so as per anova list of important vars are "num21","num14","num20","num17","num4","num3","num18"
final_num_vars<-c("num21","num14","num20","num17","num4","num3","num18")


###################### #Assumptions Check#########################################################################


################check for multicollinearilty treatment######################################################################
View(data_train[numerical_vars])
corr_mat<-cor(data_train[final_num_vars],use = "complete.obs")#bivariate analysis,
View(corr_mat)
write.csv(corr_mat,"correlation.csv")

#-------------------------------------------------------------------------------------------------------------
### DECIDING NUMBER OF FACTORS USING SCREE PLOT

scree(corr_mat, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT
FA<-fa(r=corr_mat, 4, rotate="varimax", fm="pa")      ### CONDUCTING FACTOR ANALYSIS
print(FA)                                                    ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)  

#-------------------------------------------------------------------------------------------------------------

write.csv(FA_SORT$loadings, "loadings1.csv") ### ### CAPTURING ONLY LOADINGS 



#-------------------------------------------------------------------------------------------------------------

######list of important variables(categorical and numerical variables after tranformation)
data_train_final=NULL
data_train_final=dplyr::select(data_train,c(final_num_vars,final_char_vars,"target"))
View(data_train_final)
names(data_train_final)
###########################################################################################################################

###########################################Building Models for training dataset


model<-glm(target~ .,data = data_train_final,
           family = binomial(logit))


summary(model) #Output of Logistic Regression
#-------------------------------------------------------------------------------------------------------------

train1<- cbind(data_train_final, Prob=predict(model, type="response")) 
View(train1)

str(train1)
train1$target=as.factor(train1$target)

C=Concordance(train1$target, train1$Prob)
View(C$Concordance)
SD=somersD(train1$target, train1$Prob)
cut1<-optimalCutoff(train1$target, train1$Prob, optimiseFor = "Both", returnDiagnostics = TRUE)

#ROC Curve
plotROC(train1$target, train1$Prob)
ROCTable<-data.frame(cut1$sensitivityTable)
View(ROCTable)
write.csv(ROCTable, "ROCTable.csv")

confusionMatrix(train1$target, train1$Prob, threshold=0.03927)
#------------------------------------------------------------
test=read.csv("file:///C:/Users/ruchika.kumari/Desktop/test/ds_data/data_test.csv")

#----------------------------------------------------------------------------------
###treating missing value and outlier for test data for both numerical and categorical vars separately
test[,numerical_vars]<-data.frame(sapply(test[numerical_vars],FUN = missing_value_treat))
test[,categorical_vars] <- apply(data.frame(test[,categorical_vars]), 2, 
                                            function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})
											
test[,numerical_vars]<-data.frame(apply(test[numerical_vars],2,FUN = outlier))

#for categorical
test[,categorical_vars] <- apply(data.frame(test[,categorical_vars]), 2,
                                            function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})
											
#-----------------------------------------------------------------------------------------											
test$cat14=as.factor(test$cat14)
test$cat3=as.factor(test$cat3)
test$cat7=as.factor(test$cat7)
test$cat9=as.factor(test$cat9)
test$cat10=as.factor(test$cat10)
test$cat4=as.factor(test$cat4)
test<- cbind(test, Prob=predict(model,newdata=test, type="response")) 
View(test)
final=sqldf::sqldf("select id,Prob from test")
write.csv(final,"final_result.csv")
#-------------------------end-----------------------------------


