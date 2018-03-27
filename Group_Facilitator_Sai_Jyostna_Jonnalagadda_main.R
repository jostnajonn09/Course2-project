setwd("D:/UPGRAD/course 2/Gramener Case Study")

#libraries used
library(ggplot2)
library(tidyr)


loan<-read.csv("loan.csv",stringsAsFactors =FALSE)

#Observing the structure of loan data set
str(loan)

#Data Cleaning and preperation
#Removing all columns which have  only missing values(NA's)

check_na<-function(x){
  if (length(unique(x))==1 & is.na(unique(x))){
    x<-NULL
    return(NULL)
    
  }else{
    return(x)
  }
}  

for (i in colnames(loan)){
  loan[,i]<- loan[,i] %>%  check_na 
}

#Observing the structure of loan data set after removing All NA value columns
str(loan)







# Plotting a bar chart on the target variable loan_status
ggplot(loan,aes(loan_status))+geom_bar(fill="royal blue")+geom_text(stat='count',aes(label=..count..),vjust=1,position = position_dodge(width = 1))

#calculating the proportion of defaulted applicants in total applicants
prop.table(table(loan$loan_status))