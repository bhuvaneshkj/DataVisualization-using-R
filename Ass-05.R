setwd("C:/Users/chat2/Downloads")
data<- read.csv("budgets.csv")
view(data)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(lubridate)
install.packages("reshape")
library(reshape)
library(gridExtra)
view(data)

#Question 2 - Grouped Bar Chart for spending year 2021- 2030
#selecting
df<-select(data, -c( X2019, X2020))
view(df)
#names(df)[3]<-"2021"
#names(df)[4]<-"2022"
#names(df)[5]<-"2023"
#names(df)[6]<-"2024"
#names(df)[7]<-"2025"
#names(df)[8]<-"2026"
#names(df)[9]<-"2027"
#names(df)[10]<-"2028"
#names(df)[11]<-"2029"
#names(df)[12]<-"2030"
#view(df)



#putting two datasets into two separate dataframes - discretionary and mandatory
discretionary<- df[c(1:2),]
view(discretionary)
up_discretionary<- select(discretionary, -c(programs))
view(up_discretionary)
#melting for discretionary
melt_up_discretionary<-melt(up_discretionary, id=c("category"))
view(melt_up_discretionary)
plot_2<-ggplot(melt_up_discretionary, aes(fill=category, y=melt_up_discretionary$value, x=melt_up_discretionary$variable)) +
  geom_bar(position="dodge", stat="identity")  +xlab("year") + ylab("value") + ggtitle("Discretionary Group Bar Chart- spending over 2021 to 2030")
plot_2


#Mandatory data
mandatory<- df[c(3:6),]
view(mandatory)
up_mandatory<- select(mandatory, -c(programs))
view(up_mandatory)
#melting for mandatory
melt_up_mandatory<- melt(up_mandatory, id=c("category"))
view(melt_up_mandatory)
plot_3<-ggplot(melt_up_mandatory, aes(fill=category, y=melt_up_mandatory$value, x=melt_up_mandatory$variable)) +
  geom_bar(position="dodge", stat="identity")  +xlab("year") + ylab("value") + ggtitle("Mandatory Group Bar Chart- spending over 2021 to 2030")
plot_3
grid.arrange(plot_2, plot_3, top="Grouped Bar Charts for the two Programs")

#Question 1

theme_set(theme_classic())
view(data)

#data$'Category_budget'<-rownames(data)
view(data)
#data$z_score<- round((data$X2020)-mean(data$X2020)/sd(data$X2020),2)
#view(data)
#data$add<- data$X2020+data$X2021
data$percdev <- ((data$X2021 - data$X2020)/data$X2020)
view(data)
#computed_value= data
# 2021_spending-2020_spending)/2020_spending)
data$avg<- ifelse(data$percdev < 0, "Below", "Above")
#data<-data[order(data$percdev)]
library(ggplot2)
#data$'Category_budget' <- factor(data$'Category_budget',levels = data$'Category_budget')  
plot<-ggplot(data,aes( x=data$category,y=data$percdev,label=data$category)) +
  geom_bar(stat="identity",aes(fill=data$avg), width= 0.75)+coord_flip()
plot
view(data)