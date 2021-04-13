##ggplot on titanic dataset

df<-read.csv('titanic.csv')
df<-unique(df)
df<-df[-c(5,6,9,10,11)]
df
colSums(is.na(df))
library(ggplot2)
library(dplyr)
head(df)
ggplot(df,aes(x=Pclass))+
  geom_histogram(bins=3,fill='green')+
  labs(title ='GGPLOT',xlab='Pclasss',ylab='Count',subtitle ="Counting")

ggplot(df,aes(x=Pclass))+
  geom_density(fill='red')

ggplot(df,aes(x=Pclass))+
  geom_dotplot()

ggplot(df,aes(x=Survived,y=Pclass,colors=Embarked))+
  geom_point()


getwd()
df<-read.csv("Loan payments data.csv")
head(df)
dim(df)
df<-unique(df)
dim(df)

colSums(is.na(df))
summary(df)

df$past_due_days[is.na(df$past_due_days)]<-median(df$past_due_days,na.rm = T)

library(dplyr)
library(ggplot2)

new_group<-group_by(df,loan_status)
summarise(new_group,mean(Principal),as.integer(mean(age)))

mode<-function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

new_group1<-group_by(df,loan_status)
summarise(new_group1,mode(Gender),mode(education))

class(df$date)
df$date<-as.Date(df$effective_date,"%d-%m-%Y")

df %>%
  group_by(education) %>%
  summarise(sum(Principal))

ggplot(df,aes(x=date,y=Principal))+
  geom_line()

ggplot(df,aes(age))+
  geom_density(fill='red')

ggplot(df,aes(loan_status))+
  geom_bar(fill="yellow")

ggplot(df,aes(x=Gender,y=age))+
  geom_boxplot()