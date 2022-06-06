setwd("C:/Users/Micha³/R-project/data/raw")
getwd()
library(read.csv)
tab<-read.csv("heart_2020_cleaned.csv")
str(tab)
length(tab$BMI)
is.na(tab$BMI)


tab1<-tab
tab1$Smoking <- ifelse(tab1$Smoking %in% c("Yes"), 1, 0)#yes=1 no=0

tab1$AlcoholDrinking <- ifelse(tab1$AlcoholDrinking %in% c("Yes"), 1, 0)#yes=1 no=0

tab1$Stroke <- ifelse(tab1$Stroke %in% c("Yes"), 1, 0)#yes=1 no=0

tab1$DiffWalking <- ifelse(tab1$DiffWalking %in% c("Yes"), 1, 0)#yes=1 no=0

tab1$Sex <- ifelse(tab1$Sex %in% c("Male"), 1, 0)#male=1 female=0

tab1$Race <- ifelse(tab1$Race %in% c("White"), 1, 0)#white=1 black=0

tab1$Diabetic <- ifelse(tab1$Diabetic %in% c("Yes"), 1, 0)#yes=1 no=0

tab1$PhysicalActivity<- ifelse(tab1$PhysicalActivity %in% c("Yes"), 1, 0)#yes=1 no=0

tab1$Asthma <- ifelse(tab1$Asthma %in% c("Yes"), 1, 0)#yes=1 no=0

tab1$KidneyDisease <- ifelse(tab1$KidneyDisease %in% c("Yes"), 1, 0)#yes=1 no=0

tab1$AlcoholDrinking <- ifelse(tab1$AlcoholDrinking %in% c("Yes"), 1, 0)#yes=1 no=0

tab1$SkinCancer <- ifelse(tab1$SkinCancer %in% c("Yes"), 1, 0)#yes=1 no=

tab1$HeartDisease <- ifelse(tab1$HeartDisease %in% c("Yes"), 1, 0)#yes=1 no=0

class_GH <- data.frame(GenHealth=c("Poor","Fair","Good","Very good","Excellent"),
                       Class=c(0,1,2,3,4), 
                       stringsAsFactors = FALSE)#ramka 

tab1 <- na.omit(tab1)#usniecie pustych wierszy
tab1$GenHealth

tab2 <- merge(tab1, class_GH, by = "GenHealth", all.x =TRUE)#Dodanie dodatkowej kolumny która zawiera odpowiednie wartosci w zaleznosci od genhealth


class_AGE <- data.frame(AGE=c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80 or older"),
                       ClassAGE=c(0,1,2,3,4,5,6,7,8,9,10,11,12), 
                       stringsAsFactors = FALSE)#ramka 

tab2$AgeCategory

tab3 <- merge(tab2, class_AGE, by = "AgeCategory", all.x =TRUE)

n<-length(tab2$AgeCategory)
n

for (i in 1:n){
  if (tab2[i,11]=="18-24"){tab2[i,11]=0}
  if (tab2[i,11]=="25-29"){tab2[i,11]=1}
  if (tab2[i,11]=="30-34"){tab2[i,11]=2}
  if (tab2[i,11]=="35-39"){tab2[i,11]=3}
  if (tab2[i,11]=="40-44"){tab2[i,11]=4}
  if (tab2[i,11]=="45-49"){tab2[i,11]=5}
  if (tab2[i,11]=="50-54"){tab2[i,11]=6}
  if (tab2[i,11]=="55-59"){tab2[i,11]=7}
  if (tab2[i,11]=="60-64"){tab2[i,11]=8}
  if (tab2[i,11]=="65-69"){tab2[i,11]=9}
  if (tab2[i,11]=="70-74"){tab2[i,11]=10}
  if (tab2[i,11]=="75-79"){tab2[i,11]=11}
  if (tab2[i,11]=="80 or older"){tab2[i,11]=12}
}



