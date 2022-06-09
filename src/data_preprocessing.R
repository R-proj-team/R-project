working_dir <- paste(substr(getwd(), 1, nchar(getwd()) - 3), "data/raw", sep="")
setwd(working_dir)
getwd()

tab <- read.csv("heart_2020_cleaned.csv")

# Expect data
str(tab)
length(tab$BMI)
is.na(tab$BMI)

tab_1 <- tab

tab_1$Smoking <- ifelse(tab_1$Smoking %in% c("Yes"), 1, 0)#yes=1 no=0
tab_1$AlcoholDrinking <- ifelse(tab_1$AlcoholDrinking %in% c("Yes"), 1, 0)#yes=1 no=0
tab_1$Stroke <- ifelse(tab_1$Stroke %in% c("Yes"), 1, 0)#yes=1 no=0
tab_1$DiffWalking <- ifelse(tab_1$DiffWalking %in% c("Yes"), 1, 0)#yes=1 no=0
tab_1$Sex <- ifelse(tab_1$Sex %in% c("Male"), 1, 0)#male=1 female=0
tab_1$Race <- ifelse(tab_1$Race %in% c("White"), 1, 0)#white=1 black=0
tab_1$Diabetic <- ifelse(tab_1$Diabetic %in% c("Yes"), 1, 0)#yes=1 no=0
tab_1$PhysicalActivity<- ifelse(tab_1$PhysicalActivity %in% c("Yes"), 1, 0)#yes=1 no=0
tab_1$Asthma <- ifelse(tab_1$Asthma %in% c("Yes"), 1, 0)#yes=1 no=0
tab_1$KidneyDisease <- ifelse(tab_1$KidneyDisease %in% c("Yes"), 1, 0)#yes=1 no=0
tab_1$AlcoholDrinking <- ifelse(tab_1$AlcoholDrinking %in% c("Yes"), 1, 0)#yes=1 no=0
tab_1$SkinCancer <- ifelse(tab_1$SkinCancer %in% c("Yes"), 1, 0)#yes=1 no=
tab_1$HeartDisease <- ifelse(tab_1$HeartDisease %in% c("Yes"), 1, 0)#yes=1 no=0

class_gen_health <- data.frame(
  GenHealth=c("Poor", "Fair", "Good", "Very good", "Excellent"),
  ClassGenHealth=c(0,1,2,3,4), 
  stringsAsFactors = FALSE
)

tab_1 <- na.omit(tab_1) # usuniecie pustych wierszy
tab_1$GenHealth

tab_2 <- merge(tab_1, class_gen_health, by = "GenHealth", all.x =TRUE)# Dodanie dodatkowej kolumny kt�ra zawiera odpowiednie wartosci w zaleznosci od genhealth

class_age_cat <- data.frame(
  AgeCategory=c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80 or older"),
  ClassAge=0:12,
  stringsAsFactors = FALSE
)

tab_3 <- merge(tab_2, class_age_cat, by = "AgeCategory", all.x =TRUE)

write.csv(tab_3,"../preprocessed/Preprocessed_data.csv")

