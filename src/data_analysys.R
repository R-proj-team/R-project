working_dir <- paste(substr(getwd(), 1, nchar(getwd()) - 3), "data/preprocessed", sep="")
setwd(working_dir)
getwd()


library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)
library(webr)
tabelka <- read.csv("data_preprocessed.csv")
tabelka <- na.omit(tabelka)

wiek <- tabelka[,c(2,4,12)]
#male=1 female=0
tabelaw1 <- tabelka[,c(2,12)]
tabelaw1 <- unique(tabelaw1)

Ftabelka1824 <- wiek %>% filter(AgeCategory=="18-24",Sex==0,HeartDisease==1) 
Mtabelka1824 <- wiek %>% filter(AgeCategory=="18-24",Sex==1,HeartDisease==1)
Ftabelka2529 <- wiek %>% filter(AgeCategory=="25-29",Sex==0,HeartDisease==1)
Mtabelka2529 <- wiek %>% filter(AgeCategory=="25-29",Sex==1,HeartDisease==1)
Ftabelka3034 <- wiek %>% filter(AgeCategory=="30-34",Sex==0,HeartDisease==1)
Mtabelka3034 <- wiek %>% filter(AgeCategory=="30-34",Sex==1,HeartDisease==1)
Ftabelka3539 <- wiek %>% filter(AgeCategory=="35-39",Sex==0,HeartDisease==1)
Mtabelka3539 <- wiek %>% filter(AgeCategory=="35-39",Sex==1,HeartDisease==1)
Ftabelka4044 <- wiek %>% filter(AgeCategory=="40-44",Sex==0,HeartDisease==1)
Mtabelka4044 <- wiek %>% filter(AgeCategory=="40-44",Sex==1,HeartDisease==1)
Ftabelka4549 <- wiek %>% filter(AgeCategory=="45-49",Sex==0,HeartDisease==1)
Mtabelka4549 <- wiek %>% filter(AgeCategory=="45-49",Sex==1,HeartDisease==1)
Ftabelka5054 <- wiek %>% filter(AgeCategory=="50-54",Sex==0,HeartDisease==1)
Mtabelka5054 <- wiek %>% filter(AgeCategory=="50-54",Sex==1,HeartDisease==1)
Ftabelka5559 <- wiek %>% filter(AgeCategory=="55-59",Sex==0,HeartDisease==1)
Mtabelka5559 <- wiek %>% filter(AgeCategory=="55-59",Sex==1,HeartDisease==1)
Ftabelka6064 <- wiek %>% filter(AgeCategory=="60-64",Sex==0,HeartDisease==1)
Mtabelka6064 <- wiek %>% filter(AgeCategory=="60-64",Sex==1,HeartDisease==1)
Ftabelka6569 <- wiek %>% filter(AgeCategory=="65-69",Sex==0,HeartDisease==1)
Mtabelka6569 <- wiek %>% filter(AgeCategory=="65-69",Sex==1,HeartDisease==1)
Ftabelka7074 <- wiek %>% filter(AgeCategory=="70-74",Sex==0,HeartDisease==1)
Mtabelka7074 <- wiek %>% filter(AgeCategory=="70-74",Sex==1,HeartDisease==1)
Ftabelka7579 <- wiek %>% filter(AgeCategory=="75-79",Sex==0,HeartDisease==1)
Mtabelka7579 <- wiek %>% filter(AgeCategory=="75-79",Sex==1,HeartDisease==1)
Ftabelka80 <- wiek %>% filter(AgeCategory=="80 or older",Sex==0,HeartDisease==1)
Mtabelka80 <- wiek %>% filter(AgeCategory=="80 or older",Sex==1,HeartDisease==1)

wF18 <- nrow(Ftabelka1824)
wM18 <- nrow(Mtabelka1824)
wF25 <- nrow(Ftabelka2529)
wM25 <- nrow(Mtabelka2529)
wF30 <- nrow(Ftabelka3034)
wM30 <- nrow(Mtabelka3034)
wF35 <- nrow(Ftabelka3539)
wM35 <- nrow(Mtabelka3539)
wF40 <- nrow(Ftabelka4044)
wM40 <- nrow(Mtabelka4044)
wF45 <- nrow(Ftabelka4549)
wM45 <- nrow(Mtabelka4549)
wF50 <- nrow(Ftabelka5054)
wM50 <- nrow(Mtabelka5054)
wF55 <- nrow(Ftabelka5559)
wM55 <- nrow(Mtabelka5559)
wF60 <- nrow(Ftabelka6064)
wM60 <- nrow(Mtabelka6064)
wF65 <- nrow(Ftabelka6569)
wM65 <- nrow(Mtabelka6569)
wF70 <- nrow(Ftabelka7074)
wM70 <- nrow(Mtabelka7074)
wF75 <- nrow(Ftabelka7579)
wM75 <- nrow(Mtabelka7579)
wF80 <- nrow(Ftabelka80)
wM80 <- nrow(Mtabelka80)

wektorwartosci <- c(wF18,wM18,wF25,wM25,wM30,wF30,wF35,wM35,wF40,wM40,wM45,wF45,wM50,wF50,wM55,wF55,wF60,wM60,wM65,wF65,wF70,wM70,wF75,wM75,wF80,wM80)
tabelaw1 <- cbind(tabelaw1,wektorwartosci)

data <- as.data.frame(tabelaw1)
head(data)
tail(data)

PD = data %>% group_by(AgeCategory, Sex) %>% summarise(n = sum(wektorwartosci))
print(PD)

PieDonut(PD, aes(Sex, AgeCategory, count=n),r0=0.4, r1=1, r2=1.2,selected=1,labelposition=0, title = "Iloœæ ataków serca z podzia³em na p³eæ i grupê wiekow¹\n0 - Kobiety  1 - Mê¿czyŸni")

#________________________________________________________________________________________________________________

library(Hmisc)
library(corrplot)


#macierz korelacji dla ludzi u których wyst¹pi³ zawa³
domacierzy <- tabelka %>% filter(HeartDisease==1)
domacierzy <- domacierzy[,c(5:21)]
res <- cor(domacierzy)
cor(domacierzy, use = "complete.obs")
res <- res[c(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17),c(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 90)


#macierz korelacji dla ludzi u których nie by³o problemów z sercem
domacierzy2 <- tabelka %>% filter(HeartDisease==0)
domacierzy2 <- domacierzy2[,c(5:21)]
res2 <- cor(domacierzy2)
cor(domacierzy2, use = "complete.obs")
res2 <- res2[c(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17),c(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

corrplot(res2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 90)



#macierz korelacji dla wszystkich przebadanych
domacierzy3 <- tabelka[,c(4:21)]
res3 <- cor(domacierzy3)
cor(domacierzy3, use = "complete.obs")
res3 <- res3[c(1,2,3,5,6,7,8,9,10,11,12,13,14,15,16,17,18),c(1,2,3,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]

corrplot(res3, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 90)

#___________________________________________________________________________________________________________________

tbmi <- tabelka
#round(tbmi$BMI)

tbmi$HeartDisease <- ifelse(pzs$HeartDisease %in% c("1"),"Yes", "No")
ggplot(tbmi,aes(x = BMI)) +
  geom_histogram(bins = 40,aes(fill = HeartDisease)) +
  geom_histogram(bins = 40, fill =NA , color = 'black')+
ggtitle("Podzia³ badanych wg BMI oraz wystêpowania u nich problemów sercowych")+
  theme_minimal()



pzs <- tabelka
#pzs <- pzs %>% filter(MentalHealth>0) 
pzs$HeartDisease <- ifelse(pzs$HeartDisease %in% c("1"),"Yes", "No")
ggplot(pzs,aes(x = PhysicalActivity)) +
  geom_histogram(bins = 2,aes(fill = HeartDisease)) +
  geom_histogram(bins = 2, fill =NA , color = 'black')+
  ggtitle("Ile badanych prowadzi lub nie prowadzi aktywnoœæi fizycznej")+
  theme_minimal()


spanko <- tabelka
spanko$HeartDisease <- ifelse(spanko$HeartDisease %in% c("1"),"Yes", "No")
ggplot(spanko,aes(x = SleepTime)) +
  geom_histogram(bins = 25,aes(fill = HeartDisease)) +
  geom_histogram(bins = 25, fill =NA , color = 'black') +
  ggtitle("Zale¿noœæ œrednich godzin snu od problemów sercowych")+
  theme_minimal()


mental <- tabelka
mental <- mental %>% filter(MentalHealth>0)
mental$HeartDisease <- ifelse(mental$HeartDisease %in% c("1"),"Yes", "No")
ggplot(mental,aes(x = MentalHealth)) +
  geom_histogram(bins = 10,aes(fill = HeartDisease)) +
  geom_histogram(bins = 10, fill =NA , color = 'black')+
  ggtitle("mental")+
  theme_minimal()

phys <- tabelka
phys <- phys %>% filter(PhysicalHealth>0)
phys$HeartDisease <- ifelse(phys$HeartDisease %in% c("1"),"Yes", "No")
ggplot(phys,aes(x = PhysicalHealth)) +
  geom_histogram(bins = 10,aes(fill = HeartDisease)) +
  geom_histogram(bins = 10, fill =NA , color = 'black')+
  ggtitle("phys")+
  theme_minimal()

#________________________________________________________________________________________________
