# ustawienie WD, bazuj¹c na folderze ~/src (folder z kodem)
working_dir <- paste(substr(getwd(), 1, nchar(getwd()) - 3), "data/preprocessed", sep="")
setwd(working_dir)
getwd()

# wczytanie danych
frame <- read.csv("data_preprocessed.csv")[, -(1:3)]

# sprawdzenie zakresu niektórych kolumn
min(frame$MentalHealth)
max(frame$MentalHealth)

min(frame$PhysicalHealth)
max(frame$PhysicalHealth)

# dodatkowa obróbka
# usuniêcie "nieracjonalnych przypadków" (3, 14 wziête z analizy)
frame <- frame[frame$SleepTime > 3 && frame$SleepTime < 14]

# balansowanie danych
frame_with_disease <- frame[frame$HeartDisease == 1,]
frame_without_disease <- frame[frame$HeartDisease == 0,]

nrow(frame_with_disease)
nrow(frame_without_disease)

frame_without_disease <- frame_without_disease[sample(1:nrow(frame_without_disease), nrow(frame_with_disease)), ]

frame_balanced <- rbind(frame_with_disease, frame_without_disease)
frame_balanced <- frame_balanced[sample(1:nrow(frame_balanced)),]

# bazuj¹c na macierzach korelacji wybieramy tylko kolumny które mog¹ mieæ wp³yw na wynik
# family=binomial - logistic regression
model <- glm(HeartDisease ~ AlcoholDrinking + PhysicalHealth + DiffWalking + Race + Diabetic + KidneyDisease + ClassGenHealth + ClassAge, data=frame_balanced, family="binomial")

# przyk³adowe przewidywanie
to_predict <- data.frame(c(1, 1, 0), c(16, 7, 10), c(1, 0, 1), c(1, 0, 0), c(1, 0, 1), c(1, 0, 1), c(1, 0, 0), c(6, 2, 5))
colnames(to_predict) <- c("AlcoholDrinking", "PhysicalHealth", "DiffWalking", "Race", "Diabetic", "KidneyDisease", "ClassGenHealth", "ClassAge")

scores <- predict(model, to_predict, type="response")
round(scores)