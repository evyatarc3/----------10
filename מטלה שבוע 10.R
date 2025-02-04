library(readr)
library(tidyverse)
library(pROC)

#### טעינת והכנת הנתונים ----
df <- read_csv("Titanic.csv")

# טיפול בערכים חסרים והכנת הנתונים
df <- df |> 
  # הסרת שורות עם ערכים חסרים
  drop_na() |>
  # יצירת משתנים
  mutate(
    gender = factor(Sex, levels = c("male", "female")),  # קידוד גברים כרמת בסיס
    pclass_factor = factor(ifelse(PClass == "1st", "First", "Other"),  # יצירת פקטור למחלקה ראשונה מול אחרות
                           levels = c("First", "Other"))  # קידוד מחלקה ראשונה כרמת בסיס
  )

# בדיקת מספר התצפיות לאחר הטיפול בערכים חסרים
print(paste("מספר התצפיות בדאטה:", nrow(df)))

# בדיקת הקידוד
print("קידוד משתנה מגדר:")
print(contrasts(df$gender))
print("קידוד משתנה מחלקה:")
print(contrasts(df$pclass_factor))

#### סטטיסטיקה תיאורית ----
# יצירת טבלה מסכמת של מספר נוסעים ושורדים לפי מגדר ומחלקה
summary_table <- df |>
  group_by(gender, PClass) |>
  summarise(
    total_passengers = n(),
    survivors = sum(Survived),
    survival_rate = round(mean(Survived) * 100, 1)
  ) |>
  arrange(PClass, gender)

print("טבלה מסכמת:")
print(summary_table)

#### רגרסיה לוגיסטית ----
# מודל 1: אינטרספט בלבד
model1 <- glm(Survived ~ 1, data = df, family = binomial)

# מודל 2: אינטרספט ומגדר
model2 <- glm(Survived ~ 1 + gender, data = df, family = binomial)

# מודל 3: אינטרספט, מגדר ומחלקה
model3 <- glm(Survived ~ 1 + gender + pclass_factor, data = df, family = binomial)

# הצגת תוצאות המודלים
print("תוצאות מודל 1 (אינטרספט בלבד):")
print(summary(model1))
print("תוצאות מודל 2 (אינטרספט + מגדר):")
print(summary(model2))
print("תוצאות מודל 3 (אינטרספט + מגדר + מחלקה):")
print(summary(model3))

#### ניתוח ROC ----
# חיזוי הסתברויות עבור כל מודל
df$predict_model1 <- predict(model1, type = "response")
df$predict_model2 <- predict(model2, type = "response")
df$predict_model3 <- predict(model3, type = "response")

# חישוב עקומות ROC
roc_model1 <- roc(df$Survived, df$predict_model1)
roc_model2 <- roc(df$Survived, df$predict_model2)
roc_model3 <- roc(df$Survived, df$predict_model3)

# חישוב שטח מתחת לעקומה (AUC)
auc_scores <- c(
  Model1 = auc(roc_model1),
  Model2 = auc(roc_model2),
  Model3 = auc(roc_model3)
)

print("ציוני AUC:")
print(round(auc_scores, 3))

# יצירת גרף ROC
plot(roc_model1, col = "blue", main = "השוואת עקומות ROC",
     xlab = "1-Specificity", ylab = "Sensitivity")
plot(roc_model2, add = TRUE, col = "red")
plot(roc_model3, add = TRUE, col = "green")
legend("bottomright", 
       legend = c(
         paste("Model 1 (AUC =", round(auc_scores[1], 3), ")"),
         paste("Model 2 (AUC =", round(auc_scores[2], 3), ")"),
         paste("Model 3 (AUC =", round(auc_scores[3], 3), ")")
       ),
       col = c("blue", "red", "green"),
       lwd = 2)
