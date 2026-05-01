data <- read.csv(file = "add.csv", header = TRUE, sep = ",")


# Remove unrelated columns (including index column)
data <- data[, -c(1, 6:1559)]

# Rename columns
colnames(data) <- c("Height", "Width", "Aspect_ratio", "Local", "Is_Ad")

head(data)

# Replace ? with NA
data <- as.data.frame(lapply(data, function(x) ifelse(grepl("\\?", x), NA, x)))


# Convert Is_Ad to binary value (0 = nonad, 1 = ad) 
data$Is_Ad <- ifelse(data$Is_Ad == "ad.", 1, 0)

table(data$Is_Ad)

# Convert all values to numeric
data <- as.data.frame(sapply(data, as.numeric))

summary(data)

head(data)

# Count number of NA
na_count <- sapply(data, function(x) sum(is.na(x)))
na_count

# Fill NA with median value (in Height, Width)
for (col in c("Height", "Width")) {
  data[col] <- lapply(data[col], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
}

# For Local, use the rounded mean
data["Local"] <- lapply(data["Local"], function(x) ifelse(is.na(x), round(mean(x, na.rm = TRUE)), x))


# Recalculate aspect ratio
data["Aspect_ratio"] <- round(data["Width"] / data["Height"], 4)

# Export preprocessed data
write.csv(data, "preprocessed.csv", row.names = FALSE)

## ================= INFERENTIAL STATISTICS =================

# ================= SETUP =================
set.seed(25042026)

# Split 75/25
index <- sample(1:nrow(data), size = 0.75 * nrow(data))
train_set <- data[index, ]
test_set  <- data[-index, ]


# ================= MODELS =================

# Model A: Height + Width
model_hw <- glm(Is_Ad ~ Height + Width, data = train_set, family = binomial)

# Model B: Width
model_w <- glm(Is_Ad ~ Width, data = train_set, family = binomial)

# Model C: Aspect Ratio
model_ar <- glm(Is_Ad ~ Aspect_ratio, data = train_set, family = binomial)


# ================= SUMMARY =================
cat("===== MODEL HW =====\n")
summary(model_hw)

cat("===== MODEL W =====\n")
summary(model_w)

cat("===== MODEL AR =====\n")
summary(model_ar)


# ODDS RATIO 
cat("\n===== ODDS RATIO (Width model) =====\n")
print(exp(coef(model_w)))

# CONFIDENCE INTERVAL
cat("\n===== CONFIDENCE INTERVAL (Width model) =====\n")
print(confint(model_w))

#  EVALUATION
evaluate_model <- function(model, dataset, name) {
  
  prob <- predict(model, newdata = dataset, type = "response")
  pred <- ifelse(prob > 0.5, 1, 0)
  
  pred <- factor(pred, levels = c(0,1))
  actual <- factor(dataset$Is_Ad, levels = c(0,1))
  
  acc <- mean(pred == actual)
  
  cat("\n======", name, "======\n")
  cat("Accuracy:", round(acc, 4), "\n")
  cat("Confusion Matrix:\n")
  print(table(Predicted = pred, Actual = actual))
}


evaluate_model(model_w, test_set, "Model 1 (Width)")
evaluate_model(model_ar, test_set, "Model 2 (Aspect Ratio)")

# ================= ROC =================
library(pROC)

prob_w  <- predict(model_w, newdata = test_set, type = "response")
prob_ar <- predict(model_ar, newdata = test_set, type = "response")

roc_w  <- roc(test_set$Is_Ad, prob_w)
roc_ar <- roc(test_set$Is_Ad, prob_ar)

png("roc_upgrade.png", width = 800, height = 600)

plot(roc_w, col = "blue", lwd = 3, main = "ROC Curve Comparison")
lines(roc_ar, col = "orange", lwd = 3)

legend("bottomright",
       legend = c("Width Model", "Aspect Ratio Model"),
       col = c("blue", "orange"),
       lwd = 3)

dev.off()

# ================= AUC =================
cat("\n===== AUC =====\n")
cat("Width:", auc(roc_w), "\n")
cat("Aspect Ratio:", auc(roc_ar), "\n")

