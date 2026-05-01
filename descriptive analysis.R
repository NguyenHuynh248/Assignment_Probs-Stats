# =========================================
# 0. Cài thư viện cần thiết
# =========================================
install.packages(c("ggplot2", "reshape2"))
library(ggplot2)
library(reshape2)

# =========================================
# 1. Load & preprocessing (giữ logic từ file của bạn)
# =========================================
df <- read.csv("add.csv", header = FALSE, na.strings = "?")

# Tách X và y
X <- df[, -ncol(df)]
y <- df[, ncol(df)]

# Loại cột missing >50%
na_count <- colSums(is.na(X))
threshold <- nrow(X) * 0.5
X <- X[, na_count < threshold]

# Loại cột không biến thiên
X <- X[, sapply(X, function(col) length(unique(col)) > 1)]

# Lấy 20 feature đầu
num_features <- min(20, ncol(X))
X_subset <- X[, 1:num_features]
colnames(X_subset) <- paste0("Feature_", 1:ncol(X_subset))

# Clean label
y <- as.factor(trimws(y))

# Ép numeric
X_subset[] <- lapply(X_subset, function(col) {
  suppressWarnings(as.numeric(as.character(col)))
})

# Xử lý missing
X_subset[] <- lapply(X_subset, function(col) {
  if (all(col %in% c(0, 1, NA))) {
    col[is.na(col)] <- 0
  } else {
    col[is.na(col)] <- median(col, na.rm = TRUE)
  }
  col
})

# Data cuối
df_clean <- data.frame(X_subset, Label = y)

# =========================================
# 2. SUMMARY STATISTICS
# =========================================
summary(df_clean)

# =========================================
# 3. HISTOGRAM (Feature_1)
# =========================================
p1 <- ggplot(df_clean, aes(x = Feature_1, fill = Label)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  theme_minimal() +
  labs(title = "Histogram Feature_1", x = "Value", y = "Count")

print(p1)
ggsave("hist_feature1.png", p1)

# =========================================
# 4. DENSITY PLOT
# =========================================
p2 <- ggplot(df_clean, aes(x = Feature_1, color = Label)) +
  geom_density(size = 1) +
  theme_minimal() +
  labs(title = "Density Plot Feature_1")

print(p2)
ggsave("density_feature1.png", p2)

# =========================================
# 5. SCATTER PLOT (Feature_1 vs Feature_2)
# =========================================
p3 <- ggplot(df_clean, aes(x = Feature_1, y = Feature_2, color = Label)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Scatter Plot")

print(p3)
ggsave("scatter.png", p3)

# =========================================
# 6. CORRELATION HEATMAP
# =========================================
corr_matrix <- cor(df_clean[, -ncol(df_clean)])

melted_corr <- melt(corr_matrix)

p4 <- ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap")

print(p4)
ggsave("heatmap.png", p4)

# =========================================
# 7. BOXPLOT (so sánh ad vs nonad)
# =========================================
p5 <- ggplot(df_clean, aes(x = Label, y = Feature_1, fill = Label)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot Feature_1")

print(p5)
ggsave("boxplot.png", p5)

# =========================================
# 8. DONE
# =========================================
cat("All plots saved successfully!")

