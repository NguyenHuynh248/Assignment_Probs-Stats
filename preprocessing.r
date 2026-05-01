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


