# Učitavanje biblioteka

```{r}
library(tidyverse)
library(tidymodels)
library(caret)
library(pheatmap)
library(vip)
library(pROC)
library(gridExtra)
library(naniar)
library(MLmetrics)
library(reshape2)
```

# Priprema podataka

```{r}
input_dir <- "/Users/aleksamilovanovic/Downloads/f1"

races <- read_csv(file.path(input_dir, "races.csv"), na = "\\N")
results <- read_csv(file.path(input_dir, "results.csv"), na = "\\N")
drivers <- read_csv(file.path(input_dir, "drivers.csv"), na = "\\N")
constructors <- read_csv(file.path(input_dir, "constructors.csv"), na = "\\N")

race_results <- races %>%
  inner_join(results, by = "raceId") %>%
  inner_join(drivers, by = "driverId") %>%
  inner_join(constructors, by = "constructorId")

race_results <- race_results %>%
  mutate(podium_finish = if_else(positionOrder <= 3, 1, 0),
         driver_name = paste(forename, surname)) 

relevant_columns <- c(
  "raceId", "year", "round", "circuitId", "grid", "position",
  "points", "laps", "fastestLapSpeed", "driverId", "driver_name",
  "constructorId", "podium_finish"
)

race_results <- race_results %>% select(all_of(relevant_columns))
```

## Prikaz podataka
```{r}
head(race_results)
tail(race_results)
colSums(is.na(race_results))
```

### Sređivanje NA
```{r}
numerical_columns <- sapply(race_results, is.numeric)
race_results[, numerical_columns] <- lapply(race_results[, numerical_columns], function(x) ifelse(is.na(x), 0, x))

categorical_columns <- sapply(race_results, function(x) is.character(x) | is.factor(x))
race_results[, categorical_columns] <- lapply(race_results[, categorical_columns], function(x) {
  x[is.na(x)] <- "Unknown"
  return(x)
})

race_results <- race_results[!duplicated(race_results), ]
```

```{r}
cat("\nCleaned dataset info:\n")
str(race_results)
summary(race_results)
cat("\nDataset dimensions (rows, columns): ", dim(race_results), "\n")
```

### Podaci koje koristimo
```{r preview-data}
print("First 10 rows:")
print(head(race_results, 10))

print("Last 10 rows:")
print(tail(race_results, 10))

cat("\nFinal shape of the cleaned dataset:\n")
print(dim(race_results))

cat("\nColumns in the cleaned dataset:\n")
print(colnames(race_results))

print(summary(race_results))
```

# Osnovni plot prikazi

### Podijum
```{r}
ggplot(race_results, aes(x = factor(podium_finish))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Raspodela završetka na podijumu",
       x = "Podium Finish (1 = Da, 0 = Ne)",
       y = "Broj") +
  theme_minimal()
```

### Heatmap glavnih features
```{r}
race_results_cleaned <- race_results %>%
  mutate(across(c('grid', 'position', 'points', 'laps', 'fastestLapSpeed', 'podium_finish'), as.numeric))

corr <- cor(race_results_cleaned[, c('grid', 'position', 'points', 'laps', 'fastestLapSpeed', 'podium_finish')], use = "pairwise.complete.obs")

corr_melt <- melt(corr)
ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1,1), name = "Correlation") +
  theme_minimal() +
  labs(title = "Heatmap korelacija glavnih atributa", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

### Procenat završetaka na podijumu po startnoj poziciji
```{r}
podium_rate <- race_results %>%
  group_by(grid) %>%
  summarise(podium_finish_rate = mean(as.numeric(podium_finish), na.rm = TRUE)) %>%
  ungroup()

ggplot(podium_rate, aes(x = as.numeric(grid), y = podium_finish_rate)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Procenat završetaka na podijumu po startnoj poziciji",
       x = "Starting Grid Position",
       y = "Podium Finish Rate") +
  theme_minimal()
```

### Top 10 konstruktora po broju osvajanja podijuma
```{r}
top_constructors <- race_results %>%
  group_by(constructorId) %>%
  summarise(podium_finish = sum(as.numeric(podium_finish), na.rm = TRUE)) %>%
  arrange(desc(podium_finish)) %>%
  slice_head(n = 10) %>%
  left_join(constructors %>% select(constructorId, name), by = "constructorId")

ggplot(top_constructors, aes(x = reorder(name, podium_finish), y = podium_finish)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  
  labs(title = "Top 10 konstruktora po broju osvajanja podijuma",
       x = "Konstruktori",
       y = "Broj osvajanja podijuma") +
  theme_minimal()
```

### Top 5 vozača po performansama tokom godina
```{r}
top_drivers <- race_results %>%
  group_by(driver_name) %>%
  summarise(total_points = sum(as.numeric(points), na.rm = TRUE)) %>%
  arrange(desc(total_points)) %>%
  slice_head(n = 5) %>%
  pull(driver_name)

top_drivers_data <- race_results %>%
  filter(driver_name %in% top_drivers) %>%
  mutate(points = as.numeric(points))

yearly_points <- top_drivers_data %>%
  group_by(year, driver_name) %>%
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop")

ggplot(yearly_points, aes(x = year, y = total_points, color = driver_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Top 5 vozača po performansama tokom godina",
       x = "Godina",
       y = "Ukupni bodovi",
       color = "Vozač") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right"
  ) +
  scale_x_continuous(breaks = unique(yearly_points$year)) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
```

### Top 10 vozača po broju osvajanja podijuma
```{r}
top_drivers <- race_results %>%
  group_by(driver_name) %>%
  summarise(podium_finish = sum(as.numeric(podium_finish), na.rm = TRUE)) %>%
  arrange(desc(podium_finish)) %>%
  slice_head(n = 10)

ggplot(top_drivers, aes(x = reorder(driver_name, podium_finish), y = podium_finish)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + 
  labs(title = "Top 10 vozača po broju osvajanja podijuma",
       x = "Vozač",
       y = "Broj osvajanja podijuma") +
  theme_minimal()
```

### Promene pozicije tokom poslednje trke
```{r}
latest_race <- max(race_results$raceId, na.rm = TRUE)
race_data <- race_results %>%
  filter(raceId == latest_race) %>%
  arrange(grid) %>%
  select(driver_name, grid, position)

race_long <- race_data %>%
  pivot_longer(cols = c(grid, position), names_to = "stage", values_to = "pos") %>%
  mutate(stage = factor(stage, levels = c("grid", "position"), labels = c("Start", "Finish")))

ggplot(race_long, aes(x = stage, y = pos, group = driver_name)) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  geom_text(data = race_long %>% filter(stage == "Finish"),
            aes(label = driver_name), hjust = -0.1, size = 3) +
  scale_y_reverse(breaks = seq(max(race_long$pos), 1)) +  
  labs(title = "Promene pozicije tokom poslednje trke",
       x = "Tok trke",
       y = "Pozicija") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 11)) +
  coord_cartesian(clip = "off") 
```

# Priprema podataka za treniranje
```{r}
features <- c('grid', 'round', 'fastestLapSpeed')
data <- race_results_cleaned %>%
  select(all_of(features), podium_finish) %>%
  mutate(podium_finish = as.factor(podium_finish))

set.seed(42)
train_index <- createDataPartition(data$podium_finish, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

preProcValues <- preProcess(train_data[, features], method = c("center", "scale"))

train_scaled <- predict(preProcValues, train_data[, features])
test_scaled <- predict(preProcValues, test_data[, features])

X_train_scaled <- train_scaled
y_train <- train_data$podium_finish

X_test_scaled <- test_scaled
y_test <- test_data$podium_finish
```

```{r}
cat("Features used:", paste(features, collapse = ", "), "\n\n")

cat("Shape of X_train:", dim(X_train_scaled), "\n")
cat("Shape of X_test:", dim(X_test_scaled), "\n")

cat("\nFirst few rows of scaled training data:\n")
print(head(X_train_scaled))

na_counts <- colSums(is.na(X_train_scaled))
na_cols <- na_counts[na_counts > 0]
cat("\nColumns with NA values in X_train_scaled:\n")
print(na_cols)

inf_counts <- colSums(is.infinite(as.matrix(X_train_scaled)))
inf_cols <- inf_counts[inf_counts > 0]
cat("\nColumns witzh infinite values in X_train_scaled:\n")
print(inf_cols)
```

# Modeli

## Logistička regresija
```{r}
train_df <- cbind(X_train_scaled, podium_finish = y_train)
train_df$podium_finish <- factor(train_df$podium_finish, levels = c("0", "1"))

set.seed(42)
majority <- train_df[train_df$podium_finish == "0", ]
minority <- train_df[train_df$podium_finish == "1", ]
minority_upsampled <- minority[sample(nrow(minority), size = nrow(majority), replace = TRUE), ]
train_balanced <- rbind(majority, minority_upsampled)

lr_model <- glm(podium_finish ~ ., data = train_balanced, family = binomial)
lr_pred_prob <- predict(lr_model, newdata = X_test_scaled, type = "response")
lr_pred <- factor(ifelse(lr_pred_prob >= 0.5, 1, 0), levels = levels(y_test))

conf_mat_lr <- confusionMatrix(lr_pred, y_test, positive = "1")

cat("Logistic Regression Results:\n")
cat(sprintf("Accuracy: %.2f%%\n", conf_mat_lr$overall['Accuracy']*100))
print(conf_mat_lr$byClass)
```

## K-Nearest Neighbours
```{r}
f1_summary <- function(data, lev = NULL, model = NULL) {
  F1 <- F1_Score(y_true = data$obs, y_pred = data$pred, positive = "1")
  c(F1 = F1)
}

train_control <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = f1_summary,
  classProbs = FALSE,
  savePredictions = "final"
)

tune_grid <- expand.grid(k = seq(3, 15, by = 2))

knn_fit <- train(
  podium_finish ~ ., 
  data = train_df,
  method = "knn",
  tuneGrid = tune_grid,
  trControl = train_control,
  metric = "F1"
)

cat("\nNajbolji KNN parametri:\n")
print(knn_fit$bestTune)

knn_pred <- predict(knn_fit, newdata = X_test_scaled)
conf_mat_knn <- confusionMatrix(knn_pred, factor(y_test, levels = c("0", "1")), positive = "1")

cat("\nKNN Rezultati:\n")
cat(sprintf("Accuracy: %.2f%%\n", conf_mat_knn$overall["Accuracy"] * 100))
print(conf_mat_knn$byClass)
```

## Random Forest
```{r}
train_df_rf <- cbind(X_train_scaled, podium_finish = y_train)
train_df_rf$podium_finish <- factor(train_df_rf$podium_finish, levels = c("0", "1"), labels = c("no", "yes"))
y_test_factor <- factor(y_test, levels = c("0", "1"), labels = c("no", "yes"))

train_control_rf <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  verboseIter = FALSE
)

tune_grid_rf <- expand.grid(
  mtry = c(1, 2, 3),
  splitrule = "gini",
  min.node.size = 1
)

rf_fit <- train(
  podium_finish ~ ., 
  data = train_df_rf,
  method = "ranger",
  tuneGrid = tune_grid_rf,
  trControl = train_control_rf,
  metric = "ROC"
)

cat("\nNajbolji Random Forest parametri:\n")
print(rf_fit$bestTune)

rf_pred <- predict(rf_fit, newdata = X_test_scaled)
conf_mat_rf <- confusionMatrix(rf_pred, y_test_factor, positive = "yes")

cat("\nRandom Forest Rezultati:\n")
cat(sprintf("Accuracy: %.2f%%\n", conf_mat_rf$overall["Accuracy"] * 100))
print(conf_mat_rf$byClass)
```

## Evaluacija modela
```{r}
models <- list(
  "Logistic Regression" = lr_model,
  "KNN" = knn_fit,
  "Random Forest" = rf_fit
)

for (name in names(models)) {
  pred <- switch(name,
                 "Logistic Regression" = {
                   prob <- predict(models[[name]], newdata = X_test_scaled, type = "response")
                   factor(ifelse(prob >= 0.5, "1", "0"), levels = c("0", "1"))
                 },
                 "KNN" = predict(models[[name]], newdata = X_test_scaled),
                 "Random Forest" = {
                   p <- predict(models[[name]], newdata = X_test_scaled)
                   factor(ifelse(p == "yes", "1", "0"), levels = c("0", "1"))
                 }
  )
  
  conf_mat <- confusionMatrix(pred, y_test, positive = "1")
  cat(sprintf("\n%s Accuracy: %.4f\n", name, conf_mat$overall["Accuracy"]))
  print(conf_mat$byClass)
}
```

```{r}
library(gridExtra)

plot_conf_matrix <- function(conf_mat, title) {
  cm_table <- as.data.frame(conf_mat$table)
  cm_table$Prediction <- factor(cm_table$Prediction, levels = rev(levels(cm_table$Prediction)))
  
  ggplot(cm_table, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), color = "white", size = 6) +
    scale_fill_gradient(low = "steelblue", high = "darkblue") +
    labs(title = title, x = "Stvarna Klasa", y = "Predviđena Klasa") +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
}

p1 <- plot_conf_matrix(conf_mat_lr, "Logistička Regresija")
p2 <- plot_conf_matrix(conf_mat_knn, "KNN")
p3 <- plot_conf_matrix(conf_mat_rf, "Random Forest")

grid.arrange(p1, p2, p3, nrow = 1)
```

```{r}
library(pROC)

lr_probs <- predict(lr_model, newdata = X_test_scaled, type = "response")
roc_lr <- roc(response = as.numeric(as.character(y_test)), predictor = lr_probs)

knn_probs <- predict(knn_fit, newdata = X_test_scaled, type = "prob")[, "1"]
roc_knn <- roc(response = as.numeric(as.character(y_test)), predictor = knn_probs)

rf_probs <- predict(rf_fit, newdata = X_test_scaled, type = "prob")[, "yes"]
roc_rf <- roc(response = as.numeric(as.character(y_test)), predictor = rf_probs)

plot(roc_lr, col = "blue", lwd = 2, main = "ROC Krive za Modele")
plot(roc_knn, col = "green", lwd = 2, add = TRUE)
plot(roc_rf, col = "red", lwd = 2, add = TRUE)
legend("bottomright", legend = c("Logistička Regresija", "KNN", "Random Forest"),
       col = c("blue", "green", "red"), lwd = 2)
```
## Važnost varijabli
```{r}
library(vip)
library(ranger)             
rf_ranger <- ranger(
  formula = podium_finish ~ ., 
  data = train_df_rf,
  importance = 'impurity',
  probability = TRUE
)

importance_values <- rf_ranger$variable.importance
print(importance_values)

importance_df <- data.frame(
  Feature = names(importance_values),
  Importance = importance_values
)

library(ggplot2)
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance iz Ranger Random Forest modela",
       x = "Feature",
       y = "Importance") +
  theme_minimal()
```
