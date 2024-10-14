# Load libraries
library(randomForest)
library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(caret)
library(writexl)

# Load the data from the file
data <- read_delim("morfo_females_3spp.txt", delim = "\t")

# Convert the Species variable to a factor
data$Species <- as.factor(data$Species)

# Build the Random Forest model with 1000 trees
set.seed(123) # For reproducibility
rf_model <- randomForest(Species ~ VE + SC + SVL + TL, data = data, importance = TRUE, ntree = 1000)

# View the importance of the variables
var_importance <- importance(rf_model)
print(var_importance)

# Predict the values of Species
data$Predicted_Species <- predict(rf_model, data)

# Create the Confusion Matrix using caret
conf_matrix <- confusionMatrix(data = data$Predicted_Species, reference = data$Species)

# Convert the confusion matrix to a table
conf_matrix_table <- as.data.frame.matrix(conf_matrix$table)

# Calculate class error for each species: 
# (sum of misclassified instances for the species) / (total instances for the species)
conf_matrix_table$Class.Error <- 1 - diag(as.matrix(conf_matrix$table)) / rowSums(as.matrix(conf_matrix$table))

# View the resulting confusion matrix table with Class.Error
print(conf_matrix_table)

# Export the confusion matrix with class errors to an Excel file
write_xlsx(conf_matrix_table, path = "females_3spp_confusion_matrix_with_error.xlsx")

# Print message to confirm export
print("Confusion matrix with classification error exported to 'males_confusion_matrix_with_error.xlsx'")

# Optional: Plot the importance of variables (if needed for visualization)
importance_plot <- data.frame(Variable = rownames(var_importance),
                              Importance = var_importance[, 1])

ggplot(importance_plot, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance in Classification",
       x = "Variable",
       y = "Importance") +
  theme_minimal()

# Define the training control for k-fold cross-validation (k=10, for example)
train_control <- trainControl(method = "cv", number = 10)

# Train the Random Forest model using k-fold cross-validation
rf_cv_model <- train(Species ~ VE + SC + SVL + TL, data = data, method = "rf", 
                     trControl = train_control, ntree = 1000)

# Print model accuracy (from cross-validation)
print(rf_cv_model)

# Function to calculate the convex hull, only if there are at least 3 points
convex_hull <- function(df) {
  if (nrow(df) < 3) return(NULL)  # If there are less than 3 points, return NULL
  df[chull(df$TL, df$VE), ]  # Calculate the convex hull for the group
}

# Apply the convex hull function for each predicted species, handling groups with less than 3 points
hulls <- data %>%
  group_by(Predicted_Species) %>%
  group_split() %>%
  map_df(~ {
    hull <- convex_hull(.)
    if (!is.null(hull)) return(hull) else return(data.frame())  # Return empty data frame for NULL results
  })

# Generate a plot of the data with prediction (using VE vs TL as an example)
ggplot(data, aes(x = TL, y = VE, color = Predicted_Species, shape = Predicted_Species)) +
  geom_point(size = 3) + 
  geom_polygon(data = hulls, aes(x = TL, y = VE, fill = Predicted_Species), 
               color = "black", alpha = 0.3) +  # Draw the convex hull for each species
  labs(title = "Morphometric Analysis with Random Forest",
       x = "TL (Tail Length)",
       y = "VE (Ventral Scales)") +
  theme_minimal() +
  scale_color_viridis_d() +  # Automatic palette for the species
  scale_fill_viridis_d() +   # Use the same palette for filling the convex hulls
  scale_shape_manual(values = 1:length(unique(data$Predicted_Species)))  # Different symbols for each species
