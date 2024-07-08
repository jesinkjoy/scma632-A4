# Load necessary libraries
library(dplyr)
library(car)  
library(ggplot2)  

# Load the dataset 
df <- read.csv('E:\\JESIN\\DOCUMENTS\\scma\\A4\\pizza_data.csv')

# Convert categorical variables to factors
df$brand <- as.factor(df$brand)
df$price <- as.factor(df$price)
df$weight <- as.factor(df$weight)
df$crust <- as.factor(df$crust)
df$cheese <- as.factor(df$cheese)
df$size <- as.factor(df$size)
df$toppings <- as.factor(df$toppings)
df$spicy <- as.factor(df$spicy)

# Set sum contrasts for categorical variables
contrasts(df$brand) <- contr.sum(length(unique(df$brand)))
contrasts(df$price) <- contr.sum(length(unique(df$price)))
contrasts(df$weight) <- contr.sum(length(unique(df$weight)))
contrasts(df$crust) <- contr.sum(length(unique(df$crust)))
contrasts(df$cheese) <- contr.sum(length(unique(df$cheese)))
contrasts(df$size) <- contr.sum(length(unique(df$size)))
contrasts(df$toppings) <- contr.sum(length(unique(df$toppings)))
contrasts(df$spicy) <- contr.sum(length(unique(df$spicy)))

# Define the model formula
model <- as.formula("ranking ~ brand + price + weight + crust + cheese + size + toppings + spicy")

# Fit the OLS model
model_fit <- lm(model, data = df)

# Print the summary of the model
summary(model_fit)

# List of conjoint attributes
conjoint_attributes <- c('brand', 'price', 'weight', 'crust', 'cheese', 'size', 'toppings', 'spicy')

# Initialize lists to store results
level_name <- list()
part_worth <- list()
part_worth_range <- c()
important_levels <- list()

# Loop through each attribute to calculate part-worths
for (item in conjoint_attributes) {
  nlevels <- length(unique(df[[item]]))
  levels <- levels(df[[item]])
  level_name[[item]] <- levels
  
  # Extract part-worths for the current attribute
  coef_names <- names(coef(model_fit))
  attribute_coef <- coef_names[grepl(item, coef_names)]
  
  new_part_worth <- coef(model_fit)[attribute_coef]
  new_part_worth <- c(new_part_worth, (-1) * sum(new_part_worth))
  
  # Ensure the part-worths are in the correct order
  part_worth[[item]] <- setNames(new_part_worth, levels)
  
  # Identify the most important level
  important_levels[[item]] <- which.max(new_part_worth)
  part_worth_range <- c(part_worth_range, max(new_part_worth) - min(new_part_worth))
}

# Calculate attribute importance
attribute_importance <- round(100 * (part_worth_range / sum(part_worth_range)), 2)

# Print results
print("-------------------------------------------------------------")
print("level name:")
print(level_name)
print("part worth:")
print(part_worth)
print("part_worth_range:")
print(part_worth_range)
print("important levels:")
print(important_levels)
print("attribute importance:")
print(attribute_importance)

# Plot the relative importance of attributes
ggplot(data.frame(Attribute = conjoint_attributes, Importance = attribute_importance), 
       aes(x = Attribute, y = Importance)) +
  geom_bar(stat = "identity") +
  labs(title = 'Relative importance of attributes', x = 'Attributes', y = 'Importance') +
  theme_minimal()

# Calculate utility for each profile
utility <- apply(df, 1, function(row) {
  sum(sapply(conjoint_attributes, function(attr) part_worth[[attr]][row[[attr]]]))
})

df$utility <- utility

# Print the profile with the highest utility score
print("The profile that has the highest utility score:")
print(df[which.max(df$utility), ])

# Print preferred levels for each attribute
for (i in conjoint_attributes) {
  print(paste("Preferred level in", i, "is ::", level_name[[i]][important_levels[[i]]]))
}
