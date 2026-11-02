# COP2073c - Programming Assignment 8
# Michael Carpenter
# 11/1/2026
# load zombie cookie data, run anova, and test ingredients

# Create data frame for zombie cookie data for recipes
recipeA <- c(0,4,4,5,4,1,2,3,3,5)
recipeB <- c(7,6,4,2,3,0,8,6,2,3)
recipeC <- c(1,0,1,0,NA,1,1,NA,1,0)
df <- data.frame(RecipeA = recipeA, RecipeB = recipeB, RecipeC = recipeC)

# Stack data frame and remove NA values
newdf <- na.omit(stack(df))
print("stacked data frame:")
print(newdf)

# print summary of new data frame
initial_summary <- summary(newdf)
print("initial summary:")
print(initial_summary)

# one-way analysis of variance test
anova_result <- aov(values ~ ind, data = newdf)

# run summary on the aov data and print
anova_summary <- summary(anova_result)
print("anova summary")
print(anova_summary)

# extract p-value and test the null hypothesis
pval <- anova_summary[[1]]$`Pr(>F)`[1]
print(paste("p_value is", pval))

if (pval < 0.05) {
  print("Sufficient evidence to reject the null hypothesis")
  print("Ingredient X may have an effect on the results.")
} else {
  print("Insufficient evidence to reject the null hypothesis.")
}
