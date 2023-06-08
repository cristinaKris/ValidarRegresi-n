# Load required libraries
library(conflicted)
library(tidyverse)
library(broom)
library(lmtest)
# Set conflict resolution to "error" for filter and lag functions
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
# Load data
Tabla6 <- data.frame(
  Puntaje = c(18, 15, 16, 19, 18, 12, 13, 16, 17, 14, 10, 15, 18, 12, 16, 17, 11, 17, 13, 20),
  Agujeros = c(32, 15, 21, 40, 36, 10, 14, 18, 22, 16, 6, 20, 37, 12, 23, 27, 10, 26, 16, 52)
)
# Rename columns with spaces
names(Tabla6) <- c("Puntuaje", "No. de Agujeros")
# Perform linear regression
model <- lm(`Puntuaje` ~ `No. de Agujeros`, data = Tabla6)
# Extract model summary
summary <- summary(model)
# Extract coefficients
coefficients <- tidy(model)
# Extract residuals
residuos <- resid(model)
#Variance Analysis
anova_result <- aov(`Puntuaje` ~ `No. de Agujeros`, data = Tabla6)
anova <- summary(anova_result)
# Test for normality of residuals
normality_test <- shapiro.test(residuos)
# Test for homoscedasticity
homoscedasticity_test <- bptest(model)
# Display results
cat("\nModel Summary:\n")
print(summary)
cat("\nCoefficients:\n")
print(coefficients)
cat("\nResiduals:\n")
print(residuos)
cat("Anova Summary:\n")
print(anova)
cat("\nNormality Test of Residuals:\n")
print(normality_test)
cat("\nHomoscedasticity Test:\n")
print(homoscedasticity_test)
