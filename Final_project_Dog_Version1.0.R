dog_data <- read.csv("C:/Users/lijia/OneDrive/桌面/Study in University Of Calgary/DATA 602/Final Project/Full_genotype_dataset.csv")

# filter data only in America
library(dplyr)
dog_USA_data <- dog_data %>%
  filter(`Region.of.origin` == "United States")

# Clean data (delete all row with blank, and keep the data only with "A1/A1", "A1/A2", "A2/A2")
# Using one of the most popular genetic disease: Degenerative Myelopathy(DM) as an example
cleaned_dog_USA_data <- dog_USA_data %>%
  filter(if_all(everything(), ~ . != "" & !is.na(.)))
cleaned_dog_USA_data <- cleaned_dog_USA_data %>%
  filter(`Degenerative.Myelopathy..DM.` %in% c("A1/A1", "A1/A2", "A2/A2"))

# make two subgroup, one is mixed breed, and the other is pure breed.
mixed_breed <- cleaned_dog_USA_data %>%
  filter(Breed == "Mixed breed")
pure_breed <- cleaned_dog_USA_data %>%
  filter(Breed != "Mixed breed")

# Two-sample test for comparing two population proportions
total_rows_pure <- nrow(pure_breed)
Observed_A2_A2_pure <- sum(pure_breed$`Degenerative.Myelopathy..DM.` == "A2/A2", na.rm = TRUE)
total_rows_mixed <- nrow(mixed_breed)
Observed_A2_A2_mixed <- sum(mixed_breed$`Degenerative.Myelopathy..DM.` == "A2/A2", na.rm = TRUE)
prop.test(c(Observed_A2_A2_pure,Observed_A2_A2_mixed),c(total_rows_pure,total_rows_mixed),
          alternative = "greater", correct = FALSE) 
# p-value < 2.2e-16 
# 95 percent confidence interval:
#  ( 0.007161377 1.000000000 ), zero not included

# Chi-square Test
# First, find each genotype counts in each subgroup
Observed_A1_A2_pure <- sum(pure_breed$`Degenerative.Myelopathy..DM.` == "A1/A2", na.rm = TRUE)
Observed_A1_A1_pure <- sum(pure_breed$`Degenerative.Myelopathy..DM.` == "A1/A1", na.rm = TRUE)
Observed_A1_A2_mixed <- sum(mixed_breed$`Degenerative.Myelopathy..DM.` == "A1/A2", na.rm = TRUE)
Observed_A1_A1_mixed <- sum(mixed_breed$`Degenerative.Myelopathy..DM.` == "A1/A1", na.rm = TRUE)
# Second, calculate proportion for each loci A1 & A2 in each subgroup
proportion_A1_pure <- (Observed_A1_A2_pure + Observed_A1_A1_pure*2)/(total_rows_pure*2)
proportion_A2_pure <- (Observed_A1_A2_pure + Observed_A2_A2_pure*2)/(total_rows_pure*2)
proportion_A1_mixed <- (Observed_A1_A2_mixed + Observed_A1_A1_mixed*2)/(total_rows_mixed*2)
proportion_A2_mixed <- (Observed_A1_A2_mixed + Observed_A2_A2_mixed*2)/(total_rows_mixed*2)
proportion_A1_pure
proportion_A2_pure
# Do Chi-square Test in pure subgroup
Expected_A1_A1_pure = (proportion_A1_pure^2)*total_rows_pure
Expected_A1_A2_pure = (2*proportion_A1_pure*proportion_A2_pure)*total_rows_pure
Expected_A2_A2_pure = (proportion_A2_pure^2)*total_rows_pure
x_pure=c(Observed_A1_A1_pure,Observed_A1_A2_pure,Observed_A2_A2_pure); p_pure=c(Expected_A1_A1_pure,Expected_A1_A2_pure,Expected_A2_A2_pure)/total_rows_pure
chisq.test(x_pure, p=p_pure) # X-squared = 528.66, df = 2, p-value < 2.2e-16

# Do Chi-square Test in mixed subgroup
Expected_A1_A1_mixed = (proportion_A1_mixed^2)*total_rows_mixed
Expected_A1_A2_mixed = (2*proportion_A1_mixed*proportion_A2_mixed)*total_rows_mixed
Expected_A2_A2_mixed = (proportion_A2_mixed^2)*total_rows_mixed
x_mixed=c(Observed_A1_A1_mixed,Observed_A1_A2_mixed,Observed_A2_A2_mixed); p_mixed=c(Expected_A1_A1_mixed,Expected_A1_A2_mixed,Expected_A2_A2_mixed)/total_rows_mixed
chisq.test(x_mixed, p=p_mixed) # X-squared = 82.63, df = 2, p-value < 2.2e-16

# Supplementary information for Expected & Observed
Expected_A1_A1_pure
Expected_A1_A2_pure
Expected_A2_A2_pure
Observed_A1_A1_pure
Observed_A1_A2_pure
Observed_A2_A2_pure

Expected_A1_A1_mixed
Expected_A1_A2_mixed
Expected_A2_A2_mixed
Observed_A1_A1_mixed
Observed_A1_A2_mixed
Observed_A2_A2_mixed
