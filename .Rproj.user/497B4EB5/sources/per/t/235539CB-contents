library(readxl)
library(dplyr)
Zootherapy_v4 <- read_excel("Zootherapy_v4.xlsx")
View(Zootherapy_v4)

#Data Cleaning
head(Zootherapy_v4)
summary(Zootherapy_v4)

# Remove rows with NA value in "Country" column
Zootherapy_v4 <- Zootherapy_v4[!is.na(Zootherapy_v4$Country), ]

# Calculate the total number of studies
total_studies <- nrow(Zootherapy_v4)

# Calculate the number of unique countries
unique_countries <- Zootherapy_v4 %>%
   distinct(Country) %>%
   nrow()

# Calculate the number of studies per country
studies_per_country <- Zootherapy_v4 %>%
  group_by(Country) %>%
  summarise(number_of_studies = n()) %>%
  arrange(desc(number_of_studies))


# Print the results
cat("Total number of studies:", total_studies, "\n")
cat("Number of unique countries:", unique_countries, "\n")
print(studies_per_country)


