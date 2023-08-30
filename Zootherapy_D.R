library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(pander)


# Specify the path to the Excel file
excel_file <- "Zootherapy_v5.xlsx"

# List the sheets available in the Excel file
sheet_names <- excel_sheets(excel_file)

# Read the 'data' sheet into a data frame
data_df <- read_excel(excel_file, sheet = "Data")

# Read the 'metadata' sheet into a data frame
metadata_df <- read_excel(excel_file, sheet = "Metadata")

# Filter rows based on "Inclusion"
included_studies <- metadata_df[metadata_df$Inclusion == "yes" & !is.na(metadata_df$Country), ]

#Clean
metadata_df$`Study size` <- as.numeric(metadata_df$`Study size`)

# Number of studies
num_studies <- nrow(included_studies)

# Number of unique countries
unique_countries <- unique(included_studies$Country)
num_countries <- length(unique_countries)

# Number of studies per country
studies_per_country <- table(included_studies$Country)

# Convert the result to a data frame
studies_per_country_df <- data.frame(
  Country = names(studies_per_country),
  Number_of_Studies = as.numeric(studies_per_country)
)

# Overall study size (total number of people interviewed)
included_studies$`Study size` <- as.numeric(included_studies$`Study size`)
overall_study_size <- sum(included_studies$`Study size`, na.rm = TRUE)

# Study size per country
study_size_per_country <- tapply(
  included_studies$`Study size`,
  included_studies$Country,
  FUN = function(x) sum(x, na.rm = TRUE)
)

# Convert the result to a data frame
study_size_df <- data.frame(
  Country = names(study_size_per_country),
  Study_Size = as.numeric(study_size_per_country)
)

# Frequency of target population
target_population_frequency <- table(included_studies$`Study population`) #### Each kind of population per country 

# Convert the result to a data frame
target_population_df <- data.frame(
  Target_Population = names(target_population_frequency),
  Frequency = as.numeric(target_population_frequency)
)
########## Each kind of study population per country ????
#Each study population per country
study_population_data <- included_studies %>%
  group_by(Country, `Study population`) %>%
  summarise(StudySize = sum(`Study size`, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = `Study population`, values_from = StudySize, values_fill = 0)

#add the total
study_population_data_tot <- study_population_data %>%
  mutate(Total = rowSums(select(., -Country)))

#Country, Study population, Count
study_population_data_2 <- included_studies %>% 
  group_by(Country, `Study population`, `Study size`) %>%
  summarise(Count = n())

# Filter studies published in or after 2020
studies_after_2020 <- included_studies %>%
  filter(as.numeric(Date) >= 2020)

# Calculate the number of studies
num_studies_after_2020 <- nrow(studies_after_2020)

# Filter out rows where Methods is not missing
studies_with_methods <- included_studies %>%
  filter(!is.na(Methods))

# Calculate the number of Methods per country
practices_per_country <- studies_with_methods %>%
  group_by(Country) %>%
  summarise(NumberOfPractices = n_distinct(Methods))

# Split the "Ailments treated" column by comma and create a list
ailments_list <- strsplit(data_df$`Ailments treated`, ",")

# Flatten the list and remove leading/trailing whitespace
ailments <- trimws(unlist(ailments_list))

# Calculate the number of unique ailments
num_ailments_treated <- length(unique(ailments))

# Print the result
cat("Number of ailments treated:", num_ailments_treated, "\n")

# Create a table of ailment frequencies
ailment_frequency <- table(ailments)

# Convert the result to a data frame
top_10_ailments_df <- data.frame(
  Ailment = names(sort(ailment_frequency, decreasing = TRUE)[1:10]),
  Frequency = as.numeric(sort(ailment_frequency, decreasing = TRUE)[1:10])
)

# Filter out rows where Disease Category is not 'unknown'
known_disease_df <- data_df %>%
  filter(!is.na(`Disease Category`))

# Calculate the frequency of each disease category
disease_category_frequency <- table(known_disease_df$`Disease Category`)

# Convert the result to a data frame       ################### (Problem from data set not from code)
disease_category_df <- data.frame(
  Disease_Category = names(disease_category_frequency),
  Frequency = as.numeric(disease_category_frequency)
)

# Sort the data frame by frequency in descending order
sorted_disease_category_df <- disease_category_df[order(-disease_category_df$Frequency), ]

# Get the top 10 most common ailments treated
top_10_common_ailments <- head(sorted_disease_category_df, 10)

# Print the top 10 most common ailments treated
cat("Top 10 most common ailments treated:\n")
print(top_10_common_ailments)


# Count the number of occurrences of "unknown" in the "Ailments treated" column
num_unknown_ailments <- sum(data_df$`Ailments treated` == "Unknown")

# Print the result
cat("Number of unknown ailments:", num_unknown_ailments, "\n")


# Calculate the number of unique species based on Scientific name
num_species_used <- length(unique(data_df$`Scientific name`))

# Print the result
cat("Number of species used:", num_species_used, "\n")

# Calculate the frequency of each species
species_frequency <- table(data_df$`Scientific name`)

# Convert the result to a data frame
species_df <- data.frame(
  Scientific_Name = names(species_frequency),
  Frequency = as.numeric(species_frequency)
)

# Sort the data frame by frequency in descending order
sorted_species_df <- species_df[order(-species_df$Frequency), ]

# Top 10 most commonly used species
top_10_most_common_species <- head(sorted_species_df, 10)

# Top 10 least commonly used species
top_10_least_common_species <- tail(sorted_species_df, 10)

# Print the results
cat("Top 10 most commonly used species:\n")
print(top_10_most_common_species)

cat("\nTop 10 least commonly used species:\n")
print(top_10_least_common_species)

## -	Number of body parts used & rank most to least commonly used   - ##

# Calculate the frequency of each organ category
organ_category_frequency <- table(data_df$`Organ Category`)

# Convert the result to a data frame
organ_category_df <- data.frame(
  Organ_Category = names(organ_category_frequency),
  Frequency = as.numeric(organ_category_frequency)
)

# Sort the data frame by frequency in descending order
sorted_organ_category_df <- organ_category_df[order(-organ_category_df$Frequency), ]

# Print the number of body parts used
cat("Number of body parts used:", nrow(sorted_organ_category_df), "\n")

# Print the ranked list from most to least commonly used
cat("Rank of organ categories from most to least commonly used:\n")
print(sorted_organ_category_df)


## -	Number of ‘unknown’ diseases treated (NA in WHO Category)  - ##


# Calculate the number of 'unknown' diseases treated
num_unknown_diseases <- sum(is.na(data_df$`Disease Category`))

# Print the result
cat("Number of 'unknown' diseases treated:", num_unknown_diseases, "\n")

## -	Number of distinct practices  - ##

# Select relevant columns
selected_columns <- c("Scientific name", "Organ Category", "Disease Category", "Treatment Category")

# Filter and preprocess the data to include only relevant columns and remove rows with missing values
filtered_data <- data_df %>%
  select(all_of(selected_columns)) %>%
  na.omit()

# Count the number of distinct practices
num_distinct_practices <- filtered_data %>%
  distinct() %>%
  nrow()

# Print the result
cat("Number of distinct practices:", num_distinct_practices, "\n")


# Step 1: Filter and preprocess the data
filtered_data <- data_df %>%
  select(`Scientific name`, `Organ Category`, `Disease Category`, `Treatment Category`, `Ailments treated`, `Country`)

# Remove rows with missing values in key columns
filtered_data <- na.omit(filtered_data)

# Step 2: Identify similar or identical practices
similar_practices <- filtered_data %>%
  group_by(`Scientific name`, `Organ Category`, `Disease Category`, `Treatment Category`) %>%
  mutate(Frequency = n()) %>%
  filter(Frequency > 1)

# Step 3: Create a table to display the identified practices ####### IDENTICAL PRACTICES AMONG COUNTRY AND PLOT IT !!!
#table_of_practices <- similar_practices %>%
#  select(`Scientific name`, `Organ Category`, `Disease Category`, `Treatment Category`, `Ailments treated`, `Country`, `Frequency`) %>%
#  distinct() %>%
#  arrange(desc(Frequency))

#Step 3 (Fixed)
practices_used_in_multiple_countries <- table_of_practices %>%
  group_by(`Scientific name`, `Organ Category`, `Disease Category`, `Treatment Category`, `Ailments treated`) %>%
  mutate(NumCountries = n_distinct(Country), Countries = toString(unique(Country))) %>%
  filter(NumCountries > 1) %>%
  distinct(`Scientific name`, `Organ Category`, `Disease Category`, `Treatment Category`, `Ailments treated`, Countries)

# Step 4: Optionally, create a graph
#ggplot(table_of_practices, aes(x = reorder(Country, -Frequency), y = Frequency, fill = Country)) +
#  geom_bar(stat = "identity") +
#  labs(title = "Frequency of Similar/Identical Practices by Country/Region",
#       x = "Country/Region",
#       y = "Frequency") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##### PLOT #### tot study size per country
grouped_data <- included_studies %>%
      group_by(Country, `Study population`) %>%
      summarise(Frequency = n(),
                                TotalStudySize = sum(`Study size`, na.rm = TRUE))
world <- ne_countries(scale = "medium", returnclass = "sf")
map_data <- left_join(world, grouped_data, by = c("name" = "Country"))
ggplot(data = map_data) +
     geom_sf(aes(fill = TotalStudySize)) +
     scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Total Study Size") +
     labs(title = "Total Study Size by Country",
                     subtitle = "Focus on Africa",
                     caption = "Source: Your Data Source") +
     theme_minimal() +
     theme(legend.position = "bottom")


### THEME B #####

# -	Most frequently cited phylogenetic category
ordered_categories <- data_df %>%
  group_by(`Phylogenetic category`) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

# Top 5 species within each phylogenetic category
top_scientific_names <- data_df %>%
  group_by(`Phylogenetic category`, `Scientific name`) %>%
  summarise(Frequency = n()) %>%
  
  # Rank scientific names by frequency within each category
  arrange(`Phylogenetic category`, desc(Frequency)) %>%
  group_by(`Phylogenetic category`) %>%
  mutate(Rank = row_number()) %>%
  
  # Filter for the top 5 scientific names in each category
  filter(Rank <= 5)

# -	Number of endangered species 
endangered_count <- data_df %>%
  filter(`IUCN` %in% c("VU", "CR", "EN", "DD")) %>%
  distinct(`Scientific name`) %>%
  nrow()

# non-endangered 
non_endangered_count <- data_df %>%
  filter(`IUCN` %in% c("LC", "DOM")) %>%
  distinct(`Scientific name`) %>%
  nrow()


# Perform a chi-squared test
chi_squared_test <- chisq.test(c(endangered_count, non_endangered_count))
cat("Chi-Squared Test p-value:", chi_squared_test$p.value, "\n")
#The p-value from the chi-squared test will indicate whether the difference between 
#the two groups is statistically significant. 
#If the p-value is less than a chosen significance level (e.g., 0.05), 
#it suggests a significant difference.


# - Top 10 most frequent endangered species? 
sorted_endangered_species <- data_df %>%
  filter(`IUCN` %in% c("VU", "CR", "EN", "DD")) %>%
  group_by(`Scientific name`) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

# Get the top 10 most frequent endangered species
top_10_endangered_species <- head(sorted_endangered_species, 10)

##### GLM Test ##### 

# Create another data set with a binary variable indicating if the practice requires a live animal
data_df <- data_df %>%
  mutate(RequiresLiveAnimal = ifelse(`Organ Category` %in% c("Dung", "Faeces", "Feathers", "Secretions"), 1, 0))

# Categorize species into endangered and non-endangered
data_df$SpeciesCategory <- ifelse(data_df$`IUCN` %in% c("VU", "CR", "EN", "DD"), "Endangered",
                                  ifelse(data_df$`IUCN` %in% c("LC", "DOM"), "Non-Endangered", "Other"))

# Fit a logistic regression model
glm_result <- glm(RequiresLiveAnimal ~ SpeciesCategory, data = data_df, family = binomial)

# Create a summary table for the GLM result
summary_table <- summary(glm_result)
pander(summary_table)

# Create a graph to visualize the relationship

ggplot(data_df, aes(x = SpeciesCategory, fill = factor(RequiresLiveAnimal))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Practices Requiring Live Animals",
       x = "Species Category",
       y = "Proportion") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), name = "Requires Live Animal") +
  theme_minimal()
