library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(pander)
library(openxlsx)

## - Preliminary data cleaning - ##

# Specify the path to the Excel file
excel_file <- "Zootherapy_final.xlsx"

# List the sheets available in the Excel file
sheet_names <- excel_sheets(excel_file)

# Read the 'data' sheet into a data frame
data_df <- read_excel(excel_file, sheet = "Data") #2573 obs of 16 var

# Read the 'metadata' sheet into a data frame
metadata_df <- read_excel(excel_file, sheet = "Metadata")

# Filter rows based on "Inclusion"
included_studies <- metadata_df[metadata_df$Inclusion == "yes" & !is.na(metadata_df$Country), ]

# As.numeric
metadata_df$`Study size` <- as.numeric(metadata_df$`Study size`)


## - Metadata analysis - ##


# Number of studies included
num_studies <- nrow(included_studies) #53

# Number of unique countries
unique_countries <- unique(included_studies$Country)
num_countries <- length(unique_countries) #24, but one = sub-Sahara --> 23 distinct countries

# Number of studies per country
studies_per_country <- table(included_studies$Country)

# Convert the result to a data frame
studies_per_country_df <- data.frame(
  Country = names(studies_per_country),
  Number_of_Studies = as.numeric(studies_per_country)
)

# Save df as excel

# Create a new Excel workbook
wb1 <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb1, "studies_per_country_df") # Create a worksheet
writeData(wb1, sheet = "studies_per_country_df", x = studies_per_country_df, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb1, "studies_per_country.xlsx")

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

# Create a new Excel workbook
wb2 <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb2, "study_size_df") # Create a worksheet
writeData(wb2, "study_size_df", study_size_df, startCol = 1, startRow = 1) # Write data to the worksheet
writeData(wb2, sheet = "study_size_df", x = study_size_df, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb2, "study_size_df.xlsx")

# Frequency of target population
target_population_frequency <- table(included_studies$`Study population`) #### Each kind of population per country 

# Convert the result to a data frame
target_population_df <- data.frame(
  Target_Population = names(target_population_frequency),
  Count = as.numeric(target_population_frequency)
)

#Each study population per country
study_population_data <- included_studies %>%
  group_by(Country, `Study population`) %>%
  summarise(StudySize = sum(`Study size`, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = `Study population`, values_from = StudySize, values_fill = 0)

#add the total
study_population_data_tot <- study_population_data %>%
  mutate(Total = rowSums(select(., -Country)))

# Create a new Excel workbook
wb3 <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb3, "study_population_data_tot") 
writeData(wb3, sheet = "study_population_data_tot", x = study_population_data_tot, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb3, "study_population_data_tot.xlsx")

# Country, Study population, Count
study_population_data_2 <- included_studies %>% 
  group_by(Country, `Study population`, `Study size`) %>%
  summarise(Count = n())

# Number of practices per country
#practice_counts_per_country <- filtered_data %>%
 # group_by(Country) %>%
  #summarise(Number_of_Practices = n_distinct(paste(`Tissue Category`, `Disease Category`, `Treatment Category`, `Ailment Treated`)))

#sum(practice_counts_per_country$Number_of_Practices)
#[1] 1021

practice_counts_per_country <- data_df %>% ## this one to include all practices, even if not entirely known (e.g., mode of administration not known)
  group_by(Country) %>%
  summarise(Number_of_Practices = n_distinct(paste(`Scientific Name`,`Tissue Category`, `Disease Category`, `Treatment Category`, `Ailment Treated`)))

sum(practice_counts_per_country$Number_of_Practices) # when adding 'Scientific Name' --> FIXED
#[1] 2435

# Create a new Excel workbook
wb3b <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb3b, "practice_counts_per_country") 
writeData(wb3b, sheet = "practice_counts_per_country", x = practice_counts_per_country, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb3b, "practice_counts_per_country.xlsx")

# Filter studies published in or after 2020
studies_after_2020 <- included_studies %>%
  filter(as.numeric(Date) >= 2020)

# Calculate the number of studies
num_studies_after_2020 <- nrow(studies_after_2020) # 21

# Filter out rows where Methods is not missing
#studies_with_methods <- included_studies %>%
 # filter(!is.na(Methods))

# Calculate the number of Methods per country
#practices_per_country <- studies_with_methods %>%
 # group_by(Country) %>%
  #summarise(NumberOfMethods = n_distinct(Methods))

# Split the "Ailments treated" column by comma and create a list
ailments_list <- strsplit(data_df$`Ailment Treated`, ",")

# Flatten the list and remove leading/trailing whitespace
ailments <- trimws(unlist(ailments_list))

# Calculate the number of unique ailments
num_ailments_treated <- length(unique(ailments)) # 318

# Print the result
cat("Number of ailments treated:", num_ailments_treated, "\n")

# Create a table of ailment frequencies
ailment_frequency <- table(ailments)

# Convert the result to a data frame
top_10_ailments_df <- data.frame(
  Ailment = names(sort(ailment_frequency, decreasing = TRUE)[1:10]),
  Count = as.numeric(sort(ailment_frequency, decreasing = TRUE)[1:10])
)

# Create a new Excel workbook
wb4 <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb4, "top_10_ailments_df") 
writeData(wb4, sheet = "top_10_ailments_df", x = top_10_ailments_df, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb4, "top_10_ailments_df.xlsx")

# Filter out rows where Disease Category is not 'unknown'
known_disease_df <- data_df %>%
  filter(!is.na(`Disease Category`))

# Calculate the frequency of each disease category
disease_category_frequency <- table(known_disease_df$`Disease Category`)

# Convert the result to a data frame       
disease_category_df <- data.frame(
  Disease_Category = names(disease_category_frequency),
  Count = as.numeric(disease_category_frequency)
)

# Sort the data frame by frequency in descending order
sorted_disease_category_df <- disease_category_df[order(-disease_category_df$Count), ]

# Create a new Excel workbook
wb5 <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb5, "sorted_disease_category_df") 
writeData(wb5, sheet = "sorted_disease_category_df", x = sorted_disease_category_df, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb5, "sorted_disease_category_df.xlsx")

# Get the top 10 most common ailments treated
top_10_common_ailments <- head(sorted_disease_category_df, 10)

# Print the top 10 most common ailments treated
cat("Top 10 most common ailments treated:\n")
print(top_10_common_ailments)

# Count the number of occurrences of "unknown" in the "Ailments treated" column
num_unknown_ailments <- sum(data_df$`Ailment Treated` == "Unknown", na.rm = TRUE)

# Print the result
cat("Number of unknown ailments:", num_unknown_ailments, "\n") # 11

# Calculate the number of unique species based on Scientific name
num_species_used <- length(unique(data_df$`Scientific Name`))

# Print the result
cat("Number of species used:", num_species_used, "\n") # 528

# Calculate the frequency of each species
species_frequency <- table(data_df$`Scientific Name`)

# Convert the result to a data frame
species_df <- data.frame(
  Scientific_Name = names(species_frequency),
  Count = as.numeric(species_frequency)
)

# Sort the data frame by frequency in descending order
sorted_species_df <- species_df[order(-species_df$Count), ]

# Create a new Excel workbook
wb6 <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb6, "sorted_species_df") 
writeData(wb6, sheet = "sorted_species_df", x = sorted_species_df, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb6, "sorted_species_df.xlsx")

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
organ_category_frequency <- table(data_df$`Tissue Category`)

# Convert the result to a data frame
organ_category_df <- data.frame(
  Organ_Category = names(organ_category_frequency),
  Count = as.numeric(organ_category_frequency)
)

# Sort the data frame by frequency in descending order
sorted_organ_category_df <- organ_category_df[order(-organ_category_df$Count), ]

# Create a new Excel workbook
wb7 <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb7, "sorted_organ_category_df") 
writeData(wb7, sheet = "sorted_organ_category_df", x = sorted_organ_category_df, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb7, "sorted_organ_category_df.xlsx")

# Print the number of body parts used
cat("Number of body parts used:", nrow(sorted_organ_category_df), "\n") # 27

# Print the ranked list from most to least commonly used
cat("Rank of organ categories from most to least commonly used:\n")
print(sorted_organ_category_df)


## -	Number of ‘unknown’ diseases treated (NA in WHO Category)  - ##


# Calculate the number of 'unknown' diseases treated
num_unknown_diseases <- sum(is.na(data_df$`Disease Category`))

# Print the result
cat("Number of 'unknown' diseases treated:", num_unknown_diseases, "\n") # 11, match Unknown Ailment Treated --> correct


## -	Number of distinct practices  - ##


# Select relevant columns
selected_columns <- c("Scientific Name", "Tissue Category", "Disease Category", "Treatment Category", "Ailment Treated")

# Filter and preprocess the data to include only relevant columns and remove rows with missing values
filtered_data <- data_df %>%
  select(all_of(selected_columns)) %>%
  na.omit()

# Count the number of distinct practices
num_distinct_practices <- filtered_data %>% # 1199 practices # 1334 when including Ailment Treated in selected_columns, correct
  distinct() %>%
  nrow()

num_distinct_practices <- data_df %>% # 1342 practices # 2458 when including Ailment Treated and Scientific Name in selected_columns, correct
  distinct() %>%
  nrow()

# Print the result
cat("Number of distinct practices:", num_distinct_practices, "\n")


## - Similarity in practices across countries - ##


# Step 1: Filter and preprocess the data
filtered_data <- data_df %>%
  select(`Scientific Name`, `Tissue Category`, `Disease Category`, `Treatment Category`, `Ailment Treated`, `Country`)

# Remove rows with missing values in key columns
filtered_data <- na.omit(filtered_data)

# Create a new Excel workbook
wb8 <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb8, "filtered_data") 
writeData(wb8, sheet = "filtered_data", x = filtered_data, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb8, "filtered_data.xlsx")

# Step 2: Identify similar or identical practices, i.e., when the variables in Scientific Name, Tissue Cat, Disease Cat, and Treatment Cat are the same: same practice
similar_practices <- filtered_data %>%
  group_by(`Scientific Name`, `Tissue Category`, `Disease Category`, `Treatment Category`) %>%
  mutate(Frequency = n()) %>% # Frequency will shown the number of practices found in the dataset (i.e., number of rows that have identical variables inScientific Name, Tissue Cat, Disease Cat, and Treatment Cat)
  filter(Frequency > 1)

# Create a new Excel workbook
wb9 <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb9, "similar_practices") 
writeData(wb9, sheet = "similar_practices", x = similar_practices, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb9, "similar_practices.xlsx")

# Step 3: Create a table to display the identified practices 
table_of_practices <- similar_practices %>%
  select(`Scientific Name`, `Tissue Category`, `Disease Category`, `Treatment Category`, `Ailment Treated`, `Country`, `Frequency`) %>%
  distinct() %>%
  arrange(desc(Frequency))

#Step 4: Identify strictly identical practices
identical_practices_used_in_multiple_countries <- table_of_practices %>%
  group_by(`Scientific Name`, `Tissue Category`, `Disease Category`, `Treatment Category`, `Ailment Treated`) %>%
  mutate(NumCountries = n_distinct(Country), Countries = toString(unique(Country))) %>%
  filter(NumCountries > 1) %>%
  distinct(`Scientific Name`, `Tissue Category`, `Disease Category`, `Treatment Category`, `Ailment Treated`, Countries)

# Create a new Excel workbook
wb10 <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb10, "identical_practices_used_in_multiple_countries") 
writeData(wb10, sheet = "identical_practices_used_in_multiple_countries", x = identical_practices_used_in_multiple_countries, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb10, "identical_practices_used_in_multiple_countries.xlsx")

#Step 5: Identify similar practices, using disease category (less specific) instead of ailment treated (more specific)
similar_pract_multip_countr <- table_of_practices %>%
  group_by(`Scientific Name`, `Tissue Category`, `Disease Category`, `Treatment Category`) %>%
  mutate(NumCountries = n_distinct(Country), Countries = toString(unique(Country))) %>%
  filter(NumCountries > 1) %>%
  distinct(`Scientific Name`, `Tissue Category`, `Disease Category`, `Treatment Category`, Countries)

similar_pract_multip_countr

# Create a new Excel workbook
wb11 <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb11, "similar_pract_multip_countr") 
writeData(wb11, sheet = "similar_pract_multip_countr", x = similar_pract_multip_countr, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb11, "similar_pract_multip_countr.xlsx")

#Step 6: Identify similar practices, using disease category (less specific) instead of ailment treated (more specific) AND animal order (less specific) instead of scientific name of species

# Filter and preprocess the data to include animal order
filtered_data2 <- data_df %>%
  select(`Animal Order`, `Tissue Category`, `Disease Category`, `Treatment Category`, `Ailment Treated`, `Country`)

# Remove rows with missing values in key columns
filtered_data2 <- na.omit(filtered_data2)

# Identify similar or identical practices
similar_practices2 <- filtered_data2 %>%
  group_by(`Animal Order`, `Tissue Category`, `Disease Category`, `Treatment Category`) %>%
  mutate(Frequency = n()) %>%
  filter(Frequency > 1)

# Create a table to display the identified practices 
table_of_practices2 <- similar_practices2 %>%
  select(`Animal Order`, `Tissue Category`, `Disease Category`, `Treatment Category`, `Ailment Treated`, `Country`, `Frequency`) %>%
  distinct() %>%
  arrange(desc(Frequency))

similar_pract_multip_countr_ao <- table_of_practices2 %>%
  group_by(`Animal Order`, `Tissue Category`, `Disease Category`, `Treatment Category`) %>%
  mutate(NumCountries = n_distinct(Country), Countries = toString(unique(Country))) %>%
  filter(NumCountries > 1) %>%
  distinct(`Animal Order`, `Tissue Category`, `Disease Category`, `Treatment Category`, Countries)

similar_pract_multip_countr_ao

# Create a new Excel workbook
wb12 <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb12, "similar_pract_multip_countr_ao") 
writeData(wb12, sheet = "similar_pract_multip_countr_ao", x = similar_pract_multip_countr_ao, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb12, "similar_pract_multip_countr_ao.xlsx")


# Step 7: Optionally, create a graph
#ggplot(table_of_practices2, aes(x = reorder(Country, -Frequency), y = Frequency, fill = Country)) +
 # geom_bar(stat = "identity") +
  #labs(title = "Frequency of Similar/Identical Practices by Country/Region",
   #    x = "Country/Region",
    #   y = "Frequency") +
 # theme_minimal() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))



##### PLOTTING #### 


## - Plot 1: total study size per country - ##


grouped_data <- included_studies %>%
      group_by(Country, `Study population`) %>%
      summarise(Frequency = n(),
        TotalStudySize = sum(`Study size`, na.rm = TRUE))

# Get African countries data
african_countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(region_un == "Africa")

# Join your grouped_data with African countries data
map_data <- left_join(african_countries, grouped_data, by = c("name_long" = "Country"))

# Plot the map
ggplot(data = map_data) +
  geom_sf(aes(fill = TotalStudySize)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Total Study Size") +
  labs(title = "Total Study Size per Country") +
       #caption = "Source: Your Data Source") +
  theme_classic() +
  theme(legend.position = "bottom")

#world <- ne_countries(scale = "medium", returnclass = "sf")
#map_data <- left_join(world, grouped_data, by = c("name" = "Country"))
#ggplot(data = map_data) +
#     geom_sf(aes(fill = TotalStudySize)) +
#     scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Total Study Size") +
#     labs(title = "Total Study Size by Country",
#                     subtitle = "Focus on Africa",
#                     caption = "Source: Your Data Source") +
#     theme_minimal() +
#     theme(legend.position = "bottom")

## - Plot 2: number of studies per country - ##

map_data2 <- left_join(african_countries, studies_per_country_df, by = c("name_long" = "Country"))

# Plot the map 
ggplot(data = map_data2) +
  geom_sf(aes(fill = Number_of_Studies)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Number of Studies", labels = scales::comma, breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Number of Studies per Country") +
       #caption = "Source: Your Data Source") +
  theme_classic () +
  theme(legend.position = "bottom")

## - Plot 3: number of studies per country - ##

map_data3 <- left_join(african_countries, practice_counts_per_country, by = c("name_long" = "Country"))

# Plot the map for the number of practices per country
ggplot(data = map_data3) +
  geom_sf(aes(fill = Number_of_Practices)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Number of Practices") +
  labs(title = "Number of Recorded Practices per Country",
       caption = "Source: Your Data Source") +
  theme_classic() +
  theme(legend.position = "bottom")


### BIODIVERSITY IMPACT #####

# -	Most frequently cited phylogenetic category
ordered_categories <- data_df %>%
  group_by(`Phylogenetic Category`) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

# Top 5 species within each phylogenetic category
top_phylo_names <- data_df %>%
  group_by(`Phylogenetic Category`, `Scientific Name`) %>%
  summarise(Frequency = n()) %>%
  
  # Rank scientific names by frequency within each category
  arrange(`Phylogenetic Category`, desc(Frequency)) %>%
  group_by(`Phylogenetic Category`) %>%
  mutate(Rank = row_number()) %>%
  
  # Filter for the top 5 scientific names in each category
  filter(Rank <= 5)

# Create a new Excel workbook
wb13 <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb13, "top_phylo_names") 
writeData(wb13, sheet = "top_phylo_names", x = top_phylo_names, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb13, "top_phylo_names.xlsx")


# -	Most frequently cited animal order
ordered_categories <- data_df %>%
  group_by(`Animal Order`) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Top 5 species within each animal order
top_ao_names <- data_df %>%
  group_by(`Animal Order`, `Scientific Name`) %>%
  summarise(Count = n()) %>%
  
  # Rank scientific names by frequency within each category
  arrange(`Animal Order`, desc(Count)) %>%
  group_by(`Animal Order`) %>%
  mutate(Rank = row_number()) %>%
  
  # Filter for the top 5 scientific names in each category
  filter(Rank <= 5)

# Create a new Excel workbook
wb14 <- createWorkbook()

# Add the data frame as an Excel table
addWorksheet(wb14, "top_ao_names") 
writeData(wb14, sheet = "top_ao_names", x = top_ao_names, startCol = 1, startRow = 1)

# Save the Excel workbook to a file
saveWorkbook(wb14, "top_ao_names.xlsx")

# -	Number of endangered species 
endangered_count <- data_df %>%
  filter(`IUCN` %in% c("VU", "CR", "EN", "NT")) %>%
  distinct(`Scientific Name`) %>%
  nrow()

# non-endangered 
non_endangered_count <- data_df %>%
  filter(`IUCN` %in% c("LC", "DOM")) %>%
  distinct(`Scientific Name`) %>%
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
  filter(`IUCN` %in% c("VU", "CR", "EN", "NT")) %>%
  group_by(`Scientific Name`) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

# Get the top 10 most frequent endangered species
top_10_endangered_species <- head(sorted_endangered_species, 10)


## PLOT the 20 most cited and their frequency and level of endangerment (DOM<LC<NT<VU<EN<CR) in color scale

# Extract the top 20 most cited species
top_20_species3 <- data_df %>%
    group_by(`Scientific Name`, IUCN) %>%
    summarise(Frequency = n()) %>%
    arrange(desc(Frequency)) %>%
    head(20)

# Filter out rows with missing Scientific Name values
top_20_species3 <- top_20_species3 %>% filter(!is.na(`Scientific Name`))

# Define the color palette from green to red
colors <- c("DOM" = "royalblue3", "LC" = "dodgerblue2", "NT" = "lightslateblue", "VU" = "mediumpurple1", "EN" = "mediumorchid1", "CR" = "magenta4")

# Define the order of IUCN levels
iucn_levels <- c("DOM", "LC", "NT", "VU", "EN", "CR")

# Convert IUCN to a factor with specified levels
top_20_species3$IUCN <- factor(top_20_species3$IUCN, levels = iucn_levels)

# Create the bar plot 
ggplot(top_20_species3, aes(x = reorder(`Scientific Name`, -Frequency), y = Frequency, fill = IUCN)) +
     geom_bar(stat = "identity") +
     scale_fill_manual(values = colors) +
     labs(
           title = "Top 20 Most Used Species by IUCN Status",
           x = "Scientific Name",
           y = "Count"
       ) +
     theme_classic() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
     guides(fill = guide_legend(title = "IUCN Status"))  # Add legend

##### GLM Test ##### 

# Create a binary variable indicating if the practice requires a live animal
data_df <- data_df %>%
  mutate(RequiresLiveAnimal = ifelse(`Tissue Category` %in% c("Milk", "Feathers", "Secretion", 
                                                              "Faeces", "Urine", "Eggs", "Derivatives"), 1, 0))

# Categorize species into endangered and non-endangered
data_df$SpeciesCategory <- ifelse(data_df$`IUCN` %in% c("VU", "CR", "EN", "NT"), "Endangered",
                                  ifelse(data_df$`IUCN` %in% c("LC", "DOM"), "Non-Endangered", "Unknown"))

#data_df$SpeciesCategory <- ifelse(data_df$`IUCN` %in% c("VU", "CR", "EN", "NT"), "Endangered","Non-Endangered")

# Fit a logistic regression model
glm_result <- glm(RequiresLiveAnimal ~ SpeciesCategory, data = data_df, family = binomial)

# Create a summary table for the GLM result
summary_table <- summary(glm_result)
pander(summary_table)

# Create a graph to visualize the relationship

data_df$RequiresLiveAnimal <- factor(data_df$RequiresLiveAnimal, levels = c(0, 1), labels = c("No", "Yes"))

ggplot(data_df, aes(x = SpeciesCategory, fill = RequiresLiveAnimal)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Practices Requiring Living Animals",
       x = "IUCN Status",
       y = "Proportion") +
  scale_fill_manual(values = c("No" = "steelblue1", "Yes" = "dodgerblue3"), name = "Requires Living Animal") +
  theme_classic()
