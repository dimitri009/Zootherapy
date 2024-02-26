### - ZOOTHERAPEUTIC PRACTICES & HEALTH: META-ANALYSIS - ###

# This code was written by Abdallah Lamane (Université Paris-Saclay & CentraleSupélec - France), Dimitri Romaric Nguinwa Mbakop (Uni Firenze - Italy & ZHAW - Switzerland) and Léa Fourchault (RBINS - Belgium).
# Contact: lfourchault@naturalsciences 

# Packages

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

###--------------------------------------------------

### - DATA CURATION - RISK SCORE - ###

# Load file
data_df <- read_excel("Documents/Zootherapy_final.xlsx", 
                      sheet = "Data")

## Apply the Python function 'f_Data_transformation' (available on Github) to obtain the risk score based on raw data.

## - ANALYSIS - HEALTH IMPACTS - ##

## - Preliminary data cleaning - ##

# Load file
Data_Risk <- read_excel("Documents/Risk_Final.xlsx")

# Rename the columns to match your specified column names
colnames(Data_Risk) <- c('Country',	'Province',	'Animal Class',	'Animal Order',	'Phylogenetic Category',	'Common Name',	'Scientific Name',	'Social',	'Ailment Treated',	'Recipient',	'Disease Category',	'Tissue Category',	'Treatment Category',	'IUCN',	'Date',	'Authors',	'Focus', 'Phyl_score', 'Soc_score', 'Tissue_score', 'Treat_score', 'Recip_score', 'Total_Score')

# Print the first few rows of the DataFrame to verify the changes
head(Data_Risk)
length(Data_Risk$Total_Score) # 2425

recip_counts <- table(Data_Risk$Recipient)
print(recip_counts)

# Visualize distribution of values
mean(Data_Risk$Total_Score) # 13.99598
sd(Data_Risk$Total_Score) # 2.793736

# By demographic categories

# Adults
adult_data <- Data_Risk %>%
  filter(Recipient %in% c('Physically sick adult', 'Seemingly physically healthy adult')) # seemingly physically healthy used for practices that aim at improving the psychological health of the person

length(adult_data$Total_Score) # 2243

# Children or pregnant/lactating people
child_data <- Data_Risk %>%
  filter(Recipient %in% c('Physically sick child', 'Seemingly physically healthy child', 'Pregnant or lactating people'))

length(child_data$Total_Score) # 182

## - GLM tests of Risk score ~ demographics- ##

## Overall risk score by demographics

glm_dem <- glm(Total_Score ~ Recipient, data = Data_Risk, family = gaussian)
summary(glm_dem) # all sig, child and preg higher (but auto-correlated because tot_score also includes Recip_score)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)sick adult                        14.01725    0.06297 222.610  < 2e-16 ***
 # RecipientPhysically sick child               4.17907    0.35956  11.623  < 2e-16 ***
#  RecipientPregnant or lactating people        1.06753    0.31033   3.440 0.000592 ***
 # RecipientSeemingly physically healthy adult -1.05384    0.13712  -7.686 2.20e-14 ***
#  RecipientSeemingly physically healthy child  2.63431    0.37990   6.934 5.22e-12 ***
 # ---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 7.017933)

#Null deviance: 18919  on 2424  degrees of freedom
#Residual deviance: 16983  on 2420  degrees of freedom
#AIC: 11614

## Excluding immunocompetent category from total risk score to avoid auto-correlation, new sub_score without Recip_score component

Data_Risk <- cbind(Data_Risk, sub_score = rowSums(Data_Risk[, c("Phyl_score", "Soc_score", "Tissue_score", "Treat_score")], na.rm = TRUE))

glm_sub <- glm(sub_score ~ Recipient, data = Data_Risk, family = gaussian)
summary(glm_sub) # actual higher risk for physically sick children, regardless of their immunocompetency score 12.02 + 1.17907  ±  0.35956  p = 0.00106 ** 

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                                 12.01725    0.06297 190.848  < 2e-16 ***
 # RecipientPhysically sick child             1.17907    0.35956   3.279  0.00106 ** 
  #RecipientPregnant or lactating people      0.06753    0.31033   0.218  0.82774    
#RecipientSeemingly physically healthy adult -0.05384    0.13712  -0.393  0.69460    
#RecipientSeemingly physically healthy child  0.63431    0.37990   1.670  0.09511 .  

plot(glm_sub) # all good, might not even need to check plots since glm_, not lm_

## Impact of Recipient category on each score component category

# Level of phylogenetic relatedness between animal and human recipient
glm_phyl <- glm(Phyl_score ~ Recipient, data = Data_Risk, family = gaussian)
summary(glm_phyl) # all categories significantly increase phyl_score except pregnant. Strongest increase in children.

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                                  3.23051    0.02764 116.882  < 2e-16 ***
# RecipientPhysically sick child               0.53735    0.15783   3.405 0.000673 ***
#RecipientPregnant or lactating people        0.11160    0.13622   0.819 0.412723    
#RecipientSeemingly physically healthy adult  0.37837    0.06019   6.287 3.84e-10 ***
# RecipientSeemingly physically healthy child  0.46949    0.16675   2.815 0.004910 ** 

#Null deviance: 3344.1  on 2424  degrees of freedom
#Residual deviance: 3272.2  on 2420  degrees of freedom
#AIC: 7620.4

# Level of gregariousness of the animal
glm_soc <- glm(Soc_score ~ Recipient, data = Data_Risk, family = gaussian)
summary(glm_soc) # sig lower for psychologically sick adults, others non sig.

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                                  2.85690    0.04295  66.517   <2e-16 ***
# RecipientPhysically sick child               0.20694    0.24526   0.844   0.3989    
#RecipientPregnant or lactating people       -0.08365    0.21168  -0.395   0.6927    
#RecipientSeemingly physically healthy adult -0.19948    0.09353  -2.133   0.0330 *  
# RecipientSeemingly physically healthy child  0.48693    0.25913   1.879   0.0603 .  

# Level of infectious potential of the animal tissue used
glm_tissue <- glm(Tissue_score ~ Recipient, data = Data_Risk, family = gaussian)
summary(glm_tissue) # sig higher for preg, others non sig.

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                                  3.73672    0.02987 125.099   <2e-16 ***
# RecipientPhysically sick child              -0.04029    0.17057  -0.236    0.813    
#RecipientPregnant or lactating people        0.31591    0.14721   2.146    0.032 *  
# RecipientSeemingly physically healthy adult  0.03072    0.06505   0.472    0.637    
#RecipientSeemingly physically healthy child -0.27672    0.18021  -1.536    0.125    

# Level of infectious potential of the treatment method used
glm_treat <- glm(Treat_score ~ Recipient, data = Data_Risk, family = gaussian)
summary(glm_treat) # sig higher for physically sick children, lower for others

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                                  2.19312    0.02474  88.635  < 2e-16 ***
# RecipientPhysically sick child               0.47508    0.14129   3.362 0.000785 ***
#  RecipientPregnant or lactating people       -0.27632    0.12195  -2.266 0.023545 *  
# RecipientSeemingly physically healthy adult -0.26345    0.05388  -4.889 1.08e-06 ***
#RecipientSeemingly physically healthy child -0.04539    0.14928  -0.304 0.761115    

## - GLM tests of Risk score ~ geography- ##

# List countries
unique_countries <- unique(Data_Risk$Country)
print(unique_countries)

# Cluster by geographic region

Data_Risk <- Data_Risk %>%
  dplyr::mutate(Geog = case_when(
    Country %in% c('Sierra Leone', 'Togo', 'Ghana', 'Nigeria', 'Benin', 'The Gambia', 'Burkina Faso') ~ 'Western Africa',
    Country %in% c('Cameroon', 'Democratic Republic of the Congo') ~ 'Central Africa',
    Country %in% c('Angola', 'Namibia', 'Swaziland', 'Zimbabwe','Botswana', 'South Africa') ~ 'Southern Africa',
    Country %in% c('Kenya', 'Uganda', 'Ethiopia', 'Mauritius', 'Tanzania', 'Sudan') ~ 'Aestern Africa', #spelling Ae to make it ref group
    Country %in% c('Morocco', 'Algeria') ~ 'riskNorthern Africa', ##risk to make it ref group when checking diff between eastern and norther
    TRUE ~ 'Multigeog'  # for one study spanning multiple countries
  ))

glm_geog <- glm(Total_Score ~ Geog, data = Data_Risk, family = gaussian)
summary(glm_geog)

# summary when eastern africa as ref

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          15.6223     0.1096 142.547  < 2e-16 *** ## here ref Aestern Africa
# GeogCentral Africa   -0.8178     0.2732  -2.994  0.00279 ** 
#GeogNorthern Africa  -0.3739     0.2633  -1.420  0.15573    
#GeogSouthern Africa  -2.1399     0.2302  -9.296  < 2e-16 ***
# GeogWestern Africa   -2.3534     0.1290 -18.237  < 2e-16 ***  

# summary when northen africa as ref

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          15.2484     0.2394  63.698  < 2e-16 ***
 # GeogAestern Africa    0.3739     0.2633   1.420    0.156    
#GeogCentral Africa   -0.4439     0.3463  -1.282    0.200    
# GeogSouthern Africa  -1.7660     0.3135  -5.633 1.97e-08 ***
 # GeogWestern Africa   -1.9795     0.2489  -7.953 2.76e-15 ***

geog_counts <- table(Data_Risk$Geog)
print(geog_counts)

## - Plot: Risk score per country map - ##

mean_risk_scores_df <- Data_Risk %>%
  group_by(Country) %>%
  summarise(Mean_Risk_Score = mean(`Total_Score`, na.rm = TRUE))

# Get the geometries of African countries
african_countries <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Merge the mean risk scores with African countries
map_data <- left_join(african_countries, mean_risk_scores_df, by = c("name_long" = "Country"))

# Plot the map
risk_map <- ggplot(data = map_data) +
  geom_sf(aes(fill = Mean_Risk_Score)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Mean risk score", na.value = "grey80") +
  # labs(title = "Mean risk score per Country in Africa") +
  theme_void() +
  theme(legend.position = "right", legend.direction = "vertical")

ggsave("mean_risk_score_per_country_map.pdf", device = "pdf")

## - Plot: HISTOGRAM OF CRITERIA SCORE PERCENTAGE OF TOTAL SCORE AS A FUNCTION OF THE COUNTRY - ##

df<-Data_Risk
df <- Data_Risk[Data_Risk$Country != 'Sub-Saharan', ]

# Define the criteria columns
criteria_columns <- c(
  'Phyl_score',
  'Soc_score',
  'Tissue_score',
  'Treat_score',
  'Recip_score'
)

# Calculate the percentage of each criteria score in the total risk score

df <- df %>%
  group_by(Country) %>%
  summarise(
    Phylogenetic_Category_Score_Percentage = (sum(`Phyl_score`) / sum(`Total_Score`)) * 100,
    Social_Score_Percentage = (sum(`Soc_score`) / sum(`Total_Score`)) * 100,
    Tissue_Category_Score_Percentage = (sum(`Tissue_score`) / sum(`Total_Score`)) * 100,
    Treatment_Category_Score_Percentage = (sum(`Treat_score`) / sum(`Total_Score`)) * 100,
    Recipient_Score_Percentage = (sum(`Recip_score`) / sum(`Total_Score`)) * 100
  )

library(RColorBrewer)

# Define a 5-color darker blue palette
#darker_blue_palette <- brewer.pal(6, "Blues")[c(6, 3, 2, 5, 4)]
color_palette <- c("lemonchiffon", "khaki", "darkseagreen", "powderblue", "steelblue3")

# Create a function to generate the data for the stacked bars
generate_stacked_data <- function(df) {
  df_long <- df %>%
    pivot_longer(
      cols = contains("_Percentage"),
      names_to = "Criteria",
      values_to = "Percentage"
    )
  df_long <- df_long %>% group_by(Country) %>%
    arrange(desc(Percentage))
  df_long$y_end <- cumsum(df_long$Percentage)
  df_long$y_start <- c(0, head(df_long$y_end, n = -1))
  return(df_long)
}

# Generate the data for the stacked bars
df_stacked <- generate_stacked_data(df)

# Change "Democratic Republic of the Congo" to "DRC"
df_stacked$Country[df_stacked$Country == "Democratic Republic of the Congo"] <- "DR Congo"

# Create the stacked bar plot with the darker blue palette and modified legend labels
stacked_plot <- ggplot(df_stacked, aes(x = Country, y = Percentage, fill = Criteria)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    #  values = darker_blue_palette,
    values = color_palette,
    name = "Criteria",
    labels = c("Phylogenetic", "Social", "Tissue", "Treatment", "Recipient")
  ) +
  labs(
    x = "Country",
   # y = "Contribution of each risk category to the risk score",
    #title = "Contribution of each risk category to the risk score of each country"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("histogram_criteria_score_percentage_per_country.pdf", device = "pdf")

### - METADATA ANALYSIS - ###

# Read the 'metadata' sheet into a data frame
metadata_df <- read_excel(excel_file, sheet = "Metadata")

# or
metadata_df <- read_excel("Documents/GitHub/Zootherapy/Zootherapy/Zootherapy_final.xlsx", 
                          sheet = "Metadata")

# Filter rows based on "Inclusion"
included_studies <- metadata_df[metadata_df$Inclusion == "yes" & !is.na(metadata_df$Country), ]

# As.numeric
metadata_df$`Study size` <- as.numeric(metadata_df$`Study size`)

## - Number of studies per country - ##

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

## - Plot: number of studies per country - ##

# Get African countries data
african_countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(region_un == "Africa")

# Join your grouped_data with African countries data
map_data2 <- left_join(african_countries, studies_per_country_df, by = c("name_long" = "Country"))

studies_map <- ggplot(data = map_data2) +
  geom_sf(aes(fill = Number_of_Studies), color = "grey60") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Number of studies", na.value = "grey80", breaks = c(2, 4, 6, 8, 10 )) +
  #labs(title = "A") +
  theme_void() +
  theme(legend.position = "right", legend.direction = "vertical")

# Save the plot as a PDF file
ggsave("studies_map.pdf", device = "pdf")

## - Overall study size per country - ##

# Overall study size (total number of people interviewed)
included_studies$`Study size` <- as.numeric(included_studies$`Study size`)
overall_study_size <- sum(included_studies$`Study size`, na.rm = TRUE) #5258

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

## - Plot: total study size per country - ##

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

study_size <- ggplot(data = map_data) +
  geom_sf(aes(fill = TotalStudySize), color = "grey60") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Study size", na.value = "grey80") +
  # labs(title = "A") +
  theme_void() +
  theme(legend.position = "right", legend.direction = "vertical")

# Save the plot as a PDF file
ggsave("study_size_map.pdf", device = "pdf")

## Analysis & Plot: Cumulative increase in number of new recorded practices over time - ##

library(tidyr)  # Load tidyr package

# Function to generate step plot for top N countries and all continents. Here, we used hash tags to remove code that allows to get % instead of real values
generate_step_plot <- function(top_countries) {
  # Load the dataset from the Excel file
  data <- Data_Risk
  
  # Find the minimum and maximum years in the dataset
  min_year <- min(data$Date) - 1
  max_year <- max(data$Date)
  
  # Create a dataframe with one row for each year and one column for each country
  df <- data.frame(Year = min_year:max_year)
  
  # Get the top N countries with the most practices
  top_country_names <- data %>%
    group_by(Country) %>%
    summarize(Num_Practices = n()) %>%
    arrange(desc(Num_Practices)) %>%
    slice_head(n = top_countries) %>%
    pull(Country)
  
  # Add columns for the top countries
  for (country in top_country_names) {
    df[, country] <- 0 
  }
  
  # Fill the dataframe with the number of practices for each year and country
  for (year in min_year:max_year) {
    for (country in top_country_names) {
      total_practices <- nrow(data[data$Country == country, ])
      country_practices <- nrow(data[data$Date <= year & data$Country == country, ])
      df[df$Year == year, country] <- country_practices #/ total_practices) * 100
    }
    
    total_practices <- nrow(data[data$Date <= max_year, ])
    continent_practices <- nrow(data[data$Date <= year, ])
    print((continent_practices / total_practices) * 100)
    df[df$Year == year, "All countries"] <- continent_practices #/ total_practices) * 100
  }
  
  # Reshape the dataframe for plotting
  df_long <- df %>%
    pivot_longer(cols = -Year, names_to = "Region", values_to = "Percentage")
  
  # Create a function to generate legend labels
  generate_legend_labels <- function(unique_regions) {
    labels <- character(length(unique_regions))
    for (i in seq_along(unique_regions)) {
      region <- unique_regions[i]
      if (region == "All countries") {
        total_practices <- nrow(data[data$Date <= max_year,])
        labels[i] <- paste(region, paste("(n=", total_practices, ")", sep = ""), sep = " ")
      } else {
        total_practices <- nrow(data[data$Country == region, ])
        labels[i] <- paste(region, paste("(n=", total_practices, ")", sep = ""), sep = " ")
      }
    }
    return(labels)
  }
  
  # Generate the legend labels
  legend_labels <- generate_legend_labels(unique(df_long$Region))
  
  # Create the plot
  plot <- ggplot(df_long, aes(x = Year, y = Percentage, linetype = Region)) +
    geom_step() +
    labs(title = "",
         x = "Year",
         y = "Newly recorded practices (cumulative)",
         linetype = "Region") +
    scale_linetype_manual(values = c("solid", "dashed", "dotdash", "longdash", "twodash", "solid", "11", "22", "33", "44", "55", "66", "77", "88", "99", "1111", "1212", "1313", "1414")) +
    
    theme_classic() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          legend.key.size = unit(1, "cm"),
          legend.key.width = unit(1, "cm"))
  ggsave("cumulative_index_over_time.pdf", device = "pdf")
  
  
  return(plot)
}

# Call the function with the top_countries parameter
# Plotting it for the top 10 countries with the most studies + all countries: write 'top_countries = 10'
top_countries_plot <- generate_step_plot(top_countries = 0) # to have only 'all countries' line

# Display the top_countries_plot and all_continents_plot
print(top_countries_plot)

ggsave("cumulative_index_over_time_no_percent.pdf", device = "pdf")

## - Plot: CUMULATIVE INDEX (of newly recorded practices) AS A FUNCTION OF THE NUMBER OF STUDIES PER COUNTRY ###

new_practices_added <- Data_Risk %>%
  arrange(Country, Authors, Date) %>%
  group_by(Country, Authors) %>%
  summarise(New_Practices = sum(!duplicated(c(`Animal Order`, `Tissue Category`, `Treatment Category`, `Disease Category`))))

## how many countries to display ?
p<-7

# Find the countries with the most studies
most_studies_countries <- new_practices_added %>%
  group_by(Country) %>%
  summarise(NumStudies = n_distinct(Authors)) %>%
  arrange(desc(NumStudies)) %>%
  slice(1:p)  # Adjust the number of countries as needed

# Initialize the data frame to store all data
all_data <- tibble(Country = character(), Authors = character(), New_Practices = numeric(), Cumulative_Practices = numeric(), Study_Index = integer())

# Create cumulative plots for the specified countries
for (i in 1:length(most_studies_countries$Country)) {
  country <- most_studies_countries$Country[i]
  country_data <- new_practices_added %>%
    filter(Country == country) %>%
    arrange(desc(New_Practices)) %>%
    mutate(Cumulative_Practices = cumsum(New_Practices))# * 100 / sum(New_Practices))
  
  test_row <- tibble(Country = country, Authors = "TEST", New_Practices = 0, Cumulative_Practices = 0)
  country_data <- bind_rows(test_row, country_data)
  country_data <- country_data %>%
    arrange(Cumulative_Practices) %>%
    mutate(Study_Index = row_number())
  
  all_data <- bind_rows(all_data, country_data)
}

num_shades <- 9
colors <- brewer.pal(num_shades, "Blues")[3:num_shades]


# Plot the cumulative practices for all data
p <- ggplot(all_data, aes(x = Study_Index-1, y = Cumulative_Practices, linetype = Country, color = Country)) +
  geom_step() +
  labs(
    x = "Number of studies",
    y = "Newly recorded practices (cumulative)")+
  # title = "New practices recorded with each new study")+
  #subtitle = paste("Total practices (normalised):", max(all_data$Cumulative_Practices))
  #) +
  scale_linetype_manual(values = c("solid", "dotted", "solid", "dotted", "solid", "dotted", "solid", "dotted", "33", "44", "55", "1111", "1212", "1313", "1414")) +
  scale_x_continuous(breaks = seq(1, nrow(all_data), by = 1)) +
  scale_color_manual(values = colors) +
  theme_classic()

print(p)

ggsave("cumulative_index_over_studies.pdf", device = "pdf")

## - Grouping graphics - ##

# Figure Metadata

# Combine the two plots using grid.arrange from gridExtra package
# Arrange the plots and save as a PDF in one step
pdf("figure_meta_abcd3.pdf", width = 8, height = 6)  # Open PDF device
grid.arrange(top_countries_plot, p, studies_map, study_size, ncol = 2)  # Arrange the plots
dev.off()  # Close the PDF device

# Figure Risk

# Combine the two plots using grid.arrange from gridExtra package
# Arrange the plots and save as a PDF in one step
pdf("figure_risk.pdf", width = 6, height = 8)  # Open PDF device
grid.arrange(risk_map, stacked_plot, ncol = 1)  # Arrange the plots
dev.off()  # Close the PDF device
