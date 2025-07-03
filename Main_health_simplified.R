### - ZOOTHERAPEUTIC PRACTICES & HEALTH: META-ANALYSIS - ###

# This code was written by Léa Fourchault (Royal Belgian Institute of Natural Sciences), Abdallah Lamane (Université Paris-Saclay & CentraleSupélec - France), and Dimitri Romaric Nguinwa Mbakop (Uni Firenze - Italy).

# updated on 06/06/2025

# Load packages

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
library(gtsummary)
library(lmtest)
library(rstatix)
library(ggpubr)
library(writexl)

###--------------------------------------------------

### - DATA CURATION - RISK SCORE - ###

# Load file
data_df <- read_excel("fakepath/Zootherapy_final.xlsx", 
                      sheet = "Data")

## Apply the Python function 'f_Data_transformation' (available on Github) to obtain the risk score based on raw data.Save as Data_Risk.

## - ANALYSIS - HEALTH IMPACTS - ##

## - Preliminary data cleaning - ##

# Load file
Data_Risk <- read_excel("fakepath/Data_Risk.xlsx")

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

## Demography ##

Data_Risk <- cbind(Data_Risk, sub_score = rowSums(Data_Risk[, c("Phyl_score", "Soc_score", "Tissue_score", "Treat_score")], na.rm = TRUE))

# By demographic categories

# Quick visual

ggplot(Data_Risk, aes(x = Recipient, y = `sub_score`, fill = Recipient)) +
  geom_boxplot() +
  labs(title = "Risk score by recipient category",
       x = "Recipient category", y = "Risk score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Kruskal-Wallis test by Recipient
kw_recipient <- kruskal.test(`sub_score` ~ Recipient, data = Data_Risk)
print(kw_recipient) # Kruskal-Wallis chi-squared = 11.392, df = 4, p-value = 0.02249

# Pairwise comparisons if Kruskal-Wallis is significant

Data_Risk%>%
  dunn_test(`sub_score` ~ Recipient, p.adjust.method = "bonferroni")

#.y.       group1                  group2    n1    n2 statistic       p  p.adj p.adj.signif
#* <chr>     <chr>                   <chr>  <int> <int>     <dbl>   <dbl>  <dbl> <chr>       
#  1 sub_score Physically sick adult   Physi…  1770    56    2.95   0.00318 0.0318 *           
#  2 sub_score Physically sick adult   Pregn…  1770    76    0.0414 0.967   1      ns          
#3 sub_score Physically sick adult   Seemi…  1770   473   -0.258  0.796   1      ns          
#4 sub_score Physically sick adult   Seemi…  1770    50    1.61   0.108   1      ns          
#5 sub_score Physically sick child   Pregn…    56    76   -2.25   0.0247  0.247  ns          
#6 sub_score Physically sick child   Seemi…    56   473   -2.93   0.00342 0.0342 *           
 # 7 sub_score Physically sick child   Seemi…    56    50   -0.873  0.383   1      ns          
#8 sub_score Pregnant or lactating … Seemi…    76   473   -0.147  0.883   1      ns          
#9 sub_score Pregnant or lactating … Seemi…    76    50    1.24   0.215   1      ns          
#10 sub_score Seemingly physically h… Seemi…   473    50    1.64   0.101   1      ns 

# Create df
stat_test_detail <- Data_Risk %>%
  dunn_test(`sub_score` ~ Recipient, p.adjust.method = "bonferroni")

# Export tibble to Excel
write_xlsx(stat_test_detail, "pairwise_results_Recip_Details.xlsx")

# Boxplot with significance annotations # https://www.datanovia.com/en/blog/ggpubr-how-to-add-p-values-generated-elsewhere-to-a-ggplot/

# Group Recipients for better visibility
Data_Risk_GP <- Data_Risk %>%
  mutate(Recipient_Grouped = case_when(
    Recipient %in% c("Seemingly physically healthy adult", "Physically sick adult") ~ "Adult",
    Recipient %in% c("Seemingly physically healthy child", "Physically sick child") ~ "Child",
    Recipient == "Pregnant or lactating people" ~ "Pregnant or lactating",
    TRUE ~ as.character(Recipient)  # In case there are other unexpected categories
  ))

# Kruskal-Wallis test by Recipient
kw_recipient <- kruskal.test(`sub_score` ~ Recipient_Grouped, data = Data_Risk_GP)
print(kw_recipient) # Kruskal-Wallis chi-squared = 10.563, df = 2, p-value = 0.005084

# Pairwise comparisons if Kruskal-Wallis is significant

Data_Risk_GP%>%
  dunn_test(`sub_score` ~ Recipient_Grouped, p.adjust.method = "bonferroni")

# A tibble: 3 × 9
#.y.       group1 group2                   n1    n2 statistic       p   p.adj p.adj.signif
#* <chr>     <chr>  <chr>                 <int> <int>     <dbl>   <dbl>   <dbl> <chr>       
 # 1 sub_score Adult  Child                  2243   106    3.25   0.00116 0.00347 **          
  #2 sub_score Adult  Pregnant or lactating  2243    76    0.0657 0.948   1       ns          
#3 sub_score Child  Pregnant or lactating   106    76   -2.10   0.0359  0.108   ns   

# Run Dunn test with Bonferroni on sub_score by Recipient
stat_test <- Data_Risk_GP %>%
  dunn_test(sub_score ~ Recipient_Grouped, p.adjust.method = "bonferroni") %>%
  add_xy_position(x = "Recipient_Grouped")

# Export tibble to Excel
write_xlsx(stat_test, "pairwise_results_Recip.xlsx")

# Create the boxplot without mean dots
final_plot <- ggboxplot(Data_Risk_GP, x = "Recipient_Grouped", y = "sub_score",
                        fill = "Recipient_Grouped", palette = "Set2",
                        color = "black",  # Restore boxplot borders
                        width = 0.6) +
  
  stat_pvalue_manual(
    stat_test,
    y.position = 20, step.increase = 0.1,
    label = "p.adj.signif",
    tip.length = 0.01,
    size = 5
  ) +
  
  labs(title = "Risk score for each patient category",
       x = "Patient category",
       y = "Risk score") +
  
  annotate("text", x = 1, y = 26, hjust = 0,
           label = "Kruskal-Wallis test with Bonferroni-adjusted post hoc comparisons",
           size = 4, fontface = "italic") +
  
  theme_pubr(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(final_plot)

# Save as high-resolution PNG
ggsave("RiskScore_Boxplot_Recipient.png", plot = final_plot,
       width = 8, height = 6, dpi = 300)
  
## Geography ##

# List countries
unique_countries <- unique(Data_Risk$Country)
print(unique_countries)

# Cluster by geographic region

Data_Risk <- Data_Risk %>%
  dplyr::mutate(Geog = case_when(
    Country %in% c('Sierra Leone', 'Togo', 'Ghana', 'Nigeria', 'Benin', 'The Gambia', 'Burkina Faso') ~ 'Western Africa', #spelling aW to make it ref group
    Country %in% c('Cameroon', 'Democratic Republic of the Congo') ~ 'Central Africa',
    Country %in% c('Angola', 'Namibia', 'Swaziland', 'Zimbabwe','Botswana', 'South Africa') ~ 'Southern Africa',
    Country %in% c('Kenya', 'Uganda', 'Ethiopia', 'Mauritius', 'Tanzania', 'Sudan') ~ 'Eastern Africa', 
    Country %in% c('Morocco', 'Algeria') ~ 'Northern Africa', ##add aa before word to make it ref group when checking diff between eastern and northern Africa
    TRUE ~ 'Multigeog'  # for one study spanning multiple regions, only 14 data points out of >2000
  ))

Data_Risk <- Data_Risk %>% filter(Geog != "Multigeog")

geog_counts <- table(Data_Risk$Geog)
print(geog_counts)

# Mean, median, SD

risk_by_geog <- Data_Risk %>%
  group_by(Geog) %>%
  summarise(
    mean_risk = mean(Total_Score, na.rm = TRUE),
    median_risk = median(Total_Score, na.rm = TRUE),
    sd_risk = sd(Total_Score, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(mean_risk))  # Sort by highest mean risk

print(risk_by_geog, n = 6)
geog_counts <- table(Data_Risk$Country)
print(geog_counts)

mean_score <- mean(Data_Risk$Total_Score, na.rm = TRUE)
var_score <- var(Data_Risk$Total_Score, na.rm = TRUE)

print(paste("Mean:", mean_score, "Variance:", var_score)) "Mean: 13.9987233477295 Variance: 7.84611613455586"

# Quick plot

ggplot(Data_Risk, aes(x = Geog, y = `Total_Score`, fill = Geog)) +
  geom_boxplot() +
  labs(title = "Total risk score by geographic region",
       x = "Region", y = "Total risk score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Kruskal-Wallis test by Geog
kw_geog <- kruskal.test(`Total_Score` ~ Geog, data = Data_Risk)
print(kw_geog) # Kruskal-Wallis chi-squared = 332.82, df = 4, p-value < 2.2e-16

# Run Dunn test with Bonferroni on Score by Geog
stat_test2 <- Data_Risk %>%
  dunn_test(Total_Score ~ Geog, p.adjust.method = "bonferroni") %>%
  add_xy_position(x = "Geog")

# Export tibble to Excel
write_xlsx(stat_test2, "pairwise_results_Geog.xlsx")

# Adjust to plot
Data_Risk$Geog <- factor(Data_Risk$Geog, levels = c(
  "Central Africa", "Eastern Africa", "Northern Africa", 
  "Southern Africa", "Western Africa"
))

stat_test2 <- dunn_test(Total_Score ~ Geog, data = Data_Risk, p.adjust.method = "bonferroni") %>%
  add_xy_position(x = "Geog")

# Create the boxplot without mean dots
final_plot2 <- ggboxplot(Data_Risk, x = "Geog", y = "Total_Score",
                        fill = "Geog", palette = "Set2",
                        color = "black",  # Restore boxplot borders
                        width = 0.6) +
  
  stat_pvalue_manual(
    stat_test2,
    y.position = 25, step.increase = 0.05,
    label = "p.adj.signif",
    tip.length = 0.01,
    size = 3
  ) +
  
  labs(title = "Risk score for each geographic region",
       x = "Geographic region",
       y = "Risk score") +
  
  annotate("text", x = 1, y = 0, hjust = 0,
           label = "Kruskal-Wallis test with Bonferroni-adjusted post hoc comparisons",
           size = 4, fontface = "italic") +
  
  theme_pubr(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(final_plot2)

# Save as high-resolution PNG
ggsave("RiskScore_Boxplot_Geog.png", plot = final_plot2,
       width = 8, height = 6, dpi = 300)

###### - Plot: Risk score per country map - #####

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

