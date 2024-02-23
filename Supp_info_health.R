### - ZOOTHERAPEUTIC PRACTICES & HEALTH: META-ANALYSIS - SUPP INFO - ###

# This code was written by Abdallah Lamane CUniversité Paris-Saclay, Central Supélec), Dimitri Romaric Nguinwa Mbakop (Uni Firenze/ZHAW Switzerland) and Léa Fourchault (RBINS Belgium).
# Contact: lfourchault@naturalsciences 

## Packages

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

# Load file
Data_Risk <- read_excel("Documents/Risk_Final.xlsx")

# Rename the columns to match your specified column names
colnames(Data_Risk) <- c('Country',	'Province',	'Animal Class',	'Animal Order',	'Phylogenetic Category',	'Common Name',	'Scientific Name',	'Social',	'Ailment Treated',	'Recipient',	'Disease Category',	'Tissue Category',	'Treatment Category',	'IUCN',	'Date',	'Authors',	'Focus', 'Phyl_score', 'Soc_score', 'Tissue_score', 'Treat_score', 'Recip_score', 'Total_Score')

# Print the first few rows of the DataFrame to verify the changes
head(Data_Risk)

# PLOTTING SPIDER CHARTS: FIG S4

# Load necessary libraries
library(tidyverse)
library(fmsb)

# Sort the DataFrame by Total Risk Score in descending order
df <- Data_Risk %>%
  arrange(desc(`Total_Score`))

# Get the top 10 high-risk practices and low-risk practices
p <- 10
top_high_risk_practices <- df[1:p, ]
top_low_risk_practices <- df[(nrow(df) - p + 1):nrow(df), ]
all <- df

# Define the risk categories
risk_categories <- c('Phyl_score', 'Soc_score', 'Tissue_score',
                     'Treat_score', 'Recip_score')

high_risk_scores <- top_high_risk_practices[, risk_categories]

# Add the rows from top_low_risk_practices to low_risk_scores
low_risk_scores <- top_low_risk_practices[, risk_categories]
all <- all[, risk_categories]

# Create labels for the radar chart
labels <- c('Phylogenetic', 'Social', 'Tissue', 'Treatment', 'Recipient')

# Convert high_risk_scores and df to matrices
high_risk_matrix <- as.matrix(high_risk_scores)
# Convert the result back to a data frame
high_risk_scores <- as.data.frame(high_risk_matrix)

# Convert high_risk_scores and df to matrices
low_risk_matrix <- as.matrix(low_risk_scores)
# Convert the result back to a data frame
low_risk_scores <- as.data.frame(low_risk_matrix)

all_matrix <- as.matrix(all)
all_scores <- as.data.frame(all_matrix)

high_risk_scores <- rbind(rep(5,5) , rep(0,5) , high_risk_scores)
low_risk_scores <- rbind(rep(5,5) , rep(0,5) , low_risk_scores)
all_scores <- rbind(rep(5,5), rep(0,5), all_scores)

# Set a single color for all polygons
single_color <- alpha("#00008B", 0.1/1.1)
single_color2 <- alpha("#00008B", 0.1/7)

# Increase the size of axis labels manually
par(cex.axis = 1.2)

#Save the plot as a PDF file
pdf("radar_chart_all_spider_chart_general.pdf", width = 8, height = 6)  # Adjust width and height as needed
spider1 <- radarchart(all_scores,
                      axistype = 1,
                      pfcol = single_color2,  # Set the same color for all polygons
                      pcol = single_color2,  # Set the same color for the grid lines
                      plwd = 0.01,
                      plty= 0.1,
                      pch = NA,
                      axislabcol = 'black',  # Set axis label color to black
                      cglwd = 1,  # Increase the grid line width
                      vlcex = 1.1,
                      title = 'Risk score for all practices',
                      caxislabels = seq(1, 5, 1)
)
dev.off()

# Increase the size of axis labels manually
par(cex.axis = 1.2)
#Save the plot as a PDF file
pdf("radar_chart_highest_spider_chart_general.pdf", width = 8, height = 6)  # Adjust width and height as needed
spider2 <- radarchart(high_risk_scores,
                      axistype = 1,
                      pfcol = single_color,  # Set the same color for all polygons
                      pcol = single_color,  # Set the same color for the grid lines
                      plwd = 0.01,
                      plty= 0.1,
                      pch = NA,
                      axislabcol = 'black',  # Set axis label color to black
                      cglwd = 1,  # Increase the grid line width
                      vlcex = 1.1,
                      title = 'Top 10 highest risk score practices',
                      caxislabels = seq(1, 5, 1)
)
dev.off()

# Set a single color for all polygons
single_color <- alpha("#008000", 0.1/1.1)

# Increase the size of axis labels manually
par(cex.axis = 1.2)

#Save the plot as a PDF file
pdf("radar_chart_lowest_spider_chart_general.pdf", width = 8, height = 6)  # Adjust width and height as needed
spider3 <-radarchart(low_risk_scores,
                     axistype = 1,
                     pfcol = single_color,  # Set the same color for all polygons
                     pcol = single_color,  # Set the same color for the grid lines
                     plwd = 0.01,
                     plty= 0.1,
                     pch = NA,
                     axislabcol = 'black',  # Set axis label color to black
                     cglwd = 1,  # Increase the grid line width
                     vlcex = 1.1,
                     title = 'Top 10 low risk score practices',
                     caxislabels = seq(1, 5, 1)
)
dev.off()

# END OF GENERAL PLOTTING SPIDER CHARTS

## SPIDER CHART FOR VULNERABLE POPULATION CATEGORIES ##

df <- df %>%
  filter(`Recip_score` >= 3)
df <- df %>%
  arrange(desc(`Total_Score`))

top_high_risk_practices <- df

# Define the risk categories
risk_categories <- c('Phyl_score', 'Soc_score', 'Tissue_score',
                     'Treat_score', 'Recip_score')

high_risk_scores <- top_high_risk_practices[, risk_categories]

# Create labels for the radar chart
labels <- c('Phylogenetic', 'Social', 'Tissue', 'Treatment', 'Recipient')

# Convert high_risk_scores and df to matrices
high_risk_matrix <- as.matrix(high_risk_scores)
# Convert the result back to a data frame
high_risk_scores <- as.data.frame(high_risk_matrix)

high_risk_scores <- rbind(rep(5,5) , rep(0,5) , high_risk_scores)

# Set a single color for all polygons
single_color <- alpha("#FF0000", 0.02)

# Increase the size of axis labels manually
par(cex.axis = 1.2)

pdf("radar_chart_highest_spider_chart_at_risk_population.pdf", width = 8, height = 6)  # Adjust width and height as needed
spider4 <- radarchart(high_risk_scores,
                      axistype = 1,
                      pfcol = single_color,  # Set the same color for all polygons
                      pcol = single_color,  # Set the same color for the grid lines
                      plwd = 0.01,
                      plty= 0.1,
                      pch = NA,
                      axislabcol = 'black',  # Set axis label color to black
                      cglwd = 1,  # Increase the grid line width
                      vlcex = 1.1,
                      title = 'Practices affecting vulnerable population categories',
                      caxislabels = seq(1, 5, 1)
)
dev.off()

### HISTOGRAMS: FIG S2

## 1) NON WEIGHTED

# PLOTTING HISTOGRAM OF NUMBER OF PRACTICES AS A FUNCTION OF TOTAL RISK SCORE

# Read the Excel file
df <- Data_Risk

# Create a histogram
p <- ggplot(df, aes(x = `Total_Score`)) +
  geom_histogram(binwidth = 1, fill = 'grey', color = 'black', alpha = 0.7) +
  labs(x = 'Risk score', y = 'Number of practices', title = 'Number of practices versus risk score') +
  geom_vline(xintercept = mean(df$`Total_Score`), color = 'red', linetype = 'dashed', linewidth = 1) +
  annotate("text", x = mean(df$`Total_Score`) + 2, y = 350, label = paste("Mean Score: ", round(mean(df$`Total_Score`), 1))) +
  theme_classic()

# Calculate the mean and standard deviation
mean_score <- mean(df$`Total_Score`)
std_deviation <- sd(df$`Total_Score`)

# Create a histogram plot
p_hist1 <- ggplot(df, aes(x = `Total_Score`)) +
  geom_histogram(binwidth = 1, fill = 'grey', color = 'grey', alpha = 0.7) +
  labs(x = 'Risk score', y = 'Number of practices', title = 'Number of practices versus risk score') +
  geom_vline(xintercept = mean_score, color = 'red', linetype = 'dashed', linewidth = 0.5) +
  
  # Add a text annotation for the mean score
  geom_text(aes(x = mean_score + 3, y = 350, label = paste("Mean score: ", round(mean_score, 1)), vjust = -0.5)) +
  
  # Add a text annotation for the standard deviation
  geom_text(aes(x = mean_score + 3, y = 330, label = paste("SD: ", round(std_deviation, 1)), vjust = -0.5)) +
  theme_classic()

# Print the histogram
print(p_hist1)
ggsave("histogram_total_practises.pdf", device = "pdf")

## 2) WEIGHTED

# Read the Excel file
df <- Data_Risk

# Calculate the new weighted total risk score
df <- df %>%
  mutate(Weighted_Total_Risk_Score = 0.5 * `Recip_score` +
           (`Phyl_score` + 0.5 * `Soc_score` + `Tissue_score` + `Treat_score`))

# Calculate the mean value
mean_score <- mean(df$`Weighted_Total_Risk_Score`)
std_deviation <- sd(df$`Weighted_Total_Risk_Score`)

# Create a histogram
p_hist3 <- ggplot(df, aes(x = Weighted_Total_Risk_Score)) +
  geom_histogram(binwidth = 1, fill = 'grey', color = 'grey', alpha = 0.7) +
  labs(x = 'Weighted risk score', y = 'Number of practices', title = 'Number of practices versus weighted risk score') +
  geom_vline(xintercept = mean(df$Weighted_Total_Risk_Score), color = 'red', linetype = 'dashed', size = 0.5) +
  
  # Add a text annotation for the mean score
  geom_text(aes(x = mean_score + 3, y = 400, label = paste("Mean score: ", round(mean_score, 1)), vjust = -0.5)) +
  
  # Add a text annotation for the standard deviation
  geom_text(aes(x = mean_score + 3, y = 350, label = paste("SD: ", round(std_deviation, 1)), vjust = -0.5)) +
  theme_classic()

# Print the histogram
print(p_hist3)
ggsave("histogram_weighted_total_practices.pdf", device = "pdf")

### MEAN RISK SCORE PER WHO DISEASE CATEGORY: FIG S5

# Calculate the mean Total Risk Score for each practice
mean_risk_by_practice <- aggregate(Data_Risk$`Total_Score`, by=list(Data_Risk$`Disease Category`), FUN=mean)

# Sort the practices by mean Total Risk Score in ascending order
mean_risk_by_practice <- mean_risk_by_practice[order(-mean_risk_by_practice$x), ]

# Calculate the standard deviation for each practice
std_dev_by_practice <- aggregate(Data_Risk$`Total_Score`, by=list(Data_Risk$`Disease Category`), FUN=sd)

# Merge the mean and standard deviation data
mean_risk_by_practice <- merge(mean_risk_by_practice, std_dev_by_practice, by.x="Group.1", by.y="Group.1")
colnames(mean_risk_by_practice) <- c("Disease Category", "Mean", "StdDev")

# Create the histogram with one color (sky blue)
ggplot(data = mean_risk_by_practice, aes(x = reorder(`Disease Category`, -Mean), y = Mean)) +
  geom_bar(stat = "identity", fill = "grey", color = "grey") +
  geom_errorbar(aes(ymin = Mean - StdDev, ymax = Mean + StdDev), width = 0.25) +
  labs(x = "WHO disease category", y = "Mean risk score") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1)) +
  ggtitle("Mean risk score versus ailment treated (sorted by decreasing risk)")

ggsave("histogram_mean_risk_score_per_WHO_categoy.pdf", device = "pdf")
