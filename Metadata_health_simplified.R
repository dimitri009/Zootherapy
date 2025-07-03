### - ZOOTHERAPEUTIC PRACTICES & HEALTH: META-ANALYSIS - ###

# This code was written by Léa Fourchault (Royal Belgian Institute of Natural Sciences), Abdallah Lamane (Université Paris-Saclay & CentraleSupélec - France), and Dimitri Romaric Nguinwa Mbakop (Uni Firenze - Italy).

# updated on 06/06/2025

### - METADATA ANALYSIS - ###

# Read the 'metadata' sheet into a data frame
metadata_df <- read_excel(excel_file, sheet = "Metadata")
library(readxl)

View(Zootherapy_final)
# or
metadata_df <- Zootherapy_final

# Filter rows based on "Inclusion"
included_studies <- metadata_df[metadata_df$Inclusion == "yes" & !is.na(metadata_df$Country), ]

# As.numeric
metadata_df$`Study size` <- as.numeric(metadata_df$`Study size`)

## - Number of studies per country - ##

# Number of studies included
num_studies <- nrow(included_studies) #53

# Number of unique countries
unique_countries <- unique(included_studies$Country)
num_countries <- length(unique_countries) #24, but one = Multigeog --> 23 distinct countries

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

## - Plot: total study size per country - ##

#grouped_data <- included_studies %>%
# group_by(Country, `Study population`) %>%
#summarise(Frequency = n(),
#         TotalStudySize = sum(`Study size`, na.rm = TRUE))

# Get African countries data
african_countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(region_un == "Africa")

# Join your grouped_data with African countries data
#map_data <- left_join(african_countries, grouped_data, by = c("name_long" = "Country"))
map_data <- left_join(african_countries, study_size_df, by = c("name_long" = "Country"))

# Plot the map

study_size <- ggplot(data = map_data) +
  geom_sf(aes(fill = Study_Size), color = "grey60") +
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
  # scale_color_manual(values = colors) +
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
