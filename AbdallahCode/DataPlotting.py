
"""

Each muted block plots a data plot amidst the ones in the Google Document. Unmute and run to display.

"""



"""import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Load the dataset
df = pd.read_excel('/Users/abdallahlamane/Documents/leadataset_with_risk_scoresESSAI2.xlsx')
#df = df[df['Recipient Score'] >= 3]

# Sort the DataFrame by Total Risk Score in descending order
df = df.sort_values(by='Total Risk Score', ascending=False)
p=round(10 * len(df) / 100)
# Get the top 10 high-risk practices and low-risk practices
top_high_risk_practices = df.head(p)
top_low_risk_practices = df.tail(p)

# Define the risk categories
risk_categories = ['Phylogenetic Category Score', 'Social Score', 'Tissue Category Score',
                   'Treatment Category Score', 'Recipient Score']

# Create a function to plot spider charts
def plot_spider_chart(data, title, ax, color):
    labels = np.array(risk_categories)
    values = np.array(data[risk_categories])

    angles = np.linspace(0, 2 * np.pi, len(risk_categories), endpoint=False).tolist()
    values = np.concatenate((values, [values[0]]))
    angles += angles[:1]

    ax.fill(angles, values, color, alpha=0.02)
    ax.set_xticks(angles[:-1])
    ax.set_xticklabels(labels)

    # Set the radial axis ticks within the plot area
    ax.set_rlabel_position(90)
    ax.set_yticklabels([])  # Hide radial axis labels

    ax.set_title(title)

# Create a single plot with two columns for both high-risk and low-risk practices
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8), subplot_kw=dict(polar=True))

# Plot spider charts for the top 10 high-risk practices
for index, row in top_high_risk_practices.iterrows():
    plot_spider_chart(row, f"Score of Practices (High Risk)", ax1, 'r')

# Plot spider charts for the top 10 low-risk practices
for index, row in top_low_risk_practices.iterrows():
    plot_spider_chart(row, f"Score of Practices (Low Risk)", ax2, 'g')

# Set a fixed aspect ratio to ensure the spider charts are centered and score names are fully visible
ax1.set_aspect('equal')
ax2.set_aspect('equal')

plt.tight_layout()

plt.show()"""
import ipdb

#plt.show()

"""
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Load the data
df = pd.read_excel('/Users/abdallahlamane/Documents/leadataset_with_risk_scoresESSAI2.xlsx')
df = df[df["Recipient Score"]>=3]

# Define weights
weight_HP = 0.5
weight_S = 0.5

# Calculate the New Score
df["New Score"] = (
    df['Social Score'] +
    df['Tissue Category Score'] +
    df['Treatment Category Score'] +
    df['Phylogenetic Category Score']
)

# Define the range for the histogram
score_range = np.arange(df["New Score"].min(), df["New Score"].max() + 1)

# Create a histogram of the number of practices within each integer risk score range
plt.figure(figsize=(10, 6))
plt.hist(df["New Score"], bins=score_range, color='b', edgecolor='k', alpha=0.7, label=f"Total Practices: {len(df)}")
plt.xlabel('New Score')
plt.ylabel('Number of Practices')
plt.title('Histogram of Number of Practices vs. Total Risk Score')

# Calculate and plot the mean
mean_score = df["New Score"].mean()
plt.axvline(mean_score, color='r', linestyle='dashed', linewidth=2, label=f"Mean Score: {mean_score:.2f}")

# Show the histogram with the mean line and total practices in the legend
plt.legend()
plt.show()"""




"""

import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
from shapely.affinity import scale

# Load the dataset with risk scores
df = pd.read_excel('/Users/abdallahlamane/Documents/leadataset_with_risk_scoresESSAI2.xlsx')

# Load the "110m Cultural Vectors" dataset directly from the unzipped shapefile
world = gpd.read_file('/Users/abdallahlamane/Downloads/ne_10m_admin_0_sovereignty/ne_10m_admin_0_sovereignty.shp')

# Extract Mauritius geometry from the world object
mauritius_geometry = world[world['NAME'] == 'Mauritius']['geometry'].iloc[0]

# Define a zoom factor (adjust as needed)
zoom_factor = 3

# Apply the zoom factor to Mauritius' geometry
mauritius_geometry_scaled = scale(mauritius_geometry, xfact=zoom_factor, yfact=zoom_factor, zfact=zoom_factor, origin='centroid')

# Update the geometry for Mauritius in the world object
world.loc[world['NAME'] == 'Mauritius', 'geometry'] = mauritius_geometry_scaled

# Select the relevant African countries
africa = world[(world['CONTINENT'] == 'Africa') | (world['NAME'] == 'Mauritius')]

# Mapping for country name corrections
name_mapping = {
    'Morrocco': 'Morocco',
    'Burkina Faso': 'Burkina Faso',
    'Togo': 'Togo',
    'Ghana': 'Ghana',
    'Sierra Leone': 'Sierra Leone',
    'Mauritius': 'Mauritius',
    'Nigeria': 'Nigeria',
    'Sudan': 'Sudan',
    'Uganda': 'Uganda',
    'Tanzania': 'Tanzania',
    'Ethiopia': 'Ethiopia',
    'Botswana': 'Botswana',
    'Cameroon': 'Cameroon',
    'RD Congo': 'Dem. Rep. Congo',
    'Democratic Republic of the Congo': 'Dem. Rep. Congo',
    'Sub-Saharan': 'Sub-Saharan',  # Keep as-is if necessary
    'South Africa': 'South Africa',
    'Zimbabwe': 'Zimbabwe',
    'Benin Republic': 'Benin',
    'United Republic of Tanzania': 'Tanzania',
    'Eswatini': 'eSwatini'
}

# Replace country names in the dataset
df['Country'] = df['Country'].replace(name_mapping)

# Calculate mean risk scores
mean_risk_scores = df.groupby('Country')['Total Risk Score'].mean().reset_index()

# Merge mean risk scores with African countries' geometries
africa_with_risk = africa.merge(mean_risk_scores, left_on='NAME', right_on='Country', how='left')

# Set a default color for countries without data
default_color = 'lightgray'

# Create a figure and axis
fig, ax = plt.subplots(1, 1, figsize=(15, 10))

# Plot the African countries' geometries with mean risk scores as colors
cax = africa_with_risk.plot(column='Total Risk Score', cmap='viridis_r', linewidth=0.8, ax=ax, edgecolor='0.8', legend=True, missing_kwds={'color': default_color})

# Set plot title
ax.set_title('Mean Risk Score per Country in Africa')

# Turn off the axis
ax.axis('off')

# Reduce the space between the map and the color bar
plt.subplots_adjust(right=0.8)

# Show the map
plt.show()


________



import pandas as pd
import matplotlib.pyplot as plt

# Load the dataset with risk scores
df = pd.read_excel('/Users/abdallahlamane/Documents/leadataset_with_risk_scoresESSAI.xlsx')

# Convert 'Disease Category' to numeric with NaN for non-finite values
df['Disease Category'] = pd.to_numeric(df['Disease Category'], errors='coerce')

# Round the numeric values to the nearest integer
df['Disease Category'] = df['Disease Category'].round().astype('Int64')

# Calculate the mean Total Risk Score for each practice
mean_risk_by_practice = df.groupby('Disease Category')['Total Risk Score'].mean().reset_index()

# Sort the practices by mean Total Risk Score in ascending order
mean_risk_by_practice = mean_risk_by_practice.sort_values(by='Total Risk Score', ascending=False)


# Create a histogram of the mean Total Risk Score as a function of Ailment Treated (WHO Disease Category)
plt.figure(figsize=(12, 6))
plt.bar(mean_risk_by_practice['Disease Category'], mean_risk_by_practice['Total Risk Score'], color='skyblue')
plt.xlabel('WHO Disease Category')
plt.ylabel('Mean Total Risk Score')
plt.title('Mean Total Risk Score as a Function of Ailment Treated (Sorted by Descending Risk)')
plt.xticks(rotation=45, ha='right')

# Show the histogram
plt.tight_layout()
plt.show()


import pandas as pd
import matplotlib.pyplot as plt

# Load the dataset with risk scores
df = pd.read_excel('/Users/abdallahlamane/Documents/leadataset_with_risk_scoresESSAI.xlsx')

# Filter the dataset to include only high-risk practices (adjust the threshold as needed)
high_risk_df = df

# Convert 'Disease Category' to integer with NaN for non-finite values
# Convert 'Disease Category' to numeric with NaN for non-finite values
high_risk_df['Disease Category'] = pd.to_numeric(high_risk_df['Disease Category'], errors='coerce')

# Round the numeric values to the nearest integer
high_risk_df['Disease Category'] = high_risk_df['Disease Category'].round().astype('Int64')

# Create a histogram of the number of practices as a function of WHO Disease Category
plt.figure(figsize=(12, 6))
high_risk_df['Disease Category'].value_counts().plot(kind='bar', color='skyblue')
plt.xlabel('WHO Disease Category')
plt.ylabel('Number of Practices')
plt.title('Number of Practices as a Function of WHO Disease Category')
plt.xticks(rotation=45, ha='right')

# Show the histogram
plt.tight_layout()
plt.show()


____
ipdb.set_trace()

import pandas as pd
import matplotlib.pyplot as plt

# Load the dataset with risk scores
df = pd.read_excel('/Users/abdallahlamane/Documents/leadataset_with_risk_scoresESSAI.xlsx')

# Create a histogram of the number of practices per country
plt.figure(figsize=(12, 6))
df['Country'].value_counts().plot(kind='bar', color='skyblue')
plt.xlabel('Country')
plt.ylabel('Number of Practices')
plt.title('Number of Practices per Country')
plt.xticks(rotation=45, ha='right')
plt.tight_layout()
plt.show()

ipdb.set_trace()
import pandas as pd
import matplotlib.pyplot as plt

# Load the dataset with risk scores
df = pd.read_excel('/Users/abdallahlamane/Documents/leadataset_with_risk_scoresESSAI.xlsx')

# Sort the DataFrame by 'Total Risk Score' in descending order and select the top 10 high-risk practices
top_10_high_risk = df.sort_values(by='Total Risk Score', ascending=False).head(10)

# Create a histogram of the countries represented for the top 10 high-risk practices
plt.figure(figsize=(12, 6))
top_10_high_risk['Country'].value_counts().plot(kind='bar', color='salmon')
plt.xlabel('Country')
plt.ylabel('Number of Practices (Top 10 High-Risk)')
plt.title('Number of Practices per Country (Top 10 High-Risk)')
plt.xticks(rotation=45)  # Rotate x-axis labels for better readability
plt.tight_layout()
plt.show()
ipdb.set_trace()

import pandas as pd
import matplotlib.pyplot as plt

# Load the dataset with risk scores
df = pd.read_excel('/Users/abdallahlamane/Documents/leadataset_with_risk_scoresESSAI.xlsx')

# Sort the DataFrame by 'Total Risk Score' in descending order and select the top 10 high-risk practices
top_10_high_risk = df.sort_values(by='Total Risk Score', ascending=True).head(10)

# Create a histogram of the countries represented for the top 10 high-risk practices
plt.figure(figsize=(12, 6))
top_10_high_risk['Country'].value_counts().plot(kind='bar', color='salmon')
plt.xlabel('Country')
plt.ylabel('Number of Practices (Top 10 Low-Risk)')
plt.title('Number of Practices per Country (Top 10 Low-Risk)')
plt.xticks(rotation=45)  # Rotate x-axis labels for better readability
plt.tight_layout()
plt.show()
"""

"""import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Load the dataset with risk scores
df = pd.read_excel('/Users/abdallahlamane/Documents/leadataset_with_risk_scoresESSAI.xlsx')

criteria_columns = [
    'Phylogenetic Category Score',
    'Social Score',
    'Tissue Category Score',
    'Treatment Category Score',
    'Recipient Score'
]
print(len(df))
# Calculate the total number of 2.5 category scores for each row
df['Num_2.5_Scores'] = (df[criteria_columns] == 2.5).sum(axis=1)

# Filter the rows where the number of 2.5 category scores is less than or equal to two
df = df[df['Num_2.5_Scores'] < 2]

# Reset the index of the DataFrame
df.reset_index(drop=True, inplace=True)

# Remove the 'Num_2.5_Scores' column as it was used for filtering
df.drop(columns=['Num_2.5_Scores'], inplace=True)
name_mapping = {
    'Morrocco': 'Morocco',
    'Burkina Faso': 'Burkina Faso',
    'Togo': 'Togo',
    'Ghana': 'Ghana',
    'Sierra Leone': 'Sierra Leone',
    'Mauritius': 'Mauritius',
    'Nigeria': 'Nigeria',
    'Sudan': 'Sudan',
    'Uganda': 'Uganda',
    'Tanzania': 'Tanzania',
    'Ethiopia': 'Ethiopia',
    'Botswana': 'Botswana',
    'Cameroon': 'Cameroon',
    'RD Congo': 'Dem. Rep. Congo',
    'Democratic Republic of the Congo': 'Dem. Rep. Congo',
    'Sub-Saharan': 'Sub-Saharan',  # Keep as-is if necessary
    'South Africa': 'South Africa',
    'Zimbabwe': 'Zimbabwe',
    'Benin Republic': 'Benin',
    'United Republic of Tanzania': 'Tanzania',
    'Eswatini': 'eSwatini'
}

# Replace country names in the dataset
df['Country'] = df['Country'].replace(name_mapping)
# Define the criteria columns
criteria_columns = [
    'Phylogenetic Category Score',
    'Social Score',
    'Tissue Category Score',
    'Treatment Category Score',
    'Recipient Score'
]

# Group the DataFrame by 'Country' and calculate the sum of each criteria and total risk score
grouped = df.groupby('Country')[criteria_columns + ['Total Risk Score']].sum()


# Calculate the percentage of each criteria score in the total risk score
for column in criteria_columns:
    grouped[column + ' Percentage'] = (grouped[column] / grouped['Total Risk Score']) * 100

# Create a bar chart
plt.figure(figsize=(12, 6))

# Initialize a color map for the categories
colors = plt.cm.get_cmap('coolwarm', len(criteria_columns))

# Iterate through each criteria and plot sub-bars for each country
bottom = np.zeros(len(grouped))
for i, column in enumerate(criteria_columns):
    criteria_labels = column.split(' ')[0]
    criteria_percentages = grouped[column + ' Percentage']
    plt.bar(grouped.index, criteria_percentages, label=criteria_labels, color=colors(i), bottom=bottom)
    bottom += criteria_percentages

# Add labels and a single legend
plt.xlabel('Country')
plt.ylabel('Percentage of Criteria Score in Total Risk Score')
plt.title('Percentage of Criteria Score in Total Risk Score for Each Country')
plt.xticks(rotation=45)
plt.legend(title='Criteria', loc='upper right', bbox_to_anchor=(1.15, 1))

# Set y-axis limit to 100% for each country
plt.ylim(0, 100)

plt.tight_layout()
plt.show()
"""



"""
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Load the dataset with risk scores
df = pd.read_excel('/Users/abdallahlamane/Documents/leadataset_with_risk_scoresESSAI.xlsx')

# Filter the top 10 highest risk scores
top_10_highest_risk = df.nlargest(10, 'Total Risk Score')

# Calculate the number of practices for each country
practices_per_country = top_10_highest_risk.groupby('Country').size().reset_index(name='Number of Practices')

# Get unique countries in the top 10 highest risk list
unique_countries = practices_per_country['Country'].tolist()

# Create a spider chart for the number of unique countries
num_unique_countries = len(unique_countries)

# Get the number of practices for each country
num_practices = practices_per_country['Number of Practices']

# Set the figure size
plt.figure(figsize=(8, 6))

# Create an array of angles for the spider chart
angles = np.linspace(0, 2 * np.pi, num_unique_countries, endpoint=False).tolist()
angles += angles[:1]  # Close the plot

# Plot the data
plt.polar(angles, num_practices.tolist() + [num_practices.iloc[0]], marker='o', linestyle='-', linewidth=2)

# Fill the area under the plot
plt.fill(angles, num_practices.tolist() + [num_practices.iloc[0]], alpha=0.25)

# Set the labels for each point
plt.xticks(angles[:-1], unique_countries, fontsize=10)

# Set the title
plt.title('Spider Chart for Number of Practices Among Unique Countries in Top 10 Highest Risk', fontsize=12)

# Show the plot
plt.show()
"""


"""
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Load the dataset with risk scores
df = pd.read_excel('/Users/abdallahlamane/Documents/leadataset_with_risk_scoresESSAI.xlsx')

# Filter the top 10 highest risk scores
top_10_highest_risk = df.nlargest(10, 'Total Risk Score')

# Filter the top 10 lowest risk scores
top_10_lowest_risk = df.nsmallest(10, 'Total Risk Score')

# Calculate the number of practices for each Disease Category and round to integers
practices_per_category_high = top_10_highest_risk.groupby('Disease Category').size().reset_index(name='Number of Practices')
practices_per_category_low = top_10_lowest_risk.groupby('Disease Category').size().reset_index(name='Number of Practices')
practices_per_category_high['Number of Practices'] = practices_per_category_high['Number of Practices'].astype(int)
practices_per_category_low['Number of Practices'] = practices_per_category_low['Number of Practices'].astype(int)

# Get unique Disease Categories
unique_categories_high = practices_per_category_high['Disease Category'].tolist()
unique_categories_low = practices_per_category_low['Disease Category'].tolist()

# Create a spider chart for the number of unique Disease Categories
num_unique_categories_high = len(unique_categories_high)
num_unique_categories_low = len(unique_categories_low)

# Get the number of practices for each category
num_practices_high = practices_per_category_high['Number of Practices']
num_practices_low = practices_per_category_low['Number of Practices']

# Pad the shorter list with zeros if needed to make them the same length
if num_unique_categories_high > num_unique_categories_low:
    num_practices_low = num_practices_low.tolist() + [0] * (num_unique_categories_high - num_unique_categories_low)
elif num_unique_categories_low > num_unique_categories_high:
    num_practices_high = num_practices_high.tolist() + [0] * (num_unique_categories_low - num_unique_categories_high)

# Set the figure size
plt.figure(figsize=(8, 6))

# Create an array of angles for the spider chart
angles = np.linspace(0, 2 * np.pi, num_unique_categories_high, endpoint=False).tolist()
angles += angles[:1]  # Close the plot

# Plot the data for top 10 highest risk practices
plt.polar(angles, num_practices_high + [num_practices_high[0]], marker='o', linestyle='-', linewidth=2, label='Top 10 Highest Risk')
plt.fill(angles, num_practices_high + [num_practices_high[0]], alpha=0.25)

# Plot the data for top 10 lowest risk practices
plt.polar(angles, num_practices_low + [num_practices_low[0]], marker='o', linestyle='-', linewidth=2, label='Top 10 Lowest Risk')
plt.fill(angles, num_practices_low + [num_practices_low[0]], alpha=0.25)

# Set the labels for each point with integer values
plt.xticks(angles[:-1], [str(int(val)) for val in unique_categories_high], fontsize=10)

# Set the y-axis ticks to integers
max_num_practices = max(max(num_practices_high), max(num_practices_low))
plt.yticks(np.arange(0, max_num_practices + 1, 1), fontsize=10)

# Set the title and legend
plt.title('Spider Chart for Number of Practices by Disease Category (Top 10 Highest and Lowest Risk)', fontsize=12)
plt.legend(loc='upper right', fontsize=10)

# Show the plot
plt.show()
"""

"""
import pandas as pd
import statsmodels.api as sm

# Load your dataset
df = pd.read_excel('/Users/abdallahlamane/Documents/leadataset_with_risk_scoresESSAI2.xlsx')

# Define the independent variables (categories)
independent_vars = [
    'Phylogenetic Category Score',
    'Social Score',
    'Tissue Category Score',
    'Treatment Category Score',
    'Recipient Score'
]

# Add a constant (intercept) term
df['Constant'] = 0

# Define the dependent variable (Total Risk Score)
dependent_var = 'Total Risk Score'

# Create a GLM model
model = sm.GLM(df[dependent_var], df[independent_vars + ['Constant']], family=sm.families.Poisson())

# Fit the model
results = model.fit()

# View GLM results
print(results.summary())



import pandas as pd

# Load the dataset from the Excel file
df = pd.read_excel('/Users/abdallahlamane/Documents/leadataset_with_risk_scoresESSAI2.xlsx')

# Calculate the means for the specified columns
total = df['Total Risk Score'].sum()
mean_phylogenetic = 100*df['Phylogenetic Category Score'].sum()/total
mean_social = 100*df['Social Score'].sum()/total
mean_tissue = 100*df['Tissue Category Score'].sum()/total
mean_treatment = 100*df['Treatment Category Score'].sum()/total
mean_recipient = 100*df['Recipient Score'].sum()/total
mean_total_risk = 100*df['Total Risk Score'].sum()/total

import matplotlib.pyplot as plt

# Mean percentage scores for each category
categories = ["Phylogenetic Category", "Social", "Tissue Category", "Treatment Category", "Recipient"]
mean_percentages = [mean_phylogenetic, mean_social, mean_tissue, mean_treatment, mean_recipient]

# Create the histogram
plt.figure(figsize=(10, 6))
plt.bar(categories, mean_percentages, color='skyblue')
plt.xlabel('Categories')
plt.ylabel('Mean Percentage')
plt.title('Mean Percentage of Category Scores on Total Risk Score')
plt.xticks(rotation=45)
plt.tight_layout()

# Show the histogram
plt.show()

"""