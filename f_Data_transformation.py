

"""

Input : csv file /Users/abdallahlamane/Documents/Zootherapy_final2.xlsx
Output : same csv file with the category risk score and overall risk score

"""



import pandas as pd
import ipdb
import math

file_path = '/Users/abdallahlamane/Documents/Zootherapy_final2.xlsx'
# Load the Excel file into a pandas DataFrame
df = pd.read_excel(file_path, sheet_name = "Data")

value_treatment_cat_missing = 0
value_recipient_missing = 0
value_tissue_cat_missing = 0
value_social_missing = 0
value_philo_missing = 0

# Define dictionaries to map values to risk scores
phylogenetic_relatedness = {
    'Primates': 5,
    'Other Mammals': 4,
    'Birds': 3,
    'Reptiles': 2,
    'Actinopterygii': 2,
    'Rodents': 4,
    'Invertebrates': 1,
    'Amphibians': 2,
    'Chondrichthyes': 2,
    'Mollusca': 1,
    'Anguilliformes': 2,
    'Cypriniformes': 2,
    'Siluriformes': 2,
    'Osteichthyes': 2,
    'Varanidae': 2,
    'Fish': 2,
    'NA': value_philo_missing
}

social_behaviour = {
    3: 3,
    5: 5,
    1: 1,
    '': value_social_missing

}

tissue_category = {
    'Fat': 4,
    'Skin': 4,
    'Bones': 3,
    'Faeces': 4,
    'Scales': 1,
    'Derivatives': 1,
    'Whole': 5,
    'Blood': 5,
    'Flesh': 4,
    'Horn': 1,
    'Urine': 4,
    'Internal organ': 5,
    'Milk': 4,
    'Toes': 1,
    'Teeth': 1,  # Corrected to 1
    'Feathers': 2,
    'Eyes': 4,
    'Secretion': 4,
    'Eggs': 4,
    'Hair/Fur': 2,
    'Venom': 1,
    'Sting': 1,
    'Milk ': 4,  # Note the extra space
    'Foetus': 5,
    'Nest': 4,
    'Fin': 4,
    'Larvae': 5,
    'Shell':1,
    'Spikes':1,
    'Head':4,
    'NA': value_tissue_cat_missing
    }



treatment_category = {
    'Topical': 3,
    'Topical on wound': 5,
    'Topical on mucosa' : 4,
    'Ingestion': 3,
    'Ingestion raw': 4,
    'Ingestion altered': 3,
    'Ingestion cooked': 1,
    'Injected': 5,
    'Injection': 5,
    'Inhalation': 4,
    'Inhalation Altered': 2,
    'Spraying/pouring': 4,
    'Unclear': value_treatment_cat_missing,
    'Others': 1,
    'NA': value_tissue_cat_missing
}

recipient_human = {
    'Physically sick adult': 2,
    'Seemingly physically healthy adult': 1,
    'Pregnant or lactating adult': 3,
    'Pregnant or lactating people' : 3,
    'Physically sick child': 5,
    'Seemingly physically healthy child': 4,
    'Unclear': value_recipient_missing
    }


# Function to calculate the risk score for a row
def calculate_risk_score(row, category):
    risk_score = 0
    # Calculate risk score for the specified category
    if category in row:
        if category=='Social':
            if row[category] in [1,5,3]:
                risk_score += row[category]
            else:
                risk_score += value_social_missing
        else:
            if pd.isna(row[category]):
                if category[1]=='r':
                    risk_score+=value_treatment_cat_missing
                elif category[1]=='e':
                    risk_score+=value_recipient_missing
                elif category[1]=='i':
                    risk_score+=value_tissue_cat_missing
                elif category[1]=='o':
                    risk_score+=value_social_missing
                else:
                    print(category)
            else:
                l = 0
                for key in category_dicts[category]:
                    key = key.strip()
                    if key in row[category] and l==0:
                        risk_score += category_dicts[category][key]
                        l+=1
    return risk_score


# Create separate columns for each risk category score
categories = ['Phylogenetic Category', 'Social', 'Tissue Category', 'Treatment Category', 'Recipient']

# Create a dictionary of dictionaries for each category
category_dicts = {
    'Phylogenetic Category': phylogenetic_relatedness,
    'Social': social_behaviour,
    'Tissue Category': tissue_category,
    'Treatment Category': treatment_category,
    'Recipient': recipient_human
}

A = df


for category in categories:
    A[category + ' Score'] = df.apply(lambda row: calculate_risk_score(row, category), axis=1)

value_treatment_cat_missing = A["Treatment Category Score"].mean()
value_recipient_missing = A["Recipient Score"].mean()
value_tissue_cat_missing = A["Tissue Category Score"].mean()
value_social_missing = A["Social Score"].mean()
value_philo_missing = A["Phylogenetic Category Score"].mean()


for category in categories:
    df[category + ' Score'] = df.apply(lambda row: calculate_risk_score(row, category), axis=1)

# Calculate the total risk score by summing up all category scores
df['Total Risk Score'] = df[
    ['Phylogenetic Category Score', 'Social Score', 'Tissue Category Score', 'Treatment Category Score',
     'Recipient Score']].sum(axis=1)
df = df.fillna("Unknown")

# Define the threshold for the number of "Unknown" values allowed in a row
threshold = 1  # Change this value as needed

# Remove rows with more than two "Unknown" values
df = df[df.apply(lambda row: row.str.count("Unknown").sum() <= threshold, axis=1)]

# Print the DataFrame with the added score columns
print(df)

# Save the DataFrame to an Excel file
df.to_excel('/Users/abdallahlamane/Documents/leadataset_with_risk_scoresESSAI2.xlsx', index=False)
print("OK")

