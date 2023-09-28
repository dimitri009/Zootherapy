import pandas as pd
import requests
from bs4 import BeautifulSoup
import re
import pandas as pd
#import ipdb


"""
Scraping research papers from Google Scholar based on the keyword list. Search across PubMed can be performed, just need to change the link structure.

Note: some websites recognizes and restrains access to engine search. What I did in that case:
- Added a time pause between each performed search (ChatGPT can do it easily)
- But sometimes they recognize the IP address that has a temporary website restaining access. In that case, I copied/pasted the code in a Google Collab notebook and ran it.
"""

def scrape_research_papers(keywords):
    papers = []
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3'
    }

    african_countries = ['Algeria', 'Angola', 'Benin', 'Botswana', 'Burkina Faso', 'Burundi', 'Cabo Verde', 'Cameroon',
                         'Central African Republic', 'Chad', 'Comoros', 'Democratic Republic of the Congo', 'Republic of the Congo',
                         'Djibouti', 'Egypt', 'Equatorial Guinea', 'Eritrea', 'Eswatini', 'Ethiopia', 'Gabon', 'Gambia', 'Ghana',
                         'Guinea', 'Guinea-Bissau', 'Ivory Coast', 'Kenya', 'Lesotho', 'Liberia', 'Libya', 'Madagascar', 'Malawi',
                         'Mali', 'Mauritania', 'Mauritius', 'Morocco', 'Mozambique', 'Namibia', 'Niger', 'Nigeria', 'Rwanda',
                         'Sao Tome and Principe', 'Senegal', 'Seychelles', 'Sierra Leone', 'Somalia', 'South Africa', 'South Sudan',
                         'Sudan', 'Tanzania', 'Togo', 'Tunisia', 'Uganda', 'Zambia', 'Zimbabwe']

    page_number = 0
    while page_number<1000:
        url = f"https://scholar.google.com/scholar?start={page_number}&q=Zoo+OR+animal+AND+health+AND+practice+AND+tradition+AND+Morocco+OR+Algeria+OR+Tunisia+OR+Libya+OR+Egypt+OR+Erythrea+OR+Djibouti+OR+Sudan+OR+Mali+OR+Chad+OR+Niger+OR+Senegal+OR+Gambia+OR+Mauritania+OR+Benin+OR+Togo+OR+Burkina+Faso+OR+Nigeria+OR+Camero&hl=en&as_sdt=0,5"
        response = requests.get(url, headers=headers)
        soup = BeautifulSoup(response.text, 'html.parser')
        #ipdb.set_trace()
        for result in soup.find_all('div', {'class': 'gs_r gs_or gs_scl'}):
            title = result.find('h3', {'class': 'gs_rt'}).text
            authors = result.find('div', {'class': 'gs_a'}).text
            if result.find('h3', {'class': 'gs_rt'}).find('a')==None:
                link=''
            else:
                link = result.find('h3', {'class': 'gs_rt'}).find('a')['href']
            description = result.find('div', {'class': 'gs_rs'}).text
            year = extract_year_from_result(result)
            if 'Brazil' not in title:
                region, country = extract_region_and_country_from_title(title, african_countries)
                papers.append({
                    'title': title,
                    'authors': authors,
                    'link': link,
                    'description': description,
                    'year': year,
                    'region': region,
                    'country': country
                })
        page_number += 10
    return papers

def extract_region_and_country_from_title(title, african_countries):
    for country in african_countries:
        if country in title:
            return 'Africa', country
    if 'Africa' in title:
        return 'Africa', None
    return None, None

def extract_year_from_result(result):
    year_element = result.find('div', {'class': 'gs_a'})
    if year_element:
        year_text = year_element.text
        year = re.search(r'\b\d{4}\b', year_text)
        if year:
            return year.group(0)
    return None



# Example usage
keywords = ['Zoo therapy animal africa']
papers = scrape_research_papers(keywords)

# Create a DataFrame from the scraped papers
df = pd.DataFrame(papers)

# Save the DataFrame to an Excel file
output_path = '../research_papers.xlsx'
df.to_excel(output_path, index=False)

print(f"Saved {len(papers)} papers to {output_path}")

