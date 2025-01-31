import pandas as pd

# Reading the data
data = pd.read_csv('plantData_2024-12-23.csv', low_memory=False)
df = pd.DataFrame(data)

# Initialize 
surveyDate = []
biome = []
Lat = []
Long = []
plantsize = []
plantheight = []
leaf_perc = []
plant_id = []
leaf_id = []

# Loop through rows and columns
for index, row in df.iterrows():
    j=0
    # Extract leaf percentage data from columns containing 'percLf'
    for col in df.columns:
        if 'percLf' in col:
            leaf_percent = row[col]
            if pd.notna(leaf_percent):
                leaf_perc.append(leaf_percent)
                plant_id.append(row['plantID3long'])
                surveyDate.append(row['surveyDate'])
                biome.append(row['Biome'])
                Lat.append(row['Lat'])
                Long.append(row['Long'])
                plantsize.append(row['sizeCentScale'])
                plantheight.append(row['plantHeight_cm'])
                leaf_id.append(j+1)
                j = j+1
    

# Create a DataFrame 
leaf_perc_df = pd.DataFrame({
    'plant_id': plant_id,
    'leaf_id': leaf_id,
    'leaf_percent': leaf_perc, 
    'Lat': Lat, 
    'Long': Long,
    'plant_size': plantsize,
    'plant_height': plantheight,
    'surveyDate': surveyDate,
    'biome': biome
})

leaf_perc_df['plant_leaf_id'] = leaf_perc_df['plant_id'] + '_' + leaf_perc_df['leaf_id'].astype(str)

leaf_perc_df = leaf_perc_df.reindex(columns = ['surveyDate','biome','Lat','Long','plant_size','plant_height','plant_id','leaf_id','plant_leaf_id','leaf_percent'])

# Save the pivoted DataFrame to a CSV file
leaf_perc_df.to_csv('pivoted.csv', index=False)
