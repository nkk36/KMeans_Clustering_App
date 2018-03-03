# Data

The data comes from the Federal Procurement Data System (FPDS) which gives government contract data on any actions valued over $3,000. I downloaded all data for FY16 and summed the contract value for every company under major sectors of industry (Agriculture, Manufacturing, Professional Services, Construction etc...).  Industry sectors were determined used NAICS codes. The data seen in this app are the percentage of revenue for each company under each major sector. Each row of the data is one company and each column is a major NAICS sector. A summary of each of the columns can also be found under the Data tab.

# Methodology

I performed K-Means clustering on the data with the number of clusters ranging from 2 to 50. You can explore each of the 49 clustering schemes on the Clusters tab. Code for this app and clustering can be found on my GitHub [here](https://github.com/nkk36/KMeans_Clustering_App). 

# Contact

If you have any questions or feedback about this app please feel free to let me email me at <nkallfa36@gmail.com>