Obtained files from SF city data webpage located at:

https://data.sfgov.org/Public-Safety/Police-Department-Incident-Reports-2018-to-Present/wg3w-h783
These are files from October 30th, 2023

wget -P data/raw/ -nc 
"https://data.sfgov.org/api/views/wg3w-h783/rows.csv?date=20231103&accessType=DOWNLOAD"
We can automate downloading dada with wget
