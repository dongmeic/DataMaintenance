# DataMaintenance
To collect data for data visualization, transportation modeling, and web mapping:

1. Transportation data: roads, transit routes and stops, bike network, sidewalks, curb ramps, traffic signals, rail lines, etc;
2. Land use data;
3. Employments data: employment, wage/income;
4. Demographic data: population, gender, race, etc.

## Low income households data for Statewide Transportation Improvement Fund (STIF)

Following the [ODOT guide](https://github.com/dongmeic/DataMaintenance/blob/main/STIF_LowIncomeHousehold_Guidance_2018.pdf), the low-come households were estimated using ACS data [B11016](https://data.census.gov/cedsci/map?q=B11016&text=B11016&g=0500000US41039%241500000&tid=ACSDT5Y2020.B11016&layer=VT_2020_150_00_PY_D1&mode=thematic&loc=41.7549,-108.5443,z9.0000) and [C17002](https://data.census.gov/cedsci/map?q=C17002&g=0500000US41039%241500000&layer=VT_2020_150_00_PY_D1&mode=thematic&loc=41.7549,-108.5443,z9.0000). After downloading the ACS data, run the [script](https://github.com/dongmeic/DataMaintenance/blob/main/Low_Income_Households.R) to get the number of low income households by block group and the average percentage of low-income households at the state level. The data set created using a block group parameter on Tableau is based on the calculation of top percents. The number of block groups at Lane County has increased from 257 to 271. 

The employment by wage class dashboards are updated using wage information, geocoded QCEW (Quarterly Census of Employment and Wages) report, and employment by occupation and industry. First, the wage classes are divided by the number of jobs to include five approximately equal wage classes. Next, the percentage of jobs by occupation and industry is calculated in each wage class, which is matched by SOC (Standard Occupational Classification) Code between the wage information and QCEW report. Then the percentage is used to calculate the number of jobs by employment area (census block).