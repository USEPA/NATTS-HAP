# NATTS-HAP
This repository contains the R scripts and datasets used to create all figures in the NATTS HAP paper.

## Set up directory
Copy NATTS-HAP to a local repository. The NATTS-HAP directory should contain 4 folders-- R Scripts, Data, Optional Files, and Figures (create an empty folder and it will be populated as the scripts are run). In order to run all the scripts you will first need to download some publically available datasets: 

### Air Toxics Annual Average Statistics Dataset 
This is the dataset the contains all the annual average concentrations of HAPs for US monitoring stations. It is available at https://www3.epa.gov/ttnamti1/toxdat.html#data at the very bottom the the page (annual average statistics). It will be downloaded as an Excel file, open and select the annual_phase13 tab, save as a csv to NATTS-HAP/Data. You may also wish to save the entire Excel file here.

### AQS PM10 and PM2.5 Daily Mass Files 
These files contain the daily PM10 and PM2.5 mass measurements. R script 7_filterPMmass.R calculates the annual mass averages, which are combined with the HAPs data and used in figure 6 (correlation matrix). They are available from https://aqs.epa.gov/aqsweb/airdata/download_files.html#Daily. PM2.5 FRM/FEM Mass (88101) and PM10 Mass (81102) zip files will need to be downloaded for the years 2013-2017 and the csv contents unzipped to NATTS-HAP/Data. If you wish to skip this step (as the mass files are very large), I have included a copy of the file (filteredNATTSwPMmass.csv) that is produced from running script 7_filterPMmass.R under NATTS-HAP/Optional-Files. Move that file to NATTS-HAP/Data and then you will not have to download PM mass files or run script 7_filterPMmass.R 

## Running Scripts
Once all the necessary datasets are in NATTS-HAP/Data you can begin running the R scripts to reproduce the paper figures. The scripts are numbered in an order that will produce the figures as they appear in the paper (with the exception of figure 2, the dataset is created in R but the figure is created in Python, which is included, but not numbered). However after 1_filterData.R has been run to create the filteredNATTS dataset, the figures can be created in any order, with the exception of 8_corrmatrix_fig6.R, which needs filteredNATTSwPMmass.csv either from NATTS-HAP/Optional Files or from running 7_filterPMmass.R. Assuming the directory structure described above, all scripts can be run from the NATTS-HAP/R scripts folder (once a script has been opened in R studio, click Session -> Set Working Directory -> Source File Location). This will get the datasets from NATTS-HAP/Data and print the figures as PDFs to NATTS-HAP/Figures.

## Producing figures
By default all the figures will be created as PDFs in NATTS-HAP/Figures. However, they can also be easily viewed in R studio by printing the plot object `print(p)`. Most of the figures will not look exactly as they appear in the paper as many stylistic components of the figures (e.g. colors, fonts) were edited in Adobe Illustrator. 
