# Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as-is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

# NATTS-HAP
This repository contains the R scripts and datasets used to create all figures in the NATTS HAP paper.
It is registered in Reusable Component Services: https://sor.epa.gov/sor_extranet/registry2/reusereg/searchandretrieve/details/general/24997

## Set Up Directory
Copy NATTS-HAP to a local repository. The NATTS-HAP directory should contain 4 folders-- R Scripts, Data, Optional Files, and Figures (create an empty folder and it will be populated as the scripts are run). In order to run all the scripts you will first need to download a publically available dataset: 

#### Air Toxics Annual Average Statistics Dataset 
This is the dataset the contains all the annual average concentrations of HAPs for US monitoring stations. It is available at https://www3.epa.gov/ttnamti1/toxdat.html#data at the very bottom the the page (annual average statistics). It will be downloaded as an Excel file, open and select the annual_phase13 tab, save as a csv to NATTS-HAP/Data. You may also wish to save the entire Excel file here. The archived version of this file is also available in the Data folder.

## Running Scripts
Once all the necessary datasets are in NATTS-HAP/Data you can begin running the R scripts to reproduce the paper figures. The scripts are numbered in an order that will produce the figures as they appear in the paper (with the exception of figure 2, the dataset is created in R but the figure is created in Python, which is included, but not numbered). However after 1_filterData.R has been run to create the filteredNATTS dataset, the figures can be created in any order. Assuming the directory structure described above, all scripts can be run from the NATTS-HAP/R scripts folder (once a script has been opened in R studio, click Session -> Set Working Directory -> Source File Location). This will get the datasets from NATTS-HAP/Data and print the figures as PDFs to NATTS-HAP/Figures.

## Producing Figures
By default all the figures will be created as PDFs in NATTS-HAP/Figures. However, they can also be easily viewed in R studio by printing the plot object `print(p)`. Most of the figures will not look exactly as they appear in the paper as many stylistic components of the figures (e.g. colors, fonts) were edited in Adobe Illustrator. 

#### Figure 2 -- Pie Chart Map
The script for this figure is written in Python using Jupyter notebooks, which can be accessed through Anaconda if not already installed. Some additional libraries will need to be installed via Anaconda or `pip install`. Documentation for Anaconda and installing Python libraries can be found here: 

https://docs.anaconda.com/anaconda/install/

https://packaging.python.org/tutorials/installing-packages/

#### Figure 6 -- Correlation Matrix
The script for this figure uses the [MICE package](https://cran.r-project.org/web/packages/mice/mice.pdf) to impute data for sites that are missing some but not more than 75% of data used in the correlation matrix. This method will produce slightly different imputation values when run so the resulting correlation matrix might not look identical to the paper figure but major clusters should be the same. 
