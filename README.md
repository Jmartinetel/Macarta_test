# Lead Analysis and Modeling Project

This repository contains the files and scripts necessary for lead data analysis and modeling.


Below is a description of the folder structure and the purpose of each component.

## Folder Structure
- cache/: Contains processed data files:
  - **prep_data.csv**: Data prepared after initial processing.
  - **Final_data_model.csv**: Final data used in the modeling process.

- data/: Contains the raw data.

- munge/: Contains the initial data processing script. This script exports and generate the clean dataset used in the analyses.

- src/: Contains the exploratory data analysis and models, divided into two subfolders:
  - eda/: Contains the exploratory data analysis (EDA) script in R.
  - model/: Contains two Jupyter Notebook files:
    - **01_DAILY_model.ipynb**: Model based on daily data.
    - **02_WEEKLY_model.ipynb**: Model based on weekly data. **This is the most important file** since is the final model
                                And contains some final conclusion and futher steps

- requirements.txt: This file lists all necessary dependencies. Use it to install the virtual environment.

## Usage Instructions

1. Set up the virtual environment (`.venv`) by installing the dependencies listed in `requirements.txt`.

2. Run the munge script to prepare the data.
3. Explore the data using the EDA script in R (src/eda/).
  
5. Use the **weekly model*** (src/model/02_WEEKLY_model.ipynb) for final results and insights based on the analysis.

