# Contributions Pro FR Data Updater

This repository contains a script to download and update the contributions-pro-fr.csv file from https://donnees.electionsquebec.qc.ca/production/provincial/financement/contribution/contributions-pro-fr.csv. The file is renamed with the current date and saved in the data/ folder. A GitHub action is scheduled to run nightly to execute the script and commit the updated file to the repository.

## Usage

To download and update the file manually, run the following command in your R environment:

