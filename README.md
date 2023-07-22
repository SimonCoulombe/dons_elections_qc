# Contributions Pro FR Data Updater

This repository contains a script to download and update the contributions-pro-fr.csv file from https://donnees.electionsquebec.qc.ca/production/provincial/financement/contribution/contributions-pro-fr.csv. 

The fileCSV  is renamed with the current date and saved in the data/ folder. 

A GitHub action is scheduled to run nightly to execute the script and commit the updated file to the repository.
Another github action generates a few tables and graphs below:

![alt text](https://github.com/SimonCoulombe/dons_elections_qc/blob/master/data/plot_dons.png?raw=true)  
 
 
![alt text](https://github.com/SimonCoulombe/dons_elections_qc/blob/master/data/plot_donateurs.png?raw=true)  

![alt text](https://github.com/SimonCoulombe/dons_elections_qc/blob/master/data/plot_super_tableau.png?raw=true)  

![alt text](https://github.com/SimonCoulombe/dons_elections_qc/blob/master/data/plot_matrice_od.png?raw=true)  

