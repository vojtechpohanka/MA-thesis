This repository includes replication script for my master's thesis *Party Competition on Cultural Issues in Central and Eastern Europe: Examining Patterns in Issue Salience and Content*.

The scripts included in this repository were ran using R version 4.4.2. 
# Overview of scripts 
This repository includes the following scripts to be loaded from "scripts/" in your working directory: 
   * "data_prep.R" - script for preparing and exporting the datasets used in "countrylevel_analysis.R" (chapter 3) and "partylevel_analysis.R" (chapter 4).
   * "pattern_extraction.R" - script for preparing the dataset used in "debate_analysis.R" (chapter 5). Run this script before running "debate_analysis.R".
   * "countrylevel_analysis.R" - replication file for analysis in Chapter 3
   * "partylevel_analysis.R" - replication file for analysis in Chapter 4
   * "debate_analysis.R" - replication file for analysis in Chapter 5


# Instructions for downloading datasets used in the analysis

In addition to the Manifesto Project (MARPOR) dataset, which is loaded from the manifestoR package, the scripts **data_prep.R** and **patern_extraction.R** include several datasets to be loaded from the "data/" directory. To fully reproduce the analysis, you will need to download the following datasets and store them in your "data/" directory: 
*  **European Social Survey European Research Infrastructure (ESS ERIC) (2025) ESS11 - integrated file, edition 3.0 [Data set]**. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ess11e03_0. Download as CSV and save the zip file as "ESS11.zip" in your "data/" directory. 
* **Datasets from the open data from the World Bank Databank**. For each of the three indicators linked below, I manually selected the **countries** "Bulgaria", "Croatia", "Czech Republic","Estonia", "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia" and the **years** from 1990 until 2020 (inclusive). 
    * **Net migration: https://databank.worldbank.org/reports.aspx?source=2&series=SM.POP.NETM&country= . Manually select the countries and years listed above, download as CSV and save the zip file as "P_Data_Extract_From_World_Development_Indicators.zip" in your "data/" directory.** Citation: World Bank. 2025. “Net Migration.” Https://data.worldbank.org/indicator/SM.POP.NETM. https://data.worldbank.org/indicator/SM.POP.NETM.
    * **Total population: https://databank.worldbank.org/reports.aspx?source=2&series=SP.POP.TOTL&country= . Manually select the countries and years listed above, download as CSV and save the zip file as "wb_pop.zip" in your "data/" directory.** Citation: World Bank. 2025. “Population, Total.” Https://data.worldbank.org/indicator/SP.POP.TOTL. https://data.worldbank.org/indicator/SP.POP.TOTL.
    *  **GDP per capita:  https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.PCAP.PP.CD&country= . Manually select the countries and years listed above, download as CSV and save the zip file as "wb_gdp.zip" in your "data/" directory.** Citation:  World Bank. 2025. “GDP per Capita, PPP (Current International $).” Https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD.
* **The Populist 3.0** dataset: Van Kessel, Stijn, Sarah De Lange, Paul Taggart, et al. 2023. The PopuList 3.0. https://doi.org/10.17605/OSF.IO/2EWKQ.
    * Download CSV from https://osf.io/2ewkq/files/osfstorage and save as "Populist.csv" in your "data/directory".
* The **1999-2019 Chapell Hill Expert Survey (CHES) trend file**, **2017** and **2024** Chapell Hill Expert Surveys. Download the three CSV files from https://www.chesdata.eu/ches-europe .
    * **Save the 1999-2019 trend file as "1999-2019.csv" in your "data/" directory.** Citation: Jolly, Seth, Ryan Bakker, Liesbet Hooghe, et al. 2022. “Chapel Hill Expert Survey Trend File, 1999–2019.” Electoral Studies 75 (February): 102420. https://doi.org/10.1016/j.electstud.2021.102420.
    *   **Save the 2017 CHES as "CHES_2017.csv" in your "data/" directory.** Citation: Rovny, Jan, Ryan Bakker, Liesbet Hooghe, et al. n.d. “25 Years of Political Party Positions in Europe: The Chapel  Hill Expert Survey, 1999-2024.” Working paper.
    * **Save the 2024 CHES as "CHES_2024.csv" in your "data/" directory.** Citation: Rovny, Jan, Ryan Bakker, Liesbet Hooghe, et al. n.d. “25 Years of Political Party Positions in Europe: The Chapel  Hill Expert Survey, 1999-2024.” Working paper.
* **PARLGOV data**:  Döring, Holger, and Philip Manow. 2024. **“ParlGov 2024 Release.”** Version 1.0. With Holger Döring. Harvard Dataverse. Application/octet-stream,text/markdown,application/octet-stream,text/tab-separated-values,text/tab-separated-values,text/tab-separated-values,text/plain,application/pdf,text/tab-separated-values,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, 7307264, 106797, 5308416, 7829, 1925238, 283337, 7171, 248194, 1243395, 1567278. https://doi.org/10.7910/DVN/2VZ5ZC.
    * From https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/2VZ5ZC download:
        * **"view_cabinet.csv" into your "data/directory"** and
        * **"view_party.csv" into your "data/directory"**
* **Czech parliamentary speeches**:  Jabůrek, Štěpán, 2024, "Czech Parliamentary Speeches Dataset 1993-2023; full transcripts of plenary speeches from the Chamber of Deputies", https://doi.org/10.7910/DVN/FOQUZF, Harvard Dataverse, V2  and Rauh, Christian; Schwalbach, Jan, 2020, "The ParlSpeech V2 data set: Full-text corpora of 6.3 million parliamentary speeches in the key legislative chambers of nine representative democracies", https://doi.org/10.7910/DVN/L4OAKN, Harvard Dataverse, V1
    * download CSV file from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/FOQUZF and save as "CzechParlPlenaryUpdated_13.07.2024.csv" in your "data/" directory
    

 
  

 

