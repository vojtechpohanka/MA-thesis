This repository includes replication scripts for my master's thesis *Party Competition on Cultural Issues in Central and Eastern Europe: Examining Patterns in Issue Salience and Content*.

The scripts included in this repository were run using R version 4.4.2. 
# Overview of scripts 
This repository includes the following scripts to be loaded from "scripts/" in your working directory: 
   * "data_prep.R" - script for preparing and exporting the datasets used in "countrylevel_analysis.R" (chapter 3) and "partylevel_analysis.R" (chapter 4).
   * "pattern_extraction.R" - script for preparing and exporting the dataset used in "debate_analysis.R" (chapter 5).
   * "countrylevel_analysis.R" - replication file for analysis in chapter 3
   * "partylevel_analysis.R" - replication file for analysis in chapter 4 
   * "debate_analysis.R" - replication file for analysis in chapter 5 


# Instructions for downloading datasets used in the analysis

In addition to the Manifesto Project (MARPOR) dataset, which is loaded from the manifestoR package, the scripts **data_prep.R** and **pattern_extraction.R** include several datasets to be loaded from the "data/" directory. To fully reproduce the analysis, download the following datasets and store each in your "data/" directory.
* pooled dataset from 11 waves of the **European Social Survey (ESS)**:
    * Download the file from the ESS data builder: https://ess.sikt.no/en/data-builder/?rounds=0.6_13_26_32+1.6_8_13_26_31_32+2.3_8_13_26_31_32+3.3_4_6_8_13_19_26_28_31_32+4.3_4_6_8_13_20_26_31_32+5.3_6_8_13_20_26_31_32+6.6_8_13_20_26_32+7.6_8_13_20_26_32+8.3_4_6_8_13_19_20_26_31_32+9.3-8_13_20_31_32+10.19_26+11.3_4_13_19_20_26_31_32&seriesVersion=905&tab=download&variables=2.0.61
    * Download as CSV and save the zip file as "ESS_subset.zip" in your "data/" directory.
    * Citations for the survey waves:
        *  **European Social Survey European Research Infrastructure (ESS ERIC) (2025) ESS11 - integrated file, edition 3.0 [Data set]**. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.21338/ess11e03_0.
  
* **Datasets from the World Bank Databank**. For each of the three indicators linked below, I manually selected the **countries** "Bulgaria", "Croatia", "Czech Republic","Estonia", "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia" and the **years** from 1990 until 2020 (inclusive). 
    * **Net migration: https://databank.worldbank.org/reports.aspx?source=2&series=SM.POP.NETM&country= . Manually select the countries and years listed above, download as CSV and save the zip file as "P_Data_Extract_From_World_Development_Indicators.zip" in your "data/" directory.** Citation: World Bank. 2025. “Net Migration.” Https://data.worldbank.org/indicator/SM.POP.NETM. https://data.worldbank.org/indicator/SM.POP.NETM.
    * **Total population: https://databank.worldbank.org/reports.aspx?source=2&series=SP.POP.TOTL&country= . Manually select the countries and years listed above, download as CSV and save the zip file as "wb_pop.zip" in your "data/" directory.** Citation: World Bank. 2025. “Population, Total.” Https://data.worldbank.org/indicator/SP.POP.TOTL. https://data.worldbank.org/indicator/SP.POP.TOTL.
    *  **GDP per capita:  https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.PCAP.PP.CD&country= . Manually select the countries and years listed above, download as CSV and save the zip file as "wb_gdp.zip" in your "data/" directory.** Citation:  World Bank. 2025. “GDP per Capita, PPP (Current International $).” Https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD.
    
* **The Populist 3.0** dataset: Van Kessel, Stijn, Sarah De Lange, Paul Taggart, et al. 2023. The PopuList 3.0. https://doi.org/10.17605/OSF.IO/2EWKQ.
    * Download CSV from https://osf.io/2ewkq/files/osfstorage and save as "Populist.csv" in your "data/" directory.
 
* The **1999-2019 Chapell Hill Expert Survey (CHES) trend file**, **2017** and **2024** Chapell Hill Expert Surveys. Download the three CSV files from https://www.chesdata.eu/ches-europe .
    * **Save the 1999-2019 trend file as "1999-2019.csv" in your "data/" directory.** Citation: Jolly, Seth, Ryan Bakker, Liesbet Hooghe, et al. 2022. “Chapel Hill Expert Survey Trend File, 1999–2019.” Electoral Studies 75 (February): 102420. https://doi.org/10.1016/j.electstud.2021.102420.
    *   **Save the 2017 CHES as "CHES_2017.csv" in your "data/" directory.** Citation: Polk, Jonathan, Jan Rovny, Ryan Bakker, et al. 2017. “Explaining the Salience of Anti-Elitism and Reducing Political Corruption for Political Parties in Europe with the 2014 Chapel Hill Expert Survey Data.” Research & Politics 4 (1): 2053168016686915. https://doi.org/10.1177/2053168016686915.
    * **Save the 2024 CHES as "CHES_2024.csv" in your "data/" directory.** Citation: Rovny, Jan, Ryan Bakker, Liesbet Hooghe, et al. n.d. “25 Years of Political Party Positions in Europe: The Chapel  Hill Expert Survey, 1999-2024.” Working paper.
      
* **PARLGOV data**:  Döring, Holger, and Philip Manow. 2024. **“ParlGov 2024 Release.”** Version 1.0. With Holger Döring. Harvard Dataverse. Application/octet-stream,text/markdown,application/octet-stream,text/tab-separated-values,text/tab-separated-values,text/tab-separated-values,text/plain,application/pdf,text/tab-separated-values,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, 7307264, 106797, 5308416, 7829, 1925238, 283337, 7171, 248194, 1243395, 1567278. https://doi.org/10.7910/DVN/2VZ5ZC.
    * From https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/2VZ5ZC, download:
        * **"view_cabinet.csv" into your "data/" directory** and
        * **"view_party.csv" into your "data/"  directory**
        
* **Czech parliamentary speeches**: Jabůrek, Štěpán, 2024, "Czech Parliamentary Speeches Dataset 1993-2023; full transcripts of plenary speeches from the Chamber of Deputies", https://doi.org/10.7910/DVN/FOQUZF, Harvard Dataverse, V3 and Rauh, Christian; Schwalbach, Jan, 2020, "The ParlSpeech V2 data set: Full-text corpora of 6.3 million parliamentary speeches in the key legislative chambers of nine representative democracies", https://doi.org/10.7910/DVN/L4OAKN, Harvard Dataverse, V1
    * The analysis uses a cleaned version of the dataset provided personally by Štěpán Jabůrek, which slightly differs from the version available on the Harvard Dataverse. Due to file sizes restrictions on GitHub, I have hosted the file on OSF:
        * Download **debate_data.csv** from https://osf.io/67uca/files/osfstorage and store it in your "data/" directory
    

 
  

 

