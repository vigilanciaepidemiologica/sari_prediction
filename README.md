# vigilancia_epidemiologica
This project contains the result of the master's project by Amauri Duarte da Silva entitled "Epidemiological Surveillance: Use of Machine Learning to Predict Severe Acute Respiratory Syndrome Outbreaks" (UFCSPA Mar/2022)

## Knime
1) The "csv Reader" nodes have possible paths to the data sets used. This must be adjusted according to the location of the data sets on your machine. 
2) The datasets used are in the "KNIME/datasets_examples/" folder.
3) The "git_clima_sudeste_v1.0.knwf" and "git_clima_sul_v1.0.knwf" workflows generate the datasets of the average temperatures by region, but do not need to be executed, as the files "media_temperatura_sudeste.csv" and "media_temperatura_sul.csv" are already available. These daasets are input to the "git_project_SG_v8.knwf" workflow.
4) The dataset "clean_data_srag_epiweek_W52.csv" is zipped in the file "clean_data_srag_epiweek_W52.zip" due to space limitations on Github. It must be unzipped for use.

## Tools version
1) This project uses Knime (version 4.4.1) and R language (version 4.2.0) with the Shiny library (version 1.6.0)
