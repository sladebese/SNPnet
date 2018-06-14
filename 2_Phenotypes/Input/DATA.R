## @knitr DATA

## Objective : Create a big data frame by merging as much information as possible


#Files to be merged
dataIndividual <- read.csv( file.path( dirOutput,"0_Donnees_droso/individual.csv"), sep ="\t")
dataPhenoNoout <- read.csv( file.path( dirOutput,"0_Donnees_droso/phenotype_data.csv_control_nooutliers.csv"), sep ="\t")
dataDateCorrection <- read.csv( file.path( dirOutput,"0_Donnees_droso/phenotype_data.csv_date_correction.csv"), sep ="\t")
dataStrain <- read.csv( file.path( dirOutput,"0_Donnees_droso/strain.csv"), sep ="\t")

#Merge of pheno et individual data
phenoIndiv  <- merge.data.frame( x = dataPhenoNoout, y = dataIndividual, 
                                 by.x = c( "individual_name", "age"), by.y = c( "name", "age"), all = FALSE)
phenoIndiv$id.x <- NULL
phenoIndiv$id.y <- NULL

#Merge new data frame with Date correction 
phenoIndivDate <- merge.data.frame( phenoIndiv, dataDateCorrection, by.x = c( "age", "date", "phenotype_name"), 
                                    by.y = c( "age", "date", "phenotype_name"), all = FALSE)
phenoIndivDate$id.x <- NULL
phenoIndivDate$id.y <- NULL

#Merge with strain data
phenoIndivDateStrain <- merge.data.frame( phenoIndivDate, dataStrain, by.x = "strain_number", by.y = "number", all = FALSE)
phenoIndivDateStrain$id.x <- NULL
phenoIndivDateStrain$id.y <- NULL

#Reordering columns
phenoIndivDateStrain <- phenoIndivDateStrain[ ,c("individual_name", "age", "sex", "strain_number", 
                                                 "control", "user", "date","yeardate", 
                                                 "phenotype_name", "value", "value.correction")]
phenoIndivDateStrain$value.corrected <- phenoIndivDateStrain$value - phenoIndivDateStrain$value.correction

#Final file
DATA_raw <- phenoIndivDateStrain

#Export dataframe created 
write.table( DATA_raw, file=file.path( dirOutput, "2_Phenotypes/Output/01_DATA_raw.txt"))

#Subsetting dgrp lines 
DATA_dgrp <- DATA_raw[ -which( DATA_raw$control == 1),]
strainorder <- unique( as.character( DATA_dgrp$strain_number))
DATA_dgrp$strain_number = factor( DATA_dgrp$strain_number, levels = strainorder)

#Export dataframe created 
write.table( DATA_dgrp, file=file.path( dirOutput, "2_Phenotypes/Output/02_DATA_dgrp.txt"))

#Cleaning : 
rm( strainorder)

#Created and kept variables 
# DATA_dgrp : data concerning only dgrp drosophiles
# DATA_raw : all data, from phenoIndivDateStrain, fixed 
# dataDateCorrection : data from phenotype_data.csv_date_correction.csv
# dataIndividual : data from individual.csv
# dataPhenoNoout : data from phenotype_data.csv_control_nooutliers.csv
# dataStrain : data from strain.csv
# phenoIndiv : merge dataIndividual + dataPhenoNoout
# phenoIndivDate : merge phenoIndiv + dataDateCorrection
# phenoIndivDateStrain : merge phenoIndivDate + dataStrain