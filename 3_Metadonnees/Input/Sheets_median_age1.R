## @knitr sheets

##Objectives : download all metadata from Laurence's board which have a link with 
##females or mixed sex


#Prepare the list of data to download by putting the name of sheet in data list and then
#the name of the column of interest from the sheet
data_list <- list()

data_list[[ "RecombRateMean_female"]] <- c( "Required_RecombRate1Mean_female",
                                            "Required_RecombRate2Mean_female")
data_list[[ "starvationResistanceMean_female"]] <- c( "Required_StarvationResistanceMean_female")
data_list[[ "StartleResponseMean_female"]] <- c( "Required_StartleResponseMean_female")
data_list[[ "ChillComaMean_female"]] <- c( "Required_ChillComaMean_female")
data_list[[ "SurvivalParaquat_mean_female"]] <- c( "Required_SurvivalParaquat_mean_female")
data_list[[ "SurvivalMSB_mean_female"]] <- c( "Required_SurvivalMSB_mean_female")
#data_list[[ "StartleResponseMSB_female"]] <- c( "")
##### CONTROL OU FEMELLE 
#data_list[[ "NegGeotaxisMSB_female"]] <- c( "")
##### CONTROL OU FEMELLE 
data_list[[ "FoodIntake_female"]] <- c( "Required_FoodIntake_Mean_female")
slide <- read_excel("~/workspace/AnalysisSteph/0_Donnees_droso/DGRP METAANALYSIS_RAw_DATA_Steph.xlsx", 
                    sheet = "CHC_Mean_Female", col_names = TRUE)
namesslide <- colnames( slide[ , -which( colnames( slide) == "strain_number")])
#data_list[[ "CHC_Mean_Female"]] <- namesslide #too young
data_list[[ "OBBenzaldehydeSWARUP_female"]] <- c( "Required_OBBenzaldehydeSWARUP_Mean_female")
data_list[[ "OBHexanal_female"]] <- c( "Required_OBHexanal_ResponseScore_Mean_female")
data_list[[ "OBCitral_female"]] <- c( "Required_OBCitral_ResponseScore_Mean_female")
data_list[[ "OB2PhenylEthylAlcohol_female"]] <- c( "Required_OB2PhenylEthylAlcohol_ResponseScore_Mean_female")
data_list[[ "OB2heptanone_female"]] <- c( "Required_OB2heptanone_ResponseScore_Mean_female")
data_list[[ "OBMethylSalicylate_female"]] <- c( "Required_OBMethylSalicylate_ResponseScore_Mean_female")
data_list[[ "OBBenzaldehydeAya2015_female"]] <- c( "Required_OBBenzaldehydeAya2015_ResponseScore_Mean_female")
data_list[[ "OBAcetophenone_female"]] <- c( "Required_OBAcetophenone_ResponseScore_Mean_female")
data_list[[ "OBEugenol_female"]] <- c( "Required_OBEugenol_ResponseScore_Mean_female")
data_list[[ "OBHelional_female"]] <- c( "Required_OBHelional_ResponseScore_Mean_female")
data_list[[ "OBLCarvone_female"]] <- c( "Required_OBLCarvone_ResponseScore_Mean_female")
data_list[[ "OBDCarvone_female"]] <- c( "Required_OBDCarvone_ResponseScore_Mean_female")
data_list[[ "OB1hexanol_female"]] <- c( "Required_OB1hexanol_ResponseScore_Mean_female")
data_list[[ "OBEthylAcetate_female"]] <- c( "Required_OBEthylAcetate_ResponseScore_Mean_female")
data_list[[ "OBEthylButyrate_female"]] <- c( "Required_OBEthylButyrate_ResponseScore_Mean_female")
data_list[[ "OBBenzaldehydeArya2010_femal"]] <- c( "Required_OBBenzaldehydeArya2010_ResponseScore_Mean_female")
data_list[[ "OBAcetophenoneArya2010_female"]] <- c( "Required_OBAcetophenoneArya2010_ResponseScore_Mean_female")
data_list[[ "OB1HexanolArya2010_female"]] <- c( "Required_OB1HexanolArya2010_ResponseScore_Mean_female")
data_list[[ "OBHexanalArya2010_female"]] <- c( "Required_OBHexanalArya2010_ResponseScore_Mean_female")
data_list[[ "Longevity_Arya2010_female"]] <- c( "Required_Longevity_Arya2010_days_mean_female")
data_list[[ "Longevity_Ivanov2015_female"]] <- c( "Required_Longevity_Ivanov2015_days_Mean_Female")
data_list[[ "AbdPigm_T5_T6_mean_se_female"]] <- c( "Required_AbdPigmentationScore_T5_mean_female")
data_list[[ "EtOHSensitivity_1_ female"]] <- c( "Required_EtOHsensitivity_MET_E1_Mean_female")
data_list[[ "EtOHSensitivity_2_female"]] <- c( "Required_EtOHsensitivity_MET_E2_Mean_female")
data_list[[ "EtOHTolerance_female"]] <- c( "Required_EtOHsensitivity_Tolerance_Mean_female")
data_list[[ "αAmanitinResistance_mixedSex"]] <- c( "Required_αAmanitin_Concentration02_HatchingFlies_Mean", "Required_αAmanitin_Concentration2_HatchingFlies_Mean")
data_list[[ "Toxicity_MeHg_mixed_sex"]] <- c( "Required_Toxicity_MeHg0_mean", "Required_Toxicity_MeHg5_mean", "Required_Toxicity_MeHg10_mean",
                                              "Required_Toxicity_MeHg15_mean", "Required_Toxicity_MeHg0andCaffeine_mean", "Required_Toxicity_MeHg10andCaffeine_mean")
data_list[[ "PhototaxisScore1W_Mean_female"]] <- c( "Required_PhototaxisScore1W_Mean_female")
data_list[[ "PhototaxisScore2W_Mean_female"]] <- c( "Required_PhototaxisScore2W_Mean_female")
data_list[[ "PhototaxisScore4W_Mean_female"]] <- c( "Required_PhototaxisScore4W_Mean_female")
#data_list[[ "WingDiscGrowth_CS_female"]] <- c( "Required_WingDiscGrowth_CS_Mean_female") # too young
#data_list[[ "EyeAntDiscGrowth_IOD_female"]] <- c( "Required_EyeAntDiscGrowth_IOD_Mean_female") # too young
data_list[[ "FemaleSpermUse_P1_score_mean"]] <- c( "Required_FemaleSpermUse_P1_score_mean")
#Voir si on utilise les autres 
#data_list[[ "TotalLegLength_mean_female"]] <- c( "Required_TotalLegLength_mean_female") # too young
#data_list[[ "LegLength_female_RAW"]] <- c( "Average_TotalLegLength_female") # too young
data_list[[ "SleepTraits_mean female"]] <- c( "Required_NightSleep_min_mean_female", "Required_DaySleep_min_mean_female",
                                              "Required_NightPeriodNumber_mean_female",	"Required_DayPeriodNumber_mean_female",
                                              "Required_NightAvgPeriodLength_min_mean_female",	"Required_DayAvgPeriodLength_min_mean_female",
                                              "Required_WakingActivity_CountsPerMin_mean_female")
data_list[[ "Infection_enteric_Pe_MixedSex"]] <- c( "Required_Infection_enteric_Pe_percentDead")
#data_list[[ "ResistInfectionWang2017_female"]] <- c( "Required_ResistanceInfectionWang2017_Ma549_MeanLT50_Female", "Required_ResistanceInfectionWang2017_Pa14_MeanLT50_Female") # Too young
#data_list[[ "AzinphosMethylSurvival_LD50"]] <- c( "Required_AzinphosMethylSurvival_LD50") #Larves 
#data_list[[ "Infection_Pr_female"]] <- c( "")
##A voir

#Prepare the dfmedian file 
dfmedian <- read.table("~/workspace/AnalysisSteph/2_Phenotypes/Output/median_age1.csv", header = TRUE, sep = ",", row.names = 1)
rownames( dfmedian) <- dfmedian[ , "strain_number"]
dfmedian <- dfmedian[ , -which( names( dfmedian) == "strain_number")]

#We download each sheet we need
for( slide_name in names( data_list)){
  current_slide <- read_excel("~/workspace/AnalysisSteph/0_Donnees_droso/DGRP METAANALYSIS_RAw_DATA_Steph.xlsx", 
                                      sheet = slide_name, col_names = TRUE)
  cat("<H3>", slide_name, "</H3><br>")
  
#In the sheet, we take only data of interest then we merge the column with dfmedian
#In this case, it permits to have correlation with same strain
  for( col_name in data_list[[ slide_name]]){

    cat("<H4>", col_name, "</H4><br>")

    current_data_df <- data.frame( current_slide[ , c( "strain_number", col_name)])
    rownames( current_data_df) <- current_data_df[ , "strain_number"]    
    
    #Number of strain which could be in both files
    common_strain <- current_data_df$strain_number[ current_data_df$strain_number %in% rownames( dfmedian)]
    nb_common_strain <- length( common_strain)    
    cat( "Nombre de lignées communes aux deux fichiers de base, pouvant être traitées :", nb_common_strain)
    
    #Plot of the correlation for each phenotype between metadata and one of the phenotypes 
    correlation <- lapply( names(dfmedian), function(namePheno) {
        mg_info_df <- data.frame( pheno = dfmedian[, namePheno], meta = current_data_df[ rownames( dfmedian), col_name] )
        mg_info_df_sub <- mg_info_df[ complete.cases( mg_info_df),]
        plot <- ggplot( mg_info_df_sub, aes( x = mg_info_df_sub[ ,1], y = mg_info_df_sub[,2])) +
                geom_point( size = 0.8) +
                geom_smooth( method=lm, se=TRUE, col= "red") +
                theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position = "None")
        return( plot)
      })
    do.call( "grid.arrange", c( correlation, ncol=6))
    
    #Apply to get for each phenotype the Spearman correlation between metadata and all phentoypes
    cor_list <- list()
    cor_list <- do.call("rbind.data.frame", 
                        lapply( names(dfmedian), function(namePheno) {
                        number <-  which( names(dfmedian) == namePheno)
                        mg_info_df <- data.frame( pheno = dfmedian[, namePheno], meta = current_data_df[ rownames( dfmedian), col_name] )
                        mg_info_df_sub <- mg_info_df[ complete.cases( mg_info_df),]
                        spearman <- cor.test( x = mg_info_df_sub[ ,1], y = mg_info_df_sub[,2], method = "spearman")
                        cor_list <- list( nb = number, pheno = namePheno, coefficient = spearman$estimate, pvalue =spearman$p.value)
                        return( cor_list)
    }))
  
   rownames( cor_list) <- cor_list[ , "pheno"]
   cor_list <- cor_list[ , -which( names( cor_list) == "pheno")]
   
   #Printing only if the correlation coefficient is superior to 0.7
   coeff_list <- cor_list[ abs( cor_list$coefficient) > 0.2, ]
   if ( dim( coeff_list)[1] > 0) {
     cat( "<br>")
     print( kable( coeff_list, format = "html"))
     cat( "<br>")
   } else {
     cat( "<br>Pas de correlation avec un coefficient superieur a 0.2<br>")
   }
     
  }
}

