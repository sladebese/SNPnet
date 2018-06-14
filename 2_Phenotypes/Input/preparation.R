## @knitr preparation 

##Objective : Reorganize strains factors to have cleaned plot and extract a new datadrame

#Reorganizing factors by strains remaining 
DATA_analysis <- DATA_QC[ DATA_QC$QC != "dup" & DATA_QC$QC2 != "under", ]
strainsFiltered <- as.character( sort( unique(DATA_analysis$strain_number)))
DATA_analysis$strain_number <- factor( DATA_analysis$strain_number, levels = strainsFiltered)

#Export data
write.table( DATA_analysis, file=file.path( dirOutput, paste("2_Phenotypes/Output/04_DATA_analysis.txt")))

#Table to have frequency by strains
DATA_analysis_test <- DATA_analysis[ !duplicated(DATA_analysis$individual_name), ]
tabDATA_analysis <- data.frame( table( DATA_analysis_test$strain_number, DATA_analysis_test$age))
colnames(tabDATA_analysis) <- c( "strain_number", "age", "freq")

#Verifying by a plot
plotStrainphenoanalysis <- ggplot( tabDATA_analysis, aes( x= strain_number, y= freq, fill = as.factor(age))) +
  geom_bar( position="dodge", stat="identity")+
  coord_cartesian( ylim = c(0,25)) +
  xlab( "Strains") +
  ylab( "Number of drosophila") +
  labs(fill = "Age") +
  ggtitle( paste( "Unreplicated and high-numbered strains \nNumber of drosophila by strain and age")) +
  theme( plot.title = element_text( hjust = 0.5),
         axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
         legend.position = "bottom") +
  geom_hline( aes( yintercept = 8), lty = 2)
plotStrainphenoanalysis

#Cleaning 
rm( strainsFiltered, DATA_analysis_test)

#Created and kept variables 
# DATA_analysis : dataframe with all data but no duplicated of low-numbered strains
# tabDATA_analysis : frequency of data in DATA_analysis frame
# plotStrainphenoanalysis : plot of individus distribution with no duplicates, 
#                          low-numbered strains and with factor in the good order