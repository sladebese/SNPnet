## @knitr filter

##Objective :  Some strains don't have the minimal number of drosophiles before 
##any filter, we take them off 


#New colum to DATA_QC
DATA_QC[,"QC2"] <- ""

#Listing of strains with less than 8 samples for each age of each strains
DATAUniq <- DATA_QC[ (!duplicated(DATA_QC$individual_name)),]
tabDATA_QCpheno <- data.frame( table( DATAUniq$strain_number, DATAUniq$age))
nameToRemoveQC <- unique( tabDATA_QCpheno[ tabDATA_QCpheno$Freq <8,]$Var1)
cat("Strains with less than 8 drosophiles :", as.character( nameToRemoveQC))
idxToRemove <- which( DATA_QC$strain_number %in% nameToRemoveQC)
DATA_QC[ idxToRemove,]$QC2 <- "under"

#Export new dataframe we will work with
write.table( DATA_QC, file=file.path( dirOutput, "2_Phenotypes/Output/03_DATA_FilterStrains.txt"))

#Plot to see and verify removed strains
DATA_filter <- DATA_QC[ !duplicated(DATA_QC$individual_name) & DATA_QC$QC != "dup" & DATA_QC$QC2 != "under", ]
tabDATA_filter <- data.frame( table( DATA_filter$strain_number, DATA_filter$age))
colnames(tabDATA_filter) <- c( "strain_number", "age", "freq")

plotStrainphenofilter  <- ggplot( tabDATA_filter, aes( x= strain_number, y= freq, fill = as.factor(age))) +
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
plotStrainphenofilter

#Cleaning
rm( DATAUniq, tabDATA_QCpheno, idxToRemove, DATA_filter, tabDATA_filter)
 
#Created and kept variables : 
# nameToRemoveQC : names of strains with less than 8 samples
# plotStrainphenofilter : plot of individus distribution by strain and age without 
#                         duplicates or low-numbered strains 