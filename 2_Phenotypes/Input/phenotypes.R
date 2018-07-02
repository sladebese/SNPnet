## @knitr phenotypes

##Objective : Draw a draw of drosophiles' data by phenotypes 


#Table of frequence by phenotype 
tabPheno <- data.frame( table( DATA_analysis$phenotype_name, DATA_analysis$age))
colnames( tabPheno) <- c( "phenotype", "age", "freq")

plotbdTabPhenoAge  <- ggplot( tabPheno, aes( fill=age, y=freq, x=phenotype)) +
                      geom_bar( position="dodge", stat="identity", width=0.8)+
                      xlab( "Phenotypes") +
                      ylab( "Number of drosophila") +
                      ggtitle( "Number of drosophila by phenotype and age") +
                      theme( plot.title = element_text( hjust = 0.5),
                             axis.text.x = element_text(angle = 60, hjust = 1, size = 8))
plotbdTabPhenoAge 

#Cleaning : no cleaning to do 

#Created and kept variables : 
# tabPheno : frequency of individus by phenotype
# plotbdTabPhenoAge : plot of individus distribution by age and phenotype