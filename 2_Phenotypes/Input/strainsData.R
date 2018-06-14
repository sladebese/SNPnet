## @knitr strainsData

## Objective : Plot a graph to show the repartitions of drosophiles by strains


#Table Strain by Age to see how many drosophiles by age and strains we got
DATAdup <- DATA_dgrp
DATAdup <- DATAdup[ !duplicated( DATAdup$individual_name),]
tabDATA <- data.frame( table( DATAdup$strain_number, DATAdup$age))
colnames( tabDATA) <- c( "strain_number", "age", "freq")

# Barplot : see how many droso by strains and age
plotDgrpAge <- ggplot( tabDATA, aes( x=strain_number, y=freq, fill=as.factor(age))) +
              geom_bar( position=position_dodge(), stat="identity") +
              coord_cartesian( ylim = c( 0, 25)) +
              xlab( "Strains") +
              ylab( "Number of drosophila") +
              labs(fill = "Age") +
              ggtitle( "Number of drosophila by strain and age") +
              theme( plot.title = element_text( hjust = 0.5),
                     axis.text.x = element_text(angle = 90, hjust = 1, size = 7, lineheight = 8),
                     legend.position = "bottom") 
plotDgrpAge

#Cleaning 
rm (DATAdup)

#Created and kept variables 
# plotDgrpAge : plot of individus distribution by age and strain
# tabDATA : dataframe with frequency of strains at each age
