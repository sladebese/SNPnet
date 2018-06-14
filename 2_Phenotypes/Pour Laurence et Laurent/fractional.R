## @knitr fractional

#Laurence et Lionel 

#Liste des fractional shortening 
dfFractional <- DATA_analysis[ DATA_analysis$phenotype_name == "FractionalShortening", c("individual_name", "age", "strain_number", "value")]
dfFractional <- dfFractional[ order( dfFractional$value),]
datatable( dfFractional, rownames = F)
histo_fractional <- ggplot( dfFractional) + 
                    geom_histogram( aes( x = value), bins = 100) + 
                    ggtitle( "Répartition des Fractional shortening")

ggplotly( histo_fractional)
