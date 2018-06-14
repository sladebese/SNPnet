## @knitr battements

#Fonction to determinate extrems data
is_outlier <- function(x) {
  return(x <= quantile(x, 0.25) - 1.5 * IQR(x) | x >= quantile(x, 0.75) + 1.5 * IQR(x))
}

#Collecting values of Total DI for each individual
dfBattements <- DATA_analysis[ DATA_analysis$phenotype_name == "Total_DI", c("individual_name", "age", "strain_number", "value")]
dfBattements$extrem <- ""
#Break the dataframe in two tables, depend on age
dfBattements1 <- dfBattements[ dfBattements$age == 1,]
dfBattements4 <- dfBattements[ dfBattements$age == 4,]

#Display results

#Age 1
histo_battements1 <- ggplot( dfBattements1) + 
                     geom_histogram( aes( x = value), binwidth = 1) + 
                     ggtitle( "Répartition des mouches par nombre de battements cardiaques, Age 1")
ggplotly( histo_battements1)



## @knitr battements3
#Determinating if the value is extrem or not
strains <- unique( dfBattements$strain_number)
for ( strain in strains){
  extrem_list1 <- is_outlier( dfBattements1[ ( dfBattements1$strain_number == strain), "value"])
  individus1 <- which( dfBattements1$strain_number == strain)
  extrems1 <- individus1[ extrem_list1]
  dfBattements1[ extrems1, "extrem"] <- "EXTREME"
}

datatable( dfBattements1[ order( dfBattements1$value), -which( names( dfBattements1) == "age")], rownames = F)


cat( "<br><H3>4 Semaines</H3><br>")
#Age 4
histo_battements4 <- ggplot( dfBattements4) + 
                     geom_histogram( aes( x = value), binwidth = 1) + 
                     ggtitle( "Répartition des mouches par nombre de battements cardiaques, Age 4")
ggplotly( histo_battements4)

## @knitr battements5
#Determinating if the value is extrem or not
for ( strain in strains){
  extrem_list4 <- is_outlier( dfBattements4[ ( dfBattements4$strain_number == strain), "value"])
  individus4 <- which( dfBattements4$strain_number == strain)
  extrems4 <- individus4[ extrem_list4]
  dfBattements4[ extrems4, "extrem"] <- "EXTREME"
}
#Display results
datatable( dfBattements4[ order( dfBattements4$value) , -which( names( dfBattements4) == "age")], rownames = F)

#PROBLEME : calcul différent entre boxplot et fonction is outliers
