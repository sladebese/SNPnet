## @knitr doublon

##Objective : Some strains are duplicated. We want to keep only one of them, 
##the most complete and close to other age's date


#Keeping only one trio of strain/age/number and creating new variables
DATA_dup <- DATA_dgrp[ !duplicated( DATA_dgrp[ , c("strain_number", "age", "yeardate")]), ]
DATA_QC <- DATA_dgrp
DATA_QC[,"QC"] <- ""
tabdupbind <- data.frame( c())
dateNames <- c()

for (i in c(1,4)) { 
  #Getting names of duplicated strains : table of strains 
  tab <- table( DATA_dup[ DATA_dup$age ==i, ]$strain_number)
  dup <- as.character( data.frame( tab[ tab >= 2])$Var1 )
  name <- paste( "nameStrainDup", i, sep = "")  
  assign( name, dup) 
  
  #Choice between strains duplicated 
  #Sort by unique individual names and age
  DATAUniq <- DATA_dgrp[ !duplicated( DATA_dgrp$individual_name), ]
  DATAUniqSort <- DATAUniq[ DATAUniq$age == i,]
  
  #Table of frequency for each strain by date, then add strains
  #and age to do a dataframe
  
  for (names in dup){
    DATAUniqSortStrain <- DATAUniqSort[ DATAUniqSort$strain_number == names,]
    tab2 <- table( DATAUniqSortStrain$yeardate)
    tab3 <- data.frame( tab2)
    tab3$strain_number <- names
    tab3$age <- i
    tabdupbind <- rbind.data.frame( tabdupbind, tab3) 
    
    #Choosing the duplicate with more samples 
    if (tab2[[1]] != tab2[[2]]) { 
      min <- row.names( data.frame( which.min( c( tab2[1], tab2[2]))))
      DATA_QC[ ( DATA_QC$strain_number == names) & ( DATA_QC$yeardate == as.integer(min)), "QC"] <- "dup"
    } else {  
      #Choosing the duplicate with closer dates for age 1 and 4
      #Extracting of yeardate of the same strain for age different to i
      dateNames <- c( dateNames, names)
      dateNoI <- unique( DATA_QC[( DATA_QC$strain_number == names) & (DATA_QC$age != i) &
                                   (DATA_QC$QC != "dup"),]$yeardate)
      dateNoI <- as.Date( as.character( dateNoI), "%y%m%d")
      #Look at proximity of dates and choose one
      date1 <- row.names( data.frame( tab2[1]))
      date2 <- row.names( data.frame( tab2[2]))
      dates <- c( date1, date2)
      choice <- which.min( c( abs( as.Date( dates[1], "%y%m%d") - dateNoI),
                              abs( as.Date( dates[2], "%y%m%d") - dateNoI)))
      datechosen <- dates[choice]
      DATA_QC[ ( DATA_QC$strain_number == names) & ( DATA_QC$yeardate == datechosen), "QC"] <- "dup"
    }
  }
}

#Barplot to check the disposal of duplicated strains by age and strains
colnames( tabdupbind) <- c( "date", "Freq", "strain_number", "age")
tabdupbind$date <- as.Date( tabdupbind$date, "%y%m%d")
plotStrainDuplicated <- ggplot( tabdupbind, aes( x = as.factor(date), y=Freq, fill = as.factor(age))) +
                        geom_bar( position=position_dodge(), stat="identity", width = 0.8) +
                        facet_wrap(~ strain_number)+
                        xlab( "Dates") +
                        ylab( "Number of drosophila") +
                        labs( fill = "Age") +
                        ggtitle( "Number of drosophila for duplicated strains") +
                        theme( plot.title = element_text( hjust = 0.5)) +
                        theme( axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
plotStrainDuplicated

cat("Chosen by date :", dateNames)

#Plot to see and verify removed strains
DATA_StrainDup <- DATA_QC[ ( !duplicated(DATA_QC$individual_name)) & (DATA_QC$QC != "dup"), ]
tabDATA_StrainDup <- data.frame( table( DATA_StrainDup$strain_number, DATA_StrainDup$age))
colnames(tabDATA_StrainDup) <- c( "strain_number", "age", "freq")

plotStrainphenoStrainDup  <- ggplot( tabDATA_StrainDup, aes( x= strain_number, y= freq, fill = as.factor(age))) +
                            geom_bar( position="dodge", stat="identity")+
                            coord_cartesian( ylim = c(0,25)) +
                            xlab( "Strains") +
                            ylab( "Number of drosophila") +
                            ggtitle( paste( "Unreplicated strains \nNumber of drosophila by strain and age")) +
                            theme( plot.title = element_text( hjust = 0.5),
                                   axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
                                   legend.position = "bottom") +
                            geom_hline( aes( yintercept = 8), lty = 2)
plotStrainphenoStrainDup

#Cleaning 
rm( DATA_dup, i, tab, dup, name, names, DATAUniq, DATAUniqSort, DATAUniqSortStrain, tab2, tab3,
   min, dateNoI, date1, date2, dates, choice, datechosen, DATA_StrainDup, tabDATA_StrainDup )

#Created and kept variables 
# DATA_QC : all data contained in DATA_dgrp and two more column about quality
# dateNames : duplicates strains names
# nameStrainDup1 : duplicates strains names for age 1
# nameStrainDup4 : duplicates strains names for age 4
# plotStrainDuplicated : plot of duplicated strains by dates
# plotStrainphenoStrainDup : plot of individus distribution by strain and age without duplicates
# tabdupbind : date, frequency, name and age of duplicated strains
