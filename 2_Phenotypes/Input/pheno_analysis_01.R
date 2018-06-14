## @knitr phenoPlots

## Objective : loop of analysis for each phenotype with a high number
##of strains with more than 8 drosophiles for each age


#Preparing the loop
phenotypes <- as.character( sort( unique( DATA_analysis$phenotype_name)))
RemainingStrain <- c()
nameOutliers1 <- c()
nameOutliers4 <- c()

for (namePheno in phenotypes){
  
  #Subsetting
  DATA_pheno <- DATA_analysis[ DATA_analysis$phenotype_name == namePheno,]
  tabDATA_pheno <- data.frame( table( DATA_pheno$strain_number, DATA_pheno$age))
  nameToRemove <- unique( as.character( tabDATA_pheno[ tabDATA_pheno$Freq <8, "Var1"]))
  idxToRemove <- which( DATA_pheno$strain_number %in% nameToRemove)
  if ( length( idxToRemove) != 0) {
    DATA_pheno_rm <- DATA_pheno[ -idxToRemove, ]
  } else {
    DATA_pheno_rm <- DATA_pheno
  }
 
  #Extracting dataframe used for each phenotype
  write.table( DATA_pheno_rm, file=file.path( dirOutput, paste("2_Phenotypes/Output/Phenotypes/", namePheno, ".txt")))
    
  #Not traited if not enought data  
  RemainingStrain_nb <- ( length( unique( DATA_pheno_rm$strain_number)) *100) / length( 
                                    unique( DATA_analysis$strain_number)) 
  RemainingStrain <- c( RemainingStrain, RemainingStrain_nb)
  
  if ( RemainingStrain_nb > 80){
       
    #Title
     cat("<H4>", namePheno, "</H4><br>")
    
    #Plot of remaining strains 
    cat("<br>Ci-dessous est affiché le barplot des lignées restantes après le second filtre. Il reste", 
        RemainingStrain_nb, "% des lignées de DATA_Analysis.<br>")
    tabDATA_pheno_rm <- data.frame( table( DATA_pheno_rm$strain_number, DATA_pheno_rm$age))
    colnames(tabDATA_pheno_rm) <- c( "strain_number", "age", "freq")
    plotStrainpheno  <- ggplot( tabDATA_pheno_rm, aes( x= strain_number, y= freq, fill = as.factor(age))) +
                        geom_bar( position="dodge", stat="identity") +
                        coord_cartesian( ylim = c( 0,25)) +
                        xlab( "Strains") +
                        ylab( "Frequency") +
                        ggtitle( paste( namePheno,"\nNumber of drosophila by strain and age")) +
                        theme( plot.title = element_text( hjust = 0.5),
                               axis.text.x = element_text( angle = 90, hjust = 1, size = 7),
                               legend.position = "bottom") +
                        geom_hline( aes( yintercept = 8), lty = 2)
    print( plotStrainpheno)
  
    #Graphique boxplot des valeursen fonction de chacune des lignées ordonnées selon la médiane
    cat("<br>Les boxplots suivants sont les valeurs du phénotype étudiés pour chaque lignée ordonnées selon leur médiane.<br>")
    lenStrain <- length( unique( DATA_pheno_rm$strain_number))
    plotHRMpheno <- ggplot( DATA_pheno_rm) + 
                    geom_boxplot( aes( x = reorder(x =strain_number, X = value, FUN =  median), y = value), 
                                  fill = rainbow(lenStrain)) +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
                    xlab( "Strains") +
                    ylab( "Phenotype values") +
                    ggtitle( paste( namePheno, "\nphenotype values between strains organized by median")) +
                    theme( plot.title = element_text( hjust = 0.5),
                           axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) 
    print( plotHRMpheno) 
    
    #Plot of overall difference between Age1 et Age4
    cat("<br>Les valeurs phénotypiques sont maintenant organisées en fonction des âges, sans un regard à la 
        lignée, afin de voir si on a une tendance générale.<br>")
    plotHRM14 <- ggplot( DATA_pheno_rm) + 
                 geom_violin( aes( x = as.factor( age), y = value, fill = as.factor(age))) +
                 scale_fill_manual( values = c("1"= "lemonchiffon3", "4"= "gray60" )) +
                 geom_boxplot( aes( x = as.factor( age), y = value), width = 0.1) +
                 xlab( "Age") +
                 ylab( "Phenotype values") +
                 ggtitle( paste( namePheno, "\nphenotype values between ages")) +
                 theme( plot.title = element_text( hjust = 0.5)) 
    print( plotHRM14)
  
    # Wilcoxon test on age
    cat("<br>Un premier test de Wilcoxon est appliquée à ces données. Si la p-valeur est inférieur à 0.05, 
        cela signifie que les deux populations (age1, age4) ont une moyenne significativement différente 
        et ne suivent donc pas une même distribution.")
    wilcoxon <- wilcox.test(value ~ age, data = DATA_pheno_rm)
    print( wilcoxon)
    
    ## MEDIANE AND MEAN
    
    #Plot of difference median 1 - median 4 
    dfmedian <- sapply( unique( DATA_pheno_rm$strain_number), function(x){
      median1 <- median(DATA_pheno_rm[ (DATA_pheno_rm$strain_number == x) & (DATA_pheno_rm$age == 1),]$value)
      median4 <- median(DATA_pheno_rm[ (DATA_pheno_rm$strain_number == x) & (DATA_pheno_rm$age == 4),]$value)
      return( c( "strain_number" = as.character(x), "median1" = median1, "median4" = median4))
    })
    dfmedian <- data.frame( t( dfmedian))
    dfmedian$median1 <- as.numeric( as.character( dfmedian$median1))
    dfmedian$median4 <- as.numeric( as.character( dfmedian$median4))
    dfmedian$diff <- dfmedian$median4 - dfmedian$median1
    dfmedian[["sign"]] <- ifelse(dfmedian[["diff"]] >= 0, "positive", "negative")
    strainorder <- dfmedian[order(dfmedian$diff),]$strain_number
    dfmedian$strain_number = factor( dfmedian$strain_number, levels = strainorder)
    
    #Plot of difference between Age1 et Age4 with median of each age 
    data1 <- dfmedian[, c( "strain_number", "median1")]
    data1[["age"]] = 1
    colnames( data1) <- c( "strain_number", "median", "age")
    data4 <- dfmedian[, c( "strain_number", "median4")]
    data4[["age"]] = 4
    colnames( data4) <- c( "strain_number", "median", "age")
    dfmedian_bis <- rbind.data.frame( data1, data4)
    
    #Fonction pour déterminer les outliers
    is_outlier <- function(x) {
      return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
    }
    
    outliers1 <- dfmedian_bis[is_outlier( dfmedian_bis[dfmedian_bis$age ==1,]$median) & dfmedian_bis$age ==1, "strain_number"]
    nameOutliers1 <- c(nameOutliers1, as.character(outliers1))
    outliers4 <- dfmedian_bis[is_outlier( dfmedian_bis[dfmedian_bis$age ==4,]$median) & dfmedian_bis$age ==4, "strain_number"]
    nameOutliers4 <- c(nameOutliers4, as.character( outliers4))
    
    cat("\nLes médianes des lignées pour les deux âges ont été calculées et mises sous forme d'un boxplot 
        comparable avec le précédent, il nous permet de voir la tendance générale ainsi que les lignées ayant 
        un comportement extrême, qui ont été labellisées.<br>")
    plotHRM14bis <- ggplot( dfmedian_bis) + 
                geom_violin( aes( x = as.factor( age), y = median, fill = as.factor(age))) +
                scale_fill_manual( values = c( "1"= "lemonchiffon3", "4"= "gray60" )) +
                geom_boxplot( aes( x = as.factor( age), y = median), width = 0.1) +
                geom_text_repel( 
                       data = dfmedian_bis[is_outlier( dfmedian_bis[dfmedian_bis$age ==1, ]$median) & dfmedian_bis$age ==1,],
                       aes(x = as.factor(age), y=median, label = strain_number), nudge_x = 0.2) +
                geom_text_repel( 
                  data = dfmedian_bis[is_outlier( dfmedian_bis[dfmedian_bis$age ==4,]$median) & dfmedian_bis$age ==4,],
                  aes(x = as.factor(age), y=median, label = strain_number), nudge_x = 0.2) +
                xlab( "Age") +
                ylab( "median of phenotype values of strains") +
                ggtitle( paste( namePheno, "\nmedian strains's values for each age")) +
                theme( plot.title = element_text( hjust = 0.5)) 
    print( plotHRM14bis)
    
    # Wilcoxon test on age 
    cat("<br>Le test de Wilcoxon a maintenant été appliqué aux médianes des lignées pour l'âge 1
        et l'âge. On a pu faire dans ce cas-ci un test paired.")
    wilcoxon2 <- wilcox.test(median ~ age, data = dfmedian_bis, paired = TRUE)
    print( wilcoxon2)
    
    #Qplot
    cat("\nLe graphique suivant est issu de la différence de la médiane âge4 moins celle de l'âge1, les lignées 
        ont été ordonnées suivant cette différence.<br>")
    qplotmedian <- qplot(data = dfmedian, x= strain_number, y = diff, col = sign) +
                   scale_fill_manual(values = c("positive" = "deepskyblue", "negative" = "red2")) +
                   geom_hline( aes( yintercept = 0), lty = 2)+
                   xlab( "Strains") +
                   ylab( "median4 - median1") +
                   ggtitle( paste( namePheno, "\ndifference mediane age 4 - 1 for each strain")) +
                   theme( plot.title = element_text( hjust = 0.5),
                          axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) 
    print( qplotmedian)
    
    #Correlation mediane4/ mediane1
    qplotmedian14 <- ggplot(dfmedian) +
                    geom_point( aes(x= median1, y = median4)) +
                    geom_abline( slope = 1, intercept = 0, col = "red", lwd = 1) +
                    ggtitle( paste( namePheno, "\ncorrelation median age 4 - 1 for each strain")) +
                    theme( legend.position = "none", plot.title = element_text( hjust = 0.5))
    
    #Same with mean but keeping the order of strains by median to compare
    dfmean <- sapply( unique( DATA_pheno_rm$strain_number), function(x){
      mean1 <- mean(DATA_pheno_rm[ (DATA_pheno_rm$strain_number == x) & (DATA_pheno_rm$age == 1), "value"])
      mean4 <- mean(DATA_pheno_rm[ (DATA_pheno_rm$strain_number == x) & (DATA_pheno_rm$age == 4), "value"])
      return( c( "strain_number" = as.character(x), "mean1" = mean1, "mean4" = mean4))
    })
    dfmean <- data.frame( t( dfmean))
    dfmean$mean1 <- as.numeric( as.character( dfmean$mean1))
    dfmean$mean4 <- as.numeric( as.character( dfmean$mean4))
    dfmean$diff <- dfmean$mean4 - dfmean$mean1
    dfmean[["sign"]] <- ifelse(dfmean[["diff"]] >= 0, "positive", "negative")
    dfmean$strain_number = factor( dfmean$strain_number, levels = strainorder)
    
    #Plot of diff mean4 -1 in order of diff median
    cat("<br>Le graphique suivant est issu de la différence de la moyenne âge4 moins celle de l'âge1, les lignées 
        ont été ordonnées avec le même classement que les médianes.<br>")
    qplotmean <- qplot(data = dfmean, x= strain_number, y = diff, col = sign) +
                   scale_fill_manual(values = c("positive" = "deepskyblue", "negative" = "red2")) +
                   geom_hline( aes( yintercept = 0), lty = 2)+
                   xlab( "Strains") +
                   ylab( "mean4 - mean1") +
                   ggtitle( paste( namePheno, "\ndifference mean age 4 - 1 for each strain")) +
                   theme( plot.title = element_text( hjust = 0.5),
                         axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) 
    print( qplotmean)
    
    #Correlation mean4/ mean1
    qplotmean14 <- ggplot(dfmean) +
                  geom_point( aes( x= mean1, y = mean4)) +
                  geom_abline( slope = 1, intercept = 0, col = "red", lwd = 1) +
                  ggtitle( paste( namePheno, "\ncorrelation mean age 4 - 1 for each strain")) +
                  theme( legend.position = "none", plot.title = element_text( hjust = 0.5))
    
    #Plot correlations side by side
    cat("<br>Ci dessous sont les correlations entre l'age 1 et l'âge 4 pour la médiane et la moyenne.
        On voit en effet que la moyenne est plus influencée par les valeurs extrèmes.<br>")
    grid.arrange( qplotmedian14, qplotmean14, ncol = 2 , nrow =1 )

    #Spearman correlation on median
    cat("<br>")
    spearman <- cor.test( x=dfmedian$median1, y = dfmedian$median4, method = "spearman")
    print(spearman)
    
    ## MAD AND SD 
    
    #Calcul of difference mad 4 - mad 1 
    dfmad <- sapply( unique( DATA_pheno_rm$strain_number), function(x){
      mad1 <- mad(DATA_pheno_rm[ (DATA_pheno_rm$strain_number == x) & (DATA_pheno_rm$age == 1),]$value)
      mad4 <- mad(DATA_pheno_rm[ (DATA_pheno_rm$strain_number == x) & (DATA_pheno_rm$age == 4),]$value)
      return( c( "strain_number" = as.character(x), "mad1" = mad1, "mad4" = mad4))
    })
    dfmad <- data.frame( t( dfmad))
    dfmad$mad1 <- as.numeric( as.character( dfmad$mad1))
    dfmad$mad4 <- as.numeric( as.character( dfmad$mad4))
    dfmad$diff <- dfmad$mad4 - dfmad$mad1
    dfmad[["sign"]] <- ifelse(dfmad[["diff"]] >= 0, "positive", "negative")
    strainordermad <- dfmad[order(dfmad$diff),]$strain_number
    dfmad$strain_number = factor( dfmad$strain_number, levels = strainordermad)
    
    #Plot of diff mad4 -1 in order of diff sd
    cat("<br>Le graphique suivant est issu de la différence de la mad âge4 moins celle de l'âge1, les lignées 
        ont été ordonnées suivant cette différence.<br>")
    qplotmad <- qplot(data = dfmad, x= strain_number, y = diff, col = sign) +
                scale_fill_manual(values = c("positive" = "deepskyblue", "negative" = "red2")) +
                geom_hline( aes( yintercept = 0), lty = 2)+
                xlab( "Strains") +
                ylab( "mad4 - mad1") +
                ggtitle( paste( namePheno, "\ndifference mad age 4 - 1 for each strain")) +
                theme( plot.title = element_text( hjust = 0.5),
                       axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) 
    print( qplotmad)
    
    #Correlation mad4/ mad1
    qplotmad14 <- ggplot( dfmad) +
                  geom_point( aes( x= mad1, y = mad4)) +
                  geom_abline( slope = 1, intercept = 0, col = "red", lwd = 1) +
                  ggtitle( paste( namePheno, "\ncorrelation mad age 4 - 1 for each strain")) +
                  theme( legend.position = "none", plot.title = element_text( hjust = 0.5))
    
    #Same with sd but keeping the order of strains by mad to compare
    dfsd <- sapply( unique( DATA_pheno_rm$strain_number), function(x){
      sd1 <- sd(DATA_pheno_rm[ (DATA_pheno_rm$strain_number == x) & (DATA_pheno_rm$age == 1),]$value)
      sd4 <- sd(DATA_pheno_rm[ (DATA_pheno_rm$strain_number == x) & (DATA_pheno_rm$age == 4),]$value)
      return( c( "strain_number" = as.character(x), "sd1" = sd1, "sd4" = sd4))
    })
    dfsd <- data.frame( t( dfsd))
    dfsd$sd1 <- as.numeric( as.character( dfsd$sd1))
    dfsd$sd4 <- as.numeric( as.character( dfsd$sd4))
    dfsd$diff <- dfsd$sd4 - dfsd$sd1
    dfsd[["sign"]] <- ifelse(dfsd[["diff"]] >= 0, "positive", "negative")
    dfsd$strain_number = factor( dfsd$strain_number, levels = strainordermad)
    
    #Qplot
    cat("<br>Le graphique suivant est issu de la différence de la sd âge4 moins celle de l'âge1, les lignées 
        ont été ordonnées avec le même classement que les mad.<br>")
    qplotsd <- qplot(data = dfsd, x= strain_number, y = diff, col = sign) +
              scale_fill_manual(values = c("positive" = "deepskyblue", "negative" = "red2")) +
              geom_hline( aes( yintercept = 0), lty = 2)+
              xlab( "Strains") +
              ylab( "sd4 - sd1") +
              ggtitle( paste( namePheno, "\ndifference sd age 4 - 1 for each strain")) +
              theme( plot.title = element_text( hjust = 0.5),
                     axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) 
    print( qplotsd)
    
    #Correlation sd4/ sd1
    qplotsd14 <- ggplot(dfsd) +
                geom_point( aes( x= sd1, y = sd4)) +
                geom_abline( slope = 1, intercept = 0, col = "red", lwd = 1) +
                ggtitle( paste( namePheno, "\ncorrelation sd age 4 - 1 for each strain")) +
                theme( legend.position = "none", plot.title = element_text( hjust = 0.5))
    
    #Plot correlations side by side
    cat("<br>Ci dessous sont les correlations entre l'age 1 et l'âge 4 pour la mad et le sd.
        On voit en effet que le sd est plus influencé par les valeurs extrèmes.<br>")
    grid.arrange( qplotmad14, qplotsd14, ncol = 2 , nrow =1 )
    
    #Spearman on mad
    cat("<br>")
    spearman2 <- cor.test( x=dfmad$mad1, y = dfmad$mad4, method = "spearman")
    print(spearman2)
    
    cat('<br><br><br><br><hr style=" height:3px; background-color:black"><br><br><br><br>')
  }
}

cat("<H4> Summary table </H4>")

#Summary of percent remaining strains for each phenotype, so if there are analyzed or not
kable( cbind.data.frame( "phenotypes" = phenotypes, "valid_strain" = formatC( RemainingStrain, digits =2, format = "f"  )), 
       caption = "**Percentage of remaining strains** : Under 80%, phenotypes have not been studied")

cat("<br>Les deux tables qui suivent montrent les lignées qui ont été au minimum deux fois extrêmes.<br>")

kable( table( nameOutliers1)[ table( nameOutliers1) > 1], caption = "Strains most occured as extrem, age 1",
       format = "html", align = NULL) %>%
       kable_styling(full_width = F)
kable( table( nameOutliers4)[ table( nameOutliers4) > 1], caption = "Strains most occured as extrem, age4",
       format = "html", align = NULL) %>%
       kable_styling(full_width = F)

#Cleaning
#rm( c( DATA_test, DATA_test_rm)) ENLEVER LES VALEURS NON UTILES


       