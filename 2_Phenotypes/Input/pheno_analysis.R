## @knitr phenoPlots

## Objective : loop of analysis for each phenotype with a high number
##of strains with more than 8 drosophiles for each age


#Preparing the loop
phenotypes <- as.character( sort( unique( DATA_analysis$phenotype_name)))
RemainingStrain <- c()
nameOutliersMed1 <- c()
nameOutliersMed4 <- c()
nameOutliersMad1 <- c()
nameOutliersMad4 <- c()
diffmedian <- data.frame( "strain_number" = unique( DATA_analysis$strain_number)) 
diffmedian_extrems1 <- data.frame( "strain_number" = unique( DATA_analysis$strain_number)) 
diffmedian_extrems4 <- data.frame( "strain_number" = unique( DATA_analysis$strain_number)) 
diffmad <-data.frame( "strain_number" = unique( DATA_analysis$strain_number))
diffmad_extrems1 <-data.frame( "strain_number" = unique( DATA_analysis$strain_number))
diffmad_extrems4 <-data.frame( "strain_number" = unique( DATA_analysis$strain_number))
dfmedian1 <- data.frame( "strain_number" = unique( DATA_analysis$strain_number)) 
dfmad1 <- data.frame( "strain_number" = unique( DATA_analysis$strain_number)) 
mad_python_off <- list()
median_python_off <- list()

#Function to determinate potential outliers, needed twice in the loop
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


#Loop to analyse each phenotype 
for (namePheno in phenotypes){

  #Subsetting for each phenotype strains with at least 8 drosophile by age 
  DATA_pheno <- DATA_analysis[ DATA_analysis$phenotype_name == namePheno,]
  tabDATA_pheno <- data.frame( table( DATA_pheno$strain_number, DATA_pheno$age))
  nameToRemove <- unique( as.character( tabDATA_pheno[ tabDATA_pheno$Freq <8, "Var1"]))
  idxToRemove <- which( DATA_pheno$strain_number %in% nameToRemove)
  if ( length( idxToRemove) != 0) {
    DATA_pheno_rm <- DATA_pheno[ -idxToRemove, ]
  } else {
    DATA_pheno_rm <- DATA_pheno
  }
 
  #Extracting dataframe used to analyse each phenotype
  write.table( DATA_pheno_rm, file=file.path( dirOutput, paste("2_Phenotypes/Output/Phenotypes/", namePheno, ".txt")))
    
  #The phenotype is not treated if there is not enought left data after the second filter  
  #We choose more than 80 % left 
  RemainingStrain_nb <- ( length( unique( DATA_pheno_rm$strain_number)) *100) / length( 
                                    unique( DATA_analysis$strain_number)) 
  RemainingStrain <- c( RemainingStrain, RemainingStrain_nb)
  if ( RemainingStrain_nb > 80){
       
    
    
    #Title for each phenotype
     cat("<H4>", namePheno, "</H4><br>")
    
    #Plot of remaining strains after the second filter 
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
  
    # Boxplot of all individuals values by strains
    # Plotted in order of increasing strains medians
    # One boxplot for each age 
    cat("<br>Les boxplots suivants sont les valeurs du phénotype étudié pour les individus de chaque lignée, ces dernières étant
        ordonnées selon leur médiane. Le premier graphique est pour l'âge 1, le second pour l'âge 4.<br>")
    DATA_pheno_rm1 <- DATA_pheno_rm[ DATA_pheno_rm$age == 1,]
    DATA_pheno_rm4 <- DATA_pheno_rm[ DATA_pheno_rm$age == 4,]
    lenStrain <- length( unique( DATA_pheno_rm$strain_number))
    plotpheno1 <- ggplot( DATA_pheno_rm1) + 
                    geom_boxplot( aes( x = reorder(x =strain_number, X = value, FUN =  median), y = value), 
                                  fill = rainbow( lenStrain)) +
                    xlab( "Strains") +
                    ylab( "Phenotype values") +
                    ggtitle( paste( namePheno, "age 1\nphenotype values between strains organized by median")) +
                    theme( plot.title = element_text( hjust = 0.5),
                           axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) 
    print( plotpheno1) 
    plotpheno4 <- ggplot( DATA_pheno_rm4) + 
                    geom_boxplot( aes( x = reorder(x =strain_number, X = value, FUN =  median), y = value),
                                       fill = rainbow(lenStrain)) +
                    xlab( "Strains") +
                    ylab( "Phenotype values") +
                    ggtitle( paste( namePheno, "age 4\nphenotype values between strains organized by median")) +
                    theme( plot.title = element_text( hjust = 0.5),
                           axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) 
    print( plotpheno4) 
    
    #Plot of the population difference between Age1 et Age4, all strains mixed
    cat("<br>Les valeurs phénotypiques de tous les individus sont maintenant organisées en fonction des âges, sans un regard à la 
        lignée, afin de voir si on a une tendance générale.<br>")
    plotpheno14 <- ggplot( DATA_pheno_rm) + 
                 geom_violin( aes( x = as.factor( age), y = value, fill = as.factor(age))) +
                 scale_fill_manual( values = c("1"= "lemonchiffon3", "4"= "gray60" )) +
                 geom_boxplot( aes( x = as.factor( age), y = value), width = 0.1) +
                 xlab( "Age") +
                 ylab( "Phenotype values") +
                 ggtitle( paste( namePheno, "\nphenotype values between ages")) +
                 theme( plot.title = element_text( hjust = 0.5)) 
    print( plotpheno14) 
  
    # Wilcoxon test on values of all individus, divided by age 
    # ie applied on data of the last plot
    cat("<br>Un premier test de Wilcoxon est appliquée à ces données. Si la p-valeur est inférieure à 0.05, 
        cela signifie que les deux populations (age1, age4) ont des moyennes significativement différentes 
        et ne suivent donc pas une même distribution.")
    wilcoxon <- wilcox.test(value ~ age, data = DATA_pheno_rm)
    print( wilcoxon)
    
    
    
    cat("<br><H5> MEDIANE </H5><br>")
    
    #Data frame containing mediane age 1, medianeage 4,  and difference median 4 - median 1 
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
    # We reorganized the strain order by the difference between medianes 
    # to facilitate the following ploting
    strainorder <- dfmedian[order(dfmedian$diff),]$strain_number
    dfmedian$strain_number = factor( dfmedian$strain_number, levels = strainorder)
    
    #Stocking the median difference for all strains and all phenotypes in a dataframe
    # Which will be used to treat metadata and to do heatmap 
    diffmedian <- merge.data.frame( x = diffmedian, y = dfmedian[, c("strain_number", "diff")], by = "strain_number", all = TRUE)
    colnames( diffmedian)[ ncol( diffmedian)] <- namePheno
    
    #Stocking the mediane for age 1 for all strains and phenotypes in a dataframe
    # Which will be used to treat metadata
    dfmedian1 <- merge.data.frame( x = dfmedian1, y = dfmedian[, c("strain_number", "median1")], by = "strain_number", all = TRUE)
    colnames( dfmedian1)[ ncol( dfmedian1)] <- namePheno
    
    #Change the format of dfmedian from wide to long, more easier to manipulate
    data1 <- dfmedian[, c( "strain_number", "median1")]
    data1[["age"]] = 1
    colnames( data1) <- c( "strain_number", "median", "age")
    data4 <- dfmedian[, c( "strain_number", "median4")]
    data4[["age"]] = 4
    colnames( data4) <- c( "strain_number", "median", "age")
    dfmedian_bis <- rbind.data.frame( data1, data4)
    
    #Files for python, to do the GWAS analysis 
    median_python <- dfmedian[, c("strain_number", "strain_number", "diff")]
    median_python_extrem <- median_python[ -which( is_outlier( median_python$diff)),]
    median_python_off[[namePheno]] <- median_python[ which( is_outlier( median_python$diff)), "strain_number"]
    median_python_df <- data.frame( sapply( median_python_extrem, function(x) {
      gsub("dgrp", "line_", x)
    }))
    write.table( median_python_df, file=file.path( dirOutput, paste("2_Phenotypes/Output/GWAS/", namePheno, 
                                                                    "_median_aging.txt", sep = "")), col.names = F, row.names = F, quote = F)
    write.table( median_python_df[,c(1,2)], file=file.path( dirOutput, paste("2_Phenotypes/Output/GWAS/", namePheno, 
                                                                             "_median_aging_families.txt", sep="")), col.names = F, row.names = F, quote = F)
    
    #Vectors of outliers strains names for age 1 and 4
    outliersMed1 <- dfmedian_bis[is_outlier( dfmedian_bis[dfmedian_bis$age ==1,]$median) & dfmedian_bis$age ==1, "strain_number"]
    nameOutliersMed1 <- c(nameOutliersMed1, as.character(outliersMed1))
    outliersMed4 <- dfmedian_bis[is_outlier( dfmedian_bis[dfmedian_bis$age ==4,]$median) & dfmedian_bis$age ==4, "strain_number"]
    nameOutliersMed4 <- c(nameOutliersMed4, as.character( outliersMed4))
    
    #Stocking extrem strains for heatmap : if the strain is extrem for namePheno, 
    #there is a 1 in the dataframe, elsewhere there is a 0
    diffmedian_extrems1[[ namePheno]] <- 0
    diffmedian_extrems1[ diffmedian_extrems1$strain_number %in% nameOutliersMed1, namePheno] <- 1
    diffmedian_extrems4[[ namePheno]] <- 0
    diffmedian_extrems4[ diffmedian_extrems4$strain_number %in% nameOutliersMed4, namePheno] <- 1
    
    # Boxplot of median values of strains by age, extrems points are labellised 
    cat("\nIci, on ne travaille plus avec les individus mais avec les lignées. Les médianes des lignées pour 
        les deux âges ont été calculées et mises sous forme d'un boxplot 
        comparable avec le précédent, il nous permet de voir la tendance générale ainsi que les lignées ayant 
        un comportement extrême, qui ont été labellisées.<br>")
    plotpheno14med <- ggplot( dfmedian_bis) + 
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
    print( plotpheno14med)
    
    # Wilcoxon test on last boxplot ie on medians of each strain, population divided by age
    cat("<br>Le test de Wilcoxon a maintenant été appliqué aux médianes des lignées pour l'âge 1
        et l'âge. On a pu faire dans ce cas-ci un test paired.")
    wilcoxon2 <- wilcox.test(median ~ age, data = dfmedian_bis, paired = TRUE)
    print( wilcoxon2)
    
    # #Same data with log and sqrt 
    # plotpheno14med_log <- ggplot( dfmedian_bis) + 
    #               geom_violin( aes( x = as.factor( age), y = log( median), fill = as.factor(age))) +
    #               scale_fill_manual( values = c( "1"= "lemonchiffon3", "4"= "gray60" )) +
    #               geom_boxplot( aes( x = as.factor( age), y = log(median)), width = 0.1) +
    #               xlab( "Age") +
    #               ylab( "log median of phenotype values of strains") +
    #               theme( plot.title = element_text( hjust = 0.5), legend.position = "none") 
    # plotpheno14med_sqrt <- ggplot( dfmedian_bis) + 
    #               geom_violin( aes( x = as.factor( age), y = sqrt( median), fill = as.factor(age))) +
    #               scale_fill_manual( values = c( "1"= "lemonchiffon3", "4"= "gray60" )) +
    #               geom_boxplot( aes( x = as.factor( age), y = sqrt(median)), width = 0.1) +
    #               xlab( "Age") +
    #               ylab( "sqrt median of phenotype values of strains") +
    #               theme( plot.title = element_text( hjust = 0.5), legend.position = "none")
    # titlemed <- paste( namePheno, "\n log et sqrt mediane age 4 - 1 for each strain")
    # grid.arrange( plotpheno14med_log, plotpheno14med_sqrt, ncol = 2, top = titlemed)
    
    #Shapiro test on these data
    # cat("<br>Un shapiro test est appliqué pour voir si une des deux transformations (log et 
    #     racine carrée = sqrt) présente une distribution plus normale que les données réelles.<br>")
    # shapiro_list <- list()
    # shapiro_list_med <- do.call("rbind.data.frame",
    #                                  lapply( unique( dfmedian_bis$age), function( ages) {
    #                                    shapiro <- shapiro.test( dfmedian_bis[dfmedian_bis$age == ages,]$median)
    #                                    shapiro_list <- list( Value = "real", age = ages, W = shapiro$statistic, pvalue =shapiro$p.value)
    #                                    return( shapiro_list)
    #                                  }))   
    # shapiro_list_med_sqrt <- do.call("rbind.data.frame",
    #                                  lapply( unique( dfmedian_bis$age), function( ages) {
    #                                    shapiro <- shapiro.test( sqrt( dfmedian_bis[dfmedian_bis$age == ages,]$median))
    #                                    shapiro_list <- list( Value = "sqrt", age = ages, W = shapiro$statistic, pvalue =shapiro$p.value)
    #                                    return( shapiro_list)
    #                                  }))   
    # shapiro_list_med_log <- do.call("rbind.data.frame",
    #                                 lapply( unique( dfmedian_bis$age), function( ages) {
    #                                   shapiro <- shapiro.test( log( dfmedian_bis[dfmedian_bis$age == ages,]$median))
    #                                   shapiro_list <- list( Value = "log", age = ages, W = shapiro$statistic, pvalue =shapiro$p.value)
    #                                   return( shapiro_list)
    #                                 }))   
    # shapiro_med <- rbind.data.frame( shapiro_list_med, shapiro_list_med_log, shapiro_list_med_sqrt)
    # print( kable( shapiro_med, format = "html", row.names = F)  %>%
    #          kable_styling(full_width = F, position = "left"))
    # 
    # Plots of mediane 4 - mediane 1 for each strains, organized by increasing value
    # Same values are plotted in form of qplot and boxplot
    cat("<br>Les graphiques suivants sont issus de la différence de la médiane âge 4 moins celle de l'âge 1, et représentent
        les mêmes données sous des formes différentes. Les lignées du graphique de droite ont été ordonnées suivant 
        cette différence.<br>")
    qplotmedian <- qplot(data = dfmedian, x= strain_number, y = diff, col = sign) +
                   scale_fill_manual(values = c("positive" = "deepskyblue", "negative" = "red2")) +
                   geom_hline( aes( yintercept = 0), lty = 2)+
                   xlab( "Strains") +
                   ylab( "median4 - median1") +
                   theme( axis.text.x=element_blank(), legend.position ="bottom") 
    qplotmedianbox <- ggplot( dfmedian) + 
                      geom_boxplot( aes( x = "strains", y = diff), fill = "gray82") +
                      geom_text_repel( 
                          data = dfmedian[is_outlier( dfmedian$diff),],
                          aes(x = "strains", y = diff, label = strain_number), nudge_x = 0.2) 
    titlemed2 <- paste( namePheno, "\ndifference mediane age 4 - 1 for each strain")
    grid.arrange( qplotmedianbox, qplotmedian, ncol = 2, top = titlemed2)
    
    #Graph of correlation between mediane4 and mediane1
    cat("<br>Ci-dessous, on voit la correlation entre l'âge 1 et l'âge 4 pour la médiane. <br>")
    qplotmedian14 <- ggplot(dfmedian) +
                    geom_point( aes(x= median1, y = median4)) +
                    geom_smooth( aes( x = median1, y = median4), method=lm, se=FALSE, col= "red")
                    ggtitle( paste( namePheno, "\ncorrelation median age 4 - 1 for each strain")) +
                    theme( legend.position = "none", plot.title = element_text( hjust = 0.5))
    print(qplotmedian14)

    #Spearman correlation between median 1 and median 4
    cat("<br>Coefficient de correlation de Spearman pour la médiane.")
    spearman <- cor.test( x=dfmedian$median1, y = dfmedian$median4, method = "spearman")
    print(spearman)
    
  
    
    cat("<br><H5> MAD </H5><br>")   #We will do exactly same thing than for median, except there 
                                    # is no creation of file for python
    
    #Data frame containing mad age 1, mad age 4,  and difference mad 4 - mad 1 
    dfmad <- sapply( unique( DATA_pheno_rm$strain_number), function(x){
      mad1 <- mad(DATA_pheno_rm[ (DATA_pheno_rm$strain_number == x) & (DATA_pheno_rm$age == 1),]$value)
      mad4 <- mad(DATA_pheno_rm[ (DATA_pheno_rm$strain_number == x) & (DATA_pheno_rm$age == 4),]$value)
      return( c( "strain_number" = as.character(x), "mad1" = mad1, "mad4" = mad4))
    })
    dfmad <- data.frame( t( dfmad))
    dfmad$mad1 <- as.numeric( as.character( dfmad$mad1))
    dfmad$mad4 <- as.numeric( as.character( dfmad$mad4))
    dfmad$diff <- dfmad$mad4 - dfmad$mad1
    dfmad[["sign"]] <- ifelse( dfmad[["diff"]] >= 0, "positive", "negative")
    strainordermad <- dfmad[order(dfmad$diff),]$strain_number
    dfmad$strain_number = factor( dfmad$strain_number, levels = strainordermad)
    
    #Stocking the mad difference for all strains and all phenotypes in a dataframe
    # Which will be used to treat metadata and to do heatmap 
    diffmad <- merge.data.frame( x = diffmad, y = dfmad[, c("strain_number", "diff")], by = "strain_number", all = TRUE)
    colnames( diffmad)[ ncol( diffmad)] <- namePheno
    
    #Stocking the mad for age 1 for all strains and phenotypes in a dataframe
    # Which will be used to treat metadata
    dfmad1 <- merge.data.frame( x = dfmad1, y = dfmad[, c("strain_number", "mad1")], by = "strain_number", all = TRUE)
    colnames( dfmad1)[ ncol( dfmad1)] <- namePheno

    #Change the format of dfmad from wide to long, more easier to manipulate
    data1 <- dfmad[, c( "strain_number", "mad1")]
    data1[["age"]] = 1
    colnames( data1) <- c( "strain_number", "mad", "age")
    data4 <- dfmad[, c( "strain_number", "mad4")]
    data4[["age"]] = 4
    colnames( data4) <- c( "strain_number", "mad", "age")
    dfmad_bis <- rbind.data.frame( data1, data4)
    
    #Files for python, to do the GWAS analysis 
    mad_python <- dfmad[, c("strain_number", "strain_number", "diff")]
    mad_python_extrem <- mad_python[ -which( is_outlier( mad_python$diff)),]
    mad_python_off[[namePheno]] <- mad_python[ which( is_outlier( mad_python$diff)), "strain_number"] 
    mad_python_df <- data.frame( sapply( mad_python_extrem, function(x) {
      gsub("dgrp", "line_", x)
    }))
    write.table( mad_python_df, file=file.path( dirOutput, paste("2_Phenotypes/Output/GWAS/", namePheno, 
                                                                    "_mad_aging.txt", sep = "")), col.names = F, row.names = F, quote = F)
    write.table( mad_python_df[,c(1,2)], file=file.path( dirOutput, paste("2_Phenotypes/Output/GWAS/", namePheno, 
                                                                             "_mad_aging_families.txt", sep="")), col.names = F, row.names = F, quote = F)
  
    
    #Vectors of outliers strains names for age 1 and 4
    outliersMad1 <- dfmad_bis[is_outlier( dfmad_bis[dfmad_bis$age ==1,]$mad) & dfmad_bis$age ==1, "strain_number"]
    nameOutliersMad1 <- c(nameOutliersMad1, as.character(outliersMad1))
    outliersMad4 <- dfmad_bis[is_outlier( dfmad_bis[dfmad_bis$age ==4,]$mad) & dfmad_bis$age ==4, "strain_number"]
    nameOutliersMad4 <- c(nameOutliersMad4, as.character( outliersMad4))
    
    #Stocking extrem strains for heatmap : if the strain is extrem for namePheno, 
    #there is a 1 in the dataframe, elsewhere there is a 0
    diffmad_extrems1[[ namePheno]] <- 0
    diffmad_extrems1[ diffmad_extrems1$strain_number %in% nameOutliersMad1, namePheno] <- 1
    diffmad_extrems4[[ namePheno]] <- 0
    diffmad_extrems4[ diffmad_extrems4$strain_number %in% nameOutliersMad4, namePheno] <- 1
    
    # Boxplot of mad values of strains by age, extrems points are labellised 
    cat("\ De même que pour la médiane, on ne travaille plus avec les individus mais avec les lignées. 
        Les mad des lignées pour les deux âges ont été calculées et mises sous forme d'un boxplot 
        comparable avec le précédent, il nous permet de voir la tendance générale ainsi que les lignées ayant 
        un comportement extrême, qui ont été labellisées.<br>")
    plotpheno14mad <- ggplot( dfmad_bis) + 
                      geom_violin( aes( x = as.factor( age), y = mad, fill = as.factor(age))) +
                      scale_fill_manual( values = c( "1"= "lemonchiffon3", "4"= "gray60" )) +
                      geom_boxplot( aes( x = as.factor( age), y = mad), width = 0.1) +
                      geom_text_repel( 
                        data = dfmad_bis[is_outlier( dfmad_bis[dfmad_bis$age ==1, ]$mad) & dfmad_bis$age ==1,],
                        aes(x = as.factor(age), y=mad, label = strain_number), nudge_x = 0.2) +
                      geom_text_repel( 
                        data = dfmad_bis[is_outlier( dfmad_bis[dfmad_bis$age ==4,]$mad) & dfmad_bis$age ==4,],
                        aes(x = as.factor(age), y=mad, label = strain_number), nudge_x = 0.2) +
                      xlab( "Age") +
                      ylab( "mad of phenotype values of strains") +
                      ggtitle( paste( namePheno, "\nmad strains's values for each age")) +
                      theme( plot.title = element_text( hjust = 0.5)) 
    print( plotpheno14mad)
    
    # Wilcoxon test on last boxplot ie on mad of each strain, population divided by age
    cat("<br>Le test de Wilcoxon a maintenant été appliqué aux mads des lignées pour l'âge 1
        et l'âge. On a pu faire dans ce cas-ci aussi un test paired.")
    wilcoxon3 <- wilcox.test(mad ~ age, data = dfmad_bis, paired = TRUE)
    print( wilcoxon3)
    
    # #Same data with log and sqrt 
    # plotpheno14mad_log <- ggplot( dfmad_bis) + 
    #                   geom_violin( aes( x = as.factor( age), y = log( mad), fill = as.factor(age))) +
    #                   scale_fill_manual( values = c( "1"= "lemonchiffon3", "4"= "gray60" )) +
    #                   geom_boxplot( aes( x = as.factor( age), y = log(mad)), width = 0.1) +
    #                   xlab( "Age") +
    #                   ylab( "log mad of phenotype values of strains") +
    #                   theme( plot.title = element_text( hjust = 0.5), legend.position = "none") 
    # plotpheno14mad_sqrt <- ggplot( dfmad_bis) + 
    #                   geom_violin( aes( x = as.factor( age), y = sqrt( mad), fill = as.factor(age))) +
    #                   scale_fill_manual( values = c( "1"= "lemonchiffon3", "4"= "gray60" )) +
    #                   geom_boxplot( aes( x = as.factor( age), y = sqrt(mad)), width = 0.1) +
    #                   xlab( "Age") +
    #                   ylab( "sqrt mad of phenotype values of strains") +
    #                   theme( plot.title = element_text( hjust = 0.5), legend.position = "none")
    # titlemad <- paste( namePheno, "\n log et sqrt mad age 4 - 1 for each strain")
    # grid.arrange( plotpheno14mad_log, plotpheno14mad_sqrt, ncol = 2, top = titlemad)
    # 
    # #Shapiro test on new data 
    # # cat("Shapiro test for log( mad) then sqrt( mad)")
    # cat("<br>Un shapiro test est appliqué pour voir si une des deux transformations (log et 
    #     racine carrée = sqrt) présente une distribution plus normale que les données réelles.<br>")
    # shapiro_list <- list()
    # shapiro_list_mad <- do.call("rbind.data.frame",
    #                             lapply( unique( dfmad_bis$age), function( ages) {
    #                             shapiro <- shapiro.test( dfmad_bis[dfmad_bis$age == ages,]$mad)
    #                             shapiro_list <- list( Value = "real", age = ages, W = shapiro$statistic, pvalue =shapiro$p.value)
    #                             return( shapiro_list)
    #                             }))   
    # shapiro_list_mad_sqrt <- do.call("rbind.data.frame",
    #                                   lapply( unique( dfmad_bis$age), function( ages) {
    #                                   shapiro <- shapiro.test( sqrt( dfmad_bis[dfmad_bis$age == ages,]$mad))
    #                                   shapiro_list <- list( Value = "sqrt", age = ages, W = shapiro$statistic, pvalue =shapiro$p.value)
    #                                   return( shapiro_list)
    # }))   
    # shapiro_list_mad_log <- do.call("rbind.data.frame",
    #                                  lapply( unique( dfmad_bis$age), function( ages) {
    #                                    shapiro <- shapiro.test( log( dfmad_bis[dfmad_bis$age == ages,]$mad))
    #                                    shapiro_list <- list( Value = "log", age = ages, W = shapiro$statistic, pvalue =shapiro$p.value)
    #                                    return( shapiro_list)
    #                                  }))   
    # shapiro_mad <- rbind.data.frame( shapiro_list_mad, shapiro_list_mad_log, shapiro_list_mad_sqrt)
    # print( kable( shapiro_mad, format = "html", row.names = F)  %>%
    #          kable_styling(full_width = F, position = "left"))
    
    # Plots of mad 4 - mad 1 for each strains, organized by increasing value
    # Same values are plotted in form of qplot and boxplot
    cat("<br>Les graphique suivant sont issus de la différence de la mad âge 4 moins celle de l'âge1, et représentent
        les mêmes données sous des formes différentes. Les lignées du graphique de droite ont été ordonnées suivant 
        cette différence.<br>")
    qplotmad <- qplot(data = dfmad, x= strain_number, y = diff, col = sign) +
                scale_fill_manual(values = c("positive" = "deepskyblue", "negative" = "red2")) +
                geom_hline( aes( yintercept = 0), lty = 2)+
                xlab( "Strains") +
                ylab( "mad4 - mad1") +
                theme( axis.text.x = element_blank(), legend.position = "bottom") 
    qplotmadbox <- ggplot( dfmad) + 
                   geom_boxplot( aes( x = "strains", y = diff), fill = "gray82") +
                   geom_text_repel( 
                      data = dfmad[is_outlier( dfmad$diff),],
                      aes(x = "strains", y = diff, label = strain_number), nudge_x = 0.2) 
    titlemad2 <- paste( namePheno, "\ndifference mad age 4 - 1 for each strain")
    grid.arrange( qplotmadbox, qplotmad, ncol = 2, top = titlemad2)
    
    #Graph of correlation between mad 1 and mad 4
    cat("<br>Ci-dessous, on voit la correlation entre l'âge 1 et l'âge 4 pour la mad. <br>")
    qplotmad14 <- ggplot( dfmad) +
                  geom_point( aes( x= mad1, y = mad4)) +
                  geom_smooth( aes( x = mad1, y = mad4), method=lm, se=FALSE, col="red") +
                  ggtitle( paste( namePheno, "\ncorrelation mad age 4 - 1 for each strain")) +
                  theme( legend.position = "none", plot.title = element_text( hjust = 0.5))
    print(qplotmad14)
    
    #Spearman correlation between median 1 and median 4
    cat("<br>Coefficient de correlation de Spearman pour la mad.")
    spearman2 <- cor.test( x=dfmad$mad1, y = dfmad$mad4, method = "spearman")
    print(spearman2)
    
    
    cat("<br><H5> CORRELATION MEDIANE MAD </H5><br>")  # Aim of this part is to see if there is any correlation 
                                                      #between evolution of median and mad in growing up process
    
    # Plot of mad 4- mad 1 by median 4- median 1
    cat("La corrélation médiane mad affichée ci-dessous est en réalité la corrélation de la différence médiane4 -
        médiane1 et de la différence mad4 - mad1 pour chacune des lignées. <br>")
    mgmedmad <- merge.data.frame( x = dfmedian, y = dfmad, by.x = "strain_number", by.y = "strain_number" )
    colnames( mgmedmad)[c(4,8)] <- c( "diff.median", "diff.mad")
    plotmedmad <-  ggplot ( mgmedmad) +
                   geom_point( aes( x = diff.median, y = diff.mad)) + 
                   geom_smooth( aes( x = diff.median, y = diff.mad), method=lm, se=FALSE) +
                   ggtitle( paste( namePheno, "\ncorrelation median age 4 - 1 for each strain")) +
                   theme( plot.title = element_text( hjust = 0.5))
    print( plotmedmad)
    
    #The Spearman correlation is determinated between difference of medians and difference of mads 
    cat("<br>Coefficient de correlation de Spearman pour la correlation médiane mad.")
    spearman3 <- cor.test( x=mgmedmad$diff.median, y = mgmedmad$diff.mad, method = "spearman")
    print(spearman3)
    
    
    # Splicing with the next phenotype by a black line and spaces
    cat('<br><br><br><br><hr style=" height:3px; background-color:black"><br><br><br><br>')
  }
}
  
#Creation of a data frame for the execution path for GWAS for median files 
execution_dataframe_median <- do.call("rbind.data.frame", 
                      lapply( phenotypes, function(namePheno) {
                      input <- "/home/ladebese/workspace/AnalysisSteph/2_Phenotypes/Output/GWAS"
                      genotype <- "dgrp2_aging"
                      covariable <- "/home/ladebese/workspace/Data/DGRP/cov_wolbachia_inversions.txt"
                      phenotype <- paste( namePheno, "_median_aging.txt", sep = "")
                      dgrp <- "/home/ladebese/workspace/Data/DGRP/dgrp2"
                      families <- paste( "/home/ladebese/workspace/AnalysisSteph/2_Phenotypes/Output/GWAS/", 
                      namePheno, "_median_aging_families.txt", sep = "")
                      execution <- list( input = input, genotype = genotype, covariable = covariable, phenotype = phenotype,
                                         dgrp = dgrp, families = families)
                      return( execution)
                    }))

#Creation of a data frame for the execution path for GWAS for mad files
execution_dataframe_mad <- do.call("rbind.data.frame", 
                                      lapply( phenotypes, function(namePheno) {
                                        input <- "/home/ladebese/workspace/AnalysisSteph/2_Phenotypes/Output/GWAS"
                                        genotype <- "dgrp2_aging"
                                        covariable <- "/home/ladebese/workspace/Data/DGRP/cov_wolbachia_inversions.txt"
                                        phenotype <- paste( namePheno, "_mad_aging.txt", sep = "")
                                        dgrp <- "/home/ladebese/workspace/Data/DGRP/dgrp2"
                                        families <- paste( "/home/ladebese/workspace/AnalysisSteph/2_Phenotypes/Output/GWAS/", 
                                                           namePheno, "_mad_aging_families.txt", sep = "")
                                        execution <- list( input = input, genotype = genotype, covariable = covariable, phenotype = phenotype,
                                                           dgrp = dgrp, families = families)
                                        return( execution)
                                      }))

#Merging of both data frame in a unique execution path
execution_dataframe <- rbind.data.frame( execution_dataframe_mad, execution_dataframe_median)

#File to be imported 
write.csv( dfmedian1, file = "~/workspace/AnalysisSteph/2_Phenotypes/Output/median_age1.csv")  
write.csv( dfmad1, file = "~/workspace/AnalysisSteph/2_Phenotypes/Output/mad_age1.csv")  
write.csv( diffmedian, file = "~/workspace/AnalysisSteph/2_Phenotypes/Output/diffmedian.csv")  
write.csv( diffmad, file = "~/workspace/AnalysisSteph/2_Phenotypes/Output/diffmad.csv")  
write.csv( execution_dataframe, file = "~/workspace/AnalysisSteph/2_Phenotypes/Output/aging_analysis_gwas.csv", row.names = F)

#Cleaning
# rm ( is_outlier, DATA_pheno, tabDATA_pheno, nameToRemove, idxToRemove, DATA_pheno_rm, RemainingStrain_nb,
#      tabDATA_pheno_rm, plotStrainpheno, DATA_pheno_rm1, DATA_pheno_rm4, lenStrain, plotpheno1,
#      plotpheno4, plotpheno14, wilcoxon, dfmedian, strainorder, data1, data4,
#      dfmedian_bis, outliersMed1, outliersMed4, plotpheno14med, wilcoxon2, qplotmedian,
#      qplotmedianbox, qplotmedian14, spearman, dfmad, strainordermad, dfmad_bis, outliersMad1,
#      outliersMad4, plotpheno14mad, qplotmad, qplotmadbox, qplotmad14, spearman2, mgmedmad,
#      plotmedmad, spearman3, namePheno, wilcoxon3, execution_dataframe_mad, execution_dataframe_median,
#      mad_python, median_python)

#Created and kept variables 
# diffmad : data frame with the difference mad 4-1 for all phenotypes 
# diffmad1 : data frame with the mad age 1 for all phenotypes 
# diffmad_extrems1 : data frame with extrems strains indicated for mad age 1
# diffmad_extrems4 : data frame with extrems strains indicated for mad age 4
# diffmedian : data frame with the difference median 4-1 for all phenotypes 
# diffmedian1 : data frame with the median age 1 for all phenotypes
# diffmedian_extrems1 : data frame with extrems strains indicated for median age 1
# diffmedian_extrems4 : data frame with extrems strains indicated for median age 4
# execution_dataframe : data frame with all indications to run the GWAS 
# mad_python_df : data frame used to run GWAS
# median_python_df : data frame used to run GWAS
# nameOutliersMad1 : names of "extrem strains" for the mad and for age 1
# nameOutliersMed1 : names of "extrem strains" for the median and for age 1
# nameOutliersMed4 : names of "extrem strains" for the median and for age 4
# phenotypes : character vector with names of all phenotypes 
# RemainingStrain : Percent of remaining strains for all phenotypes, in order of "phenotypes"



    
     
     

       