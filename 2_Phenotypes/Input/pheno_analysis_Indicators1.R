## @knitr phenoPlots

## Objective : loop of analysis for each phenotype with a high number
##of strains with more than 8 drosophiles for each age


#Preparing the loop
phenotypes <- as.character( sort( unique( DATA_analysis$phenotype_name)))
RemainingStrain <- c()
nameOutliersMed1 <- c()
nameOutliersMed4 <- c()
nameOutliersIM <- c()
nameOutliersMad1 <- c()
nameOutliersMad4 <- c()
nameOutliersIV <- c()
diffIM <- data.frame( "strain_number" = unique( DATA_analysis$strain_number)) 
diffmedian_extrems1 <- data.frame( "strain_number" = unique( DATA_analysis$strain_number)) 
diffmedian_extrems4 <- data.frame( "strain_number" = unique( DATA_analysis$strain_number)) 
diffmedian_extremsIM <- data.frame( "strain_number" = unique( DATA_analysis$strain_number)) 
diffIV <-data.frame( "strain_number" = unique( DATA_analysis$strain_number))
diffmad_extrems1 <-data.frame( "strain_number" = unique( DATA_analysis$strain_number))
diffmad_extrems4 <-data.frame( "strain_number" = unique( DATA_analysis$strain_number))
diffmad_extremsIV <-data.frame( "strain_number" = unique( DATA_analysis$strain_number))
diffmedian1 <- data.frame( "strain_number" = unique( DATA_analysis$strain_number)) 
diffmad1 <- data.frame( "strain_number" = unique( DATA_analysis$strain_number)) 
mad_python_off <- list()
median_python_off <- list()
dfgaussien <- data.frame()
#Function to determinate potential outliers/ extrem data
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
is_outlier_inf <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x))
}
is_outlier_sup <- function(x) {
  return(x > quantile(x, 0.75) + 1.5 * IQR(x))
}


#Loop to analyse every phenotype 
for ( namePheno in phenotypes){

  #Subsetting strains with at least 8 drosophile by age for each phenotype  
  DATA_pheno <- DATA_analysis[ DATA_analysis$phenotype_name == namePheno,]
  tabDATA_pheno <- data.frame( table( DATA_pheno$strain_number, DATA_pheno$age))
  nameToRemove <- unique( as.character( tabDATA_pheno[ tabDATA_pheno$Freq <8, "Var1"]))
  idxToRemove <- which( DATA_pheno$strain_number %in% nameToRemove)
  #If there is no strain to remove, we keep the same data frame 
  if ( length( idxToRemove) != 0) {
    DATA_pheno_rm <- DATA_pheno[ -idxToRemove, ]
  } else {
    DATA_pheno_rm <- DATA_pheno
  }
  #### Extracting dataframe used to analyse each phenotype ####
  write.table( DATA_pheno_rm, file=file.path( dirOutput, paste("2_Phenotypes/Output/Phenotypes/", namePheno, ".txt")))
    
  
  #Condition : The phenotype is not treated if there is not enought left data after the second filter  
  #We choose more than 80 % left 
  RemainingStrain_nb <- ( length( unique( DATA_pheno_rm$strain_number)) *100) / length( 
                                    unique( DATA_analysis$strain_number)) 
  RemainingStrain <- c( RemainingStrain, RemainingStrain_nb)
  if ( RemainingStrain_nb > 80){
       
    
    #Title for each phenotype
    cat( "<H4>", namePheno, "</H4><br>")
    
    
    #Plot of remaining strains after the second filter 
    cat( "<br>Ci-dessous est affiché le barplot des lignées restantes après le second filtre. Il reste", 
        RemainingStrain_nb, "% des lignées de DATA_Analysis.<br>")
    tabDATA_pheno_rm <- data.frame( table( DATA_pheno_rm$strain_number, DATA_pheno_rm$age))
    colnames( tabDATA_pheno_rm) <- c( "strain_number", "age", "freq")
    plotStrainpheno <- ggplot( tabDATA_pheno_rm, aes( x= strain_number, y= freq, fill = as.factor( age))) +
                       geom_bar( position = "dodge", stat = "identity") +
                       coord_cartesian( ylim = c( 0,25)) +
                       xlab( "Strains") +
                       ylab( "Frequency") +
                       ggtitle( paste( namePheno,"\nNumber of drosophila by strain and age")) +
                       theme( plot.title = element_text( hjust = 0.5),
                              axis.text.x = element_text( angle = 90, hjust = 1, size = 7),
                              legend.position = "bottom") +
                       geom_hline( aes( yintercept = 8), lty = 2)
    print( plotStrainpheno)
  
    
    #Boxplot of all individuals values by strains
    #Plotted in order of increasing strains medians
    #Boxplot for values age 1
    cat( "<br>Les boxplots suivants sont les valeurs du phénotype étudié pour les individus de chaque lignée, ces dernières étant
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
    #Boxplot for values age 4
    plotpheno4 <- ggplot( DATA_pheno_rm4) + 
                    geom_boxplot( aes( x = reorder(x =strain_number, X = value, FUN =  median), y = value),
                                       fill = rainbow(lenStrain)) +
                    xlab( "Strains") +
                    ylab( "Phenotype values") +
                    ggtitle( paste( namePheno, "age 4\nphenotype values between strains organized by median")) +
                    theme( plot.title = element_text( hjust = 0.5),
                           axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) 
    print( plotpheno4) 
    
    
    #Plot of the population values at Age1 and Age4, all strains mixed
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
    
    
    #Wilcoxon test on values of all individus, divided by age 
    #ie applied on data of the last plot
    cat( "<br>Un premier test de Wilcoxon est appliqué à ces données. Si la p-valeur est inférieure à 0.05, 
        cela signifie que les deux populations (age1, age4) ont des moyennes significativement différentes 
        et ne suivent donc pas une même distribution.")
    wilcoxon <- wilcox.test(value ~ age, data = DATA_pheno_rm)
    print( wilcoxon)
    #If there is significativity previously, the kind of the main tendency (increase or decrease)
    #is determinated for the population
    if ( wilcoxon$p.value < 0.05){
      mean1 <- mean( DATA_pheno_rm[ DATA_pheno_rm$age == 1, "value"])
      mean4 <- mean( DATA_pheno_rm[ DATA_pheno_rm$age == 4, "value"])
      meanvalue <- mean4 - mean1
      if ( meanvalue > 0) {
        action <- "augmenter"
      } else {
        action <- "diminuer"
      }
      #Print results
      cat ("<br> Il existe une différence significative d'après le test de Wilcoxon. La valeur phénotypique de", namePheno, "tend à", 
           action,"avec l'âge au sein de la population.<br>")
    }
    
    
    
    cat("<br><H5> INDICATEUR DE MEDIANE </H5><br>")
    
    
    #Data frame containing mediane age 1, median eage 4,and (median 4 - median 1) / mediane 1
    dfIM <- sapply( unique( DATA_pheno_rm$strain_number), function(x){
      median1 <- median( DATA_pheno_rm[ (DATA_pheno_rm$strain_number == x) & (DATA_pheno_rm$age == 1), "value"])
      median4 <- median( DATA_pheno_rm[ (DATA_pheno_rm$strain_number == x) & (DATA_pheno_rm$age == 4), "value"])
      return( c( "strain_number" = as.character(x), "median1" = median1, "median4" = median4))
    })
    dfIM <- data.frame( t( dfIM))
    dfIM$median1 <- as.numeric( as.character( dfIM$median1))
    dfIM$median4 <- as.numeric( as.character( dfIM$median4))
    dfIM$diff <- ( dfIM$median4 - dfIM$median1) / dfIM$median1
    dfIM[[ "sign"]] <- ifelse( dfIM[[ "diff"]] >= 0, "positive", "negative")
    # We reorganized the strain order by the Im to facilitate the following ploting
    strainorder <- dfIM[ order( dfIM$diff), "strain_number"]
    dfIM$strain_number = factor( dfIM$strain_number, levels = strainorder)
    
    
    #### Stocking the IM for all strains and phenotypes in a dataframe
    #### Which will be used for metadata 
    diffIM <- merge.data.frame( x = diffIM, y = dfIM[ , c( "strain_number", "diff")], by = "strain_number", all = TRUE)
    colnames( diffIM)[ ncol( diffIM)] <- namePheno
    #### Stocking the mediane for age 1 for all strains and phenotypes in a dataframe
    #### Which will be used for metadata 
    diffmedian1 <- merge.data.frame( x = diffmedian1, y = dfIM[, c("strain_number", "median1")], by = "strain_number", all = TRUE)
    colnames( diffmedian1)[ ncol( diffmedian1)] <- namePheno  
    
    
    #Change the format of dfIM from wide to long, more easier to manipulate
    data1 <- dfIM[, c( "strain_number", "median1")]
    data1[[ "age"]] = 1
    colnames( data1) <- c( "strain_number", "median", "age")
    data4 <- dfIM[ , c( "strain_number", "median4")]
    data4[[ "age"]] = 4
    colnames( data4) <- c( "strain_number", "median", "age")
    dfIM_bis <- rbind.data.frame( data1, data4)
    
    
    #### Files for python, to do the GWAS analysis ####  VOIR POUR CHANGER LES NOMS ###########################################
    median_python <- dfIM[ , c( "strain_number", "strain_number", "diff")]
    median_python_extrem <- median_python[ -which( is_outlier( median_python$diff)),]
    median_python_off[[namePheno]] <- median_python[ which( is_outlier( median_python$diff)), "strain_number"]
    median_python_df <- data.frame( sapply( median_python_extrem, function(x) {
      gsub("dgrp", "line_", x)
    }))
    write.table( median_python_df, file=file.path( dirOutput, paste("2_Phenotypes/Output/GWAS/", namePheno, 
                                                                    "_median_aging.txt", sep = "")), col.names = F, row.names = F, quote = F)
    write.table( median_python_df[,c(1,2)], file=file.path( dirOutput, paste("2_Phenotypes/Output/GWAS/", namePheno, 
                                                                             "_median_aging_families.txt", sep="")), col.names = F, row.names = F, quote = F)
    
    
    #Extrem strains for median, age 1 and 4 
    #Names of all inferior and superior extrems strains are collected for the current phenotype
    #All names are collected in a vector nameOutliers for the table of extrem strains
    outliersMed1inf <- dfIM_bis[ is_outlier_inf( dfIM_bis[ dfIM_bis$age ==1,]$median) & dfIM_bis$age == 1, "strain_number"]
    outliersMed1sup <- dfIM_bis[ is_outlier_sup( dfIM_bis[ dfIM_bis$age ==1,]$median) & dfIM_bis$age == 1, "strain_number"]
    nameOutliersMed1 <- c( nameOutliersMed1, as.character( outliersMed1inf), as.character(outliersMed1sup))
    outliersMed4inf <- dfIM_bis[ is_outlier_inf( dfIM_bis[ dfIM_bis$age ==4,]$median) & dfIM_bis$age == 4, "strain_number"]
    outliersMed4sup <- dfIM_bis[ is_outlier_sup( dfIM_bis[ dfIM_bis$age ==4,]$median) & dfIM_bis$age == 4, "strain_number"]
    nameOutliersMed4 <- c( nameOutliersMed4, as.character( outliersMed4inf), as.character( outliersMed4sup))
    #### Stocking extrem strains for heatmap => Summary.R
    #### if the strain is a superior extrem for namePheno, we give it a 1
    #### for an inferior extrem -1
    #### elsewhere there is a 0
    diffmedian_extrems1[[ namePheno]] <- 0
    diffmedian_extrems1[ diffmedian_extrems1$strain_number %in% outliersMed1inf, namePheno] <- -1
    diffmedian_extrems1[ diffmedian_extrems1$strain_number %in% outliersMed1sup, namePheno] <- 1
    diffmedian_extrems4[[ namePheno]] <- 0
    diffmedian_extrems4[ diffmedian_extrems4$strain_number %in% outliersMed4inf, namePheno] <- -1
    diffmedian_extrems4[ diffmedian_extrems4$strain_number %in% outliersMed4sup, namePheno] <- 1
  
    #Extrem strains for IM
    #Names of all inferior and superior extrems strains are collected for the current phenotype
    #All names are collected in a vector nameOutliers for the table of extrem strains
    outliersIMinf <- dfIM[ is_outlier_inf( dfIM$diff), "strain_number"]
    outliersIMsup <- dfIM[ is_outlier_sup( dfIM$diff), "strain_number"]
    nameOutliersIM <- c( nameOutliersIM, as.character( outliersIMinf), as.character( outliersIMsup))
    #### Stocking extrem strains for heatmap => Summary.R
    #### if the strain is a superior extrem for namePheno, we give it a 1
    #### for an inferior extrem -1
    #### elsewhere there is a 0
    diffmedian_extremsIM[[ namePheno]] <- 0
    diffmedian_extremsIM[ diffmedian_extremsIM$strain_number %in% outliersIMinf, namePheno] <- -1
    diffmedian_extremsIM[ diffmedian_extremsIM$strain_number %in% outliersIMsup, namePheno] <- 1
    
    
    # Boxplot of median values of strains by age, extrems points are labellised 
    cat("\nIci, on ne travaille plus avec les individus mais avec les lignées. Les médianes des lignées pour 
        les deux âges ont été calculées et mises sous forme d'un boxplot comparable avec le précédent, 
        il nous permet de voir la tendance générale ainsi que les lignées ayant 
        un comportement extrême, qui ont été labellisées.<br>")
    plotpheno14med <- ggplot( dfIM_bis) + 
                      geom_violin( aes( x = as.factor( age), y = median, fill = as.factor(age))) +
                      scale_fill_manual( values = c( "1"= "lemonchiffon3", "4"= "gray60" )) +
                      geom_boxplot( aes( x = as.factor( age), y = median), width = 0.1) +
                      geom_text_repel( 
                        data = dfIM_bis[is_outlier( dfIM_bis[dfIM_bis$age ==1, ]$median) & dfIM_bis$age ==1,],
                        aes(x = as.factor(age), y=median, label = strain_number), nudge_x = 0.2) +
                      geom_text_repel( 
                        data = dfIM_bis[is_outlier( dfIM_bis[dfIM_bis$age ==4,]$median) & dfIM_bis$age ==4,],
                        aes(x = as.factor(age), y=median, label = strain_number), nudge_x = 0.2) +
                      xlab( "Age") +
                      ylab( "median of phenotype values of strains") +
                      ggtitle( paste( namePheno, "\nmedian strains's values for each age")) +
                      theme( plot.title = element_text( hjust = 0.5)) 
    print( plotpheno14med)
    # Wilcoxon test on last boxplot ie on medians of each strain, population divided by age
    cat("<br>Le test de Wilcoxon a maintenant été appliqué aux médianes des lignées pour l'âge 1
        et l'âge. On a pu faire dans ce cas-ci un test paired.")
    wilcoxon2 <- wilcox.test(median ~ age, data = dfIM_bis, paired = TRUE)
    print( wilcoxon2)
    #If there is significativity previously, the kind of the main tendency (increase or decrease)
    #is determinated for lines
    if ( wilcoxon2$p.value < 0.05){
      if ( sum( dfIM$sign == "positive") > sum( dfIM$sign == "negative")) {
        action2 <- "augmenter"
        contresign <- "negative"
      } else {
        action2 <- "diminuer"
        contresign <- "positive"
      }
      lines_contresign <- sum( dfIM$sign == contresign) 
      percent_contresign <- round( lines_contresign / length( dfIM$sign), 2) * 100 
      if ( identical( action, action2)){
        cat ( "<br> Il existe une différence significative d'après le test de Wilcoxon. La valeur phénotypique de", namePheno,
             "tend à", action2, "entre les deux âges pour la majorité des lignées, comme cela est le cas pour l'ensemble de la population.",
             lines_contresign, "lignées ont un comportement inverse minoritaire, soit", percent_contresign, "%.<br>")
      } else {
        cat ( "<br> Il existe une différence significative d'après le test de Wilcoxon. La valeur phénotypique de", namePheno,
              "tend à", action2, "entre les deux âges pour la majorité des lignées, ce qui est à l'opposé de l'ensemble de la population.",
              lines_contresign, "lignées ont un comportement inverse minoritaire, soit", percent_contresign, "%.<br>")
      }
    }
    
    
    # #Same data with log and sqrt 
    # plotpheno14med_log <- ggplot( dfIM_bis) + 
    #               geom_violin( aes( x = as.factor( age), y = log( median), fill = as.factor(age))) +
    #               scale_fill_manual( values = c( "1"= "lemonchiffon3", "4"= "gray60" )) +
    #               geom_boxplot( aes( x = as.factor( age), y = log(median)), width = 0.1) +
    #               xlab( "Age") +
    #               ylab( "log median of phenotype values of strains") +
    #               theme( plot.title = element_text( hjust = 0.5), legend.position = "none") 
    # plotpheno14med_sqrt <- ggplot( dfIM_bis) + 
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
    #                                  lapply( unique( dfIM_bis$age), function( ages) {
    #                                    shapiro <- shapiro.test( dfIM_bis[dfIM_bis$age == ages,]$median)
    #                                    shapiro_list <- list( Value = "real", age = ages, W = shapiro$statistic, pvalue =shapiro$p.value)
    #                                    return( shapiro_list)
    #                                  }))   
    # shapiro_list_med_sqrt <- do.call("rbind.data.frame",
    #                                  lapply( unique( dfIM_bis$age), function( ages) {
    #                                    shapiro <- shapiro.test( sqrt( dfIM_bis[dfIM_bis$age == ages,]$median))
    #                                    shapiro_list <- list( Value = "sqrt", age = ages, W = shapiro$statistic, pvalue =shapiro$p.value)
    #                                    return( shapiro_list)
    #                                  }))   
    # shapiro_list_med_log <- do.call("rbind.data.frame",
    #                                 lapply( unique( dfIM_bis$age), function( ages) {
    #                                   shapiro <- shapiro.test( log( dfIM_bis[dfIM_bis$age == ages,]$median))
    #                                   shapiro_list <- list( Value = "log", age = ages, W = shapiro$statistic, pvalue =shapiro$p.value)
    #                                   return( shapiro_list)
    #                                 }))   
    # shapiro_med <- rbind.data.frame( shapiro_list_med, shapiro_list_med_log, shapiro_list_med_sqrt)
    # print( kable( shapiro_med, format = "html", row.names = F)  %>%
    #          kable_styling(full_width = F, position = "left"))
    # 
    
    
    # Plots of IM for each strains, organized by increasing value
    # Same values are plotted in form of qplot and boxplot
    cat( "<br>Les graphiques suivants représentent l'indicateur de médiane pour chaque lignées sous des formes différentes. 
        Les lignées du scatterplot ont été ordonnées suivant cette valeur.<br>")
    qplotIMbox <- ggplot( dfIM) + 
                      geom_boxplot( aes( x = "strains", y = diff), fill = "gray82") +
                      geom_text_repel( 
                      data = dfIM[ is_outlier( dfIM$diff),],
                      aes( x = "strains", y = diff, label = strain_number), nudge_x = 0.2) +
                      xlab( "Strains") +
                      ylab( "IM") +
                      theme( axis.text.x=element_blank())
    qplotIM <- qplot( data = dfIM, x= strain_number, y = diff, col = sign) +
                   scale_fill_manual( values = c( "positive" = "deepskyblue", "negative" = "red2")) +
                   geom_hline( aes( yintercept = 0), lty = 2)+
                   xlab( "Strains") +
                   ylab( "IM") +
                   theme( axis.text.x=element_blank(), legend.position ="bottom") 
    titleIM2 <- paste( namePheno, "\nIM for each strain")
    grid.arrange( qplotIMbox, qplotIM, ncol = 2, top = titleIM2)
    #Shapiro test on IM values
    cat( "<br>La normalité de la distribution de l'indicateur de médiane est évaluée grâce à un shapiro test. 
        Si la p-value obtenue est inférieure à 0.05, on considère que l'échantillon a une distribution non gaussienne. <br>")
    shapiro <- shapiro.test( dfIM$diff)
    print( shapiro)
    #### The normality is kept in a dataframe for table in Summary.R
    if ( shapiro$p.value > 0.05){
      dfgaussien[ namePheno, "IM_avec_extremes"] <- "Normale"
    } else {
      dfgaussien[ namePheno, "IM_avec_extremes"] <- ""
    }
    
    
    #We want to do exactly the same thing, but without lines with extrems values
    dfIM_no_outliers <- dfIM[ -which( is_outlier( dfIM$diff)),]
    # Plots of mediane IM for each strains, organized by increasing value
    # Same values are plotted in form of qplot and boxplot
    cat( "<br>Les graphiques suivants sont semblables aux précédents, les données 'extrêmes' ont toutefois été enlevées.<br>")
    qplotIMboxno <- ggplot( dfIM_no_outliers) + 
                    geom_boxplot( aes( x = "strains", y = diff), fill = "gray82") +
                    xlab( "Strains") +
                    ylab( "IM") +
                    theme( axis.text.x=element_blank())
    qplotIMno <- qplot( data = dfIM_no_outliers, x= strain_number, y = diff, col = sign) +
                    scale_fill_manual( values = c( "positive" = "deepskyblue", "negative" = "red2")) +
                    geom_hline( aes( yintercept = 0), lty = 2)+
                    xlab( "Strains") +
                    ylab( "IM") +
                    theme( axis.text.x=element_blank(), legend.position ="bottom") 
    titleIMno2 <- paste( namePheno, "\nIM with no extrems for each strain")
    grid.arrange( qplotIMboxno, qplotIMno, ncol = 2, top = titleIMno2)
    #Shapiro test on IM values
    cat("<br>La normalité de la distribution de l'indicateur de médiane (sans extrêmes) est évaluée grâce à un shapiro test. 
        Si la p-value obtenue est inférieure à 0.05, on considère que l'échantillon a une distribution non gaussienne. <br>")
    shapirono <- shapiro.test( dfIM_no_outliers$diff)
    print( shapirono)
    #### The normality is kept in a dataframe for table in Summary.R
    if ( shapirono$p.value > 0.05){
      dfgaussien[ namePheno, "IM_sans_extremes"] <- "Normale"
    } else {
      dfgaussien[ namePheno, "IM_sans_extremes"] <- ""
    }
    
    
    #Graph of correlation between mediane4 and mediane1
    cat( "<br>Ci-dessous, on voit la correlation entre l'âge 1 et l'âge 4 pour la médiane. <br>")
    qplotmedian14 <- ggplot( dfIM) +
                    geom_point( aes( x= median1, y = median4)) +
                    geom_smooth( aes( x = median1, y = median4), method=lm, se=FALSE, col= "red") +
                    ggtitle( paste( namePheno, "\ncorrelation median age 4 - 1 for each strain")) +
                    theme( legend.position = "none", plot.title = element_text( hjust = 0.5))
    print( qplotmedian14)
    #Spearman correlation between median 1 and median 4
    cat( "<br>Coefficient de correlation de Spearman entre médiane age 1 et médiane âge 4.")
    spearman <- cor.test( x=dfIM$median1, y = dfIM$median4, method = "spearman")
    print( spearman)
    
  
    
    cat("<br><H5> INDICATEUR DE VARIATION </H5><br>")   #We will do exactly same thing than for median, except there 
                                                        #is no creation of file for python
    
    #Data frame containing mad age 1, mad age 4, and IV = ( mad4/median4)- (mad1/median1)/(mad1/median1)
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
    ##Merge of dfmad et dfIM to have mad and median in the same dataframe
    dfIV <- merge.data.frame( x = dfmad[ , c("strain_number", "mad1", "mad4")], 
                              y = dfIM[ , c( "strain_number", "median1", "median4")],
                              by = "strain_number")
    dfIV$diff <- (( dfIV$mad4/dfIV$median4) - (dfIV$mad1/dfIV$median1)) / (dfIV$mad1/dfIV$median1)
    dfIV[[ "sign"]] <- ifelse( dfIV[[ "diff"]] >= 0, "positive", "negative")
    strainorderIV <- dfIV[ order( dfIV$diff), "strain_number"]
    dfIV$strain_number = factor( dfIV$strain_number, levels = strainorderIV)
    
    #### Stocking the mad difference for all strains and all phenotypes in a dataframe 
    #### Which will be used to treat metadata and to do heatmap 
    diffIV <- merge.data.frame( x = diffIV, y = dfIV[, c("strain_number", "diff")], by = "strain_number", all = TRUE)
    colnames( diffIV)[ ncol( diffIV)] <- namePheno
    #### Stocking the mad for age 1 for all strains and phenotypes in a dataframe
    #### Which will be used to treat metadata
    diffmad1 <- merge.data.frame( x = diffmad1, y = dfmad[, c("strain_number", "mad1")], by = "strain_number", all = TRUE)
    colnames( diffmad1)[ ncol( diffmad1)] <- namePheno

    
    #Change the format of dfmad from wide to long, more easier to manipulate
    data1 <- dfmad[, c( "strain_number", "mad1")]
    data1[["age"]] = 1
    colnames( data1) <- c( "strain_number", "mad", "age")
    data4 <- dfmad[, c( "strain_number", "mad4")]
    data4[["age"]] = 4
    colnames( data4) <- c( "strain_number", "mad", "age")
    dfmad_bis <- rbind.data.frame( data1, data4)
    
    
    ### Files for python, to do the GWAS analysis ####
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

    
    #Extrem strains for mad
    #Names of all inferior and superior extrems strains are collected for the current phenotype
    #All names are collected in a vector nameOutliers for the table of extrem strains
    outliersMad1inf <- dfmad_bis[ is_outlier_inf( dfmad_bis[ dfmad_bis$age ==1,]$mad) & dfmad_bis$age ==1, "strain_number"]
    outliersMad1sup <- dfmad_bis[ is_outlier_sup( dfmad_bis[ dfmad_bis$age ==1,]$mad) & dfmad_bis$age ==1, "strain_number"]
    nameOutliersMad1 <- c( nameOutliersMad1, as.character( outliersMad1inf), as.character( outliersMad1sup))
    outliersMad4inf <- dfmad_bis[ is_outlier_inf( dfmad_bis[ dfmad_bis$age ==4,]$mad) & dfmad_bis$age ==4, "strain_number"]
    outliersMad4sup <- dfmad_bis[ is_outlier_sup( dfmad_bis[ dfmad_bis$age ==4,]$mad) & dfmad_bis$age ==4, "strain_number"]
    nameOutliersMad4 <- c( nameOutliersMad4, as.character( outliersMad4inf), as.character( outliersMad4sup))
    #Stocking extrem strains for heatmap : 
    #if the strain is a superior extrem for namePheno, we give it a 1
    #for an inferior extrem -1
    #elsewhere there is a 0
    diffmad_extrems1[[ namePheno]] <- 0
    diffmad_extrems1[ diffmad_extrems1$strain_number %in% outliersMad1inf, namePheno] <- -1
    diffmad_extrems1[ diffmad_extrems1$strain_number %in% outliersMad1sup, namePheno] <- 1
    diffmad_extrems4[[ namePheno]] <- 0
    diffmad_extrems4[ diffmad_extrems4$strain_number %in% outliersMad4inf, namePheno] <- -1
    diffmad_extrems4[ diffmad_extrems4$strain_number %in% outliersMad4sup, namePheno] <- 1
    #Extrem strains for diff 
    #Names of all inferior and superior extrems strains are collected for the current phenotype
    #All names are collected in a vector nameOutliers for the table of extrem strains
    outliersIVinf <- dfmad[ is_outlier_inf( dfmad$diff), "strain_number"]
    outliersIVsup <- dfmad[ is_outlier_sup( dfmad$diff), "strain_number"]
    nameOutliersIV <- c( nameOutliersIV, as.character( outliersIVinf), as.character( outliersIVsup))
    #Stocking extrem strains for heatmap : 
    #if the strain is a superior extrem for namePheno, we give it a 1
    #for an inferior extrem -1
    #elsewhere there is a 0
    diffmad_extremsIV[[ namePheno]] <- 0
    diffmad_extremsIV[ diffmad_extremsIV$strain_number %in% outliersIVinf, namePheno] <- -1
    diffmad_extremsIV[ diffmad_extremsIV$strain_number %in% outliersIVsup, namePheno] <- 1
    
    
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
    #If there is significativity previously, the kind of the main tendency (increase or decrease)
    #is determinated for lines
    if ( wilcoxon3$p.value < 0.05){
      if ( sum( dfmad$sign == "positive") > sum( dfmad$sign == "negative")) {
        action3 <- "augmenter"
        contresign <- "negative"
      } else {
        action3 <- "diminuer"
        contresign <- "positive"
      }
      lines_contresign <- sum( dfmad$sign == contresign) 
      percent_contresign <- round( lines_contresign / length( dfmad$sign), 2) * 100 
      cat ( "<br> Il existe une différence significative d'après le test de Wilcoxon. La variation de la valeur phénotypique de", namePheno,
            "tend à", action2, "entre les deux âges pour la majorité des lignées.",
            lines_contresign, "lignées ont un comportement inverse minoritaire, soit", percent_contresign, "%.<br>")
    }
    
    
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
    cat( "<br>Les graphiques suivants représentent l'indicateur de variation pour chaque sous des formes différentes. 
        Les lignées du scatterplot ont été ordonnées suivant cette valeur.<br>")
    qplotIVbox <- ggplot( dfIV) + 
                  geom_boxplot( aes( x = "strains", y = diff), fill = "gray82") +
                  geom_text_repel( data = dfIV[ is_outlier( dfIV$diff),],
                  aes(x = "strains", y = diff, label = strain_number), nudge_x = 0.2) +
                  xlab( "Strains") +
                  ylab( "IV") +
                  theme( axis.text.x=element_blank())
    qplotIV <- qplot( data = dfIV, x= strain_number, y = diff, col = sign) +
                scale_fill_manual(values = c("positive" = "deepskyblue", "negative" = "red2")) +
                geom_hline( aes( yintercept = 0), lty = 2)+
                xlab( "Strains") +
                ylab( "IV") +
                theme( axis.text.x = element_blank(), legend.position = "bottom") 
    titleIV2 <- paste( namePheno, "\nIV for each strain")
    grid.arrange( qplotIVbox, qplotIV, ncol = 2, top = titleIV2)
    #Shapiro test on IV value 
    cat("<br>La normalité de la distribution de l'indicateur de variation est évaluée grâce à un shapiro test. 
        Si la p-value obtenue est inférieure à 0.05, on considère que l'échantillon a une distribution non gaussienne. <br>")
    shapiro2 <- shapiro.test( dfmad$diff)
    print( shapiro2)
    #### The normality is kept in a dataframe for table in Summary.R
    if (shapiro2$p.value > 0.05){
      dfgaussien[ namePheno, "IV_avec_extremes"] <- "Normale"
    } else {
      dfgaussien[ namePheno, "IV_avec_extremes"] <- ""
    }
    
    #We want to do exactly the same thing, but without lines with extrems values
    dfIV_no_outliers <- dfIV[ -which( is_outlier( dfIV$diff)),]
    # Plots of IV for each strains, organized by increasing value
    # Same values are plotted in form of qplot and boxplot
    cat("<br>Les graphiques suivants sont semblables aux précédents, les données 'extrêmes' ont toutefois été enlevées.<br>")
    qplotIVboxno <- ggplot( dfIV_no_outliers) + 
                    geom_boxplot( aes( x = "strains", y = diff), fill = "gray82") +
                    xlab( "Strains") +
                    ylab( "IV") +
                    theme( axis.text.x=element_blank())
    qplotIVno <- qplot(data = dfIV_no_outliers, x= strain_number, y = diff, col = sign) +
                  scale_fill_manual(values = c("positive" = "deepskyblue", "negative" = "red2")) +
                  geom_hline( aes( yintercept = 0), lty = 2)+
                  xlab( "Strains") +
                  ylab( "IV") +
                  theme( axis.text.x = element_blank(), legend.position = "bottom") 
    titleIVno2 <- paste( namePheno, "\nIV with no extrems for each strain")
    grid.arrange( qplotIVboxno, qplotIVno, ncol = 2, top = titleIVno2)
    
    #Shapiro test on IV value (without extrems)
    cat("<br>La normalité de la distribution de l'indicateur de variation (sans extrêmes) est évaluée grâce à un shapiro test. 
        Si la p-value obtenue est inférieure à 0.05, on considère que l'échantillon a une distribution non gaussienne. <br>")
    shapirono2 <- shapiro.test( dfIV_no_outliers$diff)
    print( shapirono2)
    #### The normality is kept in a dataframe for table in Summary.R
    if (shapirono2$p.value > 0.05){
      dfgaussien[ namePheno, "IV_sans_extremes"] <- "Normale"
    } else {
      dfgaussien[ namePheno, "IV_sans_extremes"] <- ""
    }
    
    #Graph of correlation between mad 1 and mad 4
    cat("<br>Ci-dessous, on voit la correlation entre CVR âge 1 et CVR âge 4, pour les lignées sans extrêmes. <br>")
    qplotIV14 <- ggplot( dfIV_no_outliers) +
                  geom_point( aes( x= dfIV_no_outliers$mad1/dfIV_no_outliers$median1, 
                                   y = dfIV_no_outliers$mad4/dfIV_no_outliers$median4)) +
                  geom_smooth( aes( x= dfIV_no_outliers$mad1/dfIV_no_outliers$median1, 
                                    y = dfIV_no_outliers$mad4/dfIV_no_outliers$median4), 
                              method=lm, se=FALSE, col="red") +
                  xlab( "CVR1") +
                  ylab( "CVR2") +
                  ggtitle( paste( namePheno, "\ncorrelation CVR age 4 - 1 for each strain")) +
                  theme( legend.position = "none", plot.title = element_text( hjust = 0.5))
    print( qplotIV14)
    #Spearman correlation between CVR1 And CVR4
    cat( "<br>Coefficient de correlation de Spearman pour la correlation CVR1/CVR4.")
    spearman2 <- cor.test( x = dfIV_no_outliers$mad1/dfIV_no_outliers$median1, 
                           y = dfIV_no_outliers$mad4/dfIV_no_outliers$median4, method = "spearman")
    print( spearman2)
    
    
    cat( "<br><H5> CORRELATION IM IV </H5><br>")  # Aim of this part is to see if there is any correlation 
                                                      #between evolution of median and mad in growing up process
    
    
    # Plot of IM by IV
    cat( "La corrélation IM IV affichée ci-dessous est la corrélation de l'indicateur de médiane
        et de l'indicateur de variation pour chacune des lignées. <br>")
    mgIMIV <- merge.data.frame( x = dfIM_no_outliers[ , c( "strain_number", "diff")], 
                                y = dfIV_no_outliers[ , c( "strain_number", "diff")],
                                by = "strain_number")
    colnames( mgIMIV)[ c( 2,3)] <- c( "IM", "IV")
    plotIMIV <-  ggplot ( mgIMIV) +
                   geom_point( aes( x = IM, y = IV)) + 
                   geom_smooth( aes( x = IM, y = IV), method=lm, se=FALSE) +
                   ggtitle( paste( namePheno, "\ncorrelation IM IV for each strain")) +
                   theme( plot.title = element_text( hjust = 0.5))
    print( plotIMIV)
    #The Spearman correlation is determinated between IM and IV 
    cat( "<br>Coefficient de correlation de Spearman pour la correlation médiane mad.")
    spearman3 <- cor.test( x=mgIMIV$IM, y = mgIMIV$IV, method = "spearman")
    print( spearman3)
    
    
    # Splicing between phenotype with a black line and spaces
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


#### File to be imported #### 
write.csv( diffmedian1, file = "~/workspace/AnalysisSteph/2_Phenotypes/Output/median_age1.csv")  
write.csv( diffmad1, file = "~/workspace/AnalysisSteph/2_Phenotypes/Output/mad_age1.csv")  
write.csv( diffIM, file = "~/workspace/AnalysisSteph/2_Phenotypes/Output/diffmedian.csv")  
write.csv( diffIV, file = "~/workspace/AnalysisSteph/2_Phenotypes/Output/diffmad.csv")  
write.csv( execution_dataframe, file = "~/workspace/AnalysisSteph/2_Phenotypes/Output/aging_analysis_gwas.csv", row.names = F)


#Cleaning
rm ( is_outlier, DATA_pheno, tabDATA_pheno, nameToRemove, idxToRemove, DATA_pheno_rm, RemainingStrain_nb,
     tabDATA_pheno_rm, plotStrainpheno, DATA_pheno_rm1, DATA_pheno_rm4, lenStrain, plotpheno1,
     plotpheno4, plotpheno14, wilcoxon, dfIM, strainorder, data1, data4,
     dfIM_bis, plotpheno14med, wilcoxon2, qplotmedian14, spearman, dfmad, strainordermad, dfmad_bis,
     plotpheno14mad, spearman2, mgIMIV, spearman3, namePheno, wilcoxon3, execution_dataframe_mad, 
     execution_dataframe_median, mad_python, median_python, shapiro, shapiro2, lines_contresign, 
     percent_contresign, contresign, action2, action3, action, is_outlier_inf, is_outlier_sup, 
     outliersMed1inf, outliersMed1sup, outliersMed4inf, outliersMed4sup, qplotIMbox, qplotIM, 
     qplotIMboxno, qplotIMno, qplotIVbox, qplotIV, qplotIVboxno, qplotIVno, qplotIV14, plotIMIV, 
     titleIM2, titleIMno2, titleIV2, titleIVno2, outliersMad1inf, outliersMad1sup, outliersMad4inf, 
     outliersMad4sup, outliersIMinf, outliersIMsup, outliersIVinf, outliersIVsup, strainorderIV, 
     mean1, mean4, meanvalue, shapirono, shapirono2, dfIM, dfIV, median_python_extrem, mad_python_extrem)


#Created and kept variables 
# dfgaussien
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
# mad_python_off
# median_python_df : data frame used to run GWAS
# median_python_off 
# nameOutliersIM : names of "extrem strains" for IM 
# nameOutliersIV : names of "extrem strains" for IV 
# nameOutliersMad1 : names of "extrem strains" for the mad and for age 1
# nameOutliersMad4 : names of "extrem strains" for the mad and for age 4
# nameOutliersMed1 : names of "extrem strains" for the median and for age 1
# nameOutliersMed4 : names of "extrem strains" for the median and for age 4
# phenotypes : character vector with names of all phenotypes 
# RemainingStrain : Percent of remaining strains for all phenotypes, in order of "phenotypes"



    
     
     

       