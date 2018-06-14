## @knitr summary 

##Objectives : Tables and heatmap following the analysis of phenotypes 


#Titles
cat( "<H2> Summary </H2><br>")

#Summary of percent remaining strains for each phenotype, so if there are analyzed or not
cat( "<H4> Percent of remaining strains </H4><br>")
kable( cbind.data.frame( "phenotypes" = phenotypes, "valid_strain" = formatC( RemainingStrain, digits =2, format = "f"  )), 
       caption = "**Percentage of remaining strains** : Under 80%, phenotypes have not been studied")

#Table who shows for which phenotype the distribution of data is normal
cat( "<H4> Normality of the distribution</H4><br>")
cat( "La table suivante est un récapitulatif des distributions qui sont normales ou non, 
     détaillées plus haut pour chaque phénotype.")
kable( dfgaussien, caption = "**Normality of distributions** : normal distributions are indicated in the table")

#Differents views of extrems strains 
cat("<H4> Extrem strains </H4><br>")

#Display in a table
cat("<H5> Table of extrem strains </H5><br>")
cat( "La table qui suit montre les lignées qui ont des comportements extrêmes au moins deux fois dans le cas
    de la médiane ou de la mad, ce pour chaque âge. Ce tableau ne parle pas des différences de médiane et de mad.<br>")
#Merge of all tables of outliers
dfmed1 <- data.frame( table( nameOutliersMed1)[ table( nameOutliersMed1) > 1])
colnames( dfmed1) <- c( "strain_number", "med1")
dfmed4 <- data.frame( table( nameOutliersMed4)[ table( nameOutliersMed4) > 1])
colnames( dfmed4) <- c( "strain_number", "med4")
dfmad1 <- data.frame( table( nameOutliersMad1)[ table( nameOutliersMad1) > 1])
colnames( dfmad1) <- c( "strain_number", "mad1")
dfmad4 <- data.frame( table( nameOutliersMad4)[ table( nameOutliersMad4) > 1])
colnames( dfmad4) <- c( "strain_number", "mad4")
#Merging all tables in a big one
mgmed <- merge.data.frame( x = dfmed1, y = dfmed4, by = "strain_number", all = TRUE) 
mgmedmad1 <- merge.data.frame( x= mgmed, y = dfmad1, by = "strain_number", all = TRUE) 
mgmedmad14 <- merge.data.frame( x= mgmedmad1, y = dfmad4, by = "strain_number", all = TRUE)
mgmedmad14[ , "sum"] <- rowSums (mgmedmad14[, c(2:5)], na.rm = TRUE)
datatable( mgmedmad14, rownames = F)

#Now display in heatmap
cat("<H5> Heatmap of median extrem strains </H5><br>")
  cat ( "<br>Les heatmap suivantes montrent les données de la table précédente sous un autre format. 
      Si les lignées indiquées comme extrêmes pour la médiane à l'âge 1 (première heatmap) sont 
      des extrêmes 'supérieurs', ils sont indiqués en bleus,
      s'ils sont des extrêmes 'inférieurs', alors ils sont indiqués en rouge.<br>")
  #MEDIAN age 1
  median_extrems1 <- diffmedian_extrems1
  rownames( median_extrems1) <- median_extrems1[ , "strain_number"]
  median_extrems1 <- median_extrems1[ , -which( names( median_extrems1) == "strain_number")]
  diffmedian_extrems1_sub <- median_extrems1[ rowSums( median_extrems1) != 0,]
  pheatmap_mad1 <- pheatmap( t( diffmedian_extrems1_sub), cluster_rows = TRUE, cluster_cols = TRUE, legend = F,
                             fontsize_row = 8, fontsize_col = 8, cellwidth = 7,
                             color = c( "red", "white", "blue"), breaks = c( -1, -0.5, 0.5, 1), main = "Extrem strains for median age 1")
  #MEDIAN age 4
  median_extrems4 <- diffmedian_extrems4
  rownames( median_extrems4) <- median_extrems4[ , "strain_number"]
  median_extrems4 <- median_extrems4[ , -which( names( median_extrems4) == "strain_number")]
  diffmedian_extrems4_sub <- median_extrems4[ rowSums( median_extrems4) != 0,]
  pheatmap_mad1 <- pheatmap( t( diffmedian_extrems4_sub), cluster_rows = TRUE, cluster_cols = TRUE, legend = F,
                             fontsize_row = 8, fontsize_col = 8, cellwidth = 7,
                             color = c( "red", "white", "blue"), breaks = c( -1, -0.5, 0.5, 1), main = "Extrem strains for median age 4")
  common_median <- sort( unique( nameOutliersMed1[ nameOutliersMed1 %in% nameOutliersMed4]))
  cat( "<br> Certaines lignées sont à la fois extrêmes pour la médiane âge 1 et pour la médiane âge4. Ceci est 
      visible dans la table précédente, mais en voici le rappel :", common_median)
  #IM
cat( "<br><H5> Heatmap of IM extrem strains </H5><br>")
  cat( "La heatmap qui suit est fondée sur le même principe, montrant les extrêmes pour l'indicateur de médiane IM.<br>")
  median_extremsIM <- diffmedian_extremsIM
  rownames( median_extremsIM) <- median_extremsIM[ , "strain_number"]
  median_extremsIM <- median_extremsIM[ , -which( names( median_extremsIM) == "strain_number")]
  diffmedian_extremsIM_sub <- median_extremsIM[ rowSums( median_extremsIM) != 0,]
  pheatmap_IM <- pheatmap( t( diffmedian_extremsIM_sub), cluster_rows = TRUE, cluster_cols = TRUE, legend = F,
                             fontsize_row = 8, fontsize_col = 8, cellwidth = 7,
                             color = c( "red", "white", "blue"), breaks = c( -1, -0.5, 0.5, 1), main = "Extrem strains for IM")
  
cat( "<br><H5> Heatmap of mad extrem strains </H5><br>")
  #MAD age 1
  mad_extrems1 <- diffmad_extrems1
  rownames( mad_extrems1) <- mad_extrems1[ , "strain_number"]
  mad_extrems1 <- mad_extrems1[ , -which( names( mad_extrems1) == "strain_number")]
  diffmad_extrems1_sub <- mad_extrems1[ rowSums( mad_extrems1) != 0,]
  pheatmap_mad1 <- pheatmap( t( diffmad_extrems1_sub), cluster_rows = TRUE, cluster_cols = TRUE, legend = F,
                            fontsize_row = 8, fontsize_col = 8, cellwidth = 7,
                            color = c( "red", "white", "blue"), breaks = c( -1, -0.5, 0.5, 1), main = "Extrem strains for mad age 1")
  #MAD age 4
  mad_extrems4 <- diffmad_extrems4
  rownames( mad_extrems4) <- mad_extrems4[ , "strain_number"]
  mad_extrems4 <- mad_extrems4[ , -which( names( mad_extrems4) == "strain_number")]
  diffmad_extrems4_sub <- mad_extrems4[ rowSums( mad_extrems4) != 0,]
  pheatmap_mad1 <- pheatmap( t( diffmad_extrems4_sub), cluster_rows = TRUE, cluster_cols = TRUE, legend = F,
                             fontsize_row = 8, fontsize_col = 8, cellwidth = 7,
                             color = c( "red", "white", "blue"), breaks = c( -1, -0.5, 0.5, 1), main = "Extrem strains for mad age 4")
  common_mad <- sort( unique( nameOutliersMad1[ nameOutliersMad1 %in% nameOutliersMad4]))
  cat( "<br> Certaines lignées sont à la fois extrêmes pour la mad âge 1 et pour la mad âge4. Ceci est 
       visible dans la table précédente, mais en voici le rappel :", common_mad)
  #IV
cat( "<br><H5> Heatmap of IV extrem strains </H5><br>")
  mad_extremsIV <- diffmad_extremsIV
  rownames( mad_extremsIV) <- mad_extremsIV[ , "strain_number"]
  mad_extremsIV <- mad_extremsIV[ , -which( names( mad_extremsIV) == "strain_number")]
  diffmad_extremsIV_sub <- mad_extremsIV[ rowSums( mad_extremsIV) != 0,]
  pheatmap_IM <- pheatmap( t( diffmad_extremsIV_sub), cluster_rows = TRUE, cluster_cols = TRUE, legend = F,
                           fontsize_row = 8, fontsize_col = 8, cellwidth = 7,
                           color = c( "red", "white", "blue"), breaks = c( -1, -0.5, 0.5, 1), main = "Extrem strains for IV")
  
  
cat("<br><H4> Heatmap </H4><br>")

cat("Les deux heatmap suivante représentent l'indicateur de médiane et l'indicateur de variation pour les phenotypes étudiés. 
    Le souci est que 3 des phénotypes présentent que 84% des lignées du fichier utilisé, il y aura donc 
    des données manquantes. Les deux graphiques correspondent à deux stratégies pour palier à ceci.<br>")

cat("Première proposition : On enlève les lignes avec des données manquantes, on traite donc les 29 phénotypes
      avec 138 observations.<br>")
#Heatmap of IM
  #Reorganize 
  #First column as rownames
  rownames( diffIM) <- diffIM[ , 1]
  diffIM <- diffIM[ , -1]
  #Treating only complete rows 
  diffmedian_sub <- diffIM[ complete.cases( diffIM),]
  #Create correlation matrix
  corrmedian_sub <- round( cor( diffmedian_sub, method = "spearman"), 2)
  #Reorder correlation 
  reorder_corr <- function(x){
    dd <- as.dist((1-x)/2)
    hc <- hclust(dd)
    x <-x[hc$order, hc$order]
  }
  pheatmapIM <- pheatmap( corrmedian_sub, cluster_rows = TRUE, cluster_cols = TRUE, legend = F, 
                          fontsize_row = 8, fontsize_col = 8, cellwidth = 7, cellheight = 7, border_color = NA,
                          color = colorRampPalette( c( "blue", "white", "red"))( 50), main = "Extrem strains for IV")
  print( pheatmapIM)
  # 
  # corrmedian_sub <- reorder_corr(corrmedian_sub)
  # # Get lower triangle of the correlation matrix
  get_lower_tri<-function( x){
    x[upper.tri( x)] <- NA
    return( x)
  }
  # 
  # corrmedian_sub <- get_lower_tri( corrmedian_sub)
  # # Melt the correlation matrix
  # melted_corrmedian_sub <- melt(corrmedian_sub, na.rm = TRUE)
  # # Heatmap
  # heatmap_median_sub <- ggplot( data = melted_corrmedian_sub, aes( Var2, Var1, fill = value)) +
  #                       geom_tile(color = "white") +
  #                       scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c( -1, 1), 
  #                                            space = "Lab", name="Spearman\nCorrelation") +
  #                       theme_minimal() + 
  #                       theme( axis.text.x = element_text( angle = -60, vjust = 1, size = 8, hjust = 1)) +
  #                       coord_fixed() +
  #                       scale_x_discrete(position = "top") 
  # print( heatmap_median_sub)

#IV
  cat("<br>On fait de même avec l'indicateur de variation.<br>")
  #Reorganize 
  #First column as rownames
  rownames( diffIV) <- diffIV[ , 1]
  diffIV <- diffIV[ , -1]
  #Treating only complete rows 
  diffIV_sub <- diffIV[ complete.cases( diffIV),]
  #Create correlation matrix
  corrmad_sub <- round( cor( diffIV_sub, method = "spearman"), 2)
  pheatmapIV <- pheatmap( corrmad_sub, cluster_rows = TRUE, cluster_cols = TRUE, legend = F, 
                          fontsize_row = 8, fontsize_col = 8, cellwidth = 7, cellheight = 7, border_color = NA, 
                          color = colorRampPalette( c( "blue", "white", "red"))( 50), main = "Extrem strains for IV")
  print( pheatmapIV)
  # #Reorder correlation 
  # corrmad_sub <- reorder_corr(corrmad_sub)
  # # Get lower triangle of the correlation matrix
  # corrmad_sub <- get_lower_tri( corrmad_sub)
  # # Melt the correlation matrix
  # melted_corrmad_sub <- melt(corrmad_sub, na.rm = TRUE)
  # # Heatmap
  # heatmap_mad_sub <- ggplot( data = melted_corrmad_sub, aes( Var2, Var1, fill = value)) +
  #   geom_tile(color = "white") +
  #   scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c( -1, 1), 
  #                        space = "Lab", name="Spearman\nCorrelation") +
  #   theme_minimal() + 
  #   theme( axis.text.x = element_text( angle = -60, vjust = 1, size = 8, hjust = 1)) +
  #   coord_fixed() +
  #   scale_x_discrete(position = "top") 
  # print( heatmap_mad_sub)
  # 
  
#Autre methode IM 
  cat("<br>Deuxième proposition : Les phénotypes avec des données manquantes sont enlevés, on traite donc 26 phenotypes
      mais avec les 163 observations.<br>")
  nameDrop <- c("DiastolicMeanDiameter", "FractionalShortening", "SystolicMeanDiameter")  
  diffmedian_sub2 <- diffIM[ , -which( names( diffIM) %in% nameDrop)]
  #Create correlation matrix
  corrmedian_sub2 <- round( cor( diffmedian_sub2, method = "spearman"), 2)
  #Reorder correlation 
  corrmedian_sub2 <- reorder_corr(corrmedian_sub2)
  # Get lower triangle of the correlation matrix
  corrmedian_sub2 <- get_lower_tri( corrmedian_sub2)
  # Melt the correlation matrix
  melted_corrmedian_sub2 <- melt(corrmedian_sub2, na.rm = TRUE)
  # Heatmap
  heatmap_median_sub2 <- ggplot( data = melted_corrmedian_sub2, aes( Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c( -1, 1), 
                         space = "Lab", name="Spearman\nCorrelation") +
    theme_minimal() + 
    theme( axis.text.x = element_text( angle = -60, vjust = 1, size = 8, hjust = 1)) +
    coord_fixed() +
    scale_x_discrete(position = "top") 
  print( heatmap_median_sub2)
  
#IV
  cat("<br>On fait de même avec l'indicateur de variation.<br>")
  diffIV_sub2 <- diffIV[ , -which( names( diffIV) %in% nameDrop)]
  #Create correlation matrix
  corrmad_sub2 <- round( cor( diffIV_sub2, method = "spearman"), 2)
  #Reorder correlation 
  corrmad_sub2 <- reorder_corr(corrmad_sub2)
  # Get lower triangle of the correlation matrix
  corrmad_sub2 <- get_lower_tri( corrmad_sub2)
  # Melt the correlation matrix
  melted_corrmad_sub2 <- melt(corrmad_sub2, na.rm = TRUE)
  # Heatmap
  heatmap_mad_sub2 <- ggplot( data = melted_corrmad_sub2, aes( Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c( -1, 1), 
                         space = "Lab", name="Spearman\nCorrelation") +
    theme_minimal() + 
    theme( axis.text.x = element_text( angle = -60, vjust = 1, size = 8, hjust = 1)) +
    coord_fixed() +
    scale_x_discrete(position = "top") 
  print( heatmap_mad_sub2)
  
  
#Cleaning 
# rm( dfmed1, dfmed4, dfmad1, dfmad4, mgmed, mgmedmad1, median_extrems1, diffmedian_extrems1_sub,
#     median_extrems4, diffmedian_extrems4_sub, mad_extrems1, diffmad_extrems1_sub,
#     mad_extrems4, diffmad_extrems4_sub, diffmedian_sub, corrmedian_sub, nameDrop, 
#     diffmedian_sub2, corrmedian_sub2, diffmad_sub, corrmad_sub, reorder_corr,
#     get_lower_tri, diffmad_sub2, corrmad_sub2)

#Created and kept variables
#mgmedmad14 : Table of extrem data printed in the html form
#pheatmap_med1 : heatmap for median age 1 
#pheatmap_med4 : heatmap for median age 4
#pheatmap_mad1 : heatmap for mad age 1
#pheatmap_mad4 : heatmap for mad age 4 
#melted_corrmedian_sub : dataframe to do the heatmap of correlation for diff median, strategy 1
#heatmap_median_sub : heatmap of correlations for diff median, strategy 1
#melted_corrmedian_sub2 : dataframe to do the heatmap of correlation for diff median, strategy 2
#heatmap_median_sub2 : heatmap of correlations for diff median, strategy 2
#melted_corrmad_sub : dataframe to do the heatmap of correlation for diff mad, strategy 1
#heatmap_mad_sub : heatmap of correlations for diff mad, strategy 1
#melted_corrmad_sub2 : dataframe to do the heatmap of correlation for diff mad, strategy 2
#heatmap_mad_sub2 : heatmap of correlations for diff mad, strategy 2