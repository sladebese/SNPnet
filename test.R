#On veut trois cas différents d'évolution de médiane et de variation
DATA_test <- DATA_analysis[ DATA_analysis$phenotype_name == "Heartperiod_Mean",]
examples <- c( "dgrp301", "dgrp406", "dgrp109")
DATA_test_sub <- DATA_test[ DATA_test$strain_number %in% examples,]

plotpresentation <- ggplot( DATA_test) + 
                    geom_boxplot( aes( x = strain_number, y = value, fill = as.factor( age))) +
                    xlab( "Age") +
                    ylab( "Phenotype values") +
                    ggtitle( "Phenotypes values by age") +
                    theme( plot.title = element_text( hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) 


strainorder <- c( "dgrp301", "dgrp406", "dgrp109")
DATA_test_sub$strain_number = factor( DATA_test_sub$strain_number, levels = strainorder)
plotpresentation <- ggplot( DATA_test_sub) + 
  geom_boxplot( aes( x = strain_number, y = value, fill = as.factor( age)), outlier.shape=NA) +
  coord_cartesian(ylim = c(0.2, 1.3))+
  xlab( "Line") +
  ylab( "Phenotype values") +
  labs(fill = "Age") +
  ggtitle( "Heartperiod_Mean \nPhenotypes values by age and line") +
  theme( plot.title = element_text( hjust = 0.5)) 

#On veut deux cas de figures avec la même variation de médiane mais pas la même amplitude
strainorder <- dfmedian[order(dfmedian$diff),]$strain_number
DATA_pheno_rm$strain_number = factor( DATA_pheno_rm$strain_number, levels = strainorder)
#lenStrain <- length( unique( DATA_pheno_rm$strain_number))
plotpresentation <- ggplot( DATA_pheno_rm) + 
  geom_boxplot( aes( x = strain_number, y = value, fill = as.factor( age)), outlier.shape=NA) +
  xlab( "Age") +
  ylab( "Phenotype values") +
  labs(fill = "Age") +
  ggtitle( "Phenotypes values by age") +
  theme( plot.title = element_text( hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) 


examples <- c("dgrp761", "dgrp776")
DATA_rm_sub <- DATA_pheno_rm[ DATA_pheno_rm$strain_number %in% examples,]
plotpresentation <- ggplot( DATA_rm_sub) + 
  geom_boxplot( aes( x = strain_number, y = value, fill = as.factor( age)), outlier.shape=NA) +
  xlab( "Line") +
  ylab( "Phenotype values") +
  labs(fill = "Age") +
  ggtitle( "Phenotypes values by line and age") +
  theme( plot.title = element_text( hjust = 0.5)) 


#Exemples de quelques lignées avec des extrêmes 


