## @knitr battements2

boxplot_battements1 <- ggplot( dfBattements1) + 
                      geom_boxplot( aes (x = reorder(x =strain_number, X = value, FUN =  median), y = value)) +
                      ggtitle( "Nombre de battements par lignées, Age 1") +
                      xlab( "lignées") +
                      ylab( "nombre de battements") +
                      coord_flip() +
                      theme( plot.title = element_text( hjust = 0.5),
                             axis.text.y = element_text( hjust = 1, size = 7)) 
ggplotly( boxplot_battements1)

