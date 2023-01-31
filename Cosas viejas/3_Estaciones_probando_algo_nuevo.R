

registers_abril <- datacam_abril #Datacam que quiero usar
especie <- "axis"
especie_png <- axis
ymax <- 42
titulo <- "Registros de *Axis axis*"
subtitulo <- "En los muestreos de ab17 y ab18"
n <- "n = 442"

registers_abril$Time <- as.character(registers_abril$Time)
registers_abril$decimal <- sapply(strsplit(registers_abril$Time,":"), function(x){
   x <- as.numeric(x)
   x[1]+x[2]/60+ x[3]/3600})

especie1 <- registers_abril %>% 
   filter(Species == especie) #ESPECIE QUE QUIERO

(plot_color <- ggplot(registers_abril %>% 
                         filter(Species == especie), aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      scale_y_continuous(limits = c(0, ymax), breaks = seq(0, ymax, by = 6)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = titulo, 
           subtitle = subtitulo, #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")) +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))# +
      
      draw_image(sun, x = 0.5055, y = 0.20,
                 width = 0.07, height = 0.06) +
      draw_image(moon, x = 0.41, y = 0.7,
                 width = 0.08, height = 0.07,
                 scale = 0.75) +
      draw_image(canidae, x = 0.24, y = 0.75,
                 width = 0.13, height = 0.13)) +
      draw_text(n, x = 0.758, y = 0.15, size = 12))


ggdraw(plot_color) +
   draw_image(sun, x = 0.5055, y = 0.20, 
              width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.41, y = 0.7, 
              width = 0.08, height = 0.07,
              scale = 0.75) + 
   draw_image(canidae, x = 0.24, y = 0.75,
              width = 0.13, height = 0.13) +
   draw_text(n, x = 0.758, y = 0.15, size = 12)
