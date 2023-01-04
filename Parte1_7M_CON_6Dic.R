#### ANÁLISIS DE CÁMARAS TRAMPA ####
rm(list = ls())

library(camtrapR)
library(lubridate)
library(overlap)
library(readr)
library(tidyr)
library(dplyr)
library(tidyverse) 
library(ggtext) 
library(rphylopic)
library(cowplot) 
library(png)
library(magick)

setwd("C:/Users/shalo/OneDrive/Escritorio/Ilán/Tesis/Archivos ahora")
getwd()

datastation <- read.csv("datastation_final.csv",
                        header = TRUE, 
                        sep = ",")

datacam_delta60 <- read.csv("datacam_muestreo_delta60.csv", 
                            header = T,
                            sep = ";",
                            row.names = 1)
datacam_delta60$DateTimeOriginal <- as.factor(datacam_delta60$DateTimeOriginal)

#### PNGs ####
sun <- readPNG("sun.png")
moon <- readPNG("moon.png")
rhea <- readPNG("rhea.png")
hydrochoerus <- image_read("https://images.phylopic.org/images/9c234021-ce53-45d9-8fdd-b0ca3115a451/raster/1024x596.png?build=103")
sus <- image_read("http://phylopic.org/assets/images/submissions/3d8acaf6-4355-491e-8e86-4a411b53b98b.128.png")
cingulata <- image_read("https://images.phylopic.org/images/5d59b5ce-c1dd-40f6-b295-8d2629b9775e/raster/1024x493.png?build=103")
mazama <- image_read("http://phylopic.org/assets/images/submissions/b5f40112-0cb8-4994-aa70-28ac97ccb83f.128.png")
axis <- readPNG("axis.png")
canidae <- image_read("http://phylopic.org/assets/images/submissions/d67d3bf6-3509-4ab6-819a-cd409985347e.128.png")
leopardus <- image_read("http://phylopic.org/assets/images/submissions/cbc3f93e-0ce3-4e3b-871d-6ac688ed8810.128.png")

#### Hist_rad ####
registers <- datacam_delta60 
registers$Time <- as.character(registers$Time)
registers$decimal <- sapply(strsplit(registers$Time,":"), function(x){
   x <- as.numeric(x)
   x[1]+x[2]/60+ x[3]/3600})

## Filtramos la especie que nos interesa

#### AXIS ####
especie1 <- registers %>% 
   filter(Species == "axis") #ESPECIE QUE QUIERO

ymax <- 110

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Axis axis*", 
           #subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            # plot.background = element_rect(fill = "#f4f0cb", color = "#f4f0cb"),
            # panel.background = element_rect(fill = "#f4f0cb"),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5055, y = 0.20, 
              width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.41, y = 0.72, 
              width = 0.08, height = 0.07,
              scale = 0.75) + 
   draw_image(axis, x = 0.21, y = 0.79,
              width = 0.13, height = 0.13) +
   draw_text("n = 1253", x = 0.8, y = 0.15, size = 12)

#### HYDROCHOERUS ####
especie1 <- registers %>% 
   filter(Species == "hydrochoerus") #ESPECIE QUE QUIERO

ymax <- 110

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Hydrochoerus hydrochaeris*", 
           #subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            # plot.background = element_rect(fill = "#f4f0cb", color = "#f4f0cb"),
            # panel.background = element_rect(fill = "#f4f0cb"),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.508, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.5, y = 0.76, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(hydrochoerus), x = 0.235, y = 0.795,
              width = 0.13, height = 0.13) +
   draw_text("n = 991", x = 0.8, y = 0.15, size = 12)

#### CANIDAE ####
especie1 <- registers %>% 
   filter(Species == "canidae") #ESPECIE QUE QUIERO

ymax <- 20

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Canidae*", 
           #subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            # plot.background = element_rect(fill = "#f4f0cb", color = "#f4f0cb"),
            # panel.background = element_rect(fill = "#f4f0cb"),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.49, y = 0.76, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(canidae), x = 0.217, y = 0.8,
              width = 0.13, height = 0.13) +
   draw_text("n = 124", x = 0.8, y = 0.15, size = 12)

#### SUS ####
especie1 <- registers %>% 
   filter(Species == "sus") #ESPECIE QUE QUIERO

ymax <- 20

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Sus scrofa*", 
           #subtitle = "En los muestreos de jl17 y jl18",
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            # plot.background = element_rect(fill = "#f4f0cb", color = "#f4f0cb"),
            # panel.background = element_rect(fill = "#f4f0cb"),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.49, y = 0.76, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(sus), x = 0.23, y = 0.79,
              width = 0.13, height = 0.13) +
   draw_text("n = 85", x = 0.8, y = 0.15, size = 12)

#### RHEA ####

especie1 <- registers %>% 
   filter(Species == "rhea") #ESPECIE QUE QUIERO

ymax <- 20

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.5) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Rhea americana*", 
           #subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            # plot.background = element_rect(fill = "#f4f0cb", color = "#f4f0cb"),
            # panel.background = element_rect(fill = "#f4f0cb"),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.49, y = 0.76, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(rhea, x = 0.21, y = 0.782,
              width = 0.13, height = 0.13) +
   draw_text("n = 147", x = 0.8, y = 0.15, size = 12)

#### CINGULATA ####
especie1 <- registers %>% 
   filter(Species == "cingulata") #ESPECIE QUE QUIERO

ymax <- 22

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Cingulata*", 
           #subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            # plot.background = element_rect(fill = "#f4f0cb", color = "#f4f0cb"),
            # panel.background = element_rect(fill = "#f4f0cb"),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.5, y = 0.76, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(cingulata), x = 0.21, y = 0.8,
              width = 0.13, height = 0.13) +
   draw_text("n = 139", x = 0.8, y = 0.15, size = 12)

#### MAZAMA ####
especie1 <- registers %>% 
   filter(Species == "mazama") #ESPECIE QUE QUIERO

ymax <- 20

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Mazama gouazoubira*", 
           #subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.3),
            # plot.background = element_rect(fill = "#f4f0cb", color = "#f4f0cb"),
            # panel.background = element_rect(fill = "#f4f0cb"),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.5, y = 0.76, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(mazama), x = 0.21, y = 0.79,
              width = 0.13, height = 0.13) +
   draw_text("n = 47", x = 0.8, y = 0.15, size = 12)


#### LEOPARDUS ####
especie1 <- registers %>% 
   filter(Species == "leopardus") #ESPECIE QUE QUIERO

ymax <- 20

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.4) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Leopardus geoffroyi*", 
           #subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            # plot.background = element_rect(fill = "#f4f0cb", color = "#f4f0cb"),
            # panel.background = element_rect(fill = "#f4f0cb"),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.49, y = 0.76, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(leopardus), x = 0.245, y = 0.8,
              width = 0.13, height = 0.13) +
   draw_text("n = 42", x = 0.77, y = 0.15, size = 12)
