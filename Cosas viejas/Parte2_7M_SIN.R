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

datacam <- read.csv("datacam.csv", 
                            header = T,
                            sep = ",",
                            row.names = 1)
datacam$DateTimeOriginal <- as.factor(datacam$DateTimeOriginal)

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
registers <- datacam 
registers$Time <- as.character(registers$Time)
registers$decimal <- sapply(strsplit(registers$Time,":"), function(x){
   x <- as.numeric(x)
   x[1]+x[2]/60+ x[3]/3600})

#### Axis ####

especie1 <- registers %>% 
   filter(Species == "axis")

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0,2500) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Axis axis*", 
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_minimal() + 
      theme(text = element_text(size = 13, face = "bold"), 
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), 
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), 
            plot.title = element_markdown()))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = 2500, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = 2500, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5055, y = 0.20, 
              width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.5, y = 0.76, 
              width = 0.08, height = 0.07,
              scale = 0.75) + 
   draw_image(axis, x = 0.16, y = 0.878,
              width = 0.08, height = 0.08)

#### Hydrochoerus ####
especie1 <- registers %>% 
   filter(Species == "hydrochoerus") 

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0,2500) + 
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Hydrochoerus hydrochaeris*", 
          y = "Número de registros") +
      coord_polar(start = 0) +
      theme_minimal() + 
      theme(text = element_text(size = 13, face = "bold"), 
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), 
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), 
            plot.title = element_markdown()) 
)

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = 2500, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = 2500, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.505, y = 0.20, 
              width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.49, y = 0.76, # 
              width = 0.08, height = 0.07, 
              scale = 0.75) + 
   draw_image(image_flop(hydrochoerus), x = 0.16, y = 0.878,
              width = 0.08, height = 0.08)

#### Canidae ####
especie1 <- registers %>% 
   filter(Species == "canidae") #ESPECIE QUE QUIERO

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0,100) + 
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Canidae*", 
          y = "Número de registros") +
      coord_polar(start = 0) +
      theme_minimal() + 
      theme(text = element_text(size = 13, face = "bold"),
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),
            plot.title = element_markdown())
)

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = 100, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = 100, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.505, y = 0.20, 
              width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.49, y = 0.76,
              width = 0.08, height = 0.07, 
              scale = 0.75) + 
   draw_image(image_flop(canidae), x = 0.16, y = 0.878,
              width = 0.08, height = 0.08)

#### Sus ####
especie1 <- registers %>% 
   filter(Species == "sus")

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0,100) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Sus scrofa*", 
          y = "Número de registros") +
      coord_polar(start = 0) +
      theme_minimal() + 
      theme(text = element_text(size = 13, face = "bold"), 
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),
            plot.title = element_markdown()) 
)

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = 100, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = 100, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.505, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.49, y = 0.76, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(sus), x = 0.16, y = 0.878,
              width = 0.08, height = 0.08)

#### Rhea ####
especie1 <- registers %>% 
   filter(Species == "rhea") #ESPECIE QUE QUIERO

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0,250) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Rhea americana*", 
           #subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_minimal() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 13, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown()) # Para  hacer la especie en itálica
)

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = 250, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = 250, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.505, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.49, y = 0.76, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(rhea, x = 0.16, y = 0.878,
              width = 0.08, height = 0.08)

#### Cingulata ####
especie1 <- registers %>% 
   filter(Species == "cingulata") #ESPECIE QUE QUIERO

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0,100) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Cingulata*", 
           #subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_minimal() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 13, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown()) # Para  hacer la especie en itálica
)

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = 100, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = 100, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.493, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.49, y = 0.76, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(cingulata), x = 0.16, y = 0.878,
              width = 0.08, height = 0.08)

#### Mazama ####
especie1 <- registers %>% 
   filter(Species == "mazama") #ESPECIE QUE QUIERO

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0,100) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Mazama gouarzoubira*", 
           #subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_minimal() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 13, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown()) # Para  hacer la especie en itálica
)

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = 100, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = 100, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.505, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.49, y = 0.76, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(mazama), x = 0.16, y = 0.878,
              width = 0.08, height = 0.08)

#### Leopardus ####
especie1 <- registers %>% 
   filter(Species == "leopardus") #ESPECIE QUE QUIERO

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0,100) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Leopardus geoffroyi*", 
           #subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_minimal() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 13, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown()) # Para  hacer la especie en itálica
)

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = 100, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = 100, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.505, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.49, y = 0.76, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(leopardus), x = 0.16, y = 0.878,
              width = 0.08, height = 0.08)


#CUANDO LOS PNG ESTÁN AL REVEZ, VOY A LLAMAR DIRECTAMENTE A LA PÁGINA CON IMAGE_READ Y DESPUÉS PONGO LA IMÁGEN CON IMAGE_FLOP Y LA DA VUELTA
