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

#### CARGO DATACAM POR ESTACIONES ####
datacam <- read.csv("datacam_muestreo_delta60.csv", 
                    header = T,
                    sep = ";",
                    row.names = 1)
datacam$DateTimeOriginal <- as.factor(datacam$DateTimeOriginal)

datacam_abril <- datacam[datacam$Muestreo %in% c("ab17","ab18"),]
datacam_julio <- datacam[datacam$Muestreo %in% c("jl17","jl18"),]
datacam_verano <- datacam[datacam$Muestreo %in% c("nv17","fb18","di18"),]

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

####################################### ABRIL ########

registers_abril <- datacam_abril 
registers_abril$Time <- as.character(registers_abril$Time)
registers_abril$decimal <- sapply(strsplit(registers_abril$Time,":"), function(x){
   x <- as.numeric(x)
   x[1]+x[2]/60+ x[3]/3600})

#### Ab - Leopardus ####
especie1 <- registers_abril %>% 
   filter(Species == "leopardus") #ESPECIE QUE QUIERO

ymax <- 10

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.4) +
      scale_y_continuous(limits = c(0, ymax), breaks = seq(0, ymax, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Leopardus geoffroyi*", 
           subtitle = "En los muestreos de ab17 y ab18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.7, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(leopardus), x = 0.23, y = 0.76,
              width = 0.13, height = 0.13) +
   draw_text("n = 13", x = 0.8, y = 0.15, size = 12)

#### Ab - Rhea ####

especie1 <- registers_abril %>% 
   filter(Species == "rhea") #ESPECIE QUE QUIERO

ymax <- 10

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.5) +
      scale_y_continuous(limits = c(0,ymax), breaks = seq(0, ymax, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Rhea americana*", 
           subtitle = "En los muestreos de ab17 y ab18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.7, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(rhea, x = 0.23, y = 0.74,
              width = 0.13, height = 0.13) +
   draw_text("n = 30", x = 0.8, y = 0.15, size = 12)

#### Ab - Sus ####
especie1 <- registers_abril %>% 
   filter(Species == "sus") #ESPECIE QUE QUIERO

ymax <- 10

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      scale_y_continuous(limits = c(0,ymax), breaks = seq(0, ymax, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Sus scrofa*", 
           subtitle = "En los muestreos de ab17 y ab18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(sun, x = 0.495, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.495, y = 0.73, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(sus), x = 0.23, y = 0.77,
              width = 0.13, height = 0.13) +
   draw_text("n = 33", x = 0.8, y = 0.15, size = 12)

#### Ab - Canidae ####
especie1 <- registers_abril %>% 
   filter(Species == "canidae") #ESPECIE QUE QUIERO

ymax <- 10
   
   (plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
       geom_histogram(breaks = seq(0, 24),
                      fill = "steelblue4",
                      colour = "black", 
                      size = 0.3) +
         scale_y_continuous(limits = c(0,ymax), breaks = seq(0, ymax, by = 2)) +
       scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
       labs(title = "Registros de *Canidae*", 
            subtitle = "En los muestreos de ab17 y ab18", #NOMBRE DE LA ESPECIE QUE QUIERO
            y = "Número de registros") +
       coord_polar(start = 0) +
       theme_bw() + # Tipo de tema para quitar el gris de fondo
       theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
             axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
             axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
             plot.title = element_markdown(size = 25, hjust = 0.5),
             plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.725, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(canidae), x = 0.22, y = 0.752,
              width = 0.13, height = 0.13) +
   draw_text("n = 44", x = 0.8, y = 0.15, size = 12)

#### Ab - Axis ####
especie1 <- registers_abril %>% 
   filter(Species == "axis") #ESPECIE QUE QUIERO

ymax <- 42

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      scale_y_continuous(limits = c(0, ymax), breaks = seq(0, ymax, by = 6)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Axis axis*", 
           subtitle = "En los muestreos de ab17 y ab18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(axis, x = 0.21, y = 0.75,
              width = 0.13, height = 0.13) +
   draw_text("n = 442", x = 0.8, y = 0.15, size = 12)

#### Ab - Hydrochoerus ####
especie1 <- registers_abril %>% 
   filter(Species == "hydrochoerus") #ESPECIE QUE QUIERO

ymax <- 42

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      scale_y_continuous(limits = c(0,ymax), breaks = seq(0, ymax, by = 6)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Hydrochoerus hydrochaeris*", 
           subtitle = "En los muestreos de ab17 y ab18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.73, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(hydrochoerus), x = 0.235, y = 0.765,
              width = 0.13, height = 0.13) +
   draw_text("n = 384", x = 0.8, y = 0.15, size = 12)

#### Ab - Mazama ####
especie1 <- registers_abril %>% 
   filter(Species == "mazama") #ESPECIE QUE QUIERO

ymax <- 3

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Mazama gouazoubira*", 
           subtitle = "En los muestreos de ab17 y ab18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.3),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.73, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(mazama), x = 0.225, y = 0.76,
              width = 0.13, height = 0.13) +
   draw_text("n = 12", x = 0.8, y = 0.15, size = 12)

#### Ab - Cingulata ####
especie1 <- registers_abril %>% 
   filter(Species == "cingulata") #ESPECIE QUE QUIERO

ymax <- 9

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Cingulata*", 
           subtitle = "En los muestreos de ab17 y ab18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.73, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(cingulata), x = 0.23, y = 0.765,
              width = 0.13, height = 0.13) +
   draw_text("n = 52", x = 0.8, y = 0.15, size = 12)

###################################### JULIO ########

registers_julio <- datacam_julio 
registers_julio$Time <- as.character(registers_julio$Time)
registers_julio$decimal <- sapply(strsplit(registers_julio$Time,":"), function(x){
   x <- as.numeric(x)
   x[1]+x[2]/60+ x[3]/3600})

#### Jl - Leopardus ####
especie1 <- registers_julio %>% 
   filter(Species == "leopardus") #ESPECIE QUE QUIERO

ymax <- 2

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.4) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Leopardus geoffroyi*", 
           subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.7, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(leopardus), x = 0.23, y = 0.76,
              width = 0.13, height = 0.13) +
   draw_text("n = 10", x = 0.8, y = 0.15, size = 12)

#### Jl - Rhea ####

especie1 <- registers_julio %>% 
   filter(Species == "rhea") #ESPECIE QUE QUIERO

ymax <- 10

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.5) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Rhea americana*", 
           subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.7, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(rhea, x = 0.23, y = 0.74,
              width = 0.13, height = 0.13) +
   draw_text("n = 71", x = 0.8, y = 0.15, size = 12)

#### Jl - Sus ####
especie1 <- registers_julio %>% 
   filter(Species == "sus") #ESPECIE QUE QUIERO

ymax <- 4

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Sus scrofa*", 
           subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(sun, x = 0.495, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.46, y = 0.73, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(sus), x = 0.23, y = 0.77,
              width = 0.13, height = 0.13) +
   draw_text("n = 25", x = 0.8, y = 0.15, size = 12)

#### Jl - Canidae ####
especie1 <- registers_julio %>% 
   filter(Species == "canidae") #ESPECIE QUE QUIERO

ymax <- 7

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Canidae*", 
           subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(image_flop(canidae), x = 0.22, y = 0.77,
              width = 0.13, height = 0.13) +
   draw_text("n = 70", x = 0.8, y = 0.15, size = 12)

#### Jl - Axis ####
especie1 <- registers_julio %>% 
   filter(Species == "axis") #ESPECIE QUE QUIERO

ymax <- 43

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Axis axis*", 
           subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(axis, x = 0.23, y = 0.79,
              width = 0.13, height = 0.13) +
   draw_text("n = 393", x = 0.8, y = 0.15, size = 12)

#### Jl - Hydrochoerus ####
especie1 <- registers_julio %>% 
   filter(Species == "hydrochoerus") #ESPECIE QUE QUIERO

ymax <- 46

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Hydrochoerus hydrochaeris*", 
           subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.73, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(hydrochoerus), x = 0.235, y = 0.765,
              width = 0.13, height = 0.13) +
   draw_text("n = 397", x = 0.8, y = 0.15, size = 12)

#### Jl - Mazama ####
especie1 <- registers_julio %>% 
   filter(Species == "mazama") #ESPECIE QUE QUIERO

ymax <- 2

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Mazama gouazoubira*", 
           subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.3),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.73, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(mazama), x = 0.225, y = 0.76,
              width = 0.13, height = 0.13) +
   draw_text("n = 15", x = 0.8, y = 0.15, size = 12)

#### Jl - Cingulata ####
especie1 <- registers_julio %>% 
   filter(Species == "cingulata") #ESPECIE QUE QUIERO

ymax <- 11

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      #ylim(0, ymax) + 
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      scale_y_continuous(limits = c(0,ymax), breaks = seq(0, ymax, by = 2)) +
      labs(title = "Registros de *Cingulata*", 
           subtitle = "En los muestreos de jl17 y jl18", #NOMBRE DE LA ESPECIE QUE QUIERO
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.73, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(cingulata), x = 0.23, y = 0.765,
              width = 0.13, height = 0.13) +
   draw_text("n = 67", x = 0.8, y = 0.15, size = 12)

###################################### VERANO ########

registers_verano <- datacam_verano
registers_verano$Time <- as.character(registers_verano$Time)
registers_verano$decimal <- sapply(strsplit(registers_verano$Time,":"), function(x){
   x <- as.numeric(x)
   x[1]+x[2]/60+ x[3]/3600})

#### Ve - Leopardus ####
especie1 <- registers_verano %>% 
   filter(Species == "leopardus")

ymax <- 3

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.4) +
      ylim(0, ymax) + 
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Leopardus geoffroyi*", 
           subtitle = "En los muestreos de nv17, fb18 y di18",
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + 
      theme(text = element_text(size = 17, face = "bold"), 
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, 
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax,
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.20,
              width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.49, y = 0.7,
              width = 0.08, height = 0.07,
              scale = 0.75) + 
   draw_image(image_flop(leopardus), x = 0.23, y = 0.76,
              width = 0.13, height = 0.13) +
   draw_text("n = 19", x = 0.8, y = 0.15, size = 12)


#### Ve - Rhea ####

especie1 <- registers_verano %>% 
   filter(Species == "rhea") #ESPECIE QUE QUIERO

ymax <- 6

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.5) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Rhea americana*", 
           subtitle = "En los muestreos de nv17, fb18 y di18",
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.7, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(rhea, x = 0.23, y = 0.74,
              width = 0.13, height = 0.13) +
   draw_text("n = 46", x = 0.8, y = 0.15, size = 12)

#### Ve - Sus ####
especie1 <- registers_verano %>% 
   filter(Species == "sus") #ESPECIE QUE QUIERO

ymax <- 4

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Sus scrofa*", 
           subtitle = "En los muestreos de nv17, fb18 y di18",
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(sun, x = 0.495, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.46, y = 0.73, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(sus), x = 0.23, y = 0.77,
              width = 0.13, height = 0.13) +
   draw_text("n = 27", x = 0.8, y = 0.15, size = 12)

#### Ve - Canidae ####
especie1 <- registers_verano %>%  
   filter(Species == "canidae") #ESPECIE QUE QUIERO

ymax <- 3

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Canidae*", 
           subtitle = "En los muestreos de nv17, fb18 y di18",
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(image_flop(canidae), x = 0.22, y = 0.77,
              width = 0.13, height = 0.13) +
   draw_text("n = 10", x = 0.8, y = 0.15, size = 12)

#### Ve - Axis ####
especie1 <- registers_verano %>% 
   filter(Species == "axis") #ESPECIE QUE QUIERO

ymax <- 41

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Axis axis*", 
           subtitle = "En los muestreos de nv17, fb18 y di18",
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(axis, x = 0.23, y = 0.79,
              width = 0.13, height = 0.13) +
   draw_text("n = 418", x = 0.8, y = 0.15, size = 12)

#### Ve - Hydrochoerus ####
especie1 <- registers_verano %>% 
   filter(Species == "hydrochoerus") #ESPECIE QUE QUIERO

ymax <- 27

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Hydrochoerus hydrochaeris*", 
           subtitle = "En los muestreos de nv17, fb18 y di18",
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.73, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(hydrochoerus), x = 0.235, y = 0.765,
              width = 0.13, height = 0.13) +
   draw_text("n = 210", x = 0.8, y = 0.15, size = 12)

#### Ve - Mazama ####
especie1 <- registers_verano %>% 
   filter(Species == "mazama") #ESPECIE QUE QUIERO

ymax <- 2

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Mazama gouazoubira*", 
           subtitle = "En los muestreos de nv17, fb18 y di18",
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.3),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.73, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(mazama), x = 0.225, y = 0.76,
              width = 0.13, height = 0.13) +
   draw_text("n = 20", x = 0.8, y = 0.15, size = 12)

#### Ve - Cingulata ####
especie1 <- registers_verano %>% 
   filter(Species == "cingulata") #ESPECIE QUE QUIERO

ymax <- 4

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Cingulata*", 
           subtitle = "En los muestreos de nv17, fb18 y di18",
           y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 17, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown(size = 25, hjust = 0.3),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
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
   draw_image(moon, x = 0.49, y = 0.73, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) + 
   draw_image(image_flop(cingulata), x = 0.23, y = 0.765,
              width = 0.13, height = 0.13) +
   draw_text("n = 20", x = 0.8, y = 0.15, size = 12)

