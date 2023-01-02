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

datastation <- read.csv("datastation_final.csv", header = TRUE, sep = ",")

#### Datacam sin independencia ####
datacam <- read.csv("datacam.csv", header = T, sep = ",", row.names = 1)
datacam$DateTimeOriginal <- as.factor(datacam$DateTimeOriginal)

#### Datacam delta 60 ####
datacam_delta60 <- read.csv("datacam_delta60.csv", header = T, sep = ",", row.names = 1)
datacam_delta60$DateTimeOriginal <- as.factor(datacam_delta60$DateTimeOriginal)

#### Overlap ####
library(overlap)

speciesA_for_activity <- "axis"
speciesB_for_activity <- "mazama"

activityOverlap (recordTable   = datacam_delta60,
                 speciesA      = speciesA_for_activity,
                 speciesB      = speciesB_for_activity,
                 writePNG      = FALSE,#Si pongo TRUE me lo guarda en plotDirectory
                 plotDirectory = "D:/TESIS/Overlap",
                 plotR         = TRUE,
                 createDir     = FALSE,
                 pngMaxPix     = 1000,
                 linecol       = c("black", "blue"),
                 linewidth     = c(3,3),
                 linetype      = c(1, 2),
                 olapcol       = "darkgrey",
                 add.rug       = TRUE,
                 #extend       = "lightgrey",
                 ylim          = c(0, 0.11),
                 main          = paste("Activity overlap between", 
                                     speciesA_for_activity, "and", 
                                     speciesB_for_activity))

#### Histograma radial con superposición ####

registers <- datacam #ACÁ HAY QUE CAMBIAR EL DATACAM QUE QUIERO

registers$Time <- as.character(registers$Time)
registers$decimal <- sapply(strsplit(registers$Time,":"), function(x){
   x <- as.numeric(x)
   x[1]+x[2]/60+ x[3]/3600})

especie1 <- registers %>% filter(Species == "axis")
especie2 <- registers %>% filter(Species == "mazama")
especies = rbind(especie2,especie1)

(plot_blanco <- ggplot(especies, aes(x = decimal, fill = Species)) + 
      geom_density(breaks = seq(0, 24),
                  #aes(y = stat(count / sum(count))),
                  position = "identity", #Para que superponga las especies
                  colour = "black", 
                  alpha = 0.5, #transparencia
                  size = 0.3,
                  ylim = c(0,0.1)) +
      scale_x_continuous("", limits = c(0, 24), 
                         breaks = seq(0, 24), 
                         labels = seq(0, 24)) +
      scale_fill_manual(values = c("blue", "red")) +
      labs(title = "Registros de *Axis axis* y *Mazama Gouarzoubira*",
           y = "Número de registros") +
           #subtitle = "Durante los muestreos de abril 2017 y 2018") + #FECHA MUESTREOS
      #coord_polar(start = 0) +
      theme_minimal() + # Tipo de tema para quitar el gris de fondo
      theme(text = element_text(size = 13, face = "bold"), # Tamaño y letra en negrilla
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), # Margenes de x
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), # Margenes de y
            plot.title = element_markdown()) # Para  hacer la especie en itálica
)

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 6.5),
               ymin = 0, ymax = 2000, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 19,
               ymin = 0, ymax = 2000, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.45, y = 0.20, # Coordenadas en x y del sol
              width = 0.07, height = 0.06) + # Altura y ancho
   draw_image(moon, x = 0.45, y = 0.76, # Coordenadas en x y de la luna
              width = 0.08, height = 0.07, # Altura y ancho
              scale = 0.75) # Como la imágen de la luna es algo más grande la escalamos para que iguale al sol
