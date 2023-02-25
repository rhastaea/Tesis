#### ANÁLISIS DE CÁMARAS TRAMPA ####
rm(list = ls())

#library(camtrapR)
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

datacam <- read.csv("datacam_final.csv", 
                    header = T,
                    sep = ";",
                    row.names = 1)
datacam$DateTimeOriginal <- as.factor(datacam$DateTimeOriginal)

#### PNGs ####
sun <- readPNG("sun.png")
moon <- readPNG("moon.png")
rhea <- readPNG("rhea.png")
axis <- readPNG("axis.png")
hydrochoerus <- image_flop(image_read("https://images.phylopic.org/images/9c234021-ce53-45d9-8fdd-b0ca3115a451/raster/1024x596.png?build=103"))
sus <- image_flop(image_read("http://phylopic.org/assets/images/submissions/3d8acaf6-4355-491e-8e86-4a411b53b98b.128.png"))
cingulata <- image_flop(image_read("https://images.phylopic.org/images/5d59b5ce-c1dd-40f6-b295-8d2629b9775e/raster/1024x493.png?build=103"))
mazama <- image_flop(image_read("http://phylopic.org/assets/images/submissions/b5f40112-0cb8-4994-aa70-28ac97ccb83f.128.png"))
canidae <- image_flop(image_read("http://phylopic.org/assets/images/submissions/d67d3bf6-3509-4ab6-819a-cd409985347e.128.png"))
leopardus <- image_flop(image_read("https://www.phylopic.org/images/2fc7bbbf-8ca7-48fb-8495-d351cd3b1f99"))

#### Hist_rad ####
registers <- datacam 
registers$Time <- as.character(registers$Time)
registers$decimal <- sapply(strsplit(registers$Time,":"), function(x){
   x <- as.numeric(x)
   x[1]+x[2]/60+ x[3]/3600})
estacion <- "A"

#### Axis ####
especie1 <- registers %>% filter(Species == "axis")

ymax = 2500
pngs <- axis
n <- "n = 16015"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + 
      theme(text = element_text(size = 17, face = "bold"), 
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), 
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), 
            plot.title = element_markdown(size = 25, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.52, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.515, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(pngs, x = 0.15, y = 0.83, width = 0.15, height = 0.15) +
   draw_label(n, x = 0.86, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.23, y = 0.15, size = 25, fontface = "bold")

#### Hydrochoerus ####
especie1 <- registers %>% filter(Species == "hydrochoerus")

ymax = 2500
pngs <- hydrochoerus
n <- "n = 14490"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + 
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + 
      theme(text = element_text(size = 17, face = "bold"), 
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), 
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), 
            plot.title = element_markdown(size = 25, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.52, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.51, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(pngs, x = 0.17, y = 0.84, width = 0.15, height = 0.15) +
   draw_label(n, x = 0.86, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.23, y = 0.15, size = 25, fontface = "bold")

#### Canidae ####
especie1 <- registers %>% filter(Species == "canidae")

ymax = 100
n <- "n = 328"
pngs <- canidae

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + 
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + 
      theme(text = element_text(size = 17, face = "bold"), 
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), 
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), 
            plot.title = element_markdown(size = 25, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.515, y = 0.23, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.505, y = 0.775, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(pngs, x = 0.16, y = 0.84, width = 0.15, height = 0.15) +
   draw_label(n, x = 0.86, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.23, y = 0.15, size = 25, fontface = "bold")

#### Sus ####
especie1 <- registers %>% filter(Species == "sus")

ymax = 100
pngs <- sus
n = "n = 486"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0,100) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + 
      theme(text = element_text(size = 17, face = "bold"), 
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), 
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), 
            plot.title = element_markdown(size = 25, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", 
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.515, y = 0.23, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.505, y = 0.775, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(pngs, x = 0.16, y = 0.84, width = 0.15, height = 0.15) +
   draw_label(n, x = 0.87, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.23, y = 0.15, size = 25, fontface = "bold")

#### Rhea ####
especie1 <- registers %>% filter(Species == "rhea")

ymax = 250
pngs <- rhea
n <- "n = 1032"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + 
      theme(text = element_text(size = 17, face = "bold"), 
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), 
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), 
            plot.title = element_markdown(size = 25, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.515, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.51, y = 0.78, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(pngs, x = 0.16, y = 0.82, width = 0.15, height = 0.15) +
   draw_label(n, x = 0.87, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.23, y = 0.15, size = 25, fontface = "bold")

#### Cingulata ####
especie1 <- registers %>% filter(Species == "cingulata")

ymax = 100
pngs <- cingulata
n <- "n = 428"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + 
      theme(text = element_text(size = 17, face = "bold"), 
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), 
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), 
            plot.title = element_markdown(size = 25, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.515, y = 0.23, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.505, y = 0.775, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(pngs, x = 0.16, y = 0.84, width = 0.15, height = 0.15) +
   draw_label(n, x = 0.87, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.23, y = 0.15, size = 25, fontface = "bold")

#### Mazama ####
especie1 <- registers %>% filter(Species == "mazama")

ymax = 100
pngs <- mazama
n <- "n = 216"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + 
      theme(text = element_text(size = 17, face = "bold"), 
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), 
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), 
            plot.title = element_markdown(size = 25, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.515, y = 0.23, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.505, y = 0.775, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(pngs, x = 0.14, y = 0.83, width = 0.15, height = 0.15) +
   draw_label(n, x = 0.87, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.23, y = 0.15, size = 25, fontface = "bold")

#### Leopardus ####
especie1 <- registers %>% filter(Species == "leopardus")

ymax = 100
pngs <- leopardus
n = "n = 67"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     #aes(y = stat(count / sum(count))), #Esta línea me pone el eje y en proporción
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.3) +
      ylim(0, ymax) + #PARA TODOS MENOS PARA AXIS E HYDROCHOERUS
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + 
      theme(text = element_text(size = 17, face = "bold"), 
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")), 
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), 
            plot.title = element_markdown(size = 25, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18,0), xmax = c(24, 8),
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "grey25") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6, xmax = 20,
               ymin = 0, ymax = ymax, #VARIAR EL VALOR DE YMAX CON LA ESPECIE
               alpha = 0.3, fill = "#FFD819"))

windows(500,400)
ggdraw(plot_color) +
   draw_image(sun, x = 0.515, y = 0.23, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.505, y = 0.775, width = 0.08, height = 0.07, scale = 0.75) + 
   # draw_image(pngs, x = 0.16, y = 0.84, width = 0.15, height = 0.15) +
   draw_label(n, x = 0.87, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.23, y = 0.15, size = 25, fontface = "bold")
