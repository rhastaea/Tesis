#### ANÁLISIS DE CÁMARAS TRAMPA ####
rm(list = ls())

# library(camtrapR)
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
datacam <- read.csv("datacam_d60_final.csv", 
                    header = T,
                    sep = ";",
                    row.names = 1)
datacam$DateTimeOriginal <- as.factor(datacam$DateTimeOriginal)

# datacam_abril <- datacam[datacam$Muestreo %in% c("ab17","ab18"),]
# datacam_julio <- datacam[datacam$Muestreo %in% c("jl17","jl18"),]
# datacam_verano <- datacam[datacam$Muestreo %in% c("nv17","fb18","di18"),]

datacam_otono <- datacam[datacam$Estacion %in% c("Otono"),]
datacam_invierno <- datacam[datacam$Estacion %in% c("Invierno"),]
datacam_prive <- datacam[datacam$Estacion %in% c("Prive"),]

ymax_AeH <- 50
ymax_resto <- 12

#### PNGs ####
sun <- readPNG("sun.png")
moon <- readPNG("moon.png")
copo <- readPNG("copo.png")
flor <- readPNG("flor.png")
hoja <- readPNG("hoja.png")
rhea <- readPNG("rhea.png")
axis <- readPNG("axis.png")
hydrochoerus <- image_flop(image_read("https://images.phylopic.org/images/9c234021-ce53-45d9-8fdd-b0ca3115a451/raster/1024x596.png?build=103"))
sus <- image_flop(image_read("http://phylopic.org/assets/images/submissions/3d8acaf6-4355-491e-8e86-4a411b53b98b.128.png"))
cingulata <- image_flop(image_read("https://images.phylopic.org/images/5d59b5ce-c1dd-40f6-b295-8d2629b9775e/raster/1024x493.png?build=103"))
mazama <- image_flop(image_read("http://phylopic.org/assets/images/submissions/b5f40112-0cb8-4994-aa70-28ac97ccb83f.128.png"))
canidae <- image_flop(image_read("http://phylopic.org/assets/images/submissions/d67d3bf6-3509-4ab6-819a-cd409985347e.128.png"))
leopardus <- image_flop(image_read("http://phylopic.org/assets/images/submissions/cbc3f93e-0ce3-4e3b-871d-6ac688ed8810.128.png"))


####################################### OTOÑO ########

registers_otono <- datacam_otono
registers_otono$Time <- as.character(registers_otono$Time)
registers_otono$decimal <- sapply(strsplit(registers_otono$Time,":"), function(x){
   x <- as.numeric(x)
   x[1]+x[2]/60+ x[3]/3600})
estacion <- "A"

#### Otono - Axis ####
especie1 <- registers_otono %>% filter(Species == "axis")  
pngs <- axis
n <- "n = 316"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0, ymax_AeH), breaks = seq(0, ymax_AeH, by = 10)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.1,0), xmax = c(24, 7.367),
               ymin = 0, ymax = ymax_AeH,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.283,
               ymin = 0, ymax = ymax_AeH,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(hoja, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(pngs, x = 0.18, y = 0.84, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Otono - Hydrochoerus ####
especie1 <- registers_otono %>% filter(Species == "hydrochoerus") 
pngs <- hydrochoerus
n <- "n = 293"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_AeH), breaks = seq(0, ymax_AeH, by = 10)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.1,0), xmax = c(24, 7.367),
               ymin = 0, ymax = ymax_AeH,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.283,
               ymin = 0, ymax = ymax_AeH,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(hoja, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(pngs, x = 0.2, y = 0.84, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Otono - Canidae ####
especie1 <- registers_otono %>% filter(Species == "canidae")
pngs <- canidae
n <- "n = 27"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.1,0), xmax = c(24, 7.367),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.283,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(hoja, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(pngs, x = 0.19, y = 0.84, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Otono - Sus ####
especie1 <- registers_otono %>% filter(Species == "sus")  
pngs <- sus
n <- "n = 28"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.1,0), xmax = c(24, 7.367),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.283,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(hoja, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(pngs, x = 0.19, y = 0.84, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Otono - Rhea ####
especie1 <- registers_otono %>% filter(Species == "rhea")  
pngs <- rhea
n <- "n = 25"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.5) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.1,0), xmax = c(24, 7.367),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.283,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(hoja, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(pngs, x = 0.18, y = 0.84, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Otono - Cingulata ####
especie1 <- registers_otono %>% filter(Species == "cingulata")  
pngs <- cingulata
n <- "n = 35"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.1,0), xmax = c(24, 7.367),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.283,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(hoja, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(cingulata, x = 0.18, y = 0.84, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Otono - Mazama ####
especie1 <- registers_otono %>% filter(Species == "mazama")  
pngs <- mazama
n <- "n = 12"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.3),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.1,0), xmax = c(24, 7.367),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.283,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(hoja, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(mazama, x = 0.175, y = 0.84, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Otono - Leopardus ####
especie1 <- registers_otono %>% filter(Species == "leopardus") 
pngs <- leopardus
n <- "n = 11"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.4) +
      scale_y_continuous(limits = c(0, ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.1,0), xmax = c(24, 7.367),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.283,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(hoja, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(leopardus, x = 0.19, y = 0.84, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

###################################### INVIERNO ########

registers_invierno <- datacam_invierno 
registers_invierno$Time <- as.character(registers_invierno$Time)
registers_invierno$decimal <- sapply(strsplit(registers_invierno$Time,":"), function(x){
   x <- as.numeric(x)
   x[1]+x[2]/60+ x[3]/3600})
estacion <- "B"

#### Invierno - Axis ####
especie1 <- registers_invierno %>% filter(Species == "axis") 
pngs <- axis
n <- "n = 448"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      scale_y_continuous(limits = c(0,ymax_AeH), breaks = seq(0, ymax_AeH, by = 10)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(17.917,0), xmax = c(24, 7.883),
               ymin = 0, ymax = ymax_AeH,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.833,
               ymin = 0, ymax = ymax_AeH,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.40, y = 0.76, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(copo, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(axis, x = 0.18, y = 0.84, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Invierno - Hydrochoerus ####
especie1 <- registers_invierno %>% filter(Species == "hydrochoerus") 
pngs <- hydrochoerus
n <- "n = 396"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_AeH), breaks = seq(0, ymax_AeH, by = 10)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(17.917,0), xmax = c(24, 7.883),
               ymin = 0, ymax = ymax_AeH,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.833,
               ymin = 0, ymax = ymax_AeH,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(copo, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(hydrochoerus, x = 0.195, y = 0.84, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Invierno - Canidae ####
especie1 <- registers_invierno %>% filter(Species == "canidae")  
pngs <- canidae
n <- "n = 66"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(17.917,0), xmax = c(24, 7.883),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.833,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(copo, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(canidae, x = 0.19, y = 0.85, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Invierno - Sus ####
especie1 <- registers_invierno %>% filter(Species == "sus")  
pngs <- sus
n <- "n = 18"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(17.917,0), xmax = c(24, 7.883),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.833,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(copo, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(sus, x = 0.195, y = 0.84, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Invierno - Rhea ####
especie1 <- registers_invierno %>% filter(Species == "rhea") 
pngs <- rhea
n <- "n = 50"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.5) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(17.917,0), xmax = c(24, 7.883),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.833,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(copo, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(rhea, x = 0.19, y = 0.84, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Invierno - Cingulata ####
especie1 <- registers_invierno %>% filter(Species == "cingulata") 
pngs <- cingulata
n <- "n = 66"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(17.917,0), xmax = c(24, 7.883),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.833,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(copo, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(cingulata, x = 0.19, y = 0.85, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Invierno - Mazama ####
especie1 <- registers_invierno %>% filter(Species == "mazama")
pngs <- mazama
n <- "n = 9"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.3),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(17.917,0), xmax = c(24, 7.883),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.833,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(copo, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(mazama, x = 0.19, y = 0.85, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Invierno - Leopardus ####
especie1 <- registers_invierno %>% filter(Species == "leopardus")  
pngs <- leopardus
n <- "n = 10"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.4) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(17.917,0), xmax = c(24, 7.883),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.65, xmax = 18.833,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(copo, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(leopardus, x = 0.19, y = 0.85, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

###################################### PRIVE ########

registers_prive <- datacam_prive
registers_prive$Time <- as.character(registers_prive$Time)
registers_prive$decimal <- sapply(strsplit(registers_prive$Time,":"), function(x){
   x <- as.numeric(x)
   x[1]+x[2]/60+ x[3]/3600})
estacion <- "C"

#### Prive - Axis ####
especie1 <- registers_prive %>% filter(Species == "axis") 
pngs <- axis
n <- "n = 489"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_AeH), breaks = seq(0, ymax_AeH, by = 10)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.833,0), xmax = c(24, 6.850),
               ymin = 0, ymax = ymax_AeH,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.617, xmax = 19.183,
               ymin = 0, ymax = ymax_AeH,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.42, y = 0.77, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(flor, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(axis, x = 0.18, y = 0.85, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Prive - Hydrochoerus ####
especie1 <- registers_prive %>% filter(Species == "hydrochoerus") 
pngs <- hydrochoerus
n <- "n = 302"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_AeH), breaks = seq(0, ymax_AeH, by = 10)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.833,0), xmax = c(24, 6.850),
               ymin = 0, ymax = ymax_AeH,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.617, xmax = 19.183,
               ymin = 0, ymax = ymax_AeH,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(flor, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(hydrochoerus, x = 0.195, y = 0.85, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Prive - Canidae ####
especie1 <- registers_prive %>% filter(Species == "canidae")
pngs <- canidae
n <- "n = 31"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.833,0), xmax = c(24, 6.850),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.617, xmax = 19.183,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(flor, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(pngs, x = 0.19, y = 0.85, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Prive - Sus ####
especie1 <- registers_prive %>% filter(Species == "sus")
pngs <- sus
n <- "n = 39"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.833,0), xmax = c(24, 6.850),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.617, xmax = 19.183,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(flor, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(pngs, x = 0.19, y = 0.85, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Prive - Rhea ####
especie1 <- registers_prive %>% filter(Species == "rhea")  
pngs <- rhea
n <- "n = 72"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.5) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.833,0), xmax = c(24, 6.850),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.617, xmax = 19.183,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(flor, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(pngs, x = 0.19, y = 0.85, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Prive - Cingulata ####
especie1 <- registers_prive %>% filter(Species == "cingulata")  
pngs <- cingulata
n <- "n = 38"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.3),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.833,0), xmax = c(24, 6.850),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.617, xmax = 19.183,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(flor, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(pngs, x = 0.19, y = 0.85, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Prive - Mazama ####
especie1 <- registers_prive %>% filter(Species == "mazama") 
pngs <- mazama
n <- "n = 26"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() +  
      theme(text = element_text(size = 17, face = "bold"),  
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.3),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.833,0), xmax = c(24, 6.850),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.617, xmax = 19.183,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(flor, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(pngs, x = 0.19, y = 0.84, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

#### Prive - Leopardus ####
especie1 <- registers_prive %>% filter(Species == "leopardus")
pngs <- leopardus
n <- "n = 21"

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.4) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(y = "Número de registros") +
      coord_polar(start = 0) +
      theme_bw() + 
      theme(text = element_text(size = 17, face = "bold"), 
            axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),  
            axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),  
            plot.title = element_markdown(size = 25, hjust = 0.5),
            plot.subtitle = element_markdown(size = 16, hjust = 0.5),
            panel.grid = element_line(color = "grey")))

(plot_color <- plot_blanco +
      annotate("rect", #Sombreado gris
               xmin = c(18.833,0), xmax = c(24, 6.850),
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#696969") + 
      annotate("rect", #Sombreado amarillo
               xmin = 6.617, xmax = 19.183,
               ymin = 0, ymax = ymax_resto,  
               alpha = 0.3, fill = "#FFD819"))

ggdraw(plot_color) +
   draw_image(sun, x = 0.5, y = 0.21, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.495, y = 0.79, width = 0.08, height = 0.07, scale = 0.75) + 
   #draw_image(flor, x = 0.78, y = 0.85, width = 0.1, height = 0.1) +
   draw_image(pngs, x = 0.19, y = 0.85, width = 0.13, height = 0.13) +
   draw_label(n, x = 0.82, y = 0.15, size = 14, fontface = "bold") + 
   draw_label(estacion, x = 0.25, y = 0.15, size = 25, fontface = "bold")

