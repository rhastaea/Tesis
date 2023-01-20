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
datacam <- read.csv("datacam_muestreo_delta60.csv", 
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

#### Otono - Axis ####
especie1 <- registers_otono %>% 
   filter(Species == "axis")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0, ymax_AeH), breaks = seq(0, ymax_AeH, by = 10)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Axis axis*",
           subtitle = "En Otoño",
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.49, y = 0.7, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(hoja, x = 0.71, y = 0.75, width = 0.1, height = 0.1) +
   draw_image(axis, x = 0.23, y = 0.75, width = 0.13, height = 0.13) +
   draw_text("n = 316", x = 0.77, y = 0.15, size = 12)

#### Otono - Hydrochoerus ####
especie1 <- registers_otono %>% filter(Species == "hydrochoerus")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_AeH), breaks = seq(0, ymax_AeH, by = 10)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Hydrochoerus hydrochaeris*",
           subtitle = "En Otoño",
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.2, width = 0.07, height = 0.06) + 
   draw_image(moon, x = 0.49, y = 0.725, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(hoja, x = 0.71, y = 0.79, width = 0.1, height = 0.1) +
   draw_image(hydrochoerus, x = 0.24, y = 0.77, width = 0.13, height = 0.13) +
   draw_text("n = 293", x = 0.77, y = 0.15, size = 14)

#### Otono - Canidae ####
especie1 <- registers_otono %>% filter(Species == "canidae")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Canidae*", 
           subtitle = "En Otoño",  
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.725, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(hoja, x = 0.71, y = 0.75, width = 0.1, height = 0.1) +
   draw_image(canidae, x = 0.24, y = 0.752, width = 0.13, height = 0.13) +
   draw_text("n = 27", x = 0.77, y = 0.15, size = 12)

#### Otono - Sus ####
especie1 <- registers_otono %>% filter(Species == "sus")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Sus scrofa*", 
           subtitle = "En Otoño",  
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) +
   draw_image(hoja, x = 0.71, y = 0.75, width = 0.1, height = 0.1) +
   draw_image(sus, x = 0.24, y = 0.75, width = 0.13, height = 0.13) +
   draw_text("n = 28", x = 0.77, y = 0.15, size = 12)

#### Otono - Rhea ####

especie1 <- registers_otono %>% filter(Species == "rhea")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.5) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Rhea americana*", 
           subtitle = "En Otoño",  
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) +
   draw_image(hoja, x = 0.71, y = 0.75, width = 0.1, height = 0.1) +
   draw_image(rhea, x = 0.24, y = 0.74, width = 0.13, height = 0.13) +
   draw_text("n = 25", x = 0.77, y = 0.15, size = 12)

#### Otono - Cingulata ####
especie1 <- registers_otono %>% filter(Species == "cingulata")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Cingulata*", 
           subtitle = "En Otoño",  
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(hoja, x = 0.71, y = 0.75, width = 0.1, height = 0.1) +
   draw_image(cingulata, x = 0.24, y = 0.75, width = 0.13, height = 0.13) +
   draw_text("n = 35", x = 0.77, y = 0.15, size = 12)

#### Otono - Mazama ####
especie1 <- registers_otono %>% filter(Species == "mazama")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Mazama gouazoubira*", 
           subtitle = "En Otoño",  
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) +
   draw_image(hoja, x = 0.71, y = 0.75, width = 0.1, height = 0.1) +
   draw_image(mazama, x = 0.23, y = 0.75, width = 0.12, height = 0.12) +
   draw_text("n = 12", x = 0.77, y = 0.15, size = 12)

#### Otono - Leopardus ####
especie1 <- registers_otono %>% filter(Species == "leopardus")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.4) +
      scale_y_continuous(limits = c(0, ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Leopardus geoffroyi*", 
           subtitle = "En Otoño",  
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(hoja, x = 0.71, y = 0.75, width = 0.1, height = 0.1) +
   draw_image(leopardus, x = 0.24, y = 0.76, width = 0.13, height = 0.13) +
   draw_text("n = 11", x = 0.77, y = 0.15, size = 12)

###################################### INVIERNO ########

registers_invierno <- datacam_invierno 
registers_invierno$Time <- as.character(registers_invierno$Time)
registers_invierno$decimal <- sapply(strsplit(registers_invierno$Time,":"), function(x){
   x <- as.numeric(x)
   x[1]+x[2]/60+ x[3]/3600})

#### Invierno - Axis ####
especie1 <- registers_invierno %>% filter(Species == "axis")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      scale_y_continuous(limits = c(0,ymax_AeH), breaks = seq(0, ymax_AeH, by = 10)) +
      labs(title = "Registros de *Axis axis*", 
           subtitle = "En Invierno",  
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.41, y = 0.7, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(copo, x = 0.72, y = 0.76, width = 0.08, height = 0.08) +
   draw_image(axis, x = 0.23, y = 0.75, width = 0.13, height = 0.13) +
   draw_text("n = 448", x = 0.77, y = 0.15, size = 12)

#### Invierno - Hydrochoerus ####
especie1 <- registers_invierno %>% filter(Species == "hydrochoerus")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_AeH), breaks = seq(0, ymax_AeH, by = 10)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Hydrochoerus hydrochaeris*", 
           subtitle = "En Invierno",  
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(copo, x = 0.72, y = 0.76, width = 0.08, height = 0.08) +
   draw_image(hydrochoerus, x = 0.26, y = 0.75, width = 0.13, height = 0.13) +
   draw_text("n = 396", x = 0.77, y = 0.15, size = 12)

#### Invierno - Canidae ####
especie1 <- registers_invierno %>% filter(Species == "canidae")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Canidae*", 
           subtitle = "En Invierno",  
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(copo, x = 0.72, y = 0.76, width = 0.08, height = 0.08) +
   draw_image(canidae, x = 0.255, y = 0.75, width = 0.13, height = 0.13) +
   draw_text("n = 66", x = 0.77, y = 0.15, size = 12)

#### Invierno - Sus ####
especie1 <- registers_invierno %>% filter(Species == "sus")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Sus scrofa*", 
           subtitle = "En Invierno",  
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(copo, x = 0.72, y = 0.76, width = 0.08, height = 0.08) +
   draw_image(sus, x = 0.255, y = 0.76, width = 0.13, height = 0.13) +
   draw_text("n = 18", x = 0.77, y = 0.15, size = 12)

#### Invierno - Rhea ####

especie1 <- registers_invierno %>% filter(Species == "rhea")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.5) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Rhea americana*", 
           subtitle = "En Invierno",  
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) +
   draw_image(copo, x = 0.72, y = 0.76, width = 0.08, height = 0.08) +
   draw_image(rhea, x = 0.24, y = 0.74, width = 0.13, height = 0.13) +
   draw_text("n = 50", x = 0.77, y = 0.15, size = 12)

#### Invierno - Cingulata ####
especie1 <- registers_invierno %>% filter(Species == "cingulata")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      labs(title = "Registros de *Cingulata*", 
           subtitle = "En Invierno",  
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(copo, x = 0.72, y = 0.76, width = 0.08, height = 0.08) +
   draw_image(cingulata, x = 0.25, y = 0.75, width = 0.13, height = 0.13) +
   draw_text("n = 66", x = 0.77, y = 0.15, size = 12)

#### Invierno - Mazama ####
especie1 <- registers_invierno %>% filter(Species == "mazama")

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Mazama gouazoubira*", 
           subtitle = "En Invierno",  
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(copo, x = 0.72, y = 0.76, width = 0.08, height = 0.08) +
   draw_image(mazama, x = 0.235, y = 0.75, width = 0.12, height = 0.12) +
   draw_text("n = 9", x = 0.77, y = 0.15, size = 12)

#### Invierno - Leopardus ####
especie1 <- registers_invierno %>% filter(Species == "leopardus")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.4) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Leopardus geoffroyi*", 
           subtitle = "En Invierno",  
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) +
   draw_image(copo, x = 0.72, y = 0.76, width = 0.08, height = 0.08) +
   draw_image(leopardus, x = 0.255, y = 0.75, width = 0.13, height = 0.13) +
   draw_text("n = 10", x = 0.77, y = 0.15, size = 12)

###################################### PRIVE ########

registers_prive <- datacam_prive
registers_prive$Time <- as.character(registers_prive$Time)
registers_prive$decimal <- sapply(strsplit(registers_prive$Time,":"), function(x){
   x <- as.numeric(x)
   x[1]+x[2]/60+ x[3]/3600})

#### Prive - Axis ####
especie1 <- registers_prive %>% filter(Species == "axis")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_AeH), breaks = seq(0, ymax_AeH, by = 10)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Axis axis*", 
           subtitle = "En Primavera - Verano",
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) +
   draw_image(flor, x = 0.72, y = 0.76, width = 0.1, height = 0.1) +
   draw_image(axis, x = 0.23, y = 0.74, width = 0.13, height = 0.13) +
   draw_text("n = 489", x = 0.77, y = 0.15, size = 12)

#### Prive - Hydrochoerus ####
especie1 <- registers_prive %>% filter(Species == "hydrochoerus")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_AeH), breaks = seq(0, ymax_AeH, by = 10)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Hydrochoerus hydrochaeris*", 
           subtitle = "En Primavera - Verano",
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) +
   draw_image(flor, x = 0.72, y = 0.76, width = 0.1, height = 0.1) +
   draw_image(hydrochoerus, x = 0.255, y = 0.75, width = 0.13, height = 0.13) +
   draw_text("n = 302", x = 0.77, y = 0.15, size = 12)

#### Prive - Canidae ####
especie1 <- registers_prive %>% filter(Species == "canidae")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Canidae*", 
           subtitle = "En Primavera - Verano",
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) +
   draw_image(flor, x = 0.72, y = 0.76, width = 0.1, height = 0.1) +
   draw_image(canidae, x = 0.24, y = 0.755, width = 0.13, height = 0.13) +
   draw_text("n = 31", x = 0.77, y = 0.15, size = 12)

#### Prive - Sus ####
especie1 <- registers_prive %>% filter(Species == "sus")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Sus scrofa*", 
           subtitle = "En Primavera - Verano",
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(flor, x = 0.72, y = 0.76, width = 0.1, height = 0.1) +
   draw_image(sus, x = 0.255, y = 0.75, width = 0.13, height = 0.13) +
   draw_text("n = 39", x = 0.77, y = 0.15, size = 12)

#### Prive - Rhea ####

especie1 <- registers_prive %>% filter(Species == "rhea")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.5) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Rhea americana*", 
           subtitle = "En Primavera - Verano",
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(flor, x = 0.72, y = 0.76, width = 0.1, height = 0.1) +
   draw_image(rhea, x = 0.24, y = 0.75, width = 0.13, height = 0.13) +
   draw_text("n = 72", x = 0.77, y = 0.15, size = 12)

#### Prive - Cingulata ####
especie1 <- registers_prive %>% filter(Species == "cingulata")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Cingulata*", 
           subtitle = "En Primavera - Verano",
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(flor, x = 0.72, y = 0.76, width = 0.1, height = 0.1) +
   draw_image(cingulata, x = 0.255, y = 0.75, width = 0.13, height = 0.13) +
   draw_text("n = 38", x = 0.77, y = 0.15, size = 12)

#### Prive - Mazama ####
especie1 <- registers_prive %>% filter(Species == "mazama")  

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     linewidth = 0.3) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Mazama gouazoubira*", 
           subtitle = "En Primavera - Verano",
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) + 
   draw_image(flor, x = 0.72, y = 0.76, width = 0.1, height = 0.1) +
   draw_image(mazama, x = 0.25, y = 0.75, width = 0.12, height = 0.12) +
   draw_text("n = 26", x = 0.77, y = 0.15, size = 12)

#### Prive - Leopardus ####
especie1 <- registers_prive %>% filter(Species == "leopardus")

(plot_blanco <- ggplot(especie1, aes(x = decimal)) + 
      geom_histogram(breaks = seq(0, 24),
                     fill = "steelblue4",
                     colour = "black", 
                     size = 0.4) +
      scale_y_continuous(limits = c(0,ymax_resto), breaks = seq(0, ymax_resto, by = 2)) +
      scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24)) +
      labs(title = "Registros de *Leopardus geoffroyi*", 
           subtitle = "En Primavera - Verano",
           y = "Número de registros") +
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
   draw_image(sun, x = 0.5, y = 0.20, width = 0.07, height = 0.06) +
   draw_image(moon, x = 0.49, y = 0.71, width = 0.08, height = 0.07, scale = 0.75) +
   draw_image(flor, x = 0.72, y = 0.76, width = 0.1, height = 0.1) +
   draw_image(leopardus, x = 0.255, y = 0.76, width = 0.13, height = 0.13) +
   draw_text("n = 21", x = 0.77, y = 0.15, size = 12)

