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
library(circular)

setwd("C:/Users/shalo/OneDrive/Escritorio/Ilán/Tesis/Archivos ahora")
datastation <- read.csv("datastation_final.csv", header = TRUE, sep = ",")
datacam <- read.csv("datacam_solapamiento.csv", header = T, sep = ";", row.names = 1)
datacam$DateTimeOriginal <- as.factor(datacam$DateTimeOriginal)

datacam$r.Time <- gsub(",",".",datacam$r.Time) #Para reemplazar las comas por puntos
datacam$r.Time <- as.numeric(datacam$r.Time) #Para pasarlo a formato numérico
datacam$r.Time <- datacam$r.Time * 2 * pi #para pasarlo a radianes

datacam_axis <- datacam %>% filter(Species == "axis")
datacam_hydrochoerus <- datacam %>% filter(Species == "hydrochoerus")
datacam_canidae <- datacam %>% filter(Species == "canidae")
datacam_sus <- datacam %>% filter(Species == "sus")
datacam_rhea <- datacam %>% filter(Species == "rhea")
datacam_cingulata <- datacam %>% filter(Species == "cingulata")
datacam_mazama <- datacam %>% filter(Species == "mazama")
datacam_leopardus <- datacam %>% filter(Species == "leopardus")

ww_axis <- watson.wheeler.test(datacam_axis$r.Time ~ datacam_axis$Estacion, data = datacam_axis)
ww_hydrochoerus <- watson.wheeler.test(datacam_hydrochoerus$r.Time ~ datacam_hydrochoerus$Estacion, data = datacam_hydrochoerus)
ww_canidae <- watson.wheeler.test(datacam_canidae$r.Time ~ datacam_canidae$Estacion, data = datacam_canidae)
ww_sus <- watson.wheeler.test(datacam_sus$r.Time ~ datacam_sus$Estacion, data = datacam_sus)
ww_rhea <- watson.wheeler.test(datacam_rhea$r.Time ~ datacam_rhea$Estacion, data = datacam_rhea)
ww_cingulata <- watson.wheeler.test(datacam_cingulata$r.Time ~ datacam_cingulata$Estacion, data = datacam_cingulata)
ww_mazama <- watson.wheeler.test(datacam_mazama$r.Time ~ datacam_mazama$Estacion, data = datacam_mazama)
ww_leopardus <- watson.wheeler.test(datacam_leopardus$r.Time ~ datacam_leopardus$Estacion, data = datacam_leopardus)

Taxon <- c("Ciervo axis","Carpincho","Zorros","Jabalí","Ñandú","Mulitas","Corzuela parda","Gato montés")
WW <- c(ww_axis[3],ww_hydrochoerus[3],ww_canidae[3],ww_sus[3],ww_rhea[3],ww_cingulata[3],ww_mazama[3],ww_leopardus[3])
pvalor <- c(ww_axis[4],ww_hydrochoerus[4],ww_canidae[4],ww_sus[4],ww_rhea[4],ww_cingulata[4],ww_mazama[4],ww_leopardus[4])

tabla <- data.frame(unlist(Taxon),unlist(WW),round(unlist(pvalor),5))
colnames(tabla) <- c("Taxón", "Watson-Wheeler", "p-valor")
tabla
