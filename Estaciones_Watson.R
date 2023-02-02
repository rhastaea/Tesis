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

##################### WATSON - WILLIAMS ####

wwi_axis <- watson.williams.test(datacam_axis$r.Time ~ datacam_axis$Estacion, data = datacam_axis)
wwi_hydrochoerus <- watson.williams.test(datacam_hydrochoerus$r.Time ~ datacam_hydrochoerus$Estacion, data = datacam_hydrochoerus)
wwi_canidae <- watson.williams.test(datacam_canidae$r.Time ~ datacam_canidae$Estacion, data = datacam_canidae)
wwi_sus <- watson.williams.test(datacam_sus$r.Time ~ datacam_sus$Estacion, data = datacam_sus)
wwi_rhea <- watson.williams.test(datacam_rhea$r.Time ~ datacam_rhea$Estacion, data = datacam_rhea)
wwi_cingulata <- watson.williams.test(datacam_cingulata$r.Time ~ datacam_cingulata$Estacion, data = datacam_cingulata)
wwi_mazama <- watson.williams.test(datacam_mazama$r.Time ~ datacam_mazama$Estacion, data = datacam_mazama)
wwi_leopardus <- watson.williams.test(datacam_leopardus$r.Time ~ datacam_leopardus$Estacion, data = datacam_leopardus)

Taxon <- c("Ciervo axis","Carpincho","Zorros","Jabalí","Ñandú","Mulitas","Corzuela parda","Gato montés")
wwi <- c(wwi_axis[3],wwi_hydrochoerus[3],wwi_canidae[3],wwi_sus[3],wwi_rhea[3],wwi_cingulata[3],wwi_mazama[3],wwi_leopardus[3])
pvalor_wwi <- c(wwi_axis[4],wwi_hydrochoerus[4],wwi_canidae[4],wwi_sus[4],wwi_rhea[4],wwi_cingulata[4],wwi_mazama[4],wwi_leopardus[4])
GL_wwi <- c(wwi_axis[2],wwi_hydrochoerus[2],wwi_canidae[2],wwi_sus[2],wwi_rhea[2],wwi_cingulata[2],wwi_mazama[2],wwi_leopardus[2])

tabla_wwi <- data.frame(unlist(Taxon),unlist(wwi),round(unlist(pvalor_wwi),3),unlist(GL_wwi))
colnames(tabla_wwi) <- c("Taxón", "Watson-Williams", "p-valor","GL")
tabla_wwi[1:8,]

################################### MERDIA - WATSON - WHEELER ####
wwh_axis <- watson.wheeler.test(datacam_axis$r.Time ~ datacam_axis$Estacion, data = datacam_axis)
wwh_hydrochoerus <- watson.wheeler.test(datacam_hydrochoerus$r.Time ~ datacam_hydrochoerus$Estacion, data = datacam_hydrochoerus)
wwh_canidae <- watson.wheeler.test(datacam_canidae$r.Time ~ datacam_canidae$Estacion, data = datacam_canidae)
wwh_sus <- watson.wheeler.test(datacam_sus$r.Time ~ datacam_sus$Estacion, data = datacam_sus)
wwh_rhea <- watson.wheeler.test(datacam_rhea$r.Time ~ datacam_rhea$Estacion, data = datacam_rhea)
wwh_cingulata <- watson.wheeler.test(datacam_cingulata$r.Time ~ datacam_cingulata$Estacion, data = datacam_cingulata)
wwh_mazama <- watson.wheeler.test(datacam_mazama$r.Time ~ datacam_mazama$Estacion, data = datacam_mazama)
wwh_leopardus <- watson.wheeler.test(datacam_leopardus$r.Time ~ datacam_leopardus$Estacion, data = datacam_leopardus)

Taxon <- c("Ciervo axis","Carpincho","Zorros","Jabalí","Ñandú","Mulitas","Corzuela parda","Gato montés")
WWh <- c(WWh_axis[3],WWh_hydrochoerus[3],WWh_canidae[3],WWh_sus[3],WWh_rhea[3],WWh_cingulata[3],WWh_mazama[3],WWh_leopardus[3])
pvalor_WWh <- c(WWh_axis[4],WWh_hydrochoerus[4],WWh_canidae[4],WWh_sus[4],WWh_rhea[4],WWh_cingulata[4],WWh_mazama[4],WWh_leopardus[4])
GL_WWh <- c(WWh_axis[2],WWh_hydrochoerus[2],WWh_canidae[2],WWh_sus[2],WWh_rhea[2],WWh_cingulata[2],WWh_mazama[2],WWh_leopardus[2])

tabla_WWh <- data.frame(unlist(Taxon),unlist(WWh),round(unlist(pvalor),3),unlist(GL))
colnames(tabla_WWh) <- c("Taxón", "Watson-Wheeler", "p-valor","GL")
tabla_WWh
#########################3
