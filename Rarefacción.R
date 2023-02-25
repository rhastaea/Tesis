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
library(iNEXT)
library(ggplot2)

setwd("C:/Users/shalo/OneDrive/Escritorio/Ilán/Tesis/Archivos ahora")
getwd()

data(spider)
str(spider)

data <- read.csv("Rarefacción.csv", sep = ";", header = T)
data <- data[-25,]
data <- data[,-1]
#data <- as.list(data)
# data[27,1] <- as.integer(99)
#data$Sitio <- as.integer(data$Sitio)
str(data)

df_transpose <- data.frame(t(data[-1]))
num <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26,27,99)
colnames(df_transpose) <- num
df_transpose <- as.list(df_transpose)

Rarefaccion <- iNEXT(df_transpose, q=0, datatype = "abundance", 
                     size=NULL, endpoint=NULL, knots=40, 
                     se=F, conf = 0.95, nboot=50)

ggiNEXT(Rarefaccion, type = 1, se = FALSE, 
        facet.var = "None", color.var= "None", grey = FALSE)




