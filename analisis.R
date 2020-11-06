rm(list=ls())
setwd("~/Documents/OnCampusJob/analisis-est-covid-19")
library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(stringr)
library(devtools)
#install.packages("paletteer")
#devtools::install_github("EmilHvitfeldt/paletteer")


principal <- read.csv("csv/Principal.csv", sep=",", stringsAsFactors = FALSE)
registros <- read.csv("csv/Registros.csv", sep=",", stringsAsFactors = FALSE)
teleconsultas <- read.csv("csv/Teleconsultas.csv", sep=",", stringsAsFactors = FALSE)


colnames(principal) <- c("Estatus", "Casos")

ggplot(data=principal, aes(x=Estatus, y=Casos, fill=Estatus)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() + 
  geom_text(aes(label = Casos), hjust = -0.1, size=5) +
  ggtitle("Números de casos por estatus Covid-19") +
  xlab("Estatus") + ylab("Número de casos") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none") +
  scale_fill_brewer()

edad <- registros[!is.na(registros$Edad) & registros$Edad != 0,]$Edad
edades <- c(19,29,39,49,59,69)
rango_edades <- data.frame(
  Rango <- c("0-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70 o más"),
  Casos  <- c(0,0,0,0,0,0,0)
)
colnames(rango_edades) <- c("Rango", "Casos")
for(i in 1:length(edad)){
  count <- 1
  while(edad[i] > edades[count] & count < 7){
    count <- count + 1
  }
  rango_edades$Casos[count] <- rango_edades$Casos[count] + 1
}

ggplot(data=rango_edades, aes(x=Rango, y=Casos, fill=Rango)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() + 
  geom_text(aes(label = Casos), hjust = -0.1, size=5) +
  ggtitle("Números de casos por rango de edad Covid-19") +
  xlab("Rango de edad") + ylab("Número de casos") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none") +
  scale_fill_brewer()

estatus <- data.frame(table(registros$Estatus))
colnames(estatus) <- c("Estatus", "Casos")
ggplot(data=principal, aes(x=Estatus, y=Casos, fill=Estatus)) +
  geom_bar(stat="identity", width=0.7, color="white") + coord_flip() + 
  geom_text(aes(label = Casos), hjust = -0.1, size=5) +
  ggtitle("Números de casos por estatus Covid-19") +
  xlab("Estatus") + ylab("Número de casos") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none") +
  scale_fill_brewer()

