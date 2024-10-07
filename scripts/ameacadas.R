if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
# dados
toras <- read.table("ameacadas_uf_2010.csv", h=T,sep=",")

#arrumando o data frame
options(na.action = "na.fail")
toras$volume=as.numeric(toras$volume)

toras= toras%>%
  group_by(nome_cientifico,ano) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))

#vendo por ano

toras= toras%>%
  group_by(ano) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))

#vendo as estatisticas basicas


library(tidyr)

toras$estado=toras$uf
toras$ano=as.factor(toras$ano)
toras$estado=as.factor(toras$estado)
str(toras)

ggplot(data = toras, aes(x = ano, y = volume/1000, group = estado)) +
  geom_line(aes(colour = estado), size=0.8)+
  labs(y="Volume (dam)",x="Ano") +
  scale_colour_brewer(palette = "Dark2")

toras <- toras[with(toras,order(-Volume)),]

