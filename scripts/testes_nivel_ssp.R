if (!require("pacman")) install.packages("pacman")
library(ggplot2)
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
# dados
toras <- read.table("ameacadas_uf_2010_nivel.csv", h=T,sep=",")

######Agrupando estados

toras= toras %>%
  group_by(ano, nome_cientifico, nivel) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))

str(toras)

toras$ano=as.factor(toras$ano)
toras$nivel=as.factor(toras$nivel)

ggplot(data = toras, aes(x = ano, y =sum, group = nome_cientifico)) +
  geom_line(aes(colour = nome_cientifico), size=0.9)+
  facet_wrap(~nivel) +
  scale_y_continuous(limits = c(0, 300000), breaks = seq(0, 300000, 50000))+
  labs(y="Volume (m³)",x="Ano")
