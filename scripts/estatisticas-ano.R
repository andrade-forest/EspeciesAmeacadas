if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
# dados
toras <- read.table("ssp_ameacadas_anual.csv", h=T,sep=";")

#ssp em evolu??o por ano
toras$ano=as.factor(toras$ano)

toras_ano= toras %>%
  group_by(ano) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))
