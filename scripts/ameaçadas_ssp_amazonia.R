if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
toras <- read.table("dados.csv", h=T,sep=";")

#ssp em evolu??o por ano
toras$ano=as.factor(toras$ano)

toras_ano_ssp= toras %>%
  group_by(Especie) %>%
  summarise(
    mean = mean(Total, na.rm = TRUE),
    sum=sum(Total,na.rm = TRUE),
    sd = sd(Total, na.rm = TRUE),
    var= var(Total, na.rm=TRUE))
##diminui apenas 1 ssp = total 43


#ssp em evolu??o por ano
toras$ano=as.factor(toras$ano)

toras_ssp_total= toras %>%
  group_by(Especie) %>%
  summarise(
    mean = mean(Volume, na.rm = TRUE),
    sum=sum(Volume,na.rm = TRUE),
    sd = sd(Volume, na.rm = TRUE),
    var= var(Volume, na.rm=TRUE))
##diminui apenas 1 ssp = total 43

write.csv(toras_ssp_total, "ssp-total-2.csv", row.names=FALSE, quote=FALSE) 

