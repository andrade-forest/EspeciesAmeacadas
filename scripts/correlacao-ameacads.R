if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
toras <- read.table("bt10_produtos.csv", h=T,sep=";")

#velha
cor.test(toras$ano , toras$sum, method = "pearson")


str(toras)
velha=ggplot(toras, aes(x = ano,y = sum)) +
  geom_smooth(method = "lm", se=FALSE, na.rm=TRUE, colour="black") +
  theme_test () +
  labs(title = "",
       x = "Anos",
       y = "Numero de esp?cies")
velha


#ssp em evolu??o por ano
toras$ano=as.factor(toras$ano)
toras$ano=as.numeric(toras$ano)
toras= toras %>%
  group_by(ano) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))
##diminui apenas 1 ssp = total 43
