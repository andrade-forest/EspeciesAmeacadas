if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
# dados
toras <- read.table("ameacadas-estado.csv", h=T,sep=";")

#ssp em evolu??o por ano
toras$Ano=as.factor(toras$Ano)
#m?dia geral ao longo dos anos

toras_estado= toras %>%
  group_by(Estado) %>%
  summarise(
    mean = mean(Volume, na.rm = TRUE),
    sum=sum(Volume,na.rm = TRUE),
    sd = sd(Volume, na.rm = TRUE),
    var= var(Volume, na.rm=TRUE))

write.csv(toras_estado, "volume-estado.csv", row.names=FALSE, quote=FALSE) 

library(tidyr)

toras$estado=toras$uf_origem
toras$Ano=as.factor(toras$Ano)

ggplot(data = toras, aes(x = Ano, y = Volume/1000, group = Estado)) +
  geom_line(aes(colour = Estado), size=0.8)+
  labs(y="Volume (dam)",x="Ano") +
  scale_colour_brewer(palette = "Dark2")

toras <- toras[with(toras,order(-Volume)),]

toras$Estado <- ordered(toras$Estado, levels = c("MT",
                                                                   "PA",
                                                                   "RO",
                                                                   "AC",
                                                                   "AM",
                                                                   "AP",
                                                                   "RN"))
