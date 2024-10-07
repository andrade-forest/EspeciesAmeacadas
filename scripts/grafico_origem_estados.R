if (!require("pacman")) install.packages("pacman")
library(ggplot2)
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
# dados
toras <- read.table("ameacadas_uf_2010_nivel.csv", h=T,sep=",")

######Agrupando estados

toras= toras %>%
  group_by(ano, uf) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))

str(toras)
toras$uf <- ordered(toras$uf, levels = c("MT","PA","RO","AM","AC","AP","RO")
                                                    
toras$ano=as.factor(toras$ano)
ggplot(data = toras, aes(x = ano, y =sum, group = uf)) +
  geom_line(aes(colour = uf), size=0.9)+
  scale_y_continuous(limits = c(0, 500000), breaks = seq(0, 500000, 50000)) +
  labs(y="Volume (m³)",x="Ano") + scale_colour_manual("Origem",
                                                      values = c("darkgreen","darkorange","green","coral","red","darkred","darkblue"),
                                                        labels = c( "Acre","Amazonas", "Amapá", "Mato Grosso", "Pará","Rondônia","Roraima"))

