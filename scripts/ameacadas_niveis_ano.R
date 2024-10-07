if (!require("pacman")) install.packages("pacman")
library(ggplot2)



pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
# dados
toras <- read.table("ameacadas_uf_2010_nivel.csv", h=T,sep=",")

#ajustando dados
toras$ano=as.factor(toras$ano)
toras <- toras[with(toras,order(-volume)),]
toras$nome_cientifico <- as.factor(toras$nome_cientifico)



library(RColorBrewer)


ggplot(data = toras, aes(x =ano, y = volume/1000, group = nome_cientifico)) +
  geom_line(aes(colour = nome_cientifico) , size=0.5)+
  facet_wrap(~nivel) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
  labs(y="Volume explorado (dam³)",x="Ano", colour= "Espécie")

str(toras)
levels(toras$nome_cientifico)



ggplot(data = toras, aes(x = ano, y =volume/1000, group = nivel)) +
  geom_line(aes(colour = nivel), size=0.8)+
  labs(y="Volume (dam³)",x="Ano") 
str(toras)

toras= toras %>%
  group_by(nivel, ano) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))


ggplot(data=toras, aes(x=ano, y=sum, fill=nivel)) +
  geom_bar(stat="identity") +
  labs(x = "Year", y = "Volume (m³)", fill = "Category") +
  scale_y_continuous(limits = c(0, 850000), breaks = seq(0, 850000, 50000))+
  scale_fill_manual("Nível",
                     values = c("brown", "darkorange", "darkorange4"),
                     labels = c("Critically (CR)", "Endangered (EN)", "Vulnerable (VU)"))

  
  