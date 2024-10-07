if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
toras <- read.table("produtos_ano1.csv", h=T,sep=",")

#ssp em evolu??o por ano
toras$ano=as.factor(toras$ano)
toras$volume= toras$sum

toras_ano_ssp= toras %>%
  group_by(produto, exportacao) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))
##diminui apenas 1 ssp = total 43


write.csv(toras_ano_ssp, "toras_ano_ssp-oi.csv", row.names=FALSE, quote=FALSE) 

toras_ano_ssp= toras %>%
  group_by(produto) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))

####com face_wrap
ggplot(toras_ano_ssp, aes(x="", y=sum, fill=produto)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  facet_wrap(~exportacao)+
  theme_void()


##produtos sem exportacao
toras <- read.table("produtos-nao-exportacao.csv", h=T,sep=";")

####grafico
ggplot(toras, aes(x="", y=sum, fill=produto)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()

##produtos exportados
toras <- read.table("exportacao-produtos.csv", h=T,sep=";")

####grafico
ggplot(toras, aes(x="", y=sum, fill=produto)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()


######################GRAFICOS DE EVOLUÇÃO DE PRODUTO

#ssp em evolu??o por ano
toras$ano=as.factor(toras$ano)

toras_ano_ssp= toras %>%
  group_by(produto,ano) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))
##grafico

ggplot(data = toras_ano_ssp, aes(x = ano, y =sum/1000, group = produto)) +
  geom_line(aes(colour = produto), size=0.8)+
  labs(y="Volume (dam³)",x="Ano") 

write.csv(toras_ano_ssp, "toras_ano.csv")


ggplot(data=toras_ano_ssp, aes(x=ano, y=sum/1000, fill=produto)) +
  geom_bar(stat="identity") +
  
  labs(y="Volume em tora (dam³)",x="Ano", colour= "Destino") +
  theme_minimal()
