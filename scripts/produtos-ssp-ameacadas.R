#####1. Abrindo a base

if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
toras <- read.table("produtos.csv", h=T,sep=",")

library(reprex)

#####2. Ajustando as variáveis
toras$ano=as.factor(toras$ano)

#####3. Dinamica anual dos produtos
toras$ano=as.factor(toras$ano)
toras_ano_ssp= toras %>%
  group_by(produtos) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))

######################GRAFICOS###################

## 1. grafico linhas

ggplot(data = toras_ano_ssp, aes(x = ano, y =sum/1000, group = produtos)) +
  geom_line(aes(colour = produtos), size=0.8)+
  labs(y="Volume (dam³)",x="Ano") 

#####2. Geobar
ggplot(data=toras_ano_ssp, aes(x=ano, y=sum/1000, fill=produtos)) +
  geom_bar(stat="identity") +
  labs(y="Volume (dam³)",x="Ano", colour= "Produto") +
  theme_minimal()


  ##################################Exportação
  
  toras <- read.table("produtos.csv", h=T,sep=",")
  
  #####1. Ajustando as variáveis
  toras$ano=as.factor(toras$ano)
  
  #####2. Agrupando mercados
  
  toras_ano_ssp= toras %>%
    group_by(exportacao, ano) %>%
    summarise(
      mean = mean(volume, na.rm = TRUE),
      sum=sum(volume,na.rm = TRUE),
      sd = sd(volume, na.rm = TRUE),
      var= var(volume, na.rm=TRUE))
  
  ####3. geobar exportação
  ggplot(data=toras_ano_ssp, aes(x=ano, y=sum, fill=exportacao)) +
    geom_bar(stat="identity") +
    labs(y="Volume",x="Ano", colour= "exportacao") +
  theme_bw()
  
  ggplot(data = toras_ano_ssp, aes(x = ano, y =sum, group = exportacao)) +
    geom_line(aes(colour = exportacao), size=0.9)+
    scale_y_continuous(limits = c(0, 1500000), breaks = seq(0, 1500000, 100000))+
      labs(y="Volume (m³)",x="Ano", colour = "Exportação") 
  
  ####Exportação sem tora
  
  toras <- read.table("produtos_semtora2.csv", h=T,sep=",")
  ####1. Ajustando as variáveis
  toras$ano=as.factor(toras$ano)
  toras$Mercados=as.factor(toras$Mercados)
  #####2. Agrupando mercados
  toras_ano_ssp= toras %>%
    group_by(Mercados, ano) %>%
    summarise(
      mean = mean(volume, na.rm = TRUE),
      sum=sum(volume,na.rm = TRUE),
      sd = sd(volume, na.rm = TRUE),
      var= var(volume, na.rm=TRUE))
  #######3. geomline
  
  toras_ano_ssp$Mercados <- ordered(toras_ano_ssp$Mercados, levels = c("nao",
                                                                     "sim"))
    ggplot(data = toras_ano_ssp, aes(x = ano, y =sum, group = Mercados)) +
    geom_line(aes(colour = Mercados), size=0.9)+
    scale_y_continuous(limits = c(0, 600000), breaks = seq(0, 600000, 50000))+
    labs(y="Volume (m³)",x="Ano") + scale_colour_manual("Mercados",
                                                          values = c("darkgreen", "darkorange"),
                                                          labels = c( "Nacional","Internacional"))

  ####4. geobar exportação
  ggplot(data = toras_ano_ssp, aes(x = ano, y =sum, fill = Mercados)) +
    geom_bar(stat="identity") +
    scale_y_continuous(limits = c(0, 600000), breaks = seq(0, 600000, 50000))+
    labs(y="Volume (m³)",x="Ano") + scale_fill_manual("Mercados",
                                                        values = c("darkgreen", "darkorange"),
                                                        labels = c( "Nacional","Internacional"))
  
  #####DADOS POR ESTADO
  
  produtos_estado= toras %>%
    group_by(uf_destino, Mercados) %>%
    summarise(
      mean = mean(volume, na.rm = TRUE),
      sum=sum(volume,na.rm = TRUE),
      sd = sd(volume, na.rm = TRUE),
      var= var(volume, na.rm=TRUE))
  
  