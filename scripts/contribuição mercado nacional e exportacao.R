if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
# dados
portos <- read.table("export_3.csv", h=T,sep=",", dec = ".")
#####tirando tora e tirando municipos fora do porto
sem_tora=filter(portos,produto_padronizado!="tora")
portos_semtora=filter(sem_tora,exportado_mun_portos!="0")
######brincando com os dados gerais
toras_ano_ssp= portos_semtora %>%
  group_by(ano) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))

sum(portos$volume)

ssp= portos_semtora %>%
  group_by(nome_cientifico, produto_padronizado) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))
###tora de florestas nativa pra exportação - aquatiquara/mdc
##sai lenha de florestas nativa pra exportação? Resíduos 

###ano como fator
portos_semtora$ano=as.factor(portos_semtora$ano)
portos_semtora$volume=as.numeric(portos_semtora$volume)

###ggrafico de barras com separação por UF de origem
ggplot(data = portos_semtora, aes(x =ano, y = volume, group = produto_padronizado, fill=produto_padronizado)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Set2")+
  facet_wrap(~uf_origem) +
  labs(y="Volume em tora (m³)",x="Ano", colour= "produto_padronizado") +
  theme(legend.position = "bottom")


#agregando
toras_agregado= portos_semtora %>%
  group_by(ano, uf_origem) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))

###ggrafico de linhas com separação por UF de origem
ggplot(data = toras_agregado, aes(x = ano, y =sum, group = uf_origem)) +
  geom_line(aes(colour = uf_origem), size=0.9)+
  labs(y="Volume (m³)",x="Ano")


