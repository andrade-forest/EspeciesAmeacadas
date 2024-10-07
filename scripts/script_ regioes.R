if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
produtos <- read.table("produtos_reg.csv", h=T,sep=",")

#produto e destino final
str(produtos)

ggplot(produtos, aes(x="produtos", y=sum, fill=Regioes) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)
  
  
#com tora

ggplot(produtos, aes(x="Regioes", y=Volume, fill=Regioes)) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) +
  
  theme_void() # remove background, grid, numeric labels

###Sem tora

new_product = slice(produtos, -(174:191))

ggplot(new_product, aes(x="Regioes", y=Volume, fill=Regioes)) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) +
  theme_void() 
# remove background, grid, numeric labels
