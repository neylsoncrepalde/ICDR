########################################
# Introdução às Ciências de Dados com R
# Izabela Hendrix
# Prof. Neylson
# Meu nome super bonito
########################################

library(readr)
library(ggplot2)
library(descr)
library(readxl)
library(dplyr)

iris <- read_xlsx("iris.xlsx")
diamonds = read_csv("https://github.com/neylsoncrepalde/ICDR/blob/master/diamonds.csv?raw=true")

# Missão1 = iris, calcular estatísticas descritivas
# da variável Petal.Width.

iris %>% summarise(m = mean(Petal.Width),
                   med = median(Petal.Width),
                   variancia = var(Petal.Width),
                   desv = sd(Petal.Width))

# Agora, vamos calcular a media da mesma var para cada
# espécie

iris %>% 
  group_by(Species) %>% 
  summarise(m = mean(Petal.Width),
                   med = median(Petal.Width),
                   variancia = var(Petal.Width),
                   desv = sd(Petal.Width))

## Missão 2 = vamos criar uma nova variável a partir do tamanho
# da pétala. Se o tamanho for maior ou igual à média, a flor
# é grande. Se for menor, a flor é pequena.

iris %>% 
  mutate(tamanho = if_else(Petal.Length >= mean(Petal.Length), 
                           "Grande","Pequeno")) %>% 
  group_by(tamanho, Species) %>% 
  summarise(m = mean(Petal.Width),
            med = median(Petal.Width),
            variancia = var(Petal.Width),
            desv = sd(Petal.Width))

## Missão 3 = verificar a média e o desvio padrão do preço
# para as categorias de corte e claridade dos diamantes

diamonds %>% group_by(clarity, cut) %>% 
  summarise(m = mean(price),
            desv = sd(price))

#######################################
# Trabalhando com a PNAD 2012
#install.packages("haven")
library(haven)
pnad = read_sav("https://github.com/neylsoncrepalde/introducao_ao_r/blob/master/dados/pes_2012.sav?raw=true")
dim(pnad)

#pnad = read_csv("https://github.com/neylsoncrepalde/introducao_ao_r/blob/master/dados/pes_2012.csv?raw=true")
names(pnad)
head(pnad)

summary(pnad$V4720)

pnad$renda = pnad$V4720
pnad$renda[pnad$renda == 999999999999] = NA

summary(pnad$renda)

# M4 = verificar a diferença das médias de renda
# entre homens e mulheres
pnad %>% group_by(V0302) %>% 
  summarise(m = mean(renda, na.rm = T))

# Agora diferença de renda por cor
pnad %>% group_by(V0404) %>% 
  summarise(m = mean(renda, na.rm = T))

# Colocando os labels nas variáveis sexo e cor
pnad = pnad %>% 
  mutate(sexo = case_when(
    V0302 == 2 ~ "Masculino",
    V0302 == 4 ~ "Feminino"
  ), cor = case_when(
    V0404 == 0 ~ "Indígena",
    V0404 == 2 ~ "Branca",
    V0404 == 4 ~ "Preta",
    V0404 == 6 ~ "Amarela",
    V0404 == 8 ~ "Parda"
  ))
names(pnad)

freq(pnad$sexo)
freq(pnad$cor)
# Tira a notação científica
options(scipen = 999)
freq(pnad$cor)

# Calcular a media de renda por cor
pnad %>% group_by(cor, sexo) %>% 
  summarise(m = mean(renda, na.rm = T)) %>% 
  ggplot(aes(x = cor, y = m)) +
  geom_point(size = 4, alpha = .6) +
  facet_wrap(~sexo)

# Colocando o sexo em color
pnad %>% group_by(cor, sexo) %>% 
  summarise(m = mean(renda, na.rm = T)) %>% 
  ggplot(aes(x = cor, y = m, color = sexo)) +
  geom_point(size = 4, alpha = .6)

pnad$V0302 = as.character(pnad$V0302)
pnad$V0404 = as.character(pnad$V0404)
pnad %>% group_by(V0404, V0302) %>% 
  summarise(m = mean(renda, na.rm = T)) %>% 
  ggplot(aes(x = V0404, y = m, color = V0302)) +
  geom_point(size = 4, alpha = .6)

#####################################
# Loops

# Eu poderia executar um comando 10 vezes...
x = 1
print(x)
x = 2
print(x)
x = 3
print(x)
# Já cansei. Vamos implementar com um loop
for (i in 1:10) {
  print(i)
}

# Agora a mesma coisa, 10 mil vezes...
for (i in 1:10000) {
  print(i)
}

# Fazendo uma pequena operação matemática repetidas vezes com loop for
for (i in 1:1000) {
  res = sqrt(i + 2)
  print(res)
}
