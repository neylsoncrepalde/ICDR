## Introdução às Ciências de Dados com R
## Izabela Hendrix
## Prof. Neylson Crepalde
## Meu nome super bonito
#########################################

# Carregando os pacotes
library(descr) # estatísticas descritivas
library(dplyr) # manipulação de dados
library(ggplot2)  # gráficos
library(readr)   # leitura de bd
library(readxl)  # leitura de excel
#library(haven)  # leitura de bancos de outros softwares

# Lê um excel com o banco de dados IRIS
iris <- read_xlsx("iris.xlsx")

diamonds = read_csv("https://github.com/neylsoncrepalde/ICDR/blob/master/diamonds.csv?raw=true")

dim(iris)
dim(diamonds)
##############################
# Visualização de dados

names(diamonds)
help(diamonds)

# Price, Carat Cut
# Verificando estatísticas descritivas
class(diamonds$price)
summary(diamonds$price)
var(diamonds$price)
sd(diamonds$price)

# Fazendo um histograma dos preços dos diamantes!!
hist(diamonds$price, col = "orange",
     main = "Preço", xlab = "Preço")

# Agora com ggplot2
# ggplot(bdados, aes(x = var1, y = var2,
#                   color = var3)) +
#   geometria() +
#   customizações()
library(ggthemes)
ggplot(diamonds, aes(price)) +
  geom_histogram(color = "white", 
                  fill = "purple") +
  labs(title = "Histograma",
       x = "Preço",
       y = "Frequência",
       subtitle = "Gerado com ggplot2") +
  theme_bw()

# Distribuição do peso CARAT
ggplot(diamonds, aes(carat)) +
  geom_histogram()


# Relação entre peso e preço
ggplot(diamonds, aes(x=carat, 
                     y=price,
                     color = cut)) +
  geom_point(alpha = 0.5)

# Recodificando a variável cut para definir a ordem dos labels
class(diamonds$cut)
diamonds$cut = factor(diamonds$cut,
                      levels = c("Fair",
                                 "Good",
                                 "Very Good",
                                 "Ideal",
                                 "Premium"))

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot(fill = "red")

# Verificando a distribuição da categórica cut
freq(diamonds$cut)

ggplot(diamonds, aes(cut)) +
  geom_bar() +
  coord_flip()

###################
# Filtrando apenas o casos em que o cut for Ideal
diamonds %>% filter(cut == "Ideal") %>% 
  ggplot(aes(carat)) +
  geom_histogram()

# Criando uma nova variável binária "ideal"
diamonds %>% 
  mutate(ideal = if_else(cut == "Ideal", "Sim", "Não")) %>% 
  ggplot(aes(x = ideal, y = price)) +
  geom_boxplot()

# Criando uma variável cortea = cut agregado
diamonds %>% 
  mutate(cortea = case_when(
    cut == "Fair" ~ "Fair",
    cut == "Good" | cut == "Very Good" ~ "Good",
    cut == "Ideal" | cut == "Premium" ~ "Premium"
  )) %>% 
  ggplot(aes(x = cortea, y = carat)) +
  geom_boxplot()

# Calcular a média de preço para cada cat de corte
diamonds %>% group_by(cut) %>% 
  summarise(mpreco = mean(price),
            mcarat = mean(carat)) %>% 
  ggplot(aes(x = mcarat, 
             y = mpreco,
             color = cut)) +
  geom_point(size = 3)

#############################
# Vamos selecionar apenas os diamantes que
# tiverem mais de 2 gramas, apenas as categorias
# de corte Good, Very Good e Premium, e exibir
# num gráfico o peso, o preço, a cor do diamante
# na cor dos pontos e a claridade no formato

diamonds %>% 
  filter(carat > 2) %>% 
  filter(cut == "Good" | cut == "Very Good" |
           cut == "Premium") %>% 
  ggplot(aes(x = carat, y = price,
             color = color, shape = cut)) +
  geom_point(alpha = .7, size = 3) +
  facet_wrap(~clarity)
