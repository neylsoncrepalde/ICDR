# Curso INTRODUÇÃO ÀS CIÊNCIAS DE DADOS COM R
# Izabela Hendrix
# Semana da Extensão Universitário
# Prof. Neylson Crepalde
##############################################

# Instalando pacotes necessários
# install.packages("descr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("nycflights13")
# install.packages("readxl")

# Carregando os pacotes
library(descr) # estatísticas descritivas
library(dplyr) # manipulação de dados
library(ggplot2)  # gráficos
library(readr)   # leitura de bd
library(readxl)  # leitura de excel

# Lê um excel com o banco de dados IRIS
iris <- read_xlsx("iris.xlsx")
dim(iris)     # verifica as dimensões
names(iris)   # verifica os nomes das variáveis
str(iris)     # verifica a estrutura do banco

# Seleção
iris$Sepal.Length # operador $ - retorna o vetor
iris[1]         # operador [] - retorna classe superior
class(iris[1])  # tira a classe da variável - retorna data.frame
iris[[1]]        # operador[[]] - retorna classe da var
class(iris[[1]]) # tira a classe - retorna numeric

# Investigando a variável Sepal.Length
iris$Sepal.Length[1]  # seleciona o primeiro elemento da variável Sepal Length
iris$Sepal.Length[2]  # seleciona o segundo elemento da variável Sepal Length

# Soma o primero e o segundo elementos da variável Sepal.Length
iris$Sepal.Length[1] + iris$Sepal.Length[2] 

########
# Investigando a variável Sepal.Length
# Com variáveis numéricas verificamos algumas estatísticas descritivas:
min(iris$Sepal.Length)    # Mínimo
max(iris$Sepal.Length)    # Máximo

# Medidas de tendência central
mean(iris$Sepal.Length)   # média
median(iris$Sepal.Length) # mediana

# Medidas de dispersão
var(iris$Sepal.Length)  # variância
sd(iris$Sepal.Length)   # desvio padrão

# Conjunto de estatísticas descritivas
summary(iris$Sepal.Length)

# Variáveis categóricas - tirando uma tabela de frequência bacana!
freq(iris$Species)

###################
# Manipulação de dados com dplyr
# usando o %>% (pipe) para indicar operações a serem realizadas com o banco

# Identificar a média de Sepal Length para a especie setosa
iris %>% select(Sepal.Length, Species) %>% 
  filter(Species == "setosa") %>% 
  summarise(media = mean(Sepal.Length))

iris %>% select(Sepal.Length, Species) %>% 
  filter(Species == "versicolor") %>% 
  summarise(media = mean(Sepal.Length))

iris %>% select(Sepal.Length, Species) %>% 
  filter(Species == "virginica") %>% 
  summarise(media = mean(Sepal.Length))

#### Com R base
selecao = iris[c("Sepal.Length", "Species")]
selecao = selecao[selecao$Species == "setosa",]
mean(selecao$Sepal.Length)

############
# Calcular a media de Sepal.Length para 
# as três espécies de flor
# Vamos guardar os resultados num objeto chamado tabela
tabela = iris %>% group_by(Species) %>% 
  summarise(m = mean(Sepal.Length)) %>% 
  mutate(m2 = m^2)

# Verifica os resultados da tabela
tabela

# Verificando arquivos de ajuda
help(mutate)
help(mean)

#########
# Brincar um pouco com o banco de dados Diamond
diamonds = read_csv("https://github.com/neylsoncrepalde/ICDR/blob/master/diamonds.csv?raw=true")
dim(diamonds)
names(diamonds)

freq(diamonds$cut)
class(diamonds$price)
summary(diamonds$price)

diamonds %>%   # começa com o bd 
  group_by(cut) %>%  # agrupo pela var cut
  summarise(med = mean(price),  # crio estat desc
            mediana = median(price),
            desv = sd(price)) %>% 
  arrange(desc(med))  # ordeno pela media


#############
# Um exemplo de implementação de um cálculo com a lógica dos pipes ( %>% )

library(magrittr)
# tentativa 1
# Base
x = 5
exp(sqrt(x^3) + (3 + x/2))

# tentativa 2
# com pipes
x %>% raise_to_power(3) %>% sqrt %>% 
  add(3 + x/2) %>% exp
