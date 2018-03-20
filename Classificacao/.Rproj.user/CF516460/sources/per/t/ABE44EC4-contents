# Regressão Linear 

# ***** Esta é a versão 2.0 deste script, atualizado em 23/05/2017 *****
# ***** Esse script pode ser executado nas versões 3.3.1, 3.3.2, 3.3.3 e 3.4.0 da linguagem R *****
# ***** Recomendamos a utilização da versão 3.4.0 da linguagem R *****

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
setwd("~/Dropbox/DSA/BigDataAnalytics-R-Azure/Cap07")
getwd()

# Regressão Linear
# https://archive.ics.uci.edu/ml/datasets/Student+Performance
# Dataset com dados de estudantes
# Vamos prever a nota final (grade) dos alunos

# Carregando o dataset
df <- read.csv2('http://datascienceacademy.com.br/blog/aluno/RFundamentos/Datasets/ML/estudantes.csv')

# Explorando os dados
head(df)
summary(df)
str(df)
any(is.na(df))

# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("dplyr")
library(ggplot2)
library(ggthemes)
library(dplyr)

# Obtendo apenas as colunas numéricas
colunas_numericas <- sapply(df, is.numeric)
colunas_numericas

# Filtrando as colunas numéricas para correlação
data_cor <- cor(df[,colunas_numericas])
data_cor
head(data_cor)

# Pacotes para visualizar a análise de correlação
# install.packages('corrgram')
# install.packages('corrplot')
library(corrplot)
library(corrgram)

# Criando um corrplot
corrplot(data_cor, method = 'color')

# Criando um corrgram
corrgram(df)
corrgram(df, order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt)

# Criando um histograma
ggplot(df, aes(x = G3)) + 
  geom_histogram(bins = 20, 
                 alpha = 0.5, fill = 'blue') + 
  theme_minimal()


# Treinando e Interpretando o Modelo
# Import Library
# install.packages("caTools")
library(caTools)

# Criando as amostras de forma randômica
set.seed(101) 
?sample.split
amostra <- sample.split(df$age, SplitRatio = 0.70)

# ***** Treinamos nosso modelo nos dados de treino *****
# *****   Fazemos as predições nos dados de teste  *****

# Criando dados de treino - 70% dos dados
treino = subset(df, amostra == TRUE)

# Criando dados de teste - 30% dos dados
teste = subset(df, amostra == FALSE)

# Gerando o Modelo (Usando todos os atributos)
modelo_v1 <- lm(G3 ~ ., treino)
modelo_v2 <- lm(G3 ~ G2 + G1, treino)
modelo_v3 <- lm(G3 ~ absences, treino)
modelo_v4 <- lm(G3 ~ Medu, treino)

# Interpretando o Modelo
summary(modelo_v1)
summary(modelo_v2)
summary(modelo_v3)
summary(modelo_v4)

# par(mfrow = c(2,2))
# plot(modelo_v3)

# ****************************************************
# *** Estas informações abaixo é que farão de você ***
# *** um verdadeiro conhecedor de Machine Learning ***
# ****************************************************

# Resíduos
# Diferença entre os valores observados de uma variável e seus valores previstos
# Seus resíduos devem se parecer com uma distribuição normal, o que indica
# que a média entre os valores previstos e os valores observados é próximo de 0 (o que é bom)

# Coeficiente - Intercept - a (alfa)
# Valor de a na equação de regressão

# Coeficiente - G2 - b (beta)
# Neste caso, o valor de G2 representa o valor de b na equação de regressão

# Erro Padrão
# Medida de variabilidade na estimativa do coeficiente a (alfa). O ideal é que este valor 
# seja menor que o valor do coeficiente, mas nem sempre isso irá ocorrer.

# Asteriscos 
# Os asteriscos representam os níveis de significância de acordo com o p-value.
# Quanto mais estrelas, maior a siginificância.
# Atenção --> Muitos astericos indicam que é improvável que não exista relacionamento entre as variáveis.

# Valor t
# Define se coeficiente da variável é significativo ou não para o modelo. 
# Ele é usado para calcular o p-value e os níveis de significância.

# p-value
# O p-value representa a probabilidade que a variável não seja relevante. 
# Deve ser o menor valor possível. Se este valor for realmente pequeno, o R irá mostrar o valor como notação científica

# Significância
# São aquelas legendas próximas as suas variáveis
# Espaço em branco - ruim
# Pontos - razoável
# Asteriscos - bom
# Muitos asteriscos - muito bom

# Residual Standar Error
# Este valor representa o desvio padrão dos resíduos

# Degrees of Freedom
# É a diferença entre o número de observações na amostra de treinamento e o número de variáveis no seu modelo

# R-squared
# Ajuda a avaliar o nível de precisão do nosso modelo. Quanto maior, melhor, sendo 1 o valor ideal.

# F-statistics
# É o teste F do modelo. Esse teste obtém os parâmetros do nosso modelo e compara com um modelo que tenha menos parâmetros
# Em teoria, um modelo com mais parâmetros tem um desempenho melhor. Se o seu modelo com mais parâmetros NÃO tiver perfomance
# melhor que um modelo com menos parâmetros, o valor do p-value será bem alto. Se o modelo com mais parâmetros tem performance
# meljor que um modelo com menos parâmetros, o valor do p-value será mais baixo.

# Lembre que correlação não implica causalidade


# Visualizando o Modelo e Fazendo Previsões

# Obtendo os resíduos
res <- residuals(modelo_v1)

# Convertendo o objeto para um dataframe
res <- as.data.frame(res)
head(res)

# Histograma dos resíduos
ggplot(res, aes(res)) +  
  geom_histogram(fill = 'blue', 
                 alpha = 0.5, 
                 binwidth = 1)

# Plot do Modelo
# plot(modelo_v1)

# Fazendo as predições
modelo_v1 <- lm(G3 ~ ., treino)
prevendo_G3 <- predict(modelo_v1, teste)

prevendo_G3

# Visualizando os valores previstos e observados
resultados <- cbind(prevendo_G3, teste$G3) 
colnames(resultados) <- c('Previsto','Real')
resultados <- as.data.frame(resultados)
resultados
min(resultados)

# Tratando os valores negativos
trata_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}

# Aplicando a função para tratar valores negativos em nossa previsão
resultados$Previsto <- sapply(resultados$Previsto, trata_zero)
resultados$Previsto

# Calculando o erro médio
# Quão distantes seus valores previstos estão dos valores observados
# MSE
mse <- mean((resultados$Real - resultados$Previsto)^2)
print(mse)

# RMSE
rmse <- mse^0.5
rmse

# Calculando R Squared
SSE = sum((resultados$Previsto - resultados$Real)^2)
SST = sum( (mean(df$G3) - resultados$Real)^2)

# R-Squared
# Ajuda a avaliar o nível de precisão do nosso modelo. Quanto maior, melhor, sendo 1 o valor ideal.
R2 = 1 - (SSE/SST)
R2






