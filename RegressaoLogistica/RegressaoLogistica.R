# Regressao LogÃ�stica

# Para este exemplo, usaremos o dataset Titanic do Kaggle. Este dataset eh famoso e usamos parte dele nas aulas de SQL.
# Ele normalmente eh usado por aqueles que estao comecando em Machine Learning.
# Vamos prever uma classificacao - sobreviventes e nÃ£o sobreviventes

# https://www.kaggle.com/c/titanic/data


# Comecamos carregando o dataset de dados_treino
dados_treino <- read.csv('titanic-train.csv')
head(dados_treino)

# Analise exploratoria de dados
# Vamos usar o pacote Amelia e suas funcoes para definir o volume de dados Missing
# Clique no zoom para visualizar o grafico
# Cerca de 20% dos dados sobre idade estao Missing (faltando)
install.packages("Amelia")
library(Amelia)
missmap(dados_treino, main = "Titanic Training Data - Mapa de Dados Missing", 
        col = c("yellow", "black"), legend = FALSE)

# Visualizando os dados
library(ggplot2)
ggplot(dados_treino,aes(Survived)) + geom_bar()
ggplot(dados_treino,aes(Pclass)) + geom_bar(aes(fill = factor(Pclass)), alpha = 0.5)
ggplot(dados_treino,aes(Sex)) + geom_bar(aes(fill = factor(Sex)), alpha = 0.5)
ggplot(dados_treino,aes(Age)) + geom_histogram(fill = 'blue', bins = 20, alpha = 0.5)
ggplot(dados_treino,aes(SibSp)) + geom_bar(fill = 'red', alpha = 0.5)
ggplot(dados_treino,aes(Fare)) + geom_histogram(fill = 'green', color = 'black', alpha = 0.5)

# Limpando os dados
# Para tratar os dados missing, usaremos o recurso de imputation, ou seja, vamos preencher os dados missing,
# com o valor da media dos dados
pl <- ggplot(dados_treino, aes(Pclass,Age)) + geom_boxplot(aes(group = Pclass, fill = factor(Pclass), alpha = 0.4)) 
pl + scale_y_continuous(breaks = seq(min(0), max(80), by = 2))

# Vimos que os passageiros mais ricos, nas classes mais altas, tendem a ser mais velhos. 
# Usaremos esta media para imputar as idades Missing
impute_age <- function(age,class){
    out <- age
    for (i in 1:length(age)){
        
        if (is.na(age[i])){
            
            if (class[i] == 1){
                out[i] <- 37
                
            }else if (class[i] == 2){
                out[i] <- 29
                
            }else{
                out[i] <- 24
            }
        }else{
            out[i]<-age[i]
        }
    }
    return(out)
}

fixed.ages <- impute_age(dados_treino$Age,dados_treino$Pclass)
dados_treino$Age <- fixed.ages

# Visualizando o mapa de valores missing (nao existem mais dados missing)
missmap(dados_treino, main = "Titanic Training Data - Mapa de Dados Missing", 
        col = c("yellow", "black"), legend = FALSE)

# Construindo o Modelo

# Primeiro, uma limpeza nos dados
str(dados_treino)
head(dados_treino, 3)
library(dplyr)
dados_treino <- select(dados_treino, -PassengerId, -Name, -Ticket, -Cabin)
head(dados_treino, 3)
str(dados_treino)

# Treinando o modelo
log.model <- glm(formula = Survived ~ . , family = binomial(link = 'logit'), data = dados_treino)

# Podemos ver que as variaveis Sex, Age e Pclass sao as variaveis mais significantes
summary(log.model)

# Fazendo as previsoes nos dados de teste
library(caTools)
set.seed(101)

# Split dos dados
split = sample.split(dados_treino$Survived, SplitRatio = 0.70)

# Datasets de treino e de teste
dados_treino_final = subset(dados_treino, split == TRUE)
dados_teste_final = subset(dados_treino, split == FALSE)

# Gerando o modelo com a versao final do dataset
final.log.model <- glm(formula = Survived ~ . , family = binomial(link='logit'), data = dados_treino_final)

# Resumo
summary(final.log.model)

# Prevendo a acuracia
fitted.probabilities <- predict(final.log.model, newdata = dados_teste_final, type = 'response')

# Calculando os valores
fitted.results <- ifelse(fitted.probabilities > 0.5, 1, 0)

# Conseguimos quase 80% de acuracia
misClasificError <- mean(fitted.results != dados_teste_final$Survived)
print(paste('Acuracia', 1-misClasificError))

# Criando a confusion matrix
table(dados_teste_final$Survived, fitted.probabilities > 0.5)









