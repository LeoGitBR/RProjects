# Este codigo contem comandos para filtrar e plotar os dados de aluguel de bikes, dados que estao em nosso dataset
# Este codigo foi criado para executar tanto no Azure, quanto no RStudio.
# Para executar no Azure, altere o valor da variavel Azure para TRUE. Se o valor for FALSE, o codigo sera executado no RStudio

# ***** Esta é a versão 2.0 deste script, atualizado em 23/05/2017 *****
# ***** Esse script pode ser executado nas versões 3.3.1, 3.3.2, 3.3.3 e 3.4.0 da linguagem R *****
# ***** Recomendamos a utilização da versão 3.4.0 da linguagem R *****

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# setwd("~/Dropbox/DSA/BigDataAnalytics-R-Azure/Cap10")
# getwd()

# Variavel que controla a execucao do script
Azure <- FALSE

if(Azure){
  source("src/Tools.R")
  bikes <- maml.mapInputPort(1)
  bikes$dteday <- set.asPOSIXct(bikes)
}else{
  bikes <- bikes
}

str(bikes)


# Verificando qts bikes sao alugadas por hora
bikesPerHour <- stats::aggregate([cnt, workingday] ~ hr, bikes, mean)
barplot(bikesPerHour$cnt, names = bikesPerHour$hr)



# Verificando a correlacao entre as variaveis preditoras
linearM         <- lm(cnt ~ dteday, data = bikes) # summary(linearM)    
prediction      <- predict(linearM, newdata = bikes) # summary(prediction)    head(prediction)
countAndPredict = data.frame(BikesCount = bikes$cnt, Prediction = prediction) # head(countAndPredict)
                    


bikes$count <- bikes$cnt - prediction # head(bikes$cnt)

cols <- c("mnth", "hr", "holiday", "workingday",
          "weathersit", "temp", "hum", "windspeed",
          "isWorking", "monthCount", "dayWeek", 
          "workTime", "xformHr", "count")

# Metodos de Correlacao
# Pearson - coeficiente usado para medir o grau de relacionamento entre duas variaveis com relacao linear
# Spearman - eh um teste nao parametrico, para medir o grau de relacionamento entre duas variaveis
# Kendall - eh um teste nao parametrico, para medir a forca de dependencia entre duas variaveis
metodos <- c("pearson", "spearman")

cors <- lapply( metodos, function(method) 
  (cor(bikes[, cols], method = method)))

head(cors)

require(lattice)
plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot( levelplot(x, 
                  main = paste("Plot de Correlação usando Método", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

Map(plot.cors, cors, metodos)


## #####################################################################################
## Visualizando o relacionamento entre as variaveis preditoras e demanda por bike
## #####################################################################################
require(ggplot2)

labels <- c("Demanda de Bikes vs Temperatura",
            "Demanda de Bikes vs Humidade",
            "Demanda de Bikes vs Velocidade do Vento",
            "Demanda de Bikes vs Hora")

xAxis <- c("temp", "hum", "windspeed", "hr")

plot.scatter <- function(X, label){ 
    ggplot(bikes, aes_string(x = X, y = "cnt")) + 
        geom_point(aes_string(colour = "cnt"), alpha = 0.1) + 
        scale_colour_gradient(low = "green", high = "blue") + 
        geom_smooth(method = "loess") + 
        ggtitle(label) +
        theme(text = element_text(size = 20)) }

Map(plot.scatter, xAxis, labels)


## Explorando a interacao entre tempo e dia em dias da semana e fins de semana
labels <- list("Box plots - Demanda por Bikes as 09:00 para \n dias da semana e fins de semana",
               "Box plots - Demanda por Bikes as 18:00  para \n dias da semana e fins de semana")

Times <- list(9, 18)

plot.box2 <- function(time, label){ 
    ggplot(bikes[bikes$hr == time, ], 
           aes(x = isWorking, y = cnt, group = isWorking)) + 
        geom_boxplot( ) + ggtitle(label) +
        theme(text = element_text(size = 18)) }

Map(plot.box2, Times, labels)

## Gera saida no Azure ML
if(Azure) maml.mapOutputPort('bikes')