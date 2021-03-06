##
# Script usado no Azure também
##
Azure <- FALSE

if(Azure) {
    source("src/Tools.R")
    bikes <- maml.mapInputPort(1)
    bikes$dteday <- set.asPOSIXct(bikes)
} else {
    source("Tools.R")
    bikesOriginal <- read.csv("bikes.csv", header = TRUE, stringsAsFactors = FALSE)
    
    ### selecionar as variaveis que serao usadas
    cols <- c("dteday", "mnth", "hr", "holiday", "workingday", "weathersit", "temp", "hum", "windspeed", "cnt")

    ### criando subset de dados
    bikesOriginal <- bikesOriginal[, cols]
    
    ### transcofrmar o objeto de data
    #head(bikes$dteday)
    bikesOriginal$dteday <- char.toPOSIXct(bikesOriginal)
    
    ### normalizar as variaveis preditoras numericas
    #?scale
    cols <- c("temp", "hum", "windspeed")
    bikesOriginal[, cols] <- scale(bikesOriginal[, cols])
}

### #################################################
### Verificando valores missing
### #################################################
any(is.na(bikesOriginal))
summary(bikesOriginal)
str(bikesOriginal)

bikes <- na.omit(bikesOriginal)

any(is.na(bikes))
summary(bikes)


### Criar um nova variavel para indicar dia da semana
bikes$isWorking <- ifelse(bikes$workingday & !bikes$holiday, 1, 0)

### ################################################################################
### Criar um fator ordenado para o dia da semana
### ################################################################################
bikes$dayWeek <- base::as.factor(base::weekdays(bikes$dteday)) # table(bikes$dayWeek)
bikes$dayWeek <- base::as.numeric(base::ordered(bikes$dayWeek),
                                  levels = c("Monday","Tuesday","Wednesday"
                                             ,"Thursday","Friday","Saturday","Sunday"))




bikes$weatherName <- factor(bikes$weathersit,
                           levels = c(1,2,3,4),
                           labels = c("Clear", "Mist", "Light Snow", "Heavy Rain"))

c<-table(bikes$weatherName, bikes$cnt)
c
barplot(c, legend = rownames(c))



### Adicionando coluna com a quantidade de meses, para utilizar no modelo
bikes <- month.count(bikes)
#head(bikes)
#tail(bikes)


### Adiciona variavel com valores unicos para o h orario do dia para dias da semana e fim de semana
### Com isso diferenciamos as horas dos dias da semana das fim de semana
bikes$workTime <- ifelse(bikes$isWorking, bikes$hr, bikes$hr + 24)

### Transforma os valores de hora na madrugada, quando a demana e praticamente nula
bikes$xformHr <- ifelse(bikes$hr >4, bikes$hr - 5, bikes$hr + 19)

### Adiciona uma variavel com os valores unicos para o horario do dia para dias da semana e fim de semana
bikes$xformWorkHr <- ifelse(bikes$isWorking, bikes$xformHr, bikes$xformHr + 24)



#head(bikes)