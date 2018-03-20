## ################################################################################
## Analise de risco de credito com classificacao
## ################################################################################

library(ggplot2)
library(DMwR)
library(randomForest)

# lendo o dataset
credito <- read.csv("credito.csv", header = FALSE)

##################################################################################
# Nomeando as colunas
##################################################################################
cNames <-
    c("CheckingAcctStat",
                "Duration",
                "CreditHistory",
                "Purpose",
                "CreditAmount",
                "SavingsBonds",
                "Employment",
                "InstallmentRatePecnt",
                "SexAndStatus",
                "OtherDetorsGuarantors",
                "PresentResidenceTime",
                "Property",
                "Age",
                "OtherInstalments",
                "Housing",
                "ExistingCreditsAtBank",
                "Job",
                "NumberDependents",
                "Telephone",
                "ForeignWorker",
                "CreditStatus")

colnames(credito) <- cNames

#head(credito)
#str(credito)

##################################################################################
# Renomeando fatores
##################################################################################
credito$CreditStatus = factor(credito$CreditStatus, levels = c(1,2) , labels = c("Bom Pagador", "Mau Pagador"))
#pie(table(credito$CreditStatus))
#ggplot(credito, aes_string("Purpose")) + geom_bar() + facet_grid(. ~ CreditStatus)
#head(credito$CreditStatus)


##################################################################################
# Convertendo fatores em grupos
##################################################################################

## Funcao para valores em grupos
CreateGroups <- function(x, nlevs = 5, maxval = 1000, minval = 0, isOrdered = TRUE){
    cuts <- seq(min(x), max(x), length.out = nlevs + 1)
    cuts[1] <- minval
    cuts[nlevs + 1] <- maxval
    #print(cuts)
    groups <- cut(x, breaks = cuts, order_result = isOrdered)
    #replacedGroup <-  gsub("\\(", "\\[", groups)
    #uniqueNames <- unique(replacedGroup)
    #x <- factor(x = replacedGroup, labels = uniqueNames, ordered = isOrdered )
    
    x <- groups
}

credito[,"Duration_F"]      <- CreateGroups(credito[, "Duration"], maxval = 100)
credito[,"CreditAmount_F"]  <- CreateGroups(credito[, "CreditAmount"], maxval = 1000000)
credito[,"Age_F"]           <- CreateGroups(credito[, "Age"], maxval = 100)


#str(credito)
#ggplot(credito, aes_string("Age_F")) + geom_bar()  + facet_grid(. ~ CreditStatus)
#ggplot(credito, aes(Age_F)) + geom_bar(aes(fill = CreditStatus))



##################################################################################
# Dataset está desbalanceado, muito mais ocorrencias de bom pagador que mau pagador
# usando SMOTE para balancedar DS
##################################################################################
balancedCredit <- DMwR::SMOTE(CreditStatus ~ ., credito, k = 2, perc.under = 150)
#table(credito$CreditStatus)
#table(balancedCredit$CreditStatus)


##################################################################################
# Feature selection
##################################################################################
modelo <- randomForest(CreditStatus ~ .
                       - Duration
                       - Age
                       - CreditAmount
                       - ForeignWorker
                       - NumberDependents
                       - Telephone
                       - ExistingCreditsAtBank
                       - PresentResidenceTime
                       - Job
                       - Housing
                       - SexAndStatus
                       - InstallmentRatePecnt
                       - OtherDetorsGuarantors
                       - Age_F
                       - OtherInstalments,
                       data = balancedCredit,
                       ntree = 100, nodesize = 10, importance = TRUE)


varImpPlot(modelo)

##################################################################################
# Dividindo entre teste e treino
##################################################################################
splitData <- function(dataframe, seed = NULL) {
    if(!is.null(seed)) set.seed(seed)
    
    index <- 1:nrow(dataframe)
    trainIndex <- sample(index, trunc(length(index)/2))
    
    trainSet <- dataframe[trainIndex, ]
    testSet <- dataframe[-trainIndex, ]
    
    list(trainSet = trainSet, testSet = testSet)
}

splits <- splitData(balancedCredit, seed = 808)
dadosTeste <- splits$testSet
dadosTreino <- splits$trainSet


##################################################################################
# Criando Modelo
##################################################################################
set.seed(808)
modelo <- randomForest(CreditStatus ~ 
                         CheckingAcctStat
                       + CreditHistory
                       + Duration_F
                       + Property
                       + Purpose
                       + Employment
                       + CreditAmount_F,
                       data = dadosTreino,
                       ntree = 100, nodesize = 10)

print(modelo)

resultadoPrevisto <- data.frame(actual = balancedCredit$CreditStatus,
                                previsto = predict(modelo, newdata = dadosTeste))