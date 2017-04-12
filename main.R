rm(list = ls())
setwd("~/Data\ Stream/Classifiers/Tiago/On-Demand/")
library(stream)
library(class)
library(ggplot2)
library (caret)

MAX_TEST <- 10         #Quantidade max de TESTES do algoritmo
RESULTS_HISTORY <- c()


#------------------------------------------------------Variáveis do Dataset--------------------------------------------------------------------------------------
#Separa o Conjunto de teste e o de treino 1 por 1
TRAINING_DATASET = read.csv("DS_Datasets/Synthetic/Stationary/BG_10k/BarsGaussAN0_10000.csv")[c(TRUE, FALSE), ]
TEST_DATASET = read.csv("DS_Datasets/Synthetic/Stationary/BG_10k/BarsGaussAN0_10000.csv")[c(FALSE, TRUE), ]



TRAINING_SET_SIZE <-nrow(TRAINING_DATASET)    #Quantidade de instacias no dataset de treino
TEST_SET_SIZE <-nrow(TEST_DATASET)            #Quantidade de instancia no dataset de teste


NATTRIBUTES <- ncol(TRAINING_DATASET) -1      #Calcula quantidade de atributos presentes no dataset
class_levels <- 1:max(as.numeric(names(table(as.factor(TEST_DATASET[,NATTRIBUTES+1])))))

for(teste in 1:MAX_TEST){

  #----------------------------------------------------------------------------------------------------------------------------------------------------------------  

  source("Ondemand.R")
  RESULTS_HISTORY <- cbind(RESULTS_HISTORY,list(teste = HISTORY))
}

cat("OnDemand")
