  rm(list = ls())
  #Setando o diretorio-
  setwd("~/Data\ Stream/Classifiers/Tiago/On-Demand/")
  library(stream)
  library(class)
  library(ggplot2)
  library (caret)
  
  #lendo entrada para experimentos
  EXP = read.csv("Experiments/exp_1.csv", row.names = 1)
  MAX_TEST <- EXP["MAX_TEST",1]         #Quantidade max de TESTES do algoritmo
  NAME_EXP <- colnames(EXP)
  
  RESULTS_HISTORY <- c()
  SUM_RESULTS_HISTORY <- c()
  MISS_HISTORY <- NULL
  
  #------------------------------------------------------Variáveis do Dataset--------------------------------------------------------------------------------------
  #Lendo os caminhos para os DataSets
  ds = as.matrix(read.csv("DataSet_Path.csv"))
  DS_NAME = strsplit(strsplit(ds[EXP["DataSet",1]],"/")[[1]][5],".csv")[[1]]
  
  #Separa o Conjunto de teste e o de treino 1 por 1
  TRAINING_DATASET = read.csv(ds[EXP["DataSet",1]])[c(TRUE, FALSE), ]
  TEST_DATASET = read.csv(ds[EXP["DataSet",1]])[c(FALSE, TRUE), ]
  colnames(TRAINING_DATASET) <- c("x","y","class")
  colnames(TEST_DATASET) <- c("x","y","class")
  
  
  TRAINING_SET_SIZE <-nrow(TRAINING_DATASET)    #Quantidade de instacias no dataset de treino
  TEST_SET_SIZE <-nrow(TEST_DATASET)            #Quantidade de instancia no dataset de teste
  
  
  NATTRIBUTES <- ncol(TRAINING_DATASET) -1      #Calcula quantidade de atributos presentes no dataset
  class_levels <- 1:max(as.numeric(names(table(as.factor(TEST_DATASET[,NATTRIBUTES+1])))))
  nclass <- length(class_levels)
  xmax <- max(TRAINING_DATASET[,1])
  ymax <- max(TRAINING_DATASET[,2])
  
  xmin <- min(TRAINING_DATASET[,1])
  ymin <- min(TRAINING_DATASET[,2])
  xlim <- c(xmax,xmin)
  ylim <- c(ymax,ymin)
  
  
  for(teste in 1:MAX_TEST){
  
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------  
  
    source("Ondemand.R")
    RESULTS_HISTORY <- cbind(RESULTS_HISTORY,list(teste = HISTORY))
    SUM_RESULTS_HISTORY <- cbind(SUM_RESULTS_HISTORY,list(teste = SUM_HISTORY))
    MISS_HISTORY <- cbind(MISS_HISTORY,miss)
  }
  dir.create("Results_Buffers",showWarnings = FALSE)
  pdf(paste("Results_Buffers/OnDemand_Buffers_",NAME_EXP,".pdf",sep=""))
  
  TRAINING_INDEX <- INITNUMBER
  for(chunk in 1:length(HISTORY)){
    end_buffer <- TRAINING_INDEX + BUFFER_SIZE + KFIT
    
    mic <- cbind(TRAINING_HISTORY[[chunk]]$training_set,class_mic =TRAINING_HISTORY[[chunk]]$labels)
    test_points <- TRAINING_DATASET[TRAINING_INDEX:end_buffer,]
    test_points$class <- as.factor(test_points$class)
    
    mic_point <- geom_point(data = mic, aes(x = V1 , y = V2),color = mic$class_mic, shape = 10, size=10)
    point <- geom_point(aes(x=x ,y=y,color=class,shape=1), color = test_points$class,shape = 20)
    theme <- theme(panel.background = element_rect(fill = "white", colour = "grey50"),plot.title = element_text(hjust = 0.5))
    title_name <- paste("Buffer",chunk)
    title <- ggtitle(title_name)
    limites <- coord_cartesian(xlim = xlim, ylim = ylim)
    print(ggplot(test_points)+point+mic_point+title+theme+limites)
    
    cat("INIT: ",TRAINING_INDEX,"it: ",end_buffer,"\n")
    TRAINING_INDEX <- end_buffer
  }
  dev.off()
  
  PARAMETERS = c(EXEC = MAX_TEST, DATASET = DS_NAME, INITNUMBER = INITNUMBER, MICROCLUSTER_RATIO = MICROCLUSTER_RATIO, FRAME_MAX_CAPACITY = FRAME_MAX_CAPACITY, BUFFER_SIZE = BUFFER_SIZE, KFIT = KFIT, M = M, POINTS_PER_UNIT_TIME = POINTS_PER_UNIT_TIME, PHI = PHI, P = P, STORE_MC = STORE_MC )
  source("results_evaluate.R")
  #Avaliar resultados
  EVALUATED_RESULTS <- results.evaluate(SUM_RESULTS_HISTORY)
  dir.create("Results.Data",showWarnings = FALSE)
  #Salvar resultados
  saveRDS(PARAMETERS, paste("Results.Data/",NAME_EXP,"_Parameters.rds",sep=""))
  saveRDS(EVALUATED_RESULTS,  paste("Results.Data/",NAME_EXP,"_Results.rds",sep=""))
  saveRDS(SUM_RESULTS_HISTORY,  paste("Results.Data/",NAME_EXP,"_SumResults.rds",sep=""))
  
  cat("OnDemand")
 