library(stream)
#------------------------------------------------------Variáveis do Dataset--------------------------------------------------------------------------------------
DATASET = read.csv("../../DS_Datasets/Synthetic/Stationary/BG_10k/BarsGaussAN0_10000.csv")
DATASET_SIZE <-nrow(DATASET)    #Quantidade de instacias no dataset
NATTRIBUTES <- ncol(DATASET) -1 #Calcula quantidade de atributos

#----------------------------------------------Variáveis de inicializaćao do algoritmo-----------------------------------------------------------------------------------------
INITNUMBER <- 15             #Quantidade inicial de pontos que serão utilizados na criacao dos micro-grupos iniciais  
MAX_NUMBER_MC <- 4           #Quantidade de micro-grupos máxima por classe na criacao inicial
FRAME_MAX_CAPACITY <- 3      #Quantidade de snapshost por frame
H <- 10                      #Tamanho da janela de tempo que será processada
KFIT <- 6                    #Quantidade de pontos que serão testadas
T <- 2                       #multiplica pela distancia do micro-grupo mais proximo para definir o limite maximo do micro-grupo inicial
M <- 10                      #Quantidade de pontos em um microcluster para se tirar a média do timestamp
POINTS_PER_UNIT_TIME <- 2    #Pontos que chegarao a cada 1 unidade de tempo
MAX_ITERATIONS <- 10         #Quantidade max de iteracoes do algoritmo
PHI <- 0.5                   #Limiar para decidir se um mcrogrupo é deletado ou merge
P <- 1                       #Quantidade de horizontes para a classificacão

#-------------------------------------------Variáveis globais inicializadas automaticamente-------------------------------------------------------------------
FRAME_NUMER = round(log2(DATASET_SIZE))
FRAMES = 0:(frameNumber-1)
SNAPSHOTS = lapply(frames,function(frame){list(frameNumber=frame,frameslot=c())})
MC_ID <- 0                   #contador de id do microgrupo

#------------------------------------------------------Inicializa funcões--------------------------------------------------------------------------------------------
source("Utils.R")

#==============================================================INÍCIO===========================================================================================
#Fase 1 - Inicializacão - OFFLINE


#Criar fluxo de dados
DATASTREAM <- createDataStream(DATASET)

#Dividir os INITNUMBER primeiros pontos do fluxo por classe
firstPoints <- get_points(DATASTREAM, n = INITNUMBER, class = TRUE) #Obtem os INITNUMBER primeiros pontos do fluxo
splittedPoints <- splitByClass(firstPoints)           #divide os pontos por classe

#Utilizar o kmeans em cada grupo de classe e retornar k microgrupos para cada classe
set.seed(2)
splittedMicroClusters <- lapply(splittedPoints, function(classSet){c(kmeans(classSet[, 1:NATTRIBUTES], MAX_NUMBER_MC, nstart = 5),class=as.character(classSet[1,NATTRIBUTES+1]))}) 

#Sumariza os grupos em micro-grupos
MICROCLUSTERS <- sapply(splittedMicroClusters, function(classSetMC){
                                                           apply(classSetMC$centers,1,function(center){
                                                                                         createMicroCluster(center,classSetMC$class)
                                                                                 })                           
                                                
                                              })

#Calculo do limite de acão maximo de cada micro-grupo inicial

DistCenters <- distBetweenMicroClusters(MICROCLUSTERS) #calcula a distancia entre todos os micro-grupos inciais

MAXBOUNDARIES <- T*apply(DistCenters,1,function(microClusterDists){
                                        c(min( microClusterDists[microClusterDists!=min(microClusterDists)] ))    
                                     }) 
#==============================================================================================================================================


#Fase 2 - Manutencão ONLINE

iniTime <- 0
time <- 0
STOP_ITERATIONS <- 0
while(!STOP_ITERATIONS){
  
      iniTime <- time + 1
      endTime <- iniTime + h - kfit
      for(time in iniTime:endTime){
          if(DATASTREAM$state$counter+POINTS_PER_UNIT_TIME >= DATASET_SIZE){
            POINTS_PER_UNIT_TIME <- DATASET_SIZE - DATASTREAM$state$counter 
            STOP_ITERATIONS = 1;
          }
          
                   #Recebe novos pontos
          if(POINTS_PER_UNIT_TIME > 0){
              newPoints <- get_points(DATASTREAM, n = POINTS_PER_UNIT_TIME, class = TRUE)
              for(indexPoint in 1:POINTS_PER_UNIT_TIME){
                  newPoint <- newPoints[indexPoint,1:NATTRIBUTES]
                  class <- as.character(newPoints[indexPoint,NATTRIBUTES+1])
                  microClustersCenters <- getCenters(MICROCLUSTERS)                               #Recupera os centros CF1x/n de todos os microclusters
                  distances <- apply(microClustersCenters,1,function(centers){                    #Calcula a distancia do novo ponto para todos os microclusters
                                                                     dist(rbind(centers, newPoint[,1:NATTRIBUTES]))
                                                            })
                  founded <- 0
                  checked <- 0
                  while((founded == 0) && (checked < length(distances))){                    
                      minDist <- min(distances)
                      minDistIndex <- which.min(distances)
                      if(class == MICROCLUSTERS[[minDistIndex]]$class_id){
                        founded <- 1
                        checked <- 0
                      }else{
                      distances[minDistIndex] = Inf
                      checked <- checked + 1
                      }
                  }
                  if(MICROCLUSTERS[[minDistIndex]]$n == 1){
                    if(MAXBOUNDARIES[minDistIndex] >= minDist){
                      addPoint(as.numeric(minDistIndex),newPoint)
                    }
                  }else {
                    if(checked < length(distances)){
                     rmsd <- sqrt(sum(MICROCLUSTERS[[minDistIndex]]$CF2x)/MICROCLUSTERS[[minDistIndex]]$n - sum(MICROCLUSTERS[[minDistIndex]]$CF1x^2)/MICROCLUSTERS[[minDistIndex]]$n^2)
                     if(t*rmsd >= minDist){
                       addPoint(as.numeric(minDistIndex),newPoint)
                     }else{
                       meanTimeStamps <- sapply(MICROCLUSTERS,function(microcluster){calculateMeanTimeStamp(microcluster)})
                       minRelevance <- min(meanTimeStamps)
                       minRelevanceIndex <- which.min(meanTimeStamps)
                       if(minRelevance[1]<phi){
                    
                         deleteMicroCluster(minRelevanceIndex[1],newPoint,class)
                       }else{
                       
                         DistCenters <- distBetweenMicroClusters(MICROCLUSTERS)
                         minIndexes <- which(DistCenters == min(DistCenters[DistCenters!=min(DistCenters)]), arr.ind = TRUE)
                         mcIndex1 <- minIndexes[1,1]
                         mcIndex2 <- minIndexes[1,2]
                         while(MICROCLUSTERS[[mcIndex1]]$class_id != MICROCLUSTERS[[mcIndex2]]$class_id){
                           DistCenters[mcIndex2,mcIndex1]=Inf
                           DistCenters[mcIndex1,mcIndex2]=Inf
                           minIndexes <- which(DistCenters == min(DistCenters[DistCenters!=min(DistCenters)]), arr.ind = TRUE)
                           mcIndex1 <- minIndexes[1,1]
                           mcIndex2 <- minIndexes[1,2]
                         }
                         mergeMicroClusters(mcIndex1,mcIndex2)
                         deleteMicroCluster(mcIndex2,newPoint,class)
                       }
                         
                     }
                    }else{
                      meanTimeStamps <- sapply(MICROCLUSTERS,function(microcluster){calculateMeanTimeStamp(microcluster)})
                      minRelevance <- min(meanTimeStamps)
                      minRelevanceIndex <- which.min(meanTimeStamps)
                      if(minRelevance[1]<phi){
                        
                        deleteMicroCluster(minRelevanceIndex[1],newPoint,class)
                      }else{
                        
                        DistCenters <- distBetweenMicroClusters(MICROCLUSTERS)
                        minIndexes <- which(DistCenters == min(DistCenters[DistCenters!=min(DistCenters)]), arr.ind = TRUE)
                        mcIndex1 <- minIndexes[1,1]
                        mcIndex2 <- minIndexes[1,2]
                        while(MICROCLUSTERS[[mcIndex1]]$class_id != MICROCLUSTERS[[mcIndex2]]$class_id){
                          DistCenters[mcIndex2,mcIndex1]=Inf
                          DistCenters[mcIndex1,mcIndex2]=Inf
                          minIndexes <- which(DistCenters == min(DistCenters[DistCenters!=min(DistCenters)]), arr.ind = TRUE)
                          mcIndex1 <- minIndexes[1,1]
                          mcIndex2 <- minIndexes[1,2]
                        }
                        mergeMicroClusters(mcIndex1,mcIndex2)
                        deleteMicroCluster(mcIndex2,newPoint,class)
                      }
                      
                    }
                  }  
              }
          }
        #salvar snapshot
          for(fr in frames){
            if(((time %% 2^fr) == 0) & (time %% 2^(fr+1)!=0)){
              if(as.numeric(length(SNAPSHOTS[[fr+1]]$frameslot)) < frameMaxCapacity){
                SNAPSHOTS[[fr+1]]$frameslot <- c(SNAPSHOTS[[fr+1]]$frameslot,list(list(MICROCLUSTERS = MICROCLUSTERS, time = time))) 
              }else{
                SNAPSHOTS[[fr+1]]$frameslot <- c(SNAPSHOTS[[fr+1]]$frameslot[2:frameMaxCapacity],list(list(MICROCLUSTERS = MICROCLUSTERS, time = time))) 
              }
            }  
          }
        
      }  
     #KNN
      
      micTc <- findSnapShotByTime(time)
      micTcH <- findSnapShotByTime(time-h)
      micFitting <- findRelatingMics(micTc,micTcH)
      
      centersFitting <-getCentersAndClass(micFitting)
      knnCenters <- c()
      for(c in 1:nrow(centersFitting)){
        if(sum(as.numeric(centersFitting[c,1:NATTRIBUTES])>0))
          knnCenters <- rbind(knnCenters, centersFitting[c,])
      }
      if(nrow(knnCenters) > 1){
        centers <- apply(knnCenters[,1:NATTRIBUTES],2,as.numeric)
      }else
        centers <- as.numeric(knnCenters[1:NATTRIBUTES])
      
      centersClass <- knnCenters[,NATTRIBUTES+1]
        
      POINTS_TO_FITTING <- POINTS_PER_UNIT_TIME * kfit
      if(DATASTREAM$state$counter+POINTS_PER_UNIT_TIME*kfit >= DATASET_SIZE){
        POINTS_PER_UNIT_TIME <- DATASET_SIZE - DATASTREAM$state$counter 
        STOP_ITERATIONS = 1;
      }
        
      if(POINTS_TO_FITTING > 0){
        time <- time + kfit
        fittingPoints <- get_points(DATASTREAM, n = POINTS_TO_FITTING, class = TRUE)
        fittingP <- (data.matrix(fittingPoints[,1:NATTRIBUTES]))
        fittingPClass <- as.character(fittingPoints[,NATTRIBUTES+1])
        
        fitting <- c()
        for(p in 1:POINTS_TO_FITTING){
          
          distFitting <- apply(centers,1,function(center){eucDist(fittingP[p,],center)}) 
          indexMinFitting <- which.min(distFitting)
          fitting[p] <- centersClass[indexMinFitting] 
        } 
        print("RESULTS +++++++++++++++++++++++++++++++++++=")
        print(fitting)
        print(fittingPClass)
      
      }
        #    newPoints[,NATTRIBUTES+1] = as.character(newPoints[,NATTRIBUTES+1])
      
  #    pointsAndCenters <- rbind(knnCenters,newPoints)
      
      
}




# MICROCLUSTERS[[1]]$CF1x para acessar membros da lista 
#a <- SNAPSHOTS[[1]]$frameslot[[1]][[1]].CF1x