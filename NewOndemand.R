library(stream)
#------------------------------------------------------Variáveis do Dataset--------------------------------------------------------------------------------------
#Separa o Conjunto de teste e o de treino 1 por 1
TRAINING_DATASET = read.csv("../../DS_Datasets/Synthetic/Stationary/BG_10k/BarsGaussAN0_10000.csv")[c(TRUE, FALSE), ]
TEST_DATASET = read.csv("../../DS_Datasets/Synthetic/Stationary/BG_10k/BarsGaussAN0_10000.csv")[c(FALSE, TRUE), ]

TRAINING_SET_SIZE <-nrow(TRAINING_DATASET)    #Quantidade de instacias no dataset de treino
TEST_SET_SIZE <-nrow(TEST_DATASET)            #Quantidade de instancia no dataset de teste
NATTRIBUTES <- ncol(TRAINING_DATASET) -1      #Calcula quantidade de atributos presentes no dataset

#----------------------------------------------Variáveis de inicializacao do algoritmo-----------------------------------------------------------------------------------------
INITNUMBER <- 200            #Quantidade inicial de pontos que serão utilizados na criacao dos micro-grupos iniciais  
MICROCLUSTER_RATIO <- 5     #Quantidade de micro-grupos máxima por classe na criacao inicial
FRAME_MAX_CAPACITY <- 7      #Quantidade de snapshost por frame
BUFFER_SIZE <- 1000           #Quantidade de pontos a ser recebida até para q seja feito o teste no fluxo de teste
KFIT <- 100                   #Quantidade de pontos que serão testadas
T <- 2                       #Multiplica pela distancia do micro-grupo mais proximo para definir o limite maximo do micro-grupo inicial
M <- 0.32                    #Porcentagem de ultimos pontos a chegar no micro-grupo
POINTS_PER_UNIT_TIME <- 50   #Pontos que chegarao a cada 1 unidade de tempo
MAX_ITERATIONS <- 10         #Quantidade max de iteracoes do algoritmo
PHI <- 0.5                   #Limiar para decidir se um mcrogrupo é deletado ou merge
P <- 1                       #Quantidade de horizontes para a classificacão
STORE_MC <- 1                #Intervalo de tempo para armazenar um snapshot

#-------------------------------------------Variáveis globais inicializadas automaticamente-------------------------------------------------------------------
FRAME_NUMBER = round(log2(TRAINING_SET_SIZE))      #Quantidade de frames que haverá na tabela geométrica
FRAMES = 0:(FRAME_NUMBER-1)                    #Lista dos números dos frames ordenada de forma crescente (0 - framenumber-1)
SNAPSHOTS = lapply(FRAMES,function(frame){list(frame_number=frame,frame_slot=c())}) #Estrutura da tabela geométrica
MC_ID <- 0                                    #Contador de id dos micro-grupos
TIME <- 0                                     #Contador de tempo

#------------------------------------------------------Inicializa funcões--------------------------------------------------------------------------------------------
source("Utils.R")

#==============================================================INÍCIO===========================================================================================
#Fase 1 - Inicializacão - OFFLINE


#Criar fluxo de dados do conjunto de treino e teste
TRAINING_STREAM <- create.datastream(TRAINING_DATASET)
TEST_STREAM <- create.datastream(TEST_DATASET)  

#Dividir os INITNUMBER primeiros pontos do fluxo de treino por classe
inicialization_points <- get_points(TRAINING_STREAM, n = INITNUMBER, class = TRUE) #Obtem os INITNUMBER primeiros pontos do fluxo
splitted_points <- split.class(inicialization_points)                              #Separa os pontos por classe

#Atualiza o tempo do sistema
TIME <- INITNUMBER/POINTS_PER_UNIT_TIME

#Utilizar o kmeans em cada grupo de classe e retornar k microgrupos para cada classe
set.seed(1)
splitted_microclusters <- lapply(splitted_points, function(class_set){
                                c(kmeans(class_set[, 1:NATTRIBUTES], MICROCLUSTER_RATIO, nstart = 5),class=as.character(class_set[1,NATTRIBUTES+1]))
                           }) 

#Sumariza os grupos em micro-grupos
MICROCLUSTERS <- sapply(splitted_microclusters, function(class_set_mc){
                       apply(class_set_mc$centers,1,function(center){
                             create.microcluster(center,class_set_mc$class)
                        })                           
                  })
MICROCLUSTERS_SIZE = length(MICROCLUSTERS)

#Calculo do limite de acão maximo de cada micro-grupo inicial

dist_centers_matrix <- dist.microclusters(MICROCLUSTERS) #calcula a distancia entre todos os micro-grupos inciais

mc_initial_max_boundary <- T*apply(dist_centers_matrix,1,function(dists_microcluster){
                                        c(min( dists_microcluster[dists_microcluster!=min(dists_microcluster)] ))    
                                     }) 
#==============================================================================================================================================


#Fase 2 - Manutencão ONLINE
remaining_points = TRAINING_SET_SIZE - INITNUMBER #pontos restantes no fluxo de treino a serem processados
displacement = INITNUMBER
while(remaining_points > 0){
  
  #Verifica se a quantidade de pontos restantes é menor do que a quantidade de pontos para o buffer
  if(remaining_points < BUFFER_SIZE){
    BUFFER_SIZE < remaining_points
  }
  
  remaining_points_buffer <- BUFFER_SIZE
  points_until_store <- STORE_MC*POINTS_PER_UNIT_TIME
  #Processa BUFFER_SIZE pontos do fluxo de treino por STORE_MC*POINTS_PER_UNIT
  while(remaining_points_buffer > 0){
    
    training_points <- get_centers(TRAINING_STREAM, n=points_until_store, class = TRUE)
    for(stream_point in training_points){
      class_stream_point <- as.character(stream_point[NATTRIBUTES+1]) #Classe do ponto
      stream_point <- stream_point[1:NATTRIBUTES]                     #Ponto sem a classe
      
      #Calcular distancia do ponto para os microgrupos de sua classe
      
    }
    
    #salvar snapshot
    remaining_points_buffer <- remaining_points_buffer - points_until_store 
  }
  
  
  
  
  
  test_points <- get_centers(TEST_STREAM, n=BUFFER_SIZE+displacement,class = TRUE)
  #Pega os BUFFER_SIZE+displacement pontos do fluxo de treino para deixa-los no mesmo tempo
  displacement <- KFIT
}


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


 mod<-function(x,m)
     {
         t1<-x/m
         return(x-t1*m)
       }


# MICROCLUSTERS[[1]]$CF1x para acessar membros da lista 
#a <- SNAPSHOTS[[1]]$frameslot[[1]][[1]].CF1x