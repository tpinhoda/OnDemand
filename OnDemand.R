library(stream)
#----------------------------------------------------------------
DATASET = iris               #Mudar isso para ser mais genérico
NCOLLUMS <- ncol(DATASET)    #Retorna númedo de colunas do dataset
DATASET_SIZE <-nrow(DATASET)
NATTRIBUTES <- NCOLLUMS -1   #Calcula quantidade de atributos
INITNUMBER <- 15             #mudar isso para ser mais genérico(algo mais automático)  
MAX_NUMBER_MC <- 12          #verificar um jeito mais generico (algo mais automático)  TEM QUE POR NO KMEANS
MC_ID <- 0                   #contador de id do microgrupo
t <- 2                       #multiplica pela distancia do micro-grupo mais proximo para definir o limite maximo do micro-grupo inicial
h <- 10                      #Tamanho da janela de tempo que será processada
kfit <- h*0.3                #Quantidade de pontos que serão testadas
m <- 10
POINTS_PER_UNIT_TIME <- 2    #Pontos que chegarao a cada 1 unidade de tempo
MAX_ITERATIONS <- 10

phi <- 0.5
frameNumber = round(log2(DATASET_SIZE))
frameMaxCapacity <- 3
frames = 0:(frameNumber-1)
SNAPSHOTS = lapply(frames,function(frame){list(frameNumber=frame,frameslot=c())})
#Primeiro Pegar do fluxo e depois splitar!

#============================================FUNCÕES============================================================================
splitByClass <- function(dataset){
  classList <-dataset[,ncol(dataset)]
  splittedDF <- split(dataset,classList)
  return(splittedDF)
}

createDataStream <- function(dataset){
  #Transformar um data set em um fluxo
  stream <- DSD_Memory(dataset[,c(1:NATTRIBUTES)], class=dataset[,NCOLLUMS], loop = FALSE) #Cria o fluxo a partir do dataset escolhido 
  return(stream)
  #Caso queira um fluxo infinito mude o atributo loop para TRUE
}

getCenters <- function(MICROCLUSTERS){
  return(t(sapply(MICROCLUSTERS, function(microcluster){
    microcluster$CF1x/microcluster$n
  })))
}

getCentersAndClass <- function(MICROCLUSTERS){
  return(t(sapply(MICROCLUSTERS, function(microcluster){
      c(as.numeric(microcluster$CF1x/microcluster$n),microcluster$class_id)
  })))
}

addPoint <- function(microClusterIndex,point){
  point <- data.matrix(point)
  MICROCLUSTERS[[microClusterIndex]]$CF1x <<- MICROCLUSTERS[[microClusterIndex]]$CF1x + point
  MICROCLUSTERS[[microClusterIndex]]$CF2x <<- MICROCLUSTERS[[microClusterIndex]]$CF2x + point * point
  MICROCLUSTERS[[microClusterIndex]]$CF1t <<- MICROCLUSTERS[[microClusterIndex]]$CF1t + 1
  MICROCLUSTERS[[microClusterIndex]]$CF2t <<- MICROCLUSTERS[[microClusterIndex]]$CF2t + 1
  MICROCLUSTERS[[microClusterIndex]]$n <<- MICROCLUSTERS[[microClusterIndex]]$n + 1
 
}

createMicroCluster <- function(center,class){
  MC_ID <<- MC_ID + 1
  return(list(CF1x=center,CF2x=center^2,CF1t=0,CF2t=0,n=1,class_id=class,id=MC_ID))
}

distBetweenMicroClusters <- function(MICROCLUSTERS){
  microClustersCenters <- getCenters(MICROCLUSTERS)

    #Calcula a distancia entre os micro-grupos
  DistCenters <- dist(microClustersCenters)
  DistCenters <- as.matrix(DistCenters)
  
  rownames(DistCenters) <- c(1:length(MICROCLUSTERS))
  colnames(DistCenters) <- c(1:length(MICROCLUSTERS))
  
  return(DistCenters)
}

calculateMeanTimeStamp <- function(microcluster){
  meanTimeStamp <- microcluster$CF1t/microcluster$n
  stdTimeStamp <- sqrt((microcluster$CF2t/microcluster$n)-(microcluster$CF1t/microcluster$n)^2)
  
  if(microcluster$n < 2*m)
    return(meanTimeStamp)
  else{
    
    return(qnorm(((m-1)/(2*microcluster$n)),meanTimeStamp,stdTimeStamp))
  }  
}

deleteMicroCluster <- function(microClusterIndex,point,class){
  point <- data.matrix(point)
  MICROCLUSTERS[[microClusterIndex]]$CF1x <<-  point
  MICROCLUSTERS[[microClusterIndex]]$CF2x <<-  point*point
  MICROCLUSTERS[[microClusterIndex]]$CF1t <<-  0
  MICROCLUSTERS[[microClusterIndex]]$CF2t <<-  0
  MICROCLUSTERS[[microClusterIndex]]$n <<-  1
  MICROCLUSTERS[[microClusterIndex]]$class_id <<- class
  MC_ID <<- MC_ID + 1
  MICROCLUSTERS[[microClusterIndex]]$id <<- MC_ID
}

mergeMicroClusters <- function(mc1,mc2){
  MICROCLUSTERS[[mc1]]$CF1x <<-  MICROCLUSTERS[[mc1]]$CF1x + MICROCLUSTERS[[mc2]]$CF1x
  MICROCLUSTERS[[mc1]]$CF2x <<-  MICROCLUSTERS[[mc1]]$CF2x + MICROCLUSTERS[[mc2]]$CF2x   
  MICROCLUSTERS[[mc1]]$CF1t <<-  MICROCLUSTERS[[mc1]]$CF1t + MICROCLUSTERS[[mc2]]$CF1t
  MICROCLUSTERS[[mc1]]$CF2t <<-  MICROCLUSTERS[[mc1]]$CF2t + MICROCLUSTERS[[mc2]]$CF2t
  MICROCLUSTERS[[mc1]]$n <<-  MICROCLUSTERS[[mc1]]$n + MICROCLUSTERS[[mc2]]$n  
  MICROCLUSTERS[[mc1]]$id <<- c(MICROCLUSTERS[[mc1]]$id,MICROCLUSTERS[[mc2]]$id )
}

findSnapShotByTime <- function(t){
  while(t >= 0){
    for(fr in frames+1){
      snaps <- 1
      while(snaps < length(SNAPSHOTS[[fr]]$frameslot)){
          if(SNAPSHOTS[[fr]]$frameslot[[snaps]]$time == t){
            return(SNAPSHOTS[[fr]]$frameslot[[snaps]]$MICROCLUSTERS)
          }
          snaps <- snaps + 1
      }
    }
    t <- t - 1
  }  
}

findRelatingMics <- function(mc1,mc2) {
  idmc1 <- 1
  while(idmc1 < length(mc1)){
    idmc2 <- 1
    while(idmc2 < length(mc2)){
      if(length(setdiff(mc1[[idmc1]]$id,mc2[[idmc2]]$id))<length(mc1[[idmc1]]$id))
        mc1[[idmc1]]$CF1x <- mc1[[idmc1]]$CF1x - mc2[[idmc2]]$CF1x
        mc1[[idmc1]]$CF2x <- mc1[[idmc1]]$CF2x - mc2[[idmc2]]$CF2x
        mc1[[idmc1]]$CF1t <- mc1[[idmc1]]$CF1t - mc2[[idmc2]]$CF1t
        mc1[[idmc1]]$CF2t <- mc1[[idmc1]]$CF2t - mc2[[idmc2]]$CF2t
        mc1[[idmc1]]$CF1t <- setdiff(mc1[[idmc1]]$id,mc2[[idmc2]]$id)
        idmc2 <- idmc2 + 1
    }
    idmc1 <- idmc1 + 1
  }  
  return(mc1)
}

eucDist <- function(x1, x2){ return(sqrt(sum((x1 - x2) ^ 2))) }
#========================================INÍCIO=======================================================================
#Fase 1 - Inicializacão - OFFLINE

#O dataset iris precisa ser randomizado para que possa vir dados de mais de um classe no inicio da stream 
set.seed(1) #verificar melhor o set seed!!
DATASET <- DATASET[sample(nrow(DATASET)),]

#Criar fluxo de dados
DATASTREAM <- createDataStream(DATASET)

#Dividir os INITNUMBER primeiros pontos do fluxo por classe
firstPoints <- get_points(DATASTREAM, n = INITNUMBER, class = TRUE) #Obtem os INITNUMBER primeiros pontos do fluxo
splittedPoints <- splitByClass(firstPoints)           #divide os pontos por classe

#Pegar o grupo com menor quantidade de pontos para ser a quantidade de micro-grupos iniciais para cada classe
classSetLength <-  lapply(splittedPoints, function(classSet) {nrow(classSet)})
#maxInitMicroClusters = as.numeric(round(classSetLength[which.min(classSetLength)]/2))
maxInitMicroClusters <- 4
#Utilizar o kmeans em cada grupo de classe e retornar k microgrupos para cada classe
set.seed(2)
#Não consigo agrupar em k grupos onde k é o número de observacoes, WHY?!!
#verificar de mudar esse nstart!
splittedMicroClusters <- lapply(splittedPoints, function(classSet){c(kmeans(classSet[, 1:NATTRIBUTES], maxInitMicroClusters-1, nstart = 5),class=as.character(classSet[1,NCOLLUMS]))}) 

#Cria estrutura de micro-grupos
MICROCLUSTERS <- sapply(splittedMicroClusters, function(classSetMC){
                                                           apply(classSetMC$centers,1,function(center){
                                                                                         createMicroCluster(center,classSetMC$class)
                                                                                 })                           
                                                
                                              })

#Calculo do limite maximo de cada micro-grupo inicial

#criar lista de centros
DistCenters <- distBetweenMicroClusters(MICROCLUSTERS)


MAXBOUNDARIES <- t*apply(DistCenters,1,function(microClusterDists){
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