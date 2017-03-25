#============================================FUNCÕES============================================================================
splitByClass <- function(dataset){
  classList <-dataset[,ncol(dataset)]
  splittedDF <- split(dataset,classList)
  return(splittedDF)
}

createDataStream <- function(dataset){
  #Transformar um data set em um fluxo
  stream <- DSD_Memory(dataset[,c(1:NATTRIBUTES)], class=dataset[,NATTRIBUTES+1], loop = FALSE) #Cria o fluxo a partir do dataset escolhido 
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
