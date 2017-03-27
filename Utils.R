#============================================FUNCÕES============================================================================
create.datastream <- function(dataset){
  #Transformar um data set em um fluxo
  #Cria o fluxo a partir do dataset escolhido
  #Caso queira um fluxo infinito mude o atributo loop para TRUE
  stream <- DSD_Memory(dataset[,c(1:NATTRIBUTES)], class=dataset[,NATTRIBUTES+1], loop = FALSE)  
  return(stream)
}

split.class <- function(dataset){
  #Cria uma lista de pontos separados pela classe
  class_list <-dataset[,NATTRIBUTES+1]
  splitted <- split(dataset,class_list)
  return(splitted)
}

create.microcluster <- function(center,class){
  #Cria um novo micro-grupo
  MC_ID <<- MC_ID + 1
  return(list(CF1x=center,CF2x=center^2,CF1t=0,CF2t=0,n=1,class_id=class,id=MC_ID))
}

get.centers <- function(MICROCLUSTERS){
  #Retorna o centro de todos os micro-grupos
  return(t(sapply(MICROCLUSTERS, function(microcluster){
    microcluster$CF1x/microcluster$n
  })))
}

dist.microclusters <- function(MICROCLUSTERS){
  #Retorna uma matrix de distancia entro todosos micro-grupos
  
  micro_clusters_centers <- get.centers(MICROCLUSTERS)
  
  #Calcula a distancia entre os micro-grupos
  dist_centers <- dist(micro_clusters_centers)
  dist_centers <- as.matrix(dist_centers)
  
  #renomeia as linhas e colunas
  rownames(dist_centers) <- c(1:length(MICROCLUSTERS))
  colnames(dist_centers) <- c(1:length(MICROCLUSTERS))
  
  return(dist_centers)
}

find.microclusters <-function(MICROCLUSTERS,class){
  #retorna a lista de ids de micro-grupos da mesma classe
  ids_class <- c()
  for(microcluster_index in 1:MICROCLUSTERS_SIZE)
    if(MICROCLUSTERS[[microcluster_index]]$class_id == class)
       ids_class <- c(ids_class,microcluster_index)
  return(ids_class)  
}

get.distances <- function(MICROCLUSTERS,point){
  microclusters_centers <- get.centers(MICROCLUSTERS)              #Recupera os centros CF1x/n de todos os microclusters
  distances <- apply(microclusters_centers,1,function(centers){    #Calcula a distancia do novo ponto para todos os microclusters
                    dist(rbind(centers, point))
                })
  return(distances)
}
is.empty <- function(x) return(length(x) == 0)

nearest.microcluster <- function(MICROCLUSTERS,point,class){
  class_microclusters <- find.microclusters(MICROCLUSTERS,class)
  if(is.empty(class_microclusters))
    return("no.class")
  
  point_distances <- get.distances(MICROCLUSTERS,point)
  nearest_index <- class_microclusters[which.min(point_distances[class_microclusters])]
  nearest_dist <- which.min(point_distances[class_microclusters])
  return(c(index = nearest_index, distance = nearest_dist))
}


mean.timestamp <- function(microcluster){
  points_number <- microcluster$n
  mean_timestamp <- microcluster$CF1t/points_number
  std_timestamp <- sqrt((microcluster$CF2t/microcluster$n)-(microcluster$CF1t/points_number)^2)
  
  if(microcluster$n < 2*M*points_number)
    return(mean_timestamp)
  else{
    
    return(qnorm(((M*points_number)/(2*points_number)),mean_timestamp,std_timestamp))
  }  
}

find.min.relevant <- function(MICROCLUSTERS){
  mean_timestamps <- sapply(MICROCLUSTERS,function(microcluster){mean.timestamp(microcluster)})
  min_relevance <- min(mean_timestamps)
  min_relevant_index <- which.min(mean_timestamps)
  return(c(index = min_relevant_index, relevance = min_relevance))
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
