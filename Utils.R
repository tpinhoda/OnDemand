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
  return(list(CF1x=center,CF2x=center^2,CF1t=TIME,CF2t=TIME^2,n=1,class_id=class,id=MC_ID))
}

get.centers <- function(MICROCLUSTERS){
  #Retorna o centro de todos os micro-grupos
  return(t(sapply(MICROCLUSTERS, function(microcluster){
    if(microcluster$n >0)
      microcluster$CF1x/microcluster$n
  })))
}

get.class <- function(MICROCLUSTERS){
  #Retorna o centro de todos os micro-grupos
  return(t(sapply(MICROCLUSTERS, function(microcluster){
    if(microcluster$n >0)
      microcluster$class_id
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
    return(class_microclusters)
  point_distances <- get.distances(MICROCLUSTERS,point)
  nearest_index <- class_microclusters[which.min(point_distances[class_microclusters])]
  nearest_dist <- which.min(point_distances[class_microclusters])
  return(c(index = nearest_index, distance = nearest_dist))
}


mean.timestamp <- function(microcluster){
  points_number <- microcluster$n
  std_timestamp <- sqrt((microcluster$CF2t/microcluster$n)-(microcluster$CF1t/points_number)^2)
  mean_timestamp <- microcluster$CF1t/points_number
  
  if(microcluster$n < 2*M*points_number){
    return(mean_timestamp)
  }else
    mean_timestamp <- qnorm(((M*points_number)/(2*points_number)),mean_timestamp,std_timestamp) 
  return(mean_timestamp)
    
}

find.min.relevant <- function(MICROCLUSTERS){
  mean_timestamps <- sapply(MICROCLUSTERS,function(microcluster){mean.timestamp(microcluster)})
  min_relevance <- min(mean_timestamps)
  min_relevant_index <- which.min(mean_timestamps)
  return(c(index = min_relevant_index, relevance = min_relevance))
}


check.relevance <- function(min._relevant, point_stream, class){
  if(min_relevant['relevance'] < PHI)
    delete.microcluster(min_relevant['index'], point_stream,class)
  else{
    merge.microclusters(min_relevant,point_stream,class)
    delete.microcluster(min_relevant['index'],point_stream,class)
  }  
}

delete.microcluster <- function(microcluster_index,point,class){

  MICROCLUSTERS[[microcluster_index]]$CF1x <<-  point
  MICROCLUSTERS[[microcluster_index]]$CF2x <<-  point*point
  MICROCLUSTERS[[microcluster_index]]$CF1t <<-  TIME
  MICROCLUSTERS[[microcluster_index]]$CF2t <<-  TIME^2
  MICROCLUSTERS[[microcluster_index]]$n <<-  1
  MICROCLUSTERS[[microcluster_index]]$class_id <<- class
  MC_ID <<- MC_ID + 1
  MICROCLUSTERS[[microcluster_index]]$id <<- MC_ID
}


sum.microclusters <- function(mc1,mc2){
  MICROCLUSTERS[[mc1]]$CF1x <<-  MICROCLUSTERS[[mc1]]$CF1x + MICROCLUSTERS[[mc2]]$CF1x
  MICROCLUSTERS[[mc1]]$CF2x <<-  MICROCLUSTERS[[mc1]]$CF2x + MICROCLUSTERS[[mc2]]$CF2x   
  MICROCLUSTERS[[mc1]]$CF1t <<-  MICROCLUSTERS[[mc1]]$CF1t + MICROCLUSTERS[[mc2]]$CF1t
  MICROCLUSTERS[[mc1]]$CF2t <<-  MICROCLUSTERS[[mc1]]$CF2t + MICROCLUSTERS[[mc2]]$CF2t
  MICROCLUSTERS[[mc1]]$n <<-  MICROCLUSTERS[[mc1]]$n + MICROCLUSTERS[[mc2]]$n  
  MICROCLUSTERS[[mc1]]$id <<- c(MICROCLUSTERS[[mc1]]$id,MICROCLUSTERS[[mc2]]$id )

}

merge.microclusters <- function(min_relevant,point,class){
  min_relevant_mic <- MICROCLUSTERS[[min_relevant['index']]]
  min_relevant_class <- min_relevant_mic$class_id
  min_relevant_center <- min_relevant_mic$CF1x/min_relevant_mic$n
  class_microclusters <- find.microclusters(MICROCLUSTERS,min_relevant_class)
  if(is.empty(class_microclusters))
    delete.microcluster(min_relevant,point,class)
  else{
    point_distances <- get.distances(MICROCLUSTERS,min_relevant_center)
    point_distances[min_relevant['index']] <- Inf
    nearest_index <- class_microclusters[which.min(point_distances[class_microclusters])]
    sum.microclusters(nearest_index,min_relevant['index'])
  }
  
}

get.maxboundary <- function(MICROCLUSTERS,mic_index){
  mic <- MICROCLUSTERS[[mic_index]]
  if(mic$n == 1)
    return(mc_initial_max_boundary[mic_index])
  rmsd <- sqrt(sum(mic$CF2x)/mic$n - sum(mic$CF1x^2)/mic$n^2)
  return(T*rmsd)
}

add.point <- function(microcluster_index,point){
  
  MICROCLUSTERS[[microcluster_index]]$CF1x <<- MICROCLUSTERS[[microcluster_index]]$CF1x + point
  MICROCLUSTERS[[microcluster_index]]$CF2x <<- MICROCLUSTERS[[microcluster_index]]$CF2x + point * point
  MICROCLUSTERS[[microcluster_index]]$CF1t <<- MICROCLUSTERS[[microcluster_index]]$CF1t + TIME
  MICROCLUSTERS[[microcluster_index]]$CF2t <<- MICROCLUSTERS[[microcluster_index]]$CF2t + TIME^2
  MICROCLUSTERS[[microcluster_index]]$n <<- MICROCLUSTERS[[microcluster_index]]$n + 1
  
}

get.rows = function(training_points) lapply(seq_len(nrow(training_points)),function(index) return(data.matrix(training_points[index,])))

mod<-function(x,m){ 
  t1<-x/m
  return(x-t1*m)
}

store.snapshot <- function(MICROCLUSTERS, TIME){
  for(frame_number in FRAMES){
    frame_index <- frame_number + 1
    if((TIME%%2^frame_number==0) & (TIME%%2^(frame_number+1)>0))  {
      if(as.numeric(length(SNAPSHOTS[[frame_index]]$frame_slot)) < FRAME_MAX_CAPACITY){
        SNAPSHOTS[[frame_index]]$frame_slot <<- c(SNAPSHOTS[[frame_index]]$frame_slot,list(list(MICROCLUSTERS = MICROCLUSTERS, time = TIME)))
      }else{
        SNAPSHOTS[[frame_index]]$frame_slot <<- c(SNAPSHOTS[[frame_index]]$frame_slot[2:FRAME_MAX_CAPACITY],list(list(MICROCLUSTERS = MICROCLUSTERS, time = TIME))) 
      }
    }  
  }
}


get.centersclass <- function(MICROCLUSTERS){
  return(t(sapply(MICROCLUSTERS, function(microcluster){
    c(as.numeric(microcluster$CF1x/microcluster$n),microcluster$class_id)
  })))
}


find.snapshot <- function(t){
  while(t >= 0){
    for(fr in FRAMES+1){
      snaps <- 1
      while(snaps < length(SNAPSHOTS[[fr]]$frame_slot)){
        if(SNAPSHOTS[[fr]]$frame_slot[[snaps]]$time == t){
          return(SNAPSHOTS[[fr]]$frame_slot[[snaps]]$MICROCLUSTERS)
        }
        snaps <- snaps + 1
      }
    }
    t <- t - 1
  }  
}

relating.microcluster <- function(time,horizon) {
  mc1 <- find.snapshot(time)
  mc2 <- find.snapshot(time-horizon)
  idmc1 <- 1
  while(idmc1 < length(mc1)){
    idmc2 <- 1
    while(idmc2 < length(mc2)){
      if(length(setdiff(mc1[[idmc1]]$id,mc2[[idmc2]]$id))<length(mc1[[idmc1]]$id))
        mc1[[idmc1]]$CF1x <- mc1[[idmc1]]$CF1x - mc2[[idmc2]]$CF1x
      mc1[[idmc1]]$CF2x <- mc1[[idmc1]]$CF2x - mc2[[idmc2]]$CF2x
      mc1[[idmc1]]$CF1t <- mc1[[idmc1]]$CF1t - mc2[[idmc2]]$CF1t
      mc1[[idmc1]]$CF2t <- mc1[[idmc1]]$CF2t - mc2[[idmc2]]$CF2t
      mc1[[idmc1]]$n <- mc1[[idmc1]]$n - mc2[[idmc2]]$n
      mc1[[idmc1]]$CF1t <- setdiff(mc1[[idmc1]]$id,mc2[[idmc2]]$id)
      idmc2 <- idmc2 + 1
    }
    idmc1 <- idmc1 + 1
  }  
  return(mc1)
}

eucDist <- function(x1, x2){ return(sqrt(sum((x1 - x2) ^ 2))) }

calculate.accuracy <- function(y_pred,label){
  index_label <- 1
  right <- 0
  for(y in y_pred){
    if(y == label[index_label])
      right <- right + 1
    index_label <- index_label + 1
  }
  return(right/index_label)
}