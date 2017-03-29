library(stream)
library(class)
#------------------------------------------------------Variáveis do Dataset--------------------------------------------------------------------------------------
#Separa o Conjunto de teste e o de treino 1 por 1
TRAINING_DATASET = read.csv("../../DS_Datasets/Synthetic/Non-Stationary/Bench2_10k/Benchmark2_10000.csv")[c(TRUE, FALSE), ]
TEST_DATASET = read.csv("../../DS_Datasets/Synthetic/Non-Stationary/Bench2_10k/Benchmark2_10000.csv")[c(FALSE, TRUE), ]

TRAINING_SET_SIZE <-nrow(TRAINING_DATASET)    #Quantidade de instacias no dataset de treino
TEST_SET_SIZE <-nrow(TEST_DATASET)            #Quantidade de instancia no dataset de teste
NATTRIBUTES <- ncol(TRAINING_DATASET) -1      #Calcula quantidade de atributos presentes no dataset

#----------------------------------------------Variáveis de inicializacao do algoritmo-----------------------------------------------------------------------------------------
INITNUMBER <- 500            #Quantidade inicial de pontos que serão utilizados na criacao dos micro-grupos iniciais  
MICROCLUSTER_RATIO <- 3      #Quantidade de micro-grupos máxima por classe na criacao inicial
FRAME_MAX_CAPACITY <- 8      #Quantidade de snapshost por frame
BUFFER_SIZE <- 600          #Quantidade de pontos a ser recebida até para q seja feito o teste no fluxo de teste
KFIT <- 200                  #Quantidade de pontos que serão testadas
T <- 2                       #Multiplica pela distancia do micro-grupo mais proximo para definir o limite maximo do micro-grupo inicial
M <- 0.32                    #Porcentagem de ultimos pontos a chegar no micro-grupo
POINTS_PER_UNIT_TIME <- 50   #Pontos que chegarao a cada 1 unidade de tempo
MAX_ITERATIONS <- 10         #Quantidade max de iteracoes do algoritmo
PHI <- 25*1000                #Limiar para decidir se um mcrogrupo é deletado ou merge
P <- 1                       #Quantidade de horizontes para a classificacão
STORE_MC <- 1                #Intervalo de tempo para armazenar um snapshot

#-------------------------------------------Variáveis globais inicializadas automaticamente-------------------------------------------------------------------
FRAME_NUMBER = round(log2(TRAINING_SET_SIZE))      #Quantidade de frames que haverá na tabela geométrica
FRAMES = 0:(FRAME_NUMBER-1)                    #Lista dos números dos frames ordenada de forma crescente (0 - framenumber-1)
SNAPSHOTS = lapply(FRAMES,function(frame){list(frame_number=frame,frame_slot=c())}) #Estrutura da tabela geométrica
MC_ID <- 0                                    #Contador de id dos micro-grupos
TIME <- 0                                     #Contador de tempo em unit * 1000
HORIZONS <- 2^(1:FRAME_MAX_CAPACITY)*1000              #Horizontes para verificar os kfit pontos

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
TIME <- (INITNUMBER/POINTS_PER_UNIT_TIME)*1000

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
displacement = INITNUMBER + KFIT
it <- 0
while(remaining_points >= BUFFER_SIZE+KFIT){
  it <- it + 1
  cat("Buffer: ", it ,"\n")
  remaining_points_buffer <- BUFFER_SIZE
  points_until_store <- STORE_MC*POINTS_PER_UNIT_TIME
  #Processa BUFFER_SIZE pontos do fluxo de treino por STORE_MC*POINTS_PER_UNIT
  while(remaining_points_buffer > 0){
    
    training_points <- get_points(TRAINING_STREAM, n=points_until_store, class = TRUE)
    
    #Atualizacao dos micro-gupos
    for(stream_point in get.rows(training_points)){
     
      class_stream_point <- as.character(stream_point[NATTRIBUTES+1]) #Classe do ponto
      stream_point <- t(stream_point[1:NATTRIBUTES])                     #Ponto sem a classe
  
      #Calcular distancia do ponto para os microgrupos de sua classe
      nearest <- nearest.microcluster(MICROCLUSTERS,stream_point,class_stream_point)
      
      min_relevant <- find.min.relevant(MICROCLUSTERS)
      if(is.empty(nearest))
        check.relevance(min_relevant,stream_point,class_stream_point)
      else{
        microcluster_maxboundary <- get.maxboundary(MICROCLUSTERS,nearest['index'])
        if(nearest['distance'] <= microcluster_maxboundary )
          add.point(nearest['index'],stream_point)
        else
          check.relevance(min_relevant,stream_point,class_stream_point)
      }
    }
    
    #salvar snapshot
    TIME <- TIME + (STORE_MC*1000)
    remaining_points_buffer <- remaining_points_buffer - points_until_store
    store.snapshot(MICROCLUSTERS,TIME)
  }
  
  #Fitting dos kfit pontos do fluxo de treino
  kfit_points <- get_points(TRAINING_STREAM, n=KFIT, class = TRUE)
  knn_testset <- kfit_points[,-(NATTRIBUTES+1)]
  knn_labelstest <- kfit_points[,(NATTRIBUTES+1)]
  HORIZONS_FITTING <- c()
  for(h in HORIZONS){
    horizon_microluster <- relating.microcluster(TIME,h)
    knn_trainingset <-as.data.frame(get.centers(horizon_microluster))
    classes <- as.factor(t(get.class(horizon_microluster)))
    complete_cases <- complete.cases(knn_trainingset)
    knn_trainingset <- knn_trainingset[complete_cases, ]
    classes <- classes[complete_cases]
    y_pred <- knn(knn_trainingset,knn_testset,classes,k=1)
    accuracy_kfit <- calculate.accuracy(y_pred,knn_labelstest)
    HORIZONS_FITTING <- c(HORIZONS_FITTING,list(list(training_set = knn_trainingset, accuracy = accuracy_kfit, labels = classes)))
    
   }
  
  #Fase de teste
  best_horizons <- get.besthorizons(HORIZONS_FITTING,P)
  test_points <- get_points(TEST_STREAM, n=BUFFER_SIZE+displacement,class = TRUE)
  test_set <- test_points[,-(NATTRIBUTES+1)]
  labels_test <- test_points[,(NATTRIBUTES+1)]
  horizons_pred <- c()
  
  #calcula o knn para cada p horizon
  for(horizon in best_horizons){
    training_set <- horizon$training_set
    training_labels <- horizon$labels
    test_pred <- knn(training_set,test_set,training_labels,k=1)
    horizons_pred <- cbind(horizons_pred,test_pred)
  }
  
  #pegar a classe que mais aparece nos p horizontes resultados para cada exemplo de test
  prediction <- apply(horizons_pred,1,function(row){as.numeric(names(which.max(table(row))))})
  accuracy_test <- calculate.accuracy(prediction,labels_test)
  cat("Accuracy: ", accuracy_test, "\n")
  #Pega os BUFFER_SIZE+displacement pontos do fluxo de treino para deixa-los no mesmo tempo
  displacement <- KFIT
  remaining_points <- remaining_points - (BUFFER_SIZE + KFIT)
}

