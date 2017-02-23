library(stream)
#----------------------------------------------------------------
DATASET = iris               #Mudar isso para ser mais genérico
NCOLLUMS <- ncol(DATASET)    #Retorna númedo de colunas do dataset
NATTRIBUTES <- NCOLLUMS -1   #Calcula quantidade de atributos
INITNUMBER <- 15             #mudar isso para ser mais genérico(algo mais automático)  
MAX_NUMBER_MC <- 10          #verificar um jeito mais generico (algo mais automático)  
MC_ID = 0                    #contador de id do microgrupo
#Primeiro Pegar do fluxo e depois splitar!

#FUNCÕES---------------------------------------------------------
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
  #--------------------------------------------------------------------------------------------------------------------
}



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
maxInitMicroClusters = as.numeric(classSetLength[which.min(classSetLength)])

#Utilizar o kmeans em cada grupo de classe e retornar k microgrupos para cada classe

set.seed(2)
#Não consigo agrupar em k grupos onde k é o número de observacoes, WHY?!!
#verificar de mudar esse nstart!
splittedMicroClusters <- lapply(splittedPoints, function(classSet){kmeans(classSet[, 1:NATTRIBUTES], maxInitMicroClusters-1, nstart = 5)}) 

#TODO
#criar data frame de todos os micro-grupos(lista de micro-grupos) e adicionar campo id e área de limite máximo
#(id_micro-grupo, centro, area de limite maximo,CF2x,CF1x,CF2t,CF1t,class_id)
#criar funcao de distancia euclidiana
#calcular o limite max de área para cada microgrupo

splittedMicroClusters[["setosa"]]$centers





#Fase 2 - Manutencão ONLINE

