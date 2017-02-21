library(stream)
#----------------------------------------------------------------
dataset = iris               #Mudar isso
NCOLLUMS <- ncol(dataset)    #Retorna númedo de colunas do dataset
NATTRIBUTES <- NCOLLUMS -1   #Calcula quantidade de atributos


#FUNCÕES---------------------------------------------------------
splitByClass <- function(dataset){
  classList <-dataset[,NCOLLUMS]
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



