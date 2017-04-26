
results.evaluate <- function(R_H){
  
  Matriz_Chunks = NULL
  
  # Na vdd deve receber a lista contendo todos os elementos
  Matriz_Chunks = R_H
  
  #Recebe a quantidade de chunks que foi processado (a quantidade de listas que ter?)
  N = length(Matriz_Chunks[[1]])
  
  #Recebe a quantidade de vezes que foi executado o programa
  exec = length(Matriz_Chunks)
  
  #Criando a lista de resultados finais
  FINAL_RESULTS = NULL
  
  #Encontrando quantas classes tem na tabela atual
  qtdClass = ncol(Matriz_Chunks[[exec]][[N]]$table)
  
  #Fazendo chunk vezes, para passar por todos os chunks
  for(chunk in 1:N){
    
    #Daria para criar uma matriz generica (Ficaria mais facil)
    #Sum_matrix = matrix(nrow = qtdClass, ncol = 11)
    
    #Encontrando quantas classes tem na tabela atual
    #qtdClass = ncol(Matriz_Chunks[[1]][[chunk]]$table)
    
    #Inicializando as variaveis para somatorio
    Sum_Sensitivity = vector(length = qtdClass)
    Sum_F1 = vector(length = qtdClass)
    Sum_Precision = vector(length = qtdClass)
    Sum_Recall = vector(length = qtdClass)
    Sum_Accuracy = 0
    
    #Fazendo os somatorios
    for(i in 1:exec){
      Sum_Sensitivity = Sum_Sensitivity + Matriz_Chunks[[i]][[chunk]]$byClass[,"Sensitivity"]
      Sum_F1 = Sum_F1 + Matriz_Chunks[[i]][[chunk]]$byClass[,"F1"]
      Sum_Precision = Sum_Precision + Matriz_Chunks[[i]][[chunk]]$byClass[,"Precision"]
      Sum_Recall = Sum_Recall + Matriz_Chunks[[i]][[chunk]]$byClass[,"Recall"]
      Sum_Accuracy = Sum_Accuracy + Matriz_Chunks[[i]][[chunk]]$overall["Accuracy"]
    }
    
    #Encontrando Medias
    Ave_Sensitivity = Sum_Sensitivity / exec
    Ave_F1 = Sum_F1 / exec
    Ave_Precision = Sum_Precision / exec
    Ave_Recall = Sum_Recall / exec
    Ave_Accuracy = Sum_Accuracy / exec
    
    #Inicializando as variaveis para desvio padrao
    Dev_Sensitivity = vector(length = qtdClass)
    Dev_F1 = vector(length = qtdClass)
    Dev_Precision = vector(length = qtdClass)
    Dev_Recall = vector(length = qtdClass)
    Dev_Accuracy = 0
    FPR = vector(length = qtdClass)
    FNR = vector(length = qtdClass)
    ExC = vector(length = qtdClass)
    Ex = 0
    
    #Fazendo o somatorio da diferenca da media com o valor atual para o Desvio Padrao
    for(i in 1:exec){
      Dev_Sensitivity = Dev_Sensitivity + (Matriz_Chunks[[i]][[chunk]]$byClass[,"Sensitivity"] - Ave_Sensitivity)^2
      Dev_F1 = Dev_F1 + (Matriz_Chunks[[i]][[chunk]]$byClass[,"F1"] - Ave_F1) ^ 2
      Dev_Precision = Dev_Precision + (Matriz_Chunks[[i]][[chunk]]$byClass[,"Precision"] - Ave_Precision) ^ 2
      Dev_Recall = Dev_Recall + (Matriz_Chunks[[i]][[chunk]]$byClass[,"Recall"] - Ave_Recall) ^ 2
      Dev_Accuracy = Dev_Accuracy + (Matriz_Chunks[[i]][[chunk]]$overall["Accuracy"] - Ave_Accuracy) ^ 2
      
      #Encontrando CER
      FPR = FPR + (1 - Matriz_Chunks[[i]][[chunk]]$byClass[,"Specificity"])
      FNR = FNR + (1 - Matriz_Chunks[[i]][[chunk]]$byClass[,"Sensitivity"])
      
      #Encontrando constantes para CER
      for(t in 1:qtdClass){
        ExC[t] = ExC[t] + sum(Matriz_Chunks[[i]][[chunk]]$table[t,])
      }
      Ex = Ex + sum(Matriz_Chunks[[i]][[chunk]]$table)
    }
    
    #Encontrando as medias dos DEV
    Dev_Sensitivity = Dev_Sensitivity/exec
    Dev_F1 = Dev_F1/exec
    Dev_Precision = Dev_Precision/exec
    Dev_Recall = Dev_Recall/exec
    Dev_Accuracy = Dev_Accuracy/exec
    
    #Encontrando medias para fazer o somatorio do CER
    FPR = FPR/exec
    FNR = FNR/exec
    ExC = ExC/exec
    Ex = Ex/exec
    
    #encontrando CER
    CER = (sum((ExC*FPR)/Ex) + sum((ExC*FNR)/Ex)) / 2
    
    #Encontrando o desvio padrao do CER (Fazendo separado pq fiz depois)
    dcer = vector(length = exec)
    for(i in 1:exec){
      for(t in 1:qtdClass){
        ExC[t] = sum(Matriz_Chunks[[i]][[chunk]]$table[t,])
      }
      Ex = sum(Matriz_Chunks[[i]][[chunk]]$table)
      FPR = FPR + (1 - Matriz_Chunks[[i]][[chunk]]$byClass[,"Specificity"])
      FNR = FNR + (1 - Matriz_Chunks[[i]][[chunk]]$byClass[,"Sensitivity"])
      
      dcer[i] = (sum((ExC*FPR)/Ex) + sum((ExC*FNR)/Ex)) / 2
    }
    
    Dev_CER = sd(dcer)
    
    #Caso exec = 1,  Dev_CER vira NA
    if(is.na(Dev_CER)){
      Dev_CER = 0
    }
    
    #Encontrando o Desvio Padrao
    Dev_Sensitivity = sqrt(Dev_Sensitivity / (N-1))
    Dev_F1 = sqrt(Dev_F1 / (N-1))
    Dev_Precision = sqrt(Dev_Precision / (N-1))
    Dev_Recall = sqrt(Dev_Recall / (N-1))
    Dev_Accuracy = sqrt(Dev_Accuracy / (N-1))
    
    
    #Criando Matriz com os valores
    mat = matrix(nrow = qtdClass, ncol = 8)
    
    #Colocando os valores na matriz
    for(i in 1:qtdClass){
      mat[i,1] = Ave_F1[i]
      mat[i,2] = Ave_Precision[i]
      mat[i,3] = Ave_Recall[i]
      mat[i,4] = Ave_Sensitivity[i]
      mat[i,5] = Dev_F1[i]
      mat[i,6] = Dev_Precision[i]
      mat[i,7] = Dev_Recall[i]
      mat[i,8] = Dev_Sensitivity[i]
    }
    
    
    #modificando os nomes das colunas e linhas da matriz
    colnames(mat) = c("Ave_F1", "Ave_Precision", "Ave_Recall", "Ave_Sensitivity", "Dev_F1", "Dev_Precision", "Dev_Recall", "Dev_Sensitivity")
    
    #Criando a lista
    lista = NULL
    
    #indexando a matriz para uma lista
    lista$byclass = mat
    lista$overall["Ave_Accuracy"] = Ave_Accuracy
    lista$overall["Dev_Accuracy"] = Dev_Accuracy
    lista$overall["CER"] = CER
    lista$overall["Dev_CER"] = Dev_CER
    
    # Adiciona lista para a lista resultante
    FINAL_RESULTS[[chunk]] = lista
  }
  
  #Retornando a lista
  return(FINAL_RESULTS)
  
}