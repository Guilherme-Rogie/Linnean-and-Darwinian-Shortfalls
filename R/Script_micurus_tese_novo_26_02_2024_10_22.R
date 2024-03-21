#Ler pacotes
library(picante)
library(ape)
library(phytools)
library(beepr)#Pacote som
library(openxlsx)
library(caper)

#Carregar filogenia 
micrurus_tree<-read.nexus("sunplin_Micrurus.txt")
vazio<-numeric()




    
    for(i in 1:1000){
      tree <- force.ultrametric(micrurus_tree[[i]])
      output<-matrix(NA,100,21)
      colnames(output)<-c("R2_lat_antes","lambda_lat_antes","alpha_lat_antes","beta_lat_antes",
                          "R2_quad_lat_antes","lambda_quad_lat_antes","alpha_quad_lat_antes","beta1_quad_lat_antes","beta2_quad_lat_antes",
                          "R2_lat_depois","lambda_lat_depois","alpha_lat_depois","beta_lat_depois",
                          "R2_quad_lat_depois","lambda_quad_lat_depois","alpha_quad_lat_depois","beta1_quad_lat_depois","beta2_quad_lat_depois",
                          "split","index_run","index_tree")
      s<-read.xlsx("Vetor.xlsx",1)#pesos (Incerteza taxonômica) de cada espécie de micurus
      s$Vetor<-s$ITU*1.5#Calibrando para ter 40 quebras
      s<-s[,c(1,4,6,7,9)]
      # Calcular Diversificação -------------------------------------------------
      
      ##Calcular tip rates (viés presente)
      equal_splits<- evol.distinct(tree, type = "equal.splits", 
                                   scale = FALSE, use.branch.lengths = TRUE)#Calcular indíce de diversidade genética
      tip_dr <- 1/equal_splits$w # inverse of equal splits (taxa de especiação de cada ponta)
      
      
      # Exportando variáveis antes das quebras ----------------------------------
      # output
      #R^2 Dr~lat
      lat= as.numeric(decostand(s$LatC,method="standardize"))
      X <-data.frame(Species=s$Species,
                     lat=as.numeric(decostand(s$LatC,method="standardize")),
                     lat2=as.numeric(decostand(s$LatC^2,method="standardize")),
                     tip_dr)
      tryCatch({#Caso ocorre erro de otimização
        res_mod_lat <- summary(pgls(tip_dr~lat,comparative.data(tree,X,"Species"), lambda="ML"))
        res_mod_quad_lat <- summary(pgls(tip_dr~lat+lat2,comparative.data(tree,X,"Species"), lambda="ML"))
        
        output[1:100,1] <- res_mod_lat$adj.r.squared
        output[1:100,2] <- res_mod_lat$param[2]
        output[1:100,3] <- res_mod_lat$coefficients[1,1]
        output[1:100,4] <- res_mod_lat$coefficients[2,1]
        
        
        output[1:100,5] <- res_mod_quad_lat$adj.r.squared
        output[1:100,6] <- res_mod_quad_lat$param[2]
        output[1:100,7] <- res_mod_quad_lat$coefficients[1,1]
        output[1:100,8] <- res_mod_quad_lat$coefficients[2,1]
        output[1:100,9] <- res_mod_quad_lat$coefficients[3,1]
      }, error = function(err) {
        cat("Erro capturado:", conditionMessage(err), "\n") # O loop continuará mesmo que ocorra um erro
      })
       for (j in 1:100) {
         tree <- force.ultrametric(micrurus_tree[[i]])
    #Carregar vetores
    s<-read.xlsx("Vetor.xlsx",1)#pesos (Incerteza taxonômica) de cada espécie de micurus
    s$Vetor<-s$ITU*1.5
    s<-s[,c(1,4,6,7,9)]
#Reajustar árvore
test <-runif(Ntip(tree),0,1)#Criar vetor de testagem para splits
##Adotar escolha de de split para cada ponta 
quebra<-ifelse(test < s$Vetor, 1,0)#Vetor, no qual 1= terá split e 0=não terá split
names(quebra)<-tree$tip.label#Nomes dos resultados de quebra serão as espécies

##Descrição de novas espécies e/ou adição de dados moleculares de espécies que não tinham
tip<-paste0("?",1:sum(quebra))#Salvar espécies potenciais ainda não descobertas
sister<-names(quebra[quebra==1])#Salvar quais espécies serão irmãs de cada uma ainda não descoberta
where<-numeric()#criar vetor local vazio

 novas<-data.frame(Species=tip,LatMin=NA,LatMax=NA,LatC=NA,Vetor=runif(length(tip),0,1))
 A<-numeric()
 for(n in 1:length(sister)){#loop de 1 até o número de splits atual
  where<-c(where,which(tree$tip.label==sister[n]))# salvar posição de cada nova descoberta
  
  A<-c(A,sample(seq(0.1,1,0.1),1))
  
  if(A[n]==1){
    novas$LatMin[n]<-s$LatMin[which(s$Species==sister[n])]
    novas$LatMax[n]<-s$LatMax[which(s$Species==sister[n])]
    novas$LatC[n]<-s$LatC[which(s$Species==sister[n])]
  }else{
    novas$LatMin[n]<-s$LatMin[which(s$Species==sister[n])]
    s$LatMin[which(s$Species==sister[n])]<-s$LatMin[which(s$Species==sister[n])]-A[n]
    s$LatC[which(s$Species==sister[n])]<-(s$LatMax[which(s$Species==sister[n])]+s$LatMin[which(s$Species==sister[n])])/2
    novas$LatMax[n]<-s$LatMin[which(s$Species==sister[n])]
    novas$LatC[n]<-(novas$LatMax[n] + novas$LatMin[n])/2
  }
  
  #Qubre as espécies
  tree_LTG <-bind.tip(tree,tip[n],where=where[n],
                      position=runif(1,0.25,0.75)*tree$edge.length[which(tree$edge[,2]==where[n])])# Adicionar nova ponta ao lado da ponta irmã
  tree<-tree_LTG#Atualizar árvore
}#fechar for dos splits 
 
 #Novo Tip_rates
equal_splits.final <- evol.distinct(tree, type = "equal.splits", scale = FALSE, use.branch.lengths = TRUE)#calcular estimativa de importância filogenética
tip_dr.final <- 1/equal_splits.final$w # inverse of equal splits
s_final<-rbind(s,novas)
dados <- s_final[order(match(s_final$Species, tree$tip.label)), ]

# output
#R^2 Dr~lat
X_final <-data.frame(Species=dados$Species,
                     lat=as.numeric(decostand(dados$LatC,method="standardize")),
                     lat2=as.numeric(decostand(dados$LatC^2,method="standardize")),
                     tip_dr.final)
tryCatch({#Caso ocorre erro de otimização
  res_mod_lat2 <- summary(pgls(tip_dr.final~lat,comparative.data(tree,X_final,"Species"), lambda="ML"))
  res_mod_quad_lat2 <- summary(pgls(tip_dr.final~lat+lat2,comparative.data(tree,X_final,"Species"), lambda="ML"))
  
  output[j,10] <- res_mod_lat2$adj.r.squared
  output[j,11] <- res_mod_lat2$param[2]
  output[j,12] <- res_mod_lat2$coefficients[1,1]
  output[j,13] <- res_mod_lat2$coefficients[2,1]
  
  output[j,14] <- res_mod_quad_lat2$adj.r.squared
  output[j,15] <- res_mod_quad_lat2$param[2]
  output[j,16] <- res_mod_quad_lat2$coefficients[1,1]
  output[j,17] <- res_mod_quad_lat2$coefficients[2,1]
  output[j,18] <- res_mod_quad_lat2$coefficients[3,1]

  }, error = function(err) {
    cat("Erro capturado:", conditionMessage(err), "\n") # O loop continuará mesmo que ocorra um erro
  })
output[j,19]<-n
output[j,20]<-j
print(paste(j,"off",100))
       }
  output[1:20,21]<-i
  vazio<-rbind(vazio,output)
  print(paste(i,"off",1000))
  write.table(vazio,"resul_livia_paper2.csv",row.names = F) 
  
}

    