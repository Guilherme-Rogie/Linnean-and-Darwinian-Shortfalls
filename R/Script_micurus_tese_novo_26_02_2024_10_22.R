#library packages
library(picante)
library(ape)
library(phytools)
library(beepr)
library(openxlsx)
library(caper)

#phylogeny 
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
      s<-read.xlsx("Vetor.xlsx",1) #weights (Index of Taxonomic Uncertainty) of each Micrurus species
      s$Vetor<-s$ITU*1.5 #calibrating to have 40 splits
      s<-s[,c(1,4,6,7,9)]
      # calculate Diversification -------------------------------------------------
      
      #calculate tip rates
      equal_splits<- evol.distinct(tree, type = "equal.splits", 
                                   scale = FALSE, use.branch.lengths = TRUE) #calculate genetic diversity
      tip_dr <- 1/equal_splits$w # inverse of equal splits (speciation rate of each tip)
      
      
      # exporting variables before the splits ----------------------------------
      # output
      #R^2 Dr~lat
      lat= as.numeric(decostand(s$LatC,method="standardize"))
      X <-data.frame(Species=s$Species,
                     lat=as.numeric(decostand(s$LatC,method="standardize")),
                     lat2=as.numeric(decostand(s$LatC^2,method="standardize")),
                     tip_dr)
      tryCatch({ #in case an optimization error occurs
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
        cat("Erro capturado:", conditionMessage(err), "\n") #the loop will continue even if an error occurs
      })
       for (j in 1:100) {
         tree <- force.ultrametric(micrurus_tree[[i]])
    #vectors
    s<-read.xlsx("Vetor.xlsx",1) #weights (Index of Taxonomic Uncertainty) of each Micrurus species
    s$Vetor<-s$ITU*1.5
    s<-s[,c(1,4,6,7,9)]
#adjust tree
test <-runif(Ntip(tree),0,1) #create a testing vector for splits
#adopt split choice for each tip 
quebra<-ifelse(test < s$Vetor, 1,0) #vector, where 1 = will have split and 0 = will not have split
names(quebra)<-tree$tip.label #the names of the splits results will be the species

#description of new species
tip<-paste0("?",1:sum(quebra)) #save potential undescribed species
sister<-names(quebra[quebra==1]) #save which species will be sister taxa of each undescribed species
where<-numeric() #create empty local vector

 novas<-data.frame(Species=tip,LatMin=NA,LatMax=NA,LatC=NA,Vetor=runif(length(tip),0,1))
 A<-numeric()
 for(n in 1:length(sister)){ #loop from 1 to the current number of splits
  where<-c(where,which(tree$tip.label==sister[n])) #save the position of each new description
  
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
  
  #split the species
  tree_LTG <-bind.tip(tree,tip[n],where=where[n],
                      position=runif(1,0.25,0.75)*tree$edge.length[which(tree$edge[,2]==where[n])]) #add new tip close to the sister tip
  tree<-tree_LTG #update the tree
} #close the loop for the splits
 
 #new tip rates
equal_splits.final <- evol.distinct(tree, type = "equal.splits", scale = FALSE, use.branch.lengths = TRUE)
tip_dr.final <- 1/equal_splits.final$w #inverse of equal splits
s_final<-rbind(s,novas)
dados <- s_final[order(match(s_final$Species, tree$tip.label)), ]

# output
#R^2 Dr~lat
X_final <-data.frame(Species=dados$Species,
                     lat=as.numeric(decostand(dados$LatC,method="standardize")),
                     lat2=as.numeric(decostand(dados$LatC^2,method="standardize")),
                     tip_dr.final)
tryCatch({ #in case an optimization error occurs
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
    cat("Erro capturado:", conditionMessage(err), "\n") #the loop will continue even if an error occurs
  })
output[j,19]<-n
output[j,20]<-j
print(paste(j,"off",100))
       }
  output[1:20,21]<-i
  vazio<-rbind(vazio,output)
  print(paste(i,"off",1000))
  write.table(vazio,"Simulation_results.csv",row.names = F) 
  
}

    
