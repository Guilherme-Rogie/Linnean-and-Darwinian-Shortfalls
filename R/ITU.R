library(ape)
library(caper)
library(openxlsx)
library(car)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)

#####################___________________________________________________________
#phylogeny
tree_coralsnakes<-read.tree("Micrurus.tree.tre")
tree_coralsnakes$tip.label

#data frame
coralsnakes_df<-read.xlsx("coralsnakes_data_ITU.xlsx")
colnames(coralsnakes_df)
head(coralsnakes_df)


##################______________________________________________________________
#check if there are any missing species
setdiff(tree_coralsnakes$tip.label, coralsnakes_df$Species)
str(coralsnakes_df)


#verify the data

hist(log(coralsnakes_df$Splitting))
hist(coralsnakes_df$Description_date)
hist(log(coralsnakes_df$Range_size))
hist(coralsnakes_df$Synonyms)
hist(log(coralsnakes_df$Body_size))
hist(log(coralsnakes_df$Papers))
hist(coralsnakes_df$Records)
hist(coralsnakes_df$DR)

##
tree_coralsnakes$node.label<-NULL


#create the comparative data
Revision.PGLS <- comparative.data(phy = tree_coralsnakes, data = coralsnakes_df,
                                  names.col = Species, vcv = TRUE,
                                  na.omit = FALSE, warn.dropped = TRUE)


##########_____________likelihood of a species being split  

model.pgls <- pgls(log(Splitting) ~ scale (log (Body_size)) + scale (Synonyms) + scale (Latitude) + scale (log (Range_size)) + scale (Papers) + scale (Records) + scale (Description_date) + scale (DR),
                     data = Revision.PGLS, lambda = "ML")

summary(model.pgls)


########check the assumptions
par(mfrow = c(2, 2))
plot(model.pgls)

 

#######_________________________________________________________________________

fitted <- data.frame(Prediction = model.pgls$fitted)
fitted <- fitted %>% 
  rownames_to_column(var = "rowname")


fitted <- rename(fitted, Species = rowname)



# scale the fitted values 
fitted$scaled_values <- scale(fitted$Prediction)



# Adjust the scaled values to range from 0 to 1
fitted$scaled_data <- (fitted$scaled_values - min(fitted$scaled_values)) / (max(fitted$scaled_values) - min(fitted$scaled_values))
max(fitted$scaled_data)
min(fitted$scaled_data)


#
fitted <- fitted %>%
  select(Species, scaled_data)

#the Index of Taxonomic Uncertainty
fitted <- fitted %>%
  arrange(Species)




