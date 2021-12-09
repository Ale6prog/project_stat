library(tidyverse)
library(magrittr)
library(neuralnet)
library(caret)

# Load des données modifiées scalées, fichier à modif
df_act_scaled <- read_csv("./scaled_dude_erk2_mk01_Descriptors.csv")

# 1 : 50/50
# 2 : 75/25
# 3 : toutes les données
list_cases <- vector(mode = "list", length = 3)

# 3
list_cases[[3]] <- df_act_scaled

# 1
df_act_equi <- df_act_scaled %>%
  group_by(is_active) %>%
  slice_head(n = 79)

list_cases[[1]] <- df_act_equi

# 2
df_act_25 <- df_act_scaled %>%
  filter(is_active == 1)
df_act_75 <- df_act_scaled %>%
  filter(is_active == 0) %>%
  slice_sample(n = 3 * nrow(df_act_25))
df_act_25_75 <- rbind(df_act_25, df_act_75)

list_cases[[2]] <- df_act_25_75

# Création des échantillons d'apprentissage et test

#vecteur pour les noms 

listenom <- c("cas1","cas2","cas3")

# boucle qui va creer pop_(app,val et test) de chaque population et enlever id et X1

for (i in 1:3) {
  nomapp <- paste(listenom[i],"app",sep="_")
  nomval <- paste(listenom[i],"val",sep="_")
  nomtest <- paste(listenom[i],"test",sep="_")
  ref <- seq(1:nrow(list_cases[[i]]))
  aleat1 <- sample(ref, nrow(list_cases[[i]])*1/3) 
  ref <- ref[-aleat1]
  aleat2 <- sample(ref,nrow(list_cases[[i]])*1/3)
  aleat3 <- setdiff(ref,aleat2)
  assign(nomapp,list_cases[[i]][3:9][aleat1,])
  assign(nomval,list_cases[[i]][3:9][aleat2,])
  assign(nomtest,list_cases[[i]][3:9][aleat3,])
}

listapp <- list(cas1_app,cas2_app,cas3_app)
listval <- list(cas1_val,cas2_val,cas3_val)
listtest <- list(cas1_test,cas2_test,cas3_test)

# boucle pour les réseau de neurone sur app (ça prend du temps)

  # Liste contenant les vecteur de topologie
listopos <- list(c(2),c(5,3),c(6),c(2,2),c(6,4,2))


for (i in 1:3) {
  for (j in 1:5){
    nomtopo <- paste("topos",j,sep="")
    nomres <- paste("res.app",listenom[i],nomtopo,sep="_")
    assign(nomres,neuralnet(is_active~SlogP+NumLipinskiHBA +NumLipinskiHBD +NumRotatableBonds + NumRings,data=listapp[[i]],hidden= listopos[[j]],linear.output=T))
  }
}

#list contenant tous les réseau selon la population étudié
rescas1 <- list(res.app_cas1_topos1,res.app_cas1_topos2,res.app_cas1_topos3,res.app_cas1_topos4,res.app_cas1_topos5)
rescas2 <- list(res.app_cas2_topos1,res.app_cas2_topos2,res.app_cas2_topos3,res.app_cas2_topos4,res.app_cas2_topos5)
rescas3 <- list(res.app_cas3_topos1,res.app_cas3_topos2,res.app_cas3_topos3,res.app_cas3_topos4,res.app_cas3_topos5)

reseau <- list(rescas1,rescas2,rescas3)

# boucle pour la validation  une liste contenant les 3 cas qui contiennent eux même les 5 topos

for (i in 1:3) {
  for (j in 1:5) {
    nomvalres <- paste("resultcas",i,"topos",j,sep="")
    nomtopo <- paste("topos",j,sep="")
    nomresult <- paste("result.val",listenom[i],nomtopo,sep="_")
    assign(nomresult, predict(reseau[[i]][[j]], listval[[i]]))
  }
}

# liste de validation pour chaque cas
result_cas1 <- list(result.val_cas1_topos1,result.val_cas1_topos2,result.val_cas1_topos3,result.val_cas1_topos4,result.val_cas1_topos5)
result_cas2 <- list(result.val_cas2_topos1,result.val_cas2_topos2,result.val_cas2_topos3,result.val_cas2_topos4,result.val_cas2_topos5)
result_cas3 <- list(result.val_cas3_topos1,result.val_cas3_topos2,result.val_cas3_topos3,result.val_cas3_topos4,result.val_cas3_topos5)

#  une liste contenant les 3 cas qui contiennent eux même les 5 topos
listresult <- list(result_cas1,result_cas2,result_cas3)



