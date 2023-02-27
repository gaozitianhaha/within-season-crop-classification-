#ternary Plots for test 2 paper 2
# created by ZG on 27 June 2022


library(randomForest)
library(dplyr)
library(psych)
library(caret)
library(ggplot2)
library(raster)

library(EnvStats)
library(abind)
library("ggsci")
library(ggplot2)
library(sf)
library(abind)
library("ggpubr")
library(gridExtra)
library(rgdal)
library(sf)
library(abind)
library(Rmisc)
library(ggtern)

setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/paper2_DATA/")
load("paper2_data_Filt.RData")

## test 2 - proportions of classes--------------------------------
proportion_data_flexible = function(TRAIN,which_month,
                                    prop_corn, prop_cotton, prop_rice){
  #croptype means which crop you would like to get a fixed proportion
  #trim to month
  training<- sapply(TRAIN[,1:(7*which_month)],as.numeric)%>%data.frame()
  training$label = TRAIN$label
  
  #count the fewest sample size -  the fewest number is slightly different
  # in each run, because we used a random function to the 80% of 
  # training samples.
  count_mat = training%>%group_by(label) %>% dplyr::summarise(n = n())
  fewest_num = count_mat%>%dplyr::select(n)%>%min()
  
  #trim each class
  dt_corn = training %>% dplyr::filter(label =="CORN")
  dt_corn_filt = dt_corn[sample(1:nrow(dt_corn),fewest_num),]
  
  dt_cotton = training %>% dplyr::filter(label =="COTTON")
  dt_cotton_filt = dt_cotton[sample(1:nrow(dt_cotton),fewest_num),]
  
  dt_rice = training %>% dplyr::filter(label =="RICE")
  dt_rice_filt = dt_rice[sample(1:nrow(dt_rice),fewest_num),]
  
  
  TRAIN_df = rbind(dt_corn_filt[sample(1:nrow(dt_corn_filt),
                                       fewest_num*prop_corn),],
                   dt_cotton_filt[sample(1:nrow(dt_cotton_filt),
                                         fewest_num*prop_cotton),],
                   dt_rice_filt[sample(1:nrow(dt_rice_filt),
                                       fewest_num*prop_rice),])
  
  return(TRAIN_df)
}

ModelAccuracy = function(which_ml,T_Data,V_Data){
  
  if(which_ml=="RF"){
    model = randomForest(label~., data=T_Data, 
                         ntree=500, proximity=T,importance=TRUE)
  }
  if(which_ml=="SVM"){
    model =   svm(label~ .,data = T_Data,
                  type = 'C-classification',
                  kernel = 'linear')
  }
  
  vali_prop = predict(model,V_Data)
  vali_prop_CM = confusionMatrix(vali_prop,V_Data$label)$table
  
  #OA
  OA_prop = sum(diag(vali_prop_CM))/ sum(vali_prop_CM)
  return(OA_prop)
}

#calculate the actual proportion from survey
autual_pcent_mat = rep(NA,3)

autual_prop = RF_df%>%
  group_by(label)%>%
  dplyr::summarise(n = n())%>%
  dplyr::select(n)
temp_pcent = (autual_prop[,1]/sum(autual_prop[,1]))%>%
  round(digits = 3)
autual_pcent_mat = temp_pcent[,1]

#make a combination mat
comb_mat = 
  rbind(c(0.1,0.1,0.8),
        c(0.1,0.2,0.7),
        c(0.1,0.3,0.6),
        c(0.1,0.4,0.5),
        c(0.1,0.5,0.4),
        c(0.1,0.6,0.3),
        c(0.1,0.7,0.2),
        c(0.1,0.8,0.1),
        
        c(0.2,0.1,0.7),
        c(0.2,0.2,0.6),
        c(0.2,0.3,0.5),
        c(0.2,0.4,0.4),
        c(0.2,0.5,0.3),
        c(0.2,0.6,0.2),
        c(0.2,0.7,0.1),
        
        c(0.3,0.1,0.6),
        c(0.3,0.2,0.5),
        c(0.3,0.3,0.4),
        c(0.3,0.4,0.3),
        c(0.3,0.5,0.2),
        c(0.3,0.6,0.1),
        
        c(0.4,0.1,0.5),
        c(0.4,0.2,0.4),
        c(0.4,0.3,0.3),
        c(0.4,0.4,0.2),
        c(0.4,0.5,0.1),
        
        c(0.5,0.1,0.4),
        c(0.5,0.2,0.3),
        c(0.5,0.3,0.2),
        c(0.5,0.4,0.1),
        
        c(0.6,0.1,0.3),
        c(0.6,0.2,0.2),
        c(0.6,0.3,0.1),
        
        c(0.7,0.1,0.2),
        c(0.7,0.2,0.1),
        
        c(0.8,0.1,0.1))


#run

OA_prop_LST_rf = OA_prop_LST_svm = list()

for (iter in 1:25){
  
  RF_df<- rbind(Total_corn[[5]], Total_cotton[[5]],Total_rice[[5]])
  RF_df$label <- RF_df$label%>%as.factor()
  
  ind = sample(2, nrow(RF_df), replace=TRUE, prob=c(0.8,0.2))
  TRAIN = RF_df[ind==1,]
  TEST=RF_df[ind==2,]
  
   
  OA_prop_rf = OA_prop_svm = matrix(NA,36,6)
  
  for(k in 1:6){
    
    testing <- sapply(TEST[,1:(7*k)],as.numeric)%>%data.frame()
    testing$label = TEST$label
    
    for(i in 1:36){

      myTRAIN = proportion_data_flexible(TRAIN,k,
                                         comb_mat[i,1],comb_mat[i,2],comb_mat[i,3])
      
      OA_prop_rf[i,k] = ModelAccuracy("RF",myTRAIN,testing)
      OA_prop_svm[i,k] = ModelAccuracy("SVM",myTRAIN,testing)
    }
  }
  OA_prop_LST_rf[[iter]] = OA_prop_rf
  OA_prop_LST_svm[[iter]] = OA_prop_svm
  
  print(iter)
}

setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/")
save.image("Ternary_rf_svm_output.RData")