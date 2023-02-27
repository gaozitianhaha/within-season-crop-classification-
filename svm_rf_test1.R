# This is Test 1 to evaluate the influence of sample size on early-season crop
# classification 
# The script written and modified by Zitian Gao on 16 Jan 2023

#load required libraries
library(dplyr)
library(e1071)
library(randomForest)
library(caret)

#load prepared Landsat 8 data at individual fields
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/paper2_DATA/")
load("paper2_data_Filt.RData")

#tidy up data - w=year, 6 years (2013-2019)
data_pool = list()
for (w in 1:6){
  data_pool[[w]]<- rbind(Total_corn[[w]], Total_cotton[[w]],Total_rice[[w]])
}
data_pool_BareSoil =list()
for (w in 1:6){
  data_pool_BareSoil[[w]]<- Total_bareSoil[[w]]
}


SVM_model <-function(train_dt,vali_dt){
  # this is a function to calculate validation OA using SVM
  # given a certain training and validation sets
  # k=month, 6 months
  OA_V = rep(NA,6)
  for(k in 1:6){
    #training
    TRAIN<- sapply(train_dt[,1:(7*k)],as.numeric)%>%data.frame()
    TRAIN$label = train_dt$label
    
    svm_model = svm(label~ .,
                    data = TRAIN,
                    type = 'C-classification',
                    kernal = "linear")
    
    # prediction
    vali <- predict(svm_model,vali_dt)
    # validation confusion matrix
    vali_CM = confusionMatrix(vali,vali_dt$label)$table
    
    OA_V[k]<-(sum(diag(vali_CM)) / sum(vali_CM)) * 100
  }
  
  return(OA_V)
}

RF_model <-function(train_dt,vali_dt){
  # this is a function to calculate validation OA using RF
  # given a certain training and validation sets
  # k=month, 6 months
  OA_V = rep(NA,6)
  for(k in 1:6){
    #training
    TRAIN = sapply(train_dt[,1:(7*k)],as.numeric)%>%data.frame()
    TRAIN$label = train_dt$label
    
    rf_model = randomForest(label~., data=TRAIN, 
                           ntree=500, proximity=T,importance=TRUE)
  
    # validation
    vali = predict(rf_model,vali_dt)
    # validation confusion matrix
    vali_CM = confusionMatrix(vali,vali_dt$label)$table
    
    OA_V[k]<-(sum(diag(vali_CM)) / sum(vali_CM)) * 100
  }
  return(OA_V)
}

Pcent_of_train_dt=
  function(pcent, train_corn, train_cotton,train_rice){
    #this is a helper function to subset samples based on the required percentage
    #subset data
    corn_dt = train_corn[sample(1:nrow(train_corn),nrow(train_corn)*pcent),]
    cotton_dt = train_cotton[sample(1:nrow(train_cotton),nrow(train_cotton)*pcent),]
    rice_dt = train_rice[sample(1:nrow(train_rice),nrow(train_rice)*pcent),]
    
    #combine
    final_df = rbind(corn_dt,cotton_dt,rice_dt)%>%as.data.frame()
    final_df$label = as.factor(final_df$label)
    return(final_df)
  }


# test 1 - influence of sample size
# use 2017-2018 as an example so w=5
OA_V_SS_lst_rf = OA_V_SS_lst_svm<-list() #e.g. OA_validation_samplesize_list_rf"

#count sample number
Count_trainSam_number = matrix(NA,10,3)
Count_valiSam_number = rep(NA,3)

for(iter in 1:25){
  
  OA_V_sampleSize_rf = OA_V_sampleSize_svm = matrix(NA,10,6)
  
  count=1
  
  #2017-2018
  train_corn = Total_corn[[5]]
  train_cotton = Total_cotton[[5]]
  train_rice = Total_rice[[5]]
  
  #80% training samples from 2017-18 for training and the rest for validation
  CORN_LOC = sample(1:nrow(train_corn),0.2*nrow(train_corn))
  sub_corn_T = train_corn[-CORN_LOC,]
  sub_corn_V = train_corn[ CORN_LOC,]
  
  COTTON_LOC = sample(1:nrow(train_cotton),0.2*nrow(train_cotton))
  sub_cotton_T = train_cotton[- COTTON_LOC ,]
  sub_cotton_V = train_cotton[ COTTON_LOC ,]
  
  RICE_LOC = sample(1:nrow(train_rice),0.2*nrow(train_rice))
  sub_rice_T = train_rice[- RICE_LOC,]
  sub_rice_V = train_rice[  RICE_LOC,]
  
  #validation data.frame
  vali_dt = rbind(sub_corn_V,sub_cotton_V, sub_rice_V)%>%
    as.data.frame()%>%
    mutate(label = as.factor(label))
  
  Count_valiSam_number =  c(which(vali_dt$label =="CORN")%>%length(),
                            which(vali_dt$label =="COTTON")%>%length(),
                            which(vali_dt$label =="RICE")%>%length())
  
  for(i in seq(0.1,1,0.1)){
    #split training samples (80% out of the total 2017-18 samples)
    train_dt = Pcent_of_train_dt(i,sub_corn_T , sub_cotton_T,sub_rice_T)
    
    Count_trainSam_number[count,] = 
      c(which(train_dt$label =="CORN")%>%length(),
        which(train_dt$label =="COTTON")%>%length(),
        which(train_dt$label =="RICE")%>%length())
    
    #validation kappa
    rf_result_svm = SVM_model(train_dt,vali_dt)
    rf_result_rf = RF_model(train_dt,vali_dt)
    
    OA_V_sampleSize_svm[count,] = rf_result_svm #validation accuracy
    OA_V_sampleSize_rf[count,] = rf_result_rf
    
    count = count+1
  }
  
  OA_V_SS_lst_rf[[iter]] = OA_V_sampleSize_rf
  OA_V_SS_lst_svm[[iter]] = OA_V_sampleSize_svm
  
  print(iter)
}

#save data
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2")
save.image("Test1_rf_svm_output.RData")

