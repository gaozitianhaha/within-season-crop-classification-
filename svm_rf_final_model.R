# Develop a final model for prediction on unknown fields
# This script is written by Zitian Gao on 16 Jan 2023.

#load required libraries
library(dplyr)
library(e1071)
library(randomForest)
library(caret)

# load prepared Landsat 8 data at individual fields
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/paper2_DATA/")
load("paper2_data_Filt.RData")

# Tidy up data - w=year, 6 years (2013-2019)
data_pool = list()
for (w in 1:6){
  data_pool[[w]]<- rbind(Total_corn[[w]], Total_cotton[[w]],Total_rice[[w]])
}
data_pool_BareSoil =list()
for (w in 1:6){
  data_pool_BareSoil[[w]]<- Total_bareSoil[[w]]
}

# Count the number of each class
Num_of_crop <-matrix(NA,7,4)
for (w in 1:6){
  Num_of_crop[w,1] <- nrow(Total_corn[[w]])
  Num_of_crop[w,2] <- nrow(Total_cotton[[w]])
  Num_of_crop[w,3] <- nrow(Total_rice[[w]])
  Num_of_crop[w,4] <- nrow(Total_bareSoil[[w]]) 
}
Num_of_crop[7,1] <-sum(Num_of_crop[1:6,1])
Num_of_crop[7,2] <-sum(Num_of_crop[1:6,2])
Num_of_crop[7,3] <-sum(Num_of_crop[1:6,3])
Num_of_crop[7,4] <-sum(Num_of_crop[1:6,4])

ModelAccuracy = function(which_ml,T_Data,V_Data){
  # This is a helper function to calculate the OA, PA and UA
  # for ML and SVM, respectively.
  # select ML model
  if(which_ml=="RF"){
    model = randomForest(label~., data=T_Data, 
                         ntree=500, proximity=T,importance=TRUE)
  }
  if(which_ml=="SVM"){
    model = svm(label~ .,data = T_Data,
                type = 'C-classification',
                kernel = 'linear')
  }
  # prediction
  vali_prop = predict(model,V_Data)
  # validation confusion matrix
  vali_prop_CM = confusionMatrix(vali_prop,V_Data$label)$table
  
  #check the orders of corn, cotton and rice
  if(!identical(colnames(vali_prop_CM),c("CORN","COTTON","RICE","BareSoil"))){
    stop("The order of crop location is wrong. Refer to function ModelAccuracy.")
  }
  
  #accuracy metrics - OA
  OA_prop = sum(diag(vali_prop_CM))/ sum(vali_prop_CM)
  # UA/PA
  # the crop_loc indicates crop type:1=corn,2=cotton,3=rice, 4=Baresoil
  prop_UA = as.numeric(diag(vali_prop_CM) / rowSums(vali_prop_CM) * 100)
  prop_PA = as.numeric(diag(vali_prop_CM) / colSums(vali_prop_CM) * 100)
  names(prop_UA) = names(prop_PA) = c("CORN","COTTON","RICE","BareSoil")
  #save the results in a list
  RE = list(model, OA_prop,prop_UA,prop_PA)
  names(RE) = c("model","OA","UA","PA")
  return(RE)
}

#first prepare data for each year using dynamic proportions of crop classes
Final_model_train_data = Final_model_vali_data = list()
for(iter in 1:25){
  tmp_train = tmp_test = list()
  for (w in 1:6){
    remain_w <- setdiff(rep(1:6),w)
    
    YEAR_1 = data_pool[[remain_w[1]]]
    YEAR_2 = data_pool[[remain_w[2]]]
    YEAR_3 = data_pool[[remain_w[3]]]
    YEAR_4 = data_pool[[remain_w[4]]]
    YEAR_5 = data_pool[[remain_w[5]]]
    
    DATA<-rbind(YEAR_1,YEAR_2,YEAR_3,YEAR_4,YEAR_5)  
    COMB_CORN = DATA%>%dplyr::filter(label =="CORN")
    COMB_COTTON = DATA%>%dplyr::filter(label =="COTTON")
    COMB_RICE = DATA%>%dplyr::filter(label =="RICE")
    
    #BARESOIL
    BS_1 = data_pool_BareSoil[[remain_w[1]]]
    BS_2 = data_pool_BareSoil[[remain_w[2]]]
    BS_3 = data_pool_BareSoil[[remain_w[3]]]
    BS_4 = data_pool_BareSoil[[remain_w[4]]]
    BS_5 = data_pool_BareSoil[[remain_w[5]]]
    
    COMB_BareSoil = rbind(BS_1,BS_2,BS_3,BS_4,BS_5)
    
    # make sure the minimum number is >200
    Num_of_crop_trim = Num_of_crop[1:6,1:3] #6 years, 3 crop types
    prop_crop = (Num_of_crop_trim[w,]/sum(Num_of_crop_trim[w,]))
    
    min_loc = which(Num_of_crop_trim[w,] == min(Num_of_crop_trim[w,]))
    
    if(min_loc ==1){ # corn is the minimum number
      cotton_number = min(nrow(COMB_CORN),250*prop_crop[2]/prop_crop[1])
      rice_number = min(nrow(COMB_RICE),250*prop_crop[3]/prop_crop[1])
      baresoil_number = (cotton_number + rice_number + 250)*0.2/0.8  
      new_df<-rbind(COMB_CORN[sample(1:nrow(COMB_CORN),250),],
                    COMB_COTTON[sample(1:nrow(COMB_COTTON),cotton_number),],
                    COMB_RICE[sample(1:nrow(COMB_RICE),rice_number),],
                    COMB_BareSoil[sample(1:nrow(COMB_BareSoil),baresoil_number),])
      
    }else if(min_loc ==2){# cotton is the minimum number
      corn_number = min(nrow(COMB_CORN),250*prop_crop[1]/prop_crop[2])
      rice_number = min(nrow(COMB_RICE),250*prop_crop[3]/prop_crop[2])
      baresoil_number = (corn_number + rice_number + 250)*0.2/0.8
      new_df<-rbind(COMB_CORN[sample(1:nrow(COMB_CORN),corn_number),],
                    COMB_COTTON[sample(1:nrow(COMB_COTTON),250),],
                    COMB_RICE[sample(1:nrow(COMB_RICE),rice_number),],
                    COMB_BareSoil[sample(1:nrow(COMB_BareSoil),baresoil_number),])
      
    }else{ # rice is the minimum number
      corn_number = min(nrow(COMB_CORN),250*prop_crop[1]/prop_crop[3])
      cotton_number = min(nrow(COMB_COTTON),250*prop_crop[2]/prop_crop[3])
      baresoil_number = (corn_number + cotton_number + 250)*0.2/0.8  
      new_df<-rbind(COMB_CORN[sample(1:nrow(COMB_CORN),corn_number),],
                    COMB_COTTON[sample(1:nrow(COMB_COTTON),cotton_number),],
                    COMB_RICE[sample(1:nrow(COMB_RICE),250),],
                    COMB_BareSoil[sample(1:nrow(COMB_BareSoil),baresoil_number),])
    }
    # divide into training and validation
    tmp_train[[w]] = new_df
    #should validate on the classification year 27 Feb
    n_vali_bs = round(nrow(data_pool[[w]])*0.2)
    tmp_test[[w]] = rbind(data_pool[[w]],
                          data_pool_BareSoil[[w]][sample(1:nrow(data_pool_BareSoil[[w]]),
                                                         n_vali_bs),])
                    
  }
  Final_model_train_data[[iter]] = tmp_train
  
  
  Final_model_vali_data[[iter]] = tmp_test
  print(iter)
}


# calculate the final proportions in the validation data
# it is worth to know that in the final split into train and validation, 
# the proportions of classes might be slightly change 
# (e.g. bare soil may have a slightly >20% due to randomness)

#final model
#rf
OA_V_final_lst_rf = 
  Final_corn_UA_V_lst_rf = 
  Final_cotton_UA_V_lst_rf = 
  Final_rice_UA_V_lst_rf = 
  Final_baresoil_UA_V_lst_rf = list()

#svm
OA_V_final_lst_svm = 
  Final_corn_UA_V_lst_svm = 
  Final_cotton_UA_V_lst_svm = 
  Final_rice_UA_V_lst_svm = 
  Final_baresoil_UA_V_lst_svm = list()

for(iter in 1:25){
  OA_V_final_rf = OA_V_final_svm = matrix(NA,6,6) 
    
  Final_corn_UA_V_rf = Final_cotton_UA_V_rf = 
  Final_rice_UA_V_rf = Final_baresoil_UA_V_rf = matrix(NA,6,6)
  
  Final_corn_UA_V_svm = Final_cotton_UA_V_svm = 
  Final_rice_UA_V_svm = Final_baresoil_UA_V_svm = matrix(NA,6,6)
  
  for (w in 1:6){

    for(k in 1:6){
      # Training data
      training_data = sapply(Final_model_train_data[[iter]][[w]][,1:(7*k)],as.numeric)%>%
        data.frame()
      training_data$label = Final_model_train_data[[iter]][[w]]$label
      # Validation data
      vali_data = sapply(Final_model_vali_data[[iter]][[w]][,1:(7*k)],as.numeric)%>%
        data.frame() 
      vali_data$label = Final_model_vali_data[[iter]][[w]]$label
      
      #run the RF model
      RF_result = ModelAccuracy("RF",training_data,vali_data)
      OA_V_final_rf[w,k] = RF_result$OA
      Final_corn_UA_V_rf[w,k] = RF_result$UA[1]
      Final_cotton_UA_V_rf[w,k] = RF_result$UA[2]
      Final_rice_UA_V_rf[w,k] = RF_result$UA[3]
      Final_baresoil_UA_V_rf[w,k] = RF_result$UA[4]
      
      #run the SVM model
      SVM_result = ModelAccuracy("SVM",training_data,vali_data)
      OA_V_final_svm[w,k] = SVM_result$OA
      Final_corn_UA_V_svm[w,k] = SVM_result$UA[1]
      Final_cotton_UA_V_svm[w,k] = SVM_result$UA[2]
      Final_rice_UA_V_svm[w,k] = SVM_result$UA[3]
      Final_baresoil_UA_V_svm[w,k] = SVM_result$UA[4]
    }
    print(paste0("Year = ",w,"is completed!"))
  }
  # RF model results
  OA_V_final_lst_rf[[iter]] = OA_V_final_rf
  
  Final_corn_UA_V_lst_rf[[iter]] =  Final_corn_UA_V_rf
  Final_cotton_UA_V_lst_rf[[iter]] = Final_cotton_UA_V_rf
  Final_rice_UA_V_lst_rf[[iter]] = Final_rice_UA_V_rf
  Final_baresoil_UA_V_lst_rf[[iter]] = Final_baresoil_UA_V_rf
  
  
  # SVM model results
  OA_V_final_lst_svm[[iter]] = OA_V_final_svm
  
  Final_corn_UA_V_lst_svm[[iter]] =  Final_corn_UA_V_svm
  Final_cotton_UA_V_lst_svm[[iter]] = Final_cotton_UA_V_svm
  Final_rice_UA_V_lst_svm[[iter]] = Final_rice_UA_V_svm
  Final_baresoil_UA_V_lst_svm[[iter]] = Final_baresoil_UA_V_svm
  
  print(paste0("iter=",iter))
}

# save final model
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/")
save(list=c("OA_V_final_lst_rf",
            "Final_corn_UA_V_lst_rf",
            "Final_cotton_UA_V_lst_rf",
            "Final_rice_UA_V_lst_rf",
            "Final_baresoil_UA_V_lst_rf",
            
            "OA_V_final_lst_svm",
            "Final_corn_UA_V_lst_svm",
            "Final_cotton_UA_V_lst_svm",
            "Final_rice_UA_V_lst_svm",
            "Final_baresoil_UA_V_lst_svm"),
     
     file="FINAL_MODEL_svm_rf_output.RData")
