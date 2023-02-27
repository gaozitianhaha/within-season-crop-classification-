# This is Test 4 to do LOOCV
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

ModelAccuracy_test4 = function(which_ml,T_Data,V_Data){
  # This is a helper function to calculate the validation OA
  # Select ML model
  if(which_ml=="RF"){
    model = randomForest(label~., data=T_Data, 
                         ntree=500, proximity=T,importance=TRUE)
  }
  if(which_ml=="SVM"){
    model = svm(label~ .,data = T_Data,
                type = 'C-classification',
                kernel = 'linear')
  }
  
  vali_prop = predict(model,V_Data)
  vali_prop_CM = confusionMatrix(vali_prop,V_Data$label)$table
  
  #accuracy metrics - OA
  OA_prop = sum(diag(vali_prop_CM))/ sum(vali_prop_CM)
  
  return(OA_prop)
}


LOOCV <-function(w,num_to_trim){
  # this function will collect the same amount of same number from other years
  remain_w <- setdiff(rep(1:6),w)
  
  remain_DATA<-rbind(data_pool[[remain_w[1]]],
                     data_pool[[remain_w[2]]],
                     data_pool[[remain_w[3]]],
                     data_pool[[remain_w[4]]],
                     data_pool[[remain_w[5]]]) 
  
  COMB_CORN = remain_DATA%>%dplyr::filter(label =="CORN")
  COMB_COTTON = remain_DATA%>%dplyr::filter(label =="COTTON")
  COMB_RICE = remain_DATA%>%dplyr::filter(label =="RICE")
  
  # make the sample number the same as the "sample from the tested year"
  new_df<-
    rbind(COMB_CORN[sample(1:nrow(COMB_CORN),num_to_trim[1]),],
          COMB_COTTON[sample(1:nrow(COMB_COTTON),num_to_trim[2]),],
          COMB_RICE[sample(1:nrow(COMB_RICE),num_to_trim[3]),])
  
  return(new_df)
}

YEAR_NAME<-c("13","14","15","16","17","18","19")
#rf
OA_T_new_lst_rf = OA_T_lst_rf = OA_V_new_lst_rf = OA_V_lst_rf = list()
#svm
OA_T_new_lst_svm = OA_T_lst_svm = OA_V_new_lst_svm = OA_V_lst_svm = list()

for(iter in 1:25){
  #RF
  OA_v_new_rf = matrix(NA,6,6)
  OA_v_rf = matrix(NA,6,6)
  
  #SVM
  OA_v_new_svm = matrix(NA,6,6)
  OA_v_svm = matrix(NA,6,6)
  
  # w=1:6 for 6 years
  for (w in 1:6){
    RF_df<- rbind(Total_corn[[w]],Total_cotton[[w]],Total_rice[[w]])
    RF_df$label <- RF_df$label%>%as.factor()
    
    # k=1:6 for 6 months
    for(k in 1:6){
      DATA_thisYr = sapply(RF_df[,1:(7*k)],as.numeric)%>%data.frame()
      DATA_thisYr$label = RF_df$label
      
      ind = sample(2, nrow(DATA_thisYr), replace=TRUE, prob=c(0.8,0.2))
      training = DATA_thisYr[ind==1,]
      testing = DATA_thisYr[ind==2,] 
      
      OA_v_rf[w,k] = ModelAccuracy_test4("RF",training,testing)
      OA_v_svm[w,k] = ModelAccuracy_test4("SVM",training,testing)
      
      # validation -independent-LOOCV data
      tmp = training$label %>%summary()
      if(identical(names(tmp),c("CORN","COTTON","RICE"))){
        num_to_trim = tmp%>%as.numeric()
      }
      # prepare LOOCV training data
      new_RF_df<-LOOCV(w,num_to_trim)
      
      #trimmed to a particular month
      new_data = sapply(new_RF_df[,1:(7*k)],as.numeric)%>%data.frame()
      new_data$label = new_RF_df$label
      
      OA_v_new_rf[w,k] = ModelAccuracy_test4("RF",new_data,testing)
      OA_v_new_svm[w,k] = ModelAccuracy_test4("SVM",new_data,testing)
    }
    print(w)
  }
  
  # OA - rf
  OA_V_new_lst_rf[[iter]] = OA_v_new_rf
  OA_V_lst_rf[[iter]] = OA_v_rf
  
  # OA - svm
  OA_V_new_lst_svm[[iter]] = OA_v_new_svm
  OA_V_lst_svm[[iter]] = OA_v_svm
  
  print(iter)
}

#save 
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/")
save.image("Test4_rf_svm_output.RData")
