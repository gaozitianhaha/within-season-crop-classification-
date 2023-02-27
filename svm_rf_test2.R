# This is Test 1 to evaluate the influence of proportions of crop classes
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

ModelAccuracy = function(which_ml,T_Data,V_Data,crop_loc){
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
  if(!identical(colnames(vali_prop_CM),c("CORN","COTTON","RICE"))){
    stop("The order of crop location is wrong. Refer to function ModelAccuracy.")
  }
  
  #accuracy metrics
  #OA
  OA_prop = sum(diag(vali_prop_CM))/ sum(vali_prop_CM)
  # UA/PA
  # the crop_loc indicates crop type:1=corn,2=cotton,3=rice
  prop_UA = as.numeric(diag(vali_prop_CM) / 
                         rowSums(vali_prop_CM) * 100)[crop_loc] 
  prop_PA = as.numeric(diag(vali_prop_CM) / 
                         colSums(vali_prop_CM) * 100)[crop_loc] 
  
  #save the results in a list
  RE = list(OA_prop,prop_UA,prop_PA)
  names(RE) = c("OA","UA","PA")
  return(RE)
}

proportion_data = function(TRAIN,which_month,croptype,the_prop){
  # this is a helper function to trim data based on the certain proportions
  # croptype means which crop you would like to get a fixed proportion
  # trim to month
  training<- sapply(TRAIN[,1:(7*which_month)],as.numeric)%>%data.frame()
  training$label = TRAIN$label
  
  # count the fewest sample size -  the fewest number is slightly different
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
  
  if(croptype =="CORN"){
    TRAIN_df = rbind(dt_corn_filt[sample(1:nrow(dt_corn_filt),
                                         fewest_num*the_prop),],
                     dt_cotton_filt[sample(1:nrow(dt_cotton_filt),
                                           fewest_num/2*(1-the_prop)),],
                     dt_rice_filt[sample(1:nrow(dt_rice_filt),
                                         fewest_num/2*(1-the_prop)),])
    
  }else if(croptype =="COTTON"){
    TRAIN_df = rbind(dt_cotton_filt[sample(1:nrow(dt_cotton_filt),
                                           fewest_num*the_prop),],
                     dt_corn_filt[sample(1:nrow(dt_corn_filt),
                                         fewest_num/2*(1-the_prop)),],
                     dt_rice_filt[sample(1:nrow(dt_rice_filt),
                                         fewest_num/2*(1-the_prop)),])
  }else{
    TRAIN_df = rbind(dt_rice_filt[sample(1:nrow(dt_rice_filt),
                                         fewest_num*the_prop),],
                     dt_cotton_filt[sample(1:nrow(dt_cotton_filt),
                                           fewest_num/2*(1-the_prop)),],
                     dt_corn_filt[sample(1:nrow(dt_corn_filt),
                                         fewest_num/2*(1-the_prop)),])
  }
  return(TRAIN_df)
}

# test 2 - influence of crop classes proportions
# calculate the actual proportion from survey
# using 2017 - 2018 data 
RF_df = rbind(Total_corn[[5]], Total_cotton[[5]],Total_rice[[5]])

autual_prop = 
  RF_df%>%
  group_by(label)%>%
  dplyr::summarise(n = n())%>%
  dplyr::select(n)

temp_pcent = (autual_prop[,1]/sum(autual_prop[,1]))%>%round(digits = 3)
autual_pcent_mat = temp_pcent[,1]

# empty lists
#"OA_prop_LST_CORN" means the overall accuracy of model when crop proportion is fixed
corn_prop_UA_LST_rf = corn_prop_PA_LST_rf = 
  cotton_prop_UA_LST_rf = cotton_prop_PA_LST_rf =
  rice_prop_UA_LST_rf = rice_prop_PA_LST_rf = 
  OA_prop_LST_CORN_rf = OA_prop_LST_COTTON_rf = OA_prop_LST_RICE_rf =list()

corn_prop_UA_LST_svm = corn_prop_PA_LST_svm = 
  cotton_prop_UA_LST_svm = cotton_prop_PA_LST_svm =
  rice_prop_UA_LST_svm = rice_prop_PA_LST_svm = 
  OA_prop_LST_CORN_svm = OA_prop_LST_COTTON_svm = OA_prop_LST_RICE_svm =list()

for (iter in 1:25){
  
  RF_df = rbind(Total_corn[[5]], Total_cotton[[5]],Total_rice[[5]])
  RF_df$label = RF_df$label%>%as.factor()
  
  ind = sample(2, nrow(RF_df), replace=TRUE, prob=c(0.8,0.2))
  TRAIN = RF_df[ind==1,]
  TEST=RF_df[ind==2,]
  
  #9 scenarios, 6 months
  corn_prop_UA_rf= corn_prop_PA_rf = 
    cotton_prop_UA_rf =cotton_prop_PA_rf =
    rice_prop_UA_rf = rice_prop_PA_rf =  
    OA_prop_CORN_rf = OA_prop_COTTON_rf = OA_prop_RICE_rf = matrix(NA,9,6) 
  
  corn_prop_UA_svm = corn_prop_PA_svm = 
    cotton_prop_UA_svm =cotton_prop_PA_svm =
    rice_prop_UA_svm = rice_prop_PA_svm =  
    OA_prop_CORN_svm = OA_prop_COTTON_svm = OA_prop_RICE_svm = matrix(NA,9,6)
  
  pcent = seq(0.1,0.9,0.1)
  
  for(k in 1:6){
    #validation data
    testing <- sapply(TEST[,1:(7*k)],as.numeric)%>%data.frame()
    testing$label = TEST$label
    
    for(i in 1:9){
      # corn
      CORN_TRAIN = proportion_data(TRAIN,k,"CORN",pcent[i])
      #rf
      RF_CORN = ModelAccuracy("RF",CORN_TRAIN,testing,1)
      OA_prop_CORN_rf[i,k] = RF_CORN$OA
      corn_prop_UA_rf[i,k] = RF_CORN$UA
      corn_prop_PA_rf[i,k] = RF_CORN$PA
      #svm
      SVM_CORN = ModelAccuracy("SVM",CORN_TRAIN,testing,1)
      OA_prop_CORN_svm[i,k] = SVM_CORN$OA
      corn_prop_UA_svm[i,k] = SVM_CORN$UA
      corn_prop_PA_svm[i,k] = SVM_CORN$PA 
        
      # cotton
      COTTON_TRAIN = proportion_data(TRAIN,k,"COTTON",pcent[i])
      #rf
      RF_COTTON = ModelAccuracy("RF",COTTON_TRAIN,testing,2)
      OA_prop_COTTON_rf[i,k] = RF_COTTON$OA
      cotton_prop_UA_rf[i,k] = RF_COTTON$UA
      cotton_prop_PA_rf[i,k] = RF_COTTON$PA
      #svm
      SVM_COTTON = ModelAccuracy("SVM",COTTON_TRAIN,testing,2)
      OA_prop_COTTON_svm[i,k] = SVM_COTTON$OA
      cotton_prop_UA_svm[i,k] = SVM_COTTON$UA
      cotton_prop_PA_svm[i,k] = SVM_COTTON$PA 
      
      # rice
      RICE_TRAIN = proportion_data(TRAIN,k,"RICE",pcent[i])
      #rf
      RF_RICE = ModelAccuracy("RF",RICE_TRAIN,testing,3)
      OA_prop_RICE_rf[i,k] = RF_RICE$OA
      rice_prop_UA_rf[i,k] = RF_RICE$UA
      rice_prop_PA_rf[i,k] = RF_RICE$PA
      #svm
      SVM_RICE = ModelAccuracy("SVM",RICE_TRAIN,testing,3)
      OA_prop_RICE_svm[i,k] = SVM_RICE$OA
      rice_prop_UA_svm[i,k] = SVM_RICE$UA
      rice_prop_PA_svm[i,k] = SVM_RICE$PA      
    }
  }
  #RF
  OA_prop_LST_CORN_rf[[iter]] = OA_prop_CORN_rf
  OA_prop_LST_COTTON_rf[[iter]] = OA_prop_COTTON_rf
  OA_prop_LST_RICE_rf[[iter]] = OA_prop_RICE_rf
  
  corn_prop_UA_LST_rf[[iter]]=corn_prop_UA_rf
  corn_prop_PA_LST_rf[[iter]]=corn_prop_PA_rf
  cotton_prop_UA_LST_rf[[iter]]=cotton_prop_UA_rf
  cotton_prop_PA_LST_rf[[iter]]=cotton_prop_PA_rf
  rice_prop_UA_LST_rf[[iter]]=rice_prop_UA_rf
  rice_prop_PA_LST_rf[[iter]]=rice_prop_PA_rf
  
  #SVM
  OA_prop_LST_CORN_svm[[iter]] = OA_prop_CORN_svm
  OA_prop_LST_COTTON_svm[[iter]] = OA_prop_COTTON_svm
  OA_prop_LST_RICE_svm[[iter]] = OA_prop_RICE_svm
  
  corn_prop_UA_LST_svm[[iter]]=corn_prop_UA_svm
  corn_prop_PA_LST_svm[[iter]]=corn_prop_PA_svm
  cotton_prop_UA_LST_svm[[iter]]=cotton_prop_UA_svm
  cotton_prop_PA_LST_svm[[iter]]=cotton_prop_PA_svm
  rice_prop_UA_LST_svm[[iter]]=rice_prop_UA_svm
  rice_prop_PA_LST_svm[[iter]]=rice_prop_PA_svm
  
  print(iter)
}

#save data
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/")
save.image("Test2_rf_svm_output.RData")