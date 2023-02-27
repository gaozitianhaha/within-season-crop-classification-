# This is Test 3 to evaluate the influence of inclusion of non-crop class
# The script written and modified by Zitian Gao on 16 Jan 2023

#load required libraries
library(randomForest)
library(dplyr)
library(caret)
library(e1071)

#load prepared Landsat 8 data at individual fields
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/paper2_DATA/")
load("paper2_data_Filt.RData")


# using 2017 - 2018 data 
RF_df = rbind(Total_corn[[5]], Total_cotton[[5]],Total_rice[[5]])
# calculate the actual proportion from survey
autual_prop = RF_df%>%
  group_by(label)%>%
  dplyr::summarise(n = n())%>%
  dplyr::select(n)
temp_pcent = (autual_prop[,1]/sum(autual_prop[,1]))%>%
  round(digits = 3)
autual_pcent_mat = temp_pcent[,1]

Prop_helper = function(training_common, diff_num_per_class){
  # this is a helper function to make the sample size for
  # 3-class and 4-class models identical.
  TEMP_CORN = training_common%>%dplyr::filter(label =="CORN")
  TEMP_CORN_trim = 
    TEMP_CORN[sample(1:nrow(TEMP_CORN),
                     (nrow(TEMP_CORN)-diff_num_per_class[1])),]
  
  TEMP_COTTON = training_common%>%dplyr::filter(label =="COTTON")
  TEMP_COTTON_trim = 
    TEMP_COTTON[sample(1:nrow(TEMP_COTTON),
                       (nrow(TEMP_COTTON) - diff_num_per_class[2])),]
  
  TEMP_RICE = training_common%>%dplyr::filter(label =="RICE")
  TEMP_RICE_trim = 
    TEMP_RICE[sample(1:nrow(TEMP_RICE),
                     (nrow(TEMP_RICE) - diff_num_per_class[3])),]
  
  TEMP_BARESOIL = training_common%>%dplyr::filter(label =="BareSoil")
  
  final_df = rbind(TEMP_CORN_trim,
                   TEMP_COTTON_trim,
                   TEMP_RICE_trim,
                   TEMP_BARESOIL)
  return(final_df)
}

#run
ModelAccuracy_test3 = function(which_ml,T_Data,V_Data,num_class){
  #select ML
  if(which_ml=="RF"){
    model = randomForest(label~., data=T_Data, 
                         ntree=500, proximity=T,importance=TRUE)
  }
  if(which_ml=="SVM"){
    model =   svm(label~ .,data = T_Data,
                  type = 'C-classification',
                  kernel = 'linear')
  }
  
  #prediction
  vali <- predict(model, V_Data)
  vali_CM = confusionMatrix(vali,V_Data$label)$table
  
  #cheack if the order is correct
  if(num_class==3){
    if(FALSE %in% (colnames(vali_CM)==c("CORN","COTTON","RICE"))){
      stop("error in CM level order")
    }
  }
  if(num_class==4){
    if(FALSE %in% 
       (colnames(vali_CM)==c("CORN","COTTON","RICE","BareSoil"))){
      stop("error in CM level order")
    }
  }
  UA_V = as.numeric(diag(vali_CM) / rowSums(vali_CM) * 100)
  PA_V = as.numeric(diag(vali_CM) / colSums(vali_CM) * 100)
  
  RE = list(UA_V,PA_V)
  names(RE) = c("UA","PA")
  
  return(RE)
}

TEST3_NUM_CLASS = function(baresoil_num){
  
  PA_4Class_V_lst_rf = UA_4Class_V_lst_rf = 
  PA_3Class_V_lst_rf = UA_3Class_V_lst_rf = 
  PA_4Class_V_lst_svm = UA_4Class_V_lst_svm = 
  PA_3Class_V_lst_svm = UA_3Class_V_lst_svm = list()
  
  count_matrix = matrix(NA,2,8)
  
  for (iter in 1:25){
    
    PA_4Class_V_rf = UA_4Class_V_rf = matrix(NA,4,6)
    PA_3Class_V_rf = UA_3Class_V_rf = matrix(NA,3,6)
    
    PA_4Class_V_svm = UA_4Class_V_svm = matrix(NA,4,6)
    PA_3Class_V_svm = UA_3Class_V_svm = matrix(NA,3,6)
    
    #4 class model
    RF_df<- 
      rbind(Total_corn[[5]], 
            Total_cotton[[5]],
            Total_rice[[5]],
            Total_bareSoil[[5]][sample(1:nrow(Total_bareSoil[[5]]),baresoil_num),])
    
    RF_df$label <- RF_df$label%>%as.factor()
    
    ind = sample(2, nrow(RF_df), replace=TRUE, prob=c(0.8,0.2))
    TRAIN = RF_df[ind==1,]
    TEST = RF_df[ind==2,]
    
    for(k in 1:6){
      #COMMON 3 & 4 class
      training_common = sapply(TRAIN[,1:(7*k)],as.numeric)%>%data.frame()
      training_common$label = TRAIN$label
      
      testing_common = sapply(TEST[,1:(7*k)],as.numeric)%>%data.frame()
      testing_common$label = TEST$label
      
      #3 class training 
      # remove non-crop class
      training_3CLASS = training_common%>%dplyr::filter(label!="BareSoil")
      #remove the empty class "baresoil"
      training_3CLASS$label = factor(training_3CLASS$label)
      
      #remove non-crop class from validation data
      testing_3CLASS  = testing_common%>%dplyr::filter(label!="BareSoil")
      testing_3CLASS$label = factor(testing_3CLASS$label)

      RE_3class_RF = ModelAccuracy_test3("RF",training_3CLASS,testing_3CLASS,3) #3=num of class

      RE_3class_SVM = ModelAccuracy_test3("SVM",training_3CLASS,testing_3CLASS,3)
      
      UA_3Class_V_rf[,k] = RE_3class_RF$UA
      PA_3Class_V_rf[,k] = RE_3class_RF$PA
      UA_3Class_V_svm[,k] = RE_3class_SVM$UA
      PA_3Class_V_svm[,k] = RE_3class_SVM$PA
      
      # count the sample size in training_3CLASS
      num_3class_T = nrow(training_3CLASS)
      num_3class_V = nrow(testing_3CLASS)
      
      # difference sample numbers between 3 and 4-class for training
      diff_num_per_class_T = round((nrow(training_common) - num_3class_T)*autual_pcent_mat)
      # difference sample numbers between 3 and 4-class for testing
      diff_num_per_class_V = round((nrow(testing_common) - num_3class_V)*autual_pcent_mat)
      
      #4 class training 
      #trim 4-class training data to the same size of 3-class training data
      training_4CLASS =  Prop_helper(training_common, diff_num_per_class_T)
      testing_4CLASS =  Prop_helper(testing_common, diff_num_per_class_V)
      
      #run the model for 4 class
      RE_4class_RF = ModelAccuracy_test3("RF",training_4CLASS,testing_4CLASS,4)
      RE_4class_SVM = ModelAccuracy_test3("SVM",training_4CLASS,testing_4CLASS,4)
      
      UA_4Class_V_rf[,k] = RE_4class_RF$UA
      PA_4Class_V_rf[,k] = RE_4class_RF$PA
      UA_4Class_V_svm[,k] = RE_4class_SVM$UA
      PA_4Class_V_svm[,k] = RE_4class_SVM$PA
      
      #COUNT NUMBER
      train_4class_N = 
        training_4CLASS%>%
        group_by(label)%>% 
        dplyr::summarise(n = n())
      
      train_3class_N = 
        training_3CLASS%>%
        group_by(label)%>% 
        dplyr::summarise(n = n())
      
      vali_4class_N = 
        testing_4CLASS%>%
        group_by(label)%>% 
        dplyr::summarise(n = n())
      
      vali_3class_N = 
        testing_3CLASS%>%
        group_by(label)%>% 
        dplyr::summarise(n = n())
      
      count_matrix[1,1:3]<-
        paste0(train_3class_N$n,"(",(train_3class_N$n / 
                                       sum(train_3class_N$n))%>%
                 round(digits=2)*100,"%",")")
      count_matrix[2,1:4]<-
        paste0(train_4class_N$n,"(",(train_4class_N$n / 
                                       sum(train_4class_N$n))%>%
                 round(digits=2)*100,"%",")")
      
      count_matrix[1,5:7]<- 
        paste0(vali_3class_N$n,"(",(vali_3class_N$n / 
                                      sum(vali_3class_N$n))%>%
                 round(digits=2)*100,"%",")")
      count_matrix[2,5:8]<- 
        paste0(vali_4class_N$n,"(",(vali_4class_N$n / 
                                      sum(vali_4class_N$n))%>%
                 round(digits=2)*100,"%",")")
    }
    
    #rf
    PA_4Class_V_lst_rf[[iter]] = PA_4Class_V_rf
    UA_4Class_V_lst_rf[[iter]] = UA_4Class_V_rf
    
    PA_3Class_V_lst_rf[[iter]] = PA_3Class_V_rf
    UA_3Class_V_lst_rf[[iter]] = UA_3Class_V_rf
    
    #svm
    PA_4Class_V_lst_svm[[iter]] = PA_4Class_V_svm
    UA_4Class_V_lst_svm[[iter]] = UA_4Class_V_svm
    
    PA_3Class_V_lst_svm[[iter]] = PA_3Class_V_svm
    UA_3Class_V_lst_svm[[iter]] = UA_3Class_V_svm
    
    print(iter)
  }
  return(list(PA_4Class_V_lst_rf,
              UA_4Class_V_lst_rf,
              PA_3Class_V_lst_rf,
              UA_3Class_V_lst_rf,
              
              PA_4Class_V_lst_svm,
              UA_4Class_V_lst_svm,
              PA_3Class_V_lst_svm,
              UA_3Class_V_lst_svm,
              
              count_matrix))
}
#COMBINE THE BARE SOIL ONE SO THAT THERE ARE CONSISTENT (75 ITERATIONS)
#add 10% of the total_baresoil
total_crop_number = nrow(rbind(Total_corn[[5]],Total_cotton[[5]],Total_rice[[5]])) 
total_baresoil_number = nrow(Total_bareSoil[[5]])

#10% Baresoil x/(x+crop) = 0.1
bs_num_0.1 = round(total_crop_number/9)
TEST3_NUM_CLASS_0.1 = TEST3_NUM_CLASS(bs_num_0.1)
#10% - rf
PA_4Class_V_lst_0.1_rf = TEST3_NUM_CLASS_0.1[[1]]
UA_4Class_V_lst_0.1_rf = TEST3_NUM_CLASS_0.1[[2]]
PA_3Class_V_lst_0.1_rf = TEST3_NUM_CLASS_0.1[[3]]
UA_3Class_V_lst_0.1_rf = TEST3_NUM_CLASS_0.1[[4]]
#10% - svm
PA_4Class_V_lst_0.1_svm = TEST3_NUM_CLASS_0.1[[5]]
UA_4Class_V_lst_0.1_svm = TEST3_NUM_CLASS_0.1[[6]]
PA_3Class_V_lst_0.1_svm = TEST3_NUM_CLASS_0.1[[7]]
UA_3Class_V_lst_0.1_svm = TEST3_NUM_CLASS_0.1[[8]]

count_0.1 = TEST3_NUM_CLASS_0.1[[9]]

#20% Baresoil x/(x+crop) = 0.2
bs_num_0.2 = round(total_crop_number/4)
TEST3_NUM_CLASS_0.2 = TEST3_NUM_CLASS(bs_num_0.2)
#20% - rf
PA_4Class_V_lst_0.2_rf = TEST3_NUM_CLASS_0.2[[1]]
UA_4Class_V_lst_0.2_rf = TEST3_NUM_CLASS_0.2[[2]]
PA_3Class_V_lst_0.2_rf = TEST3_NUM_CLASS_0.2[[3]]
UA_3Class_V_lst_0.2_rf = TEST3_NUM_CLASS_0.2[[4]]
#20% - svm
PA_4Class_V_lst_0.2_svm = TEST3_NUM_CLASS_0.2[[5]]
UA_4Class_V_lst_0.2_svm = TEST3_NUM_CLASS_0.2[[6]]
PA_3Class_V_lst_0.2_svm = TEST3_NUM_CLASS_0.2[[7]]
UA_3Class_V_lst_0.2_svm = TEST3_NUM_CLASS_0.2[[8]]

count_0.2 = TEST3_NUM_CLASS_0.2[[9]]

#30% Baresoil x/(x+crop) = 0.3
bs_num_0.3 = round(total_crop_number*3/7)
TEST3_NUM_CLASS_0.3 = TEST3_NUM_CLASS(bs_num_0.3)

#30% - rf
PA_4Class_V_lst_0.3_rf = TEST3_NUM_CLASS_0.3[[1]]
UA_4Class_V_lst_0.3_rf = TEST3_NUM_CLASS_0.3[[2]]
PA_3Class_V_lst_0.3_rf = TEST3_NUM_CLASS_0.3[[3]]
UA_3Class_V_lst_0.3_rf = TEST3_NUM_CLASS_0.3[[4]]
#30% - svm
PA_4Class_V_lst_0.3_svm = TEST3_NUM_CLASS_0.3[[5]]
UA_4Class_V_lst_0.3_svm = TEST3_NUM_CLASS_0.3[[6]]
PA_3Class_V_lst_0.3_svm = TEST3_NUM_CLASS_0.3[[7]]
UA_3Class_V_lst_0.3_svm = TEST3_NUM_CLASS_0.3[[8]]

count_0.3 = TEST3_NUM_CLASS_0.3[[9]]

#40% Baresoil x/(x+crop) = 0.4
bs_num_0.4 = round(total_crop_number*4/6)
TEST3_NUM_CLASS_0.4 = TEST3_NUM_CLASS(bs_num_0.4)

#40% - rf
PA_4Class_V_lst_0.4_rf = TEST3_NUM_CLASS_0.4[[1]]
UA_4Class_V_lst_0.4_rf = TEST3_NUM_CLASS_0.4[[2]]
PA_3Class_V_lst_0.4_rf = TEST3_NUM_CLASS_0.4[[3]]
UA_3Class_V_lst_0.4_rf = TEST3_NUM_CLASS_0.4[[4]]
#40% - svm
PA_4Class_V_lst_0.4_svm = TEST3_NUM_CLASS_0.4[[5]]
UA_4Class_V_lst_0.4_svm = TEST3_NUM_CLASS_0.4[[6]]
PA_3Class_V_lst_0.4_svm = TEST3_NUM_CLASS_0.4[[7]]
UA_3Class_V_lst_0.4_svm = TEST3_NUM_CLASS_0.4[[8]]

count_0.4 = TEST3_NUM_CLASS_0.4[[9]]


#save data
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2")
save.image("Test3_rf_svm_output.RData")


