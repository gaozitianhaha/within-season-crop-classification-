#plot the results from Test 2 (crop classes proportions)
# The script written and modified by Zitian Gao on 16 Jan 2023

#load required libraries
library(dplyr)
library(ggsci)
library(ggplot2)

#load data from the main analysis of test 3
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/")
load("Test3_rf_svm_output.RData")

tidy_df_3class = function(value){
  # This is a helper function to tidy up 3-class data for ggplot
  df = data.frame(value = c(value),
                 crop = rep(c("Corn","Cotton","Rice"),6),
                 month = c(rep("Oct",3),rep("Oct~Nov",3),
                           rep("Oct~Dec",3),rep("Oct~Jan",3),
                           rep("Oct~Feb",3),rep("Oct~Mar",3)))
  df$class = rep("No non-crop")
  df$month = factor(df$month, 
                    levels = c("Oct","Oct~Nov","Oct~Dec",
                               "Oct~Jan","Oct~Feb","Oct~Mar"))
  return(df)
}

tidy_df_4class = function(value){
  # This is a helper function to tidy up 4-class data for ggplot
  df = data.frame(value = c(value),
                 crop = rep(c("Corn","Cotton","Rice","Non-crop"),6),
                 month = c(rep("Oct",4),rep("Oct~Nov",4),
                           rep("Oct~Dec",4),rep("Oct~Jan",4),
                           rep("Oct~Feb",4),rep("Oct~Mar",4)))
  df$class = rep("With non-crop")
  df$month = factor(df$month, 
                    levels =  c("Oct","Oct~Nov","Oct~Dec",
                                "Oct~Jan","Oct~Feb","Oct~Mar"))
  return(df)
}


Fig_8_plot_overall = function(UA_3Class_V_lst_0.1,UA_4Class_V_lst_0.1,
                              UA_3Class_V_lst_0.2,UA_4Class_V_lst_0.2,
                              UA_3Class_V_lst_0.3,UA_4Class_V_lst_0.3,
                              UA_3Class_V_lst_0.4,UA_4Class_V_lst_0.4){
  
    tmp_BS_01_3Class_lst = tmp_BS_01_4Class_lst =
    tmp_BS_02_3Class_lst = tmp_BS_02_4Class_lst =
    tmp_BS_03_3Class_lst = tmp_BS_03_4Class_lst =
    tmp_BS_04_3Class_lst = tmp_BS_04_4Class_lst = list()
    
  for(iter in 1:25){
    tmp_BS_01_3Class_lst[[iter]] = tidy_df_3class(UA_3Class_V_lst_0.1[[iter]])
    tmp_BS_01_4Class_lst[[iter]] = tidy_df_4class(UA_4Class_V_lst_0.1[[iter]])
    tmp_BS_02_3Class_lst[[iter]] = tidy_df_3class(UA_3Class_V_lst_0.2[[iter]])
    tmp_BS_02_4Class_lst[[iter]] = tidy_df_4class(UA_4Class_V_lst_0.2[[iter]])
    tmp_BS_03_3Class_lst[[iter]] = tidy_df_3class(UA_3Class_V_lst_0.3[[iter]])
    tmp_BS_03_4Class_lst[[iter]] = tidy_df_4class(UA_4Class_V_lst_0.3[[iter]])
    tmp_BS_04_3Class_lst[[iter]] = tidy_df_3class(UA_3Class_V_lst_0.4[[iter]])
    tmp_BS_04_4Class_lst[[iter]] = tidy_df_4class(UA_4Class_V_lst_0.4[[iter]])
  }
  
  df_baresSoil_0.1=
    rbind(do.call(rbind, tmp_BS_01_3Class_lst),
          do.call(rbind, tmp_BS_01_4Class_lst))
  
  df_baresSoil_0.1$label = c("10% Non-crop")
  
  df_baresSoil_0.2=
    rbind(do.call(rbind, tmp_BS_02_3Class_lst),
          do.call(rbind, tmp_BS_02_4Class_lst))
  
  df_baresSoil_0.2$label = c("20% Non-crop")
  
  df_baresSoil_0.3=
    rbind(do.call(rbind, tmp_BS_03_3Class_lst),
          do.call(rbind, tmp_BS_03_4Class_lst))
  
  df_baresSoil_0.3$label = c("30% Non-crop")
  
  df_baresSoil_0.4=
    rbind(do.call(rbind, tmp_BS_04_3Class_lst),
          do.call(rbind, tmp_BS_04_4Class_lst))
  
  df_baresSoil_0.4$label = c("40% Non-crop")
  
  Inclu_nonCrop_ggdf = rbind(df_baresSoil_0.1,
                             df_baresSoil_0.2,
                             df_baresSoil_0.3,
                             df_baresSoil_0.4)
  #reorder classes
  Inclu_nonCrop_ggdf$crop = 
    factor(Inclu_nonCrop_ggdf$crop,
           levels=c("Non-crop","Corn","Cotton","Rice"))
  
  #final ggplot data
  data = 
    Inclu_nonCrop_ggdf%>%
    data.frame()%>%
    mutate(month = as.factor(month),
           class = as.factor(class),
           crop = as.factor(crop),
           value = as.numeric(value),
           label = factor(label))%>%
    group_by(month,crop,class,label)%>%
    summarise(ave_value = mean(value,na.rm=T))
  
  #ggplot
  Figure_8 =
    data%>%
    ggplot(aes(x=month,y=ave_value,color=class,group=class))+
    geom_point()+
    geom_line(linewidth=1)+
    ylim(c(0,100))+
    xlab(" ")+
    scale_color_npg()+
    facet_grid(label~crop)+
    ylab("User's Accuracy")+
    theme_bw()+
    theme(axis.text = element_text(size=12),
          axis.text.x = element_text(angle=90),
          axis.title = element_text(size=12),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          strip.text = element_text(size=12),
          legend.position = "bottom")
  
  return(Figure_8)
}
#rf
Fig_8 = 
  Fig_8_plot_overall(UA_3Class_V_lst_0.1_rf,UA_4Class_V_lst_0.1_rf,
                     UA_3Class_V_lst_0.2_rf,UA_4Class_V_lst_0.2_rf,
                     UA_3Class_V_lst_0.3_rf,UA_4Class_V_lst_0.3_rf,
                     UA_3Class_V_lst_0.4_rf,UA_4Class_V_lst_0.4_rf)
#svm
Fig_S3 = 
  Fig_8_plot_overall(UA_3Class_V_lst_0.1_svm,UA_4Class_V_lst_0.1_svm,
                     UA_3Class_V_lst_0.2_svm,UA_4Class_V_lst_0.2_svm,
                     UA_3Class_V_lst_0.3_svm,UA_4Class_V_lst_0.3_svm,
                     UA_3Class_V_lst_0.4_svm,UA_4Class_V_lst_0.4_svm)

#save the figures
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/Draft/Comp_and_Elec_in_Agri/Revision/Figure/")
ggsave("Figure 8.jpeg",width = 9, height = 10,Fig_8,dpi = 600)
ggsave("Figure S3.jpeg",width = 9, height = 10,Fig_S3,dpi = 600)
