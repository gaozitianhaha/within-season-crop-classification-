#plot the results from Test 2 (crop classes proportions)
# The script written and modified by Zitian Gao on 16 Jan 2023

#load required libraries
library(dplyr)
library(abind)
library(ggsci)
library(ggplot2)

#load data from the main analysis of test 2
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/")
load("Test2_rf_svm_output.RData")

setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/paper2_DATA/")
load("paper2_data_Filt.RData")


gg_UA_PA_Prop = function(x1,x2,actual_thres,crop){
  # This is helper function to prepare data for UA/PA for
  # individual crop type
  df= 
    rbind(
      data.frame(value = c(x1),
                 prop = rep(seq(0.1,0.9,0.1),6),
                 type =rep("Commission Error"),
                 month = c(rep("(a) Oct",9),rep("(b) Oct~Nov",9),
                           rep("(c) Oct~Dec",9),rep("(d) Oct~Jan",9),
                           rep("(e) Oct~Feb",9),rep("(f) Oct~Mar",9))),
      
      data.frame(value = c(x2),
                 prop = rep(seq(0.1,0.9,0.1),6),
                 type =rep("Omission Error"),
                 month = c(rep("(a) Oct",9),rep("(b) Oct~Nov",9),
                           rep("(c) Oct~Dec",9),rep("(d) Oct~Jan",9),
                           rep("(e) Oct~Feb",9),rep("(f) Oct~Mar",9))))
  
  df$value = 100 - df$value #convert UA/PA to commission/omission error
  
  #fix the order of month
  df$month = factor(df$month, 
                    levels =  c("(a) Oct","(b) Oct~Nov","(c) Oct~Dec",
                                "(d) Oct~Jan","(e) Oct~Feb","(f) Oct~Mar"))
  df$crop = rep(crop)
  df$actual_thres = rep(actual_thres)
  
  return(df)
}

Fig_6_plot_overall = function(corn_prop_UA_LST,corn_prop_PA_LST,
                              cotton_prop_UA_LST,cotton_prop_PA_LST,
                              rice_prop_UA_LST,rice_prop_PA_LST){
  #data in 2017-2018
  RF_df<- rbind(Total_corn[[5]], Total_cotton[[5]],Total_rice[[5]])
  
  #calculate the actual proportion from survey
  autual_prop = 
    RF_df%>%
    group_by(label)%>%
    dplyr::summarise(n = n())%>%
    dplyr::select(n)
  temp_pcent = (autual_prop[,1]/sum(autual_prop[,1]))%>%
    round(digits = 3)
  autual_pcent_mat = temp_pcent[,1]

  #CORN 
  #average across 25 iterations
  temp <- do.call(abind, c(corn_prop_UA_LST, list(along=3)))
  corn_prop_UA_ave = apply(temp, 1:2, function(x) mean(x,na.rm=T))
  temp <- do.call(abind, c(corn_prop_PA_LST, list(along=3)))
  corn_prop_PA_ave = apply(temp, 1:2, function(x) mean(x,na.rm=T))
  
  data_corn_test2 = gg_UA_PA_Prop(corn_prop_UA_ave,
                                  corn_prop_PA_ave, 
                                  autual_pcent_mat[1],
                                  "CORN")
  
  #COTTON
  temp <- do.call(abind, c(cotton_prop_UA_LST, list(along=3)))
  cotton_prop_UA_ave = apply(temp, 1:2, function(x) mean(x,na.rm=T))
  temp <- do.call(abind, c(cotton_prop_PA_LST, list(along=3)))
  cotton_prop_PA_ave = apply(temp, 1:2, function(x) mean(x,na.rm=T))
  
  data_cotton_test2 = gg_UA_PA_Prop(cotton_prop_UA_ave,
                                    cotton_prop_PA_ave, 
                                    autual_pcent_mat[2],
                                    "COTTON")
  
  #RICE
  temp <- do.call(abind, c(rice_prop_UA_LST, list(along=3)))
  rice_prop_UA_ave = apply(temp, 1:2, function(x) mean(x,na.rm=T))
  temp <- do.call(abind, c(rice_prop_PA_LST, list(along=3)))
  rice_prop_PA_ave = apply(temp, 1:2, function(x) mean(x,na.rm=T))
  
  data_rice_test2 = gg_UA_PA_Prop(rice_prop_UA_ave,
                                  rice_prop_PA_ave, 
                                  autual_pcent_mat[3],
                                  "RICE")
  
  df = rbind(data_corn_test2,data_cotton_test2,data_rice_test2)
  
  Figure_6 = 
    ggplot(data=df)+
    geom_line(aes(x=prop,y=value,color=type),linewidth=1)+
    geom_point(aes(x=prop,y=value,color=type),size=2)+
    xlab("Selected proportion in the training data")+
    ylab("Error (%)")+
    scale_color_npg()+
    facet_grid(crop~month)+
    geom_vline(data=df,aes(xintercept=actual_thres),size=1, linetype = "dashed")+
    theme_bw()+
    theme(legend.title = element_blank(),
          axis.text = element_text(size=12),
          strip.text = element_text(size=16,hjust = 0),
          axis.title = element_text(size=16),
          legend.text = element_text(size=16),
          legend.position = "bottom")
  
  return(Figure_6)
}
#rf
Fig_6 = Fig_6_plot_overall(
                   corn_prop_UA_LST_rf,corn_prop_PA_LST_rf,
                   cotton_prop_UA_LST_rf,cotton_prop_PA_LST_rf,
                   rice_prop_UA_LST_rf,rice_prop_PA_LST_rf)
#svm
Fig_S1 = Fig_6_plot_overall(
                   corn_prop_UA_LST_svm,corn_prop_PA_LST_svm,
                   cotton_prop_UA_LST_svm,cotton_prop_PA_LST_svm,
                   rice_prop_UA_LST_svm,rice_prop_PA_LST_svm)

#save the figure
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/Draft/Comp_and_Elec_in_Agri/Revision/Figure/")
ggsave("Figure 6.jpeg",width = 10, height = 8,Fig_6,dpi = 600)
ggsave("Fig_S1.jpeg",width = 10, height = 8,Fig_S1,dpi = 600)
