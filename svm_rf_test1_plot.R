#plot the results from Test 1 (sample size)
# The script written and modified by Zitian Gao on 16 Jan 2023

#load required libraries
library(ggsci)
library(ggplot2)
library(dplyr)

#load data from the main analysis of test 1
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2")
load("Test1_rf_svm_output.RData")

tidy_df<-function(OA_V_SS_lst, iter){
  #this is a helper function to prepare data for ggplot
  #10 scenarios of sample size, 6 months
  final_df<-
    data.frame(value = c(OA_V_SS_lst[[iter]])%>%as.numeric(),
               month = c(rep(1,10),rep(2,10),rep(3,10),
                         rep(4,10),rep(5,10),rep(6,10))%>%as.factor(),
               Percentage =rep(seq(0.1,1,0.1)*100,6)%>%as.factor())
  return(final_df)
}

Get_OA_improvement = function(data){
  #This is a helper function to calculate the improvement from 10% to 100% sample
  #inclusion per month
  best_OA = data %>% dplyr::filter(Percentage==100)
  worst_OA = data %>%dplyr::filter(Percentage==10)
  
  #check whether values are sorted by month 1-6
  if(identical(best_OA$month,sort(best_OA$month))){
    if(identical(worst_OA$month,sort(worst_OA$month))){
      result = round(best_OA$ave_value - worst_OA$ave_value,digits=1)
    }else{
      stop("The order of OA is wrong. Refer to Get_OA_improvement function")
    }
  }else{
    stop("The order of OA is wrong. Refer to Get_OA_improvement function")
  }
  return(result)
  
}

#calculate the total number of samples
sampleSize_value = apply(Count_trainSam_number,1,sum)

#tidy up data for ggplot
tmp_SS_lst_rf = tmp_SS_lst_svm = list()
for(iter in 1:25){
  tmp_SS_lst_rf[[iter]] = tidy_df(OA_V_SS_lst_rf, iter)
  tmp_SS_lst_svm[[iter]] = tidy_df(OA_V_SS_lst_svm, iter)
}

#RF data
gg_SS_df_rf =do.call(rbind,tmp_SS_lst_rf)%>%data.frame()
gg_SS_df_rf$type = rep("RF")
gg_SS_df_rf = 
  gg_SS_df_rf %>% 
  group_by(Percentage,month,type)%>% 
  summarise(ave_value = mean(value))

#SVM data
gg_SS_df_svm =do.call(rbind,tmp_SS_lst_svm)%>%data.frame()
gg_SS_df_svm$type = rep("SVM")
gg_SS_df_svm = 
  gg_SS_df_svm %>% 
  group_by(Percentage,month,type)%>% 
  summarise(ave_value = mean(value))

#calculate the improvement from 10%-100% sample inclusion per month
RF_diff = Get_OA_improvement(gg_SS_df_rf)
SVM_diff = Get_OA_improvement(gg_SS_df_svm)

#ggplot
Figure_5 = 
  rbind(gg_SS_df_svm, gg_SS_df_rf)%>%
  ggplot(aes(x=Percentage,y=ave_value,color=type,group=type))+
  geom_point(size=2)+
  geom_line(linewidth=1)+
  scale_color_npg()+
  facet_wrap(~month,
             labeller = 
   labeller(month = c("1" = paste0("(a) Oct\n","RF improvement=",
                                   RF_diff[1],"%\n","SVM improvement=",
                                   SVM_diff[1],"%"),
                      "2" = paste0("(b) Oct~Nov\n","RF improvement=",
                                   RF_diff[2],"%\n","SVM improvement=",
                                   SVM_diff[2],"%"),
                      "3" = paste0("(c) Oct~Dec\n","RF improvement=",
                                   RF_diff[3],"%\n","SVM improvement=",
                                   SVM_diff[3],"%"),
                      "4" = paste0("(d) Oct~Jan\n","RF improvement=",
                                   RF_diff[4],"%\n","SVM improvement=",
                                   SVM_diff[4],"%"),
                      "5" = paste0("(e) Oct~Feb\n","RF improvement=",
                                   RF_diff[5],"%\n","SVM improvement=",
                                   SVM_diff[5],"%"),
                      "6" = paste0("(f) Oct~Mar\n","RF improvement=",
                                   RF_diff[6],"%\n","SVM improvement=",
                                   SVM_diff[6],"%"))),
             nrow=3,ncol=2)

#annotation
Figure_5 = 
  Figure_5 +
  annotate(geom="text", x=1, y=50, 
           label=sampleSize_value[1],
           color="#00A087B2")+
  annotate(geom="text", x=2, y=50, 
           label=sampleSize_value[2],
           color="#00A087B2")+
  annotate(geom="text", x=3, y=50, 
           label=sampleSize_value[3],
           color="#00A087B2")+
  annotate(geom="text", x=4, y=50, 
           label=sampleSize_value[4],
           color="#00A087B2")+
  annotate(geom="text", x=5, y=50, 
           label=sampleSize_value[5],
           color="#00A087B2")+
  annotate(geom="text", x=6, y=50, 
           label=sampleSize_value[6],
           color="#00A087B2")+
  annotate(geom="text", x=7, y=50, 
           label=sampleSize_value[7],
           color="#00A087B2")+
  annotate(geom="text", x=8, y=50, 
           label=sampleSize_value[8],
           color="#00A087B2")+
  annotate(geom="text", x=9, y=50, 
           label=sampleSize_value[9],
           color="#00A087B2")+
  annotate(geom="text", x=10, y=50, 
           label=sampleSize_value[10],
           color="#00A087B2")+
  xlab("Proportion of training samples (%)")+
  ylab("Overall Accuracy (OA,%)")+theme_bw()+
  ylim(c(50,100))+
  theme(strip.text = element_text(size=13,hjust = 0),
        axis.text = element_text(size=12),
        axis.title = element_text(size=13),
        legend.text = element_text(size=13),
        legend.position = "bottom",
        legend.title = element_blank())

#save the figure
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/Draft/Comp_and_Elec_in_Agri/Revision/Figure/")
ggsave("Figure 5.jpeg",width = 8, height = 10,Figure_5,dpi = 600)
