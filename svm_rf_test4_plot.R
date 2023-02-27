#plot the results from Test 4 (LOOCV)
# The script written and modified by Zitian Gao on 16 Jan 2023

library(dplyr)
library(ggpubr)
library(ggsci)

#load data from the main analysis of test 1
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/")
load("Test4_rf_svm_output.RData")

tidy_df_LOOCV<-function(value,myclass){
  df = 
    data.frame(value = c(value),
               year = rep(c(gsub("-", "\U2012","2013-2014"),
                            gsub("-", "\U2012","2014-2015"),
                            gsub("-", "\U2012","2015-2016"),
                            gsub("-", "\U2012","2016-2017"),
                            gsub("-", "\U2012","2017-2018"),
                            gsub("-", "\U2012","2018-2019")),6)%>%as.factor(),
               month = c(rep(1,6),rep(2,6),rep(3,6),
                         rep(4,6),rep(5,6),rep(6,6))%>%as.numeric(),
               type =rep(myclass))
  return(df)
}


Test4_tidy_data = function(OA_v_lst,OA_v_new_lst){
  # This is a helper function to prepare ggplot data
  tmp_LOOCV_new_lst = tmp_LOOCV_lst = list()
  
  #change data to the format that is accepted for ggplot, for each iteration
  for(iter in 1:25){
    tmp_LOOCV_new_lst[[iter]] = tidy_df_LOOCV(OA_v_new_lst[[iter]],
                                              "LOOCV samples")
    tmp_LOOCV_lst[[iter]] = tidy_df_LOOCV(OA_v_lst[[iter]],
                                          "Samples from the classification year")
  }
  
  data = 
    rbind(do.call(rbind,tmp_LOOCV_new_lst),
          do.call(rbind,tmp_LOOCV_lst))%>%
    as.data.frame()%>%
    mutate(year = as.factor(year),
           type = as.factor(type),
           month = as.factor(month),
           value = as.numeric(value)*100)
  data$type = factor(data$type,levels = c("Samples from the classification year",
                                          "LOOCV samples"))
  return(data)
}

#prepare data for RF
data_rf = 
  Test4_tidy_data(OA_v_lst_rf,OA_v_new_lst_rf)%>%
  group_by(year,month,type)%>%
  summarise(ave_value = mean(value))
data_rf$ML = rep("RF")

#prepare data for SVM
data_svm = 
  Test4_tidy_data(OA_v_lst_svm,OA_v_new_lst_svm)%>%
  group_by(year,month,type)%>%
  summarise(ave_value = mean(value))
data_svm$ML = rep("SVM")

COMB_DATA = rbind(data_rf,data_svm)

p1 = 
  ggplot() + 
  geom_line(data = COMB_DATA%>%dplyr::filter(type == 'Samples from the classification year' &
                                                ML =="RF"), 
            aes(x=month, y=ave_value,group=1,color = type),linewidth=1) + 
  geom_point(data = COMB_DATA%>%dplyr::filter(type == 'Samples from the classification year' &
                                                ML =="RF"), 
            aes(x=month, y=ave_value,group=1,color = type),size=2) +
  geom_line(data = COMB_DATA%>%dplyr::filter(type == 'LOOCV samples'&
                                             ML =="RF"),
            aes(x=month, y=ave_value,group=1,color = type),linewidth=1) + 
  geom_point(data = COMB_DATA%>%dplyr::filter(type == 'LOOCV samples'&
                                               ML =="RF"),
            aes(x=month, y=ave_value,group=1,color = type),size=2) + 
  facet_wrap(~year)+
  scale_x_discrete(breaks = seq(1, 6, by = 1)%>%as.factor,
                   labels = c("Oct","Oct~Nov",
                              "Oct~Dec","Oct~Jan",
                              "Oct~Feb","Oct~Mar"))+
  xlab(" ")+ylab("Overall Accuracy")+
  scale_color_npg()+
  theme_bw()+
  theme( legend.position = "bottom",
         axis.text.x = element_text(angle = 90,size=11),
         axis.text.y = element_text(size=11),
         axis.title = element_text(size=11),
         legend.text = element_text(size=11),
         legend.title = element_blank(),
         strip.text = element_text(size=11))+
  ylim(c(0,100))+
  ggtitle("a")

p2 =  
  ggplot() + 
  geom_line(data = COMB_DATA%>%dplyr::filter(type == 'Samples from the classification year' &
                                                ML =="SVM"), 
            aes(x=month, y=ave_value,group=1,color = type),linewidth=1) + 
  geom_point(data = COMB_DATA%>%dplyr::filter(type == 'Samples from the classification year' &
                                                ML =="SVM"), 
            aes(x=month, y=ave_value,group=1,color = type),size=2) + 
  geom_line(data = COMB_DATA%>%dplyr::filter(type == 'LOOCV samples' &
                                                ML =="SVM"),
            aes(x=month, y=ave_value,group=1,color = type),linewidth=1) + 
  geom_point(data = COMB_DATA%>%dplyr::filter(type == 'LOOCV samples' &
                                                ML =="SVM"),
            aes(x=month, y=ave_value,group=1,color = type),size=2) + 
  facet_wrap(~year)+
  scale_x_discrete(breaks = seq(1, 6, by = 1)%>%as.factor,
                   labels = c("Oct","Oct~Nov",
                              "Oct~Dec","Oct~Jan",
                              "Oct~Feb","Oct~Mar"))+
  xlab(" ")+ylab(" ")+
  scale_color_npg()+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90,size=11),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size=11),
        legend.text = element_text(size=11),
        legend.title = element_blank(),
        strip.text = element_text(size=11))+ 
  ylim(c(0,100))+
  ggtitle("b")

Figure_9 = ggarrange(p1,p2, common.legend = T, nrow=1)
#save the figure
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/Draft/Comp_and_Elec_in_Agri/Revision/Figure/")
ggsave("Figure 9.jpeg",width = 8, height = 6,Figure_9,dpi = 600)


Cal_diff_by_month = function(data){
  # This is a helper function to calculate difference by month
  dt1 = 
    data%>%
    dplyr::filter(type == 'Samples from the classification year')%>%
    group_by(month)%>%
    summarise(year_ave_same = mean(ave_value))
  
  dt2 =
    data%>%
    dplyr::filter(type == 'LOOCV samples')%>%
    group_by(month)%>%
    summarise(year_ave_loocv = mean(ave_value))
  
  result = 
    dt1 %>% left_join(dt2)%>%
    mutate(diff = year_ave_same - year_ave_loocv)
  
  return(result)
}

LOOCV_diff_rf = Cal_diff_by_month(data_rf)
LOOCV_diff_svm = Cal_diff_by_month(data_svm)

#save the table
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/Draft/Comp_and_Elec_in_Agri/Revision/Figure/")
write.csv(file = "LOOCV_diff_rf.csv",LOOCV_diff_rf)
write.csv(file = "LOOCV_diff_svm.csv",LOOCV_diff_svm)
