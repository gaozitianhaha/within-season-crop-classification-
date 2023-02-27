library(Rmisc) # this package is for CI()
library(ggpubr)
library(abind)

setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/")
load("FINAL_MODEL_svm_rf_output.RData")

Final_model_plot = function(OA_V_final_lst, 
                            Final_corn_UA_V_lst,
                            Final_cotton_UA_V_lst, 
                            Final_rice_UA_V_lst,
                            Final_baresoil_UA_V_lst){
  
  #average across iterations
  temp_OA_final <- do.call(abind, c(OA_V_final_lst, list(along=3)))
  OA_final_ave = apply(temp_OA_final, 1:2, function(x) mean(x,na.rm=T))
  
  temp_CORN_final <- do.call(abind, c(Final_corn_UA_V_lst, list(along=3)))
  CORN_final_ave = apply(temp_CORN_final, 1:2,function(x) mean(x,na.rm=T))
  
  temp_COTTON_final <- do.call(abind, c(Final_cotton_UA_V_lst, list(along=3)))
  COTTON_final_ave = apply(temp_COTTON_final, 1:2, function(x) mean(x,na.rm=T))
  
  temp_RICE_final <- do.call(abind, c(Final_rice_UA_V_lst, list(along=3)))
  RICE_final_ave = apply(temp_RICE_final, 1:2, function(x) mean(x,na.rm=T))
  
  temp_BARESOIL_final <- do.call(abind, c(Final_baresoil_UA_V_lst, list(along=3)))
  BARESOIL_final_ave = apply(temp_BARESOIL_final, 1:2, function(x) mean(x,na.rm=T))
  
  #tidy up data
  final_model_ggdt = 
    rbind(data.frame(value = c(CORN_final_ave),
                     month = c(rep("Oct",6), rep("Oct~Nov",6),
                               rep("Oct~Dec",6), rep("Oct~Jan",6),
                               rep("Oct~Feb",6),rep("Oct~Mar",6)),
                     type = rep("Corn")),
          data.frame(value = c(COTTON_final_ave),
                     month = c(rep("Oct",6), rep("Oct~Nov",6),
                               rep("Oct~Dec",6), rep("Oct~Jan",6),
                               rep("Oct~Feb",6),rep("Oct~Mar",6)),
                     type = rep("Cotton")),
          data.frame(value = c(RICE_final_ave),
                     month = c(rep("Oct",6), rep("Oct~Nov",6),
                               rep("Oct~Dec",6), rep("Oct~Jan",6),
                               rep("Oct~Feb",6),rep("Oct~Mar",6)),
                     type = rep("Rice")),
          data.frame(value = c(BARESOIL_final_ave),
                     month = c(rep("Oct",6), rep("Oct~Nov",6),
                               rep("Oct~Dec",6), rep("Oct~Jan",6),
                               rep("Oct~Feb",6),rep("Oct~Mar",6)),
                     type = rep("Non-crop")))
  
  final_model_ggdt$month = factor(final_model_ggdt$month,
                                  levels = c("Oct","Oct~Nov",
                                             "Oct~Dec","Oct~Jan",
                                             "Oct~Feb","Oct~Mar"))
  
  #Calculate 95% CI ------------
  OA_ci = apply(OA_final_ave,2,function(x) paste0(round(CI(x, ci = 0.95)*100),"%"))
  rownames(OA_ci) = c("lower","mean","upper")
  colnames(OA_ci)= c("Oct","Oct~Nov","Oct~Dec",
                     "Oct~Jan","Oct~Feb","Oct~Mar")
  
  CORN_ci = apply(CORN_final_ave,2,
                  function(x){
                    return(rbind(
                      max(x,na.rm=T)%>%round(digits=0),
                      mean(x,na.rm=T)%>%round(digits=0),
                      min(x,na.rm=T)%>%round(digits=0)))
                  })
  
  COTTON_ci = apply(COTTON_final_ave,2,
                    function(x){
                      return(rbind(
                        max(x,na.rm=T)%>%round(digits=0),
                        mean(x,na.rm=T)%>%round(digits=0),
                        min(x,na.rm=T)%>%round(digits=0)))
                    })
  RICE_ci = apply(RICE_final_ave[1:5,],2,
                  function(x){
                    return(rbind(
                      max(x,na.rm=T)%>%round(digits=0),
                      mean(x,na.rm=T)%>%round(digits=0),
                      min(x,na.rm=T)%>%round(digits=0)))
                  })
  BARESOIL_ci = apply(BARESOIL_final_ave,2,
                      function(x){
                        return(rbind(
                          max(x,na.rm=T)%>%round(digits=0),
                          mean(x,na.rm=T)%>%round(digits=0),
                          min(x,na.rm=T)%>%round(digits=0)))
                      })

  
  combine_ci_df = rbind(CORN_ci,COTTON_ci,RICE_ci,BARESOIL_ci,OA_ci)
  #setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/paper2_DATA/")
  #write.csv(combine_ci_df,"combine_ci_df.csv")
  
  # convert into fig
  comb_df_withCI = rbind(
    data.frame(value = c(CORN_ci),
               month=c(rep("Oct",3),
                       rep("Oct~Nov",3),
                       rep("Oct~Dec",3),
                       rep("Oct~Jan",3),
                       rep("Oct~Feb",3),
                       rep("Oct~Mar",3)),
               type = rep("Corn"),
               name = rep(c("Upper","Mean","Lower"),6)),
    data.frame(value = c(COTTON_ci),
               month=c(rep("Oct",3),
                       rep("Oct~Nov",3),
                       rep("Oct~Dec",3),
                       rep("Oct~Jan",3),
                       rep("Oct~Feb",3),
                       rep("Oct~Mar",3)),
               type = rep("Cotton"),
               name = rep(c("Upper","Mean","Lower"),6)),
    data.frame(value = c(RICE_ci),
               month=c(rep("Oct",3),
                       rep("Oct~Nov",3),
                       rep("Oct~Dec",3),
                       rep("Oct~Jan",3),
                       rep("Oct~Feb",3),
                       rep("Oct~Mar",3)),
               type = rep("Rice"),
               name = rep(c("Upper","Mean","Lower"),6)),
    data.frame(value = c(BARESOIL_ci),
               month=c(rep("Oct",3),
                       rep("Oct~Nov",3),
                       rep("Oct~Dec",3),
                       rep("Oct~Jan",3),
                       rep("Oct~Feb",3),
                       rep("Oct~Mar",3)),
               type = rep("Non-crop"),
               name = rep(c("Upper","Mean","Lower"),6)))%>%
    mutate(month = factor(month, 
                          levels =  c("Oct","Oct~Nov","Oct~Dec",
                                      "Oct~Jan","Oct~Feb","Oct~Mar")),
           type = factor(type,levels =  c("Corn","Cotton",
                                          "Rice","Non-crop")))
  
  
  MEAN_MAT =comb_df_withCI%>%dplyr::filter(name=="Mean")
  UPPER_MAT= comb_df_withCI%>%dplyr::filter(name=="Upper")
  LOWER_MAT= comb_df_withCI%>%dplyr::filter(name=="Lower")
  
  
  Figure_10 = 
    ggplot(MEAN_MAT,aes(x=month,y=value,fill=type))+
    geom_bar(stat="identity",position = position_dodge(),alpha=0.5)+
    geom_errorbar(aes(ymin=LOWER_MAT$value, ymax=UPPER_MAT$value), width=.2,
                  position=position_dodge(.9),size=0.8)+
    geom_text(aes(label = LOWER_MAT$value, x=month,y=LOWER_MAT$value-3),
              position=position_dodge(.9),color="grey50")+
    geom_text(aes(label = UPPER_MAT$value, x=month,y=UPPER_MAT$value+3),
              position=position_dodge(.9),color="grey50")+
    scale_fill_manual(values = c("#8491B4B2","#E64B35B2",
                                 "#91D1C2B2","#7E6148B2"))+
    xlab(" ")+
    ylab("User's Accuracy (UA) %")+theme_bw()+
    geom_hline(yintercept = 80,linetype="dashed")+
    theme(legend.title = element_blank(),
          axis.text = element_text(size=13),
          axis.title = element_text(size=13),
          legend.text = element_text(size=13),
          legend.position = "bottom")+
    annotate(geom="text", x=1, y=12.5, 
             label=paste0("OA=",OA_ci[2,1],"\n","[",OA_ci[3,1],",",OA_ci[1,1],"]"),
             color="black")+
    annotate(geom="text", x=2, y=12.5, 
             label=paste0("OA=",OA_ci[2,2],"\n","[",OA_ci[3,2],",",OA_ci[1,2],"]"),
             color="black")+
    annotate(geom="text", x=3, y=12.5, 
             label=paste0("OA=",OA_ci[2,3],"\n","[",OA_ci[3,3],",",OA_ci[1,3],"]"),
             color="black")+
    annotate(geom="text", x=4, y=12.5, 
             label=paste0("OA=",OA_ci[2,4],"\n","[",OA_ci[3,4],",",OA_ci[1,4],"]"),
             color="black")+
    annotate(geom="text", x=5, y=12.5, 
             label=paste0("OA=",OA_ci[2,5],"\n","[",OA_ci[3,5],",",OA_ci[1,5],"]"),
             color="black")+
    annotate(geom="text", x=6, y=12.5, 
             label=paste0("OA=",OA_ci[2,6],"\n","[",OA_ci[3,6],",",OA_ci[1,6],"]"),
             color="black")
  return(Figure_10)
}

Fig_10_rf = Final_model_plot(OA_V_final_lst_rf, 
                             Final_corn_UA_V_lst_rf,
                             Final_cotton_UA_V_lst_rf, 
                             Final_rice_UA_V_lst_rf,
                             Final_baresoil_UA_V_lst_rf)

Fig_10_svm = Final_model_plot(OA_V_final_lst_svm, 
                              Final_corn_UA_V_lst_svm,
                              Final_cotton_UA_V_lst_svm, 
                              Final_rice_UA_V_lst_svm,
                              Final_baresoil_UA_V_lst_svm)

Figure_10 = ggarrange(Fig_10_rf,Fig_10_svm, 
                      nrow=2, common.legend = T,
                      legend = "bottom",
                      labels = c("a","b"))
#save the figure
setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/Draft/Comp_and_Elec_in_Agri/Revision/Figure/")
ggsave("Figure 10.jpeg",Figure_10,dpi=600,width=8,height=9)
