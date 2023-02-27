library("viridis") 
library(abind)

setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/")
load("Ternary_rf_svm_output.RData")

#OA_prop_LST = OA_prop_LST_rf
ternary_data = function(OA_prop_LST){
  temp <- do.call(abind, c(OA_prop_LST, list(along=3)))
  OA_prop_ave_flex = apply(temp, 1:2, function(x) mean(x,na.rm=T))
  
  actual_point = autual_pcent_mat*100
  comb_mat_pcent = apply(comb_mat,2,function(x) x*100)
  comb_mat_pcent = data.frame(comb_mat_pcent)
  
  value_df = data.frame(value = c(OA_prop_ave_flex)*100,
                        month = c(rep("Oct",36),
                                  rep("Oct~Nov",36),
                                  rep("Oct~Dec",36),
                                  rep("Oct~Jan",36),
                                  rep("Oct~Feb",36),
                                  rep("Oct~Mar",36)))
  gg_df = cbind(rbind(comb_mat_pcent,
                      comb_mat_pcent,
                      comb_mat_pcent,
                      comb_mat_pcent,
                      comb_mat_pcent,
                      comb_mat_pcent),value_df)
  
  colnames(gg_df) = c("Corn","Cotton","Rice","OA","Month")
  gg_df$Month = factor(gg_df$Month,levels = c("Oct","Oct~Nov","Oct~Dec",
                                              "Oct~Jan","Oct~Feb","Oct~Mar"))
  gg_df$Corn = gg_df$Corn/100
  gg_df$Cotton = gg_df$Cotton/100
  gg_df$Rice = gg_df$Rice/100
  
  return(gg_df)
}

RF_ternary_dt = ternary_data(OA_prop_LST_rf)
SVM_ternary_dt = ternary_data(OA_prop_LST_svm)


OAbreaks = seq(from = 0, to = 100, by = 2)
#RF plot
RF_ternary =
  ggtern(data = RF_ternary_dt, mapping = aes(x =  Corn  , y = Cotton, z = Rice)) +
  geom_interpolate_tern(mapping = aes(color = ..level.., value = OA),
                        breaks = OAbreaks, base = "identity", method = "glm",
                        formula = value ~ poly(x, y, degree = 2)) +
  facet_wrap(~Month)+
  geom_point(aes(color=OA))+
  scale_color_viridis() +
  annotate(geom  = 'point',
           x     = actual_point[1],
           y     = actual_point[2],
           z     = actual_point[3],color="red",size=4)+
  ggtern::theme_bw() + 
  theme_arrowlarge()+
  theme(tern.axis.arrow = element_line(size = 1))+
  labs(color='OA (%)') +
  theme(strip.text = element_text(size=16),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=16),
        panel.spacing = unit(2, "lines"),
        legend.title = element_text(size=16),
        legend.position = "bottom") 

#svm plot
SVM_ternary =
  ggtern(data = SVM_ternary_dt, mapping = aes(x =  Corn  , y = Cotton, z = Rice)) +
  geom_interpolate_tern(mapping = aes(color = ..level.., value = OA),
                        breaks = OAbreaks, base = "identity", method = "glm",
                        formula = value ~ poly(x, y, degree = 2)) +
  facet_wrap(~Month)+
  geom_point(aes(color=OA))+
  scale_color_viridis() +
  annotate(geom  = 'point',
           x     = actual_point[1],
           y     = actual_point[2],
           z     = actual_point[3],color="red",size=4)+
  ggtern::theme_bw() + 
  theme_arrowlarge()+
  theme(tern.axis.arrow = element_line(size = 1))+
  labs(color='OA (%)') +
  theme(strip.text = element_text(size=16),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        legend.text = element_text(size=16),
        panel.spacing = unit(2, "lines"),
        legend.title = element_text(size=16),
        legend.position = "bottom") 

setwd("C:/Users/zitiang/OneDrive - The University of Melbourne/paper2/Draft/Comp_and_Elec_in_Agri/Revision/Figure/")
ggsave("Figure 7.jpeg",width = 11.5, height =8.5,RF_ternary,dpi = 600)
ggsave("Figure S2.jpeg",width = 11.5, height =8.5,SVM_ternary,dpi = 600)
