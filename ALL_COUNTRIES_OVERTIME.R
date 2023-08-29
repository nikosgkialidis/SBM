## Clean Environment
rm(list = ls())
cat("\014")

library(gridExtra)
library(ggplot2)

folder_path_OVER_ALL = paste0("Inference_results//ALL_COUNTRIES_ANALYSIS")
dir.create(folder_path_OVER_ALL)


countries=c("I1","G1","F1","E0","D1","SP1")

folder_path_OVER = paste0("Inference_results//",countries[1],"//mcmc_OVER_TIME_ANALYSIS")
load(paste0(folder_path_OVER, "//mcmc_OVER_TIME.RData"))
all_countries_data <- data.frame(bonus_plots_data,top_teams= how_many_on_top,country=countries[1],shape=15)

for (k in 2:length(countries)) {
  folder_path_OVER = paste0("Inference_results//",countries[k],"//mcmc_OVER_TIME_ANALYSIS")
  load(paste0(folder_path_OVER, "//mcmc_OVER_TIME.RData"))
  all_countries_data<-rbind(all_countries_data,data.frame(bonus_plots_data,top_teams = how_many_on_top,country=countries[k],shape=20+k))
}

all_countries_data


avg_posterior_K <- tapply(all_countries_data$posterior_k_over_time,all_countries_data$country,mean)
avg_top_teams <- tapply(all_countries_data$top_teams ,all_countries_data$country,mean) # να γίνει ποσοστό
avg_hhcib<-tapply(all_countries_data$HHCIB,all_countries_data$country,mean)
avg_rel_ent<-tapply(all_countries_data$Relative_entropy,all_countries_data$country,mean)


rel_ent_countries <- ggplot(all_countries_data,aes(x=season_numbers,y=Relative_entropy,colour=country)) + 
  geom_point(position = position_jitter(), size = 3, shape =19 ) +
  xlab("Season") +
  ylab("Relative Entropy") +
  scale_x_continuous(breaks = seq(1994,2022,by=4))+
  scale_y_continuous(breaks = c(0.96,0.97,0.98,0.99,1),limits = c(0.96,1))+
  scale_color_manual(values=c("black", "red3", "#56B4E9","green3","orange1","darkblue")) + 
  theme_classic(base_size = 22)
rel_ent_countries    
ggsave(paste0(folder_path_OVER_ALL,"//Relative_Entropy_ALL.pdf"),width = 8,height = 6)

rel_ent_p_k1 <- ggplot(all_countries_data,aes(x=season_numbers,y=Relative_entropy,colour=posterior_one_block)) + 
  geom_point(position = position_jitter(), size = 3) +
  geom_line(data = data.frame(p_g1,x=c(1994:2022)),aes(x=x,y=p_g1),colour="blue",linewidth=2)+
  xlab("Season") +
  ylab("Relative Entropy") +
  scale_x_continuous(breaks = seq(1994,2022,by=4))+
  scale_y_continuous(breaks = c(0.96,0.97,0.98,0.99,1),limits = c(0.96,1))+
  scale_color_continuous(name = "P(K=1)",low="black", high="skyblue1") +
  theme_classic(base_size = 22)
rel_ent_p_k1
ggsave(paste0(folder_path_OVER_ALL,"//Relative_Entropy_ALL_PosteriorK.pdf"),width = 8,height = 6)


rel_ent_p_k1_shapes <- ggplot(all_countries_data,aes(x=season_numbers,y=Relative_entropy,colour=posterior_one_block)) + 
  geom_point(position = position_jitter(),aes(shape = country), size = 3 ) +
  scale_shape_manual(values = c(15:21))+
  xlab("Season") +
  ylab("Relative Entropy") +
  scale_x_continuous(breaks = seq(1994,2022,by=4))+
  scale_y_continuous(breaks = c(0.96,0.97,0.98,0.99,1),limits = c(0.96,1))+
  scale_color_continuous(name = "P(K=1)") +
  theme_classic(base_size = 22)
rel_ent_p_k1_shapes
ggsave(paste0(folder_path_OVER_ALL,"//Relative_Entropy_ALL_PosteriorK_shapes.pdf"),width = 8,height = 6)


hhcib_countries<-ggplot(all_countries_data,aes(x=season_numbers,y=HHCIB,colour=country)) + 
  geom_point(position = position_jitter(), size = 3 ) +
  xlab("Season") +
  ylab("HHCIB") +
  scale_x_continuous(breaks = seq(1994,2022,by=4))+
  scale_color_discrete()+
  theme_classic(base_size = 22)
hhcib_countries
ggsave(paste0(folder_path_OVER_ALL,"//HHCIB_ALL.pdf"),width = 8,height = 6)


hhcib_countries_p_k1<-ggplot(all_countries_data,aes(x=season_numbers,y=HHCIB,colour=posterior_one_block)) + 
  geom_point(position = position_jitter(),size=3 ) +
  xlab("Season") +
  ylab("HHCIB") +
  scale_x_continuous(breaks = seq(1994,2022,by=4))+
  scale_color_continuous(name = "P(K=1)",low="black", high="skyblue1")+
  theme_classic(base_size = 22)
hhcib_countries_p_k1
ggsave(paste0(folder_path_OVER_ALL,"//HHCIB_ALL_PosteriorK.pdf"),width = 8,height = 6)

hhcib_countries_p_k1_shapes<-ggplot(all_countries_data,aes(x=season_numbers,y=HHCIB,colour=posterior_one_block,shape = country)) + 
  geom_point(position = position_jitter(),size=3 ) +
  scale_shape_manual(values = c(15:21))+
  xlab("Season") +
  ylab("HHCIB") +
  scale_x_continuous(breaks = seq(1994,2022,by=4))+
  scale_color_continuous(name = "P(K=1)")+
  theme_classic(base_size = 22)
hhcib_countries_p_k1_shapes
ggsave(paste0(folder_path_OVER_ALL,"//HHCIB_ALL_PosteriorK_shapes.pdf"),width = 8,height = 6)

# HHICB and Relative Entropy with Average K color 

ggplot(all_countries_data,aes(x=season_numbers,y=HHCIB,colour=posterior_k_over_time)) + 
  geom_point(position = position_jitter(),size=3 ) +
  xlab("Season") +
  ylab("HHICB") +
  scale_x_continuous(breaks = seq(1994,2022,by=4))+
  scale_color_continuous(name = "Average K",low="skyblue1", high="black")+
  theme_classic(base_size = 22)
ggsave(paste0(folder_path_OVER_ALL,"//HHICB_Posterior_k_over_time.pdf"),width = 8,height = 6)


ggplot(all_countries_data,aes(x=season_numbers,y=Relative_entropy,colour=posterior_k_over_time)) + 
  geom_point(position = position_jitter(),size=3 )+
  xlab("Season") +
  ylab("Relative Entropy") +
  scale_x_continuous(breaks = seq(1994,2022,by=4))+
  scale_color_continuous(low="skyblue1", high="black",name = "Average K")+
  theme_classic(base_size = 22)
ggsave(paste0(folder_path_OVER_ALL,"//Relative_Entropy_Posterior_k_over_time.pdf"),width = 8,height = 6)

write.csv(all_countries_data,paste0(folder_path_OVER_ALL,"//all_countries_data.csv"))


source("HHICB_plot_lines.R")
source("RelativeEntropy_plot_lines.R")

pdf(paste0(folder_path_OVER_ALL,"//Average posterior blocks.pdf"),width = 8, height = 5)
#change if necessary
names(avg_posterior_K)=c("Germany","England","France","Greece","Italy","Spain")
barplot(sort(avg_posterior_K),col = "navyblue",main = "Average posterior blocks")
dev.off()

one_block=numeric(dim(all_countries_data)[1])
one_block[all_countries_data$posterior_one_block>50]=1
all_countries_data=cbind(all_countries_data,one_block)

one_block_seasons=tapply(all_countries_data$one_block,all_countries_data$country,sum)
names(one_block_seasons)=c("Germany","England","France","Greece","Italy","Spain")
barplot(sort(one_block_seasons),horiz = T,las=2)

bardata=data.frame(x=sort(one_block_seasons),y=names(sort(one_block_seasons)))
ggplot(data=bardata,aes(x=x,y=y))+
  geom_bar(stat = "identity",fill="navyblue")+
  geom_text(aes(label=x),hjust=-0.5,cex=5)+
  xlab("")+
  ylab("")+
  theme_classic(base_size = 18)
ggsave(paste0(folder_path_OVER_ALL,"/one block seasons.pdf"),width = 9,height = 5)

