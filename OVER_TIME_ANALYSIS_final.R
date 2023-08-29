# OVER TIME ANALYSIS - OVER ALL SEASONS
## Clean Environment
rm(list = ls())
cat("\014")
library(ggplot2)
# IMPORTANT: Set this as current working directory
country='SP1'
# RUN THE ANALYSIS FOR ALL SEASONS IN THE DATA FOLDER
####################################################################################
years = c(1994:2022) 
source("SEASON_LABELS_nik_GR.R")

posterior_list = vector(mode = "list", length = length(season_numbers))
HHCIB =  vector(mode = "numeric", length = length(season_numbers))
Relative_entropy = vector(mode = "numeric", length = length(season_numbers))
# If you just want to run the summary analysis but you already have all season's results
# you can put TRUE to the following code line, else put FALSE and run the analyses
############################################################################

all_seasons = F

for(yy in 1:length(updated_season_numbers)){
  season = updated_season_numbers[yy]
  if (all_seasons){
    load(paste0("Inference_results//",country,"//mcmc_Season_",season,"-",season+1,country,
                "//WS_Premier_Season_",season,"-",season+1,country,"_200k_seed_1909.RData"))
    all_seasons = TRUE
  }else{
    all_seasons = FALSE
    source("MCMC_main_nik_GR.R")
  
  }
  posterior_list[[yy]] = posterior_k
  # calculate these here else i have to run all seasons again / change later
  points_share=Tabellone/sum(Tabellone)
  HHCIB[[yy]] = length(teams)*sum(points_share^2)
  Relative_entropy[[yy]] = sum(points_share*log(points_share))/log(1/length(teams))
  rm(list=setdiff(ls(), c("Relative_entropy","HHCIB","updated_season_labels","updated_season_numbers", "posterior_list", "all_seasons","country")))
  #cat("\014")
}

years = c(1994:2022) 
source("SEASON_LABELS_nik_GR.R")

############################################################################

#new folder path
folder_path_OVER = paste0("Inference_results//",country,"//mcmc_OVER_TIME_ANALYSIS")

#load workspace in case it's ready
#load(paste0(folder_path_OVER, "//mcmc_OVER_TIME.RData"))

# Create OVER_TIME_ANALYSIS directory insinde "Inference_results" folder
dir.create(folder_path_OVER)
# Saving the posterior of K on a separate WorkSpace inside "OVER_TIME_ANALYSIS" folder



  # Print Posterior of K table
####################################################################################
library(xtable)

# Using what is written below
max_k_over = max(unlist(lapply(posterior_list, FUN = function(x){length(x)}),
                        recursive = TRUE, use.names = TRUE))

my_fun = function(x, max_k_over){
  ll = length(x)
  if (ll<max_k_over){
    clust_diff = max_k_over-ll
    for (jj in 1:clust_diff){
      x = cbind(x, 0.0)
    }
    colnames(x) = 1:max_k_over
    x
  }else{
    x
  }
}

posterior_list_updates = lapply(posterior_list, FUN = my_fun, max_k_over = max_k_over)

posterior_k_matrix <- matrix(unlist(posterior_list_updates), ncol = max_k_over, byrow = TRUE)
colnames(posterior_k_matrix) = 1:max_k_over
row.names(posterior_k_matrix) = season_labels

# HHCIB and Relative_Entropy plots
library(ggplot2)
bonus_plots_data = data.frame(season_labels,season_numbers,HHCIB,Relative_entropy,posterior_one_block=posterior_k_matrix[,1],
                              posterior_k_over_time = posterior_k_matrix %*% c(1:dim(posterior_k_matrix)[2]) /100)
ggplot(bonus_plots_data,aes(x=season_numbers,y=HHCIB,colour=posterior_one_block)) + 
  geom_point(position = position_identity(), size = 2 ) +
  xlab("Season") +
  scale_color_continuous(name = "P(K=1)")
ggsave(paste0(folder_path_OVER,"//HHCIB.pdf"))

ggplot(bonus_plots_data,aes(x=season_numbers,y=Relative_entropy,colour=posterior_one_block)) + 
  geom_point(position = position_identity(), size = 2 ) +
  xlab("Season") +
  ylab("Relative Entropy") +
  scale_color_continuous(name = "P(K=1)")
ggsave(paste0(folder_path_OVER,"//RelativeEntropy.pdf"))

# into x table
posterior_k_matrix_table<-xtable(posterior_k_matrix, digits = 2)
print.xtable(posterior_k_matrix_table, type="latex", file=paste0(folder_path_OVER,
                                                        "//Posterior_K_table_OVER_TIME.txt"))
####################################################################################


save.image(paste0(folder_path_OVER, "//mcmc_OVER_TIME.RData"))
#load(paste0("Inference_results//mcmc_Premier_OVER_TIME_ANALYSIS//mcmc_Premier_OVER_TIME.RData"))

# Computing the probability of belonging to the top block
####################################################################################
# P(top block we need posterior of K and the )
# P(k=1) + P(K=2)*P(top|k=2) + ...

#Useful Info
S = 250000
burn_in_level<-50000
my_seed<-1909

for (yy in 1:length(season_numbers)){
  season = season_numbers[yy]
  season_full<-paste0(season,"-",season+1)
  folder_path<-paste0("Inference_results//",country,"//mcmc_Season_",season_full,country,"//")
  after_object<-paste0("Season_",season_full,country,"_", (S-burn_in_level)/1000 ,"k_seed",my_seed)
  #load
  load(paste0("Inference_results//",country,"//mcmc_Season_",season_full,country,
              "//WS_Premier_Season_", season_full,country, "_", (S-burn_in_level)/1000,
              "k_seed_",my_seed,".RData"))
  # adjust labels
  season_numbers = updated_season_numbers
  season_labels = updated_season_labels
  #------------------
  # Computing P(top)
  max_k_season = length(posterior_k)
  acronym_winner<-colnames(O)[pos_winner<-which(rownames(Results)==rownames(Ordered_Tabellone)[1])]
  
  P_top = matrix(NA, N, max_k_season)
  P_top[,1] = rep(posterior_k[1], N)*1
  for (kk in 2:max_k_season){
    current_clust_model = get(paste0("Cluster_Percentages_Model", kk))
    # find the label of top block
    pos_winner_labs = which(colnames(current_clust_model)==acronym_winner)
    winner_lab_model = which.max(current_clust_model[,pos_winner_labs])
    p_top_block_k = current_clust_model[winner_lab_model,]*(posterior_k[kk]/100)
    P_top[,kk] = p_top_block_k
  }
 # sum over
  p_top_over_k = t(as.matrix(rowSums(P_top)))
  colnames(p_top_over_k) = colnames(O)
  row.names(p_top_over_k) = season
  assign(paste0("P_top_block_season", season), p_top_over_k)
}
####################################################################################
years = c(1994:2022)
source("SEASON_LABELS_nik_GR.R")

# Probability of Top block for each Championship Over time (Figure 6)
####################################################################################
greyscale_plot = FALSE
put_title = FALSE

balance_or_not = apply(posterior_k_matrix, MARGIN = 1, FUN = function(x) which.max(x))
if (greyscale_plot){
  colour_bean = ifelse(balance_or_not==1, "white", "azure4")
  if (put_title){
    pdf(paste0(folder_path_OVER, "//TopBlock_prob_datapoints_JITTERED_greyscale.pdf"), width = 20, height=10)
  }else{
    pdf(paste0(folder_path_OVER, "//TopBlock_prob_datapoints_JITTERED_greyscale_noTitle.pdf"), width = 20, height=10) 
  }
  
}else{
  colour_bean = ifelse(balance_or_not==1, "navajowhite2", "#B0E0E6")
  if (put_title){
    pdf(paste0(folder_path_OVER, "//TopBlock_prob_datapoints_JITTERED.pdf"), width = 20, height=10)
  }else{
    pdf(paste0(folder_path_OVER, "//TopBlock_prob_datapoints_JITTERED_noTitle.pdf"), width = 20, height=10)
  }
}

season_points = c(1:length(season_numbers))
par(mar=c(5,10,4,1)+.1)
if ((length(season_points) %% 2) == 0){
  plot(season_points,rep(c(0,1),length(season_points)/2), "n", xact = NULL, xlab = "Season", ylab = " ",
       cex.main = 3,xaxt="n", las = 1, cex.axis = 2, cex.lab = 2)
}else{
  plot(season_points,c(rep(c(0,1),length(season_points)/2), 0), "n", xact = NULL, xlab = "Season", ylab = " ",
       cex.main = 3,xaxt="n", las = 1, cex.axis = 2, cex.lab = 2)
}
if (put_title)
title("Posterior probabilities of belonging to the top block over time")
title(ylab = "P(top block)", cex.lab = 2.5,
      line = 4)
axis(1, at=1:length(season_numbers), labels=FALSE, cex.axis = 2)
# know it is every 5
#selection_points= rep(1, (as.integer(length(season_numbers))/3))+(0:((as.integer(length(season_numbers))/3)-1))*4
selection_points=1:length(season_numbers)
axis(1, at=(1:length(season_numbers))[selection_points],
     labels=season_labels[selection_points], cex.axis = 1, lwd.ticks = 4, tck = 0.01)
selection_points= rep(1, (as.integer(length(season_numbers))/3))+(0:((as.integer(length(season_numbers))/3)-1))*4
axis(1, at=(1:length(season_numbers))[selection_points],
     labels=season_labels[selection_points], cex.axis = 1, lwd.ticks = 4, tck = -0.01)
for (yy in 1:length(season_numbers)){
  season = season_numbers[yy]
  current_p_top = get(paste0("P_top_block_season", season))/100

  x_vals = rep(yy, length(current_p_top))
  x_jitt = rnorm(length(x_vals), 0, 0.1)
  y_jitt= rnorm(length(current_p_top), 0, 0.01)
  points(x_vals+x_jitt, current_p_top+y_jitt, cex = 1.35, pch = 21, col = "black", bg = colour_bean[yy])
  # adding mean
  #lines(c(seas-0.4, seas +0.4), rep(mean(probs_top),2), lwd = 2.5)
}
dev.off()


# Table for number of teams in top block (Figure 6)
####################################################################################
how_many_on_top = rep(NA, length(season_numbers))
for (yy in 1:length(season_numbers)){
  season = season_numbers[yy]
  current_p_top = get(paste0("P_top_block_season", season))/100
  how_many_top_this_season = sum(current_p_top>=0.5)
  how_many_on_top[yy] = how_many_top_this_season
}

# Print
if (greyscale_plot){
  if (put_title){
    pdf(paste0(folder_path_OVER, "//TopBlock_Size_barplot_greyscale.pdf"), width = 20, height=10)
  }else{
    pdf(paste0(folder_path_OVER, "//TopBlock_Size_barplot_greyscale_noTitle.pdf"), width = 20, height=10)
  }
}else{
  if (put_title){
    pdf(paste0(folder_path_OVER, "//TopBlock_Size_barplot.pdf"), width = 20, height=10)
  }else{
    pdf(paste0(folder_path_OVER, "//TopBlock_Size_barplot_noTitle.pdf"), width = 20, height=10)
  }
}

par(mar = c(5,9,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1
bp  =barplot(how_many_on_top, xlab = "Season",
              col = colour_bean, ylim = c(0, max(how_many_on_top)+1.5),
             cex.main = 3,xaxt="n", las = 1, cex.axis = 2, cex.lab = 2.5)
if (put_title){
title(main= "Size of top block over seasons")}
title(ylab = "Number of teams in top block", cex.lab = 2.5,
      line = 4)
grid(nx=NA, ny=NULL)
axis(1, at=bp, labels=season_labels, cex.axis = 2)
# know it is every 5
#selection_points= rep(1, (as.integer(length(season_numbers))/3))+(0:((as.integer(length(season_numbers))/3)-1))*4
selection_points=1:length(season_numbers)
axis(1, at=bp[selection_points],
     labels=season_labels[selection_points], cex.axis = 2, lwd.ticks = 4, tck = 0.01)
selection_points= rep(1, (as.integer(length(season_numbers))/3))+(0:((as.integer(length(season_numbers))/3)-1))*4
axis(1, at=bp[selection_points],
     labels=season_labels[selection_points], cex.axis = 2, lwd.ticks = 4, tck = -0.01)
# Add numbers
text(x = bp, y = how_many_on_top, label = how_many_on_top, pos = 3, cex = 2, col = "black")
dev.off()
####################################################################################


# Print table
####################################################################################
top_block_size_table = cbind(how_many_on_top)
rownames(top_block_size_table) = season_labels
colnames(top_block_size_table) = "Size of Top Block"
top_block_size_table_table<-xtable(top_block_size_table, digits = 0)
print.xtable(top_block_size_table_table, type="latex", file=paste0(folder_path_OVER,"//Size_top_block_table.txt"))

####################################################################################


# Top block prob over time for KNOWN TEAMS    
####################################################################################

# list of all teams labels
All_teams_labs = c()
for (yy in 1:length(season_numbers)){
  season = season_numbers[yy]
  current_p_top = get(paste0("P_top_block_season", season))/100
  All_teams_labs = c(All_teams_labs, colnames(current_p_top))
}
All_teams_labs = unique(All_teams_labs)

# Extracting p(top block) for each team in all leagues over time
# absent are assigned NA
Team_prob_over_time = matrix(NA, length(All_teams_labs), length(season_numbers))
for (yy in 1:length(season_numbers)){
  season = season_numbers[yy]
  current_p_top = get(paste0("P_top_block_season", season))/100
  current_colnames = colnames(current_p_top)
  for(tt in 1:length(All_teams_labs)){
    current_team_pos = which(current_colnames == All_teams_labs[tt])
    #Not in that league
    if (identical(current_team_pos, integer(0))){
      Team_prob_over_time[tt, yy] = NA
    }else{
      Team_prob_over_time[tt, yy] = current_p_top[current_team_pos]
    }
  }
}

colnames(Team_prob_over_time) = season_labels
row.names(Team_prob_over_time) = All_teams_labs


# PLOTTING P(TOP BLOCK) OVER TIME FOR EACH TEAM - Together making Figure 8
####################################################################################
xleft_team<-c(1:length(season_labels)-0.5)
ybottom_team<-rep(0, length(xleft_team))
xright_team<-c(2:(length(season_labels)+1)-0.5)
ytop_BALANCE<-ifelse(balance_or_not==1, 1.05, 0)
ytop_UNBALANCE<-ifelse(balance_or_not!=1, 1.05, 0)

## 2/12/21 - creating a parameter for greyscale

# Creating a separate directory inside "OVER TIME" for all the figures for each team
folder_path_OVER_for_TEAMS = paste0(folder_path_OVER, "//Over_time_Teams")
dir.create(folder_path_OVER_for_TEAMS)

for(tt in 1:length(All_teams_labs)){
  current_team = row.names(Team_prob_over_time)[tt]
  if (greyscale_plot){
    pdf(paste0(folder_path_OVER_for_TEAMS,"//",current_team ,"_Top_block_over_time_greyscale.pdf"),
        width = 15, height=8)
  }else{
    pdf(paste0(folder_path_OVER_for_TEAMS,"//",current_team ,"_Top_block_over_time.pdf"),
        width = 15, height=8)
  }
  # p(top) for team tt
  pp_top_team_over_time = Team_prob_over_time[tt, ]
  par(mar = c(5,7,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1
  
  if (greyscale_plot){
    plot(pp_top_team_over_time, pch=19,ylim=c(0,1),
         main=current_team, xaxt = "n", ylab = " ",
         xlab=c("Season"), col=ifelse(pp_top_team_over_time>=0.5,"black", "white"), yaxt = "n", cex=2,
         cex.lab = 2)
  }else{
    plot(pp_top_team_over_time, pch=19,ylim=c(0,1),
         main=current_team, xaxt = "n", ylab = " ",
         xlab=c("Season"), col=ifelse(pp_top_team_over_time>=0.5,"midnightblue", "chocolate1"), yaxt = "n", cex=2,
         cex.lab = 2)
  }
  title(ylab = "Pr(strong cluster)", cex.lab = 2,
        line = 3)
  axis(1, at=seq(2,length(season_labels),by=2), labels=season_labels[seq(2, length(season_labels), by=2)],
       cex.axis = 1.5, lwd.ticks = 2)
  axis(2, at=seq(0,1, by=0.2), labels=seq(0,1, by=0.2),
       cex.axis = 1.5)
  # Adding balance bars
  if (greyscale_plot){
    rect(xleft=xleft_team, ybottom=ybottom_team, xright=xright_team,
         ytop=ytop_BALANCE, col="white", border=NA,density=400)
    rect(xleft=xleft_team, ybottom=ybottom_team, xright=xright_team,
         ytop=ytop_UNBALANCE, col="azure4", border=NA,density=400)
  }else{
    rect(xleft=xleft_team, ybottom=ybottom_team, xright=xright_team,
         ytop=ytop_BALANCE, col="navajowhite2", border=NA,density=400)
    rect(xleft=xleft_team, ybottom=ybottom_team, xright=xright_team,
         ytop=ytop_UNBALANCE, col="navajowhite1", border=NA,density=400)
  }
  #
  for(yy in 1:length(season_labels)){
    if(!(is.na(pp_top_team_over_time[yy])))
      segments(x0=yy, y0=0, x1=yy,
               y1=pp_top_team_over_time[yy], lwd = 3.5) 
  }
  if (greyscale_plot){
    points(pp_top_team_over_time, col = "black",bg=ifelse(pp_top_team_over_time>=0.5,"black", "white"),
           cex = 3.5, pch =21)
  }else{
    points(pp_top_team_over_time, col = "black",bg=ifelse(pp_top_team_over_time>=0.5,"midnightblue", "chocolate1"),
           cex = 3.5, pch =21)
  }
  dev.off()
}

#into txt table
bi_data = data.frame(season_numbers,season_labels_full,HHCIB,Relative_entropy,posterior_k_matrix,how_many_on_top,country)
write.csv(bi_data,paste0(folder_path_OVER,"//bi_data.csv"))

#big teams over time
over_time_teams = dim(Team_prob_over_time)[1]
seasons_in_top = Team_prob_over_time[,1]
for (i in 1:over_time_teams){
  seasons_in_top[i] <-  length(Team_prob_over_time[i,Team_prob_over_time[i,]> 0.5 & is.na(Team_prob_over_time[i,])==F]) 
}
big_four = names(head(sort(seasons_in_top,decreasing = T),4))
big_four_seasons <- data.frame(Team_prob_over_time[big_four,]>0.5)
colnames(big_four_seasons)=season_labels
rownames(big_four_seasons)= big_four
write.csv(big_four_seasons,paste0(folder_path_OVER,"//Big Four per Season.csv"))

bars_dat = data.frame(s=sort(seasons_in_top,decreasing = T),n=names(sort(seasons_in_top,decreasing = T)))[1:10,]
ggplot(bars_dat,aes(x=n,y=s))+
  geom_bar(stat="identity",fill="midnightblue",width=0.7) +
  theme_void()+
  scale_x_discrete(limits = bars_dat$n, guide = guide_axis(angle = 0))+
  geom_text(aes(label=s), vjust=-0.3, size=5)+
  theme(axis.text.x=element_text(size=12,hjust=0,face = "bold"),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
ggsave(paste0(folder_path_OVER,"//Seasons at top block.pdf"),width = 12,height = 8) 

# Save all
save.image(paste0(folder_path_OVER, "//mcmc_OVER_TIME.RData"))




