#Results[Results$HomeTeam=="Panathinaikos",]
#season<-"9900"
#fix the algorithm to read the data from season
#season<-"2001-2002"
if(season<2017)  {file<-paste0("all-euro-data-",season_full,'.xls')} else {file<-paste0("all-euro-data-",season_full,'.xlsx')}
library(readxl)
Results <- read_excel(paste0("all_euro_data//",file),sheet=country)[,c('HomeTeam','AwayTeam','FTR','FTHG','FTAG')];head(Results)
Results<-Results[complete.cases(Results),]

teams<-levels(as.factor(Results$HomeTeam))
Results$FTR[Results$FTR=="H"]<-"W"
Results$FTR[Results$FTR=="A"]<-"L"
N<-length(teams)

count_mat<-matrix(NA,N,N)
for (i in 1:N) {
  for (j in 1:N) {
    if(i!=j){count_mat[i,j]<-length(Results$FTR[Results$HomeTeam==teams[i] & Results$AwayTeam==teams[j]])}
  }
}

res_mat<-matrix(NA,N,N)
colnames(res_mat)<-rownames(res_mat)<-teams
for (i in 1:N) {
  for (j in 1:N) {
    if(i!=j){res_mat[i,j]<-Results$FTR[Results$HomeTeam==teams[i] & Results$AwayTeam==teams[j]][1]}
  }
}

Results<-res_mat
O_value<-res_mat
O<-matrix(NA,N,N)
O[O_value=="W"]<-1
O[O_value=="D"]<-2
O[O_value=="L"]<-3

colnames(O)<-rownames(O)<-colnames(O_value)
#Function for the adjacency matrix from O
to_adjacency<-function(O_matrix){
  N_nodes=dim(O_matrix)[1]
  adja_y=array(data=NA, dim=c(N,N,3))
  for(i in 1:N_nodes){
    for(j in 1:N_nodes){
      #do not consider self loops
      if(i!=j){
        #value 1 for W
        if(O_matrix[i,j]==1)
          adja_y[i,j,]=c(1,0,0)
        #Draw
        if(O_matrix[i,j]==2)
          adja_y[i,j,]=c(0,1,0)
        #Loss
        if(O_matrix[i,j]==3)
          adja_y[i,j,]=c(0,0,1)
      }
    }
  }
  return(adja_y)
}

y<-to_adjacency(O)

#number of nodes
N= dim(O)[1]

#do not need to build a generalised adjacency matrix as it would be a waste of
#memory and more complicate to work with
O_value=O
O_value<-ifelse(O==1, "W", ifelse(O==2, "D", "L"))

O_value
