y=all_countries_data$Relative_entropy[all_countries_data$country=="E0"]
x=all_countries_data$season_numbers[all_countries_data$country=="E0"]
#plot(all_countries_data$Relative_entropy[all_countries_data$country=="E0"],pch=16)
m_E0<-lm(y~x)
summary(m_E0)
p_e0 = predict(m_E0,data.frame(x=c(1994:2022)))
#lines(p_e0,col="red")

y=all_countries_data$Relative_entropy[all_countries_data$country=="D1"]
x=all_countries_data$season_numbers[all_countries_data$country=="D1"]
#points(all_countries_data$Relative_entropy[all_countries_data$country=="D1"])
m_D1<-glm(y~x+I(x^2))
summary(m_D1)
p_d1 = predict(m_D1,data.frame(x=c(1994:2022)))
#lines(p_d1,col="black")

y=all_countries_data$Relative_entropy[all_countries_data$country=="SP1"]
x=all_countries_data$season_numbers[all_countries_data$country=="SP1"]
#plot(all_countries_data$Relative_entropy[all_countries_data$country=="SP1"])
m_SP1<-glm(y~x+I(x^2)+I(x^3))
summary(m_SP1)
p_sp1 = predict(m_SP1,data.frame(x=c(1994:2022)))
#lines(p_sp1,col="yellow3")

y=all_countries_data$Relative_entropy[all_countries_data$country=="F1"]
x=all_countries_data$season_numbers[all_countries_data$country=="F1"]
#plot(all_countries_data$Relative_entropy[all_countries_data$country=="F1"])
m_F1<-glm(y~x+I(x^2))
summary(m_F1)
p_f1 = predict(m_F1,data.frame(x=c(1994:2022)))
#lines(p_f1,col="blue4")


y=all_countries_data$Relative_entropy[all_countries_data$country=="I1"]
x=all_countries_data$season_numbers[all_countries_data$country=="I1"]
#plot(all_countries_data$Relative_entropy[all_countries_data$country=="I1"])
m_I1<-glm(y~x+I(x^2))
summary(m_I1)
p_i1 = predict(m_I1,data.frame(x=c(1994:2022)))
#lines(p_i1,col="green4")

y=all_countries_data$Relative_entropy[all_countries_data$country=="G1"]
x=all_countries_data$season_numbers[all_countries_data$country=="G1"]
#plot(all_countries_data$Relative_entropy[all_countries_data$country=="G1"],x=all_countries_data$season_numbers[all_countries_data$country=="G1"] )
m_G1<-glm(y~x+I(x^2)+I(x^3))
summary(m_G1)
p_g1 = predict(m_G1,data.frame(x=c(1994:2022)))
#lines(p_g1,col="blue")




colours_list=c("green4","blue","#56B4E9","red","black","yellow3")
jit=rnorm(dim(all_countries_data)[1])/5
c_colours=numeric()
k=1
for (i in countries) {
  c_colours=append(c_colours,rep(colours_list[k], length(all_countries_data$country[all_countries_data$country==i])))
  k=k+1
}


pdf(paste0(folder_path_OVER_ALL,"//RelativeEntropy_Lines.pdf"),width = 8, height = 5)

post_cols=character(dim(all_countries_data)[1])
post_cols[all_countries_data$posterior_one_block>80]="skyblue1"
post_cols[all_countries_data$posterior_one_block>60 & all_countries_data$posterior_one_block<=80]="skyblue3"
post_cols[all_countries_data$posterior_one_block>40 & all_countries_data$posterior_one_block<=60]="blue4"
post_cols[all_countries_data$posterior_one_block<=40]="black"

par(mar=c(4.1, 4.1, 1.1, 8.1), xpd=TRUE)
plot(x=c(1994:2022),p_g1,type = "l",xlim = c(1993,2023),col="blue",ylim=c(0.96,1),xlab="Season",ylab="Relative Entropy")
points(all_countries_data$Relative_entropy,x=all_countries_data$season_numbers+jit,pch=20,col=post_cols)
lines(c(1994:2022),p_i1,col="green4",lwd=2)
lines(c(1994:2022),p_g1,col="blue",lwd=2)
lines(c(1994:2022),p_f1,col="#56B4E9",lwd=2)
lines(c(1994:2022),p_e0,col="red",lwd=2)
lines(c(1994:2022),p_d1,col="black",lwd=2)
lines(c(1994:2022),p_sp1,col="yellow3",lwd=2)

legend(title="Country",x="topright",legend = countries,col=colours_list,lty = 1,cex = .8,xpd = TRUE,inset=c(-0.2,0),lwd=3)
legend(title="P(K=1)",x="bottomright",legend = c("0.80","0.60","0.40","0.20"),col=c("skyblue1","skyblue2","blue4","black"),pch=20,lty=NA,cex = .9,xpd = TRUE,inset=c(-0.2,0),lwd=3)

dev.off() 
