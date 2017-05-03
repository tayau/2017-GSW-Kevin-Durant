### GSW KD Project

#setting working directory
rm(list=ls())
setwd("C:/Users/Trevor/Desktop/R Programs/Personal Projects/Basketball/GSW2017")

#reading in 2016-17 data
data.GSW.16.17.full<-read.table('GSW_2016-2017.txt', header=T,sep=',')
data.GSW.16.17<-data.GSW.16.17.full[7:8]
data.GSW.16.17$PT.diff<-data.GSW.16.17$Tm-data.GSW.16.17$Opp.1

#reading in Gamesplayed by Kevin Durant
KD.GP<-read.table('Kevin_Durant-GamesPlayed.txt',header = T)
data.GSW.16.17$GamesPlayed<-KD.GP
new.data<-as.data.frame(data.GSW.16.17[,c(1,3:4)])
new.data$GamesPlayed<-factor(unlist(new.data$GamesPlayed))
played.data0<-new.data[which(new.data$GamesPlayed=='0'),]
played.data1<-new.data[which(new.data$GamesPlayed=='1'),]
mean(played.data0[,2])
mean(played.data1[,2])

#Recoding our Data from 0 and 1's with labels
new.data$GamesPlayed <- recode(new.data$GamesPlayed,
                         " 1='w/ KD'; 0='w/o KD'")

#Scatterplot 
library(ggplot2)
KDplot1<-ggplot(data=new.data,aes(rep(1:82),new.data$PT.diff, color=new.data$GamesPlayed))+
  scale_colour_manual(name = "Games Played", values=c('#56B4E9','#E69F00'))+ #Changes Legend title
  geom_point()+ggtitle('2016-17 KD Point Differential Scatterplot')+
  xlab('Game Number')+ylab('Point Differential')
KDplot1
ggsave(plot=KDplot1, file='2016-17 KD Point Differential Scatterplot.png', height = 5,width = 8, type='cairo-png')


#Building our model
model1<-glm(PT.diff~GamesPlayed, data=new.data)
model1
summary(model1)


#Performing ANOVA
library(car)
Anova(model1) # Performing LRT, same as anova(model1, test='Chi')

library(lsmeans)
glm.lsmean<-lsmeans::lsmeans(model1,~GamesPlayed)
glm.lsmean
summary(glm.lsmean)

#plotting CI box plot
library(reshape2)

plot.confint<-ggplot(data=summary(glm.lsmean),
                          aes(x=GamesPlayed, y=lsmean, group=GamesPlayed))+ #aes() function specifies various asthetics of the plot ie: What are the X and Y variables
  ggtitle("Comparison of Point differential of Games with/without Kevin Durant")+
  xlab("Games Played")+ylab("Point Differential Estimates and their 95% CI")+
  geom_point()+ #geom_point adds the points to the graph
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL),width=0.2) #adds the 95% CI from our data frame 
ggsave(plot=plot.confint, file='2016-17 KD Point Differential Comparison.png', height = 5,width = 8, type='cairo-png')
plot.confint







