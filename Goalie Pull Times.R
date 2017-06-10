rm(list=ls())
#setwd('C:/Users/Trevor/Desktop/R Programs/Personal Projects/Hockey/Goalie Pull times')
#data<-read.csv("Canucks-score-times.csv", header=TRUE,stringsAsFactors=FALSE)
#data
#data<-data[7]
#data
#data[data=='n/a']<-'na'
#data$Total.Seconds<-as.numeric(data$Total.Seconds)

inf<-data.frame(seq(0:4000))
#data<-na.omit(data)
#data

#Cumulative distribution function
expfun<-function(x,lambda){
  v<-1-exp(-lambda*x)
  
  return(v)
}

#inverse cumulative distribution function
invexpfun<-function(x,lambda){
  v<-(log(-x+1)/(-lambda))
  return(v)
}
expfun(120,0.000579268)
invexpfun(0.05,0.000579268) #overall
invexpfun(0.05,0.000640244) #3rd
invexpfun(0.05,0.00054878) #home
invexpfun(0.05,0.000609756) #away

invexpfun(0.10,0.000579268) #overall
invexpfun(0.10,0.000640244) #3rd
invexpfun(0.10,0.00054878) #home
invexpfun(0.10,0.000609756) #away

invexpfun(0.25,0.000579268) #overall
invexpfun(0.25,0.000640244) #3rd
invexpfun(0.25,0.00054878) #home
invexpfun(0.25,0.000609756) #away

invexpfun(0.85,0.000579268) #overall
v_overall<-expfun(inf,0.000579268) #Overall
v_3rd <-expfun(inf,0.000640244)
v_home <-expfun(inf,0.00054878)
v_away <-expfun(inf,0.000609756)


inf_overall<-cbind(inf,v_overall)
inf_3rd <- cbind(inf,v_3rd)
inf_home <- cbind(inf,v_home)
inf_away <- cbind(inf,v_away)



colnames(inf_overall)[1]<-'Time'
colnames(inf_overall)[2]<-'Value'
colnames(inf_3rd)[1]<-'Time'
colnames(inf_3rd)[2]<-'Value'
colnames(inf_home)[1]<-'Time'
colnames(inf_home)[2]<-'Value'
colnames(inf_away)[1]<-'Time'
colnames(inf_away)[2]<-'Value'


#inf<-inf[1:4000,]
#plot(x=inf$Time, y=inf$Value)


#For Overall rate
#expfun1<-function(x){
#  v<-1-exp(???0.00058*x)

#  return(v)
#}



library(ggplot2)
ggplot(data = inf_overall, aes(x = Time, y=Value))+
  geom_line()+
  geom_area()


ggplot(data = inf_3rd, aes(x = Time, y=Value))+
  geom_line()+
  geom_area()

ggplot(data = inf_home, aes(x = Time, y=Value))+
  geom_line()+
  geom_area()

ggplot(data = inf_away, aes(x = Time, y=Value))+
  geom_line()+
  geom_area()


#============================================
install.packages('ggthemes')
require(ggthemes)
require(grid)
require(gridGraphics)

plot1 <- ggplot(data = inf_overall, aes(x = Time, y=Value))+
  geom_line()+
  ylim(c(0,1))+ggtitle('Entire Season')+
  geom_area(fill='red', alpha=.4)+
  theme_fivethirtyeight()+
  xlab('Time (Seconds)')+ylab('Probability')+
  theme(axis.title=element_text(size=8))
plot1


plot2 <- ggplot(data = inf_3rd, aes(x = Time, y=Value))+
  geom_line()+
  ylim(c(0,1))+ggtitle('3rd Period')+
  geom_area(fill='blue', alpha=.4)+
  theme_fivethirtyeight()+
  xlab('Time (Seconds)')+ ylab('Probability')+
  theme(axis.title=element_text(size=8))

plot3 <- ggplot(data = inf_away, aes(x = Time, y=Value))+
  geom_line()+
  ylim(c(0,1))+ggtitle('Away')+
  geom_area(fill='yellow', alpha=.4)+
  theme_fivethirtyeight()+
  xlab('Time (Seconds)')+ ylab('Probability')+
  theme(axis.title=element_text(size=8))

plot4 <- ggplot(data = inf_home, aes(x = Time, y=Value))+
  geom_line()+
  ylim(c(0,1))+ggtitle('Home')+
  geom_area(fill='orange', alpha=.4)+
  theme_fivethirtyeight()+
  xlab('Time (Seconds)')+ylab('Probability')+
  theme(axis.title=element_text(size=8))

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(1, 2))
print(plot3, vp = vplayout(2, 1))
print(plot4, vp = vplayout(2, 2))
  






