library(ggplot2)
u1<-runif(1200, min = 0, max = 1) 
u2<-runif(1200, min = 0, max = 1)
v1<-runif(90, min = 0, max = 1) 
v2<-runif(90, min = 0, max = 1)

for (i in 1:1200)
{
  if (u1[i]+u2[i]>1){
    u1[i]=1-u1[i]
    u2[i]=1-u2[i]
  }
}
for (i in 1:90)
{
  if (v1[i]+v2[i]>1){
    v1[i]=1-v1[i]
    v2[i]=1-v2[i]
  }
}

a=c(2,0)
b=c(1,3)
x<-u1*2+u2
x1<-v1*2+v2
y1<-3*v2
y<-3*u2
col<-c(rep(c("1","1","1","1","1","3","1","1","3","4"),times=120),rep(c("5"),times=80))
x<-c(u1*2+u2,runif(80, min = 0.8, max = 1.2)) 
y<-c(3*u2,runif(80, min = -0.3, max =0))
col1<-rep(c(3,4),times=450)

df<-data.frame(x,y,col)

ggplot(df)+
  geom_point(aes(x,y,colour=col),size=3)+
  scale_color_manual(values=c("darkgreen","red","green","#5C4033"))+
  theme(panel.background = element_rect(fill = 'white', color='white'))+
  theme(plot.background = element_rect(fill = 'white', color='white'))+
  theme(legend.position = "none")+
  labs(y = "", x ="")+
    xlim(0, 2.5)+
    ylim(-0.5, 3.5)
  
