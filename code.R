setwd("/Users/chengshuang/Desktop/summary/")
GraphSAGE_aggregator<-read.csv("GraphSAGE_aggregator.csv",header=T)
GraphSAGE_Lossfunction_optimize<-read.csv("GraphSAGE_Lossfunction_optimize.csv",header=T)
GraphSAGE_NetworkStructure<-read.csv("GraphSAGE_NetworkStructure.csv",header=T)
Model_effect_comparison<-read.csv("Model_effect_comparison.csv",header=T,row.names=1)
GCN_epoch<-read.csv("GCN_epoch.csv",header=T)
GCN_epoch$type<-"GCN"
GraphSAGE_epoch<-read.csv("GraphSAGE_epoch.csv",header=T)
GraphSAGE_epoch$type<-"GraphSAGE"
GAT_epoch<-read.csv("GAT_epoch.csv",header=T)
GAT_epoch$type<-"GAT"
epoch<-rbind(GCN_epoch,GraphSAGE_epoch,GAT_epoch)

library("ggplot2")
p1<-ggplot(GCN_epoch, aes(x = epoch, y = ACC,colour=ACC,size=ACC)) +
  # 折线图函数
  geom_point() +
  scale_colour_gradient(low = 'lightblue', high = 'darkblue')+
  theme_bw() +
 theme(legend.position="none")+
  geom_line(size = 1) 
  # 散点图函数：size设置大小，shape设置形状，fill设置填充颜色
 # geom_point(size = 4, shape = 21,fill = "white")
p1
ggsave("GCN_epoch.pdf",p1 )

p2<-ggplot(GraphSAGE_epoch, aes(x = epoch, y = ACC,colour=ACC,size=ACC)) +
  # 折线图函数
  geom_point() +
  scale_colour_gradient(low = 'lightblue', high = 'darkblue')+
  theme_bw() +
  theme(legend.position="none")+
  geom_line(size = 1) 
# 散点图函数：size设置大小，shape设置形状，fill设置填充颜色
# geom_point(size = 4, shape = 21,fill = "white")
p2
ggsave("GraphSAGE_epoch.pdf",p2  )   ###,width =4 ,height =3

p3<-ggplot(GAT_epoch, aes(x = epoch, y = ACC,colour=ACC,size=ACC)) +
  # 折线图函数
  geom_point() +
  scale_colour_gradient(low = 'lightblue', high = 'darkblue')+
  theme_bw() +
  theme(legend.position="none")+
  geom_line(size = 1) 
# 散点图函数：size设置大小，shape设置形状，fill设置填充颜色
# geom_point(size = 4, shape = 21,fill = "white")
p3
ggsave("GAT_epoch.pdf",p3  )   ###,width =4 ,height =3

p4<-ggplot(epoch, aes(x = epoch, y = ACC, shape = type,color=type)) +
  # 折线图函数：position设置偏置项
  geom_line() +
  # 散点图函数：position设置偏置项
  geom_point( size = 2)+
  theme_bw()+
  theme(legend.position = "bottom")
p4
ggsave("epoch.pdf",p4 )  



p5<-ggplot(data=GraphSAGE_Lossfunction_optimize,mapping=aes(x=Loss_function.optimizer,y=ACC,fill=Loss_function.optimizer,color = Loss_function.optimizer,group=factor(1)))+
  geom_bar(stat="identity",width=0.5)+
  #geom_point( )+
  geom_text(aes(label = ACC, vjust = -0.8, hjust = 0.5, color = Loss_function.optimizer), show.legend = FALSE)+
  theme_bw() +
  theme(legend.position="none")+
  xlab(" ")+ylab("ACC")

#  ylim(0,0.9)
# p5

p51 <- p5 + coord_cartesian(ylim = c(0.85, 0.903))

p51
ggsave("GraphSAGE_Lossfunction_optimize.pdf",p51)  


p6<-ggplot(data=GraphSAGE_NetworkStructure,mapping=aes(x=Network_Structure,y=ACC,fill=Network_Structure,group=factor(1)))+
  geom_bar(stat="identity",width=0.5)+
  geom_text(aes(label = ACC, vjust = -0.8, hjust = 0.5, color = Network_Structure), show.legend = FALSE)+
  theme_bw() +
  theme(legend.position="none")+
  xlab(" ")+ylab("ACC")
p6

p61 = p6 + coord_cartesian(ylim = c(0.88, 0.901))

p61
ggsave("GraphSAGE_NetworkStructure.pdf",p61)  


Model_effect_comparison<-read.csv("Model_effect_comparison.csv",header=T,row.names=1)
Model_effect_comparison<-as.numeric(Model_effect_comparison)
Model_effect_comparison<-as.matrix(Model_effect_comparison)
install.packages("gplots","RColorBrewer")
library(gplots)
library(RColorBrewer)
install.packages("ggsci","scales")
library(ggsci)
library('scales')
show_col(colorRampPalette(brewer.pal(8,'Accent'))(16))
show_col(colorRampPalette(pal_npg('nrc',alpha = 0.8)(10))(20))#展示所选颜色 
#coul <- colorRampPalette(brewer.pal(8, "PiYG"))(24)#换个好看的颜色
#coul = colorRampPalette(colors = c("blue","white","red"))(100)
coul = colorRampPalette(colors = c("#91D1C2","white","#E64B35"))(100)

hM <- format(round(Model_effect_comparison, 4))#对数据保留2位小数
p7<-heatmap.2(Model_effect_comparison,
          trace="none",#不显示trace
          col=coul,#修改热图颜色
          density.info = "none",#图例取消density
          key.xlab ='ACC',
          key.title = "",
          cexRow = 1,cexCol = 1,#修改横纵坐标字体
          Rowv = F,Colv = F, #去除聚类
         margins = c(5, 10),
          cellnote = hM,notecol='black'#添加相关系数的值及修改字体颜色
)
p7
###直接在export保存的

GraphSAGE_aggregator
p8<-ggplot(GraphSAGE_aggregator, aes(x = epoch, y = ACC, shape = aggregator,color=aggregator)) +
  # 折线图函数：position设置偏置项
  geom_line() +
  # 散点图函数：position设置偏置项
  geom_point( size = 2)+
  theme_bw()+
  theme(legend.position = "bottom")
p8
ggsave("GraphSAGE_aggregator.pdf",p8)  

