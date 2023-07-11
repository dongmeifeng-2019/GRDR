
#####change to your working directory
setwd('C:/Research/GlobalQ/global_Q_code/') 

df_pos_sta = read.csv('data/Figure2C_data_pos.csv')
df_neg_sta = read.csv('data/Figure2C_data_neg.csv')

regs = c('USE','DOF','URB','RL')
pval = array(dim=0)
yposs = array(dim=0)
for(ireg in regs){
  sta = t.test(df_pos_sta[,ireg], df_neg_sta[,ireg], var.equal=FALSE)
  ypos = quantile(df_pos_sta[,ireg],0.75)
  if(ireg=='RL'){ypos=10.8}
  pval = c(pval,sta$p.value)
  yposs = c(yposs,ypos)
}
sigs = array(dim=0)
for(p in pval){
  if(p<0.001){
    sig = '***'
  }else if(p<0.01){
    sig = '**'
  }else if(p<0.1){
    sig = '*'
  }else{
    sig = ''
  }
  sigs = c(sigs, sig)
}
prep = data.frame(reg=regs,sig = sigs,ypos = yposs)
prep = prep[order(-prep$ypos),]
prep$x = c(1:4)

df_agg = df1%>%
  transmute(group=group,RL=n_reg/n_basin*100,DOF=DOF,USE=USE,URB=URB)%>%
  gather(key='reg',value='level',-group)

prep1 = df_agg%>%
  left_join(prep,by='reg')

p = ggplot(df_agg, aes(fill=reorder(group,-level), y=level,x=reorder(reg,-level,na.rm=T),
                       color=reorder(group,-level))) +
  geom_boxplot(alpha = 0.5,outlier.size = 0.8)+
  scale_y_continuous("Regulation level (%)",limits = c(0,20)) +
  scale_x_discrete("", labels=c("Total regulation", "Fragmentation",'Water consumption','Urbanization')) +
  scale_fill_manual(values = c('#4d857c','#593E25'),
                    labels = c('Upstream shift basins','Downstream shift basins'))+
  scale_color_manual(values = c('#4d857c','#593E25'))+
  theme_bw()+
  guides(fill = guide_legend(override.aes= list(alpha = 0.5)),color = 'none')+
  theme(axis.text.y = element_text(color = 'black',size=10),
        axis.title.y = element_text(color = 'black',size=10),
        axis.text.x = element_text(color = 'black',size=10,angle=15,vjust = 1,hjust=0.7),
        axis.title.x = element_text(color = 'black',size=10),
        legend.position = c(0.6,0.90),
        legend.title = element_blank(),
        legend.text = element_text(color = 'black',size=9),
        legend.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
p = p + geom_text(data=prep1,aes(x=x,y=ypos,label=sig),color='black')

ggsave('graph/Figure2C.jpg',
       p,dpi=600,width=3,height=3,units='in')