#####change to your working directory
setwd('C:/Research/GlobalQ/global_Q_code/') 

df = read.csv('data/Figure1_inset_data.csv')

significance_level = 0.05
cp = c('gray','#024959','#F2AE30','#593E25','black')
#cp = c('#A1A2A6','#024959','#F2C12E','#F2AE30','#593E25')

prep5 <- df %>%
  group_by(order) %>%
  mutate(
    #slope = format(tfpwmk(frac)[2], digits=2),
    #slope_perc = format(tfpwmk(frac)[2]/mean(frac)*100,digits=2),
    slope = format(summary(lm(frac~year))$coefficients[2,1], digits=2),
    slope_perc = format(summary(lm(frac~year))$coefficients[2,1]/mean(frac)*100,digits=2),
    significance = tfpwmk(frac)[4],
    unct = format(summary(lm(frac~year))$coefficients[2,2]/mean(frac)*100,digits=2),
    x = ifelse(order%in%c('Stream Order = 1','Stream Order = 2','Stream Order = 3',
                          'Stream Order = 4','Stream Order = 5','Stream Order = 7'),
               min(year)+13,max(year)-13),   # x coordinate for slope label
    ymax = max(frac)  ,   # y coordinate for slope label
    ymin = min(frac),
    font_face = ifelse(significance<=significance_level,'bold','plain'),
    cl = ifelse(significance<=significance_level,'yellow',cp[4])
  )

scaleFUN <- function(x) sprintf("%.4f", x)

for (iorder in unique(prep5$order)){
  idf = df%>%
    filter(order==iorder)
  iprep5 = prep5%>%
    filter(order==iorder)
  
  cl = ifelse(iprep5$significance<=significance_level,cp[3],cp[5])
  
  p = ggplot(data=idf,aes(x=year,y=frac))+
    geom_line(color=cp[5],linewidth=0.3)+
    #facet_wrap(~order,ncol=4,strip.position = 'top',scales="free_y")+
    geom_smooth(method=lm, formula=y~x, level=0.95,se=T,color = cp[2],size=0.3,fill = cp[1],linetype='dashed')+
    scale_y_continuous(label = scaleFUN,breaks=equal_breaks(n=4, s=0.05))#+
  #scale_x_continuous(breaks=c(1985,1995,2005,2015))
  
  p = p + 
    geom_text(
    data = iprep5, aes(x = x, y = ymax-0.05*(ymax-ymin),
                       label = sprintf('%s %s %s',slope_perc,"\u00B1",unct),
                       fontface  = font_face),color = cl, size=2.2)+
    theme_bw()+
    theme(legend.position = 'none',
          #strip.background = element_rect(fill = cp[1]),
          #strip.text = element_text(color = cp[5],size=7),
          axis.text.y = element_blank(), #element_text(color='black',size=6),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(0.05,0.05,0.05,0.05), "cm"),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          #axis.line =  element_line(color = 'black'),
          axis.line =  element_blank(),
          panel.grid.minor = element_blank())
  
  p
  if(iorder=='Stream Order >= 8'){
    iorder = 'Stream Order = 8'
  }
  ggsave(sprintf('graph/Figure1_inset_%s.jpg',iorder),
         p,dpi=600,width=0.8,height=0.65,units='in')
}