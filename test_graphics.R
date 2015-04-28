#Number of livestock purchased in Live2Markets programme
p1 <- ggplot(x_live)
p1 <- p1 + geom_bar(aes(x=year, y=ksh1),fill=nrt_red,colour="red",stat="identity", width=0.5, bg="white")
p1 <- p1 + theme_bw() + 
          theme(panel.border = element_blank(), 
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                axis.line = element_line(colour = "grey",lineend="butt",size=2))
p1 <- p1 + theme(axis.title.x = element_blank())   # Remove x-axis label
p1 <- p1 + xlab("")
p1 <- p1 + ylab("KSh - Millions") 
p1 <- p1 +  ggtitle("Livestock Purchased") 
#p1 <- p1+ theme(axis.title.x=element_text(vjust=0,family="serif", size=12, face="bold"))
p1 <- p1 + theme(axis.title.y=element_text(angle=90, vjust=1,family="", size=10, face="bold")) 
p1 <- p1 + theme(axis.text=element_text(family="", size=10, face="bold"))
#p1 <- p1 + theme(plot.title=element_text(size=15, vjust=3)) 
#p1 <- p1 + theme(plot.margin = unit(c(1,1,1,1), "cm")) 
p1 <- p1 + scale_y_continuous(labels = function(x) format(x, width = 5))
p1 <- p1 + geom_text(aes(x=year, y=ksh1,label=x_live$ksh1,face="bold", colour="blue", font="tondo",vjust=1.5))
p1 <- p1 + theme(legend.position="none")
p1


