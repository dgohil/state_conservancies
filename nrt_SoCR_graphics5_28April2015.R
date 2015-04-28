
#####################################################
#  State of Conservancy Report Graphics - 2014
#
#
#
#  last updated by Deepali Gohil on 28 April 2015
#
#
####################################################

library(openxlsx)
library (ggplot2)
library (extrafont)

fonts()
#installs fonts
#font_import()
y
loadfonts(device="postscript")
getwd()

dir("/Users/dgohil/Dropbox (NRT Kenya)/state_conservancies/state_conservancies/data")

x_live <- read.xlsx("data/nrt_enterprise_soc2014.xlsx",sheet="livestock")
x_employ <- read.xlsx("data/nrt_enterprise_soc2014.xlsx",sheet="employment")
x_bead <- read.xlsx("data/nrt_enterprise_soc2014.xlsx",sheet="beads")


nrt_red <- rgb(160,15,15,max=255)
nrt_green1 <- rgb(80,120,20,max=255)
nrt_yellow <- rgb(190,235,0,max=255)
wh <- rgb(255,255,255,max=255)
nrt_black <- rgb(0,0,0,max=255)
nrt_brown <- rgb(135,90,40,max=255)
nrt_green2 <- rgb(110,105,0,max=255)
nrt_red1 <- rgb(155,20,0,max=255)
nrt_blue <- rgb(0,80,160,max=255)


# Goal 6 - Enterprise
# Livestock to Markets Programme

# livestock to market sales data

x_live$ksh1 <- round(x_live$ksh/1000000,1)

windows(width=6,height=4,record=T)

postscriptFonts()
#Number of livestock purchased in Live2Markets programme
postscript(file="l2m_ksh_28Apr2015.eps",family="ITCOfficinaSans LT Book")
p1 <- ggplot(x_live) + 
    geom_bar(aes(x=year, y=ksh1),fill= nrt_red,stat="identity",bg="white", width=0.5) +
    theme_bw() + 
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "grey",lineend="butt",size=2)) +
    theme(axis.title.x = element_blank()) +   # Remove x-axis label
    xlab("")+
    ylab("KSh - Millions") +
  ggtitle("Livestock Purchased") +  theme(plot.title = element_text(size = 15))+
  #theme(axis.title.x=element_text(vjust=0,family="serif", size=12, face="bold")) +
  theme(axis.title.y=element_text(angle=90, vjust=1,family="", size=10, face="bold")) +
  theme(axis.text=element_text(family="", size=10, face="bold"))+
  #theme(plot.title=element_text(size=15, vjust=3)) +
  #theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_y_continuous(labels = function(x) format(x, width = 5))
p1 <- p1 + geom_text(aes(x=year, y=ksh1,label=x_live$ksh1),colour="white", vjust=1.5)
p1 <- p1 + theme(legend.position="none")
p1
dev.off()

ggsave("C:\\Worden\\Dropbox (Personal)\\State_of_conservancies\\_revised_March2015\\final_figures_data\\l2m_ksh_28Apr2015.eps")


# number of households participating in Live2Markets program
postscript(file="l2m_hh_28Apr2015.eps",family="ITCOfficinaSans LT Book")
p2 <- ggplot(x_live) + 
  geom_bar(aes(x=year, y=hh),fill=nrt_red,stat="identity",bg="white", width=0.5) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "grey",lineend="butt",size=2)) +
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  xlab("")+
  ylab("Households") +
  ggtitle("Number of Households Participating") +  theme(plot.title = element_text(size = 15))
  #theme(axis.title.x=element_text(vjust=0,family="serif", size=12, face="bold")) +
  theme(axis.title.y=element_text(angle=90, vjust=1,family="", size=14, face="bold")) +
  theme(axis.text=element_text(family="", size=12, face="bold")) +
  #theme(plot.title=element_text(size=15, vjust=3)) +
  #theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_y_continuous(labels = function(x) format(x, width = 5))
p2 <- p2 + geom_text(aes(x=year, y=hh,label=hh),colour="white", vjust=1.5)
p2 <- p2 + theme(legend.position="none")
p2
dev.off()


ggsave("C:\\Worden\\Dropbox (Personal)\\State_of_conservancies\\_revised_March2015\\final_figures_data\\l2m_hh_28Apr2015.eps")



# bead income to womens groups

x_bead$ksh1 <- round(x_bead$ksh/1000000,1)

windows(width=6,height=4,record=T)

p3 <- ggplot(x_bead) + 
  geom_bar(aes(x=year, y=ksh1),fill=nrt_green1,stat="identity",bg="white") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "grey",lineend="butt",size=2)) +
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  xlab("")+
  ylab("KSh - Millions") +
  ggtitle("Beadwork Purchased") +
  #theme(axis.title.x=element_text(vjust=0,family="serif", size=12, face="bold")) +
  theme(axis.title.y=element_text(angle=90, vjust=1,family="", size=10, face="bold")) +
  theme(axis.text=element_text(family="", size=10, face="bold"))+
  #theme(plot.title=element_text(size=15, vjust=3)) +
  #theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_y_continuous(labels = function(x) format(x, width = 5))
p3

ggsave("C:\\Worden\\Dropbox (Personal)\\State_of_conservancies\\_revised_March2015\\final_figures_data\\beads_ksh_27Apr2015.eps")

# number of women participating in NRTT's beadwork programme
p4 <- ggplot(x_bead) + 
  geom_bar(aes(x=year, y=people),fill=nrt_green1,stat="identity",bg="white") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "grey",lineend="butt",size=2)) +
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  xlab("")+
  ylab("Women") +
  ggtitle("Number of Women Participating") +
  #geom_text(aes(x=year,y=people,label=x_bead$people,size=8, vjust=1.2)) +
  #theme(axis.title.x=element_text(vjust=0,family="serif", size=12, face="bold")) +
  theme(axis.title.y=element_text(angle=90, vjust=1,family="", size=10, face="bold")) +
  theme(axis.text=element_text(family="", size=10, face="bold"))+
  #theme(plot.title=element_text(size=15, vjust=3)) +
  #theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_y_continuous(labels = function(x) format(x, width = 5))
p4

p4 + geom_text()
  p4 + geom_text(aes(x=year, y=people, label=people, vjust=1.2,size=8)) 

ggsave("C:\\Worden\\Dropbox (Personal)\\State_of_conservancies\\_revised_March2015\\final_figures_data\\beads_women_label_27Apr2015.eps")



# Employment data

# Conservancy employees - permanent

p5 <- ggplot(x_employ) + 
  geom_bar(aes(x=year, y=ccy_perm),fill=nrt_red,stat="identity",bg="white") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "grey",lineend="butt",size=2)) +
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  xlab("")+
  ylab("Employees") +
  ggtitle("Conservancy Employees - Permanent") +
  #geom_text(aes(x=year,y=people,label=x_bead$people,size=8, vjust=1.2)) +
  #theme(axis.title.x=element_text(vjust=0,family="serif", size=12, face="bold")) +
  theme(axis.title.y=element_text(angle=90, vjust=1,family="", size=10, face="bold")) +
  theme(axis.text=element_text(family="", size=10, face="bold"))+
  #theme(plot.title=element_text(size=15, vjust=3)) +
  #theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_y_continuous(labels = function(x) format(x, width = 5))
p5

#p5 + geom_text(aes(x=year, y=ccy_perm, label=ccy_perm, vjust=1.2,size=8)) 

ggsave("C:\\Worden\\Dropbox (Personal)\\State_of_conservancies\\_revised_March2015\\final_figures_data\\employ_ccy_perm_27Apr2015.eps")



# Conservancy employees - casual

p6 <- ggplot(x_employ) + 
  geom_bar(aes(x=year, y=ccy_casual),fill=nrt_red,stat="identity",bg="white") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "grey",lineend="butt",size=2)) +
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  xlab("")+
  ylab("Employees") +
  ggtitle("Conservancy Employees - Casual") +
  #geom_text(aes(x=year,y=people,label=x_bead$people,size=8, vjust=1.2)) +
  #theme(axis.title.x=element_text(vjust=0,family="serif", size=12, face="bold")) +
  theme(axis.title.y=element_text(angle=90, vjust=1,family="", size=10, face="bold")) +
  theme(axis.text=element_text(family="", size=10, face="bold"))+
  #theme(plot.title=element_text(size=15, vjust=3)) +
  #theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_y_continuous(labels = function(x) format(x, width = 5))
p6

#p6 + geom_text(aes(x=year, y=ccy_perm, label=ccy_casual, vjust=1.2,size=8)) 

ggsave("C:\\Worden\\Dropbox (Personal)\\State_of_conservancies\\_revised_March2015\\final_figures_data\\employ_ccy_casual_27Apr2015.eps")


# Tourism employees - casual + permanent

p7 <- ggplot(x_employ) + 
  geom_bar(aes(x=year, y=tourism_casual_perm),fill=nrt_red,stat="identity",bg="white") +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "grey",lineend="butt",size=2)) +
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  xlab("")+
  ylab("Employees") +
  ggtitle("Tourism Employees - Permanent and Casual") +
  #geom_text(aes(x=year,y=people,label=x_bead$people,size=8, vjust=1.2)) +
  #theme(axis.title.x=element_text(vjust=0,family="serif", size=12, face="bold")) +
  theme(axis.title.y=element_text(angle=90, vjust=1,family="", size=10, face="bold")) +
  theme(axis.text=element_text(family="", size=10, face="bold"))+
  #theme(plot.title=element_text(size=15, vjust=3)) +
  #theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_y_continuous(labels = function(x) format(x, width = 5))
p7

#p7 + geom_text(aes(x=year, y=tourism_casual_perm, label=tourism_casual_perm, vjust=1.2,size=8,face="bold",color="white")) 

ggsave("C:\\Worden\\Dropbox (Personal)\\State_of_conservancies\\_revised_March2015\\final_figures_data\\employ_tourism_27Apr2015.eps")




########################################################################
# Conservancy income and expenditure


x_exp1 <- read.xlsx("conservancy_expenditure_SoC2014.xlsx",sheet="Sheet1")
x_exp2 <- read.xlsx("conservancy_expenditure_SoC2014.xlsx",sheet="Sheet2")

x_exp2a <- x_exp2[x_exp2$inc_exp=="expense",]
x_exp2a$ksh1 <- x_exp2a$ksh/1000000
#x_exp2a$type <- as.numeric(x_exp2a$type)

windows(record=T)
p8 <- ggplot(x_exp2a,aes(x=year, y=ksh1, fill=type)) + 
  geom_bar(stat="identity") +
  xlab("") +
  ylab("KSh - Millions)")+
  ggtitle("Conservancy and Community Fund Expenditures")+
  scale_fill_manual(values=c(nrt_red,"olivedrab4","olivedrab3","olivedrab1"),"\n",
                    labels = c("Conservancy - Operations", "Community - Education", 
                               "Community - Health","Community - Other")) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "grey",lineend="butt",size=2)) +  
  theme(axis.title.x=element_text(vjust=0,family="", size=10, face="bold")) +
  theme(axis.title.y=element_text(vjust=1.5,family="", size=10, face="bold")) +
  theme(axis.text=element_text(family="", size=10, face="bold")) +
  theme(legend.position = c(0.2, 0.9))
    
    p8

ggsave("C:\\Worden\\Dropbox (Personal)\\State_of_conservancies\\_revised_March2015\\final_figures_data\\ccy_expenditure_27Apr2015.eps")




# conservancy income
library(RColorBrewer)
rb1 <- brewer.pal(9,"Set1")
rb2 <- rb1[c(1,3,2,6,7,5,9,4,8)]
rb3 <- rev(rb2[1:4])
x_inc <- read.xlsx("Financial Overview.xlsx",sheet="income_summary")
x_inc$ksh1 <- round(x_inc$ksh/1000000,2)

x_inc <- droplevels(x_inc[x_inc$type=="Tourism"|x_inc$type=="Tourism"|x_inc$type=="Livestock Levy"|x_inc$type=="Grass seed sales"|x_inc$type=="Sand harvesting levies"|x_inc$type=="Bird shooting"|x_inc$type=="PES"|x_inc$type=="Other",])
x_inc <- x_inc[x_inc$ksh1 >0,]

lab <- unique(x_inc$type)
lab <- lab[c(1,4,2,3)]

windows(record=T)
p9 <- ggplot(x_inc,aes(x=year, y=ksh1, fill=type))  
p9 <- p9 +  geom_bar(stat="identity") 
p9 <- p9 +  xlab("") + ylab("KSh - Millions")
p9 <- p9 + ggtitle("Commercial Income")
p9 <- p9 +  scale_fill_manual(values=rev(c(nrt_red,nrt_blue,nrt_green1,nrt_yellow)),labels = rev(lab),"\n") +              
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "grey",lineend="butt",size=2)) +  
  theme(axis.title.x=element_text(vjust=0,family="", size=10, face="bold")) +
  theme(axis.title.y=element_text(vjust=1.5,family="", size=10, face="bold")) +
  theme(axis.text=element_text(family="", size=12, face="bold")) +
  theme(legend.position = c(0.2, 0.9))

p9

ggsave("C:\\Worden\\Dropbox (Personal)\\State_of_conservancies\\_revised_March2015\\final_figures_data\\ccy_income_27Apr2015.eps")


########################################################################
# PIKE


pike <- read.xlsx("PIKE.xlsx",sheet="Sheet1")
pike$perc <- pike$PIKE*100

windows(7,7,record=T)

op <- par
p10 <- ggplot(pike,aes(x = Year, y = perc)) + 
  geom_bar(aes(x=Year, y=perc),fill=nrt_green1,stat="identity",bg="white") +
  geom_text(aes(x=Year, y=perc, label=(paste(perc, "%")),parse = TRUE, vjust=-1),size=4) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "grey",lineend="butt",size=2)) +
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  theme(axis.title.y = element_blank()) +   # Remove x-axis label
  theme(axis.text=element_text(family="", size=12, face="plain"))+
  scale_y_continuous(limits=c(0,100),label=function(x){return(paste( x, "%"))})
 #stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 2,se=F)

p10

ggsave("C:\\Worden\\Dropbox (Personal)\\State_of_conservancies\\_revised_March2015\\final_figures_data\\PIKE_27Apr2015.eps")


##########################################################################
# wildlife sightings trends

library(grid)

spp <- read.csv("keysp10_14.csv", header=T)
s1 <- unique(spp$sp)


# regular sightings
# sightings displayed as 100's of sightings to make the y axis more clear

windows(6,4,record=T)

for(i in 1:length(s1)){
  ssp1 <- spp[spp$sp==s1[i],]
  ssp1$sum100 <- round(ssp1$sum/100,0)
  #ssp1$stan <- ssp1$sum
  #ssp1$stan <- (ssp1$sum-mean(ssp1$sum))/sd(ssp1$sum)
  #ssp1$stan <- (ssp1$sum-min(ssp1$sum))/(max(ssp1$sum)-min(ssp1$sum))
  
  my_grob = grobTree(textGrob(label=ssp1$sp, x=0.8,  y=0.9, hjust=0,
                              gp=gpar(col="grey30", fontsize=12, fontface="bold")))
  
  p11 <- ggplot(ssp1,aes(x = yr, y = sum100)) + 
    geom_bar(aes(x=yr, y=sum100),fill=nrt_red,stat="identity",bg="white",width=.6) +
    #geom_text(aes(x=year, y=stan, label=(paste(perc, "%")),parse = TRUE, vjust=-1),size=4) +
    #ylab("Sightings (100's)") +
    annotation_custom(my_grob) +
    theme_bw() + 
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "grey",lineend="butt",size=2)) +
    theme(axis.title.x = element_blank()) +   # Remove x-axis label
    theme(axis.title.y = element_blank()) +   # Remove x-axis label
    theme(axis.text=element_text(family="", size=10, face="plain")) +
    #scale_y_continuous(limits=c(0,100),label=function(x){return(paste( x, "%"))})+
    stat_smooth(method = "lm", formula = y ~ poly(x, 3), size = 2,se=F,colour=nrt_blue)
  print(p11)
  ggsave(paste("C:\\Worden\\Dropbox (Personal)\\State_of_conservancies\\soc_final_figures_April27\\wildlife_trends2\\",s1[i],"_27Apr2015.eps"))
  
}




