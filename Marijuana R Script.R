#Data on the Rocks
#April 2017
#Marijuana


#REQUIRED PACKAGES
library(xlsx) #package to import xls files directly
library(pastecs) #summary stats function stat.desc
library(ggplot2)
library(dplyr) #data wrangling
library(tidyr) #restructuring data
library(multilevel)

#CUSTOM FUNCTIONS
Sp.Desc <- function(data)
{
  print(t(stat.desc(data))[,-c(2,3,6,7,11,14)])
}

DoR.Theme <- function(axis.text.size=16, axis.title.size=16, title.size=20, legend.position="none")
{
  theme(panel.grid.major = element_line(colour="grey90"), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x=element_line(colour="black"), axis.line.y=element_line(colour="black"),
        axis.title.x=element_text(colour = "black", size=axis.title.size), axis.title.y=element_text(colour = "black", size=axis.title.size),
        axis.text.x=element_text(colour = "black", size=axis.text.size), axis.text.y=element_text(colour = "black", size=axis.text.size),
        plot.title=element_text(colour = "black",size=title.size, face="bold", hjust=.5),
        axis.ticks=element_line(colour="black"), legend.position = legend.position, legend.key=element_blank())
}


#DATA IMPORT----
#With only legalizing and surrounding states----
Fat.MVMD.W <- read.xlsx("C:/Users/sbuja/Documents/Data on the Rocks/Marijuana/Traffic Fatalities by State.xlsx", sheetName="Fat.MVMD")
Fat.MVMD.W <- na.exclude(Fat.MVMD.W) #Delete blank row which is there for unexplained reason
Sp.Desc(Fat.MVMD.W)
dim(Fat.MVMD.W)
write.csv(Fat.MVMD.W, "Fat.MVMD.W.csv", row.names=F)
#13 states

#Restructure from wide data frame to long
Fat.MVMD.L <- gather(Fat.MVMD.W, State, Fat.MVMD, Colorado:Montana, factor_key=TRUE)
Fat.MVMD.L
Sp.Desc(Fat.MVMD.L)
write.csv(Fat.MVMD.L, "Fat.MVMD.L.csv", row.names=F)

#Code based on the criminalization status of marijuana
#piecewise MLM 
#St.Decrim: L2 variable of whether it is a state that decriminalized
#PrePost2013: L1 binary variable of whether MJ was criminalized in that state in that year
#YearSeg1: L1 Year variable for slope part 1
#YearSeg2: L1 Year variable for slope part 2
#Need to delete Oregon 2015 data since that's after they decriminalized. 

Fat.MVMD.L$St.Decrim <- 0
Fat.MVMD.L$PrePost2013 <- 0
Fat.MVMD.L$YearSeg1 <- Fat.MVMD.L$Year - 2006
Fat.MVMD.L$YearSeg2 <- 0

for(i in 1:dim(Fat.MVMD.L)[1]){
  if(Fat.MVMD.L$State[i]=="Colorado" || Fat.MVMD.L$State[i]=="Washington") Fat.MVMD.L$St.Decrim[i] <- 1
  if(Fat.MVMD.L$Year[i]>=2013){
    Fat.MVMD.L$PrePost2013[i] <- 1
    Fat.MVMD.L$YearSeg1[i] <- 7
    Fat.MVMD.L$YearSeg2[i] <- Fat.MVMD.L$Year[i] - 2013
  } 
  if(Fat.MVMD.L$State[i]=="Oregon" && Fat.MVMD.L$Year[i]==2015) Fat.MVMD.L$Fat.MVMD[i] <- NA
}
Fat.MVMD.L
Fat.MVMD.L$YearSeg1.2 <- Fat.MVMD.L$YearSeg1^2
Sp.Desc(Fat.MVMD.L)

write.csv(Fat.MVMD.L, "Fat.MVMD.L.csv", row.names=F)

Traffic.MLM <-  lme(Fat.MVMD~YearSeg1 + YearSeg1.2 + YearSeg2 + PrePost2013, 
                    data=Fat.MVMD.L, na.action=na.exclude,
                    random = ~1|State,
                    control=list(maxIter=400))
summary(Traffic.MLM)

PredValues <- data.frame(Year=c(2006,2010,2013,2013,2015),
                         YearSeg1=c(0,4, 7,7,7), 
                         YearSeg2=c(0,0, 0,0,2),
                         PrePost2013=c(0,0,0,1,1),
                         State=rep("National",5))
PredValues$YearSeg1.2<- PredValues$YearSeg1^2
PredValues$Fatalities <- Traffic.MLM$coef$fixed["(Intercept)"] +
  Traffic.MLM$coef$fixed["YearSeg1"] * PredValues$YearSeg1 +
  Traffic.MLM$coef$fixed["YearSeg1.2"] * PredValues$YearSeg1.2 +
  Traffic.MLM$coef$fixed["YearSeg2"] * PredValues$YearSeg2 +
  Traffic.MLM$coef$fixed["PrePost2013"] * PredValues$PrePost2013
PredValues

#Checking the fit of the piecewise regression
Fat.MVMD.W$Average <- rowMeans(Fat.MVMD.W[,2:14], na.rm = FALSE)

ggplot(data=Fat.MVMD.L, aes(x=Year, y=Fat.MVMD, colour=State)) + 
  geom_line(size=1) + 
  stat_smooth(data=PredValues[1:3,], aes(x=Year, y=Fatalities), method = "lm", formula = y ~ poly(x, 2), size = 2, se=F, colour="black") +
  #geom_line(data=PredValues[1:3,], aes(x=Year, y=Fatalities), colour="black", size=2) + #Segment 1 of the MLM piecewise regression
  geom_line(data=PredValues[4:5,], aes(x=Year, y=Fatalities), colour="black", size=2) + #Segment 2 of the MLM piecewise regression
  #geom_point(data=Fat.MVMD.W, aes(x=Year, y=Average), size=3, colour="black") + #double checking piecewise regression
  scale_x_continuous("Year", limits=c(2006,2015), breaks=c(seq(2006,2015,2)), expand = c(0,0)) +
  scale_y_continuous("Traffic Fatalities per Million Vehicle Miles", limits=c(0.4,2.5), breaks=seq(.5,2.5,.5), expand = c(0,0)) +
  ggtitle("Traffic Fatalities by State") +
  DoR.Theme(legend.position = "right") +
  guides(colour = guide_legend(override.aes = list(size=4)))

#Adding in state decriminalization predictor
Traffic.MLM.Decrim <-  lme(Fat.MVMD~YearSeg1 + YearSeg1.2 + YearSeg2 + PrePost2013 + 
                             St.Decrim + St.Decrim:YearSeg1 + St.Decrim:YearSeg1.2 + St.Decrim:YearSeg2 + St.Decrim:PrePost2013, 
                           data=Fat.MVMD.L, na.action=na.exclude,
                           random = ~1|State,
                           control=list(maxIter=400))
summary(Traffic.MLM.Decrim)
#                           Value  Std.Error  DF   t-value p-value
# (Intercept)            1.6896537 0.08100906 108 20.857589  0.0000
# YearSeg1              -0.1517532 0.02487267 108 -6.101205  0.0000
# YearSeg1.2             0.0139177 0.00398281 108  3.494451  0.0007
# YearSeg2               0.0224003 0.02650444 108  0.845154  0.3999
# PrePost2013           -0.0598477 0.06595784 108 -0.907363  0.3662
# St.Decrim             -0.5592965 0.20653339  11 -2.708020  0.0204
# YearSeg1:St.Decrim     0.0755032 0.06341311 108  1.190657  0.2364
# YearSeg1.2:St.Decrim  -0.0083820 0.01015422 108 -0.825473  0.4109
# YearSeg2:St.Decrim     0.0375997 0.06608173 108  0.568987  0.5705
# PrePost2013:St.Decrim  0.0686573 0.16809429 108  0.408445  0.6838

PredValues <- data.frame(Year=rep(c(2006,2010,2013,2013,2015),2),
                         YearSeg1=rep(c(0,4,7,7,7),2), 
                         YearSeg2=rep(c(0,0,0,0,2),2),
                         PrePost2013=rep(c(0,0,0,1,1),2),
                         St.Decrim=c(rep(0,5),rep(1,5)))
PredValues$YearSeg1.2 <- PredValues$YearSeg1^2
PredValues$Fatalities <- Traffic.MLM.Decrim$coef$fixed["(Intercept)"] +
  Traffic.MLM.Decrim$coef$fixed["YearSeg1"] * PredValues$YearSeg1 +
  Traffic.MLM.Decrim$coef$fixed["YearSeg1.2"] * PredValues$YearSeg1.2 +
  Traffic.MLM.Decrim$coef$fixed["YearSeg2"] * PredValues$YearSeg2 +
  Traffic.MLM.Decrim$coef$fixed["PrePost2013"] * PredValues$PrePost2013 +
  Traffic.MLM.Decrim$coef$fixed["St.Decrim"] * PredValues$St.Decrim +
  Traffic.MLM.Decrim$coef$fixed["YearSeg1:St.Decrim"] * PredValues$YearSeg1 * PredValues$St.Decrim +
  Traffic.MLM.Decrim$coef$fixed["YearSeg1.2:St.Decrim"] * PredValues$YearSeg1.2 * PredValues$St.Decrim +
  Traffic.MLM.Decrim$coef$fixed["YearSeg2:St.Decrim"] * PredValues$YearSeg2 * PredValues$St.Decrim +
  Traffic.MLM.Decrim$coef$fixed["PrePost2013:St.Decrim"] * PredValues$PrePost2013 * PredValues$St.Decrim
PredValues

#Checking the fit of the piecewise regression
Fat.MVMD.W$D.Average <- rowMeans(Fat.MVMD.W[c("Colorado", "Washington")], na.rm = FALSE)
Fat.MVMD.W$ND.Average <- rowMeans(Fat.MVMD.W[c("Utah", "Wyoming", "Arizona", "New.Mexico", "Oklahoma", 
                                               "Kansas", "Nebraska", "Texas", "Oregon", "Idaho", "Montana")], na.rm = FALSE)

#Difference in overall average rates
(mean(Fat.MVMD.W$ND.Average) - mean(Fat.MVMD.W$D.Average)) / mean(Fat.MVMD.W$ND.Average)

#Adding in State Abbreviations test
Fat.MVMD.2015 <- subset(Fat.MVMD.L, Year == 2015)
Fat.MVMD.2015$Fat.MVMD[11] <- 1.03 #impute oregon 2014 value
Fat.MVMD.2015$St.Abr <- c("CO", "UT", "WY", "AZ", "NM", "OK", "KS", "NE", "TX", "WA", "OR", "ID", "MT")
Fat.MVMD.2015$X <- c(2015.3, 2015.7, 2015.3, 2015.3, 2015.7, 2015.3, 2015.3,  2015.3, 2015.7, 2015.3, 2015.7, 2015.3, 2015.3)
#for overlap arizona needs to be moved up a bit
Fat.MVMD.2015$Fat.MVMD[4] <-Fat.MVMD.2015$Fat.MVMD[4] +.03

#GRAPHING DATA----
colorscale=c("#089126","navyblue","navyblue","navyblue","navyblue","navyblue","navyblue","navyblue","navyblue",
             "#089126","navyblue","navyblue","navyblue")
Marijuana.plot <- ggplot(data=Fat.MVMD.L, aes(x=Year, y=Fat.MVMD, colour=State)) + 
  geom_vline(xintercept=2013, colour="grey50", linetype="11", size=1.5) + 
  geom_line(size=1.5, alpha=.2) + 
  stat_smooth(data=PredValues[1:3,], aes(x=Year, y=Fatalities), method = "lm", formula = y ~ poly(x, 2), size = 3, se=F, colour="navyblue") + #Surround State Segment 1 #Surround State Segment 1
  stat_smooth(data=PredValues[4:5,], aes(x=Year, y=Fatalities), method = "lm", size = 3, se=F, colour="navyblue") + #Surround State Segment 2
  stat_smooth(data=PredValues[6:8,], aes(x=Year, y=Fatalities), method = "lm", formula = y ~ poly(x, 2), size = 3, se=F, colour="#089126") + #Surround State Segment 1 #Surround State Segment 1
  stat_smooth(data=PredValues[9:10,], aes(x=Year, y=Fatalities), method = "lm", size = 3, se=F, colour="#089126") + #Surround State Segment 2
  geom_text(data=Fat.MVMD.2015, aes(x=X, y=Fat.MVMD, label=St.Abr), size=4, fontface="bold") +
  annotate("text", label="Before\nLegalization", x=2011.4, y=2.3, colour="grey50", size=6, fontface="bold", hjust=.5, vjust=.5) + 
  annotate("text", label="After\nLegalization", x=2014.6, y=2.3, colour="grey50", size=6, fontface="bold", hjust=.5, vjust=.5) + 
  annotate("rect", fill="white", alpha=.8, xmin = 2009.4, xmax = 2012.6, ymin = 1.53, ymax = 1.67) +
  annotate("text", label="Surrounding States", x=2011, y=1.6, colour="navyblue", size=6, fontface="bold", hjust=.5, vjust=.5) + 
  annotate("text", label="States with Legal Marijuana", x=2012.4, y=0.6, colour="#089126", size=6, fontface="bold", hjust=1, vjust=0) + 
  scale_colour_manual(values=colorscale) + 
  scale_x_continuous("Year", limits=c(2006,2016), breaks=c(seq(2006,2015,2),2015), expand = c(0,0)) +
  scale_y_continuous("Traffic Fatalities per 100 Million Miles Driven", limits=c(0.4,2.5), breaks=seq(.5,2.5,.5), expand = c(0,0)) +
  ggtitle("Marijuana Legalization and Traffic Fatalities") +
  DoR.Theme()
Marijuana.plot
#guides(colour = guide_legend(override.aes = list(size=4, alpha=1)))
ggsave(Marijuana.plot, filename="Marijuana.plot.Final.png", width = 8, height=7, dpi=500)




#Explore with all states----
#All States
#Break into 3 groups, Legal, Surrounding, Other
Fatal.All.W <- read.xlsx("C:/Users/sbuja/Documents/Data on the Rocks/Marijuana/Traffic Fatalities by State Full.xlsx", sheetName="Sheet1")
Sp.Desc(Fatal.All.W)
dim(Fatal.All.W)

Fatal.All.L <- gather(Fatal.All.W, Year, Fat.MVMD, F.2006:F.2015, factor_key=TRUE)
dim(Fatal.All.L) #500 x 4

#Make Year variable a number
Fatal.All.L<-mutate(Fatal.All.L,Year=as.character(Year))
Fatal.All.L<-mutate(Fatal.All.L,Year=sapply(strsplit(Fatal.All.L$Year, split='.', fixed=TRUE),function(x) (x[2])))
Fatal.All.L$Year <- as.double(Fatal.All.L$Year)

#sort by state
Fatal.All.L <- Fatal.All.L[order(Fatal.All.L$State),]

#Check data
View(Fatal.All.L)
Sp.Desc(Fatal.All.L)

#Variable coding: piecewise MLM 
#St.Decrim: L2 variable of whether state decriminalized (vs. "other" reference group)
#St.Surround: L2 variable of whether state surrounds a decriminalized state (vs. "other" reference group)
#PrePost2013: L1 binary variable of whether first or second line segment. 
#YearSeg1: L1 Year variable for slope part 1
#YearSeg2: L1 Year variable for slope part 2
#Need to delete Oregon 2015 data since that's after they decriminalized. 

#Default values
Fatal.All.L$St.Decrim <- 0
Fatal.All.L$St.Surround <- 0
Fatal.All.L$St.Group <- "Other"
Fatal.All.L$PrePost2013 <- 0
Fatal.All.L$YearSeg1 <- Fatal.All.L$Year - 2006
Fatal.All.L$YearSeg2 <- 0

Decrim.States <- c("Colorado", "Washington")
Surround.States <- c("Utah", "Wyoming", "Arizona", "New Mexico", "Oklahoma", 
                     "Kansas", "Nebraska", "Texas", "Oregon", "Idaho", "Montana")

for(i in 1:dim(Fatal.All.L)[1]){
  if(Fatal.All.L$State[i] %in% Decrim.States) {
    Fatal.All.L$St.Decrim[i] <- 1
    Fatal.All.L$St.Group[i] <- "Legal"
  }
  if(Fatal.All.L$State[i] %in% Surround.States) {
    Fatal.All.L$St.Surround[i] <- 1
    Fatal.All.L$St.Group[i] <- "Surround"
  }
  
  if(Fatal.All.L$Year[i]>=2013){
    Fatal.All.L$PrePost2013[i] <- 1
    Fatal.All.L$YearSeg1[i] <- 7
    Fatal.All.L$YearSeg2[i] <- Fatal.All.L$Year[i] - 2013
  } 
  if(Fatal.All.L$State[i]=="Oregon" && Fatal.All.L$Year[i]==2015) Fatal.All.L$Fat.MVMD[i] <- NA
}
Fatal.All.L$St.Group <- factor(Fatal.All.L$St.Group, levels=c("Other","Surround","Legal")) #order factors
Fatal.All.L$YearSeg1.2 <- Fatal.All.L$YearSeg1^2
table(Fatal.All.L$St.Decrim) #should be 20
table(Fatal.All.L$St.Surround) #should be 110
table(Fatal.All.L$St.Group)
View(Fatal.All.L)
Sp.Desc(Fatal.All.L)

#examine overall piecewise fit
#Seems to work fine. 
Traffic.MLM <-  lme(Fat.MVMD~YearSeg1 + YearSeg1.2 + YearSeg2 + PrePost2013, 
                    data=Fatal.All.L, na.action=na.exclude,
                    random = ~1|State,
                    control=list(maxIter=400))
summary(Traffic.MLM)

PredValues <- data.frame(Year=c(2006,2010,2013,2013,2015),
                         YearSeg1=c(0,4, 7,7,7), 
                         YearSeg2=c(0,0, 0,0,2),
                         PrePost2013=c(0,0,0,1,1),
                         State=rep("National",5))
PredValues$YearSeg1.2<- PredValues$YearSeg1^2
PredValues$Fatalities <- Traffic.MLM$coef$fixed["(Intercept)"] +
  Traffic.MLM$coef$fixed["YearSeg1"] * PredValues$YearSeg1 +
  Traffic.MLM$coef$fixed["YearSeg1.2"] * PredValues$YearSeg1.2 +
  Traffic.MLM$coef$fixed["YearSeg2"] * PredValues$YearSeg2 +
  Traffic.MLM$coef$fixed["PrePost2013"] * PredValues$PrePost2013
PredValues

Yr.Means <- data.frame(Year=c(2006:2015), Average=colMeans(Fatal.All.W[,3:12], na.rm = FALSE))

ggplot(data=Fatal.All.L, aes(x=Year, y=Fat.MVMD , colour=State)) + 
  geom_line(size=1) + 
  stat_smooth(data=PredValues[1:3,], aes(x=Year, y=Fatalities), method = "lm", formula = y ~ poly(x, 2), size = 2, se=F, colour="black") +
  #geom_line(data=PredValues[1:3,], aes(x=Year, y=Fatalities), colour="black", size=2) + #Segment 1 of the MLM piecewise regression
  geom_line(data=PredValues[4:5,], aes(x=Year, y=Fatalities), colour="black", size=2) + #Segment 2 of the MLM piecewise regression
  geom_point(data=Yr.Means, aes(x=Year, y=Average), colour="black", size=5) +
  scale_x_continuous("Year", limits=c(2006,2015), breaks=c(seq(2006,2015,2)), expand = c(0,0)) +
  scale_y_continuous("Traffic Fatalities per Million Vehicle Miles", limits=c(0.4,2.5), breaks=seq(.5,2.5,.5), expand = c(0,0)) +
  ggtitle("Traffic Fatalities by State") +
  DoR.Theme(legend.position = "right") +
  guides(colour = guide_legend(override.aes = list(size=4)))

#Adding in state level predictors
Traffic.MLM.DecSur <-  lme(Fat.MVMD~YearSeg1 + YearSeg1.2 + YearSeg2 + PrePost2013 + 
                             St.Decrim + St.Decrim:YearSeg1 + St.Decrim:YearSeg1.2 + St.Decrim:YearSeg2 + St.Decrim:PrePost2013 +
                             St.Surround + St.Surround:YearSeg1 + St.Surround:YearSeg1.2 + St.Surround:YearSeg2 + St.Surround:PrePost2013, 
                           data=Fatal.All.L, na.action=na.exclude,
                           random = ~1|State,
                           control=list(maxIter=400))
summary(Traffic.MLM.DecSur)
#                             Value  Std.Error  DF    t-value p-value
# (Intercept)              1.4711712 0.05181024 437  28.395376  0.0000
# YearSeg1                -0.1302124 0.01262712 437 -10.312120  0.0000
# YearSeg1.2               0.0129408 0.00202196 437   6.400135  0.0000
# YearSeg2                 0.0029730 0.01310378 437   0.226879  0.8206
# PrePost2013             -0.0982883 0.03346935 437  -2.936666  0.0035
# St.Decrim               -0.3408140 0.22878781  47  -1.489651  0.1430
# St.Surround              0.2184825 0.10822806  47   2.018723  0.0492
# YearSeg1:St.Decrim       0.0539624 0.05575984 437   0.967764  0.3337
# YearSeg1.2:St.Decrim    -0.0074051 0.00892872 437  -0.829355  0.4074
# YearSeg2:St.Decrim       0.0570270 0.05786473 437   0.985523  0.3249
# PrePost2013:St.Decrim    0.1070978 0.14779663 437   0.724630  0.4691
# YearSeg1:St.Surround    -0.0215409 0.02637719 437  -0.816648  0.4146
# YearSeg1.2:St.Surround   0.0009770 0.00422373 437   0.231300  0.8172
# YearSeg2:St.Surround     0.0192838 0.02794157 437   0.690148  0.4905
# PrePost2013:St.Surround  0.0384884 0.06994017 437   0.550305  0.5824

PredValues <- data.frame(Year=rep(c(2006,2010,2013,2013,2015),3),
                         YearSeg1=rep(c(0,4,7,7,7),3), 
                         YearSeg2=rep(c(0,0,0,0,2),3),
                         PrePost2013=rep(c(0,0,0,1,1),3),
                         St.Decrim=c(rep(0,10),rep(1,5)),
                         St.Surround=c(rep(0,5),rep(1,5),rep(0,5)),
                         St.Group=c(rep("Other",5),rep("Surround",5),rep("Legal",5)),
                         State=rep("National",15))
PredValues$YearSeg1.2<- PredValues$YearSeg1^2
PredValues$St.Group <- factor(PredValues$St.Group, levels=c("Other","Surround","Legal")) #order factors
PredValues$Fatalities <- Traffic.MLM.DecSur$coef$fixed["(Intercept)"] +
  Traffic.MLM.DecSur$coef$fixed["YearSeg1"] * PredValues$YearSeg1 +
  Traffic.MLM.DecSur$coef$fixed["YearSeg1.2"] * PredValues$YearSeg1.2 +
  Traffic.MLM.DecSur$coef$fixed["YearSeg2"] * PredValues$YearSeg2 +
  Traffic.MLM.DecSur$coef$fixed["PrePost2013"] * PredValues$PrePost2013 +
  Traffic.MLM.DecSur$coef$fixed["St.Decrim"] * PredValues$St.Decrim +
  Traffic.MLM.DecSur$coef$fixed["YearSeg1:St.Decrim"] * PredValues$YearSeg1 * PredValues$St.Decrim +
  Traffic.MLM.DecSur$coef$fixed["YearSeg1.2:St.Decrim"] * PredValues$YearSeg1.2 * PredValues$St.Decrim +
  Traffic.MLM.DecSur$coef$fixed["YearSeg2:St.Decrim"] * PredValues$YearSeg2 * PredValues$St.Decrim +
  Traffic.MLM.DecSur$coef$fixed["PrePost2013:St.Decrim"] * PredValues$PrePost2013 * PredValues$St.Decrim +
  Traffic.MLM.DecSur$coef$fixed["St.Surround"] * PredValues$St.Surround +
  Traffic.MLM.DecSur$coef$fixed["YearSeg1:St.Surround"] * PredValues$YearSeg1 * PredValues$St.Surround +
  Traffic.MLM.DecSur$coef$fixed["YearSeg1.2:St.Surround"] * PredValues$YearSeg1.2 * PredValues$St.Surround +
  Traffic.MLM.DecSur$coef$fixed["YearSeg2:St.Surround"] * PredValues$YearSeg2 * PredValues$St.Surround +
  Traffic.MLM.DecSur$coef$fixed["PrePost2013:St.Surround"] * PredValues$PrePost2013 * PredValues$St.Surround
PredValues

#Checking the fit of the piecewise regression
Fat.MVMD.W$D.Average <- rowMeans(Fat.MVMD.W[c("Colorado", "Washington")], na.rm = FALSE)
Fat.MVMD.W$ND.Average <- rowMeans(Fat.MVMD.W[c("Utah", "Wyoming", "Arizona", "New.Mexico", "Oklahoma", 
                                               "Kansas", "Nebraska", "Texas", "Oregon", "Idaho", "Montana")], na.rm = FALSE)

#Determining color order
Groups <- Fatal.All.L[which(Fatal.All.L$Year==2015),]$St.Group
#colors: Other = #112BB2, Surround = #FF784B, Legal = #5EB21A
colorscale <- ifelse(Groups=="Other", "#112BB2", ifelse(Groups=="Surround", "#FF784B", "#5EB21A"))
table(colorscale)

#GRAPHING DATA----
Marijuana.plot <- ggplot(data=Fatal.All.L, aes(x=Year, y=Fat.MVMD, colour=State)) + 
  geom_vline(xintercept=2013, colour="grey50", linetype="11", size=1.5) + 
  geom_line(size=1.5, alpha=.2) + 
  stat_smooth(data=PredValues[1:3,], aes(x=Year, y=Fatalities), method = "lm", 
              formula = y ~ poly(x, 2), size = 3, se=F, colour="#112BB2") + #Other State Segment 1
  stat_smooth(data=PredValues[4:5,], aes(x=Year, y=Fatalities), method = "lm", size = 3, se=F, colour="#112BB2") + #Other State Segment 2
  stat_smooth(data=PredValues[6:8,], aes(x=Year, y=Fatalities), method = "lm", 
              formula = y ~ poly(x, 2), size = 3, se=F, colour="#FF784B") + #Surround State Segment 1
  stat_smooth(data=PredValues[9:10,], aes(x=Year, y=Fatalities), method = "lm", size = 3, se=F, colour="#FF784B") + #Surround State Segment 2
  stat_smooth(data=PredValues[11:13,], aes(x=Year, y=Fatalities), method = "lm", 
              formula = y ~ poly(x, 2), size = 3, se=F, colour="#5EB21A") + #Legal State Segment 1
  stat_smooth(data=PredValues[14:15,], aes(x=Year, y=Fatalities), method = "lm", size = 3, se=F, colour="#5EB21A") + #Legal State Segment 2
  #geom_line(data=PredValues[1:2,], aes(x=Year, y=Fatalities), colour="#112BB2", size=3) + #Other State Segment 1
  #geom_line(data=PredValues[3:4,], aes(x=Year, y=Fatalities), colour="#112BB2", size=3) + #Other State Segment 2
  #geom_line(data=PredValues[5:6,], aes(x=Year, y=Fatalities), colour="#FF784B", size=3) + #Surround State Segment 1
  #geom_line(data=PredValues[7:8,], aes(x=Year, y=Fatalities), colour="#FF784B", size=3) + #Surround State Segment 2
  #geom_line(data=PredValues[9:10,], aes(x=Year, y=Fatalities), colour="#5EB21A", size=3) + #Legal State Segment 1
  #geom_line(data=PredValues[11:12,], aes(x=Year, y=Fatalities), colour="#5EB21A", size=3) + #Legal State Segment 2
  #geom_text(data=Fat.MVMD.2015, aes(x=X, y=Fat.MVMD, label=St.Abr), size=4, fontface="bold") +
  annotate("text", label="Before\nLegalization", x=2011.3, y=2.3, colour="grey50", size=6, fontface="bold", hjust=.5, vjust=.5) + 
  annotate("text", label="After\nLegalization", x=2014.1, y=2.3, colour="grey50", size=6, fontface="bold", hjust=.5, vjust=.5) + 
  annotate("rect", fill="white", alpha=.8, xmin = 2006.5, xmax = 2009.7, ymin = 1.83, ymax = 1.97) +
  annotate("text", label="Surrounding States", x=2008, y=1.9, colour="#FF784B", size=6, fontface="bold", hjust=.5, vjust=.5) + 
  annotate("rect", fill="white", alpha=.8, xmin = 2010.5, xmax = 2012.5, ymin = 1.53, ymax = 1.67) +
  annotate("text", label="Other States", x=2011.5, y=1.6, colour="#112BB2", size=6, fontface="bold", hjust=.5, vjust=.5) + 
  annotate("rect", fill="white", alpha=.8, xmin = 2007.9, xmax = 2012.1, ymin = 0.53, ymax = 0.67) +
  annotate("text", label="States with Legal Marijuana", x=2010, y=0.6, colour="#5EB21A", size=6, fontface="bold", hjust=.5, vjust=.5) + 
  scale_colour_manual(values=colorscale) + 
  scale_x_continuous("Year", limits=c(2006,2015), breaks=c(seq(2006,2015,2)), expand = c(0,0)) +
  scale_y_continuous("Traffic Fatalities per 100 Million Miles Driven", limits=c(0.4,2.5), breaks=seq(.5,2.5,.5), expand = c(0,0)) +
  ggtitle("Marijuana Legalization and Traffic Deaths") +
  DoR.Theme()
Marijuana.plot
ggsave(Marijuana.plot, filename="Marijuana.plot2.png", width = 8, height=7, dpi=500)






