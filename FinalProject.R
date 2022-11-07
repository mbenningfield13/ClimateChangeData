#?:Does the political party or geographic location play a greater role in 
#defining attitudes towards climate change?
#Difference in means for Dem/GOP 
#lm for geographic location


#libraries
library(ggplot2)
library(dplyr)

#Set wd
setwd("~/Stats2022")

#Load data
dataLat<-read.csv("YaleProgramCLimateLat.csv")
region<-read.csv("YaleProgramClimateRegion.csv")
View(dataLat)
View(region)
#Transform group into boolean
dataLat$dem[dataLat$Group=="Dem"]<-1
dataLat$dem[dataLat$Group=="Rep"]<-0

dataLat$rep[dataLat$Group=="Dem"]<-0
dataLat$rep[dataLat$Group=="Rep"]<-1

#create region column
dataLat$region[region$northeast==1]<-"Northeast"
dataLat$region[region$west==1]<-"West"
dataLat$region[region$southeast==1]<-"Southeast"
dataLat$region[region$midwest==1]<-"Midwest"
dataLat$region[region$southwest==1]<-"Southwest"

avgdata$region<-NA
avgdata$Region<-dataLat$region[(dataLat$GEOID<=56 & dataLat$GEOID>=1 & dataLat$Group=="Dem") & !(dataLat$GeoName=="Alaska"|dataLat$GeoName=="Hawaii")]

#states only dataset
statesdata<-dataLat[dataLat$GeoType=="State",]

#avg dem/gop data
avgdata<-read.csv("avgdata.csv")
#remove alaska and hawaii
avgdata = avgdata[!avgdata$GeoName=="Alaska",]
avgdata = avgdata[!avgdata$GeoName=="Hawaii",]

View(statesdata)
#T tests for worried
t.test(worried~dem, data=dataLat)
t.test(worried~rep, data=data)

#T tests for Happening
t.test(happening~dem, data=dataLat)
t.test(happening~rep, data=data)

#T tests for Regulate
t.test(regulate~dem, data=dataLat)
t.test(regulate~rep, data=data)

#T tests for Funding
t.test(fundrenewables~dem, data=dataLat)
t.test(fundrenewables~rep, data=data)

#Plot Dem vs Worried T Test
dataSummary<-dataLat[dataLat$GEOID=="9999",]%>%
  group_by(dem)%>%
  summarise(meanWor=mean(worried), sdWor=sd(worried), nWor=n(), 
            SEWor=sd(sdWor)/sqrt(nWor))

#Dem v happening t test
dataSummary<-dataLat[dataLat$GEOID=="9999",]%>%
  group_by(dem)%>%
  summarise(meanWor=mean(happening), sdWor=sd(happening), nWor=n(), 
            SEWor=sd(sdWor)/sqrt(nWor))

#Plot Dem vs Worried T Test
dataSummary<-dataLat[dataLat$GEOID=="9999",]%>%
  group_by(dem)%>%
  summarise(meanWor=mean(fundrenewables), sdWor=sd(fundrenewables), nWor=n(), 
            SEWor=sd(sdWor)/sqrt(nWor))

#Dem v regulate t test
dataSummary<-dataLat[dataLat$GEOID=="9999",]%>%
  group_by(dem)%>%
  summarise(meanWor=mean(regulate), sdWor=sd(regulate), nWor=n(), 
            SEWor=sd(sdWor)/sqrt(nWor))


#graph most recent t test
ggplot(dataSummary, aes(x=dem, y=meanWor))+
  geom_col(aes(fill=factor(dem)))+
  labs(title = "The effect of politcal party on nationwide estimated percentage 
who somewhat/strongly support funding research into 
renewable energy sources",
  x = "Republican                             Democrat", 
  y = "Percent Supporting Funding (%)")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none")+
  scale_fill_manual(values = c("#E81B23", "#00AEF3"))

#regression latitude v avgdata
reg1<-lm(avgWorried~lat, data=avgdata)
summary(reg1)
plot(reg1, which=1)

reg2<-lm(avgHappening~lat, data=avgdata)
summary(reg2)
plot(reg2, which=1)

reg3<-lm(avgFunding~lat, data=avgdata)
summary(reg3)
plot(reg3, which=1)

reg4<-lm(avgRegulate~lat, data=avgdata)
summary(reg4)
plot(reg4, which=1)

#graph regression lat v. avgdata
ggplot(avgdata, aes(x=lat*111,139, y=avgWorried))+
  geom_point(aes(color=Region))+
  geom_smooth(method="lm")+#Create a OLS line of best fit
  labs(title = "Effect of US states's distance from the equator on 
estimated percentage who are somewhat/very worried about 
global warming",
caption="*Alaska and Hawaii excluded",
x = "Distance (m)", y = "Percent Worried (%)")+
  geom_vline(xintercept = 36*111,139)+
  annotate("text", x=4025, y=55, 
           label="36th Parellel", fontface=2, angle=90, size=3)+
  scale_color_manual(values = c("#dd614a", "#313638", "#f6bd60", 
                                "#76949f", "#92dce5"))

ggplot(avgdata, aes(x=lat*111,139, y=avgRegulate))+
  geom_point(aes(color=Region))+
  geom_smooth(method="lm")+#Create a OLS line of best fit
  labs(title = "Effect of US states's distance from the equator on estimated 
percentage who somewhat/strongly support regulating CO2 as a pollutant",
      caption="*Alaska and Hawaii excluded",
      x = "Distance (m)", y = "Percent Supporting Regulation (%)")+
  geom_vline(xintercept = 36*111,139)+
  annotate("text", x=4025, y=75, 
           label="36th Parellel", fontface=2, angle=90, size=3)+
  scale_color_manual(values = c("#dd614a", "#313638", "#f6bd60", 
                                "#76949f", "#92dce5"))

ggplot(avgdata, aes(x=lat*111,139, y=avgFunding))+
  geom_point(aes(color=Region))+
  geom_smooth(method="lm")+#Create a OLS line of best fit
  labs(title = "Effect of US states's distance from the equator on estimated 
percentage who somewhat/strongly support funding research into 
renewable energy sources",
    caption="*Alaska and Hawaii excluded",
    x = "Distance (m)", y = "Percent Supporting Funding (%)")+
  geom_vline(xintercept = 36*111,139)+
  annotate("text", x=4025, y=82, 
           label="36th Parellel", fontface=2, angle=90, size=3)+
  scale_color_manual(values = c("#dd614a", "#313638", "#f6bd60", 
                                "#76949f", "#92dce5"))

ggplot(avgdata, aes(x=lat*111,139, y=avgHappening))+
  geom_point(aes(color=Region))+
  geom_smooth(method="lm")+#Create a OLS line of best fit
  labs(title = "Effect of US states's distance from the equator on estimated 
percentage who think that global warming is happening",
    caption="*Alaska and Hawaii excluded",
    x = "Distance (m)", y = "Percent Believing (%)")+
  geom_vline(xintercept = 36*111,139)+
  annotate("text", x=4025, y=60, 
           label="36th Parellel", fontface=2, angle=90, size=3)+
  scale_color_manual(values = c("#dd614a", "#313638", "#f6bd60", 
                                "#76949f", "#92dce5"))



