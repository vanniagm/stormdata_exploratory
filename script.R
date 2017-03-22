download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","repdata%2Fdata%2FStormData.csv.bz2")
data<-read.csv("repdata%2Fdata%2FStormData.csv.bz2",sep=",",header=TRUE,stringsAsFactors = FALSE)#[,c(2,6,7,8,23:28)]
vars<-names(data)
nrow(data)

#Select only those that are useful to analyse health and property damages
library(lubridate)
setClass("myDate")
setAs("character","myDate", function(from) as.Date(from, format="%m/%d/%Y %H:%M:%S",origin="1950-01-01") )
data<-read.csv("repdata%2Fdata%2FStormData.csv.bz2",sep=",",header=TRUE,colClasses = c("NULL","myDate",rep("NULL",4),"factor","factor",rep("NULL",14),rep("numeric",3),"factor","numeric","factor",rep("NULL",7),"character","NULL"))


# Cleaning the data
#All event types were actually started to be recorder from 1993, this is according to
# the NOAA web site http://www.ncdc.noaa.gov/stormevents/details.jsp?type=collection. For that reason in this exploratory analysis only events starting from 1993 will be compared.

#To show this let's explore data befor and after 1993, comparing the number of ddiiferen levels in the factor variable "EVTYPE" before and after 1993, and 1996

str(data$EVTYPE)
#958 levels

df5093<-data[data$BGN_DATE<="1993-01-01" & data$BGN_DATE>="1950-01-01","EVTYPE"]
df5093<-factor(df5093)
# Number of levels
str(df5093)
# 11 levels

df9396<-data[data$BGN_DATE<="1996-01-01" & data$BGN_DATE>="1993-01-01","EVTYPE"]
df9396<-factor(df9396)
str(df9396)
#601 levels

# Therefore, my opinion is that we should include those events starting from 1993


#Select data starting at 1993
df<-data[data$BGN_DATE>="1993-01-01",]

# As we are interested only in those event records that cause any type of health or property damage, events with 0 fatalities, injuries or damages will be excluded

#Check for NA or NULL values
vars<-names(df)
checknanull<-sapply(vars[4:9],function(x){c(sum(is.na(df[x])),sum(is.null(df[x])))})

#About exponential values
# Several analysis have been done by (David Hood or Eddie Song) on the interpretation of exponential values different from M,m=E6, B,b=E9, K,k=E3 or H,h=E2
# may by a typo when filling the database. Most of the numeric values in the EXP variables can be interpreted as numeric:{1,9} =E1, ? = NA, - = NA, += E0, however on an updated 
# Let us look at the frequency of these levels

table(df$PROPDMGEXP)
table(df$CROPDMGEXP)

# We can see that when compared to PROPDMEXP= E9,E6 or CROPDMEXP =E9,E6, all the numeric values, and the "?,+,-" which are within [E0,E1], are not significant. Therefore we can ignore them. 
# The null values are all equal to NA on total damages cost, so we will exclude them too.

expsubsetprop<-grep("\\?|\\+|-|^\\s*$|[0-9]",df$PROPDMGEXP)
expsubsetcrop<-grep("\\?|\\+|-|^\\s*$|[0-9]",df$CROPDMGEXP)
df$PROPDMG[expsubsetprop]<-0 #replace all the corresponding PROPDMG or CROPDMG with 0 
df$CROPDMG[expsubsetcrop]<-0
df$PROPDMGEXP[expsubsetprop]<-0 #replace all the corresponding PROPDMG or CROPDMG with 0 
df$CROPDMGEXP[expsubsetcrop]<-0
df$PROPDMGEXP<-toupper(df$PROPDMGEXP)
df$CROPDMGEXP<-toupper(df$CROPDMGEXP)

#exclude those from which there was no damage whatsoever
dfdam<-df[df$FATALITIES!=0 | df$INJURIES!=0 | df$PROPDMG!=0 | df$CROPDMG!=0, 1:9]
#dfhealth<-df[df$FATALITIES!=0 | df$INJURIES!=0,]
#dfdamg<-df[df$PROPDMG !=0| df$CROPDMG !=0, ]
#Replace {H,K,M,B} with {10^2,10^3,10^6,10^9}
# Replace PROPDMG and CROPDMG with the corresponding values: PROPDMG * PROPDMGEXP
library(plyr)
dfdam$PROPDMGEXP<-revalue(dfdam$PROPDMGEXP, c("B"="E9", "M"="E6","K"="E3","H"="E2")) # or levels(dfdam$PROPDMGEXP)<-c("E9","E2","E3","E6")
dfdam$CROPDMGEXP<-revalue(dfdam$CROPDMGEXP, c("B"="E9", "M"="E6","K"="E3"))
dfdam$PROPDMG <- do.call(paste, c(dfdam[c("PROPDMG","PROPDMGEXP")], sep=""))
dfdam$CROPDMG <- do.call(paste, c(dfdam[c("CROPDMG","CROPDMGEXP")], sep=""))
dfdam$PROPDMG<-as.numeric(dfdam$PROPDMG)
dfdam$CROPDMG<-as.numeric(dfdam$CROPDMG)
dfdam<-dfdam[-c(7,9)]


# The storm database has actually 48 type of events described in the NOAA website, so we should match the 985 levels with those 48 levels 
dfdam$EVTYPE<-toupper(dfdam$EVTYPE)
numeventtype<-unique(dfdam$EVTYPE)

#export reference list for event types in table 2.1.1 from the National Weather Service Storm Data Documentation
EVTYPEREF<-read.csv("\files\evtyperef.csv",header=TRUE,colClasses = "character");EVTYPEREF<-toupper(EVTYPEREF$EVTYPEREF)
# Let us take a first look on approximate matches with the reference list above from all the evtype values
uniquematches<-lapply(EVTYPEREF,function(x){unique(grep(x,dfdam$EVTYPE,value = TRUE))})
#uniquematches
#FIRST I SEARCH FOR EACH OF THE MOST SEVERE EVENTS AND START CATEGORIZING BY SEVERENESS, FOR EXAMPLE IF 
# AN EVENT TYPE IS DESCRIBED AS HEAVY RAIN/WINTER STORM, THEN I CATEGORIZE IT AS WINTER STORM
dfdam$EVTYPE[grep("ASTRONOMICAL LOW TIDE|LOW TIDE",dfdam$EVTYPE)]<-"ASTRONOMICAL LOW T"
dfdam$EVTYPE[grep("HIGH WATER|SEAS|HIGH TIDE|SURF|SWELL|WAVE|RISING",dfdam$EVTYPE)]<-"HIGH SURF"
dfdam$EVTYPE[grep("(?<!LOW )(?<! HIGH)TIDE",dfdam$EVTYPE,perl=TRUE)]<-"SURGE/TIDE"
dfdam$EVTYPE[grep("(?<!COASTAL )SURGE",dfdam$EVTYPE,perl=TRUE)]<-"SURGE/TIDE"
dfdam$EVTYPE[grep("MARINE THUNDERSTORM WIND|MARINE TSTM WIND",dfdam$EVTYPE)]<-"MARINE TDSTM W"
dfdam$EVTYPE[grep("(?<!DUST )(?<!WINTER )(?<!ICE )(?<!TROPICAL )STORM",dfdam$EVTYPE,perl=TRUE)]<-"THUNDERSTORM W"
dfdam$EVTYPE[grep("(?<!MARINE )TDSTM|(?<!MARINE )TSTM|MICROB|DOWNB",dfdam$EVTYPE,perl = TRUE)]<-"THUNDERSTORM W"
dfdam$EVTYPE[agrep("ICE STORM",dfdam$EVTYPE,max.distance = 2)]<-"I STORM"
dfdam$EVTYPE[grep("WINTER STORM",dfdam$EVTYPE)]<-"WINTER STORM"
dfdam$EVTYPE[grep("TROPICAL STORM",dfdam$EVTYPE)]<-"TROPICAL STORM"
dfdam$EVTYPE[grep("DUST STORM|BLOWING DUST",dfdam$EVTYPE)]<-"DUST STORM"
dfdam$EVTYPE[grep("FLASH",dfdam$EVTYPE)]<-"FLASH F"
dfdam$EVTYPE[grep("(?<!COASTAL )(?<!ICE )FLOOD",dfdam$EVTYPE,perl = TRUE)]<-"FLOOD"
dfdam$EVTYPE[grep("COASTAL",dfdam$EVTYPE)]<-"COASTAL F"
dfdam$EVTYPE[agrep("LIGHTNING",dfdam$EVTYPE,max.distance = 2)]<-"LIGHTNING"
#unique(grep("WINTER",dfdam$EVTYPE,perl = TRUE, value = TRUE))
dfdam$EVTYPE[grep("WINTER WEATHER",dfdam$EVTYPE)]<-"WINTER WEATHER"
#unique(grep("TROPICAL",dfdam$EVTYPE,perl = TRUE, value = TRUE))
dfdam$EVTYPE[grep("TROPICAL DEPRESSION",dfdam$EVTYPE)]<-"TROPICAL DEPRESSION"

dfdam$EVTYPE[agrep("WATERSPROUT",dfdam$EVTYPE,max.distance = 2)]<-"WATERSPROUT"
dfdam$EVTYPE[agrep("TORNADO",dfdam$EVTYPE,max.distance = 2)]<-"TORNADO"
dfdam$EVTYPE[grep("MARINE STRONG WIND",dfdam$EVTYPE)]<-"MARINE STRONG W"
dfdam$EVTYPE[agrep("RIP CURRENT",dfdam$EVTYPE,max.distance = 2)]<-"RIP CURRENT"
dfdam$EVTYPE[agrep("SLEET",dfdam$EVTYPE,max.distance = 2)]<-"SLEET"
dfdam$EVTYPE[agrep("AVALANCHE",dfdam$EVTYPE,max.distance = 2)]<-"AVALANCHE"
dfdam$EVTYPE[agrep("BLIZZARD",dfdam$EVTYPE,max.distance = 2)]<-"BLIZZARD"
dfdam$EVTYPE[grep("LAKE-EFFECT SNOW",dfdam$EVTYPE)]<-"LAKE-EFFECT S"
dfdam$EVTYPE[grep("SNOW",dfdam$EVTYPE)]<-"HEAVY SNOW"
dfdam$EVTYPE[grep("HURRICANE|TYPHOON",dfdam$EVTYPE)]<-"HURRICANE"
dfdam$EVTYPE[agrep("WILDFIRE",dfdam$EVTYPE,max.distance = 2)]<-"WILDFIRE"
dfdam$EVTYPE[agrep("DUST DEVIL",dfdam$EVTYPE,max.distance = 2)]<-"DUST DEVIL"
dfdam$EVTYPE[agrep("WILDFIRE",dfdam$EVTYPE,max.distance = 2)]<-"WILDFIRE"
dfdam$EVTYPE[agrep("DROUGHT",dfdam$EVTYPE,max.distance = 2)]<-"DROUGHT"
dfdam$EVTYPE[agrep("EXCESSIVE HEAT",dfdam$EVTYPE,max.distance = 2)]<-"EXCESSIVE H"
dfdam$EVTYPE[grep("HEAT|WARM",dfdam$EVTYPE)]<-"HEAT"
dfdam$EVTYPE[grep("MARINE HAIL",dfdam$EVTYPE)]<-"MARINE HL"
dfdam$EVTYPE[grep("MARINE HIGH WIND",dfdam$EVTYPE)]<-"MARINE HIGH W"
dfdam$EVTYPE[grep("HAIL",dfdam$EVTYPE)]<-"HAIL"
dfdam$EVTYPE[grep("STRONG WIND",dfdam$EVTYPE)]<-"STRONG W"
dfdam$EVTYPE[grep("EXTREME COLD|HYPOTHERMIA|HYPERTHERMIA",dfdam$EVTYPE)]<-"EXTREME C/W CHILL"
dfdam$EVTYPE[grep("COLD|LOW TEMPERATURE|COOL",dfdam$EVTYPE)]<-"C/W CHILL"
dfdam$EVTYPE[grep("FREEZING FOG",dfdam$EVTYPE)]<-"F FOG"
dfdam$EVTYPE[grep("(?<!FREEZING )FOG",dfdam$EVTYPE,perl=TRUE)]<-"D FOG"
dfdam$EVTYPE[grep("DENSE SMOKE|SMOKE",dfdam$EVTYPE)]<-"D SMOKE"
dfdam$EVTYPE[grep("ICE|FROST|FREEZ|ICY",dfdam$EVTYPE)]<-"FROST/FREEZE"
dfdam$EVTYPE[grep("RAIN|PRECIP|SHOWER",dfdam$EVTYPE)]<-"HEAVY RAIN"
dfdam$EVTYPE[grep("WIND",dfdam$EVTYPE)]<-"HIGH W"
dfdam$EVTYPE[grep("SLIDE|SLUMP|SPOUT",dfdam$EVTYPE)]<-"LANDSLIDE"
dfdam$EVTYPE[grep("I STORM",dfdam$EVTYPE)]<-"ICE STORM"
dfdam$EVTYPE[grep("FIRE",dfdam$EVTYPE)]<-"WILDFIRE"
dfdam$EVTYPE[grep("\\?|APACHE|BEACH|DAM|DENSE|FUNNEL|GLAZE|MIX|TURBULENCE|URBAN|ACCIDENT|DROWNING|MISHAP|HIGH$",dfdam$EVTYPE)]<-"OTHER"

events<-unique(dfdam[order(dfdam$EVTYPE),"EVTYPE"])



#Exploring the data

#Frequency of events per year
freqevents<-dfdam%>%group_by(EVTYPE)%>%summarise(freq=n())
freqevents<-setNames(aggregate(year(dfdam$BGN_DATE)~dfdam$EVTYPE,FUN = length),c("Nevents","Frequency"))

g<-ggplot(freqevents,aes(Nevents,Frequency))
g+geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

g1<-ggplot(dfdam,aes(dfdam$EVTYPE))
g1+geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title="Total number of events from 1993 to 2011",x="Event type",y="Total number of events")

#fATALITIES AND INJURIES
#Maximum fatalities and injuries
dfdam[dfdam$FATALITIES==max(dfdam$FATALITIES),]
dfdam[dfdam$INJURIES==max(dfdam$INJURIES),]

fatevents<-setNames(aggregate(dfdam$FATALITIES~dfdam$EVTYPE,FUN = sum),c("Type","Fatalities"))
fattotal<-setNames(aggregate(dfdam$FATALITIES~year(dfdam$BGN_DATE),FUN = sum),c("Year","Fatalities"))


fatevents<-setNames(aggregate(dfdam$FATALITIES+dfdam$INJURIES~dfdam$EVTYPE,FUN = sum),c("Type","Incidents"))

fatevents<-setNames(aggregate(dfdam$FATALITIES~dfdam$EVTYPE,FUN = sum),c("Type","Fatalities"))
injevents<-setNames(aggregate(dfdam$INJURIES~dfdam$EVTYPE,FUN = sum),c("Type","Injuries"))
fatinjevents2<-dfdam%>%group_by(EVTYPE)%>%summarise(fatalities=sum(FATALITIES),injevents=sum(INJURIES))
fatinjevents<-data.frame(fatevents$Type,fatevents$Fatalities,injevents$Injuries)


library(lattice)
attach(dfdam)
dotplot(EVTYPE~FATALITIES+INJURIES, 
       main="Total number of incidents",
        xlab="Incidents")
library(reshape2)
dfdam_melted <- melt(dfdam[,c("EVTYPE","FATALITIES","INJURIES")], id=c("EVTYPE"))
g2<-ggplot(dfdam_melted,aes(x=value,y=EVTYPE,color=variable))+geom_point()
g2+labs(title="Total number of incidents",x="",y="")+theme(legend.title=element_blank())

#All incidents
library(reshape2)
dfdam_melted <- melt(dfdam[,c("EVTYPE","FATALITIES","INJURIES")], id=c("EVTYPE"))
g3<-ggplot(dfdam_melted,aes(x=value,y=EVTYPE))+geom_point(alpha=1/2)
g3+labs(title="Total number of incidents",x="",y="")+ facet_grid(.~variable)+ theme(legend.title=element_blank())

#Total number of events
fatevents<-setNames(aggregate(dfdam$FATALITIES~dfdam$EVTYPE,FUN = sum),c("Type","Fatalities"))
injevents<-setNames(aggregate(dfdam$INJURIES~dfdam$EVTYPE,FUN = sum),c("Type","Injuries"))
fatinjevents<-data.frame(fatevents$Type,fatevents$Fatalities,injevents$Injuries)
df_melted <- melt(fatinjevents, id=c("fatevents.Type"))
g2<-ggplot(df_melted,aes(x=value,y=fatevents.Type,color=variable))+geom_point()
g2+labs(title="Total number of incidents",x="",y="")+theme(legend.title=element_blank())



#PROPERTY AND CROP DAMAGES
dfdam[dfdam$PROPDMG==max(dfdam$PROPDMG),]
dfdam[dfdam$CROPDMG==max(dfdam$CROPDMG),]


freqevents<-setNames(aggregate(year(dfdam$BGN_DATE)~dfdam$EVTYPE,FUN = length),c("Nevents","Year"))
freqevents<-dfdam%>%group_by(EVTYPE)%>%summarise(freq=n())
