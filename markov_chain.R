#Markov chain piece


#read in behaviour data
movsdf.rbind<-read.csv('movsdf.rbind_orientationcorrected.csv')


#####
# 2.3 Aggregating surfaces into categories for Transition Matrices
detach("package:dplyr", unload = TRUE)
library(plyr)
movsdf.rbind$SurfaceCategories<-revalue(movsdf.rbind$Surface,c("AlcOutside"="HygieneOutside",
                                                               "In"="In",
                                                               "Door"="FarPatient",
                                                               "Other"="FarPatient",
                                                               "Table"="NearPatient",
                                                               "Tray"="Equipment" ,
                                                               "Patient"="Patient",
                                                               "Sharps"="Equipment"  ,
                                                               "Waste"="HygieneInside" ,
                                                               "Sink"="HygieneInside",
                                                               "Soap"="HygieneInside" ,
                                                               "PaperTowel"="HygieneInside" ,
                                                               "IV"="Equipment" ,
                                                               "Out"="Out" ,
                                                               "Alc"="Alcohol" ,
                                                               "ObsTrolley"="Equipment",
                                                               "Wipes"="HygieneInside",
                                                               "Bed"="NearPatient",
                                                               #"GlovesOn"="GlovesOn", #will determine with more specific rule below
                                                               #"GlovesOff"="GlovesOff", #will determine with more specific rule below
                                                               "Chair"="NearPatient",
                                                               "ApronOn"="HygieneOutside", #changed "ApronOnOutside" to "ApronOn"
                                                               "ApronOff"="ApronOff",
                                                               #"GlovesOnOutside"="HygieneOutside",
                                                               "Stethoscope"="Equipment"))

#if gloves are put on after an "out," then this is a HygieneOutside moment
#for (i in 1:max(movsdf.rbind$ActivityID)){

# if this particular participant has a GlovesOn moment....
# if(length(movsdf.rbind$Surface[movsdf.rbind$Surface=="GlovesOn" & movsdf.rbind$ActivityID==i])!=0){

#and the time at which this happens is greater than the time at which an "out" event happens for this same person...
# or the time at which this happens is smaller than the time at which an "in" event happens for this same person...
#   if(movsdf.rbind$Time[movsdf.rbind$ActivityID==i & movsdf.rbind$Surface=="GlovesOn"] - movsdf.rbind$Time[movsdf.rbind$ActivityID==i & movsdf.rbind$Surface=="Out"] > 0 |
#     movsdf.rbind$Time[movsdf.rbind$ActivityID==i & movsdf.rbind$Surface=="In"] - movsdf.rbind$Time[movsdf.rbind$ActivityID==i & movsdf.rbind$Surface=="GlovesOn"] > 0){

#then this is a "HygieneOutside" moment
#   movsdf.rbind$SurfaceCategories[movsdf.rbind$ActivityID==i & movsdf.rbind$Surface=="GlovesOn"]<-"HygieneOutside"
#  }else{
#otherwise, this is a "HygienInside" moment that we'll designate as "GlovesOn"
#  movsdf.rbind$SurfaceCategories[movsdf.rbind$ActivityID==i & movsdf.rbind$Surface=="GlovesOn"]<-"GlovesOn"
#}
#}
#}

#we will remove all "HygienOutside" moments, because we are only interested in capturing what happens between
#"In" and "Out" events

movsdf.rbind<-movsdf.rbind[movsdf.rbind$SurfaceCategories!="HygieneOutside",]

#will also remove "ApronOff" moments, because we are not determining whether they put them on before entering or not
movsdf.rbind<-movsdf.rbind[movsdf.rbind$SurfaceCategories!="ApronOff",]


#remove HygieneOutside 

# Creating lists so we don't count transitions between "out" and first state of next observed episode
# It looksl ike Activity ID is unique per observation

TObs.left.list<-list()
TObs.right.list<-list()
TIV.left.list<-list()
TIV.right.list<-list()
TRounds.left.list<-list()
TRounds.right.list<-list()

for (i in 1:max(movsdf.rbind$ActivityID)){
  TObs.left.list[[i]]<-movsdf.rbind$SurfaceCategories[movsdf.rbind$CareType=="Obs" & movsdf.rbind$Orientation=="leftFacing" & movsdf.rbind$ActivityID==i]
  TObs.right.list[[i]]<-movsdf.rbind$SurfaceCategories[movsdf.rbind$CareType=="Obs" & movsdf.rbind$Orientation=="rightFacing" & movsdf.rbind$ActivityID==i]
  
  TIV.left.list[[i]]<-movsdf.rbind$SurfaceCategories[movsdf.rbind$CareType=="IV" & movsdf.rbind$Orientation=="leftFacing" & movsdf.rbind$ActivityID==i]
  TIV.right.list[[i]]<-movsdf.rbind$SurfaceCategories[movsdf.rbind$CareType=="IV" & movsdf.rbind$Orientation=="rightFacing" & movsdf.rbind$ActivityID==i]
  
  TRounds.left.list[[i]]<-movsdf.rbind$SurfaceCategories[movsdf.rbind$CareType=="Rounds" & movsdf.rbind$Orientation=="leftFacing" & movsdf.rbind$ActivityID==i]
  TRounds.right.list[[i]]<-movsdf.rbind$SurfaceCategories[movsdf.rbind$CareType=="Rounds" & movsdf.rbind$Orientation=="rightFacing" & movsdf.rbind$ActivityID==i]
  
}

require(markovchain)

TObs.left<-markovchainFit(TObs.left.list)
TObs.right<-markovchainFit(TObs.right.list)

TIV.left<-markovchainFit(TIV.left.list)
TIV.right<-markovchainFit(TIV.right.list)

TRounds.left<-markovchainFit(TRounds.left.list)
TRounds.right<-markovchainFit(TRounds.right.list)


detach("package:plyr", unload = TRUE)
library(dplyr)

#Forces an "out" absorbing state for each matrix
TObs.left$estimate@transitionMatrix[9,]=0
TObs.left$estimate@transitionMatrix[9,9]=1
TObs.left$estimate@transitionMatrix[,5]=0
TObs.left$estimate@transitionMatrix[,4]=0

TObs.right$estimate@transitionMatrix[9,]=0
TObs.right$estimate@transitionMatrix[9,9]=1
TObs.right$estimate@transitionMatrix[,5]=0
TObs.right$estimate@transitionMatrix[,4]=0

TIV.left$estimate@transitionMatrix[9,]=0
TIV.left$estimate@transitionMatrix[9,9]=1
TIV.left$estimate@transitionMatrix[,5]=0
TIV.left$estimate@transitionMatrix[,4]=0

TIV.right$estimate@transitionMatrix[9,]=0
TIV.right$estimate@transitionMatrix[9,9]=1
TIV.right$estimate@transitionMatrix[,5]=0
TIV.right$estimate@transitionMatrix[,4]=0

TRounds.left$estimate@transitionMatrix[9,]=0
TRounds.left$estimate@transitionMatrix[9,9]=1
TRounds.left$estimate@transitionMatrix[,5]=0
TRounds.left$estimate@transitionMatrix[,4]=0

TRounds.right$estimate@transitionMatrix[9,]=0
TRounds.right$estimate@transitionMatrix[9,9]=1
TRounds.right$estimate@transitionMatrix[,5]=0
TRounds.right$estimate@transitionMatrix[,4]=0

#when HygieneOutside removed, the number of behaviors went from 10 to 9 for some matrices that
#previously included HygieneOutside moments

## HEAT MAPS ##

#Plot a heatmap
library(reshape2)
library(plotly)
TobR<-melt(TObs.right$estimate@transitionMatrix)
TobL<-melt(TObs.left$estimate@transitionMatrix)
TIVL<-melt(TIV.left$estimate@transitionMatrix)
TIVR<-melt(TIV.right$estimate@transitionMatrix)
TRoundsL<-melt(TRounds.left$estimate@transitionMatrix)
TRoundsR<-melt(TRounds.right$estimate@transitionMatrix)

matrixtoplot<-TobR #or whichever matrix you want
#have to rerun melt before new plot
a<-plot_ly(matrixtoplot, y = ~Var1, x = ~Var2, z = ~value, colors= "Blues", colorbar=list(
  title='Probability'
)) %>%
  add_heatmap()%>%
  colorbar(limits = c(0, 1))%>%
  layout(xaxis = list(title="To"), yaxis = list(title="Observation"))

matrixtoplot<-TobL
b<-plot_ly(matrixtoplot, y = ~Var1, x = ~Var2, z = ~value, colors= "Blues", showscale=FALSE,colorbar=list(
  title='Probability'
)) %>%
  add_heatmap()%>%
  colorbar(limits = c(0, 1))%>%
  layout(xaxis = list(title="To"), yaxis = list(title="From"))

matrixtoplot<-TIVL
c<-plot_ly(matrixtoplot, y = ~Var1, x = ~Var2, z = ~value, colors= "Blues", showscale=FALSE,colorbar=list(
  title='Probability'
)) %>%
  add_heatmap()%>%
  colorbar(limits = c(0, 1))%>%
  layout(xaxis = list(title="To"), yaxis = list(title="IV"))

matrixtoplot<-TIVR
d<-plot_ly(matrixtoplot, y = ~Var1, x = ~Var2, z = ~value, colors= "Blues", showscale=FALSE,colorbar=list(
  title='Probability'
)) %>%
  add_heatmap()%>%
  colorbar(limits = c(0, 1))%>%
  layout(xaxis = list(title="To"), yaxis = list(title="From"))

matrixtoplot<-TRoundsL
e<-plot_ly(matrixtoplot, y = ~Var1, x = ~Var2, z = ~value, colors= "Blues", showscale=FALSE, colorbar=list(
  title='Probability'
)) %>%
  add_heatmap()%>%
  colorbar(limits = c(0, 1))%>%
  layout(xaxis = list(title="Left-facing"), yaxis = list(title="Doctors' Rounds"))


matrixtoplot<-TRoundsR
f<-plot_ly(matrixtoplot, y = ~Var1, x = ~Var2, z = ~value, colors= "Blues", showscale=FALSE,colorbar=list(
  title='Probability'
)) %>%
  add_heatmap()%>%
  colorbar(limits = c(0, 1))%>%
  layout(xaxis = list(title="Right-Facing"), yaxis = list(title="From"))

p<-subplot(a,b,c,d,e,f,shareX = TRUE,shareY=TRUE,titleY=TRUE,nrows=3)
