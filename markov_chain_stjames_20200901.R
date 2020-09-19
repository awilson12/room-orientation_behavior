# Amanda Wilson and Marco-Felipe King
# University of Arizona and Univeristy of Leeds collaboration

#---- Descripton of contents -----------------------------------------------------------------------------------------------------------------------------------------------------

# This code includes...

    #1 #Fitting of distributions to posterior lambda distribution from ABC method
       # with PhiX174 and MS2 hand<-->fomite transfer data
       # Requires lambda.csv file (accessible in output folder in Google Drive 20190911 output)

    #2 # Adapted version of Marco-Felipe King's code for creaitng transitional
       # probability matrices for hand-to-fomite, -patient, and hand hygiene
       # moments for IV, Obs, and Doctor Rounds care for two room orientations 
       # (approach patient from left or from right)
       # Requires movsdf_rbind file from Google Drive in Markov chain behavior sequence
       # folder
       # There is also adapted code from Marco-Felipe King for creating heatmaps to communicate/compare transitional
       # probability matrices. 

    #3 # Reading in ANSYS results to make variable surface concentrations

    #4 # Function for estimating behavior sequences and viral exposure based on...
            # inputs:
               #room orientation ('left' or 'right')
               #care type ('IV', 'Obs', 'Rounds')
               #number of sequences
            # outputs:
               #exposure.frame - list of data.frames with behaviors and inputs/outputs..
                    #handR = concentration on right hand per contact
                    #handL = concentration on left hand per contact
                    #hand = which hand was used per contact
                    #hygiene = reduction associated with hygiene moments
                    #behavior = behavior occuring for that contact
                    #SH = fraction of hand used for that contact
                    #transfer = transfer efficiency for that contact
                    #surfconc = surface concentration for that hand-to-surface contact
               #behavior.total - list of behavior sequences as vectors

     #4 # Comparison of estimated exposures for different types of care and room orientations

#------- Notes --------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Needs to be updated as of 10/27/2019 (AW)...
      #1 - Transfer efficiency for hand-to-patient contact
      #2 - Clarification on "alcohol" contact - contacting dispenser or is this a hand hygiene moment?
      #3 - Assumed surface concentrations need to be updated
          #Different concentrationso based on surface type?
      #4 - Plotting change in concentration on hands on same plot as behaviors (dual y axis, one with 
          #discrete behaviors and the other with log10 conc?)
    #------------------------------- ADDRESSED 10/27-------------------------------------------------------------------------
      #5 - Double-check GlovesOn/Off issues - Are there sequences in which someone takes off gloves having never put them on?
          #or instances of "HygienOutside" if someone is still inside?
    #-------------------------------------------------------------------------------------------------------------------------
      
      #6 - Right now there is a 50/50 chance of using right or left hand for any given contact
          #will explore more literature to see if there's an update on this
            #Was this recorded in the transcription of St. James videos? If not, I could get this info.
      #7 - Address distribution of lambda - does this jive with what we're reporting as "best distribution" in 
           #ABC pub?

#10/27: Some adjustments made to categorization of surface contacts (removed some outside-room events) (AW)
#10/28: Fixed issue regarding initial contact concentrations (door knob contact on "In") (AW)
#10/29: Added ANSYS piece - inform variability in surface conc based on  (have not added conc into simulation portion yet)
#10/29: fixed behavior data file orientation labeling (everything for all days flipped rightFacing<-->leftFacing)
#11/4: Added code to analyze when decreases in hand conc were found and more code for plots
#1/24: Changed code for new ANSYS data (particle tracking instead of CO2 proxy)
#1/24: Solved  sampel function issue where only one concentration was in vector (for example, door surfaces)

-------- #1  Fitting of distributions to posterior lambda distribution from ABC method----------------------------------------------------------------------------------------------


#read in csv file for transfer efficiencies
library(readr)
lambda<-read_csv("lambda.csv",col_names=FALSE)
lambda<-lambda$X1

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

#------------------------- #3 Reading in ANSYS results to make variable surface concentrations ---------------------------------------------------------------------------

ANSYS<-read.csv('C:/Users/wilso/Documents/Research/dissertation/preparation of chapters/Other materials/ANSYS St. James files/particle_comparison.csv')
ANSYS$SURFACEAREAm2<-ANSYS$SURFACEAREAm2*10000 #convert m2 to cm2
View(ANSYS)

#orientation 1 = right-facing, orientation 2 = left-facing
ANSYS$ORIENTATION[ANSYS$ORIENTATION==1]<-"Right-facing"
ANSYS$ORIENTATION[ANSYS$ORIENTATION==2]<-"Left-facing"

ANSYS$FRACTION<-(ANSYS$NUMBER.PARTICLE/ANSYS$TOTAL.PARTICLE)*100

windows()
ggplot(data=ANSYS,aes(x=SURFACE,y=FRACTION,fill=SURFACE))+
  scale_y_continuous(name="Percent of Deposited Particles")+
  scale_x_discrete(name="Surface")+
  geom_bar(stat="identity",color="black")+
  geom_text(aes(label=round(FRACTION,2)), position=position_dodge(width=0.1), vjust=-0.25,size=5)+
  coord_flip()+
  facet_wrap(~ORIENTATION)+
  theme_pubr()+
  theme(strip.text = element_text(size=15),axis.title = element_text(size=15),
        axis.text = element_text(size=15),legend.position="none")

#ANSYS <- ANSYS[order(ANSYS$SURFACEAREAm2),]
windows()
ggplot(data=ANSYS[ANSYS$ORIENTATION=="Right-facing",],aes(x=SURFACE,y=SURFACEAREAm2/10000,fill=SURFACE))+
  scale_y_continuous(name=expression("Surface Area (m"^2*")"))+
  scale_x_discrete(name="Surface")+
  geom_bar(stat="identity",color="black")+
   coord_flip()+
  theme_pubr()+
  theme(strip.text = element_text(size=15),axis.title = element_text(size=15),
        axis.text = element_text(size=15),legend.position="none")


#------------------------- #4 Function for estimating behavior sequences and viral exposure -------------------------------------------------------------------------------


behavior.sim<-function(room.orientation=c("left","right"),caretype=c("IV","Obs","Rounds"),numsequence){
   set.seed(34)
  
 
  #Behavior model section

    sample.space<-c("Alcohol","Equipment","FarPatient","GlovesOff","GlovesOn","HygieneInside","In","NearPatient","Out","Patient")
  
  #will save output sequences in behavior.total
  behavior.total<-list() #creating a list to store behaviors
  exposure.frame<-list() #creating a list to store exposure.frame data.frames
  
  #set up matrix for type of care
  if (room.orientation=="left"){
    if (caretype=="IV"){
      prob.mat<-TIV.left$estimate #left-facing, IV
    }else if (caretype=="Obs"){
      prob.mat<-TObs.left$estimate #left-facing, Obs
    }else{
      prob.mat<-TRounds.left$estimate #left-facing, Rounds
    }
  }else{
    if (caretype=="IV"){
      prob.mat<-TIV.right$estimate #right-facing IV
    }else if (caretype=="Obs"){
      prob.mat<-TObs.right$estimate #right-facing Obs
    }else{
      prob.mat<-TRounds.right$estimate #right-facing Rounds
    }
  }
  
  
  #create 
  for (j in 1:numsequence){
    
    #-----------------------------------------------------------------------------------------------
    
    #setting up concentrations.....
    
    #orientation 1 (right-facing)---------------------------------------------------------
    estimated.breath.particle<-runif(1,5,215) #genome copies/m^3 from Alsved et al. (2019) 
    #take this value and * expected exhaled breath in 30 min 
    #(.5 hr x breathing rate from exposure factors handbook)
    exhaled.breath<-runif(1,4.19*10^-3,6.65*10^-3)*30 #m^3/min * 30 min (min average and max average of ventilation rates (m^3/min) from Exposure
    #Factors Handbook for men and women combined, Tables 6-17 and 6-19 in Inhalation Chapter)
    estimated.particle<-estimated.breath.particle*exhaled.breath # (genome copies/m^3 * m^3 = genome copies)
    #assume each genome copy represents particle - conservative assumption based on Van Abel et al. (2019)
    #on best practices for norovirus QMRA, specifically
    
    #UPDATED ON 1/24: Changed to # particles @ mouth x fraction of total viruses expelled to be deposited on specific surface
    #EMAILED MARCO ON 1/24 TO ASK ABOUT OPTIONS FOR INCORPORATING ANSYS INFO...
    
    
    
    #Farpatient (desk)-------------------------
    
    #desk
    particle.desk.1<-ANSYS$NUMBER.PARTICLE[ANSYS$SURFACE=="desk" & ANSYS$ORIENTATION==1]/ANSYS$TOTAL.PARTICLE[ANSYS$SURFACE=="desk" & ANSYS$ORIENTATION==1]
    SA.desk.1<-ANSYS$SURFACEAREAm2[ANSYS$SURFACE=="desk" & ANSYS$ORIENTATION==1]
    estimated.desk.1<-(estimated.particle*particle.desk.1)/SA.desk.1
    Farpatient.1<-estimated.desk.1
    
    #Nearpatient (trolley, bed, chair, door)-----------------
    
    #trolley (sidetable)
    particle.trolley.1<-ANSYS$NUMBER.PARTICLE[ANSYS$SURFACE=="side table" & ANSYS$ORIENTATION==1]/ANSYS$TOTAL.PARTICLE[ANSYS$SURFACE=="side table" & ANSYS$ORIENTATION==1]
    SA.trolley.1<-ANSYS$SURFACEAREAm2[ANSYS$SURFACE=="side table" & ANSYS$ORIENTATION==1]
    estimated.trolley.1<-estimated.particle*particle.trolley.1/SA.trolley.1
    
    #bed
    particle.bed.1<-ANSYS$NUMBER.PARTICLE[ANSYS$SURFACE=="bed" & ANSYS$ORIENTATION==1]/ANSYS$TOTAL.PARTICLE[ANSYS$SURFACE=="bed" & ANSYS$ORIENTATION==1]
    SA.bed.1<-ANSYS$SURFACEAREAm2[ANSYS$SURFACE=="bed" & ANSYS$ORIENTATION==1]
    estimated.bed.1<-estimated.particle*particle.bed.1/SA.bed.1
    
    #chair
    particle.chair.1<-ANSYS$NUMBER.PARTICLE[ANSYS$SURFACE=="chair" & ANSYS$ORIENTATION==1]/ANSYS$TOTAL.PARTICLE[ANSYS$SURFACE=="chair" & ANSYS$ORIENTATION==1]
    SA.chair.1<-ANSYS$SURFACEAREAm2[ANSYS$SURFACE=="chair" & ANSYS$ORIENTATION==1]
    estimated.chair.1<-estimated.particle*particle.chair.1/SA.chair.1
    
    #door
    particle.door.1<-ANSYS$NUMBER.PARTICLE[ANSYS$SURFACE=="door" & ANSYS$ORIENTATION==1]/ANSYS$TOTAL.PARTICLE[ANSYS$SURFACE=="door" & ANSYS$ORIENTATION==1]
    SA.door.1<-ANSYS$SURFACEAREAm2[ANSYS$SURFACE=="door" & ANSYS$ORIENTATION==1]
    estimated.door.1<-estimated.particle*particle.door.1/SA.door.1
    
    Nearpatient.1<-c(estimated.trolley.1,estimated.bed.1,estimated.chair.1,estimated.door.1)
    
    # Out (door) ---------------------------------------------
    
    out.1<-estimated.door.1
    
    #patient (rest of patient, face) -------------------------------
    particle.restofpatient.1<-ANSYS$NUMBER.PARTICLE[ANSYS$SURFACE=="patient" & ANSYS$ORIENTATION==1]/ANSYS$TOTAL.PARTICLE[ANSYS$SURFACE=="patient" & ANSYS$ORIENTATION==1]
    SA.patient.1<-ANSYS$SURFACEAREAm2[ANSYS$SURFACE=="patient" & ANSYS$ORIENTATION==1]
    estimated.patient.1<-estimated.particle*particle.restofpatient.1/SA.patient.1
    
    Patient.1<-c(estimated.patient.1)
    
    
    #orientation 2 (left-facing)
    
    #Farpatient (door)-------------------------
    
    #door
    particle.door.2<-ANSYS$NUMBER.PARTICLE[ANSYS$SURFACE=="door" & ANSYS$ORIENTATION==2]/ANSYS$TOTAL.PARTICLE[ANSYS$SURFACE=="door" & ANSYS$ORIENTATION==2]
    SA.door.2<-ANSYS$SURFACEAREAm2[ANSYS$SURFACE=="door" & ANSYS$ORIENTATION==2]
    estimated.door.2<-estimated.particle*particle.door.2/SA.door.2
    
    Farpatient.2<-estimated.door.2
    
    #Nearpatient (trolley, bed, chair, desk)-----------------
    
    #trolley (sidetable)
    particle.trolley.2<-ANSYS$NUMBER.PARTICLE[ANSYS$SURFACE=="side table" & ANSYS$ORIENTATION==2]/ANSYS$TOTAL.PARTICLE[ANSYS$SURFACE=="side table" & ANSYS$ORIENTATION==2]
    SA.trolley.2<-ANSYS$SURFACEAREAm2[ANSYS$SURFACE=="side table" & ANSYS$ORIENTATION==2]
    estimated.trolley.2<-estimated.particle*particle.trolley.2/SA.trolley.2
    
    #bed
    particle.bed.2<-ANSYS$NUMBER.PARTICLE[ANSYS$SURFACE=="bed" & ANSYS$ORIENTATION==2]/ANSYS$TOTAL.PARTICLE[ANSYS$SURFACE=="bed" & ANSYS$ORIENTATION==2]
    SA.bed.2<-ANSYS$SURFACEAREAm2[ANSYS$SURFACE=="bed" & ANSYS$ORIENTATION==2]
    estimated.bed.2<-estimated.particle*particle.bed.2/SA.bed.2
    
    #chair
    particle.chair.2<-ANSYS$NUMBER.PARTICLE[ANSYS$SURFACE=="chair" & ANSYS$ORIENTATION==2]/ANSYS$TOTAL.PARTICLE[ANSYS$SURFACE=="chair" & ANSYS$ORIENTATION==2]
    SA.chair.2<-ANSYS$SURFACEAREAm2[ANSYS$SURFACE=="chair"& ANSYS$ORIENTATION==2]
    estimated.chair.2<-estimated.particle*particle.chair.2/SA.chair.2
    
    #desk
    particle.desk.2<-ANSYS$CO2[ANSYS$X1=="desk" & ANSYS$Orientation==2]/ANSYS$TOTAL.PARTICLE[ANSYS$SURFACE=="desk" & ANSYS$ORIENTATION==2]
    SA.desk.2<-ANSYS$SURFACEAREAm2[ANSYS$SURFACE=="desk" & ANSYS$ORIENTATION==2]
    estimated.desk.2<-estimated.particle*particle.desk.2/SA.desk.2
    
    Nearpatient.2<-c(estimated.trolley.2,estimated.bed.2,estimated.chair.2,estimated.desk.2)
    
    # Out (door) ---------------------------------------------
    
    out.2<-estimated.door.2
    
    #patient (rest of patient, face) -------------------------------
    particle.patient.2<-ANSYS$NUMBER.PARTICLE[ANSYS$SURFACE=="patient" & ANSYS$ORIENTATION==2]/ANSYS$TOTAL.PARTICLE[ANSYS$SURFACE=="patient" & ANSYS$ORIENTATION==2]
    SA.patient.2<-ANSYS$SURFACEAREAm2[ANSYS$SURFACE=="patient" & ANSYS$ORIENTATION==2]
    estimated.patient.2<-estimated.particle*particle.patient.2/SA.patient.2
    
    Patient.2<-c(estimated.patient.2)
    
    #----------------------------------------------------------------------------------------------------------------------
    
    
    behavior<-"In" #first behavior is "In"
    k<-2 #setting up k for while loop
    

    
    #while the previous behavior is not "Out," make new behaviors
    while (behavior[k-1]!="Out"){
      
      #If previous step was not out, create new behavior
      #where the prob of selecting a behavior from sample.space is determined by the row
      #of probabilities corresponding to the previous behavior contact (prob.mat[behavior[k-1],])
      
      behavior[k]<-sample(sample.space,1,replace=TRUE,prob=prob.mat[behavior[k-1],])
      
      #now we have to handle issues where someone is taking GlovesOff having never put them on
      
      #if the current selected behavior is GlovesOff and they've never put GlovesOn or the maximum position of a previous GlovesOff is greater than
      # the maximum position of a previous GlovesOn
      behaviorcount<-1:length(behavior)
      glovestate<-"placeholder"
      
      if(behavior[k]=="GlovesOn" & glovestate!=1){
        glovestate<-1
        behavior[k]<-behavior[k]
      }else if (behavior[k]=="GlovesOn"){
        while(behavior[k]=="GlovesOn"){
          behavior[k]<-sample(sample.space,1,replace=TRUE,prob=prob.mat[behavior[k-1],])
        }
      }else{
        behavior[k]<-behavior[k]
      }
      
      if(behavior[k]=="GlovesOff" & glovestate!=1){
        while(behavior[k]=="GlovesOff"){
          behavior[k]<-sample(sample.space,1,replace=TRUE,prob=prob.mat[behavior[k-1],])
        }
      }else if (behavior[k]=="GlovesOff"){
        behavior[k]<-behavior[k]
        glovestate<-0
      }
      

      #advance contact number by 1
      k<-k+1
    }
   
    
    ###  exposure simulation  ###
    
    #--------- TRANSFER EFFICIENCY -------------------------------------------------------
    
    #initialize transfer efficiencies
    transfer<-rep(NA,length(behavior))
    
    #transfer efficiencies for equipment, far patient, near patient contacts, and door handle contacts (in/out)
    transfer[behavior=="Equipment" | behavior=="FarPatient" | behavior=="In" | behavior=="Out" | behavior=="NearPatient"|behavior=="HygieneInside"]<-sample(lambda,length(    transfer[behavior=="Equipment" | behavior=="FarPatient" | behavior=="In" | behavior=="Out" | behavior=="NearPatient"|behavior=="HygieneInside"]))
    
    #transfer efficiency patient
    transfer[behavior=="Patient"]<-0.02 #PLACE HOLDER... LIT REIVEW
    
    #-------------- SURFACE CONCENTRATIONS -----------------------------------------------------------------------------
    
    #Initialize surface concentration (PLACE HOLDER VALUE RIGHT NOW.. WILL DO LIT REVIEW TO INFORM THIS PARAMETER)
    surfconc<-rep(NA,length(behavior))
    
    #concentrations for hand-to-surface contact moments
    if (room.orientation=="right"){
      
      #right-facing = orientation 1
      
      #Assume Out conc ~ In conc
      surfconc[behavior=="In" | behavior=="Out"]<-rep(out.1,length(surfconc[behavior=="In" | behavior=="Out"]))
      
      #Assume Farpatient represents Far patient and hygiene areas
      surfconc[behavior=="FarPatient" | behavior=="HygieneInside"]<-rep(Farpatient.1,length(surfconc[behavior=="FarPatient" | behavior=="HygieneInside"]))
      
      #Assume Nearpatient represents Near patient and Equipment surfaces
      surfconc[behavior=="NearPatient" | behavior=="Equipment"]<-sample(Nearpatient.1,length(surfconc[behavior=="NearPatient" | behavior=="Equipment"]),replace=TRUE)
      
      #Patient surfaces
      surfconc[behavior=="Patient"]<-rep(Patient.1,length(surfconc[behavior=="Patient"]))
      
      
    }else{
      #left-facing = orientation 2
      
      #Assume Out conc ~ In conc
      surfconc[behavior=="In" | behavior=="Out"]<-rep(out.2,length(surfconc[behavior=="In" | behavior=="Out"]))
      
      #Assume Farpatient represents Far patient and hygiene areas
      surfconc[behavior=="FarPatient" | behavior=="HygieneInside"]<-rep(Farpatient.2,length(surfconc[behavior=="FarPatient" | behavior=="HygieneInside"]))
      
      #Assume Nearpatient represents Near patient and Equipment surfaces
      surfconc[behavior=="NearPatient" | behavior=="Equipment"]<-sample(Nearpatient.2,length(surfconc[behavior=="NearPatient" | behavior=="Equipment"]),replace=TRUE)
      
      #Patient surfaces
      surfconc[behavior=="Patient"]<-rep(Patient.2,length(surfconc[behavior=="Patient"]))
      
      
    }
    
    #-------------- FRACTIONAL HAND SURFACE AREA ------------------------------------------------------------------
    
    #Initialize fraction of hand used for contact
    SH<-rep(NA,length(behavior))
    
    #fractional surface area for open hand grip
    SH[behavior=="In" | behavior=="Out"]<-runif(length(SH[behavior=="In" | behavior=="Out"]),0.10,0.21) #min and max of left and right hands in AuYeung et al. (2008)
    
    #fractional surface area for patient contact (front partial fingers)
    SH[behavior=="Patient"]<-runif(length(SH[behavior=="Patient"]),0.4,0.6)
    
    #fractional surface area for variety of grip types (non "in"/"out" contacts)
    SH[behavior=="Equipment"|behavior=="FarPatient"|behavior=="NearPatient"|behavior=="HygieneInside"]<-runif(length(SH[behavior=="Equipment"|behavior=="FarPatient"|behavior=="NearPatient"|behavior=="HygieneInside"]),0.01,0.25)
    #min and max of left and right hands in AuYeung et al. (2008) for various hand grip and hand press contacts (hand immersion contacts not included)
    
    #------------- RIGHT HAND VS. LEFT HAND ---------------------------------------------------------------------------
    
    #initialize R or L hand
    hand<-rep(NA,length(behavior))
    hand[sample(1:length(behavior),length(behavior)/2)]<-"right"
    hand[is.na(hand)]<-"left"
    
    #-------------- HAND HYGIENE EFFICACY -----------------------------------------------------------------------------
    
    #hand hygiene
    hygiene<-rep(NA,length(behavior))
    hygiene[behavior=="Alcohol"]<-10^3
    
    #-------------- EXPOSURE SIMULATION ------------------------------------------------------------------------------
    
    #initialize conccentration on R and L hand estimates
    handR<-rep(0,length(behavior))
    handL<-rep(0,length(behavior))
    
    for (a in 2:(length(behavior))){
      
      if(hand[1]=="right"){
        handR[1]<-0-transfer[1]*SH[1]*(0-surfconc[1])
        handL[1]<-0  
      }else{
        handL[1]<-0-transfer[1]*SH[1]*(0-surfconc[1])
        handR[1]<-0 
      }
    
      if(hand[a]=="right"){
  
        if(behavior[a]!="GlovesOn" & behavior[a]!="GlovesOff" & behavior[a]!="Alcohol"){
          handR[a]<-(handR[a-1]-transfer[a]*SH[a]*(handR[a-1]-surfconc[a]))
          handL[a]<-handL[a-1]
        }else if (behavior[a]=="GlovesOn"){
          handR[a]<-0 #placeholder
          handL[a]<-0 #placeholder
        }else if (behavior[a]=="Alcohol"){
          handR[a]<-handR[a-1]/hygiene[a]
          handL[a]<-handL[a-1]/hygiene[a]
        }else{
          #look for most recent GlovesOn moment previous to this GlovesOff moment
            behaviorcount<-1:length(behavior)
            #max count position of GlovesOn that have happened so far - 1 (previous moment before GlovesOn)
            beforegloveson<- max(behaviorcount[behavior=="GlovesOn" & behaviorcount <=a]) - 1
            handR[a]<-handR[beforegloveson]
            handL[a]<-handL[beforegloveson]
        }
      }else{
    
       if(behavior[a]!="GlovesOn" & behavior[a]!="GlovesOff" & behavior[a]!="Alcohol"){
        handL[a]<-(handL[a-1]-transfer[a]*SH[a]*(handL[a-1]-surfconc[a]))
       handR[a]<-handR[a-1]
        }else if (behavior[a]=="GlovesOn"){
          #when gloves are put on, concentration on hands goes to 0
          handR[a]<-0 #placeholder
          handL[a]<-0 #placeholder
        }else if (behavior[a]=="Alcohol"){
          handR[a]<-handR[a-1]/hygiene[a]
          handL[a]<-handL[a-1]/hygiene[a]
        }else{
          #look for most recent GlovesOn moment previous to this GlovesOff moment
          behaviorcount<-1:length(behavior)
          #max count position of GlovesOn that have happened so far - 1 (previous moment before GlovesOn)
          beforegloveson<- max(behaviorcount[behavior=="GlovesOn" & behaviorcount <=a]) - 1
          handR[a]<-handR[beforegloveson]
          handL[a]<-handL[beforegloveson]
        }
      }
    }

    # -------------------------------- SAVE OUTPUT FOR SIMULATION FOR SINGLE PERSON ----------------------------------------------------------------------------------
    exposure.frame.temp<-data.frame(handR=handR,handL=handL,hand=hand,hygiene=hygiene,behavior=behavior,SH=SH,transfer=transfer,surfconc=surfconc)
    #print(exposure.frame)
    #save behavior sequence in list with position j
    behavior.total[[j]]<-behavior
    exposure.frame[[j]]<-exposure.frame.temp
    #remove behavior from environment so it's overwritten for next sequence created
    rm(behavior)
  }
  # --------------------------------- SAVE ALL OUTPUTS TO GLOBAL ENV --------------------------------------------------------------------------------------------------
  behavior.total<<-behavior.total
  exposure.frame<<-exposure.frame
  print("Order up! :)")
  
}

#--------------------#5  Comparison of estimated exposures for different types of care and room orientations ----------------------------------------------------------------------------------------------

#running function for left and right room orientation - IV care
behavior.sim(room.orientation = "left",caretype = "IV",numsequence = 1000)
seq.IV.left<-behavior.total
exposure.IV.left<-exposure.frame

behavior.sim(room.orientation = "right",caretype = "IV",numsequence = 1000)
seq.IV.right<-behavior.total
exposure.IV.right<-exposure.frame

# running function for left and right room orientation - Obs care
behavior.sim(room.orientation = "left",caretype = "Obs",numsequence = 1000)
seq.Obs.left<-behavior.total
exposure.Obs.left<-exposure.frame

behavior.sim(room.orientation = "right",caretype = "Obs",numsequence = 1000)
seq.Obs.right<-behavior.total
exposure.Obs.right<-exposure.frame

# running function for left and right room orientation - Doctors' rounds
behavior.sim(room.orientation = "left",caretype = "Rounds",numsequence = 1000)
seq.Rounds.left<-behavior.total
exposure.Rounds.left<-exposure.frame

behavior.sim(room.orientation = "right",caretype = "Rounds",numsequence = 1000)
seq.Rounds.right<-behavior.total
exposure.Rounds.right<-exposure.frame

# example plots

handconc<-c(exposure.Obs.right[[2]]$handR,exposure.Obs.right[[2]]$handL)
time<-c(rep(1:length(exposure.Obs.right[[2]]$handR),2))
behavior<-rep(exposure.Obs.right[[2]]$behavior,2)
hand<-c(rep("right",length(handconc)/2),rep("left",length(handconc)/2))
frame.1<-data.frame(handconc=handconc,behavior=behavior,hand=hand,time=time)

library(ggplot2)
windows()
ggplot(data=frame.1,aes(x=time,y=handconc,group=hand))+
  geom_line(aes(linetype=hand),size=1)+geom_point(aes(colour=behavior),size=3)+
  scale_y_continuous(name=expression("Viral particls/cm"^2*"on hands"))+
  scale_x_continuous(name="Number of contacts")+
  scale_linetype(name="Hand")+
  scale_color_discrete(name="Behaviour")+
  theme_pubr()+
  annotate("text",x=25,y=4e-5,label="Hand Hygiene Event",size=6)+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical")
  

#extract summary statistics of exposures
hand.R.max<-rep(0,6000)
hand.R.mean<-rep(0,6000)
hand.L.max<-rep(0,6000)
hand.L.mean<-rep(0,6000)
hand.both.max<-rep(0,6000)
hand.both.mean<-rep(0,6000)
numcontacts<-rep(0,6000)
gloves<-rep(0,6000)
for (i in 1:6000){
  if(i<=1000){
    frame<-exposure.IV.left[[i]]
  }else if (i<=2000){
    frame<-exposure.IV.right[[i-1000]]
  }else if (i<=3000){
    frame<-exposure.Obs.left[[i-2000]]
  }else if (i<=4000){
    frame<-exposure.Obs.right[[i-3000]]
  }else if (i<=5000){
    frame<-exposure.Rounds.left[[i-4000]]
  }else{
    frame<-exposure.Rounds.right[[i-5000]]
  }
  
  #left and right hand grouped
  hand.both.max[i]<-max(c(frame$handR,frame$handL))
  hand.both.mean[i]<-mean(c(frame$handR,frame$handL))
  
  #right hand only
  hand.R.max[i]<-max(frame$handR)
  hand.R.mean[i]<-mean(frame$handR)
  
  #left hand only 
  hand.L.max[i]<-max(frame$handL)
  hand.L.mean[i]<-mean(frame$handL)
  
 # if (length(frame$behavior[frame$behavior=="GlovesOn"])>0){
 #    gloves[i]<-"yes"
 # }else{
 #  gloves[i]<-"no"
 # }
  
  numcontacts[i]<-length(frame$handR)

}

room.face<-rep(c(rep("Left-facing",1000),rep("Right-facing",1000)),3)
care<-c(rep("IV",2000),rep("Observation",2000),rep("Rounds",2000))
data<-data.frame(hand.R.max=hand.R.max,hand.both.max=hand.both.max,hand.both.mean=hand.both.mean,hand.R.mean=hand.R.mean,hand.L.max=hand.L.max,hand.L.mean=hand.L.mean,numcontacts=numcontacts,room.face=room.face,care=care,gloves=gloves)

behavior.example1<-exposure.IV.left[[3]]
behavior.example1$numcontact<-c(1:length(behavior.example1$handR))

behavior.example2<-exposure.Obs.left[[3]]
behavior.example2$numcontact<-c(1:length(behavior.example2$handR))

behavior.example3<-exposure.Rounds.right[[13]]
behavior.example3$numcontact<-c(1:length(behavior.example3$handR))

merged<-rbind(behavior.example1,behavior.example2,behavior.example3)
merged$caretype<-c(rep("IV",length(behavior.example1$numcontact)),
                   rep("Observations",length(behavior.example2$numcontact)),
                   rep("Doctors' rounds",length(behavior.example3$numcontact)))
merged<-merged[order(merged$numcontact),]
merged$behavior<-as.character(merged$behavior)
merged$behavior[merged$behavior=="HygieneInside"]<-"Hygiene Surface"
merged$behavior[merged$behavior=="FarPatient"]<-"Far Patient Surface"
merged$behavior[merged$behavior=="Alcohol"]<-"Hand Hygiene"
merged$behavior[merged$behavior=="In"]<-"Entrance"
merged$behavior[merged$behavior=="Out"]<-"Exit"

#-------------------- FIGURES FOR U01 GRANT ------------------------------------------------------------------------------------------

ggplot(data=merged[merged$numcontact<20,])+geom_point(aes(x=numcontact,y=behavior,group=caretype,colour=caretype),size=3)+
  geom_step(aes(x=numcontact,y=behavior,group=caretype,colour=caretype),size=1)+
  facet_wrap(~caretype)+
  scale_x_continuous(name="Contact Number")+
  scale_y_discrete(name="Behavior")+theme_pubr(base_size=14,border=TRUE)+theme(legend.title=element_blank(),legend.position="none",strip.text=element_text(size=14))


ggplot(data=merged[merged$numcontact<20,])+geom_point(aes(x=numcontact,y=handR+handL,group=caretype,colour=behavior),size=3)+
  geom_step(aes(x=numcontact,y=handR+handL,group=caretype,colour=behavior),size=1)+
  facet_wrap(~caretype)+theme_pubr(base_size=14,border=TRUE)+theme(strip.text=element_text(size=14))+
  scale_y_continuous(name=expression("Concentration (CFU/cm"^2*") on Hands"),trans="log10")+
  scale_x_continuous(name="Contact Number")+
  scale_colour_discrete(name="Behavior")

# ------------------ trends over course of contacts over all

#one large data set with all results

for (i in 1:6000){
  
  if(i<=1000){
    if(i==1){
      total<-exposure.IV.left[[i]]
      total$care<-rep("IV",length(total$handR))
      total$orientation<-rep("left",length(total$handR))
      total$numcontact<-c(1:length(total$handR))
      total$run<-rep(i,length(total$handR))
      total$loss[1]<-NA
      if(length(total$behavior[total$behavior=="Alcohol" | total$behavior=="GlovesOn"])>0){
        total$alcohol<-rep("yes",length(total$behavior))
      }else{
        total$alcohol<-rep("no",length(total$behavior))
      }
      for (j in 2:length(total$behavior)){
        if(total$handR[j-1] > total$handR[j]){
          total$loss[j]<-"yes"
        }else{
          total$loss[j]<-"no"
        }
      }
      total$proploss<-rep(length(total$loss[total$loss=="yes"])/length(total$behavior),length(total$behavior))
    }else{
      frame<-exposure.IV.left[[i]]
      frame$care<-rep("IV",length(frame$handR))
      frame$orientation<-rep("left",length(frame$handR))
      frame$numcontact<-c(1:length(frame$handR))
      frame$run<-rep(i,length(frame$handR))
      frame$loss[1]<-NA
      if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="GlovesOn"])>0){
        frame$alcohol<-rep("yes",length(frame$behavior))
      }else{
        frame$alcohol<-rep("no",length(frame$behavior))
      }
      for (j in 2:length(frame$behavior)){
        if(frame$handR[j-1] > frame$handR[j]){
          frame$loss[j]<-"yes"
        }else{
          frame$loss[j]<-"no"
        }
      }
      frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
    }
  }else if (i<=2000){
  frame<-exposure.IV.right[[i-1000]]
  frame$care<-rep("IV",length(frame$handR))
  frame$orientation<-rep("right",length(frame$handR))
  frame$numcontact<-c(1:length(frame$handR))
  frame$run<-rep(i,length(frame$handR))
  frame$loss[1]<-NA
  if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="GlovesOn"])>0){
    frame$alcohol<-rep("yes",length(frame$behavior))
  }else{
    frame$alcohol<-rep("no",length(frame$behavior))
  }
  for (j in 2:length(frame$behavior)){
    if(frame$handR[j-1] > frame$handR[j]){
      frame$loss[j]<-"yes"
    }else{
      frame$loss[j]<-"no"
    }
  }
  frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
  
  }else if (i<=3000){
  frame<-exposure.Obs.left[[i-2000]]
  frame$care<-rep("Obs",length(frame$handR))
  frame$orientation<-rep("left",length(frame$handR))
  frame$numcontact<-c(1:length(frame$handR))
  frame$run<-rep(i,length(frame$handR))
  frame$loss[1]<-NA
  if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="GlovesOn"])>0){
    frame$alcohol<-rep("yes",length(frame$behavior))
  }else{
    frame$alcohol<-rep("no",length(frame$behavior))
  }
  for (j in 2:length(frame$behavior)){
    if(frame$handR[j-1] > frame$handR[j]){
      frame$loss[j]<-"yes"
    }else{
      frame$loss[j]<-"no"
    }
  }
  frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
  
  }else if (i<=4000){
  frame<-exposure.Obs.right[[i-3000]]
  frame$care<-rep("Obs",length(frame$handR))
  frame$orientation<-rep("right",length(frame$handR))
  frame$numcontact<-c(1:length(frame$handR))
  frame$run<-rep(i,length(frame$handR))
  frame$loss[1]<-NA
  if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="GlovesOn"])>0){
    frame$alcohol<-rep("yes",length(frame$behavior))
  }else{
    frame$alcohol<-rep("no",length(frame$behavior))
  }
  for (j in 2:length(frame$behavior)){
    if(frame$handR[j-1] > frame$handR[j]){
      frame$loss[j]<-"yes"
    }else{
      frame$loss[j]<-"no"
    }
  }
  frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
  
  }else if (i<=5000){
  frame<-exposure.Rounds.left[[i-4000]]
  frame$care<-rep("Rounds",length(frame$handR))
  frame$orientation<-rep("left",length(frame$handR))
  frame$numcontact<-c(1:length(frame$handR))
  frame$run<-rep(i,length(frame$handR))
  frame$loss[1]<-NA
  if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="GlovesOn"])>0){
    frame$alcohol<-rep("yes",length(frame$behavior))
  }else{
    frame$alcohol<-rep("no",length(frame$behavior))
  }
  for (j in 2:length(frame$behavior)){
    if(frame$handR[j-1] > frame$handR[j]){
      frame$loss[j]<-"yes"
    }else{
      frame$loss[j]<-"no"
    }
  }
  frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
  
  }else{
  frame<-exposure.Rounds.right[[i-5000]]
  frame$care<-rep("Rounds",length(frame$handR))
  frame$orientation<-rep("right",length(frame$handR))
  frame$numcontact<-c(1:length(frame$handR))
  frame$run<-rep(i,length(frame$handR))
  frame$loss[1]<-NA
  if(length(frame$behavior[frame$behavior=="Alcohol" | frame$behavior=="GlovesOn"])>0){
    frame$alcohol<-rep("yes",length(frame$behavior))
  }else{
    frame$alcohol<-rep("no",length(frame$behavior))
  }
  for (j in 2:length(frame$behavior)){
    if(frame$handR[j-1] > frame$handR[j]){
      frame$loss[j]<-"yes"
    }else{
      frame$loss[j]<-"no"
    }
  }
  frame$proploss<-rep(length(frame$loss[frame$loss=="yes"])/length(frame$behavior),length(frame$behavior))
}
  
  

  if(i==1){
    total<-total
  }else{
    total<-rbind(total,frame)
  }
}

#-----extract hand hygiene info-----------------
alcohol<-rep(NA,6000)
care<-rep(NA,6000)
room<-rep(NA,6000)

for(i in 1:6000){
  alcohol[i]<-total$alcohol[total$run==i][1]
  care[i]<-total$care[total$run==i][1]
  room[i]<-total$orientation[total$run==i][1]
  
  if(room[i]=="left"){
    room[i]<-"Left-facing"
  }else{
    room[i]<-"Right-facing"
  }
  
}

frame.alcohol<-data.frame(alcohol=alcohol,care=care,room=room)

ggplot(frame.alcohol)+geom_bar(aes(fill=alcohol,x=care))+facet_wrap(~room)+
  scale_x_discrete(name="Care Type")+
  scale_y_continuous(name="Count")+
  scale_fill_discrete(name="Hand Hygiene Conducted?",labels=c("No","Yes"))+
  theme_pubr()+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))

  

### extract mean and sd for each contact number to do geom_ribbon later on
number.of.contacts<-100
mean.IV.left<-rep(NA,number.of.contacts)
sd.IV.left<-rep(NA,number.of.contacts)

mean.IV.right<-rep(NA,number.of.contacts)
sd.IV.right<-rep(NA,number.of.contacts)

mean.Obs.left<-rep(NA,number.of.contacts)
sd.Obs.left<-rep(NA,number.of.contacts)

mean.Obs.right<-rep(NA,number.of.contacts)
sd.Obs.right<-rep(NA,number.of.contacts)

mean.Rounds.left<-rep(NA,number.of.contacts)
sd.Rounds.left<-rep(NA,number.of.contacts)

mean.Rounds.right<-rep(NA,number.of.contacts)
sd.Rounds.right<-rep(NA,number.of.contacts)

for (i in 1:100){
  #------------ IV --------------------------------
  #IV - left facing
  mean.IV.left[i]<-mean(total$handR[total$care=="IV" & total$orientation=="left" & total$numcontact==i])
  sd.IV.left[i]<-sd(total$handR[total$care=="IV" & total$orientation=="left" & total$numcontact==i])
  
  #IV - right facing
  mean.IV.right[i]<-mean(total$handR[total$care=="IV" & total$orientation=="right" & total$numcontact==i])
  sd.IV.right[i]<-sd(total$handR[total$care=="IV" & total$orientation=="right" & total$numcontact==i])
  
  #------------ Obs --------------------------------
  #Obs - left facing
  mean.Obs.left[i]<-mean(total$handR[total$care=="Obs" & total$orientation=="left" & total$numcontact==i])
  sd.Obs.left[i]<-sd(total$handR[total$care=="Obs" & total$orientation=="left" & total$numcontact==i])
  
  #Obs - right facing
  mean.Obs.right[i]<-mean(total$handR[total$care=="Obs" & total$orientation=="right" & total$numcontact==i])
  sd.Obs.right[i]<-sd(total$handR[total$care=="Obs" & total$orientation=="right" & total$numcontact==i])
  
  #------------ Rounds --------------------------------
  #Rounds - left facing
  mean.Rounds.left[i]<-mean(total$handR[total$care=="Rounds" & total$orientation=="left" & total$numcontact==i])
  sd.Rounds.left[i]<-sd(total$handR[total$care=="Rounds" & total$orientation=="left" & total$numcontact==i])
  
  #Rounds - right facing
  mean.Rounds.right[i]<-mean(total$handR[total$care=="Rounds" & total$orientation=="right" & total$numcontact==i])
  sd.Rounds.right[i]<-sd(total$handR[total$care=="Rounds" & total$orientation=="right" & total$numcontact==i])
  
}

# combine into data.frame for geom_ribbon 

means<-c(mean.IV.left,mean.IV.right,mean.Obs.left,mean.Obs.right,mean.Rounds.left,mean.Rounds.right)
sd<-c(sd.IV.left,sd.IV.right,sd.Obs.left,sd.Obs.right,sd.Rounds.left,sd.Rounds.right)
care<-c(rep("IV",length(c(mean.IV.left,mean.IV.right))),
        rep("Obs",length(c(mean.Obs.left,mean.Obs.right))),
        rep("Rounds",length(c(mean.Rounds.left,mean.Rounds.right)))
        )
numcount<-rep(c(1:length(mean.IV.left)),6)
facing<-rep(c(rep("Left-facing",length(mean.IV.left)),rep("Right-facing",length(mean.IV.right))),3)

data.all<-data.frame(means=means,sd=sd,care=care,facing=facing,numcount=numcount)

windows()
ggplot(data=data.all)+
  geom_ribbon(aes(x=numcount,ymin=means-sd*1.96/sqrt(1000),ymax=means+sd*1.96/sqrt(1000),group=care,fill=care),alpha=0.5)+
  geom_line(aes(x=numcount,y=means,group=care,linetype=care,colour=care),size=1.5)+
  scale_y_continuous(name="Mean Concentration on Right Hand")+
  scale_x_continuous(name="Contact Number")+
  scale_color_discrete(name="Care Type")+
  scale_linetype_discrete(name="Care Type")+
  scale_fill_discrete(name="Care Type")+
  facet_wrap(~facing,ncol=1)+theme_pubr()+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15),
        legend.text=element_text(size=15),legend.title=element_text(size=15),
        legend.box="vertical",strip.text = element_text(size=15))

# ---------- Calculate average breakdown of contacts per run 

prop.farpatient<-rep(0,6)
prop.nearpatient<-rep(0,6)
prop.equipment<-rep(0,6)
prop.hygiene<-rep(0,6)
prop.alcohol<-rep(0,6)
prop.gloves<-rep(0,6)
prop.in<-rep(0,6)
prop.out<-rep(0,6)
prop.patient<-rep(0,6)

combo<-list()
combo[[1]]<-c("IV","left")
combo[[2]]<-c("IV","right")
combo[[3]]<-c("Obs","left")
combo[[4]]<-c("Obs","right")
combo[[5]]<-c("Rounds","left")
combo[[6]]<-c("Rounds","right")


  for (i in 1:6){
    combination<-combo[[i]]
    all.behaviors<-length(total$behavior[total$care==combination[1] & total$orientation==combination[2]])
    prop.farpatient[i]<-length(total$behavior[total$behavior=="FarPatient"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
    prop.nearpatient[i]<-length(total$behavior[total$behavior=="NearPatient"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
    prop.equipment[i]<-length(total$behavior[total$behavior=="Equipment"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
    prop.hygiene[i]<-length(total$behavior[total$behavior=="HygieneInside"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
    prop.alcohol[i]<-length(total$behavior[total$behavior=="Alcohol"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
    prop.gloves[i]<-length(total$behavior[total$behavior=="GlovesOn"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
    prop.in[i]<-length(total$behavior[total$behavior=="In"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
    prop.out[i]<-length(total$behavior[total$behavior=="Out"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
    prop.patient[i]<-length(total$behavior[total$behavior=="Patient"  & total$care==combination[1] & total$orientation==combination[2]])/all.behaviors
  }

percentage<-c(prop.farpatient,prop.nearpatient,prop.equipment,prop.hygiene,prop.alcohol,prop.gloves,prop.in,prop.out,prop.patient)
behavior<-c(rep("Far Patient",6),rep("Near Patient",6),rep("Equipment",6),rep("Hygiene",6),rep("Alcohol",6),rep("Gloves",6),rep("In",6),rep("Out",6),rep("Patient",6))
care<-c(rep(c("IV","IV","Obs","Obs","Rounds","Rounds"),9))
orientation<-rep(c("left","right","left","right","left","right"),9)
bar.frame<-data.frame(pecentage=percentage,behavior=behavior,care=care,orientation=orientation)

ggplot(data=bar.frame)+geom_bar(aes(y=percentage,x=interaction(care,orientation),fill=behavior),stat="identity")

####################################################################################


# ANALYSIS OF MOMENTS WHEN CONC DECREASED

#
library(ggplot2)
ggplot(total[total$numcontact==2,])+geom_violin(aes(x=care,y=proploss,fill=orientation))+
  scale_y_continuous(name="Proportion of Contacts per Run that Resulted in Loss")+
  scale_fill_discrete(name="Orientation",labels=c("Left-facing","Right-facing"))+
  scale_x_discrete(name="Care Type",labels=c("IV","Observation","Doctors' Rounds"))+facet_wrap(~alcohol)

hist(total$numcontact[total$loss=="yes"])

hist(total$numcontact[total$loss=="no"])

ggplot(data=total[!is.na(total$loss),])+geom_density(aes(x=numcontact,fill=interaction(care,orientation)),alpha=0.5)+
  scale_x_continuous(trans="log10")+
  facet_wrap(~loss)

View(total[total$proploss>.50,])

ggplot(data=total[!is.na(total$loss),])+geom_point(aes(x=numcontact,y=behavior,colour=loss))+facet_wrap(orientation~care)

# -------- NEED TO FIX THIS SECTION... ------------------------------------------------------------------------

in.maxyes<-rep(NA,max(total$numcontact)-1)
out.maxyes<-rep(NA,max(total$numcontact)-1)
patient.maxyes<-rep(NA,max(total$numcontact)-1)
hygieneinside.maxyes<-rep(NA,max(total$numcontact)-1)
equipment.maxyes<-rep(NA,max(total$numcontact)-1)
alcohol.maxyes<-rep(NA,max(total$numcontact)-1)
nearpatient.maxyes<-rep(NA,max(total$numcontact)-1)
farpatient.maxyes<-rep(NA,max(total$numcontact)-1)
gloveson.maxyes<-rep(NA,max(total$numcontact)-1)
for(j in 1:6){
  for (i in 2:max(total$numcontact)){
    in.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="In" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    out.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="Out" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    patient.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="Patient" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    hygieneinside.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="HygieneInside" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    equipment.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="Equipment" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    alcohol.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="Alcohol" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    nearpatient.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="NearPatient" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    farpatient.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="FarPatient" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
    gloveson.maxyes[i]<-length(total$loss[total$loss=="yes" & total$behavior=="GlovesOn" & total$numcontact==i & total$orientation==combo[[j]][2] & total$care==combo[[j]][1]])
  }
  if(j==1){
    in.maxyes.total<-in.maxyes
    out.maxyes.total<-out.maxyes
    patient.maxyes.total<-patient.maxyes
    hygieneinside.maxyes.total<-hygieneinside.maxyes
    equipment.maxyes.total<-equipment.maxyes
    alcohol.maxyes.total<-alcohol.maxyes
    nearpatient.maxyes.total<-nearpatient.maxyes
    farpatient.maxyes.total<-farpatient.maxyes
    gloveson.maxyes.total<-gloveson.maxyes
    orientation.total<-rep(combo[[j]][2],max(total$numcontact))
    care.total<-rep(combo[[j]][1],max(total$numcontact))
  }else{
    in.maxyes.total<-c(in.maxyes.total,in.maxyes)
    out.maxyes.total<-c(out.maxyes.total,out.maxyes)
    patient.maxyes.total<-c(patient.maxyes.total,patient.maxyes)
    hygieneinside.maxyes.total<-c(hygieneinside.maxyes.total,hygieneinside.maxyes)
    equipment.maxyes.total<-c(equipment.maxyes.total,equipment.maxyes)
    alcohol.maxyes.total<-c(alcohol.maxyes.total,alcohol.maxyes)
    nearpatient.maxyes.total<-c(nearpatient.maxyes.total,nearpatient.maxyes)
    farpatient.maxyes.total<-c(farpatient.maxyes.total,farpatient.maxyes)
    gloveson.maxyes.total<-c(gloveson.maxyes.total,gloveson.maxyes)
    
    orientation<-rep(combo[[j]][2],max(total$numcontact))
    care<-rep(combo[[j]][1],max(total$numcontact))
    
    orientation.total<-c(orientation.total,orientation)
    care.total<-c(care.total,care)
    
  }
  
}

totalyes<-c(in.maxyes.total,out.maxyes.total,patient.maxyes.total,hygieneinside.maxyes.total,
            equipment.maxyes.total,alcohol.maxyes.total,nearpatient.maxyes.total,
            farpatient.maxyes.total,gloveson.maxyes.total)
length.all<-length(in.maxyes.total)
behavior<-c(rep("In",length.all),rep("Out",length.all),rep("Patient",length.all),rep("Hygiene Inside",length.all),
            rep("Equipment",length.all),rep("Alcohol",length.all),rep("Near Patient",length.all),rep("Far Patient",length.all),
            rep("GlovesOn",length.all))
numcount<-rep(1:max(total$numcontact),54)
care.total<-rep(care.total,9)
orientation.total<-rep(orientation.total,9)

frame.compare.yes.behavior<-data.frame(totalyes=totalyes,behavior=behavior,care.total=care.total,orientation.total=orientation.total,numcount=numcount)


#frame.compare.yes.behavior<-frame.compare.yes.behavior[frame.compare.yes.behavior$care.total=="IV",]
ggplot(data=frame.compare.yes.behavior[frame.compare.yes.behavior$numcount<=50,],aes(numcount,behavior))+geom_tile(aes(fill=totalyes),colour="white")+
  scale_fill_gradient(low="white",high="blue",name="Number of Decreases")+scale_x_continuous(name="Contact Number")+
  scale_y_discrete("Behavior")+facet_wrap(orientation.total~care.total)



#histogram of lambda and lambda prior

prior<-runif(100000,0.0001,0.406)

transf<-c(lambda)
type<-c(rep("posterior",length(lambda)))

frame.te<-data.frame(trasnf=transf,type=type)

windows()
ggplot(frame.te)+geom_density(aes(x=transf),fill="light blue",alpha=0.3)+
  geom_histogram(aes(x=transf,y=..density..),fill="light blue",color="black",alpha=0.3)+
  scale_x_continuous(name="Posterior Transfer Efficiency")+
  scale_y_continuous(name="Density")+
  theme_pubr()+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15))

#------------- other plots-------------------------

#ggplot(data=data,aes(x=numcontacts,y=hand.R.max,group=care))+geom_point(aes(x=numcontacts,y=hand.R.max,colour=care))+
#  geom_smooth(method='lm',aes(colour=care))+facet_wrap(~room.face)

A<-ggplot(data=data,aes(x=numcontacts,y=hand.R.max,group=interaction(gloves,care)))+geom_point(aes(x=numcontacts,y=hand.R.max,colour=interaction(gloves,care)))+
  geom_smooth(method='lm',aes(colour=interaction(gloves,care)))+facet_wrap(~room.face)+
  scale_y_continuous(name="Max Conc. on Right Hand")+
  scale_x_continuous(name="Number of Contacts")+
  scale_color_discrete(name="Care Type")

B<-ggplot(data=data,aes(x=numcontacts,y=hand.R.mean,group=interaction(gloves,care)))+geom_point(aes(x=numcontacts,y=hand.R.mean,colour=interaction(gloves,care)))+
  geom_smooth(method='lm',aes(colour=interaction(gloves,care)))+facet_wrap(~room.face)+
  scale_y_continuous(name="Mean Conc. on Right Hand")+
  scale_x_continuous(name="Number of Contacts")+
  scale_color_discrete(name="Care Type")

C<-ggplot(data=data,aes(x=numcontacts,y=hand.L.max,group=interaction(gloves,care)))+geom_point(aes(x=numcontacts,y=hand.L.max,colour=interaction(gloves,care)))+
  geom_smooth(method='lm',aes(colour=interaction(gloves,care)))+facet_wrap(~room.face)+
  scale_y_continuous(name="Max Conc. on Left Hand")+
  scale_x_continuous(name="Number of Contacts")+
  scale_color_discrete(name="Care Type")

D<-ggplot(data=data,aes(x=numcontacts,y=hand.L.mean,group=interaction(gloves,care)))+geom_point(aes(x=numcontacts,y=hand.L.mean,colour=interaction(gloves,care)))+
  geom_smooth(method='lm',aes(colour=interaction(gloves,care)))+facet_wrap(~room.face)+
  scale_y_continuous(name="Mean Conc. on Left Hand")+
  scale_x_continuous(name="Number of Contacts")+
  scale_color_discrete(name="Care Type")

library(ggpubr)
windows()
ggarrange(A,B,C,D,common.legend=TRUE)


E<-ggplot(data=data,aes(x=numcontacts,y=hand.both.max,group=interaction(gloves,care)))+geom_point(aes(x=numcontacts,y=hand.R.max,colour=interaction(gloves,care)))+
  # geom_smooth(method='lm',aes(colour=interaction(gloves,care)))+
  facet_wrap(~room.face)+
  scale_y_continuous(name="Max Conc. on Hands",trans="log10")+
  scale_x_continuous(name="Number of Contacts",trans="log10")+
  scale_color_discrete(name="Care Type")

G<-ggplot(data=data,aes(x=numcontacts,y=hand.both.mean,group=interaction(gloves,care)))+geom_point(aes(x=numcontacts,y=hand.R.mean,colour=interaction(gloves,care)))+
  #geom_smooth(method='lm',aes(colour=interaction(gloves,care)))+
  facet_wrap(~room.face)+
  scale_y_continuous(name="Mean Conc. on a Single Hand",trans="log10")+
  scale_x_continuous(name="Number of Contacts",trans="log10")+
  scale_color_discrete(name="Care Type",labels=c("IV, No Gloves","IV, Gloves","Observation, No Gloves",
                                                 "Observation, Gloves","Rounds, No Gloves"))

windows()
ggarrange(E,G,common.legend = TRUE,ncol=1)

windows()

ggplot(data=data)+geom_density(aes(x=hand.both.mean,fill=interaction(gloves,room.face)),alpha=.5)+facet_wrap(~care)+scale_x_continuous(trans="log10")
ggplot(data=data)+geom_density(aes(x=numcontacts,fill=interaction(gloves,room.face)),alpha=.5)+facet_wrap(~care)+scale_x_continuous(trans="log10")
ggplot(data=data[data$numcontacts==20,])+geom_density(aes(x=hand.both.mean,fill=interaction(gloves,room.face)),alpha=.5)+facet_wrap(~care)+scale_x_continuous(trans="log10")

G


