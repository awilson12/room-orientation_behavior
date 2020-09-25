# Amanda Wilson and Marco-Felipe King
# University of Arizona and Univeristy of Leeds collaboration

#read in csv file for transfer efficiencies
lambda<-read_csv("lambda.csv",col_names=FALSE)
lambda<-lambda$X1


#-------------------------  Function for estimating behavior sequences and viral exposure -------------------------------------------------------------------------------

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
    
    ANSYS$ORIENTATION[ANSYS$ORIENTATION=="Right-facing"]<-1
    ANSYS$ORIENTATION[ANSYS$ORIENTATION=="Left-facing"]<-2
    
    
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

