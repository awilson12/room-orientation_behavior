#exploring frequency of hand hygiene and frequency of the glove donning/doffing sequence issue to
#address reviewer comments


airsurfarg=TRUE

#running function for left and right room orientation - IV care
behavior.sim(room.orientation = "left",caretype = "IV",numsequence = iter, airsurf=airsurfarg)
seq.IV.left<-behavior.total
exposure.IV.left<-exposure.frame

behavior.sim(room.orientation = "right",caretype = "IV",numsequence = iter, airsurf=airsurfarg)
seq.IV.right<-behavior.total
exposure.IV.right<-exposure.frame

# running function for left and right room orientation - Obs care
behavior.sim(room.orientation = "left",caretype = "Obs",numsequence = iter, airsurf=airsurfarg)
seq.Obs.left<-behavior.total
exposure.Obs.left<-exposure.frame

behavior.sim(room.orientation = "right",caretype = "Obs",numsequence = iter, airsurf=airsurfarg)
seq.Obs.right<-behavior.total
exposure.Obs.right<-exposure.frame

# running function for left and right room orientation - Doctors' rounds
behavior.sim(room.orientation = "left",caretype = "Rounds",numsequence = iter, airsurf=airsurfarg)
seq.Rounds.left<-behavior.total
exposure.Rounds.left<-exposure.frame

behavior.sim(room.orientation = "right",caretype = "Rounds",numsequence = iter, airsurf=airsurfarg)
seq.Rounds.right<-behavior.total
exposure.Rounds.right<-exposure.frame

