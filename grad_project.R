###################################
#allisons code
##################################


library(stringr)

Numextract <- function(string){
  as.numeric(unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string))))
}

ecan <- openxlsx::read.xlsx("C:/Users/scl29/OneDrive/Desktop/Kansas State/lab/Personal project/2020-ecan.xlsx")

# ecan calcs
ecan$biomass <- NA
ecan <- ecan[-c(490, 491),] # this one has problems-- N and l don't match

options(warn=2)
for (i in 1:dim(ecan)[1]){
  ecan$biomass[i] <- sum(Numextract(ecan$n_ros[i]) *Numextract(ecan$l_ros[i])) + 
    sum(Numextract(ecan$n_f[i]) *Numextract(ecan$l_f[i]))
}

ecan$totF <- ecan$f

"For amca, size is simply N (the number of stalks)* basal area of the tallest stalk. Fruiting effort is the length of each flowering spike, summed across spikes. We measure each spike separately and write the data in the same way as for N_f, above. 

Here is some code that does that, storing fruiting effort as totF:"

amca <- openxlsx::read.xlsx("C:/Users/scl29/OneDrive/Desktop/Kansas State/lab/Personal project/2020-amca.xlsx")

amca$biomass <- amca$N * ((amca$D/2)^2)

amca$totF <- NA
for (i in 1:dim(amca)[1]){
  amca$totF[i] <- sum(Numextract(amca$L_F[i]))}


kueu <- openxlsx::read.xlsx("C:/Users/scl29/OneDrive/Desktop/Kansas State/lab/Personal project/2020-kueu.xlsx")


"For kueu, size is number of stalks x height, and fruiting effort is measured in the same way as for amca. See code below, which  stores fruiting effort as totF: "


kueu$biomass <- kueu$n *(kueu$h)

kueu$totF <- NA
for (i in 1:dim(kueu)[1]){
  kueu$totF[i] <- sum(Numextract(kueu$f[i]))
}

"For asob (my favorite species), size is  height of stalk* basal area of stalk, summed across all stalks. We just count the number of fruits (column f). See code below: "

asob <- openxlsx::read.xlsx("C:/Users/scl29/OneDrive/Desktop/Kansas State/lab/Personal project/2020-asob.xlsx")

# asob calcs
asob$biomass <- NA
for (i in 1:dim(asob)[1]){
  asob$biomass[i] <- sum((Numextract(asob$d[i])/2)^2 *Numextract(asob$h[i]))}

########################################################
#make all files into dataframes and subtract unnecessary colunms 
###################################################
amca.df <- as.data.frame(amca)
str(amca.df)
amca.df <- subset (amca.df, select = -c(x,y,mrk,N,D,L_F,cov,meas,note,comm))

asob.df <- as.data.frame(asob)
str(asob.df)
asob.df <- subset (asob.df, select = -c(x,y,mrk,h,d,cov,meas,note,comm))

kueu.df <- as.data.frame(kueu)
str(kueu.df)
kueu.df <- subset (kueu.df, select = -c(x,y,mrk,n,h,f,cov,meas,note,comm))

ecan.df <- as.data.frame(ecan)
str(ecan.df)
ecan.df <- subset (ecan.df, select = -c(x,y,mrk,n_ros,l_ros,f,l_f,cov,meas,note,comm))


mylogit <- glm(biomass ~ bison + fire, data = amca.df)

#summary(mylogit)

#plot(mylogit)



#?glm

#n, bins, patches = pl.hist(R, nbins, normed=True)

###############
#Amca#
##############

#summary(Grazers_amca)

#subset K1A watershed 
K1A<-subset(amca.df,watershed=="K1A", na.rm=TRUE) 


#subset K1B watershed
K1B<-subset(amca.df,watershed=="K1B", na.rm=TRUE) 

k1combined<- rbind(K1A,K1B)


#subset K2A watershed 
K2A<-subset(amca.df,watershed=="K2A", na.rm=TRUE) 

#subset K4A watershed 
K4A<-subset(amca.df,watershed=="K4A", na.rm=TRUE) 


#subset N1A watershed 
N1A<-subset(amca.df,watershed=="N1A", na.rm=TRUE) 

#subset N2A watershed 
N2A<-subset(amca.df,watershed=="N2A", na.rm=TRUE) 


#subset N4D watershed 
N4D<-subset(amca.df,watershed=="N4D", na.rm=TRUE) 


##########################
#Asob#
##########################

#subset K2A watershed 
K2AA<-subset(asob.df,watershed=="K2A", na.rm=TRUE) 

#subset K4A watershed 
K4AA<-subset(asob.df,watershed=="K4A", na.rm=TRUE) 



##########################
#Ecan#
##########################

#subset K1A watershed 
K1Ae<-subset(ecan.df,watershed=="K1A", na.rm=TRUE) 

#subset K1B watershed 
K1Be<-subset(ecan.df,watershed=="K1B", na.rm=TRUE) 

#subset K2A watershed 
K2Ae<-subset(ecan.df,watershed=="K2A", na.rm=TRUE) 

#subset K4A watershed 
K4Ae<-subset(ecan.df,watershed=="K4A", na.rm=TRUE) 

#subset N1A watershed 
N1Ae<-subset(ecan.df,watershed=="N1A", na.rm=TRUE) 

#subset N2A watershed 
N2Ae<-subset(ecan.df,watershed=="N2A", na.rm=TRUE) 

#subset N4D watershed 
N4De<-subset(ecan.df,watershed=="N4D", na.rm=TRUE) 



##########################
#Kueu#
##########################

#subset K1A watershed 
K1Ak<-subset(kueu.df,watershed=="K1A", na.rm=TRUE) 

#subset K2A watershed 
K2Ak<-subset(kueu.df,watershed=="K2A", na.rm=TRUE) 

#subset K4A watershed 
K4Ak<-subset(kueu.df,watershed=="K4A", na.rm=TRUE) 

#subset N1A watershed
N1Ak<-subset(kueu.df,watershed=="N1A", na.rm=TRUE) 

K4AA K2AA K1Ae K2Ae K4Ae N1Ae N2Ae N4De K1Ak K2Ak K4Ak N1Ak

# project: AMCA

#11/16/2021
#
#subset K1A watershed 


### grazer= y/n (2) fri=1,2,4 (3)
#AMCA
K1A
K1B
K2A
K4A

N1A
N2A 
N4D


#ecan

K1Ae
K2Ae
K4Ae 

N1Ae
N2Ae
N4De

#ASOB -imma exclude because no graze effect
K4AA
K2AA 

#KuEU
K1Ak
K2Ak
K4Ak 

N1Ak



#myfile<-cbind(K1A$biomass,K1B$biomass)

k1combined<- rbind(K1A,K1B)

#
#
# this script uses a smoothed spline (loess function) to look at diffs in size struture (PDF-- not CDF) across levels and treatments
#
# packages: gtools 
#
# written: 

#lines 11-90 just cleaning up the data 2021 comment

# modified: 
library(gtools)

# AMCA

ugAMCAmymatrix <- matrix(NA, nrow=618, ncol=3)



ugAMCAmymatrix[1:length(k1combined$biomass),1]= k1combined$biomass
ugAMCAmymatrix[1:length(K2A$biomass),2]= K2A$biomass
ugAMCAmymatrix[1:length(K4A$biomass),3]= K4A$biomass


gAMCAmymatrix <- matrix(NA, nrow=420, ncol=3)
gAMCAmymatrix[1:length(N1A$biomass),1]= N1A$biomass
gAMCAmymatrix[1:length(N2A$biomass),2]= N2A$biomass
gAMCAmymatrix[1:length(N4D$biomass),3]= N4D$biomass


#ecan
ugECANmymatrix <- matrix(NA, nrow=618, ncol=3)

K1Ae

ugECANmymatrix[1:length(K1Ae$biomass),1]= K1Ae$biomass
ugECANmymatrix[1:length(K2Ae$biomass),2]= K2Ae$biomass
ugECANmymatrix[1:length(K4Ae$biomass),3]= K4Ae$biomass


gECANmymatrix <- matrix(NA, nrow=420, ncol=3)
gECANmymatrix[1:length(N1Ae$biomass),1]= N1Ae$biomass
gECANmymatrix[1:length(N2Ae$biomass),2]= N2Ae$biomass
gECANmymatrix[1:length(N4De$biomass),3]= N4De$biomass
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
######################shannon################################################

#an ecdf is an empirical cdf just the cdf counts
#setting up a code to generate ecdf
#95-213 just rearraging things (2021 comment)
AMCA_K1All <-ugAMCAmymatrix[,1]
AMCA_K2A <- ugAMCAmymatrix[,2]
AMCA_K4A <- ugAMCAmymatrix[,3]


AMCA_N1A <- gAMCAmymatrix[,1]
AMCA_N2A <- gAMCAmymatrix[,2]
AMCA_N4D <- gAMCAmymatrix[,3]


ECAN_K1Ae <-ugECANmymatrix[,1]
ECAN_K2Ae <- ugECANmymatrix[,2]
ECAN_K4Ae <- ugECANmymatrix[,3]


ECAN_N1Ae <- gECANmymatrix[,1]
ECAN_N2Ae <- gECANmymatrix[,2]
ECAN_N4De <- gECANmymatrix[,3]

AMCA_K1All <- AMCA_K1All[!is.na(AMCA_K1All)]
AMCA_K2A <- AMCA_K2A[!is.na(AMCA_K2A)]
AMCA_K4A <- AMCA_K4A[!is.na(AMCA_K4A)]
AMCA_N1A <- AMCA_N1A[!is.na(AMCA_N1A)]
AMCA_N2A <- AMCA_N2A[!is.na(AMCA_N2A)]
AMCA_N4D <- AMCA_N4D[!is.na(AMCA_N4D)]

ECAN_K1Ae <- ECAN_K1Ae[!is.na(ECAN_K1Ae)]
ECAN_K2Ae <- ECAN_K2Ae[!is.na(ECAN_K2Ae)]
ECAN_K4Ae <- ECAN_K4Ae[!is.na(ECAN_K4Ae)]
ECAN_N1Ae <- ECAN_N1Ae[!is.na(ECAN_N1Ae)]
ECAN_N2Ae <- ECAN_N2Ae[!is.na(ECAN_N2Ae)]
ECAN_N4De <- ECAN_N4De[!is.na(ECAN_N4De)]

#ugAMCAmymatrix1<-cbind(AMCA_K1A,AMCA_K1B,AMCA_K2A,AMCA_K4A)


ugAMCAmymatrix1 <- matrix(data=NA,nrow=max(c(length(AMCA_K1All), length(AMCA_K2A), length(AMCA_K4A))), ncol=3)
ugAMCAmymatrix1[1:length(AMCA_K1All),1] <-AMCA_K1All
ugAMCAmymatrix1[1:length(AMCA_K2A), 2] <- AMCA_K2A
ugAMCAmymatrix1[1:length(AMCA_K4A),3]  <- AMCA_K4A

gAMCAmymatrix1 <- matrix(data=NA,nrow=max(c(length(AMCA_N1A), length(AMCA_N2A), length(AMCA_N4D))), ncol=3)
gAMCAmymatrix1[1:length(AMCA_N1A),1] <-AMCA_N1A
gAMCAmymatrix1[1:length(AMCA_N2A), 2] <- AMCA_N2A
gAMCAmymatrix1[1:length(AMCA_N4D), 3] <- AMCA_N4D


ugECANmymatrix1 <- matrix(data=NA,nrow=max(c(length(ECAN_K1Ae), length(ECAN_K2Ae), length(ECAN_K4Ae))), ncol=3)
ugECANmymatrix1[1:length(ECAN_K1Ae),1] <-ECAN_K1Ae
ugECANmymatrix1[1:length(ECAN_K2Ae), 2] <- ECAN_K2Ae
ugECANmymatrix1[1:length(ECAN_K4Ae),3]  <- ECAN_K4Ae

gECANmymatrix1 <- matrix(data=NA,nrow=max(c(length(ECAN_N1Ae), length(ECAN_N2Ae), length(ECAN_N4De))), ncol=3)
gECANmymatrix1[1:length(ECAN_N1Ae),1] <-ECAN_N1Ae
gECANmymatrix1[1:length(ECAN_N2Ae), 2] <- ECAN_N2Ae
gECANmymatrix1[1:length(ECAN_N4De), 3] <- ECAN_N4De


#storing these cdf values my set up is likely to be different cause all my treatment combinations

ugAMCAstoreCDF <- matrix(data=NA,nrow=115, ncol=4)

gAMCAstoreCDF <- matrix(data=NA,nrow=115, ncol=4)

#lines 214-220 setting up "breakpoints" for CDF that are always the same
AMCAbrkpnts <- seq(0,max(c(ugAMCAmymatrix1,gAMCAmymatrix1), na.rm=T),max(c(ugAMCAmymatrix1 ,gAMCAmymatrix1), na.rm=T)/114)


ugECANstoreCDF <- matrix(data=NA,nrow=115, ncol=4)

gECANstoreCDF <- matrix(data=NA,nrow=115, ncol=4)

#lines 214-220 setting up "breakpoints" for CDF that are always the same
ECANbrkpnts <- seq(0,max(c(ugECANmymatrix1,gECANmymatrix1), na.rm=T),max(c(ugECANmymatrix1 ,gECANmymatrix1), na.rm=T)/114)


#414.1298 dim(AMCAbrkpnts)
#*******************shannon************************** 
#lines 229- 246 code that actually calcuates the CDF (2021 comment)

# one dimensional case: 
for (i in c(1,2,3)){
  
  # Ndiam
  
  ugwatershed <- ugAMCAmymatrix1[,i] # setting the treatment within N diameter
  ugwatershed <- ugwatershed[!is.na(ugwatershed)] # removing NAs
  
  #cycle through break points and say how many then tell me how many instances are less than or equal to the next break point are to a */ cdf= notPDF #break points should be equal all in the trt she pick 80 and 100
  ####
  PDF <- vector(mode="numeric", length=length(AMCAbrkpnts))
  for (ii in 1:length(AMCAbrkpnts)){
    PDFvalue <- length(ugwatershed[ugwatershed<= AMCAbrkpnts[ii+1]]) # how many plants are under the bounds described by Ndiambrkpnts
    PDF[ii] <- PDFvalue} # put that number in the appropriate location in the PDF
  #
  ugAMCAstoreCDF[,i] <- PDF} #store it away
ugAMCAstoreCDF[,4] <- AMCAbrkpnts # store the breakpoints
ugAMCA_cdf <- ugAMCAstoreCDF # rename it
#lines 248-307 code just repeat the loop on lines 229-246, so I think you can ignore (2021 comment)


#*****************************************************************************************************


#grazed
# one dimensional case: 
for (i in c(1,2,3)){
  
  # Ndiam
  
  gwatershed <- gAMCAmymatrix1[,i] # setting the treatment within N diameter
  gwatershed <- gwatershed[!is.na(gwatershed)] # removing NAs
  
  #cycle through break points and say how many then tell me how many instances are less than or equal to the next break point are to a */ cdf= notPDF #break points should be equal all in the trt she pick 80 and 100
  ####
  PDF <- vector(mode="numeric", length=length(AMCAbrkpnts))
  for (ii in 1:length(AMCAbrkpnts)){
    PDFvalue <- length(gwatershed[gwatershed<= AMCAbrkpnts[ii+1]]) # how many plants are under the bounds described by Ndiambrkpnts
    PDF[ii] <- PDFvalue} # put that number in the appropriate location in the PDF
  #
  gAMCAstoreCDF[,i] <- PDF} #store it away
gAMCAstoreCDF[,4] <- AMCAbrkpnts # store the breakpoints
gAMCA_cdf <- gAMCAstoreCDF # rename it



# one dimensional case: 
for (i in c(1,2,3)){
  
  # Ndiam
  
  ugwatershede <- ugECANmymatrix1[,i] # setting the treatment within N diameter
  ugwatershede <- ugwatershede[!is.na(ugwatershed)] # removing NAs
  
  #cycle through break points and say how many then tell me how many instances are less than or equal to the next break point are to a */ cdf= notPDF #break points should be equal all in the trt she pick 80 and 100
  ####
  PDF <- vector(mode="numeric", length=length(ECANbrkpnts))
  for (ii in 1:length(ECANbrkpnts)){
    PDFvalue <- length(ugwatershede[ugwatershede<= ECANbrkpnts[ii+1]]) # how many plants are under the bounds described by Ndiambrkpnts
    PDF[ii] <- PDFvalue} # put that number in the appropriate location in the PDF
  #
  ugECANstoreCDF[,i] <- PDF} #store it away
ugECANstoreCDF[,4] <- ECANbrkpnts # store the breakpoints
ugECAN_cdf <- ugECANstoreCDF # rename it
#lines 248-307 code just repeat the loop on lines 229-246, so I think you can ignore (2021 comment)


#*****************************************************************************************************


#grazed
# one dimensional case: 
for (i in c(1,2,3)){
  
  # Ndiam
  
  gwatershede <- gECANmymatrix1[,i] # setting the treatment within N diameter
  gwatershede <- gwatershede[!is.na(gwatershede)] # removing NAs
  
  #cycle through break points and say how many then tell me how many instances are less than or equal to the next break point are to a */ cdf= notPDF #break points should be equal all in the trt she pick 80 and 100
  ####
  PDF <- vector(mode="numeric", length=length(ECANbrkpnts))
  for (ii in 1:length(ECANbrkpnts)){
    PDFvalue <- length(gwatershede[gwatershede<= ECANbrkpnts[ii+1]]) # how many plants are under the bounds described by Ndiambrkpnts
    PDF[ii] <- PDFvalue} # put that number in the appropriate location in the PDF
  #
  gECANstoreCDF[,i] <- PDF} #store it away
gECANstoreCDF[,4] <- ECANbrkpnts # store the breakpoints
gECAN_cdf <- gECANstoreCDF # rename it

# lines 311-322: changing CDF from raw counts to proportions

ugAMCAstoreCDF[,1] <- ugAMCAstoreCDF[,1]/max(ugAMCAstoreCDF[,1])# changing those raw counts to proportions
ugAMCAstoreCDF[,2] <- ugAMCAstoreCDF[,2]/max(ugAMCAstoreCDF[,2])
ugAMCAstoreCDF[,3] <- ugAMCAstoreCDF[,3]/max(ugAMCAstoreCDF[,3])



gAMCAstoreCDF[,1] <- gAMCAstoreCDF[,1]/max(gAMCAstoreCDF[,1])
gAMCAstoreCDF[,2] <- gAMCAstoreCDF[,2]/max(gAMCAstoreCDF[,2])
gAMCAstoreCDF[,3] <- gAMCAstoreCDF[,3]/max(gAMCAstoreCDF[,3])


ugECANstoreCDF[,1] <- ugECANstoreCDF[,1]/max(ugECANstoreCDF[,1])# changing those raw counts to proportions
ugECANstoreCDF[,2] <- ugECANstoreCDF[,2]/max(ugECANstoreCDF[,2])
ugECANstoreCDF[,3] <- ugECANstoreCDF[,3]/max(ugECANstoreCDF[,3])



gECANstoreCDF[,1] <- gECANstoreCDF[,1]/max(gECANstoreCDF[,1])
gECANstoreCDF[,2] <- gECANstoreCDF[,2]/max(gECANstoreCDF[,2])
gECANstoreCDF[,3] <- gECANstoreCDF[,3]/max(gECANstoreCDF[,3])


# lines 324-342: changing those proportions to logit transformations
ugAMCAstoreCDFlogit <- ugAMCAstoreCDF 

gAMCAstoreCDFlogit <- gAMCAstoreCDF 

ugECANstoreCDFlogit <- ugECANstoreCDF 

gECANstoreCDFlogit <- gECANstoreCDF 


#detach(package:gtools)
#library(gtools)
ugAMCAstoreCDFlogit[,1] <- logit(ugAMCAstoreCDF[,1], min=0.025, max=1.025)# changing those raw counts to proportions
ugAMCAstoreCDFlogit[,2] <- logit(ugAMCAstoreCDF[,2], min=0.025, max=1.025)
ugAMCAstoreCDFlogit[,3] <- logit(ugAMCAstoreCDF[,3], min=0.025, max=1.025)



gAMCAstoreCDFlogit[,1] <- logit(gAMCAstoreCDF[,1], min=0.025, max=1.025)# changing those raw counts to proportions
gAMCAstoreCDFlogit[,2] <- logit(gAMCAstoreCDF[,2], min=0.025, max=1.025)
gAMCAstoreCDFlogit[,3] <- logit(gAMCAstoreCDF[,3], min=0.025, max=1.025)


ugECANstoreCDFlogit[,1] <- logit(ugECANstoreCDF[,1], min=0.025, max=1.025)# changing those raw counts to proportions
ugECANstoreCDFlogit[,2] <- logit(ugECANstoreCDF[,2], min=0.025, max=1.025)
ugECANstoreCDFlogit[,3] <- logit(ugECANstoreCDF[,3], min=0.025, max=1.025)


gECANstoreCDFlogit[,1] <- logit(gECANstoreCDF[,1], min=0.025, max=1.025)# changing those raw counts to proportions
gECANstoreCDFlogit[,2] <- logit(gECANstoreCDF[,2], min=0.025, max=1.025)
gECANstoreCDFlogit[,3] <- logit(gECANstoreCDF[,3], min=0.025, max=1.025)

# lines 344- 517 are fitting the logit-transformed CDF values as a function of size+size^2+size^3, etc. reaplace with: 
##dredge(glm(y~size+size^2+size^3... size^10, familly= binomial) each treatment combination (2021 comment)
# fitting raw counts: each trmt separately.~333- 493 come back to her after I get to this point maybe try up to the 10th power then use aicc to compare them

#1 is control
# 2 is LMH
# 3 is MESO
# 4 is MEGA
#5 is size classes

#  if any AIC chooses the highest order model ( I believe 39th order), then need to try higher order models in the AIC

mypredictorsstartlist <- list(ugAMCAstoreCDFlogit[,4], gAMCAstoreCDFlogit[,4]) # makes a list of the predictors for each of the sites/ metric combinations
myresponsestartlist <- list(ugAMCAstoreCDFlogit, gAMCAstoreCDFlogit)# makes a list of the responses for each of the sites/ metric combinations
allvalues <- array(data=NA,dim=c(40,3,3,2))# the first dimension is the models of differing orders. the second dimension is the 
#info about each of those models (LL, p, q), and the third dimension here is the different treatments 
#within a site; the 4th dimension here is the differnet sites/metrics (north diameter, central diameter, south diameter)
bestsindex <- matrix(NA,nrow=3,ncol=2) # this matrix will store the order number of the best model for each treatment (row), and size metric/ location combination (column). 
# change number of columns when you add in  height

for (ii in 1:2){# will be 1:6 when I get height in there
  
  # generating squared, cubed, etc. values of diameter
  mypredictorsstart <- mypredictorsstartlist[[ii]] # setting the predictors either at North, Central, or South
  mylogitresponse <- myresponsestartlist[[ii]] # setting the responses either at North, Central, or South
  predictors <- cbind(mypredictorsstart, mypredictorsstart^2, mypredictorsstart^3, mypredictorsstart^4, mypredictorsstart^5, mypredictorsstart^6, mypredictorsstart^7, mypredictorsstart^8, mypredictorsstart^9, mypredictorsstart^10, mypredictorsstart^11, mypredictorsstart^12, mypredictorsstart^13, mypredictorsstart^14, mypredictorsstart^15, mypredictorsstart^16, mypredictorsstart^17, mypredictorsstart^18, mypredictorsstart^19, mypredictorsstart^20, mypredictorsstart^21, mypredictorsstart^22, mypredictorsstart^23, mypredictorsstart^24, mypredictorsstart^25, mypredictorsstart^26, mypredictorsstart^27, mypredictorsstart^28, mypredictorsstart ^29, mypredictorsstart ^30, mypredictorsstart ^31, mypredictorsstart ^32, mypredictorsstart^33, mypredictorsstart^34, mypredictorsstart^35, mypredictorsstart^36, mypredictorsstart^37, mypredictorsstart^38, mypredictorsstart ^39, mypredictorsstart ^40)
  
  
  values <- array(NA,c(40,3,3)) # the first dimension is the models of differing orders. the second dimension is the info about each of those models (LL, p, q), and the third dimension here is the different treatments within a site
  for (i in 1:3){# I'm going to assume that the logLik() function gives you the max loglike (as in Doak and Morris)
    
    # here, the rows are the different models, with the columns corresponding to Loglik, p, and q, respectively
    values[1,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1])) # log likelihood
    # need to bring down the i here to make sure all have this same reference in them. 
    values[1,2,i] <- 2# p
    values[1,3,i] <- length(mylogitresponse[,i])# q
    values[2,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]))
    values[2,2,i] <- 3# p
    values[2,3,i] <- length(mylogitresponse[,i])# q
    values[3,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]))
    values[3,2,i] <- 4# p
    values[3,3,i] <- length(mylogitresponse[,i])# q
    values[4,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]))
    values[4,2,i] <- 5# p
    values[4,3,i] <- length(mylogitresponse[,i])# q
    values[5,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]))
    values[5,2,i] <- 6# p
    values[5,3,i] <- length(mylogitresponse[,i])# q
    values[6,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]))
    values[6,2,i] <- 7# p
    values[6,3,i] <- length(mylogitresponse[,i])# q
    values[7,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]))
    values[7,2,i] <- 8
    values[7,3,i] <- length(mylogitresponse[,i])# q
    values[8,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]))
    values[8,2,i] <- 9
    values[8,3,i] <- length(mylogitresponse[,i])# q
    values[9,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]))
    values[9,2,i] <- 10
    values[9,3,i] <- length(mylogitresponse[,i])# q
    values[10,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]))
    values[10,2,i] <- 11
    values[10,3,i] <- length(mylogitresponse[,i])# q
    values[11,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]))
    values[11,2,i] <- 12
    values[11,3,i] <- length(mylogitresponse[,i])# q
    values[12,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]))
    values[12,2,i] <- 13
    values[12,3,i] <- length(mylogitresponse[,i])# q
    values[13,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]))
    values[13,2,i] <- 14
    values[13,3,i] <- length(mylogitresponse[,i])# q
    values[14,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]))
    values[14,2,i] <- 15
    values[14,3,i] <- length(mylogitresponse[,i])# q
    values[15,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]))
    values[15,2,i] <- 16
    values[15,3,i] <- length(mylogitresponse[,i])# q
    values[16,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]))
    values[16,2,i] <- 17
    values[16,3,i] <- length(mylogitresponse[,i])# q
    values[17,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]))
    values[17,2,i] <- 18
    values[17,3,i] <- length(mylogitresponse[,i])# q
    values[18,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]))
    values[18,2,i] <- 19
    values[18,3,i] <- length(mylogitresponse[,i])# q
    values[19,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]))
    values[19,2,i] <- 20
    values[19,3,i] <- length(mylogitresponse[,i])# q
    values[20,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]))
    values[20,2,i] <- 21
    values[20,3,i] <- length(mylogitresponse[,i])# q
    values[21,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]))
    values[21,2,i] <- 22
    values[21,3,i] <- length(mylogitresponse[,i])# q
    values[22,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]))
    values[22,2,i] <- 23
    values[22,3,i] <- length(mylogitresponse[,i])# q
    values[23,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]))
    values[23,2,i] <- 24
    values[23,3,i] <- length(mylogitresponse[,i])# q
    values[24,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]))
    values[24,2,i] <- 25
    values[24,3,i] <- length(mylogitresponse[,i])# q
    values[25,1,i] <- logLik(lm(mylogitresponse[,i]~  predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]))
    values[25,2,i] <- 26
    values[25,3,i] <- length(mylogitresponse[,i])# q
    values[26,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]))
    values[26,2,i] <- 27
    values[26,3,i] <- length(mylogitresponse[,i])# q
    values[27,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]))
    values[27,2,i] <- 28
    values[27,3,i] <- length(mylogitresponse[,i])# q
    values[28,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]))
    values[28,2,i] <- 29
    values[28,3,i] <- length(mylogitresponse[,i])# q
    values[29,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+  predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]))
    values[29,2,i] <- 30
    values[29,3,i] <- length(mylogitresponse[,i])# q
    values[30,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]))
    values[30,2,i] <- 31
    values[30,3,i] <- length(mylogitresponse[,i])
    values[31,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]))
    values[31,2,i] <- 32
    values[31,3,i] <- length(mylogitresponse[,i])
    values[32,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]))
    values[32,2,i] <- 33
    values[32,3,i] <- length(mylogitresponse[,i])
    values[33,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]))
    values[33,2,i] <- 34
    values[33,3,i] <- length(mylogitresponse[,i])
    values[34,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]+predictors[,34]))
    values[34,2,i] <- 35
    values[34,3,i] <- length(mylogitresponse[,i])
    values[35,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]+predictors[,34]+predictors[,35]))
    values[35,2,i] <- 36
    values[35,3,i] <- length(mylogitresponse[,i])
    values[36,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]+predictors[,34]+predictors[,35]+predictors[,36]))
    values[36,2,i] <- 37
    values[36,3,i] <- length(mylogitresponse[,i])
    values[37,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]+predictors[,34]+predictors[,35]+predictors[,36]+predictors[,37]))
    values[37,2,i] <- 38
    values[37,3,i] <- length(mylogitresponse[,i])
    values[38,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]+predictors[,34]+predictors[,35]+predictors[,36]+predictors[,37]+predictors[,38]))
    values[38,2,i] <- 39
    values[38,3,i] <- length(mylogitresponse[,i])
    values[39,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]+predictors[,34]+predictors[,35]+predictors[,36]+predictors[,37]+predictors[,38]+predictors[,39]))
    values[39,2,i] <- 40
    values[39,3,i] <- length(mylogitresponse[,i])
    
    values[40,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]+predictors[,34]+predictors[,35]+predictors[,36]+predictors[,37]+predictors[,38]+predictors[,39]+predictors[,40]))
    values[40,2,i] <- 41
    
    values[40,3,i] <- length(mylogitresponse[,i])}# q
  allvalues[,,,ii] <- values} #this should go at the end
# all values looks good-- want to start at the AIC calculation


# find the best model in "values":
AIseesee <- array(data=NA, c(3,dim(values)[1],2)) # this last dimension needs to be changed when I add in height. you're doing awesome, Al!! :)
for (iii in 1:2){ # again, needs to be changed to 6 when I get height in there!
  
  for (j in 1:3){
    for (ii in 1:dim(values)[1]){AIseesee[j, ii,iii] <- -2*allvalues[ii,1,j,iii]+(2* allvalues[ii,2,j,iii]* allvalues[ii,3,j,iii]/(allvalues[ii,3,j,iii]-allvalues[ii,2,j,iii]-1))} # generating AICc values for each model
    # getting the denominator
    minimumAIC <- min(AIseesee[j,,iii], na.rm=TRUE)
    tosum <- exp(-0.5*(AIseesee[j,,iii]-minimumAIC))
    AIseeseesum <- sum(tosum)
    AIseeseeweights <- exp(-0.5*(AIseesee[j,,iii]-minimumAIC))/AIseeseesum # calculating weights
    bestsindex[j,iii] <- which(AIseeseeweights==max(AIseeseeweights))# stores the index of the best supported model using AIC, but the indexing here is incorrect
    
  } 
  
} # stores the index of the best supported model using AIC
summary(AIseesee)


# now, bestsinsdex gives you a list of the best model for each of the treatments (rows)/ site-diameter combinations (columns)
# the following code fits proportional differences among treatments

par(mfrow=c(1,2))
plot(k1ALLAMCAlogit ~predictors[,1])
plot(k2aAMCAlogit ~predictors[,1])
plot(k4aAMCAlogit ~predictors[,1])
dev.off() 

predictors <- cbind(ugAMCAstoreCDFlogit[,4], ugAMCAstoreCDFlogit[,4]^2, ugAMCAstoreCDFlogit[,4]^3, ugAMCAstoreCDFlogit[,4]^4, ugAMCAstoreCDFlogit[,4]^5, ugAMCAstoreCDFlogit[,4]^6, ugAMCAstoreCDFlogit[,4]^7, ugAMCAstoreCDFlogit[,4]^8, ugAMCAstoreCDFlogit[,4]^9, ugAMCAstoreCDFlogit[,4]^10, ugAMCAstoreCDFlogit[,4]^11, ugAMCAstoreCDFlogit[,4]^12, ugAMCAstoreCDFlogit[,4]^13, ugAMCAstoreCDFlogit[,4]^14, ugAMCAstoreCDFlogit[,4]^15, ugAMCAstoreCDFlogit[,4]^16, ugAMCAstoreCDFlogit[,4]^17, ugAMCAstoreCDFlogit[,4]^18, ugAMCAstoreCDFlogit[,4]^19, ugAMCAstoreCDFlogit[,4]^20, ugAMCAstoreCDFlogit[,4]^21, ugAMCAstoreCDFlogit[,4]^22, ugAMCAstoreCDFlogit[,4]^23, ugAMCAstoreCDFlogit[,4]^24, ugAMCAstoreCDFlogit[,4]^25, ugAMCAstoreCDFlogit[,4]^26, ugAMCAstoreCDFlogit[,4]^27, ugAMCAstoreCDFlogit[,4]^28, ugAMCAstoreCDFlogit[,4] ^29, ugAMCAstoreCDFlogit[,4] ^30, ugAMCAstoreCDFlogit[,4] ^31, ugAMCAstoreCDFlogit[,4]^32, ugAMCAstoreCDFlogit[,4]^33, ugAMCAstoreCDFlogit[,4]^34, ugAMCAstoreCDFlogit[,4]^35, ugAMCAstoreCDFlogit[,4]^36, ugAMCAstoreCDFlogit[,4]^37, ugAMCAstoreCDFlogit[,4]^38, ugAMCAstoreCDFlogit[,4] ^39, ugAMCAstoreCDFlogit[,4] ^40)

k1ALLAMCAlogit <-predict(lm(ugAMCAstoreCDFlogit[,1]~predictors[,1]+ predictors[,2]+ predictors[,3]))#+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]))
plot(k1ALLAMCAlogit ~predictors[,1])
points(ugAMCAstoreCDFlogit[,1]~predictors[,1])

plot(ugAMCAstoreCDFlogit[,1]~predictors[,1])

k2aAMCAlogit <-predict(lm(ugAMCAstoreCDFlogit[,2]~predictors[,1]+ predictors[,2]+ predictors[,3]))#+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]))
plot(k2aAMCAlogit ~predictors[,1])
points(ugAMCAstoreCDFlogit[,2]~predictors[,1])

plot(ugAMCAstoreCDFlogit[,2]~predictors[,1])


k4aAMCAlogit <-predict(lm(ugAMCAstoreCDFlogit[,3]~predictors[,1]+ predictors[,2]+ predictors[,3]))#+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]))
plot(k4aAMCAlogit ~predictors[,1])
points(ugAMCAstoreCDFlogit[,3]~predictors[,1])

par(mfrow=c(1,3))
plot(N1AAMCA ~predictors[,1])
plot(N2AAMCA ~predictors[,1])
plot(N4DAMCAlogit ~predictors[,1])
dev.off() 


predictors <- cbind(gAMCAstoreCDFlogit[,4], gAMCAstoreCDFlogit[,4]^2, gAMCAstoreCDFlogit[,4]^3, gAMCAstoreCDFlogit[,4]^4, gAMCAstoreCDFlogit[,4]^5, gAMCAstoreCDFlogit[,4]^6, gAMCAstoreCDFlogit[,4]^7, gAMCAstoreCDFlogit[,4]^8, gAMCAstoreCDFlogit[,4]^9, gAMCAstoreCDFlogit[,4]^10, gAMCAstoreCDFlogit[,4]^11, gAMCAstoreCDFlogit[,4]^12, gAMCAstoreCDFlogit[,4]^13, gAMCAstoreCDFlogit[,4]^14, gAMCAstoreCDFlogit[,4]^15, gAMCAstoreCDFlogit[,4]^16, gAMCAstoreCDFlogit[,4]^17, gAMCAstoreCDFlogit[,4]^18, gAMCAstoreCDFlogit[,4]^19, gAMCAstoreCDFlogit[,4]^20, gAMCAstoreCDFlogit[,4]^21, gAMCAstoreCDFlogit[,4]^22, gAMCAstoreCDFlogit[,4]^23, gAMCAstoreCDFlogit[,4]^24)

N1AAMCA <-predict(lm(gAMCAstoreCDFlogit[,1]~predictors[,1]+ predictors[,2]+ predictors[,3]))#+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]))
plot(N1AAMCA ~predictors[,1])
points(gAMCAstoreCDFlogit[,1]~predictors[,1])


N2AAMCA <-predict(lm(gAMCAstoreCDFlogit[,2]~predictors[,1]+ predictors[,2]+ predictors[,3]))#+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]))
plot(N2AAMCA ~predictors[,1])
points(gAMCAstoreCDFlogit[,2]~predictors[,1])

N4DAMCAlogit <-predict(lm(gAMCAstoreCDFlogit[,3]~predictors[,1]+ predictors[,2]+ predictors[,3]))#+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]))
plot(N4DAMCAlogit ~predictors[,1])
points(gAMCAstoreCDFlogit[,3]~predictors[,1])


#######
#######
#*code working up until here
#*
#*


# transforming all of these into proportions and PDFs now!
library(gtools)
#gAMCApredictedCDF <-inv.logit(gAMCAstoreCDFlogit, 0.025,1.025)
#ugAMCApredictedCDF <-inv.logit(ugAMCAstoreCDFlogit, 0.025,1.025)

N1AAMCApredictedCDF <-inv.logit(N1AAMCA, 0.025,1.025)
N2AAMCApredictedCDF <-inv.logit(N2AAMCA, 0.025,1.025)
N4DAMCApredictedCDF <- inv.logit(N4DAMCAlogit, 0.025,1.025)

k1ALLAMCApredictedCDF <- inv.logit(k1ALLAMCAlogit, 0.025,1.025)
k2aAMCAApredictedCDF <- inv.logit(k2aAMCAlogit, 0.025,1.025)
k4aAMCApredictedCDF <- inv.logit(k4aAMCAlogit, 0.025,1.025)

# adding a zero to the beginning of the CDFs
#gAMCApredictedCDF <- c(0,gAMCApredictedCDF)
#ugAMCApredictedCDF <- c(0,ugAMCApredictedCDF)

N1AAMCApredictedCDF <- c(0,N1AAMCApredictedCDF)
N2AAMCApredictedCDF <- c(0,N2AAMCApredictedCDF)
N4DAMCApredictedCDF <- c(0,N4DAMCApredictedCDF)

k1ALLAMCApredictedCDF <- c(0,k1ALLAMCApredictedCDF)
k2aAMCApredictedCDF <- c(0,k2aAMCAApredictedCDF)
k4aAMCApredictedCDF <- c(0,k4aAMCApredictedCDF)

#gAMCApredictedPDF <- gAMCApredictedCDF[2:length(gAMCApredictedCDF)]-gAMCApredictedCDF[1:length(gAMCApredictedCDF)-1]
#ugAMCApredictedPDF <- ugAMCApredictedCDF[2:length(ugAMCApredictedCDF)]-ugAMCApredictedCDF[1:length(ugAMCApredictedCDF)-1]

N1AAMCApredictedPDF <- N1AAMCApredictedCDF[2:length(N1AAMCApredictedCDF)]-N1AAMCApredictedCDF[1:length(N1AAMCApredictedCDF)-1]
N2AAMCApredictedPDF <- N2AAMCApredictedCDF[2:length(N2AAMCApredictedCDF)]-N2AAMCApredictedCDF[1:length(N2AAMCApredictedCDF)-1]
N4DAMCApredictedPDF <- N4DAMCApredictedCDF[2:length(N4DAMCApredictedCDF)]-N4DAMCApredictedCDF[1:length(N4DAMCApredictedCDF)-1]

k1ALLAMCApredictedPDF <- k1ALLAMCApredictedCDF[2:length(k1ALLAMCApredictedCDF)]-k1ALLAMCApredictedCDF[1:length(k1ALLAMCApredictedCDF)-1]
k2aAMCApredictedPDF <- k2aAMCApredictedCDF[2:length(k2aAMCApredictedCDF)]-k2aAMCApredictedCDF[1:length(k2aAMCApredictedCDF)-1]
k4aAMCApredictedPDF <- k4aAMCApredictedCDF[2:length(k4aAMCApredictedCDF)]-k4aAMCApredictedCDF[1:length(k4aAMCApredictedCDF)-1]





# making lines now for all the exclosure treatments
AMCAmidpoints <- c(0, AMCAbrkpnts[ 1:(length(AMCAbrkpnts)-1)]+ (AMCAbrkpnts[2:length(AMCAbrkpnts)]- AMCAbrkpnts[1:(length(AMCAbrkpnts)-1)])/2)

plot(exp(k1ALLAMCApredictedPDF-k2aAMCApredictedPDF)~ AMCAmidpoints,bty="l", type="l", col="tan1", lwd=3, xlab="basal area", ylab= 
       "difference between North exclosure and control PDFs", yaxt="n")
points(exp(k1ALLAMCApredictedPDF-k4aAMCApredictedPDF)~ AMCAmidpoints, bty="l",col="red4",type="l",lwd=3, pch=5)
points(exp(k2aAMCApredictedPDF-k4aAMCApredictedPDF)~ AMCAmidpoints, bty="l", type="l", col="royalblue2", lwd=3)
axis(side=2, at= seq(0.80,1.25, length.out=6),lab=round(log(seq(0.80,1.05, length.out=6)),4))



AMCAmidpoints <- c(0, AMCAbrkpnts[ 1:(length(AMCAbrkpnts)-1)]+ (AMCAbrkpnts[2:length(AMCAbrkpnts)]- AMCAbrkpnts[1:(length(AMCAbrkpnts)-1)])/2)

plot(exp(k1ALLAMCApredictedPDF-N2AAMCApredictedPDF)~ AMCAmidpoints,bty="l", type="l", col="tan1", lwd=3, xlab="basal area", ylab= 
       "difference between North exclosure and control PDFs", yaxt="n")
points(exp(k1ALLAMCApredictedPDF-k4aAMCApredictedPDF)~ AMCAmidpoints, bty="l",col="red4",type="l",lwd=3, pch=5)
points(exp(k2aAMCApredictedPDF-k4aAMCApredictedPDF)~ AMCAmidpoints, bty="l", type="l", col="royalblue2", lwd=3)
axis(side=2, at= seq(0.80,1.25, length.out=6),lab=round(log(seq(0.80,1.05, length.out=6)),4))


#max(k1ALLAMCApredictedPDF-k4aAMCApredictedPDF)
#min(k1ALLAMCApredictedPDF-k4aAMCApredictedPDF)  ylim=-0.6, 0.00758

#AMCA
#grazed verses ungrazed

AMCAmidpoints <- c(0, AMCAbrkpnts[ 1:(length(AMCAbrkpnts)-1)]+ (AMCAbrkpnts[2:length(AMCAbrkpnts)]- AMCAbrkpnts[1:(length(AMCAbrkpnts)-1)])/2)

plot(exp(k2aAMCApredictedPDF-N2AAMCApredictedPDF)~ AMCAmidpoints,bty="l", type="l", col="tan1", lwd=3, xlab="Biomass", ylab= 
       "Difference between Grazed and Ungrazed Watershed", yaxt="n")
points(exp(k1ALLAMCApredictedPDF-N1AAMCApredictedPDF)~ AMCAmidpoints, bty="l",col="red4",type="l",lwd=3, pch=5)
points(exp(k4aAMCApredictedPDF-N4DAMCApredictedPDF)~ AMCAmidpoints, bty="l", type="l", col="royalblue2", lwd=3)
axis(side=2, at= seq(0.80,1.26, length.out=6),lab=round(log(seq(0.80,1.25, length.out=6)),4))

legend(250,1.0,c("2yr FRI","1yr FRI"),fill=c("tan1","red4", "royalblue2"), bty="n")

#ASOB
#ECAN
for (ii in 1:2){# will be 1:6 when I get height in there
  
  # generating squared, cubed, etc. values of diameter
  mypredictorsstart <- mypredictorsstartlist[[ii]] # setting the predictors either at North, Central, or South
  mylogitresponse <- myresponsestartlist[[ii]] # setting the responses either at North, Central, or South
  predictors <- cbind(mypredictorsstart, mypredictorsstart^2, mypredictorsstart^3, mypredictorsstart^4, mypredictorsstart^5, mypredictorsstart^6, mypredictorsstart^7, mypredictorsstart^8, mypredictorsstart^9, mypredictorsstart^10, mypredictorsstart^11, mypredictorsstart^12, mypredictorsstart^13, mypredictorsstart^14, mypredictorsstart^15, mypredictorsstart^16, mypredictorsstart^17, mypredictorsstart^18, mypredictorsstart^19, mypredictorsstart^20, mypredictorsstart^21, mypredictorsstart^22, mypredictorsstart^23, mypredictorsstart^24, mypredictorsstart^25, mypredictorsstart^26, mypredictorsstart^27, mypredictorsstart^28, mypredictorsstart ^29, mypredictorsstart ^30, mypredictorsstart ^31, mypredictorsstart ^32, mypredictorsstart^33, mypredictorsstart^34, mypredictorsstart^35, mypredictorsstart^36, mypredictorsstart^37, mypredictorsstart^38, mypredictorsstart ^39, mypredictorsstart ^40)
  
  
  values <- array(NA,c(40,3,3)) # the first dimension is the models of differing orders. the second dimension is the info about each of those models (LL, p, q), and the third dimension here is the different treatments within a site
  for (i in 1:3){# I'm going to assume that the logLik() function gives you the max loglike (as in Doak and Morris)
    
    # here, the rows are the different models, with the columns corresponding to Loglik, p, and q, respectively
    values[1,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1])) # log likelihood
    # need to bring down the i here to make sure all have this same reference in them. 
    values[1,2,i] <- 2# p
    values[1,3,i] <- length(mylogitresponse[,i])# q
    values[2,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]))
    values[2,2,i] <- 3# p
    values[2,3,i] <- length(mylogitresponse[,i])# q
    values[3,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]))
    values[3,2,i] <- 4# p
    values[3,3,i] <- length(mylogitresponse[,i])# q
    values[4,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]))
    values[4,2,i] <- 5# p
    values[4,3,i] <- length(mylogitresponse[,i])# q
    values[5,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]))
    values[5,2,i] <- 6# p
    values[5,3,i] <- length(mylogitresponse[,i])# q
    values[6,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]))
    values[6,2,i] <- 7# p
    values[6,3,i] <- length(mylogitresponse[,i])# q
    values[7,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]))
    values[7,2,i] <- 8
    values[7,3,i] <- length(mylogitresponse[,i])# q
    values[8,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]))
    values[8,2,i] <- 9
    values[8,3,i] <- length(mylogitresponse[,i])# q
    values[9,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]))
    values[9,2,i] <- 10
    values[9,3,i] <- length(mylogitresponse[,i])# q
    values[10,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]))
    values[10,2,i] <- 11
    values[10,3,i] <- length(mylogitresponse[,i])# q
    values[11,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]))
    values[11,2,i] <- 12
    values[11,3,i] <- length(mylogitresponse[,i])# q
    values[12,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]))
    values[12,2,i] <- 13
    values[12,3,i] <- length(mylogitresponse[,i])# q
    values[13,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]))
    values[13,2,i] <- 14
    values[13,3,i] <- length(mylogitresponse[,i])# q
    values[14,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]))
    values[14,2,i] <- 15
    values[14,3,i] <- length(mylogitresponse[,i])# q
    values[15,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]))
    values[15,2,i] <- 16
    values[15,3,i] <- length(mylogitresponse[,i])# q
    values[16,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]))
    values[16,2,i] <- 17
    values[16,3,i] <- length(mylogitresponse[,i])# q
    values[17,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]))
    values[17,2,i] <- 18
    values[17,3,i] <- length(mylogitresponse[,i])# q
    values[18,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]))
    values[18,2,i] <- 19
    values[18,3,i] <- length(mylogitresponse[,i])# q
    values[19,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]))
    values[19,2,i] <- 20
    values[19,3,i] <- length(mylogitresponse[,i])# q
    values[20,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]))
    values[20,2,i] <- 21
    values[20,3,i] <- length(mylogitresponse[,i])# q
    values[21,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]))
    values[21,2,i] <- 22
    values[21,3,i] <- length(mylogitresponse[,i])# q
    values[22,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]))
    values[22,2,i] <- 23
    values[22,3,i] <- length(mylogitresponse[,i])# q
    values[23,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]))
    values[23,2,i] <- 24
    values[23,3,i] <- length(mylogitresponse[,i])# q
    values[24,1,i] <- logLik(lm(mylogitresponse[,i]~ predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]))
    values[24,2,i] <- 25
    values[24,3,i] <- length(mylogitresponse[,i])# q
    values[25,1,i] <- logLik(lm(mylogitresponse[,i]~  predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]))
    values[25,2,i] <- 26
    values[25,3,i] <- length(mylogitresponse[,i])# q
    values[26,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]))
    values[26,2,i] <- 27
    values[26,3,i] <- length(mylogitresponse[,i])# q
    values[27,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]))
    values[27,2,i] <- 28
    values[27,3,i] <- length(mylogitresponse[,i])# q
    values[28,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]))
    values[28,2,i] <- 29
    values[28,3,i] <- length(mylogitresponse[,i])# q
    values[29,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+  predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]))
    values[29,2,i] <- 30
    values[29,3,i] <- length(mylogitresponse[,i])# q
    values[30,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]))
    values[30,2,i] <- 31
    values[30,3,i] <- length(mylogitresponse[,i])
    values[31,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]))
    values[31,2,i] <- 32
    values[31,3,i] <- length(mylogitresponse[,i])
    values[32,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]))
    values[32,2,i] <- 33
    values[32,3,i] <- length(mylogitresponse[,i])
    values[33,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]))
    values[33,2,i] <- 34
    values[33,3,i] <- length(mylogitresponse[,i])
    values[34,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]+predictors[,34]))
    values[34,2,i] <- 35
    values[34,3,i] <- length(mylogitresponse[,i])
    values[35,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]+predictors[,34]+predictors[,35]))
    values[35,2,i] <- 36
    values[35,3,i] <- length(mylogitresponse[,i])
    values[36,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]+predictors[,34]+predictors[,35]+predictors[,36]))
    values[36,2,i] <- 37
    values[36,3,i] <- length(mylogitresponse[,i])
    values[37,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]+predictors[,34]+predictors[,35]+predictors[,36]+predictors[,37]))
    values[37,2,i] <- 38
    values[37,3,i] <- length(mylogitresponse[,i])
    values[38,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]+predictors[,34]+predictors[,35]+predictors[,36]+predictors[,37]+predictors[,38]))
    values[38,2,i] <- 39
    values[38,3,i] <- length(mylogitresponse[,i])
    values[39,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]+predictors[,34]+predictors[,35]+predictors[,36]+predictors[,37]+predictors[,38]+predictors[,39]))
    values[39,2,i] <- 40
    values[39,3,i] <- length(mylogitresponse[,i])
    
    values[40,1,i] <- logLik(lm(mylogitresponse[,i]~predictors[,1]+ predictors[,2]+ predictors[,3]+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+ predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]+ predictors[,17]+ predictors[,18]+ predictors[,19]+ predictors[,20]+ predictors[,21]+ predictors[,22]+ predictors[,23]+ predictors[,24]+ predictors[,25]+predictors[,26]+predictors[,27]+predictors[,28]+predictors[,29]+predictors[,30]+predictors[,31]+predictors[,32]+predictors[,33]+predictors[,34]+predictors[,35]+predictors[,36]+predictors[,37]+predictors[,38]+predictors[,39]+predictors[,40]))
    values[40,2,i] <- 41
    
    values[40,3,i] <- length(mylogitresponse[,i])}# q
  allvalues[,,,ii] <- values} #this should go at the end
# all values looks good-- want to start at the AIC calculation


# find the best model in "values":
AIseesee <- array(data=NA, c(3,dim(values)[1],2)) # this last dimension needs to be changed when I add in height. you're doing awesome, Al!! :)
for (iii in 1:2){ # again, needs to be changed to 6 when I get height in there!
  
  for (j in 1:3){
    for (ii in 1:dim(values)[1]){AIseesee[j, ii,iii] <- -2*allvalues[ii,1,j,iii]+(2* allvalues[ii,2,j,iii]* allvalues[ii,3,j,iii]/(allvalues[ii,3,j,iii]-allvalues[ii,2,j,iii]-1))} # generating AICc values for each model
    # getting the denominator
    minimumAIC <- min(AIseesee[j,,iii], na.rm=TRUE)
    tosum <- exp(-0.5*(AIseesee[j,,iii]-minimumAIC))
    AIseeseesum <- sum(tosum)
    AIseeseeweights <- exp(-0.5*(AIseesee[j,,iii]-minimumAIC))/AIseeseesum # calculating weights
    bestsindex[j,iii] <- which(AIseeseeweights==max(AIseeseeweights))# stores the index of the best supported model using AIC, but the indexing here is incorrect
    
  } 
  
} # stores the index of the best supported model using AIC
summary(AIseesee)
#
# now, bestsinsdex gives you a list of the best model for each of the treatments (rows)/ site-diameter combinations (columns)
# the following code fits proportional differences among treatments

predictors <- cbind(ugECANstoreCDFlogit[,4], ugECANstoreCDFlogit[,4]^2, ugECANstoreCDFlogit[,4]^3, ugECANstoreCDFlogit[,4]^4, ugECANstoreCDFlogit[,4]^5, ugECANstoreCDFlogit[,4]^6, ugECANstoreCDFlogit[,4]^7, ugECANstoreCDFlogit[,4]^8, ugECANstoreCDFlogit[,4]^9, ugECANstoreCDFlogit[,4]^10, ugECANstoreCDFlogit[,4]^11, ugECANstoreCDFlogit[,4]^12, ugECANstoreCDFlogit[,4]^13, ugECANstoreCDFlogit[,4]^14, ugECANstoreCDFlogit[,4]^15, ugECANstoreCDFlogit[,4]^16, ugECANstoreCDFlogit[,4]^17, ugECANstoreCDFlogit[,4]^18, ugECANstoreCDFlogit[,4]^19, ugECANstoreCDFlogit[,4]^20, ugECANstoreCDFlogit[,4]^21, ugECANstoreCDFlogit[,4]^22, ugECANstoreCDFlogit[,4]^23, ugECANstoreCDFlogit[,4]^24, ugECANstoreCDFlogit[,4]^25, ugECANstoreCDFlogit[,4]^26, ugECANstoreCDFlogit[,4]^27, ugECANstoreCDFlogit[,4]^28, ugECANstoreCDFlogit[,4] ^29, ugECANstoreCDFlogit[,4] ^30, ugECANstoreCDFlogit[,4] ^31, ugECANstoreCDFlogit[,4]^32, ugECANstoreCDFlogit[,4]^33, ugECANstoreCDFlogit[,4]^34, ugECANstoreCDFlogit[,4]^35, ugECANstoreCDFlogit[,4]^36, ugECANstoreCDFlogit[,4]^37, ugECANstoreCDFlogit[,4]^38, ugECANstoreCDFlogit[,4] ^39, ugECANstoreCDFlogit[,4] ^40)

K1AeECANlogit <-predict(lm(ugECANstoreCDFlogit[,1]~predictors[,1]+ predictors[,2]+ predictors[,3]))#+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]))
plot(K1AeECANlogit ~predictors[,1])
points(ugECANstoreCDFlogit[,1]~predictors[,1])

K2AeECANlogit <-predict(lm(ugECANstoreCDFlogit[,2]~predictors[,1]+ predictors[,2]+ predictors[,3]))#+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]))
plot(K2AeECANlogit ~predictors[,1])
points(ugECANstoreCDFlogit[,2]~predictors[,1])


K4AeECANlogit <-predict(lm(ugECANstoreCDFlogit[,3]~predictors[,1]+ predictors[,2]+ predictors[,3]))#+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]+ predictors[,12]+ predictors[,13]+ predictors[,14]+ predictors[,15]+ predictors[,16]))
plot(K4AeECANlogit ~predictors[,1])
points(ugECANstoreCDFlogit[,3]~predictors[,1])



predictors <- cbind(gECANstoreCDFlogit[,4], gECANstoreCDFlogit[,4]^2, gECANstoreCDFlogit[,4]^3, gECANstoreCDFlogit[,4]^4, gECANstoreCDFlogit[,4]^5, gECANstoreCDFlogit[,4]^6, gECANstoreCDFlogit[,4]^7, gECANstoreCDFlogit[,4]^8, gECANstoreCDFlogit[,4]^9, gECANstoreCDFlogit[,4]^10, gECANstoreCDFlogit[,4]^11, gECANstoreCDFlogit[,4]^12, gECANstoreCDFlogit[,4]^13, gECANstoreCDFlogit[,4]^14, gECANstoreCDFlogit[,4]^15, gECANstoreCDFlogit[,4]^16, gECANstoreCDFlogit[,4]^17, gECANstoreCDFlogit[,4]^18, gECANstoreCDFlogit[,4]^19, gECANstoreCDFlogit[,4]^20, gECANstoreCDFlogit[,4]^21, gECANstoreCDFlogit[,4]^22, gECANstoreCDFlogit[,4]^23, gECANstoreCDFlogit[,4]^24)

N1AeECAN <-predict(lm(gECANstoreCDFlogit[,1]~predictors[,1]+ predictors[,2]+ predictors[,3]))#+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]))
plot(N1AeECAN ~predictors[,1])
points(gECANstoreCDFlogit[,1]~predictors[,1])


N2AeECAN <-predict(lm(gECANstoreCDFlogit[,2]~predictors[,1]+ predictors[,2]+ predictors[,3]))#+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]))
plot(N2AeECAN ~predictors[,1])
points(gECANstoreCDFlogit[,2]~predictors[,1])

N4DeECANlogit <-predict(lm(gECANstoreCDFlogit[,3]~predictors[,1]+ predictors[,2]+ predictors[,3]))#+ predictors[,4]+ predictors[,5]+ predictors[,6]+ predictors[,7]+predictors[,8]+ predictors[,9]+ predictors[,10]+ predictors[,11]))
plot(N4DeECANlogit ~predictors[,1])
points(gECANstoreCDFlogit[,3]~predictors[,1])


#######
#######
#*code working up until here
#*
#*


# transforming all of these into proportions and PDFs now!
library(gtools)
#gECANpredictedCDF <-inv.logit(gECANstoreCDFlogit, 0.025,1.025)
#ugECANpredictedCDF <-inv.logit(ugECANstoreCDFlogit, 0.025,1.025)

N1AeECANpredictedCDF <-inv.logit(N1AeECAN, 0.025,1.025)
N2AeECANpredictedCDF <-inv.logit(N2AeECAN, 0.025,1.025)
N4DeECANpredictedCDF <- inv.logit(N4DeECANlogit, 0.025,1.025)

K1AeECANpredictedCDF <- inv.logit(K1AeECANlogit, 0.025,1.025)
K2AeECANApredictedCDF <- inv.logit(K2AeECANlogit, 0.025,1.025)
K4AeECANpredictedCDF <- inv.logit(K4AeECANlogit, 0.025,1.025)

# adding a zero to the beginning of the CDFs
#gECANpredictedCDF <- c(0,gECANpredictedCDF)
#ugECANpredictedCDF <- c(0,ugECANpredictedCDF)

N1AeECANpredictedCDF <- c(0,N1AeECANpredictedCDF)
N2AeECANpredictedCDF <- c(0,N2AeECANpredictedCDF)
N4DeECANpredictedCDF <- c(0,N4DeECANpredictedCDF)

K1AeECANpredictedCDF <- c(0,K1AeECANpredictedCDF)
K2AeECANpredictedCDF <- c(0,K2AeECANApredictedCDF)
K4AeECANpredictedCDF <- c(0,K4AeECANpredictedCDF)

#gECANpredictedPDF <- gECANpredictedCDF[2:length(gECANpredictedCDF)]-gECANpredictedCDF[1:length(gECANpredictedCDF)-1]
#ugECANpredictedPDF <- ugECANpredictedCDF[2:length(ugECANpredictedCDF)]-ugECANpredictedCDF[1:length(ugECANpredictedCDF)-1]

N1AeECANpredictedPDF <- N1AeECANpredictedCDF[2:length(N1AeECANpredictedCDF)]-N1AeECANpredictedCDF[1:length(N1AeECANpredictedCDF)-1]
N2AeECANpredictedPDF <- N2AeECANpredictedCDF[2:length(N2AeECANpredictedCDF)]-N2AeECANpredictedCDF[1:length(N2AeECANpredictedCDF)-1]
N4DeECANpredictedPDF <- N4DeECANpredictedCDF[2:length(N4DeECANpredictedCDF)]-N4DeECANpredictedCDF[1:length(N4DeECANpredictedCDF)-1]

K1AeECANpredictedPDF <- K1AeECANpredictedCDF[2:length(K1AeECANpredictedCDF)]-K1AeECANpredictedCDF[1:length(K1AeECANpredictedCDF)-1]
K2AeECANpredictedPDF <- K2AeECANpredictedCDF[2:length(K2AeECANpredictedCDF)]-K2AeECANpredictedCDF[1:length(K2AeECANpredictedCDF)-1]
K4AeECANpredictedPDF <- K4AeECANpredictedCDF[2:length(K4AeECANpredictedCDF)]-K4AeECANpredictedCDF[1:length(K4AeECANpredictedCDF)-1]





# making lines now for all the exclosure treatments
ECANmidpoints <- c(0, ECANbrkpnts[ 1:(length(ECANbrkpnts)-1)]+ (ECANbrkpnts[2:length(ECANbrkpnts)]- ECANbrkpnts[1:(length(ECANbrkpnts)-1)])/2)

plot(exp(K1AeECANpredictedPDF-K2AeECANpredictedPDF)~ ECANmidpoints,bty="l", type="l", col="tan1", lwd=3, xlab="basal area", ylab= 
       "difference between North exclosure and control PDFs", yaxt="n")
points(exp(K1AeECANpredictedPDF-K4AeECANpredictedPDF)~ ECANmidpoints, bty="l",col="red4",type="l",lwd=3, pch=5)
points(exp(K2AeECANpredictedPDF-K4AeECANpredictedPDF)~ ECANmidpoints, bty="l", type="l", col="royalblue2", lwd=3)
axis(side=2, at= seq(0.80,1.25, length.out=6),lab=round(log(seq(0.80,1.05, length.out=6)),4))



ECANmidpoints <- c(0, ECANbrkpnts[ 1:(length(ECANbrkpnts)-1)]+ (ECANbrkpnts[2:length(ECANbrkpnts)]- ECANbrkpnts[1:(length(ECANbrkpnts)-1)])/2)

plot(exp(K1AeECANpredictedPDF-N2AeECANpredictedPDF)~ ECANmidpoints,bty="l", type="l", col="tan1", lwd=3, xlab="basal area", ylab= 
       "difference between North exclosure and control PDFs", yaxt="n")
points(exp(K1AeECANpredictedPDF-K4AeECANpredictedPDF)~ ECANmidpoints, bty="l",col="red4",type="l",lwd=3, pch=5)
points(exp(K2AeECANpredictedPDF-K4AeECANpredictedPDF)~ ECANmidpoints, bty="l", type="l", col="royalblue2", lwd=3)
axis(side=2, at= seq(0.80,1.25, length.out=6),lab=round(log(seq(0.80,1.05, length.out=6)),4))


#max(K1AeECANpredictedPDF-K4AeECANpredictedPDF)
#min(K1AeECANpredictedPDF-K4AeECANpredictedPDF)  ylim=-0.6, 0.00758

#ECAN
#grazed verses ungrazed

ECANmidpoints <- c(0, ECANbrkpnts[ 1:(length(ECANbrkpnts)-1)]+ (ECANbrkpnts[2:length(ECANbrkpnts)]- ECANbrkpnts[1:(length(ECANbrkpnts)-1)])/2)

plot(exp(K1AeECANpredictedPDF-N1AeECANpredictedPDF)~ ECANmidpoints,bty="l", type="l", col="tan1", lwd=3, xlab="Biomass", ylab= 
       "Difference between grazed and ungrazed PDFs", yaxt="n")
points(exp(K2AeECANpredictedPDF-N2AeECANpredictedPDF)~ ECANmidpoints, bty="l",col="red4",type="l",lwd=3, pch=5)
points(exp(K4AeECANpredictedPDF-N4DeECANpredictedPDF)~ ECANmidpoints, bty="l", type="l", col="royalblue2", lwd=3)
axis(side=2, at= seq(0.80,1.25, length.out=6),lab=round(log(seq(0.80,1.05, length.out=6)),4))

legend(1500,1.20,c("1yr FRI","2yr FRI", "4yr FRI"),fill=c("tan1","red4", "royalblue2"), bty="n")

















AMCAmidpoints <- c(0, AMCAbrkpnts[ 1:(length(AMCAbrkpnts)-1)]+ (AMCAbrkpnts[2:length(AMCAbrkpnts)]- AMCAbrkpnts[1:(length(AMCAbrkpnts)-1)])/2)

plot(exp(k2aAMCApredictedPDF-N2AAMCApredictedPDF)~ AMCAmidpoints,bty="l", type="l", col="tan1", lwd=3, xlab="Biomass", ylab= 
       "Difference between Grazed and Ungrazed Watershed", yaxt="n")
points(exp(K2AeECANpredictedPDF-N2AeECANpredictedPDF)~ ECANmidpoints, bty="l",col="tan1",type="l",lwd=3, pch=3)
points(exp(k1ALLAMCApredictedPDF-N1AAMCApredictedPDF)~ AMCAmidpoints, bty="l",col="red4",type="l",lwd=3, pch=3)
points(exp(K1AeECANpredictedPDF-N1AeECANpredictedPDF)~ ECANmidpoints, bty="l",col="red4",type="l",lwd=3, pch=3)



plot(exp(k2aAMCApredictedPDF-N2AAMCApredictedPDF)~ AMCAmidpoints,bty="l", type="l", col="light blue", lwd=3, pch=1, xlab="Biomass", ylab= 
       "Difference between Grazed and Ungrazed Watershed", yaxt="n")
points(exp(K2AeECANpredictedPDF-N2AeECANpredictedPDF)~ ECANmidpoints, bty="l",col="red4",type="l",lwd=3, pch=3)
points(exp(k1ALLAMCApredictedPDF-N1AAMCApredictedPDF)~ AMCAmidpoints, bty="l",col="blue",type="l",lwd=3, pch=1)
points(exp(K1AeECANpredictedPDF-N1AeECANpredictedPDF)~ ECANmidpoints, bty="l",col="red2",type="l",lwd=3, pch=3)


axis(side=2, at= seq(0.80,1.25, length.out=6),lab=round(log(seq(0.80,1.05, length.out=6)),4))
axis(side=2, at= seq(0.80,1.26, length.out=6),lab=round(log(seq(0.80,1.25, length.out=6)),4))


legend(250,.99,c("AMCA 1yr FRI","AMCA 2yr FRI", "ECAN 1yr FRI","ECAN 2yr FRI"),fill=c(" blue", "light blue","red4", "red2"), bty="n")



plot(exp(k2aAMCApredictedPDF-N2AAMCApredictedPDF)~ AMCAmidpoints,bty="l", type="l", col="blue", lwd=3, pch=1, xlab="Biomass", ylab= 
       "Difference between Grazed and Ungrazed Watershed", yaxt="n")
points(exp(K2AeECANpredictedPDF-N2AeECANpredictedPDF)~ ECANmidpoints, bty="l",col="red3",type="l",lwd=3, pch=3)
axis(side=2, at= seq(0.80,1.25, length.out=6),lab=round(log(seq(0.80,1.05, length.out=6)),4))

legend(250,.99,c( "AMCA 2yr FRI","ECAN 2yr FRI"),fill=c("royal blue","red3"), bty="n")



plot(exp(k1ALLAMCApredictedPDF-N1AAMCApredictedPDF)~ AMCAmidpoints,bty="l", type="l", col="royal blue", lwd=3, pch=1, xlab="Biomass", ylab= 
       "Difference between Grazed and Ungrazed Watershed", yaxt="n")
points(exp(K1AeECANpredictedPDF-N1AeECANpredictedPDF)~ ECANmidpoints, bty="l",col="red4",type="l",lwd=3, pch=3)
axis(side=2, at= seq(0.80,1.25, length.out=6),lab=round(log(seq(0.80,1.05, length.out=6)),4))


legend(250,.99,c("AMCA 1yr FRI","ECAN 1yr FRI"),fill=c("royal blue","red4"), bty="n")

plot(exp(K1AeECANpredictedPDF-N1AeECANpredictedPDF)~ AMCAmidpoints,bty="l", type="l", col="royal blue", lwd=3, pch=1, xlab="Biomass", ylab= 
       "Difference between Grazed and Ungrazed Watershed", yaxt="n")
points(exp(k1ALLAMCApredictedPDF-N1AAMCApredictedPDF)~ ECANmidpoints, bty="l",col="red4",type="l",lwd=3, pch=3)
axis(side=2, at= seq(0.80,1.25, length.out=6),lab=round(log(seq(0.80,1.05, length.out=6)),4))


legend(250,.99,c("AMCA 1yr FRI","ECAN 1yr FRI"),fill=c("royal blue","red4"), bty="n")





ECANmidpoints <- c(0, ECANbrkpnts[ 1:(length(ECANbrkpnts)-1)]+ (ECANbrkpnts[2:length(ECANbrkpnts)]- ECANbrkpnts[1:(length(ECANbrkpnts)-1)])/2)

plot(exp(K1AeECANpredictedPDF-N1AeECANpredictedPDF)~ ECANmidpoints,bty="l", type="l", col="tan1", lwd=3, xlab="Biomass", ylab= 
       "Difference between grazed and ungrazed PDFs", yaxt="n")
points(exp(K2AeECANpredictedPDF-N2AeECANpredictedPDF)~ ECANmidpoints, bty="l",col="red4",type="l",lwd=3, pch=5)
points(exp(K4AeECANpredictedPDF-N4DeECANpredictedPDF)~ ECANmidpoints, bty="l", type="l", col="royalblue2", lwd=3)
axis(side=2, at= seq(0.80,1.25, length.out=6),lab=round(log(seq(0.80,1.05, length.out=6)),4))


plot(exp((K1AeECANpredictedPDF-N1AeECANpredictedPDF)-(K2AeECANpredictedPDF-N2AeECANpredictedPDF))~ ECANmidpoints,bty="l", type="l", col="tan1", lwd=3, xlab="Biomass", ylab= 
       "Difference between grazed and ungrazed PDFs", yaxt="n")
points(exp((k1ALLAMCApredictedPDF-N1AAMCApredictedPDF)-(k2aAMCApredictedPDF-N2AAMCApredictedPDF))~ ECANmidpoints, bty="l",col="red4",type="l",lwd=3, pch=5)


plot(exp(k2aAMCApredictedPDF-N2AAMCApredictedPDF)~ AMCAmidpoints,bty="l", type="l", col="light blue", lwd=3, pch=1, xlab="Biomass", ylab= 
       "Difference between Grazed and Ungrazed Watershed", yaxt="n")
points(exp(k1ALLAMCApredictedPDF-N1AAMCApredictedPDF)~ AMCAmidpoints, bty="l",col="blue",type="l",lwd=3, pch=1)
axis(side=2, at= seq(0.04,1.59, length.out=6),lab=round(log(seq(0.40,1.05, length.out=6)),4))
legend(250,.991,c("1yr FRI","2yr FRI"),fill=c("blue","light blue"), bty="n")

plot(exp(k1ALLAMCApredictedPDF-N1AAMCApredictedPDF)~ AMCAmidpoints,bty="l", type="l", col="blue", lwd=3, pch=1, xlab="Biomass", ylab= 
       "Difference between Grazed and Ungrazed Watershed", yaxt="n")

plot(exp(K1AeECANpredictedPDF-N1AeECANpredictedPDF)~ AMCAmidpoints,bty="l", type="l", col="red2", lwd=3, pch=1, xlab="Biomass", ylab= 
       "Difference between Grazed and Ungrazed Watershed", yaxt="n")

points(exp(K2AeECANpredictedPDF-N2AeECANpredictedPDF)~ ECANmidpoints, bty="l",col="red4",type="l",lwd=3, pch=3)
axis(side=2, at= seq(0.80,1.59, length.out=6),lab=round(log(seq(0.80,1.05, length.out=6)),4))
legend(250,1.59,c("1yr FRI","2yr FRI"),fill=c("red2","red4"), bty="n")

axis(side=2, at= seq(0.80,1.26, length.out=6),lab=round(log(seq(0.80,1.25, length.out=6)),4))

plot(exp(K1AeECANpredictedPDF-N1AeECANpredictedPDF)~ AMCAmidpoints,bty="l", type="l", col="red4", lwd=3, pch=1, xlab="Biomass", ylab="Difference between Grazed and Ungrazed Watershed", yaxt="n")



plot(exp(K2AeECANpredictedPDF-N2AeECANpredictedPDF)~ AMCAmidpoints,bty="l", type="l", col="red2", lwd=3, pch=1, xlab="Biomass", ylab= 
       "Difference between Grazed and Ungrazed Watershed", yaxt="n")
points(exp(K1AeECANpredictedPDF-N1AeECANpredictedPDF)~ ECANmidpoints, bty="l",col="red4",type="l",lwd=3, pch=3,ylim= c(-.22,.04888))

axis(2, seq(-.22,.05,10))

ggplot(df, aes(x, y, other aesthetics))




plot(exp(K1AeECANpredictedPDF)~ ECANmidpoints,bty="l", type="l", col="tan1", lwd=3, xlab="Biomass", ylab= 
       "K1A and N1A PDFs", yaxt="n", xlim=c(0,500))
points(exp(N1AeECANpredictedPDF)~ ECANmidpoints, bty="l",col="red",type="l",lwd=3, pch=3)

plot(exp(K2AeECANpredictedPDF)~ ECANmidpoints,bty="l", type="l", col="tan1", lwd=3, xlab="Biomass", ylab= 
       "K2A and N2A PDFs", yaxt="n", xlim=c(0,500))
points(exp(N2AeECANpredictedPDF)~ ECANmidpoints, bty="l",col="red",type="l",lwd=3, pch=3)