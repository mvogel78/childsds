##################################################################################################################
#########  WHO 2007                                                                                          #####
#########  Department of Nutrition for Health and Development                                                #####
#########  World Health Organization                                                                         #####
#########  Last modified on 08/10/2013     -     Developed using R version 3.0.1 (2013-05-16)                #####
#########  This code corcerns the  the calculation of prevalences using all vallid z-scores (non-missing)    #####
#########  for three indicators: weight-for-age (5 to 10 years), height-for-age (5 to 19 years) and BMI-for- #####
#########  age (5 to 19 years) based on the WHO 2007 references.                                             #####
#########  Exact age must be given in months (no rounding necessary), height in centimeters and weight in    #####
#########  kilograms.                                                                                        #####
##################################################################################################################
##################################################################################################################

##################################################################################################################
#########  Functions for calculating the z-scores and prevalences for a nutritional survey                   #####
##################################################################################################################


########################################################################
#### Auxiliar functions
########################################################################

#############################################################################
##### Prevalence calculation for the upper bound and corresponding 95% C.I.
#############################################################################

prevph.L <- function(a,x,w) { 
   ph <- sum((x > a)*w,na.rm=T)/sum((!is.na(x))*w,na.rm=T)
   aux <- 1.96*sqrt(ph*(1-ph)/sum((!is.na(x))*w,na.rm=T))+(1/(2*sum((!is.na(x))*w,na.rm=T)))
   vec <- c(rounde(sum((!is.na(x))*w,na.rm=T),digits=0),rounde(c(ph, max(0,ph-aux), min(1,ph+aux))*100,digits=1))
   return(vec)
   } 

#### With oedema (only for weight-for-age and bmi-for-age)

prevph <- function(a,x,w,f) { 
   f<-as.character(f)
   ph <- sum((x > a)*w,na.rm=T)/sum(((!is.na(x)) | (f=="y"))*w,na.rm=T)
   aux <- 1.96*sqrt(ph*(1-ph)/sum(((!is.na(x)) | (f=="y"))*w,na.rm=T))+(1/(2*sum(((!is.na(x)) | (f=="y"))*w,na.rm=T)))
   vec <- c(rounde(sum(((!is.na(x)) | (f=="y"))*w,na.rm=T),digits=0),rounde(c(ph, max(0,ph-aux), min(1,ph+aux))*100,digits=1))
   return(vec)
   } 

#############################################################################
##### Prevalence calculation for the lower bound and corresponding 95% C.I.
#############################################################################

#### Without oedema (for height-for-age)

prevnh.L <- function(a,x,w) { 
   ph <- sum((x < a)*w,na.rm=T)/sum((!is.na(x))*w,na.rm=T)
   aux <- 1.96*sqrt(ph*(1-ph)/sum((!is.na(x))*w,na.rm=T))+(1/(2*sum((!is.na(x))*w,na.rm=T)))
   vec <- c(rounde(sum((!is.na(x))*w,na.rm=T),digits=0),rounde(c(ph, max(0,ph-aux), min(1,ph+aux))*100,digits=1))
   return(vec)
   }   

#### With oedema  (for all weight-related indicators)

prevnh <- function(a,x,w,f) { 
   f<-as.character(f)
   ph <- sum((x < a | f=="y")*w,na.rm=T)/sum(((!is.na(x)) | (f=="y"))*w,na.rm=T)
   aux <- 1.96*sqrt(ph*(1-ph)/sum(((!is.na(x)) | (f=="y"))*w,na.rm=T))+(1/(2*sum(((!is.na(x)) | (f=="y"))*w,na.rm=T)))
   vec <- c(rounde(sum(((!is.na(x)) | (f=="y"))*w,na.rm=T),digits=0),rounde(c(ph, max(0,ph-aux), min(1,ph+aux))*100,digits=1))
   return(vec)
   }   

###########################################
#### Weighted mean  and standard deviation
###########################################

wmean <- function(x,w) { return(rounde(sum(x*w,na.rm=T)/sum(w[!is.na(x)]),digits=2) )  }

wsd <- function(x,w) { 
   mh <- sum(x*w,na.rm=T)/sum((!is.na(x))*w,na.rm=T)
   sdh<-ifelse(length(x[!is.na(x)])>0,rounde(sqrt(sum(((x-mh)^2)*w,na.rm=T)/(sum((!is.na(x))*w,na.rm=T) - 1)),digits=2),NA)
   return( sdh )
   }


###########################################################################################
#### Rounding function - SPlus default rounding function uses the nearest even number rule
###########################################################################################

rounde <- function(x,digits=0) {
	expo<-10^digits
	return(ifelse(abs(x*expo) - floor(abs(x*expo)) < 0.5, sign(x*expo) * floor(abs(x*expo)), sign(x*expo) * (floor(abs(x*expo)) + 1))/expo)
}

######################################################################################
### Function for calculating individual height-for-age z-scores
######################################################################################

calc.zhfa<-function(mat,hfawho2007){

for(i in 1:length(mat$age.mo)) {
	
	if(!is.na(mat$age.mo[i]) & mat$age.mo[i]>=61 & mat$age.mo[i]<229) {
		
	### Interpolated l,m,s values
           
			low.age<-trunc(mat$age.mo[i])
          upp.age<-trunc(mat$age.mo[i]+1)
			diff.age<-(mat$age.mo[i]-low.age)
			
			if(diff.age>0) {																																																													
			l.val<-hfawho2007$l[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]+diff.age*( hfawho2007$l[hfawho2007$age==upp.age & hfawho2007$sex==mat$sex[i]]-hfawho2007$l[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]] )
			m.val<-hfawho2007$m[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]+diff.age*( hfawho2007$m[hfawho2007$age==upp.age & hfawho2007$sex==mat$sex[i]]-hfawho2007$m[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]] )
			s.val<-hfawho2007$s[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]+diff.age*( hfawho2007$s[hfawho2007$age==upp.age & hfawho2007$sex==mat$sex[i]]-hfawho2007$s[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]] )
			} else {
				l.val<-hfawho2007$l[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]
				m.val<-hfawho2007$m[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]
				s.val<-hfawho2007$s[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]
			}
		mat$zhfa[i]<-(((mat$height[i]/m.val)^l.val)-1)/(s.val*l.val)	
	
	}	else mat$zhfa[i]<- NA

}
return(mat)
}

######################################################################################
### Function for calculating individual weight-for-age z-scores
######################################################################################

calc.zwei<-function(mat,wfawho2007){

for(i in 1:length(mat$age.mo)) {

	if(!is.na(mat$age.mo[i])  & mat$age.mo[i]>=61 & mat$age.mo[i]<121 & mat$oedema[i]!="y") {
		
	### Interpolated l,m,s values

			low.age<-trunc(mat$age.mo[i])
          upp.age<-trunc(mat$age.mo[i]+1)
			diff.age<-(mat$age.mo[i]-low.age)
			
			if(diff.age>0) {																																																													
			l.val<-wfawho2007$l[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]+diff.age*( wfawho2007$l[wfawho2007$age==upp.age & wfawho2007$sex==mat$sex[i]]-wfawho2007$l[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]] )
			m.val<-wfawho2007$m[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]+diff.age*( wfawho2007$m[wfawho2007$age==upp.age & wfawho2007$sex==mat$sex[i]]-wfawho2007$m[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]] )
			s.val<-wfawho2007$s[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]+diff.age*( wfawho2007$s[wfawho2007$age==upp.age & wfawho2007$sex==mat$sex[i]]-wfawho2007$s[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]] )
			} else {
				l.val<-wfawho2007$l[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]
				m.val<-wfawho2007$m[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]
				s.val<-wfawho2007$s[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]
			}

		mat$zwfa[i]<-(((mat$weight[i]/m.val)^l.val)-1)/(s.val*l.val)
		if(!is.na(mat$zwfa[i]) & mat$zwfa[i]>3) {
						sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
						sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
						mat$zwfa[i]<- 3+((mat$weight[i]-sd3pos)/sd23pos)
						}
		if(!is.na(mat$zwfa[i]) & mat$zwfa[i]< (-3)) {
						sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
						sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
						mat$zwfa[i]<- (-3)+((mat$weight[i]-sd3neg)/sd23neg)
						}
						
		} else mat$zwfa[i]<-NA
}
return(mat)
}

######################################################################################
### Function for calulating individual BMI-for-age z-scores
######################################################################################

calc.zbmi<-function(mat,bfawho2007){

for(i in 1:length(mat$age.mo)) {
	
	if(!is.na(mat$age.mo[i]) & mat$age.mo[i]>=61 & mat$age.mo[i]<229 & mat$oedema[i]!="y") {
	
	### Interpolated l,m,s values

			low.age<-trunc(mat$age.mo[i])
          upp.age<-trunc(mat$age.mo[i]+1)
			diff.age<-(mat$age.mo[i]-low.age)
			
			if(diff.age>0) {																																																													
			l.val<-bfawho2007$l[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]+diff.age*( bfawho2007$l[bfawho2007$age==upp.age & bfawho2007$sex==mat$sex[i]]-bfawho2007$l[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]] )
			m.val<-bfawho2007$m[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]+diff.age*( bfawho2007$m[bfawho2007$age==upp.age & bfawho2007$sex==mat$sex[i]]-bfawho2007$m[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]] )
			s.val<-bfawho2007$s[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]+diff.age*( bfawho2007$s[bfawho2007$age==upp.age & bfawho2007$sex==mat$sex[i]]-bfawho2007$s[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]] )
			} else {
				l.val<-bfawho2007$l[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]
				m.val<-bfawho2007$m[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]
				s.val<-bfawho2007$s[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]
			}

		mat$zbfa[i]<-(((mat$cbmi[i]/m.val)^l.val)-1)/(s.val*l.val)
		if(!is.na(mat$zbfa[i]) & mat$zbfa[i]>3) {
						sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
						sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
						mat$zbfa[i]<- 3+((mat$cbmi[i]-sd3pos)/sd23pos)
						}
		if(!is.na(mat$zbfa[i]) & mat$zbfa[i]< (-3)) {
						sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
						sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
						mat$zbfa[i]<- (-3)+((mat$cbmi[i]-sd3neg)/sd23neg)
						}
						
		} else mat$zbfa[i]<-NA
			
}

return(mat)
}


###################################################################################
#### Main function starts here: who2007
###################################################################################

###############################################################################################################################################
#### This function can be used to:  
#### 1. Calculate the z-scores for the indicators: height-for-age, weight-for-age and body mass index-for-age
####    The output file with z-scores values is exported the file to an Excel spreadsheet (see readme file);
#### 2. Calculate the prevalence rates of stunting, underweight, wasting and overweight, and z-scores means and standard deviations. Results
####    are exported to an Excel spreadsheet, displayed by age group.
###############################################################################################################################################


#############################################################################
##### Function for calculating the z-scores for all indicators
#############################################################################

who2007 <- function(FileLab="Temp",FilePath="C:\\Documents and Settings",mydf,sex,age,weight,height,oedema=rep("n",dim(mydf)[1]),sw=rep(1,dim(mydf)[1])) {
	
#############################################################################
###########   Calculating the z-scores for all indicators
#############################################################################
 
   old <- options(warn=(-1))
   
   sex.x<-as.character(get(deparse(substitute(mydf)))[,deparse(substitute(sex))])
   age.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(age))])
   weight.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(weight))])
   height.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(height))])
   if(!missing(oedema)) oedema.vec<-as.character(get(deparse(substitute(mydf)))[,deparse(substitute(oedema))]) else oedema.vec<-oedema
   if(!missing(sw))	sw<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(sw))])	else sw<-as.double(sw)
   sw<-ifelse(is.na(sw),0,sw)

	sex.vec<-NULL
   sex.vec<-ifelse(sex.x!="NA" & (sex.x=="m" | sex.x=="M" | sex.x=="1"),1,ifelse(sex.x!="NA" & (sex.x=="f" | sex.x=="F" | sex.x=="2"),2,NA))
	age.vec<-age.x
	height.vec<-height.x
   oedema.vec<-ifelse(oedema.vec=="n" | oedema.vec=="N","n",ifelse(oedema.vec=="y" | oedema.vec=="Y","y","n"))

   mat<-cbind.data.frame(age.x,as.double(sex.vec),weight.x,height.x,oedema.vec,sw,stringsAsFactors=F)
	names(mat)<-c("age.mo","sex","weight","height","oedema","sw")
	
	mat$cbmi<-mat$weight/((height.vec/100)^2)
	mat$zhfa<-NULL
	mat$fhfa<-NULL
	mat$zwfa<-NULL
	mat$fwfa<-NULL
	mat$zbfa<-NULL
	mat$fbfa<-NULL

#############################################################################
###########   Calculating the z-scores for all indicators
#############################################################################

cat("Please wait while calculating z-scores...\n") 

### Height-for-age z-score

mat<-calc.zhfa(mat,hfawho2007)

### Weight-for-age z-score

mat<-calc.zwei(mat,wfawho2007)

### BMI-for-age z-score

mat<-calc.zbmi(mat,bfawho2007)


#### Rounding the z-scores to two decimals

			mat$zhfa<-rounde(mat$zhfa,digits=2)
			mat$zwfa<-rounde(mat$zwfa,digits=2)
			mat$zbfa<-rounde(mat$zbfa,digits=2)

#### Flagging z-score values for individual indicators

			mat$fhfa<-ifelse(abs(mat$zhfa) > 6,1,0)
			mat$fwfa<-ifelse(mat$zwfa > 5 | mat$zwfa < (-6),1,0)
			mat$fbfa<-ifelse(abs(mat$zbfa) > 5,1,0)
			
if(is.na(mat$age.mo) & mat$oedema=="y") {
mat$fhfa<-NA
mat$zwfa<-NA
mat$zbfa<-NA
}

mat<-cbind.data.frame(mydf,mat[,-c(2:6)])

###################################################################################################
######### Export data frame with z-scores and flag variables
###################################################################################################


assign("matz",mat,envir = .GlobalEnv)

write.table(matz, file=paste(FilePath,"\\",FileLab,"_z.csv",sep=""),na="",row.names = FALSE,sep=",",quote = TRUE)


cat(paste("Z-scores calculated and exported to ",FilePath,"\\",FileLab,"_z.csv\n\n",sep="")) 


#######################################################################################################
#### Calculating prevalences and summary statistics. 
#######################################################################################################

if(any(sw <0)) stop("Negative weights are not allowed and program will stop. Prevalence tables will not be produced.")

mat.out<-mat

mat.out$sw.vec<-sw
mat.out$sex.vec<-as.double(sex.vec)
mat.out$oedema.vec<-as.character(oedema.vec)
mat.out$oedema.vec1<-as.character(oedema.vec)

mat.out<-mat.out[!is.na(mat.out$age.mo) & mat.out$age.mo>=61 & mat.out$age.mo<229,]

####################################################
#### Creating age group variable in completed years
####################################################

mat.out$agegr <- floor(mat.out$age.mo/12)

##############################################
#### Make z-score as missing if it is flagged
##############################################

mat.out$zhfa<-ifelse(!is.na(mat.out$fhfa) & mat.out$fhfa!=0,NA,mat.out$zhfa)
mat.out$zwfa<-ifelse(!is.na(mat.out$fwfa) & mat.out$fwfa!=0,NA,mat.out$zwfa)
mat.out$zbfa<-ifelse(!is.na(mat.out$fbfa) & mat.out$fbfa!=0,NA,mat.out$zbfa)

if(dim(mat.out)[1]==0) stop("\n\nNo non-missing z-score values are available for calculating prevalences. Program will stop!\n\n.")


##############################################
#### Include all levels of age group variable
##############################################

mat.aux<-as.data.frame(cbind(array(rep(NA,((dim(mat.out)[2]-1)*15)),dim=c(15,(dim(mat.out)[2]-1)) ),seq(5,19,1)))

names(mat.aux)<-names(mat.out)

mat.out<-rbind(mat.out,mat.aux)

cat(" starting ")

########################################################################################################
#### Make Oedema variable to be "n" if age smaller than 61 mo or greater than 120 mo (for weight-for-age)
#### or smaller than 61 mo or greater than 228 mo (for bmi-for-age).
#### This is because children with oedema counts in the prevalence even if z-score is missing
#### for weight related indicators.
########################################################################################################

mat.out$oedema.vec<-ifelse((!is.na(mat.out$age.mo) & (mat.out$age.mo<61 | mat.out$age.mo>=229))  | mat.out$oedema.vec=="NA","n",mat.out$oedema.vec)
mat.out$oedema.vec1<-ifelse((!is.na(mat.out$age.mo) & (mat.out$age.mo<61 | mat.out$age.mo>=121))  | mat.out$oedema.vec=="NA","n",mat.out$oedema.vec)

#####################################################################################################################################################
#### Creating matrix with estimated prevalences, confidence intervals, and means and standard deviations of z-scores and exporting it to Excel file.
#####################################################################################################################################################

cat("\nPlease wait while calculating prevalences and z-score summary statistics...\n") 

#### Sexes combined

#### % < -3 SD for all the indicators
mat<- t(cbind.data.frame(#
       prevnh(-3,mat.out$zwfa,mat.out$sw.vec,mat.out$oedema.vec1),lapply(split(mat.out, mat.out$agegr),function(z) prevnh(-3, z$zwfa, z$sw.vec, z$oedema.vec1)),#
       prevnh.L(-3,mat.out$zhfa,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr), function(z) prevnh.L(-3, z$zhfa, z$sw.vec)),#
       prevnh(-3,mat.out$zbfa,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevnh(-3, z$zbfa, z$sw.vec, z$oedema.vec))))
       
#### % < -2 SD for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       prevnh(-2,mat.out$zwfa,mat.out$sw.vec,mat.out$oedema.vec1),lapply(split(mat.out, mat.out$agegr),function(z) prevnh(-2, z$zwfa, z$sw.vec, z$oedema.vec1)),#
       prevnh.L(-2,mat.out$zhfa,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr), function(z) prevnh.L(-2, z$zhfa, z$sw.vec)),#
       prevnh(-2,mat.out$zbfa,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevnh(-2, z$zbfa, z$sw.vec, z$oedema.vec))))[,-1])

#### % > +1 SD for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       prevph(1,mat.out$zwfa,mat.out$sw.vec,mat.out$oedema.vec1),lapply(split(mat.out, mat.out$agegr),function(z) prevph(1, z$zwfa, z$sw.vec, z$oedema.vec1)),#
       prevph.L(1,mat.out$zhfa,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr), function(z) prevph.L(1, z$zhfa, z$sw.vec)),#
       prevph(1,mat.out$zbfa,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevph(1, z$zbfa, z$sw.vec, z$oedema.vec))))[,-1])

#### % > +2 SD for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       prevph(2,mat.out$zwfa,mat.out$sw.vec,mat.out$oedema.vec1),lapply(split(mat.out, mat.out$agegr),function(z) prevph(2, z$zwfa, z$sw.vec, z$oedema.vec1)),#
       prevph.L(2,mat.out$zhfa,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr), function(z) prevph.L(2, z$zhfa, z$sw.vec)),#
       prevph(2,mat.out$zbfa,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevph(2, z$zbfa, z$sw.vec, z$oedema.vec))))[,-1])

#### % > +3 SD for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       prevph(3,mat.out$zwfa,mat.out$sw.vec,mat.out$oedema.vec1),lapply(split(mat.out, mat.out$agegr),function(z) prevph(3, z$zwfa, z$sw.vec, z$oedema.vec1)),#
       prevph.L(3,mat.out$zhfa,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr), function(z) prevph.L(3, z$zhfa, z$sw.vec)),#
       prevph(3,mat.out$zbfa,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevph(3, z$zbfa, z$sw.vec, z$oedema.vec))))[,-1])

#### Means of z-scores for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       wmean(mat.out$zwfa,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr),function(z) wmean(z$zwfa,z$sw.vec)),#
       wmean(mat.out$zhfa,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr),function(z) wmean(z$zhfa,z$sw.vec)),#
       wmean(mat.out$zbfa,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr),function(z) wmean(z$zbfa,z$sw.vec)))))

#### Standard deviations of z-scores for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       wsd(mat.out$zwfa,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr),function(z) wsd(z$zwfa,z$sw.vec)),#
       wsd(mat.out$zhfa,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr),function(z) wsd(z$zhfa,z$sw.vec)),#
       wsd(mat.out$zbfa,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr),function(z) wsd(z$zbfa,z$sw.vec)))))

####################################################################################################################
##### Exporting matrix to Excel file

rm(mat1)

mat1<-rbind(c("Set 1:","Sexes","combined",rep("",15)),
                       c("Weight","-for-","age",rep("",15)),
                       c("N","% < -3 SD","95%", "C.I.","% < -2 SD","95%", "C.I.","% > +1 SD","95%", "C.I.","% > +2 SD","95%", "C.I.","% > +3 SD","95%", "C.I.","Mean","SD"),mat[1:16,],#
                       c("Height","-for-","age",rep("",15)),#
                       c("N","% < -3 SD","95%", "C.I.","% < -2 SD","95%", "C.I.","% > +1 SD","95%", "C.I.","% > +2 SD","95%", "C.I.","% > +3 SD","95%", "C.I.","Mean","SD"),mat[17:32,],#
                       c("BMI","-for-","age",rep("",15)),
                       c("N","% < -3 SD","95%", "C.I.","% < -2 SD","95%", "C.I.","% > +1 SD","95%", "C.I.","% > +2 SD","95%", "C.I.","% > +3 SD","95%", "C.I.","Mean","SD"),mat[33:48,])

for(j in 1:dim(mat1)[2]) mat1[,j]<-ifelse(mat1[,j]=="NA" | mat1[,j]=="NaN","",mat1[,j])

mat1<-cbind(c("",rep(c("","Age","5-19",as.character(5:19)),3)),mat1)

	
####################################################################################################################

##### For boys and girls

for(i in 1:2) {

mat.out.sex<-mat.out[!is.na(mat.out$sex.vec) & mat.out$sex.vec==i,]	

mat.aux<-as.data.frame(cbind(array(rep(NA,((dim(mat.out.sex)[2]-1)*15)),dim=c(15,(dim(mat.out.sex)[2]-1)) ),seq(5,19,1)))
names(mat.aux)<-names(mat.out.sex)
mat.out.sex<-rbind.data.frame(mat.out.sex,mat.aux)
mat.out.sex$oedema.vec<-ifelse((!is.na(mat.out.sex$age.mo) & (mat.out.sex$age.mo<61 | mat.out.sex$age.mo>=229)) | mat.out.sex$oedema.vec=="NA","n",mat.out.sex$oedema.vec)

#### % < -3 SD for all the indicators
mat<- t(cbind.data.frame(#
       prevnh(-3,mat.out.sex$zwfa,mat.out.sex$sw.vec,mat.out.sex$oedema.vec1),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevnh(-3, z$zwfa, z$sw.vec, z$oedema.vec1)),#
       prevnh.L(-3,mat.out.sex$zhfa,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr), function(z) prevnh.L(-3, z$zhfa, z$sw.vec)),#
       prevnh(-3,mat.out.sex$zbfa,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevnh(-3, z$zbfa, z$sw.vec, z$oedema.vec))))
       
#### % < -2 SD for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       prevnh(-2,mat.out.sex$zwfa,mat.out.sex$sw.vec,mat.out.sex$oedema.vec1),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevnh(-2, z$zwfa, z$sw.vec, z$oedema.vec1)),#
       prevnh.L(-2,mat.out.sex$zhfa,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr), function(z) prevnh.L(-2, z$zhfa, z$sw.vec)),#
       prevnh(-2,mat.out.sex$zbfa,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevnh(-2, z$zbfa, z$sw.vec, z$oedema.vec))))[,-1])

#### % > +1 SD for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       prevph(1,mat.out.sex$zwfa,mat.out.sex$sw.vec,mat.out.sex$oedema.vec1),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevph(1, z$zwfa, z$sw.vec, z$oedema.vec1)),#
       prevph.L(1,mat.out.sex$zhfa,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr), function(z) prevph.L(1, z$zhfa, z$sw.vec)),#
       prevph(1,mat.out.sex$zbfa,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevph(1, z$zbfa, z$sw.vec, z$oedema.vec))))[,-1])

#### % > +2 SD for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       prevph(2,mat.out.sex$zwfa,mat.out.sex$sw.vec,mat.out.sex$oedema.vec1),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevph(2, z$zwfa, z$sw.vec, z$oedema.vec1)),#
       prevph.L(2,mat.out.sex$zhfa,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr), function(z) prevph.L(2, z$zhfa, z$sw.vec)),#
       prevph(2,mat.out.sex$zbfa,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevph(2, z$zbfa, z$sw.vec, z$oedema.vec))))[,-1])

#### % > +3 SD for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       prevph(3,mat.out.sex$zwfa,mat.out.sex$sw.vec,mat.out.sex$oedema.vec1),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevph(3, z$zwfa, z$sw.vec, z$oedema.vec1)),#
       prevph.L(3,mat.out.sex$zhfa,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr), function(z) prevph.L(3, z$zhfa, z$sw.vec)),#
       prevph(3,mat.out.sex$zbfa,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevph(3, z$zbfa, z$sw.vec, z$oedema.vec))))[,-1])

#### Means of z-scores for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       wmean(mat.out.sex$zwfa,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) wmean(z$zwfa,z$sw.vec)),#
       wmean(mat.out.sex$zhfa,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) wmean(z$zhfa,z$sw.vec)),#
       wmean(mat.out.sex$zbfa,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) wmean(z$zbfa,z$sw.vec)))))

#### Standard deviations of z-scores for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       wsd(mat.out.sex$zwfa,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) wsd(z$zwfa,z$sw.vec)),#
       wsd(mat.out.sex$zhfa,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) wsd(z$zhfa,z$sw.vec)),#
       wsd(mat.out.sex$zbfa,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) wsd(z$zbfa,z$sw.vec)))))

####################################################################################################################
##### Exporting matrix to Excel file

mat2<-rbind.data.frame(c(paste("Set ",i+1,":",sep=""),c("Males","Females")[i],rep("",16)),
                       c("Weight","-for-","age",rep("",15)),
                       c("N","% < -3 SD","95%", "C.I.","% < -2 SD","95%", "C.I.","% > +1 SD","95%", "C.I.","% > +2 SD","95%", "C.I.","% > +3 SD","95%", "C.I.","Mean","SD"),mat[1:16,],#
                       c("Height","-for-","age",rep("",15)),#
                       c("N","% < -3 SD","95%", "C.I.","% < -2 SD","95%", "C.I.","% > +1 SD","95%", "C.I.","% > +2 SD","95%", "C.I.","% > +3 SD","95%", "C.I.","Mean","SD"),mat[17:32,],#
                       c("BMI","-for-","age",rep("",15)),
                       c("N","% < -3 SD","95%", "C.I.","% < -2 SD","95%", "C.I.","% > +1 SD","95%", "C.I.","% > +2 SD","95%", "C.I.","% > +3 SD","95%", "C.I.","Mean","SD"),mat[33:48,])

for(j in 1:dim(mat2)[2]) mat2[,j]<-ifelse(mat2[,j]=="NA" | mat2[,j]=="NaN","",mat2[,j])

mat2<-cbind(c("",rep(c("","Age","5-19",as.character(5:19)),3)),mat2)

names(mat2)<-names(mat1)

mat1<-rbind(mat1,mat2)

}    #### End of loop for sex

mat1<-mat1[-c(11:19,66:74,121:129),]

###################################################################################################################
######### Export table with prevalence values and their confidence intervals, and mean and SD of the z-scores
###################################################################################################################


assign("matprev",mat1,envir = .GlobalEnv)

write.table(matprev, file=paste(FilePath,"\\",FileLab,"_prev.csv",sep=""),na=" ",row.names = FALSE,col.names=F,sep=",",quote = TRUE)

cat(paste("Prevalences and z-score summary statistics calculated and exported to ",FilePath,"\\",FileLab,"_prev.csv\n",sep="")) 

on.exit(options(old))

invisible()


}   #### End of main function who2007

# wfawho2007<-read.table("D:\\References 5-20y\\Macro R\\who2007_R\\wfawho2007.txt",header=T,sep="",skip=0)
# hfawho2007<-read.table("D:\\References 5-20y\\Macro R\\who2007_R\\hfawho2007.txt",header=T,sep="",skip=0)
# bfawho2007<-read.table("D:\\References 5-20y\\Macro R\\who2007_R\\bfawho2007.txt",header=T,sep="",skip=0)

# survey.who2007<-read.csv("D:\\References 5-20y\\Macro R\\who2007_R\\survey_who2007.csv",header=T,sep=",",skip=0,na.strings="")

# source("D:\\References 5-20y\\Macro R\\who2007_R\\who2007.r")

# who2007(FileLab = "survey_who2007", FilePath = "D:\\References 5-20y\\Macro R\\who2007_R", mydf = survey.who2007,sex = sex, age = agemons, weight = weight, height = height, sw=sw, oedema=oedema)



