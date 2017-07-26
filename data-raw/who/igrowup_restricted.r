##################################################################################################################
#########  WHO Child Growth Standards                                                                        #####
#########  Department of Nutrition for Health and Development                                                #####
#########  World Health Organization                                                                         #####
#########  Last modified on 07/10/2013     -     Developed using R version 3.0.1 (2013-05-16)                #####
#########  This code corcerns the restricted approach for the prevalences, i.e. the calculation of the       #####
#########  prevalences takes into account only records with all z-scores valid (non-missing) for the first   #####
#########  four indicators: weight-for-age, length/height-for-age, weight-for-length/height and BMI-for-age. #####
#########  Note: this analysis provides results only for these four indicators.                              #####
##################################################################################################################
##################################################################################################################


##################################################################################################################
#########  Function for calculating the z-scores and prevalences for a nutritional survey                    #####
##################################################################################################################


########################################################################
#### Auxiliar functions
########################################################################

#############################################################################
##### Prevalence calculation for the upper bound and corresponding 95% C.I.
#############################################################################

#### With oedema (only for weight-for-length/height and bmi-for-age)

prevph <- function(a,x,w,f) { 
   f<-as.character(f)
   ph <- sum((x > a)*w,na.rm=T)/sum(((!is.na(x)) | (f=="y"))*w,na.rm=T)
   aux <- 1.96*sqrt(ph*(1-ph)/sum(((!is.na(x)) | (f=="y"))*w,na.rm=T))+(1/(2*sum(((!is.na(x)) | (f=="y"))*w,na.rm=T)))
   vec <- c(rounde(sum(((!is.na(x)) | (f=="y"))*w,na.rm=T),digits=0),rounde(c(ph, max(0,ph-aux), ph+aux)*100,digits=1))
   return(vec)
   } 

#############################################################################
##### Prevalence calculation for the lower bound and corresponding 95% C.I.
#############################################################################

#### Without oedema (for length/height-for-age)

prevnh.L <- function(a,x,w) { 
   ph <- sum((x < a)*w,na.rm=T)/sum((!is.na(x))*w,na.rm=T)
   aux <- 1.96*sqrt(ph*(1-ph)/sum((!is.na(x))*w,na.rm=T))+(1/(2*sum((!is.na(x))*w,na.rm=T)))
   vec <- c(rounde(sum((!is.na(x))*w,na.rm=T),digits=0),rounde(c(ph, max(0,ph-aux), ph+aux)*100,digits=1))
   return(vec)
   }   

#### With oedema  (for all weight-related indicators)

prevnh <- function(a,x,w,f) { 
   f<-as.character(f)
   ph <- sum((x < a | f=="y")*w,na.rm=T)/sum(((!is.na(x)) | (f=="y"))*w,na.rm=T)
   aux <- 1.96*sqrt(ph*(1-ph)/sum(((!is.na(x)) | (f=="y"))*w,na.rm=T))+(1/(2*sum(((!is.na(x)) | (f=="y"))*w,na.rm=T)))
   vec <- c(rounde(sum(((!is.na(x)) | (f=="y"))*w,na.rm=T),digits=0),rounde(c(ph, max(0,ph-aux), ph+aux)*100,digits=1))
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


######################################################################################
#### Rounding function - SPlus rounding function uses the nearest even number rule
######################################################################################

rounde <- function(x,digits=0) {
	expo<-10^digits
	return(ifelse(abs(x*expo) - floor(abs(x*expo)) < 0.5, sign(x*expo) * floor(abs(x*expo)), sign(x*expo) * (floor(abs(x*expo)) + 1))/expo)
}

######################################################################################
### Function for calulating individual Length-for-age z-scores
######################################################################################

calc.zlen<-function(mat,lenanthro){

for(i in 1:length(mat$age.days)) {
	
	if(!is.na(mat$age.days[i])) {
		
		l.val<-lenanthro$l[lenanthro$age==mat$age.days[i] & lenanthro$sex==mat$sex[i]]
		m.val<-lenanthro$m[lenanthro$age==mat$age.days[i] & lenanthro$sex==mat$sex[i]]
		s.val<-lenanthro$s[lenanthro$age==mat$age.days[i] & lenanthro$sex==mat$sex[i]]
		mat$zlen[i]<-(((mat$clenhei[i]/m.val)^l.val)-1)/(s.val*l.val)	
	
	}	else mat$zlen[i]<- NA

}
return(mat)
}

######################################################################################
### Function for calulating individual Weight-for-age z-scores
######################################################################################

calc.zwei<-function(mat,weianthro){

for(i in 1:length(mat$age.days)) {

	if(!is.na(mat$age.days[i]) & mat$oedema[i]!="y") {
		
		l.val<-weianthro$l[weianthro$age==mat$age.days[i] & weianthro$sex==mat$sex[i]]
		m.val<-weianthro$m[weianthro$age==mat$age.days[i] & weianthro$sex==mat$sex[i]]
		s.val<-weianthro$s[weianthro$age==mat$age.days[i] & weianthro$sex==mat$sex[i]]

		mat$zwei[i]<-(((mat$weight[i]/m.val)^l.val)-1)/(s.val*l.val)
		if(!is.na(mat$zwei[i]) & mat$zwei[i]>3) {
						sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
						sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
						mat$zwei[i]<- 3+((mat$weight[i]-sd3pos)/sd23pos)
						}
		if(!is.na(mat$zwei[i]) & mat$zwei[i]< (-3)) {
						sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
						sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
						mat$zwei[i]<- (-3)+((mat$weight[i]-sd3neg)/sd23neg)
						}
						
		} else mat$zwei[i]<-NA
}
return(mat)
}

######################################################################################
### Function for calulating individual Weight-for-length/height z-scores
######################################################################################

calc.zwfl<-function(mat,wflanthro,wfhanthro){

for(i in 1:length(mat$age.days)) {

	mat$zwfl[i]<-NA

       if(mat$oedema[i]!="y") {
	
		if( (!is.na(mat$age.days[i]) & mat$age.days[i]<731) | (is.na(mat$age.days[i]) & !is.na(mat$l.h[i]) & (mat$l.h[i]=="l" | mat$l.h[i]=="L")) | (is.na(mat$age.days[i]) & is.na(mat$l.h[i]) & !is.na(mat$clenhei[i]) & mat$clenhei[i]<87) ) {
			
			if(!is.na(mat$clenhei[i]) & mat$clenhei[i]>=45 & mat$clenhei[i]<=110) {
		
			### Interpolated l,m,s values

			low.len<-trunc(mat$clenhei[i]*10)/10
          upp.len<-trunc(mat$clenhei[i]*10+1)/10
			diff.len<-(mat$clenhei[i]-low.len)/0.1
			
			if(diff.len>0) {																																																													
			l.val<-wflanthro$l[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]]+diff.len*( wflanthro$l[wflanthro$length==upp.len & wflanthro$sex==mat$sex[i]]-wflanthro$l[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]] )
			m.val<-wflanthro$m[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]]+diff.len*( wflanthro$m[wflanthro$length==upp.len & wflanthro$sex==mat$sex[i]]-wflanthro$m[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]] )
			s.val<-wflanthro$s[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]]+diff.len*( wflanthro$s[wflanthro$length==upp.len & wflanthro$sex==mat$sex[i]]-wflanthro$s[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]] )
			} else {
				l.val<-wflanthro$l[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]]
				m.val<-wflanthro$m[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]]
				s.val<-wflanthro$s[wflanthro$length==low.len & wflanthro$sex==mat$sex[i]]
			}
		
			mat$zwfl[i]<-(((mat$weight[i]/m.val)^l.val)-1)/(s.val*l.val)
			if(!is.na(mat$zwfl[i]) & mat$zwfl[i]>3) {
							sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
							sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
							mat$zwfl[i]<- 3+((mat$weight[i]-sd3pos)/sd23pos)
						}
			if(!is.na(mat$zwfl[i]) & mat$zwfl[i]<(-3)) {
							sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
							sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
							mat$zwfl[i]<- (-3)-((sd3neg-mat$weight[i])/sd23neg)
							}
				}
			}
			
			else 		if( (!is.na(mat$age.days[i]) & mat$age.days[i]>=731) | (is.na(mat$age.days[i]) & !is.na(mat$l.h[i]) & (mat$l.h[i]=="h" | mat$l.h[i]=="H"))  | (is.na(mat$age.days[i]) & is.na(mat$l.h[i]) & !is.na(mat$clenhei[i]) & mat$clenhei[i]>=87) ) {
			
			if(!is.na(mat$clenhei[i]) & mat$clenhei[i]>=65 & mat$clenhei[i]<=120) {
		
			### Interpolated l,m,s values

			low.len<-trunc(mat$clenhei[i]*10)/10
          		upp.len<-trunc(mat$clenhei[i]*10+1)/10
			diff.len<-(mat$clenhei[i]-low.len)/0.1
			
			if(diff.len>0) {	
			l.val<-wfhanthro$l[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]]+diff.len*( wfhanthro$l[wfhanthro$height==upp.len & wfhanthro$sex==mat$sex[i]]-wfhanthro$l[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]] )
			m.val<-wfhanthro$m[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]]+diff.len*( wfhanthro$m[wfhanthro$height==upp.len & wfhanthro$sex==mat$sex[i]]-wfhanthro$m[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]] )
			s.val<-wfhanthro$s[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]]+diff.len*( wfhanthro$s[wfhanthro$height==upp.len & wfhanthro$sex==mat$sex[i]]-wfhanthro$s[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]] )
          } else {
			l.val<-wfhanthro$l[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]]
			m.val<-wfhanthro$m[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]]
			s.val<-wfhanthro$s[wfhanthro$height==low.len & wfhanthro$sex==mat$sex[i]]
			}

			mat$zwfl[i]<-(((mat$weight[i]/m.val)^l.val)-1)/(s.val*l.val)
			if(!is.na(mat$zwfl[i]) & mat$zwfl[i]>3) {
							sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
							sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
							mat$zwfl[i]<- 3+((mat$weight[i]-sd3pos)/sd23pos)
							}
			if(!is.na(mat$zwfl[i]) & mat$zwfl[i]<(-3)) {
							sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
							sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
							mat$zwfl[i]<- (-3)-((sd3neg-mat$weight[i])/sd23neg)
							}
				}
			}
		}
		
		if(!is.na(mat$age.day[i]) & mat$age.days[i]>1856) mat$zwfl[i]<-NA

}

return(mat)
}

######################################################################################
### Function for calulating individual BMI-for-age z-scores
######################################################################################

calc.zbmi<-function(mat,bmianthro){

for(i in 1:length(mat$age.days)) {
	
	if(!is.na(mat$age.days[i]) & mat$age.days[i]>=0 & mat$age.days[i]<=1856 & mat$oedema[i]!="y") {
		
		l.val<-bmianthro$l[bmianthro$age==mat$age.days[i] & bmianthro$sex==mat$sex[i]]
		m.val<-bmianthro$m[bmianthro$age==mat$age.days[i] & bmianthro$sex==mat$sex[i]]
		s.val<-bmianthro$s[bmianthro$age==mat$age.days[i] & bmianthro$sex==mat$sex[i]]

		mat$zbmi[i]<-(((mat$cbmi[i]/m.val)^l.val)-1)/(s.val*l.val)
		if(!is.na(mat$zbmi[i]) & mat$zbmi[i]>3) {
						sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
						sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
						mat$zbmi[i]<- 3+((mat$cbmi[i]-sd3pos)/sd23pos)
						}
		if(!is.na(mat$zbmi[i]) & mat$zbmi[i]< (-3)) {
						sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
						sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
						mat$zbmi[i]<- (-3)+((mat$cbmi[i]-sd3neg)/sd23neg)
						}
						
		} else mat$zbmi[i]<-NA
			
}

return(mat)
}


###################################################################################
#### Main function starts here: igrowup
###################################################################################

###############################################################################################################################################
#### This function can be used to:  
#### 1. Calculate the z-scores for the indicators: length/height-for-age, weight-for-age, weight-for-legnth/height and body mass index-for-age
####    The output file with z-scores values is exported the file to an Excel spreadsheet (see readme file);
#### 2. Calculate the prevalence rates of stunting, underweight, wasting and overweight, and z-scores means and standard deviations. Results
####    are exported to an Excel spreadsheet, displayed by age group.
###############################################################################################################################################


#############################################################################
##### Function for calculating the z-scores for all indicators
#############################################################################

igrowup.restricted <- function(FileLab="Temp",FilePath="C:\\Documents and Settings",mydf,sex,age,age.month=F,weight,lenhei,measure=rep(NA,dim(mydf)[1]),
              oedema=rep("n",dim(mydf)[1]),sw=rep(1,dim(mydf)[1])) {
	
#############################################################################
###########   Calculating the z-scores for all indicators
#############################################################################
 
   old <- options(warn=(-1))
   
   sex.x<-as.character(get(deparse(substitute(mydf)))[,deparse(substitute(sex))])
   age.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(age))])
   weight.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(weight))])
   lenhei.x<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(lenhei))])
   if(!missing(measure)) lorh.vec<-as.character(get(deparse(substitute(mydf)))[,deparse(substitute(measure))]) else lorh.vec<-as.character(measure)
   if(!missing(oedema)) oedema.vec<-as.character(get(deparse(substitute(mydf)))[,deparse(substitute(oedema))]) else oedema.vec<-oedema
   if(!missing(sw))	sw<-as.double(get(deparse(substitute(mydf)))[,deparse(substitute(sw))])	else sw<-as.double(sw)
   sw<-ifelse(is.na(sw),0,sw)

	sex.vec<-NULL
	
	if(age.month) age.vec<-rounde(age.x*30.4375) else age.vec<-rounde(age.x)
	lenhei.vec<-ifelse((!is.na(age.vec) & age.vec<731 & !is.na(lorh.vec) & (lorh.vec=="h" | lorh.vec=="H")),lenhei.x+0.7,#
		  ifelse((!is.na(age.vec) & age.vec>=731 & !is.na(lorh.vec) & (lorh.vec=="l" | lorh.vec=="L")),lenhei.x-0.7,lenhei.x))
    
   sex.vec<-ifelse(!is.na(sex.x) & (sex.x=="m" | sex.x=="M" | sex.x=="1"),1,ifelse(!is.na(sex.x) & (sex.x=="f" | sex.x=="F" | sex.x=="2"),2,NA))

   lorh.vec<-ifelse(is.na(lorh.vec) | lorh.vec=="l" | lorh.vec=="L" | lorh.vec=="h" | lorh.vec=="H",lorh.vec,NA)

   oedema.vec<-ifelse(oedema.vec=="n" | oedema.vec=="N","n",ifelse(oedema.vec=="y" | oedema.vec=="Y","y","n"))

   mat<-cbind.data.frame(age.x,as.integer(age.vec),as.double(sex.vec),weight.x,lenhei.x,lorh.vec,lenhei.vec,oedema.vec,sw,stringsAsFactors=F)
	names(mat)<-c("age","age.days","sex","weight","len.hei","l.h","clenhei","oedema","sw")
	
	mat$cbmi<-mat$weight/((lenhei.vec/100)^2)
	mat$zlen<-NULL
	mat$flen<-NULL
	mat$zwei<-NULL
	mat$fwei<-NULL
	mat$zwfl<-NULL
	mat$fwfl<-NULL
	mat$zbmi<-NULL
	mat$fbmi<-NULL

#############################################################################
###########   Calculating the z-scores for all indicators
#############################################################################

cat("Please wait while calculating z-scores...\n") 

### Length-for-age z-score

mat<-calc.zlen(mat,lenanthro)

### Weight-for-age z-score

mat<-calc.zwei(mat,weianthro)

### Weight-for-length/height z-score

mat<-calc.zwfl(mat,wflanthro,wfhanthro)

### BMI-for-age z-score

mat<-calc.zbmi(mat,bmianthro)


#### Rounding the z-scores to two decimals

			mat$zlen<-rounde(mat$zlen,digits=2)
			mat$zwei<-rounde(mat$zwei,digits=2)
			mat$zwfl<-rounde(mat$zwfl,digits=2)
			mat$zbmi<-rounde(mat$zbmi,digits=2)

#### Flagging z-score values for individual indicators

			mat$flen<-ifelse(abs(mat$zlen) > 6,1,0)
			mat$fwei<-ifelse(mat$zwei > 5 | mat$zwei < (-6),1,0)
			mat$fwfl<-ifelse(abs(mat$zwfl) > 5,1,0)
			mat$fbmi<-ifelse(abs(mat$zbmi) > 5,1,0)

mat<-cbind.data.frame(mydf,mat[,-c(1,3:6,8:9)])

###################################################################################################
######### Export data frame with z-scores and flag variables
###################################################################################################

assign("matz",mat,envir = .GlobalEnv)

write.table(matz, file=paste(FilePath,"\\",FileLab,"_z_rc.csv",sep=""),na="",row.names = FALSE,sep=",",quote = TRUE)


cat(paste("Z-scores calculated and exported to ",FilePath,"\\",FileLab,"_z_rc.csv\n\n",sep="")) 

#######################################################################################################
#### Calculating prevalences and summary statistics. 
#######################################################################################################

if(any(sw <0)) stop("Negative weights are not allowed and program will stop. Prevalence tables will not be produced.")

mat.out<-mat

################################
#### Creating age group variable
################################

mat.out$sw.vec<-sw
mat.out$sex.vec<-as.double(sex.vec)
mat.out$oedema.vec<-as.character(oedema.vec)

if(age.month) mat.out$agegr <- ifelse(!is.na(age.x) & age.x<6,0,ifelse(!is.na(age.x) & age.x<12,6,#
                     ifelse(!is.na(age.x) & age.x<24,12,ifelse(!is.na(age.x) & age.x<36,24,#
                     ifelse(!is.na(age.x) & age.x<48,36,ifelse(!is.na(age.x) & age.x<61,48,NA)  )))))

else mat.out$agegr <- ifelse(!is.na(mat.out$age.days) & mat.out$age.days/30.4375<6,0,ifelse(!is.na(mat.out$age.days) & mat.out$age.days/30.4375<12,6,#
                     ifelse(!is.na(mat.out$age.days) & mat.out$age.days/30.4375<24,12,ifelse(!is.na(mat.out$age.days) & mat.out$age.days/30.4375<36,24,#
                     ifelse(!is.na(mat.out$age.days) & mat.out$age.days/30.4375<48,36,ifelse(!is.na(mat.out$age.days) & mat.out$age.days/30.4375<61,48,NA)  )))))

##############################################
#### Make z-score as missing if it is flagged
##############################################

mat.out$zlen<-ifelse(!is.na(mat.out$flen) & mat.out$flen!=0,NA,mat.out$zlen)
mat.out$zwei<-ifelse(!is.na(mat.out$fwei) & mat.out$fwei!=0,NA,mat.out$zwei)
mat.out$zwfl<-ifelse(!is.na(mat.out$fwfl) & mat.out$fwfl!=0,NA,mat.out$zwfl)
mat.out$zbmi<-ifelse(!is.na(mat.out$fbmi) & mat.out$fbmi!=0,NA,mat.out$zbmi)

mat.out<-mat.out[!is.na(mat.out$zlen) & !is.na(mat.out$zwei) & !is.na(mat.out$zwfl) & !is.na(mat.out$zbmi),]
if(dim(mat.out)[1]==0) stop("\n\nNo non-missing z-score values are available for calculating prevalences with the restricted approach. Program will stop!\n\n.")

##############################################
#### Include all levels of age group variable
##############################################

mat.aux<-as.data.frame(cbind(array(rep(NA,((dim(mat.out)[2]-1)*6)),dim=c(6,(dim(mat.out)[2]-1)) ),c(0,6,12,24,36,48)))
names(mat.aux)<-names(mat.out)
mat.out<-rbind(mat.out,mat.aux)

##############################################################################################
#### Make Oedema variable to be "n" if age greater than 60 completed months (>=61 months).
#### This is beacuse children with oedema counts in the prevalence even if z-score is missing
#### for weight related indicators.
##############################################################################################

mat.out$oedema.vec<-ifelse((!is.na(mat.out$age.days) & mat.out$age.days/30.4375>=61) | mat.out$oedema.vec=="NA","n",mat.out$oedema.vec)

#####################################################################################################################################################
#### Creating matrix with estimated prevalences, confidence intervals, and means and standard deviations of z-scores and exporting it to Excel file.
#####################################################################################################################################################

cat("\nPlease wait while calculating prevalences and z-score summary statistics...\n") 

#### Sexes combined

#### % < -3 SD for all the indicators
mat<- t(cbind.data.frame(#
       prevnh(-3,mat.out$zwei,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevnh(-3, z$zwei, z$sw.vec, z$oedema.vec)),#
       prevnh.L(-3,mat.out$zlen,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr), function(z) prevnh.L(-3, z$zlen, z$sw.vec)),#
       prevnh(-3,mat.out$zwfl,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevnh(-3, z$zwfl, z$sw.vec, z$oedema.vec)),#
       prevnh(-3,mat.out$zbmi,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevnh(-3, z$zbmi, z$sw.vec, z$oedema.vec))))
       
#### % < -2 SD for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       prevnh(-2,mat.out$zwei,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevnh(-2, z$zwei, z$sw.vec, z$oedema.vec)),#
       prevnh.L(-2,mat.out$zlen,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr), function(z) prevnh.L(-2, z$zlen, z$sw.vec)),#
       prevnh(-2,mat.out$zwfl,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevnh(-2, z$zwfl, z$sw.vec, z$oedema.vec)),#
       prevnh(-2,mat.out$zbmi,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevnh(-2, z$zbmi, z$sw.vec, z$oedema.vec))))[,-1])

#### % > +1 SD for weight-for-length/height and bmi-for-age
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       array(rep(NA,28),dim=c(4,7)),#
       array(rep(NA,28),dim=c(4,7)),#
       prevph(1,mat.out$zwfl,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevph(1, z$zwfl, z$sw.vec, z$oedema.vec)),#
       prevph(1,mat.out$zbmi,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevph(1, z$zbmi, z$sw.vec, z$oedema.vec))))[,-1])

#### % > +2 SD for weight-for-length/height and bmi-for-age
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       array(rep(NA,28),dim=c(4,7)),#
       array(rep(NA,28),dim=c(4,7)),#
       prevph(2,mat.out$zwfl,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevph(2, z$zwfl, z$sw.vec, z$oedema.vec)),#
       prevph(2,mat.out$zbmi,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevph(2, z$zbmi, z$sw.vec, z$oedema.vec))))[,-1])

#### % > +3 SD for weight-for-length/height and bmi-for-age
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       array(rep(NA,28),dim=c(4,7)),#
       array(rep(NA,28),dim=c(4,7)),#
       prevph(3,mat.out$zwfl,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevph(3, z$zwfl, z$sw.vec, z$oedema.vec)),#
       prevph(3,mat.out$zbmi,mat.out$sw.vec,mat.out$oedema.vec),lapply(split(mat.out, mat.out$agegr),function(z) prevph(3, z$zbmi, z$sw.vec, z$oedema.vec))))[,-1])

#### Means of z-scores for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       wmean(mat.out$zwei,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr),function(z) wmean(z$zwei,z$sw.vec)),#
       wmean(mat.out$zlen,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr),function(z) wmean(z$zlen,z$sw.vec)),#
       wmean(mat.out$zwfl,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr),function(z) wmean(z$zwfl,z$sw.vec)),#
       wmean(mat.out$zbmi,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr),function(z) wmean(z$zbmi,z$sw.vec)))))

#### Standard deviations of z-scores for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       wsd(mat.out$zwei,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr),function(z) wsd(z$zwei,z$sw.vec)),#
       wsd(mat.out$zlen,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr),function(z) wsd(z$zlen,z$sw.vec)),#
       wsd(mat.out$zwfl,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr),function(z) wsd(z$zwfl,z$sw.vec)),#
       wsd(mat.out$zbmi,mat.out$sw.vec),lapply(split(mat.out, mat.out$agegr),function(z) wsd(z$zbmi,z$sw.vec)))))


####################################################################################################################
##### Exporting matrix to Excel file

mat[1:14,8]<-mat[1:14,17]
mat[1:14,9]<-mat[1:14,18]
mat[1:14,10:18]<-array(rep("",126),dim=c(14,9))
rm(mat1)
mat1<-rbind.data.frame(c("Set 1:","Sexes","combined",rep("",15)),c("Weight","-for-","age",rep("",15)),
                       c("N","% < -3 SD","95%", "C.I.","% < -2 SD","95%", "C.I.","Mean","SD","","", "","","", "","","", ""),mat[1:7,],#
                       c("Length","/height","-for-","age",rep("",14)),
                       c("N","% < -3 SD","95%", "C.I.","% < -2 SD","95%", "C.I.","Mean","SD","","", "","","", "","","", ""),mat[8:14,],#
                       c("Weight","-for-","length","/height",rep("",14)),
                       c("N","% < -3 SD","95%", "C.I.","% < -2 SD","95%", "C.I.","% > +1 SD","95%", "C.I.","% > +2 SD","95%", "C.I.","% > +3 SD","95%", "C.I.","Mean","SD"),mat[15:21,],#
                       c("BMI","-for-","age",rep("",15)),
                       c("N","% < -3 SD","95%", "C.I.","% < -2 SD","95%", "C.I.","% > +1 SD","95%", "C.I.","% > +2 SD","95%", "C.I.","% > +3 SD","95%", "C.I.","Mean","SD"),mat[22:28,])

for(j in 1:dim(mat1)[2]) mat1[,j]<-ifelse(mat1[,j]=="NA" | mat1[,j]=="NaN","",mat1[,j])

mat1<-cbind.data.frame(c("",rep(c(""," Age"," 0-60"," 0-5"," 6-11"," 12-23"," 24-35"," 36-47"," 48-60"),4)),mat1)

	
####################################################################################################################

##### For boys and girls

for(i in 1:2) {

mat.out.sex<-mat.out[!is.na(mat.out$sex.vec) & mat.out$sex.vec==i,]	

mat.aux<-as.data.frame(cbind(array(rep(NA,((dim(mat.out.sex)[2]-1)*6)),dim=c(6,(dim(mat.out.sex)[2]-1)) ),c(0,6,12,24,36,48)))
names(mat.aux)<-names(mat.out.sex)
mat.out.sex<-rbind.data.frame(mat.out.sex,mat.aux )
mat.out.sex$oedema.vec<-ifelse((!is.na(mat.out.sex$age.days) & mat.out.sex$age.days/30.4375>=61) | mat.out.sex$oedema.vec=="NA","n",mat.out.sex$oedema.vec)

#### % < -3 SD for all the indicators
mat<-t(cbind.data.frame(#
       prevnh(-3,mat.out.sex$zwei,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevnh(-3, z$zwei, z$sw.vec, z$oedema.vec)),#
       prevnh.L(-3,mat.out.sex$zlen,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr), function(z) prevnh.L(-3, z$zlen, z$sw.vec)),#
       prevnh(-3,mat.out.sex$zwfl,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevnh(-3, z$zwfl, z$sw.vec, z$oedema.vec)),#
       prevnh(-3,mat.out.sex$zbmi,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevnh(-3, z$zbmi, z$sw.vec, z$oedema.vec))))

#### % < -2 SD for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       prevnh(-2,mat.out.sex$zwei,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevnh(-2, z$zwei, z$sw.vec, z$oedema.vec)),#
       prevnh.L(-2,mat.out.sex$zlen,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr), function(z) prevnh.L(-2, z$zlen, z$sw.vec)),#
       prevnh(-2,mat.out.sex$zwfl,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevnh(-2, z$zwfl, z$sw.vec, z$oedema.vec)),#
       prevnh(-2,mat.out.sex$zbmi,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevnh(-2, z$zbmi, z$sw.vec, z$oedema.vec))))[,-1])

#### % > +1 SD for weight-for-length/height and bmi-for-age
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       array(rep(NA,28),dim=c(4,7)),#
       array(rep(NA,28),dim=c(4,7)),#
       prevph(1,mat.out.sex$zwfl,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevph(1, z$zwfl, z$sw.vec, z$oedema.vec)),#
       prevph(1,mat.out.sex$zbmi,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevph(1, z$zbmi, z$sw.vec, z$oedema.vec))))[,-1])

#### % > +2 SD for weight-for-length/height and bmi-for-age
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       array(rep(NA,28),dim=c(4,7)),#
       array(rep(NA,28),dim=c(4,7)),#
       prevph(2,mat.out.sex$zwfl,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevph(2, z$zwfl, z$sw.vec, z$oedema.vec)),#
       prevph(2,mat.out.sex$zbmi,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevph(2, z$zbmi, z$sw.vec, z$oedema.vec))))[,-1])

#### % > +3 SD for weight-for-length/height and bmi-for-age
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       array(rep(NA,28),dim=c(4,7)),#
       array(rep(NA,28),dim=c(4,7)),#
       prevph(3,mat.out.sex$zwfl,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevph(3, z$zwfl, z$sw.vec, z$oedema.vec)),#
       prevph(3,mat.out.sex$zbmi,mat.out.sex$sw.vec,mat.out.sex$oedema.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) prevph(3, z$zbmi, z$sw.vec, z$oedema.vec))))[,-1])

#### Means of z-scores for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       wmean(mat.out.sex$zwei,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) wmean(z$zwei,z$sw.vec)),#
       wmean(mat.out.sex$zlen,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) wmean(z$zlen,z$sw.vec)),#
       wmean(mat.out.sex$zwfl,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) wmean(z$zwfl,z$sw.vec)),#
       wmean(mat.out.sex$zbmi,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) wmean(z$zbmi,z$sw.vec)))))

#### Standard deviations of z-scores for all the indicators
mat<-cbind.data.frame(mat,t(cbind.data.frame(#
       wsd(mat.out.sex$zwei,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) wsd(z$zwei,z$sw.vec)),#
       wsd(mat.out.sex$zlen,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) wsd(z$zlen,z$sw.vec)),#
       wsd(mat.out.sex$zwfl,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) wsd(z$zwfl,z$sw.vec)),#
       wsd(mat.out.sex$zbmi,mat.out.sex$sw.vec),lapply(split(mat.out.sex, mat.out.sex$agegr),function(z) wsd(z$zbmi,z$sw.vec)))))

####################################################################################################################
##### Exporting matrix to Excel file

mat[1:14,8]<-mat[1:14,17]
mat[1:14,9]<-mat[1:14,18]
mat[1:14,10:18]<-array(rep("",126),dim=c(14,9))


mat2<-rbind.data.frame(c(paste("Set ",i+1,":",sep=""),c("Males","Females")[i],rep("",16)),c("Weight","-for-","age",rep("",15)),#
                       c("N","% < -3 SD","95%", "C.I.","% < -2 SD","95%", "C.I.","Mean","SD","","", "","","", "","","", ""),mat[1:7,],#
                       c("Length","/height","-for-","age",rep("",14)),
                       c("N","% < -3 SD","95%", "C.I.","% < -2 SD","95%", "C.I.","Mean","SD","","", "","","", "","","", ""),mat[8:14,],#
                       c("Weight","-for-","length","/height",rep("",14)),
                       c("N","% < -3 SD","95%", "C.I.","% < -2 SD","95%", "C.I.","% > +1 SD","95%", "C.I.","% > +2 SD","95%", "C.I.","% > +3 SD","95%", "C.I.","Mean","SD"),mat[15:21,],#
                       c("BMI","-for-","age",rep("",15)),
                       c("N","% < -3 SD","95%", "C.I.","% < -2 SD","95%", "C.I.","% > +1 SD","95%", "C.I.","% > +2 SD","95%", "C.I.","% > +3 SD","95%", "C.I.","Mean","SD"),mat[22:28,])

for(j in 1:dim(mat2)[2]) mat2[,j]<-ifelse(mat2[,j]=="NA" | mat2[,j]=="NaN","",mat2[,j])

mat2<-cbind.data.frame(c("",rep(c("","Age"," 0-60"," 0-5"," 6-11"," 12-23"," 24-35"," 36-47"," 48-60"),4)),mat2)

names(mat2)<-names(mat1)
	
mat1<-rbind(mat1,mat2)

}    #### End of loop for sex

###################################################################################################################
######### Export table with prevalence values and their confidence intervals, and mean and SD of the z-scores
###################################################################################################################

assign("matprev",mat1,envir = .GlobalEnv)

write.table(matprev, file=paste(FilePath,"\\",FileLab,"_prev_rc.csv",sep=""),na=" ",row.names = FALSE,col.names=F,sep=",",quote = TRUE)

cat(paste("Prevalences and z-score summary statistics calculated and exported to ",FilePath,"\\",FileLab,"_prev_rc.csv\n",sep="")) 

on.exit(options(old))

invisible()


}   #### End of function igrowup.standard


