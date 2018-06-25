rm(list=ls());

setwd("U:/UF postdoc/post doc projects/Red drum synthesis project/2018/processed data/recruitment catches/FL red drum"); 

#########################################################################
### DATA SECTION ###
#########################################################################
data <- read.csv("reddrumAPM_processed_newrec.csv", stringsAsFactors=FALSE)     #reading in the raw files output from SAS 
dat <- data[which(data$gear %in% c(20,23, 160, 300)),]                   #these are the recommended gears from FIM
dim(data); dim(dat)                                                      #check to ensure gears removed

#simply grabbing the site name for plotting
sitename <- ifelse(substr(dat$reference[1],1,3)=="APM", "Apalachicola", 
              ifelse(substr(dat$reference[1],1,3)=="CKM", "Cedar Key",
                 ifelse(substr(dat$reference[1],1,3)=="TBM", "Tampa Bay", "Charlotte Harbor")
             )
       )  

### HABITAT
#bottom
dat$bot_x <- dat$bottom     #new dummy variable for habitat
dat$bot_x[which(dat$bottom %in% c("Mud", "MudSan", "MudUnk"))] = "mud"     #if there is substantial mud, call it mud
dat$bot_x[which(dat$bottom %in% c("MudStr", "MudSanStr", "MudSanStrUnk", "SanStr", "SanStrUnk", "Str", "StrUnk"))] = "str"   #if structure is present, label is as structure.  Obviously there is substrate, but assuming the dominant characteristic from fishes perspective is the structure
dat$bot_x[which(dat$bottom =="" | dat$bottom == "Unk")] = "unk"    #anything that for whatever reason is unlabled gets called unknown. 
dat$bot_x[which(dat$bot_x =="San")] = "san"
levels(as.factor(dat$bot_x))                 #Note, this implicitly keeps things labeled "San" as Sand, and things labeled Unk as unknown.  The results is only 4 levels of the bottom factor: Mud, Sand, Structure, and Unknown

#bottom vegetation (bveg)--breaking this ino simply sav and non-sav.  Could later break out with alg if you wanted...but not sure what that means (e.g., brown structured algae vs filamentous algae)
dat$bveg_x <- dat$bveg
dat$bveg_x[which(dat$bveg %in% c("SAV", "SAVAlg", "SAVNon", "Alg"))] = "sav"     #if there is SAV, call is SAV
dat$bveg_x[which(dat$bveg %in% c("Non", ""))] = "non"                     # very few "", so just assuming there was no SAV or algae there
levels(as.factor(dat$bveg_x))   #this implicitly leaves Alg as Alg, but may need to combine Alg with SAV owing to number of 

#shore     <this classification ripped from Dave's DeltaLogNorm code>
dat$shore_max = max.col(dat[,c('man','eme','ter','str')],ties.method='first')
dat$shore_x   = ifelse(dat$shore_max==1,'mang',ifelse(dat$shore_max==2,'emer',ifelse(dat$shore_max==3,'terr','strc')))
levels(as.factor(dat$shore_x))

### Class and log transformations
dat$reference <- as.factor(dat$reference)
dat$gear <- as.factor(dat$gear)
dat$zone <- as.factor(dat$zone)
dat$month <- as.factor(dat$month)
dat$year <- as.factor(dat$year)
dat$bot_x <- as.factor(dat$bot_x)
dat$bveg_x <- as.factor(dat$bveg_x)
dat$shore_x <- as.factor(dat$shore_x)
dat$temp <-ifelse(dat$temperature > 0, log(dat$temperature), dat$temperature)
dat$sal <-ifelse(dat$salinity > 0, log(dat$salinity), dat$salinity)
dat$yr_yoy <- as.factor(dat$yr_yoy)
dat$yoy_cpue <- dat$yoy/dat$effort                                       #adding CPUE

#subsetting out for only the columns you think you'll use
dat1 <- dat[,c("gear", "effort", "zone", "month", "tide", "yoy_cpue", "year", "yr_yoy", "bot_x", "bveg_x", "shore_x", "temp", "sal")]

#dropping incomplete cases (samples with NA's in them)
d <- dat1[complete.cases(dat1),]

### DATA CHECKS 
#checks on the balance of the design
#Year--actual calendar year of sample
with(d, tapply(yoy_cpue, list(year, month), sum))   #Adequate samples in years 1998:2015

#yr_yoy--back-assigned fish caught in first  have of year x to previous year (jan:aug 2015 catch was born in 2014)
with(d, tapply(yoy_cpue, list(yr_yoy, month), sum))   #Remember, yoy caught in Jan:Jul 2015 get classed as 2014, so while plenty of fish caught in 2015, only the ones after august get counted for that year.
with(d, tapply(yoy_cpue, list(yr_yoy, zone), sum))    #Pretty balanced for all zones
with(d, tapply(yoy_cpue, list(yr_yoy, bot_x), sum))   #Unknown is spotty, but currently not standardizing by this.
with(d, tapply(yoy_cpue, list(yr_yoy, bveg_x), sum))  #Well-balanced
with(d, tapply(yoy_cpue, list(yr_yoy, shore_x), sum)) #Well-balanced
with(d, tapply(yoy_cpue, list(yr_yoy, gear), sum))    #gears are good

#editing data according to above
d <- d[which(d$zone %in% c("A", "B", "C")),]
d$zone <- as.factor(as.character(d$zone))
levels(d$zone)

d <- d[which(!d$yr_yoy =="1997"),]
d$yr_yoy <- as.factor(as.character(d$yr_yoy))
levels(d$yr_yoy)

d <- d[which(!d$month %in% c("7", "8")),]
d$month <- as.factor(as.character(d$month))
levels(d$month)


### SPLITTING DATA FOR DELTA LOG NORMAL
#Splitting data for delta log normal
posdata <- d[which(d$yoy_cpue>0),]                      #these are the counts of the positive samples
padata <- d                                           #replicate data set
padata$yoy_cpue[which(padata$yoy_cpue>0)]=1           #this for the binomial, replace positives with 1 to make binomial catch


###################################################################
### MODEL ###
###################################################################
#this is basically a delta log normal model
binom.glm.res=glm(yoy_cpue ~ zone + yr_yoy + month + gear, data=padata,family="binomial")   #positive catch, yes or no
ln.glm.res=glm(log(yoy_cpue) ~ zone + yr_yoy + month + gear, data=posdata)                  #log normal catch...which seems like another distribution could have been used.
summary(ln.glm.res)

#set up grid for full prediction
tmp=expand.grid(zone=levels(padata$zone), yr_yoy=levels(padata$yr_yoy), month=levels(padata$month), gear=levels(padata$gear))

new.binom<-predict(binom.glm.res,newdata=tmp,type="response",se.fit=TRUE)
new.ln<-predict(ln.glm.res,newdata=tmp,type="response",se.fit=TRUE)

both=new.binom$fit*exp(new.ln$fit)
bothvar=new.binom$se.fit^2*new.ln$se.fit^2+new.binom$se.fit^2*mean(new.ln$fit)^2+new.ln$se.fit^2*mean(new.binom$fit)^2   #bias correction for variance?

new=cbind(tmp,both)
newvar=cbind(tmp,bothvar)

meancpue=aggregate(both ~ yr_yoy, data = new, mean)
varcpue=aggregate(bothvar ~ yr_yoy, data = newvar, mean)

#creates a list of years
years <- as.numeric(levels(d$yr_yoy))

par(mfrow=c(1,2))
plot(years, meancpue$both, type="b", ylim=c(0, .5), ylab="standardized cpue (raw)", xlab="year", main=sitename)
plot(years, meancpue$both/mean(meancpue$both), type="b", ylim=c(0, 2.0), ylab="standardized cpue (rel)", xlab="year", main=sitename)


par(mfrow=c(1,1))

x=min(years):max(years)
y=meancpue[,2]/mean(meancpue[,2])
ly=y-sqrt(varcpue[,2])*1.96
uy=y+sqrt(varcpue[,2])*1.96
matplot(x=x,y=cbind(y,ly,uy),type="n", main=sitename,
	xlab="Year",ylab="Relative Population Trend",col="black")
polygon(x=c(x,rev(x)),y=c(ly,rev(uy)),col="lightgrey",border = "lightgrey")
points(x,y,type="l",lwd=2,col="dodgerblue3")
abline(h=1,lty=2,col="red")

#creating site-specific file for outputting
apm_yrly <-cbind(meancpue, varcpue$bothvar)
head(apm_yrly)

setwd("U:/UF postdoc/post doc projects/Red drum synthesis project/2018/processed data/standardized catches/FL FIM/Annual all YOYs");
write.csv(apm_yrly, file = "apm_annual_all_yoys_newrec.csv", row.names=FALSE)







################################################################################
### Checks ###
################################################################################
# # Checking to precit with raw input data to see if results are reasonable. 
# rawtmp <- data.frame(padata$zone, padata$yr_yoy, padata$month, padata$gear)
# colnames(rawtmp) = c("zone", "yr_yoy", "month", "gear")
# head(rawtmp)

# raw.binom<-predict(binom.glm.res,newdata=rawtmp,type="response",se.fit=TRUE)
# raw.ln<-predict(ln.glm.res,newdata=rawtmp,type="response",se.fit=TRUE)

# bothraw=raw.binom$fit*exp(raw.ln$fit)
# bothvarraw=raw.binom$se.fit^2*raw.ln$se.fit^2+raw.binom$se.fit^2*mean(raw.ln$fit)^2+raw.ln$se.fit^2*mean(raw.binom$fit)^2   #bias correction for variance?

# hist(bothraw, breaks=10)
# hist(both, breaks=10)

# raw=cbind(rawtmp,bothraw)
# rawvar=cbind(rawtmp,bothvarraw)

# meancpueraw=aggregate(bothraw ~ yr_yoy, data = raw, mean)
# varcpueraw=aggregate(bothvarraw ~ yr_yoy, data = rawvar, mean)

# par(mfrow=c(1,2))
# plot(as.numeric(as.character(meancpue$yr_yoy)), meancpue$both, type="b", ylim=c(0, .5), ylab="standardized cpue (raw)", xlab="year", main="Exanded grid")
# plot(as.numeric(as.character(meancpueraw$yr_yoy)), meancpueraw$bothraw, type="b", ylim=c(0, .5), ylab="standardized cpue (raw)", xlab="year", main="raw data")

# mean(d$yoy_cpue)   #actual data
# mean(raw$bothraw)  #using raw samples to predict from model
# mean(new$both)     #using expanded grid to predict from model

