#code to look at summary figures of YOY's, "recruits" in the same figure


rm(list=ls());

setwd("U:/UF postdoc/post doc projects/Red drum synthesis project/2018/processed data/standardized catches/FL FIM/Annual all YOYs"); 


#########################################################################
### DATA SECTION ###
#########################################################################

### YOYs
apm_yoy <- read.csv("apm_annual_all_yoys_newrec.csv", stringsAsFactors=FALSE); apm_yoy$site <- "Apalachicola"; colnames(apm_yoy) <- c("year", "mean", "var", "site"); 
ckm_yoy <- read.csv("ckm_annual_all_yoys_newrec.csv", stringsAsFactors=FALSE); ckm_yoy$site <- "Cedar Key"; colnames(ckm_yoy) <- c("year", "mean", "var", "site"); 
tbm_yoy <- read.csv("tbm_annual_all_yoys_newrec.csv", stringsAsFactors=FALSE); tbm_yoy$site <- "Tampa Bay"; colnames(tbm_yoy) <- c("year", "mean", "var", "site"); 
chm_yoy <- read.csv("chm_annual_all_yoys_newrec.csv", stringsAsFactors=FALSE); chm_yoy$site <- "Charlotte Harbor";	colnames(chm_yoy) <- c("year", "mean", "var", "site"); 

### Recs
setwd("U:/UF postdoc/post doc projects/Red drum synthesis project/2018/processed data/standardized catches/FL FIM/Annual recs"); 
apm_rec <- read.csv("apm_annual_recs_newrec.csv", stringsAsFactors=FALSE); apm_rec$site <- "Apalachicola"; colnames(apm_rec) <- c("year", "mean", "var", "site"); 
ckm_rec <- read.csv("ckm_annual_recs_newrec.csv", stringsAsFactors=FALSE); ckm_rec$site <- "Cedar Key"; colnames(ckm_rec) <- c("year", "mean", "var", "site"); 
tbm_rec <- read.csv("tbm_annual_recs_newrec.csv", stringsAsFactors=FALSE); tbm_rec$site <- "Tampa Bay"; colnames(tbm_rec) <- c("year", "mean", "var", "site"); 
chm_rec <- read.csv("chm_annual_recs_newrec.csv", stringsAsFactors=FALSE); chm_rec$site <- "Charlotte Harbor"; colnames(chm_rec) <- c("year", "mean", "var", "site"); 

head(chm_rec)

#########################################################################
### PLOTTING SECTION ###
#########################################################################

### Transparency function
col2rgbA<-function(color,transparency)
{
  rgb(t(col2rgb(color))/255,alpha=transparency)
}
#col2rgbA("gray", .5)


### Plotting function
fun <- function(yoydf, recdf) 
	{
	x=min(yoydf$year):max(yoydf$year)
	y=yoydf$mean/mean(yoydf$mean)
	ly=y-sqrt(yoydf$var)*1.96
	uy=y+sqrt(yoydf$var)*1.96

	x1=min(recdf$year):max(recdf$year)
	y1=recdf$mean/mean(recdf$mean)
	ly1=y1-sqrt(recdf$var)*1.96
	uy1=y1+sqrt(recdf$var)*1.96

	ymax <- 1.25*max(c(y,y1))
	ymin <- .90*min(c(y,y1))

	matplot(x=x,y=cbind(y,ly,uy),type="n",
		xlab="Year born",ylab="Relative Population Trend",col="black", ylim=c(ymin,ymax))
	polygon(x=c(x,rev(x)),y=c(ly,rev(uy)),col=col2rgbA("lightblue", .5),border = "lightblue")
	points(x,y,type="l",lwd=2,col="dodgerblue3")

	polygon(x=c(x1,rev(x1)),y=c(ly1,rev(uy1)),col=col2rgbA("lightpink", .5),border = "lightpink")
	points(x1,y1,type="l",lwd=2,col="pink")
	abline(h=1,lty=2,col="red")
	legend("topright", legend=c("YOY's", "Recruits"), col=c(col2rgbA("lightblue", .5), col2rgbA("lightpink", .5)), lwd=c(8,8), bty="n")
	text(median(x), 0.95*ymax, yoydf$site[1], cex=2)
	}


### Single Plot
par(mfrow=c(1,1))

	pdf(file=paste("U:/UF postdoc/post doc projects/Red drum synthesis project/2018/code/FL FIM data/working code/4. plotting standardized indices/YOYs and Recs/APM_both_newrec.pdf", sep=""), width=12, height=7)	
fun(yoydf=apm_yoy, recdf=apm_rec)
	dev.off()

	pdf(file=paste("U:/UF postdoc/post doc projects/Red drum synthesis project/2018/code/FL FIM data/working code/4. plotting standardized indices/YOYs and Recs/CKM_both_newrec.pdf", sep=""), width=12, height=7)	
fun(yoydf=ckm_yoy, recdf=ckm_rec)
	dev.off()

	pdf(file=paste("U:/UF postdoc/post doc projects/Red drum synthesis project/2018/code/FL FIM data/working code/4. plotting standardized indices/YOYs and Recs/TBM_both_newrec.pdf", sep=""), width=12, height=7)	
fun(yoydf=tbm_yoy, recdf=tbm_rec)
	dev.off()

	pdf(file=paste("U:/UF postdoc/post doc projects/Red drum synthesis project/2018/code/FL FIM data/working code/4. plotting standardized indices/YOYs and Recs/CHM_both_newrec.pdf", sep=""), width=12, height=7)	
fun(yoydf=chm_yoy, recdf=chm_rec)
	dev.off()


### Multi-site plots
yoylist <- list(apm_yoy, ckm_yoy, tbm_yoy, chm_yoy)
reclist <- list(apm_rec, ckm_rec, tbm_rec, chm_rec)

	pdf(file=paste("U:/UF postdoc/post doc projects/Red drum synthesis project/2018/code/FL FIM data/working code/4. plotting standardized indices/YOYs and Recs/all_sites_both_newrec.pdf", sep=""), width=10, height=7)	
par(mfrow=c(length(yoylist), 1), mar=c(2,2,.1,.5))

for( i in 1:length(yoylist))
	{
	fun(yoydf=data.frame(yoylist[i]), recdf=data.frame(reclist[i]))
	}
	dev.off()

### Only YOYs 
fun2 <- function(yoydf) 
	{
	x1=min(yoydf$year):max(yoydf$year)
	y1=yoydf$mean/mean(yoydf$mean)
	ly1=y1-sqrt(yoydf$var)*1.96
	uy1=y1+sqrt(yoydf$var)*1.96

	ymax <- 1.25*max(y1)
	ymin <- .90*min(y1)

	matplot(x=x1,y=cbind(y1, ly1, uy1),type="n",
		xlab="Year born",ylab="Relative Population Trend",col="black", ylim=c(ymin,ymax))
	polygon(x=c(x1,rev(x1)),y=c(ly1, rev(uy1)), col=col2rgbA("lightblue", .5), border = "lightblue")
	points(x1, y1, type="l", lwd=2, col="dodgerblue3")
	abline(h=1,lty=2,col="red")
	legend("topright", legend=c("YOYs"), col=c(col2rgbA("lightblue", .5)), lwd=c(8), bty="n")
	text(median(x1), 0.95*ymax, yoydf$site[1], cex=2)
	}

	pdf(file=paste("U:/UF postdoc/post doc projects/Red drum synthesis project/2018/code/FL FIM data/working code/4. plotting standardized indices/YOYs and Recs/all_sites_yoys_newrec.pdf", sep=""), width=10, height=7)	
    par(mfrow=c(length(reclist), 1), mar=c(2,4,.1,.5))

for( i in 1:length(reclist))
	{
	fun2(yoydf=data.frame(yoylist[i]))
	}
	dev.off()

### Only recruits 
fun3 <- function(recdf) 
	{
	x1=min(recdf$year):max(recdf$year)
	y1=recdf$mean/mean(recdf$mean)
	ly1=y1-sqrt(recdf$var)*1.96
	uy1=y1+sqrt(recdf$var)*1.96

	ymax <- 1.25*max(y1)
	ymin <- .90*min(y1)

	matplot(x=x1,y=cbind(y1, ly1, uy1),type="n",
		xlab="Year born",ylab="Relative Population Trend",col="black", ylim=c(ymin,ymax))
	polygon(x=c(x1,rev(x1)),y=c(ly1, rev(uy1)), col=col2rgbA("lightpink", .5), border = "lightblue")
	points(x1, y1, type="l", lwd=2, col="pink")
	abline(h=1,lty=2,col="red")
	legend("topright", legend=c("Recruits"), col=c(col2rgbA("lightpink", .5)), lwd=c(8), bty="n")
	text(median(x1), 0.95*ymax, recdf$site[1], cex=2)
	}

	pdf(file=paste("U:/UF postdoc/post doc projects/Red drum synthesis project/2018/code/FL FIM data/working code/4. plotting standardized indices/YOYs and Recs/all_sites_recs_newrec.pdf", sep=""), width=10, height=7)	

par(mfrow=c(length(reclist), 1), mar=c(2,4,.1,.5))

for( i in 1:length(reclist))
	{
	fun3(recdf=data.frame(reclist[i]))
	}
	dev.off()



# #APM
# par(mfrow=c(1,1))

# x=min(apm_yoy$year):max(apm_yoy$year)
# y=apm_yoy$mean/mean(apm_yoy$mean)
# ly=y-sqrt(apm_yoy$var)*1.96
# uy=y+sqrt(apm_yoy$var)*1.96

# x1=min(apm_rec$year):max(apm_rec$year)
# y1=apm_rec$mean/mean(apm_rec$mean)
# ly1=y1-sqrt(apm_rec$var)*1.96
# uy1=y1+sqrt(apm_rec$var)*1.96

# ymax <- 1.25*max(c(y,y1))
# ymin <- .90*min(c(y,y1))

# matplot(x=x,y=cbind(y,ly,uy),type="n", main="Apalachicola",
# 	xlab="Year born",ylab="Relative Population Trend",col="black", ylim=c(ymin,ymax))
# polygon(x=c(x,rev(x)),y=c(ly,rev(uy)),col=col2rgbA("lightblue", .5),border = "lightblue")
# points(x,y,type="l",lwd=2,col="dodgerblue3")

# polygon(x=c(x1,rev(x1)),y=c(ly1,rev(uy1)),col=col2rgbA("lightpink", .5),border = "lightpink")
# points(x1,y1,type="l",lwd=2,col="pink")
# abline(h=1,lty=2,col="red")
# legend("topright", legend=c("YOY's", "Recruits"), col=c(col2rgbA("lightblue", .5), col2rgbA("lightpink", .5)), lwd=c(8,8), bty="n")


# #CKM
# par(mfrow=c(1,1))

# x=min(ckm_yoy$year):max(ckm_yoy$year)
# y=ckm_yoy$mean/mean(ckm_yoy$mean)
# ly=y-sqrt(ckm_yoy$var)*1.96
# uy=y+sqrt(ckm_yoy$var)*1.96

# x1=min(ckm_rec$year):max(ckm_rec$year)
# y1=ckm_rec$mean/mean(ckm_rec$mean)
# ly1=y1-sqrt(ckm_rec$var)*1.96
# uy1=y1+sqrt(ckm_rec$var)*1.96

# ymax <- 1.25*max(c(y,y1))
# ymin <- .90*min(c(y,y1))

# matplot(x=x,y=cbind(y,ly,uy),type="n", main="Cedar Key",
# 	xlab="Year born",ylab="Relative Population Trend",col="black", ylim=c(ymin,ymax))
# polygon(x=c(x,rev(x)),y=c(ly,rev(uy)),col=col2rgbA("lightblue", .5),border = "lightblue")
# points(x,y,type="l",lwd=2,col="dodgerblue3")

# polygon(x=c(x1,rev(x1)),y=c(ly1,rev(uy1)),col=col2rgbA("lightpink", .5),border = "lightpink")
# points(x1,y1,type="l",lwd=2,col="pink")
# abline(h=1,lty=2,col="red")
# legend("topright", legend=c("YOY's", "Recruits"), col=c(col2rgbA("lightblue", .5), col2rgbA("lightpink", .5)), lwd=c(8,8), bty="n")


# #TBM
# par(mfrow=c(1,1))

# x=min(tbm_yoy$year):max(tbm_yoy$year)
# y=tbm_yoy$mean/mean(tbm_yoy$mean)
# ly=y-sqrt(tbm_yoy$var)*1.96
# uy=y+sqrt(tbm_yoy$var)*1.96

# x1=min(tbm_rec$year):max(tbm_rec$year)
# y1=tbm_rec$mean/mean(tbm_rec$mean)
# ly1=y1-sqrt(tbm_rec$var)*1.96
# uy1=y1+sqrt(tbm_rec$var)*1.96

# ymax <- 1.25*max(c(y,y1))
# ymin <- .90*min(c(y,y1))

# matplot(x=x,y=cbind(y,ly,uy),type="n", main="Tampa Bay",
# 	xlab="Year born",ylab="Relative Population Trend",col="black", ylim=c(ymin,ymax))
# polygon(x=c(x,rev(x)),y=c(ly,rev(uy)),col=col2rgbA("lightblue", .5),border = "lightblue")
# points(x,y,type="l",lwd=2,col="dodgerblue3")

# polygon(x=c(x1,rev(x1)),y=c(ly1,rev(uy1)),col=col2rgbA("lightpink", .5),border = "lightpink")
# points(x1,y1,type="l",lwd=2,col="pink")
# abline(h=1,lty=2,col="red")
# legend("topright", legend=c("YOY's", "Recruits"), col=c(col2rgbA("lightblue", .5), col2rgbA("lightpink", .5)), lwd=c(8,8), bty="n")


# #CHM
# par(mfrow=c(1,1))

# x=min(chm_yoy$year):max(chm_yoy$year)
# y=chm_yoy$mean/mean(chm_yoy$mean)
# ly=y-sqrt(chm_yoy$var)*1.96
# uy=y+sqrt(chm_yoy$var)*1.96

# x1=min(chm_rec$year):max(chm_rec$year)
# y1=chm_rec$mean/mean(chm_rec$mean)
# ly1=y1-sqrt(chm_rec$var)*1.96
# uy1=y1+sqrt(chm_rec$var)*1.96

# ymax <- 1.25*max(c(y,y1))
# ymin <- .90*min(c(y,y1))

# matplot(x=x,y=cbind(y,ly,uy),type="n", main="Charlotte Harbor",
# 	xlab="Year born",ylab="Relative Population Trend",col="black", ylim=c(ymin,2.5))
# polygon(x=c(x,rev(x)),y=c(ly,rev(uy)),col=col2rgbA("lightblue", .5),border = "lightblue")
# points(x,y,type="l",lwd=2,col="dodgerblue3")

# polygon(x=c(x1,rev(x1)),y=c(ly1,rev(uy1)),col=col2rgbA("lightpink", .5),border = "lightpink")
# points(x1,y1,type="l",lwd=2,col="pink")
# abline(h=1,lty=2,col="red")
# legend("topright", legend=c("YOY's", "Recruits"), col=c(col2rgbA("lightblue", .5), col2rgbA("lightpink", .5)), lwd=c(8,8), bty="n")



