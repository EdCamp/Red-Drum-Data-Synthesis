#Code getting ready to look at correlation among sites


rm(list=ls());

require(GGally)
require(ggplot2)

setwd("U:/UF postdoc/post doc projects/Red drum synthesis project/2018/processed data/standardized catches/FL FIM/Annual all YOYs"); 


#########################################################################
### DATA SECTION ###
#########################################################################

### Creating long dataframe ##################################################
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

yoy <- rbind(apm_yoy, ckm_yoy, tbm_yoy, chm_yoy)
rec <- rbind(apm_rec, ckm_rec, tbm_rec, chm_rec)


### Creating pairwise, wide dataframe ##################################################
### Recs
#creating matching design
yearmin <- max(apm_rec$year[1], ckm_rec$year[1], tbm_rec$year[1], chm_rec$year[1])  #find the first year that exists in all samples

#creating dataframe for cor and ggcorr
recs <- cbind(apm_rec[which(apm_rec$year >= yearmin),"year"], apm_rec[which(apm_rec$year >= yearmin),"mean"], ckm_rec[which(ckm_rec$year >= yearmin),"mean"], 
		tbm_rec[which(tbm_rec$year >= yearmin),"mean"], chm_rec[which(chm_rec$year >= yearmin),"mean"])
colnames(recs) <- c("year", "Apalachicola", "Cedar Key", "Tampa Bay", "Charlotte Harbor")
recs <- data.frame(recs)
recs
str(recs)
recs$year <- as.factor(as.character(recs$year))

### YOYs
#creating dataframe for cor and ggcorr
yoys <- cbind(apm_yoy[which(apm_yoy$year >= yearmin),"year"], apm_yoy[which(apm_yoy$year >= yearmin),"mean"], ckm_yoy[which(ckm_yoy$year >= yearmin),"mean"], 
		tbm_yoy[which(tbm_yoy$year >= yearmin),"mean"], chm_yoy[which(chm_yoy$year >= yearmin),"mean"])
colnames(yoys) <- c("year", "Apalachicola", "Cedar Key", "Tampa Bay", "Charlotte Harbor")
yoys <- data.frame(yoys)
yoys
str(yoys)
yoys$year <- as.factor(as.character(yoys$year))


#########################################################################
### ANALYSES SECTION ###
#########################################################################

###Simple correlation (spearman)
#Recs
corr <- cor(recs[,2:dim(recs)[2]], method="spearman")
corr
symnum (corr)

#YOYs
corr <- cor(yoys[,2:dim(yoys)[2]], method="spearman")
corr
symnum (corr)


#########################################################################
### PLOTTING SECTION ###
#########################################################################

### using ggcorr--not statistically better, just prettier
#Recs
ggcorr(recs, method=c("pairwise.complete.obs", "spearman"), digits=3, label_round=3, label=TRUE, label_alpha=FALSE
	)  	+ ggplot2::labs(title="Red drum Recruitment Correlation", subtitle="Florida Gulf", caption="Source: FWC FIM database") +
		ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size=14)) +
		ggplot2::theme(plot.subtitle=ggplot2::element_text(size=12, face="italic", color="black"))

#YOYs
ggcorr(yoys, method=c("pairwise.complete.obs", "spearman"), digits=3, label_round=3, label=TRUE, label_alpha=FALSE
	)  	+ ggplot2::labs(title="Red drum YOY Correlation", subtitle="Florida Gulf", caption="Source: FWC FIM database") +
		ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size=14)) +
		ggplot2::theme(plot.subtitle=ggplot2::element_text(size=12, face="italic", color="black"))


#par(mfrow=c(1,2))
pdf(file=paste("U:/UF postdoc/post doc projects/Red drum synthesis project/2018/code/FL FIM data/working code/5. correlation analyses/simple corr/recs_newrec.pdf", sep=""), width=6, height=6)	
ggcorr(recs, method=c("pairwise.complete.obs", "spearman"), digits=3, label_round=3, label=TRUE, label_alpha=FALSE
	)  	+ ggplot2::labs(title="Red drum Recruitment Correlation", subtitle="Florida Gulf", caption="Source: FWC FIM database") +
		ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size=14)) +
		ggplot2::theme(plot.subtitle=ggplot2::element_text(size=12, face="italic", color="black"))
dev.off()

#YOYs
pdf(file=paste("U:/UF postdoc/post doc projects/Red drum synthesis project/2018/code/FL FIM data/working code/5. correlation analyses/simple corr/yoys_newrec.pdf", sep=""), width=6, height=6)	
ggcorr(yoys, method=c("pairwise.complete.obs", "spearman"), digits=3, label_round=3, label=TRUE, label_alpha=FALSE
	)  	+ ggplot2::labs(title="Red drum YOY Correlation", subtitle="Florida Gulf", caption="Source: FWC FIM database") +
		ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size=14)) +
		ggplot2::theme(plot.subtitle=ggplot2::element_text(size=12, face="italic", color="black"))
dev.off()