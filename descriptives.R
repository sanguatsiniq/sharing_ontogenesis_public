### Descriptive stats of sample ###
# "sharing-ontogenesis paper"
# Elspeth Ready

### Set environment ###

library(this.path)
setwd(this.path::here())

source("build_networks.R")

# functions
get_summary_stats <- function(subsample) {
  mu <- mean(subsample, na.rm=TRUE)
  std <- sd(subsample, na.rm=TRUE)
  max_ties <- max(subsample, na.rm=TRUE)
  zeroes <- sum(subsample==0, na.rm=TRUE)/sum(!(is.na(subsample)))
  N <- length(subsample)
  Obs_count <- sum(!(is.na(subsample)))
  return(cbind.data.frame(mu, std, max_ties, zeroes, N, Obs_count))
}

# make some nice colours
oos <- "black"
oos_light = "grey90"
oos_trans = rgb(0, 0, 0, 0.1)
fs2013 = rgb(0.5607843, 0.7372549, 0.5607843) #darkseagreen
fs2013_trans = rgb(0.5607843, 0.7372549, 0.5607843, 0.2)
is2013 = rgb(0.1333333, 0.5450980, 0.1333333) #forestgreen
is2013_trans = rgb(0.1333333, 0.5450980, 0.1333333, 0.2) 
fs2023 = rgb(0.8039216, 0.4117647, 0.7882353) #orchid3
fs2023_trans = rgb(0.8039216, 0.4117647, 0.7882353, 0.2)
is2023 = rgb(0.5450980, 0.2784314, 0.5372549) #orchid4
is2023_trans = rgb(0.5450980, 0.2784314, 0.5372549, 0.2) #orchid4
ts = rgb(0, 0, 0.545098) #darkblue
ts_trans = rgb(0, 0, 0.545098, 0.2)
spl =  rgb(0.961, 0.502, 0.067) # handpicked orange
spl_trans =  rgb(0.961, 0.502, 0.067, 0.2)

### Prep data ###

households2013 <- merged_data$HID_2013_occupant[which(merged_data$Temporal_type %in% c("1","0", "1 (separated)", "deceased", "moved_away", "absorbed"))]
respondents2013 <- merged_data$HID_2013_occupant[which(merged_data$Temporal_type %in% c("1", "deceased", "moved_away", "absorbed"))]

respondents2013 <- merged_data[which(merged_data$HID_2013_occupant %in% respondents2013),]
respondents2023 <- merged_data[which(merged_data$HID_2023 %in% sample2023),]
splinters_all <- merged_data[which(merged_data$Temporal_type %in% c("splinter (1)", "splinter (0)")),]
splinters_parents_sampled2013 <- merged_data[which(merged_data$Temporal_type %in% c("splinter (1)")),]
new_stems <- merged_data[which(merged_data$Temporal_type=="0" & merged_data$HID_2023<64),]
deceased <- merged_data[which(merged_data$Temporal_type=="deceased"),]
moved_away <- merged_data[which(merged_data$Temporal_type=="moved_away"),]
moved_back <- merged_data[which(merged_data$Temporal_type=="moved back"),]
stems_not_resampled <- merged_data[which(merged_data$Temporal_type=="1" & merged_data$HID_2023>64),]

list_samples <- list(respondents2013=respondents2013, 
                     respondents2023=respondents2023, 
                     splinters_all=splinters_all,
                     splinters_parents_sampled2013=splinters_parents_sampled2013,
                     deceased=deceased,
                     moved_away=moved_away,
                     moved_back=moved_back,
                     stems_not_resampled=stems_not_resampled,
                     new_stems=new_stems)

### Summarize data for subsamples ###

summary_stats <- data.frame(sample=character(),
                            mu=numeric(), std=numeric(), 
                            max_ties=numeric(), 
                            zeroes=numeric(), N=numeric(),
                            Obs_count=numeric())

for (var in c("Giving2013", "Giving2023", "Receiving2013", "Receiving2023", 
              "Giving2023_nosplint", "Receiving2023_nosplint")) {
  i=0
  for (subsample in list_samples) {
    i <- i + 1
    subsampvar <- subsample[,var]
    if (sum(!(is.na(subsampvar))>0)) {
      temp <- get_summary_stats(subsampvar)
      newline <- cbind.data.frame(sample=paste0(names(list_samples[i]), "$", var), temp)
      summary_stats <- rbind.data.frame(summary_stats, newline)  
    }
  }
}

write.csv(summary_stats, "EHBEA_summary_stats.csv")

# density plots
# pdf("receiving_2013_density.pdf", height=3, width=7, pointsize=9)
# par(mar=c(3,4,3,1))
# den1 <- density(respondents2013$Receiving2013, na.rm=TRUE, from=0, to=20, bw=0.6)
# plot(den1, ylim=c(0, 0.25), main="Receiving ties (2013)", xlab="", ylab="Density",
#      col="black")
# polygon(c(den1$x[den1$x >= 0 ], 0),
#         c(den1$y[den1$x >= 0 ], 0), col="gray90", border=0)
# lines(den1, col="grey50")
# abline(v=mean(respondents2013$Receiving2013), col="grey50")
# den3 <- density(stems_not_resampled$Receiving2013, from=0, to=20, na.rm=TRUE, bw=0.6)
# polygon(c(den3$x[den3$x >= 0 ], 0),
#         c(den3$y[den3$x >= 0 ], 0), col=is2013_trans, border=0)
# lines(den3, col=is2013)
# abline(v=mean(stems_not_resampled$Receiving2013), col=is2013)
# den2 <- density(respondents2023$Receiving2013, from=0, to=20, na.rm=TRUE, bw=0.6)
# polygon(c(den2$x[den2$x >= 0 ], 0),
#         c(den2$y[den2$x >= 0 ], 0), col=ts_trans, border=0)
# #polygon(c(den2$x[den2$x <= mean(respondents2023$Receiving2013, na.rm=TRUE)],mean(respondents2023$Receiving2013, na.rm=TRUE)),
# #        c(den2$y[den2$x <= mean(respondents2023$Receiving2013, na.rm=TRUE)],0), col=ts_trans, border=0)
# lines(den2, col=ts)
# abline(v=mean(respondents2023$Receiving2013, na.rm=TRUE), col=ts)
# #text(5, 0.2, "Sample means", adj=0)
# legend("topright", c("Full 2013 sample", "Not resampled", "Resampled"), 
#        col=c("grey50", is2013, ts), lwd=2, cex=1)
# dev.off()
# 
# pdf("giving_2013_density.pdf", height=3, width=7, pointsize=9)
# par(mar=c(3,4,3,1))
# den1 <- density(respondents2013$Giving2013, na.rm=TRUE, from=0, to=25, bw=0.8)
# plot(den1, ylim=c(0, 0.15), main="Giving ties (2013)", xlab="", 
#      main.cex=0.8)
# polygon(c(den1$x[den1$x >= 0 ], 0),
#         c(den1$y[den1$x >= 0 ], 0), col=rgb(0,0,0,alpha=0.2), border=0)
# lines(den1, col="grey50")
# abline(v=mean(respondents2013$Giving2013), col="grey50")
# den3 <- density(stems_not_resampled$Giving2013, from=0, to=25, na.rm=TRUE, bw=0.8)
# polygon(c(den3$x[den3$x >= 0 ], 0),
#         c(den3$y[den3$x >= 0 ], 0), col=is2013_trans, border=0)
# lines(den3, col=is2013)
# abline(v=mean(stems_not_resampled$Giving2013), col=is2013)
# den2 <- density(respondents2023$Giving2013, from=0, to=25, na.rm=TRUE, bw=0.8)
# polygon(c(den2$x[den2$x >= 0 ], 0),
#         c(den2$y[den2$x >= 0 ], 0), col=ts_trans, border=0)
# lines(den2, col=ts)
# abline(v=mean(respondents2023$Giving2013, na.rm=TRUE), col=ts)
# text(5.25, 0.14, "Vertical lines represent sample means", adj=0)
# legend("topright", c("Full 2013 sample", "Not resampled", "Resampled"), 
#        col=c("grey50", is2013, ts), lwd=2)
# dev.off()

pdf("sharing_2023_density.pdf", height=6, width=7, pointsize=9)
par(mar=c(3,4,3,1), mfrow=c(2,1))
den1 <- density(respondents2013$Giving2013, from=0, to=25, na.rm=TRUE, bw=0.8)
plot(den1, ylim=c(0, 0.20), main="Giving ties", xlab="", ylab="Density", 
     type="n")
polygon(c(den1$x[den1$x >= 0 ], 0),
        c(den1$y[den1$x >= 0 ], 0), col=rgb(0,0,0,0.2), border=0)
lines(den1, col="grey50")
abline(v=mean(respondents2013$Giving2013, na.rm=TRUE), col="grey50")
den2 <- density(respondents2013$Giving2023, from=0, to=25, na.rm=TRUE, bw=0.8)
polygon(c(den2$x[den2$x >= 0 ], 0),
        c(den2$y[den2$x >= 0 ], 0), col=ts_trans, border=0)
lines(den2, col=ts)
abline(v=mean(respondents2013$Giving2023, na.rm=TRUE), col=ts)
den3 <- density(splinters_all$Giving2023, from=0, to=25, na.rm=TRUE, bw=0.8)
polygon(c(den3$x[den2$x >= 0 ], 0),
        c(den3$y[den2$x >= 0 ], 0), col=spl_trans, border=0)
lines(den3, col=spl)
abline(v=mean(splinters_all$Giving2023, na.rm=TRUE), col=spl)
#text(mean(respondents2013$Giving2013)-0., 0.17, bquote(paste(mu == .(round(mean(respondents2013$Giving2013),2)))), adj=0)
#text(5.3, 0.17, bquote(paste(mu == .(round(mean(splinters_all$Giving2023),2)))), adj=0)
#text(5.3, 0.17, bquote(paste(mu == .(round(mean(respondents2013$Giving2023),2)))), adj=0)
par(mar=c(4,4,2,1))
den1 <- density(respondents2013$Receiving2013, from=0, to=25, na.rm=TRUE, bw=0.8)
plot(den1, ylim=c(0, 0.20), main="Receiving ties", xlab="", 
     ylab="Density", type="n")
mtext("Number of ties reported", side=1, line=2)
polygon(c(den1$x[den1$x >= 0 ], 0),
        c(den1$y[den1$x >= 0 ], 0), col=rgb(0,0,0,0.2), border=0)
lines(den1, col="grey50")
abline(v=mean(respondents2013$Receiving2013, na.rm=TRUE), col="grey50")
den2 <- density(respondents2013$Receiving2023, from=-0.01, to=20, na.rm=TRUE, bw=0.6)
polygon(c(den2$x[den2$x >= 0 ], 0),
        c(den2$y[den2$x >= 0 ], 0), col=ts_trans, border=0)
lines(den2, col=ts)
abline(v=mean(respondents2013$Receiving2023, na.rm=TRUE), col=ts)
den3 <- density(splinters_all$Receiving2023, from=-0.01, to=20, na.rm=TRUE, bw=0.6)
polygon(c(den3$x[den2$x >= 0 ], 0),
        c(den3$y[den2$x >= 0 ], 0), col=spl_trans, border=0)
lines(den3, col=spl)
abline(v=mean(splinters_all$Receiving2023, na.rm=TRUE), col=spl)
legend(17.5,0.07, c("2013 sample (n=109)", "2023 Resampled (n=30)", "2023 Splinters (n=26)"), 
       col=c("grey50", ts, spl), lwd=2, cex=1)
dev.off()

### Calculate difference of reobserved households ###

resampled <- respondents2023[which(respondents2023$Temporal_type==1),]
givingdiffs <- resampled$Giving2023 - resampled$Giving2013
receivingdiffs <- resampled$Receiving2023 - resampled$Receiving201

# inspect for qualitative assessment
temp_diffs <- cbind.data.frame(HID2013=respondents2023$HID_2013_occupant[which(respondents2023$Temporal_type==1)],
                               givediff=givingdiffs, 
                               receivediff=receivingdiffs)
temp_diffs[order(temp_diffs$givediff),]
temp_diffs[order(temp_diffs$receivediff),]

# plot change in ties overall with inset
pdf("compare_fullsamples.pdf", height=6, width=7, pointsize=9)
par(mfrow=c(2,1), mar=c(3,4,3,1))
den1 <- density(respondents2013$Giving2013, from=0, to=25, bw=0.8)
plot(den1, ylim=c(0, 0.15),
     xlim=c(0, 25),
     xlab="", ylab="Density", type="n",
     main="Giving 2013 vs. 2023")
polygon(c(den1$x[den1$x >= 0 ], 0),
        c(den1$y[den1$x >= 0 ], 0),
        col=rgb(0,0,0,0.2), border=0)
lines(den1, col="grey50")
abline(v=mean(respondents2013$Giving2013), col="grey50")
den2 <- density(respondents2023$Giving2023, from=0, to=25, bw=0.8)
polygon(c(den2$x[den2$x >= 0 ], 0),
        c(den2$y[den2$x >= 0 ], 0),
        col=is2023_trans, border=0)
lines(den2, col=is2023)
abline(v=mean(respondents2023$Giving2023), col=is2023)
gdiff <- mean(respondents2023$Giving2023)-mean(respondents2013$Giving2013)
text(5.3, 0.11, bquote(paste(mu == .(round(gdiff,2)))), adj=0)
arrows(mean(respondents2013$Giving2013), 0.11, mean(respondents2023$Giving2023), 0.11, length = 0.05, code = 3)
#receiving
den1 <- density(respondents2013$Receiving2013, from=0, to=25, bw=0.6)
plot(den1, ylim=c(0, 0.20), xlim=c(0, 25),
     xlab="", ylab="Density", type="n", main="Receiving 2013 vs. 2023")
mtext("Number of ties reported", side=1, line=2)
polygon(c(den1$x[den1$x >= 0 ], 0),
        c(den1$y[den1$x >= 0 ], 0),
        col=rgb(0,0,0,0.2), border=0)
lines(den1, col="grey50")
abline(v=mean(respondents2013$Receiving2013), col="grey50")
den2 <- density(respondents2023$Receiving2023, from=0, to=25, bw=0.6)
polygon(c(den2$x[den2$x >= 0 ], 0),
        c(den2$y[den2$x >= 0 ], 0),
        col=is2023_trans, border=0)
lines(den2, col=is2023)
abline(v=mean(respondents2023$Receiving2023), col=is2023)
rdiff <- mean(respondents2023$Receiving2023)-mean(respondents2013$Receiving2013)
text(4.2, 0.15, bquote(paste(mu == .(round(rdiff,2)))), adj=0)
arrows(mean(respondents2013$Receiving2013), 0.15, 
       mean(respondents2023$Receiving2023), 0.15, length = 0.05, code = 3)
legend(18.2, 0.06, c("2013 (n=109)", "2023 (n=64)", "2023 Resampled (n=30)"), col=c("grey50", is2023, ts), 
       lwd=2, cex=0.9)
#inset plot
par(fig = c(0.55,1,0.75,1), mar=c(0,0,4,2), new=TRUE, cex.axis=0.8)
den1 <- density(givingdiffs, from=-5 , to=10.1, bw=0.6)
plot(den1, ylim=c(0, 0.25), xlab="", yaxt="n", adj=0, line=0.2, cex.main=0.9,
     xlim=c(-5,10), main="Within-household differences", ylab="", type="n")
polygon(c(den1$x[which(den1$x >= -5 & den1$x <= 10)], rev(den1$x[which(den1$x >= -5 & den1$x <= 10)])),
        c(den1$y[which(den1$x >= -5 & den1$x <= 10)], rep(0, length(den1$x[which(den1$x >= -5 & den1$x <= 10)]))), 
        col=ts_trans, border=0)
lines(den1$x[which(den1$x >= -5 & den1$x <= 10)],
      den1$y[which(den1$x >= -5 & den1$x <= 10)], col=ts)
abline(v=mean(givingdiffs), col=ts)
text(2, 0.20, bquote(paste(mu == .(mean(givingdiffs)))), adj=0)
#text(1.6, 0.03, "Change in resmpled households", adj=0, cex=0.9)
par(fig = c(0.55,1,0.25,0.5), new=TRUE)
den1 <- density(receivingdiffs, from=-5, to=10.1, bw=0.6)
plot(den2, ylim=c(0, 0.30), xlim=c(-5,10), yaxt="n",  adj=0, line=0.2, cex.main=0.9,
     main="Within-household differences", ylab="", type="n",xlab="")
polygon(c(den1$x[which(den1$x >= -5 & den1$x <= 10)], rev(den1$x[which(den1$x >= -5 & den1$x <= 10)])),
        c(den1$y[which(den1$x >= -5 & den1$x <= 10)], rep(0, length(den1$x[which(den1$x >= -5 & den1$x <= 10)]))), 
        col=ts_trans, border=0)
lines(den1$x[which(den1$x >= -5 & den1$x <= 10)],
      den1$y[which(den1$x >= -5 & den1$x <= 10)], col=ts)
abline(v=mean(receivingdiffs), col=ts)
text(2.75, 0.22, bquote(paste(mu == .(round(mean(receivingdiffs),2)))), adj=0)
#text(1.6, 0.03, "Change in resampled households", adj=0, cex=0.9)
dev.off()


### Control for number of households ###

resamp_givingdiffs <- resampled$Giving2013-resampled$Giving2023_nosplint
resamp_receivingdiffs <- resampled$Receiving2013-resampled$Receiving2023_nosplint

pdf("downsampled_sharing.pdf", height=6, width=7, pointsize=9)
par(mfrow=c(2,1), mar=c(3,4,3,1))
den1 <- density(respondents2013$Giving2013, from=0, to=20, bw=0.6)
plot(den1, ylim=c(0, 0.20), xlim=c(0, 20),
     xlab="", ylab="Density", type="n", main="Sample-adjusted giving")
polygon(c(den1$x[den1$x >= 0 ], 0),
        c(den1$y[den1$x >= 0 ], 0),
        col=rgb(0,0,0,0.2), border=0)
lines(den1, col="grey50")
abline(v=mean(respondents2013$Giving2013), col="grey50")
den4 <- density(respondents2023$Giving2023_nosplint, from=0, to=25, bw=0.6)
polygon(c(den4$x[den4$x >= 0 ], 0),
        c(den4$y[den4$x >= 0 ], 0),
        col=is2023_trans, border=0)
lines(den4, col=is2023)
abline(v=mean(respondents2023$Giving2023_nosplint), col=is2023)
gdiff <- mean(respondents2023$Giving2023_nosplint)-mean(respondents2013$Giving2013)
text(4, 0.03, bquote(paste(mu == .(round(gdiff,2)))), adj=0)
arrows(mean(respondents2013$Giving2013), 0.03, mean(respondents2023$Giving2023_nosplint), 0.03, 
       length = 0.02, code = 3)
#receiving
den1 <- density(respondents2013$Receiving2013, from=0, to=20, bw=0.6)
plot(den1, ylim=c(0, 0.2), xlim=c(0, 20),
     xlab="", ylab="Density", type="n", main="Sample-adjusted receiving")
mtext("Number of ties reported", side=1, line=2)
polygon(c(den1$x[den1$x >= 0 ], 0),
        c(den1$y[den1$x >= 0 ], 0),
        col=rgb(0,0,0,0.2), border=0)
lines(den1, col="grey50")
abline(v=mean(respondents2013$Receiving2013), col="grey50")
den4 <- density(respondents2023$Receiving2023_nosplint, from=0, to=25, bw=0.6)
polygon(c(den4$x[den4$x >= 0 ], 0),
        c(den4$y[den4$x >= 0 ], 0),
        col=is2023_trans, border=0)
lines(den4, col=is2023)
abline(v=mean(respondents2023$Receiving2023_nosplint), col=is2023)
rdiff <- mean(respondents2023$Receiving2023_nosplint)-mean(respondents2013$Receiving2013)
text(3, 0.03, bquote(paste(mu == .(round(rdiff,2)))), adj=0)
arrows(mean(respondents2013$Receiving2013), 0.03, 
       mean(respondents2023$Receiving2023_nosplint), 0.03, length = 0.02, code = 3)
legend(14.5, 0.06, c("2013 (n=109)", "2023 (n=64)", "2023 Resampled (n=30)"), 
       col=c("grey50", is2023, ts), lwd=2, cex=0.9)
#inset plot
par(fig = c(0.55,1,0.75,1), mar=c(0,0,4,2), new=TRUE, cex.axis=0.8)
den1 <- density(resamp_givingdiffs, from=-10, to=10, bw=0.6)
plot(den1, ylim=c(0, 0.25), xlab="", yaxt="n", adj=0, cex.main=0.9, line=0.2,
     xlim=c(-10,10), main="Within-household differences", ylab="", type="n")
polygon(c(den1$x[which(den1$x >= -8 & den1$x <= 8)], rev(den1$x[which(den1$x >= -5 & den1$x <= 10)])),
        c(den1$y[which(den1$x >= -8 & den1$x <= 8)], rep(0, length(den1$x[which(den1$x >= -5 & den1$x <= 10)]))), 
        col=ts_trans, border=0)
lines(den1$x[which(den1$x >= -8 & den1$x <= 8)],
      den1$y[which(den1$x >= -8 & den1$x <= 8)], col=ts)
abline(v=mean(resamp_givingdiffs), col=ts)
text(-3.7, 0.22, bquote(paste(mu == .(round(mean(resamp_givingdiffs),2)))), adj=0)
#text(1.6, 0.03, "Change in resmpled households", adj=0, cex=0.9)
par(fig = c(0.55,1,0.25,0.5), new=TRUE)
den1 <- density(resamp_receivingdiffs, from=-10, to=10, bw=0.6)
plot(den1, ylim=c(0, 0.30), xlim=c(-10,10), yaxt="n",  adj=0, cex.main=0.9, line=0.2,
     main="Within-household differences", ylab="", type="n",xlab="")
polygon(c(den1$x[which(den1$x >= -8 & den1$x <= 8)], rev(den1$x[which(den1$x >= -5 & den1$x <= 10)])),
        c(den1$y[which(den1$x >= -8 & den1$x <= 8)], rep(0, length(den1$x[which(den1$x >= -5 & den1$x <= 10)]))), 
        col=ts_trans, border=0)
lines(den1$x[which(den1$x >= -8 & den1$x <= 8)],
      den1$y[which(den1$x >= -8 & den1$x <= 8)], col=ts)
abline(v=mean(resamp_receivingdiffs), col=ts)
text(-4.8, 0.25, bquote(paste(mu == .(round(mean(resamp_receivingdiffs),2)))), adj=0)
dev.off()
