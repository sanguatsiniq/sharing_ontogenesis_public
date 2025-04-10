### Network figures ###
# "sharing-ontogenesis paper"
# Elspeth Ready

### Set environment ###
library(this.path)
setwd(this.path::here())

source("build_networks.R")
library(statnet)

### 2013 data ###

sampling2013 <- read.csv("2013_data/processed_data_2013/HID_sample.csv")
Inuit2013 <- sampling2013$HID[which(sampling2013$type=="Inuit")]
respondents2013 <- sampling2013$HID[which(sampling2013$type=="Inuit" & sampling2013$survey==1)]
repeated2013 <- merged_data$HID_2013_occupant[which(merged_data$Temporal_type==1 & merged_data$HID_2023<=64)]

# giving data
giving2013 <- read.csv("2013_data/raw_data_2013/Survey_food_giving.csv")
giving2013_sample <- giving2013[which(giving2013$HID_to < 887 & giving2013$HID_from %in% respondents2013),]
giving2013_sample <- giving2013_sample[which(!(duplicated(paste(giving2013_sample$HID_to, giving2013_sample$HID_from)))),] # (disallow multiple ties per household)
# receiving data
receiving2013 <- read.csv("2013_data/raw_data_2013/Survey_food_receiving.csv")
receiving2013_sample <- receiving2013[which(receiving2013$HID_from < 887 & receiving2013$HID_to %in% respondents2013),]
receiving2013_sample <- receiving2013_sample[which(!(duplicated(paste(receiving2013_sample$HID_to, receiving2013_sample$HID_from)))),] 
#merge
all2013 <- rbind.data.frame(giving2013_sample[,2:3], receiving2013_sample[,3:2])
all2013 <- all2013[which(!(duplicated(all2013))),] 
cf_2013 <- network(all2013)

flags <- ((cf_2013%v%"vertex.names") %in% respondents2013) +1
flags[which(cf_2013%v%"vertex.names" %in% repeated2013)] <- 3

pdf("full_network_2013.pdf", height=4, width=4, pointsize=9)
layout(c(1,2), widths=c(1,1), heights=c(3,1))
par(mar=c(0,0,0,0))
palette(c("black", "grey90", "darkseagreen", "darkblue"))
plot(cf_2013, vertex.col=flags+1, arrowhead.cex=0.7, edge.col="darkgrey", edge.lwd=1, vertex.cex=1.2)
par(mar=c(0,0,2,0))
plot(1:2, 1:2, type="n", axes=FALSE, main="2013 food sharing network")
legend(1.35, 2, c("Not in sample", "2013 sample only", "Twice-sampled"), 
       pch=21, pt.bg=c("grey90", "darkseagreen", "darkblue"), cex=0.9)
dev.off()


# 2023 network
eligible_HIDs <- c(1:236, 238:287) # note 271:275, 276:287 are special cases
sample2023 <- seq(1, 64)
repeated2023 <- merged_data$HID_2023[which(merged_data$Temporal_type==1 & merged_data$HID_2023<=64)]
splinters2023 <- merged_data$HID_2023[which(merged_data$Temporal_type %in% c("splinter (0)", "splinter (1)") & merged_data$HID_2023<=64)]

# giving data
giving2023 <- read.csv("2023_data/cleaned_data_2023/Survey_food_giving_2023.csv")
giving2023_sample <- giving2023[which(!(duplicated(paste(giving2023$HID_to, giving2023$HID_from)))),] 
giving2023_sample <- giving2023_sample[which(giving2023_sample$HID_to %in% eligible_HIDs & giving2023_sample$HID_from %in% sample2023),]
#giving2023_sample <- giving2023_sample[-which(!(is.na(giving2023_sample$Town))),]
giving2023_sample <- giving2023_sample[-which(giving2023_sample$Last_time=="longer"),]
# receiving data
receiving2023 <- read.csv("2023_data/cleaned_data_2023/Survey_food_receiving_2023.csv")
receiving2023_sample <- receiving2023[which(!(duplicated(paste(receiving2023$HID_to, receiving2023$HID_from)))),] 
receiving2023_sample <- receiving2023_sample[which((receiving2023_sample$HID_from %in% eligible_HIDs) & (receiving2023_sample$HID_to %in% sample2023)),]
#receiving2023_sample <- receiving2023_sample[-which(!(is.na(receiving2023_sample$Town))),]
receiving2023_sample <- receiving2023_sample[-which(receiving2023_sample$Last_time %in% c("longer", "dk")),]
#merge
all2023 <- rbind.data.frame(giving2023_sample[,c(3,4)], receiving2023_sample[,c(3,4)])
all2023 <- all2023[which(!(duplicated(paste(all2023$HID_from, all2023$HID_to)))),]
#allties <- allties[-which(allties$HID_from > 287 | allties$HID_to > 287),]
cf2023 <- network(all2023)

flags <- ((cf2023%v%"vertex.names") %in% sample2023) +1
flags[which(cf2023%v%"vertex.names" %in% repeated2023)] <- 3
flags[which(cf2023%v%"vertex.names" %in% splinters2023)] <- 4

pdf("full_network_2023.pdf", height=4, width=4, pointsize=9)
layout(c(1,2), widths=c(1,1), heights=c(3,1))
par(mar=c(0,0,0,0))
palette(c("black", "grey90", "orchid4", "darkblue", rgb(0.961, 0.502, 0.067)))
plot(cf2023, vertex.col=flags+1, arrowhead.cex=0.7, edge.col="darkgrey", edge.lwd=1, vertex.cex=1.2)
par(mar=c(0,0,2,0))
plot(1:2, 1:2, type="n", axes=FALSE, main="2023 food sharing network")
legend(1.35, 2, c("Not in sample", "2023 sample only", "Twice-sampled", "Splinters"), 
       pch=21, pt.bg=c("grey90", "orchid4", "darkblue", rgb(0.961, 0.502, 0.067)), 
       cex=0.9)
dev.off()

