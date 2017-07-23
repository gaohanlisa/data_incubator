setwd("/Users/gaohanlisa/Box Sync/gaohanlisa—pro/study/data_incubator")
library(ggplot2)
library(dplyr)
library(plyr)
library(zipcode)
library(plotly)
library(rasterVis)
library(viridis)
library(RColorBrewer)
library(reshape2)
library(d3heatmap)
library(gridExtra)
############### Import related data #####################
data("zipcode")
us <- map_data("state")
severeinjury <- read.csv(file = "severeinjury.csv", header=T, stringsAsFactors = F)
severeinjury$State <- tolower(severeinjury$State)
State <- count(severeinjury, c("State"))
severeinjury$Zip <- formatC(severeinjury$Zip, width = 5, format = "d", flag = "0")
Zip <- count(severeinjury, c("Zip"))[-1,]
colnames(Zip) <- c("zip","Times")
Zip_C <- merge(Zip, zipcode, by = "zip", all.x = T)
################# plot the distribution of injury numbers by state ###########################
gg <- ggplot(data = us) + geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#ffffff", size=0.15)
gg <- gg + geom_map(data=State, map=us,
                    aes(fill=freq, map_id=State),
                    color="#ffffff", size=0.15)
gg <- gg + scale_fill_continuous(low="#F1F9EF", high="#1B7837", 
                                 guide='colorbar', trans = "sqrt","Frequency")
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(axis.text = element_blank())
gg <- gg + geom_point(data = Zip_C, aes(x=longitude, y=latitude, size=Times),alpha=.25) + scale_size_continuous(range = c(0.15,5))
gg <- gg + xlim(-130,-65)+ylim(25,50)
gg
ggsave(filename = "injury_by_state.svg", width=10, height = 6)

####################### Create plot # of injuries by nature and time ##################
severeinjury$Nature_divi <- substring(severeinjury$Nature, first = 1, last = 2)
Nature <- count(severeinjury, c("EventDate", "Nature_divi"))
Nature$EventDate <- as.Date(Nature$EventDate, format = "%m/%d/%Y")
Nature$EventDate <- format(Nature$EventDate, "%m/%Y")
Nature_M <- count(Nature, c("EventDate", "Nature_divi"))

level <- c("01/2015","02/2015","03/2015","04/2015","05/2015","06/2015","07/2015","08/2015","09/2015","10/2015","11/2015","12/2015",
            "01/2016","02/2016","03/2016","04/2016","05/2016","06/2016","07/2016","08/2016","09/2016","10/2016","11/2016","12/2016")
Nature_M$EventDate <- factor(Nature_M$EventDate, levels = unique(level))
Nature_dcast <- dcast(Nature_M, EventDate ~ Nature_divi)
Nature_dcast[is.na(Nature_dcast)] <- 0
Nature_title <- c("unspecified", "bones,nerves,spinal core","muscles,tendons,ligaments,joints","open wounds","surface wounds and bruises","burns and corrosions",
                  "intracranial injuries","effects of envirnmental conditions","multiple traumatic injuries and disorders","other traumatic injuries and disorders")
colnames(Nature_dcast)[2:11] <- Nature_title
Nature_M <- melt(Nature_dcast[,1:11])
Nature_M$variable <- factor(Nature_M$variable, levels = unique(Nature_title))
pp <- ggplot(data = Nature_M, aes(x=EventDate, y=variable)) + 
  geom_tile(aes(fill=value), colour = "white") + 
  scale_fill_viridis(trans="sqrt","# Events") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x=NULL, y=NULL, title="Events per Month by Injury Type")
pp <- pp + theme(axis.text = element_text(size=10, color="black"))
pp <- pp + theme(plot.margin =unit(c(0.5,0.5,0.5,0.5), "cm"))
pp
ggsave(filename = "injury_type_by_time.svg", plot = pp ,width=10.5, height = 4)
ggsave(filename = "injury_type_by_time.png", plot = pp, width=10.5, height = 4)

####################### Create plot # of injuries by source and time ##################

severeinjury$Source_divi <- substring(severeinjury$Source, first = 1, last = 1)
Source <- count(severeinjury, c("EventDate", "Source_divi"))
Source$EventDate <- as.Date(Source$EventDate, format = "%m/%d/%Y")
Source$EventDate <- format(Source$EventDate, "%m/%Y")
Source_M <- count(Source, c("EventDate", "Source_divi"))

level <- c("01/2015","02/2015","03/2015","04/2015","05/2015","06/2015","07/2015","08/2015","09/2015","10/2015","11/2015","12/2015",
           "01/2016","02/2016","03/2016","04/2016","05/2016","06/2016","07/2016","08/2016","09/2016","10/2016","11/2016","12/2016")
Source_M$EventDate <- factor(Source_M$EventDate, levels = unique(level))
Source_dcast <- dcast(Source_M, EventDate ~ Source_divi)
Source_dcast[is.na(Source_dcast)] <- 0
Source_title <- c("chemicals and chemical products","containers, furnitures and fixtures","machinery","parts and materials",
                  "persons, plants, animals, and minerals","structures and surfaces","tools, instruments and equipment","vehicles",
                  "other sources")
colnames(Source_dcast)[2:10] <- Source_title
Source_M <- melt(Source_dcast)
Nature_M$variable <- factor(Nature_M$variable, levels = unique(Nature_title))
qq <- ggplot(data = Source_M, aes(x=EventDate, y=variable)) + 
  geom_tile(aes(fill=value), colour = "white") + 
  scale_fill_viridis(option="magma",trans="sqrt","# Events") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x=NULL, y=NULL, title="Events per Month by Source Type")
qq <- qq + theme(axis.text = element_text(size=10, color="black"))
qq <- qq + theme(plot.margin =unit(c(0.5,0.5,0.5,0.5), "cm"))
qq
ggsave(filename = "source_type_by_time.svg", plot = qq,width=10.4, height = 3.7)
ggsave(filename = "source_type_by_time.png", plot = qq,width=10.4, height = 3.7)

####################### Create plot # of injuries by sector and time ##################

severeinjury$sector_divi <- substring(severeinjury$Primary.NAICS, first = 1, last = 2)
sector <- count(severeinjury, c("EventDate", "sector_divi"))
sector$EventDate <- as.Date(sector$EventDate, format = "%m/%d/%Y")
sector$EventDate <- format(sector$EventDate, "%m/%Y")
sector_M <- count(sector, c("EventDate", "sector_divi"))
sector_dcast <- dcast(sector_M, EventDate ~ sector_divi)
sector_dcast[is.na(sector_dcast)] <- 0
sector_title <- c("Agriculture, Forestry, Fishing and Hunting", "Mining","Utilities","Construction","Food, Textile and Leather related Manufacturing",
                  "Wood, Petroleum, Chemical and Plastic Related Manufacturing", "Metal, Machinery, Electrical Products and Furniture related Manufacturing", 
                  "Wholesale Trade", "Food, Health and Personal Care Products and Clothing Retail Trade", "Other Retail Trade","Transportation", "Warehousing", "Information", "Finance and Insurance",
                  "Real Estate Rental and Leasing", "Professional, Scientific and Technical Services", "Management of Companies and Enterprises",
                  "Administrative and Support and Waste Management and Remediation Services", "Educational Services","Health Care and Social Assistance",
                  "Arts, Entertainment, and Recreation", "Accommodation and Food Services", "Other Services (except Public Administration)",
                  "Public Administration")
colnames(sector_dcast)[2:25] <- sector_title
sector_M <- melt(sector_dcast[,-c(26,27)])
sector_M$variable <- factor(sector_M$variable, levels = unique(sector_title))
pp <- ggplot(data = sector_M, aes(x=EventDate, y=variable)) + 
  geom_tile(aes(fill=value), colour = "white") + 
  scale_fill_viridis(trans="sqrt","# Events") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5,hjust = 1)) +
  labs(x=NULL, y=NULL, title="Events per Month by Industry Classification")
pp <- pp + theme(axis.text = element_text(size=10, color="black"))
pp <- pp + theme(plot.margin =unit(c(0.5,0.5,0.5,0.5), "cm"))
pp
ggsave(filename = "source_type_by_classification.svg", plot = pp,width=10.4, height = 5.2)
ggsave(filename = "source_type_by_classification.png", plot = pp,width=10.4, height = 5.2)

####################### Create plot # of injuries by sector and nature type ##################
Sector_Nature <- count(severeinjury, c("sector_divi", "Nature_divi"))
Sector_Nature_dcast <- dcast(Sector_Nature, sector_divi ~ Nature_divi)
Sector_Nature_dcast[is.na(Sector_Nature_dcast)] <- 0
Sector_Nature_dcast$sector_divi[1:24] <- sector_title
colnames(Sector_Nature_dcast)[2:11] <- Nature_title
Sector_Nature_dcast <- Sector_Nature_dcast[c(1:24),]
Sector_Nature_dcast <- Sector_Nature_dcast[,c(1:11)]
Sector_Nature_M <- melt(Sector_Nature_dcast)
rownames(Sector_Nature_dcast) <- Sector_Nature_dcast$sector_divi
Sector_Nature_dcast[,1] <- NULL
Sector_Nature_dcast <- as.data.frame(t(Sector_Nature_dcast))
Sector_Nature_dcast$All <- rowSums(Sector_Nature_dcast[1:24])
Sector_Nature_dcast$Nature <- rownames(Sector_Nature_dcast)

pdf("sector_nature.pdf")
for (i in 1:25) {
  qq <- ggplot(data = Sector_Nature_dcast, aes(x = Nature, y= Sector_Nature_dcast[,i])) +
    geom_bar(aes(fill=Nature), stat = "identity")+ 
    coord_polar() +
    scale_fill_brewer("# Events",palette = "Spectral") +
    theme_bw() +
    theme(axis.text.x = element_blank(), legend.position = "none") +
    labs(x=NULL, y=NULL, title=colnames(Sector_Nature_dcast)[i])+ 
    theme(plot.margin =unit(c(2,2,2,2), "cm"),text = element_text(size=20, color = "black"),
          axis.text = element_text(size=20,color="black"))
  plot(qq)
}
dev.off()

#################### Create Event list word art #######################
Event <- paste(unlist(severeinjury$EventTitle), collapse =" ")
fileConn<-file("event_list.txt")
writeLines(Event, fileConn)
close(fileConn)
