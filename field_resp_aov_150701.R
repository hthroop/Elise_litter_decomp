# Import data 
# (note from HT: there were a few Patch_type values coded as "shrub" rather than "Shrub" and 
# there were a few Fire_blocks with a space in front of the text -- " 2B" rather than "2B".
# I also removed "_years" from "Last_burn" in order to use it as the column label on x-axis.
# I fixed those manually before importing the dataframe.)
data=read.csv("SoilResp_150701.csv",header=TRUE)

#split into two datasets to analyze seperately by month
feb_resp = data[data$Month=="February",]
#take only the data from february
april_resp = data [data$Month== "April",]
#take only the data from april

# Two-way ANOVA to test for differences in soil resp among Fire blocks and vegetation patches
aovfeb <- aov(CO_machine ~ Fire_block*Patch_type, data=feb_resp)
summary(aovfeb)
TukeyHSD(aovfeb, "Fire_block")
TukeyHSD(aovfeb, "Patch_type")
TukeyHSD(aovfeb, "Fire_block*Patch_type")

# Two-way ANOVA to test for differences in soil resp among Fire blocks and vegetation patches
aovapril <- aov(CO_machine ~ Fire_block*Patch_type, data=april_resp)
summary(aovapril)
TukeyHSD(aovapril, "Fire_block")
TukeyHSD(aovapril, "Patch_type")
TukeyHSD(aovapril, "Fire_block*Patch_type")

# posthoc comparisons using lsmean
library(multcompView) # load package multcompView
library(lsmeans) # load package lsmeans

marginal = lsmeans(aovfeb, ~ Fire_block) # not significant in this case, but I'm illustrating how to do the contrast
cld(marginal,
    alpha   = 0.05,
    Letters = letters,     ### Use lower-case letters for .group
    adjust  = "tukey")     ###  Tukey-adjusted comparisons

marginal = lsmeans(aovfeb, ~ Patch_type) # 
cld(marginal,
    alpha   = 0.05,
    Letters = letters,     ### Use lower-case letters for .group
    adjust  = "tukey")     ###  Tukey-adjusted comparisons
marginal = lsmeans(aovfeb, ~ Fire_block*Patch_type) 
cld(marginal,
    alpha   = 0.05,
    Letters = letters,     ### Use lower-case letters for .group
    adjust  = "tukey")     ###  Tukey-adjusted comparisons


# barplot - Heather's attempt
library(ggplot2)
library(plyr)

#condense the data into groups
cdata <- ddply(data, c("Month", "Fire_block", "Patch_type"), summarise,
               N    = sum(!is.na(CO_machine)),
               mean_resp = mean(CO_machine, na.rm=TRUE),
               sd_resp   = sd(CO_machine, na.rm=TRUE),
               se_resp   = sd_resp / sqrt(N) )
print(cdata)

# change order of months so that Feb will be before April (not alphabetical)
cdata$Month <- factor(cdata$Month, levels = c("February", "April"))

plot1 <- ggplot(cdata, aes(x=Fire_block, y=mean_resp, fill=Patch_type)) +
                  geom_bar(position = position_dodge(), stat="identity",
                          colour="black", # Use black outlines,
                          size=.3) +      # Thinner lines 
                  geom_errorbar(aes(ymin=mean_resp-se_resp, ymax=mean_resp+se_resp),
                          size=.3,    # Thinner lines
                          width=.2,
                          position=position_dodge(.9)) +
                  facet_grid(. ~ Month) +
                  xlab("Time Since Fire (y)") +
                  ylab("Soil Respiration") +
                  theme_bw(base_size = 20, base_family = "") + # font size
                  theme(legend.position=c(0.8,0.8)) + #legend in upper right
                  theme(panel.grid.major = element_blank(), #no grid lines
                        panel.grid.minor = element_blank())  +
                  scale_fill_manual(name="", # Legend label, use darker colors
                           values=c("#FFFFFF","#999999","#000000"))     
plot1

#p + scale_x_discrete(limits=c("live","skeleton","open")) + 
#  xlab("Patch Type") + 
#  theme_bw(base_size = 20, base_family = "") + # font size
#  theme(legend.position=c(0.9,0.9)) + #legend in upper right
#  theme(panel.grid.major = element_blank(), #no grid lines
#        panel.grid.minor = element_blank())  +
#  geom_text(aes(x = levels(status)[1], y = 11.25, label = graph_labs), 
#            data = cdata2, hjust = 7) # add panel letters





#barplot - ELISE 

#x = time since last burn (Last_burn), y= soil respiration (Co_machine)

#checking how many points per burn time
length(which(data$Last_burn==2))
length(which(data$Last_burn==3))
length(which(data$Last_burn==15))
length(which(data$Last_burn==25))

#subsetting data by burn length
lastburn2<-data[(which(data$Last_burn==2)),]
lastburn3<-data[(which(data$Last_burn==3)),]
lastburn15<-data[(which(data$Last_burn==15)),]
lastburn25<-data[(which(data$Last_burn==25)),]

#subsetting data by Month
lastburn2_april<-lastburn2[(which(lastburn2$Month=="April")),]
lastburn2_feb<-lastburn2[(which(lastburn2$Month=="name for feb here")),]

#subsetting data by Month
lastburn3_april<-lastburn3[(which(lastburn3$Month=="April")),]
lastburn3_feb<-lastburn3[(which(lastburn3$Month=="name for feb here")),]

#subsetting data by Month
lastburn15_april<-lastburn15[(which(lastburn15$Month=="April")),]
lastburn15_feb<-lastburn15[(which(lastburn15$Month=="name for feb here")),]

#subsetting data by Month
lastburn25_april<-lastburn25[(which(lastburn25$Month=="April")),]
lastburn25_feb<-lastburn25[(which(lastburn25$Month=="name for feb here")),]

#2 year

#getting means of co_machuine by month by patchtype
lastburn2_bare<-lastburn2_april[which(lastburn2_april$Patch_type=="Bare"),]
lastburn2_bare_mean<-mean(as.numeric(as.character(lastburn2_bare$CO_machine)),na.rm=T)
lastburn2_bare_mean

#getting means of co_machuine by month by patchtype
lastburn2_grass<-lastburn2_april[which(lastburn2_april$Patch_type=="Grass"),]
lastburn2_grass_mean<-mean(as.numeric(as.character(lastburn2_grass$CO_machine)),na.rm=T)
lastburn2_grass_mean

#getting means of co_machine by Month by patchtype
lastburn2_shrub<-lastburn2_april[which(lastburn2_april$Patch_type=="Shrub"),]
lastburn2_shrub_mean<-mean(as.numeric(as.character(lastburn2_shrub$CO_machine)),na.rm=T)
lastburn2_shrub_mean

#3 year

#getting means of co_machuine by month by patchtype
lastburn3_bare<-lastburn3_april[which(lastburn3_april$Patch_type=="Bare"),]
lastburn3_bare_mean<-mean(as.numeric(as.character(lastburn3_bare$CO_machine)),na.rm=T)
lastburn3_bare_mean

#getting means of co_machuine by month by patchtype
lastburn3_grass<-lastburn3_april[which(lastburn3_april$Patch_type=="Grass"),]
lastburn3_grass_mean<-mean(as.numeric(as.character(lastburn3_grass$CO_machine)),na.rm=T)
lastburn3_grass_mean

#getting means of co_machuine by month by patchtype
lastburn3_shrub<-lastburn3_april[which(lastburn3_april$Patch_type=="Shrub"),]
lastburn3_shrub_mean<-mean(as.numeric(as.character(lastburn3_shrub$CO_machine)),na.rm=T)
lastburn3_shrub_mean

#15 year

#getting means of co_machuine by month by patchtype
lastburn15_bare<-lastburn15_april[which(lastburn15_april$Patch_type=="Bare"),]
lastburn15_bare_mean<-mean(as.numeric(as.character(lastburn15_bare$CO_machine)),na.rm=T)
lastburn15_bare_mean

#getting means of co_machuine by month by patchtype
lastburn15_grass<-lastburn15_april[which(lastburn15_april$Patch_type=="Grass"),]
lastburn15_grass_mean<-mean(as.numeric(as.character(lastburn15_grass$CO_machine)),na.rm=T)
lastburn15_grass_mean

#getting means of co_machuine by month by patchtype
lastburn15_shrub<-lastburn3_april[which(lastburn15_april$Patch_type=="Shrub"),]
lastburn15_shrub_mean<-mean(as.numeric(as.character(lastburn15_shrub$CO_machine)),na.rm=T)
lastburn15_shrub_mean

#25 year

#getting means of co_machuine by month by patchtype
lastburn25_bare<-lastburn25_april[which(lastburn25_april$Patch_type=="Bare"),]
lastburn25_bare_mean<-mean(as.numeric(as.character(lastburn25_bare$CO_machine)),na.rm=T)
lastburn25_bare_mean

#getting means of co_machuine by month by patchtype
lastburn25_grass<-lastburn25_april[which(lastburn25_april$Patch_type=="Grass"),]
lastburn25_grass_mean<-mean(as.numeric(as.character(lastburn25_grass$CO_machine)),na.rm=T)
lastburn25_grass_mean

#getting means of co_machuine by month by patchtype
lastburn25_shrub<-lastburn25_april[which(lastburn25_april$Patch_type=="Shrub"),]
lastburn25_shrub_mean<-mean(as.numeric(as.character(lastburn25_shrub$CO_machine)),na.rm=T)
lastburn25_shrub_mean


#making the final dataframe 
april_barplot_data<-matrix(data=NA,nrow=12,ncol=3)
april_barplot_data[,1]<-c("Bare","Grass","Shrub","Bare","Grass","Shrub","Bare","Grass","Shrub","Bare","Grass","Shrub")
april_barplot_data[,2]<-c(2,2,2,3,3,3,15,15,15,25,25,25)
april_barplot_data[,3]<-c(lastburn2_bare_mean,lastburn2_grass_mean,lastburn2_shrub_mean,lastburn3_bare_mean,lastburn3_grass_mean,lastburn3_shrub_mean,lastburn15_bare_mean,lastburn15_grass_mean,
                          lastburn15_shrub_mean,lastburn25_bare_mean,lastburn25_grass_mean,lastburn25_shrub_mean)


april_barplot_data[,3]<-c(lastburn3_bare_mean,lastburn3_grass_mean,lastburn3_shrub_mean)
april_barplot_data[,3]<-c(lastburn15_bare_mean,lastburn15_grass_mean,lastburn15_shrub_mean)
april_barplot_data[,3]<-c(lastburn25_bare_mean,lastburn25_grass_mean,lastburn25_shrub_mean)




#make matrix into dataframe
april_data_barplot<-as.data.frame(april_barplot_data)

#name the columns
colnames(april_data_barplot) <- c("Patch_type","Last_burn","co_machine")

#make means into numbers instead of characters
#this liekly does not matter
#april_data_barplot$co_machine<-as.numeric(april_data_barplot$co_machine)

#make the barplot
ggplot(data=april_data_barplot, aes(factor(Last_burn), co_machine, fill = Patch_type)) + 
  geom_bar(stat="identity",position = "dodge" ) + 
  scale_fill_brewer(palette = "Set1")

april_data_barplot
