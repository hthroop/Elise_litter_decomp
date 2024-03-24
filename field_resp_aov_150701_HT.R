# Import data 
# (note from HT: there were a few Patch_type values coded as "shrub" rather than "Shrub" and 
# there were a few Fire_blocks with a space in front of the text -- " 2B" rather than "2B".
# I also removed "_years" from "Last_burn" in order to use it as the column label on x-axis.
# I fixed those manually before importing the dataframe.)
data=read.csv("SoilResp_150701.csv",header=TRUE)

# change Last_burn to a factor to use in ANOVA
data$Last_burn <- factor(data$Last_burn)

# make a barplot using ggplot2
library(ggplot2)
library(plyr)

#condense the data into groups
cdata <- ddply(data, c("Month", "Last_burn", "Patch_type"), summarise,
               N    = sum(!is.na(CO_machine)),
               mean_resp = mean(CO_machine, na.rm=TRUE),
               sd_resp   = sd(CO_machine, na.rm=TRUE),
               se_resp   = sd_resp / sqrt(N) )
print(cdata)

# change order of months so that Feb will be before April (not alphabetical)
cdata$Month <- factor(cdata$Month, levels = c("February", "April"))

plot1 <- ggplot(cdata, aes(x=Last_burn, y=mean_resp, fill=Patch_type)) +
                  geom_bar(position = position_dodge(), stat="identity",
                          colour="black", # Use black outlines,
                          size=.3) +      # Thinner lines 
                  geom_errorbar(aes(ymin=mean_resp-se_resp, ymax=mean_resp+se_resp), # add error bars
                          size=.3,    # Thinner lines
                          width=.2,
                          position=position_dodge(.9)) +
                  facet_grid(. ~ Month) +  # this makes the facets (separate graphs) for each month
                  xlab("Time Since Fire (y)") + # add an x-axis label
                  ylab("Soil Respiration (units go here...)") + # add a y-axis label
                  theme_bw(base_size = 20, base_family = "") + # font size 
                  theme(legend.position=c(0.2,0.8)) + #legend in upper right
                  theme(panel.grid.major = element_blank(), #no grid lines
                        panel.grid.minor = element_blank())  +
                  scale_fill_manual(name="", # don't add a legend at top of graph
                           values=c("#FFFFFF","#999999","#000000"))   # specify colors of columns
plot1 # display plot1


