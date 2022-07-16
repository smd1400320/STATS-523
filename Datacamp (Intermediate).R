##################################################
### chunk: printEx
##################################################
library(ggplot2)
library(dplyr)
library(tidyverse)
# Always name the element depending on your file name!
CoMMpass_CN_data_collapsed=read.table(file.choose(),header=T)
# Finding the total number of changes! Basically add major + minor = total!
total=CoMMpass_CN_data_collapsed$major+CoMMpass_CN_data_collapsed$minor
# Combining the new column total to the existing CoMMpass study!
CoMMpass_CN_data_collapsed=cbind(CoMMpass_CN_data_collapsed,total)
# - Create a column with  a categorical variable indicating if it is normal (diploid), 
# - deleted, gained (total == 3), amplified (total >= 4) or if LOH (total < 2) is present!
abnormal=total
# Always start with the >= value, otherwise things get screwed over!
abnormal[abnormal>=4]="AMP"
abnormal[abnormal==2]="NORM"
abnormal[abnormal==1]="LOH"
abnormal[abnormal==0]="DEL"
abnormal[abnormal==3]="GAIN"
CoMMpass_CN_data_collapsed=cbind(CoMMpass_CN_data_collapsed,abnormal)
# Renaming the data frame with the NORMAL value removed!
NEW_CoMMpass_CN_data_collapsed=CoMMpass_CN_data_collapsed[!(CoMMpass_CN_data_collapsed$abnormal=="NORM"),]
# Calculate how many patients exist(eg, 752)!
length(unique(NEW_CoMMpass_CN_data_collapsed$sample))
# Using dplyr rename all chromosomes (in order to appear in a correct order in ggplot), using the mutate function!
NEW_CoMMpass_CN_data_collapsed <- NEW_CoMMpass_CN_data_collapsed %>%
  mutate(Chrom = replace(Chrom, Chrom == 9, "09"))
# - Creating a stacked barplot showing the number of each anomaly per each chromosome by / via the number of patients 
# - (eg, finding the average number of gains/losses!
ggplot(data=NEW_CoMMpass_CN_data_collapsed,aes(x=Chrom,y=total/length(unique(NEW_CoMMpass_CN_data_collapsed$sample)),fill=abnormal))+
  geom_bar(stat="identity")
# Creating a mirrored bar plot (above the x line AMP,GAIN and below the x line LOH, DEL)!
ggplot()+
  geom_bar(data=NEW_CoMMpass_CN_data_collapsed[which(NEW_CoMMpass_CN_data_collapsed$total>2),],aes(x=Chrom,y=total/length(unique(NEW_CoMMpass_CN_data_collapsed$sample)),fill=abnormal),stat="identity")+
  geom_bar(data=NEW_CoMMpass_CN_data_collapsed[which(NEW_CoMMpass_CN_data_collapsed$total<2),],aes(x=Chrom,y=-total/length(unique(NEW_CoMMpass_CN_data_collapsed$sample)),fill=abnormal),stat="identity")+
# Renaming the y and x legends!
  ylab("Average number of CNVs per patient")+
  xlab("Chromosomes")
