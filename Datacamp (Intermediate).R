##################################################
### CNV Data Visualization
##################################################

library(ggplot2)
library(dplyr)
library(tidyverse)
library(data.table)

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
abnormal[abnormal==1]="DEL"
abnormal[abnormal==0]="LOH"
abnormal[abnormal==3]="GAIN"
CoMMpass_CN_data_collapsed=cbind(CoMMpass_CN_data_collapsed,abnormal)

# Renaming the data frame with the NORMAL value removed!
NEW_CoMMpass_CN_data_collapsed=CoMMpass_CN_data_collapsed[!(CoMMpass_CN_data_collapsed$abnormal=="NORM"),]

# Calculate how many patients exist(eg, 752)!
length(unique(NEW_CoMMpass_CN_data_collapsed$sample))

# Using dplyr rename all chromosomes (in order to appear in a correct order in ggplot), using the mutate function!
NEW_CoMMpass_CN_data_collapsed <- NEW_CoMMpass_CN_data_collapsed %>%
  mutate(Chrom = replace(Chrom, Chrom == 1, "01"))
NEW_CoMMpass_CN_data_collapsed <- NEW_CoMMpass_CN_data_collapsed %>%
  mutate(Chrom = replace(Chrom, Chrom == 2, "02"))
NEW_CoMMpass_CN_data_collapsed <- NEW_CoMMpass_CN_data_collapsed %>%
  mutate(Chrom = replace(Chrom, Chrom == 3, "03"))
NEW_CoMMpass_CN_data_collapsed <- NEW_CoMMpass_CN_data_collapsed %>%
  mutate(Chrom = replace(Chrom, Chrom == 4, "04"))
NEW_CoMMpass_CN_data_collapsed <- NEW_CoMMpass_CN_data_collapsed %>%
  mutate(Chrom = replace(Chrom, Chrom == 5, "05"))
NEW_CoMMpass_CN_data_collapsed <- NEW_CoMMpass_CN_data_collapsed %>%
  mutate(Chrom = replace(Chrom, Chrom == 6, "06"))
NEW_CoMMpass_CN_data_collapsed <- NEW_CoMMpass_CN_data_collapsed %>%
  mutate(Chrom = replace(Chrom, Chrom == 7, "07"))
NEW_CoMMpass_CN_data_collapsed <- NEW_CoMMpass_CN_data_collapsed %>%
  mutate(Chrom = replace(Chrom, Chrom == 8, "08"))
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

# Open the gene.txt file with the start, end position of the specific genes along with the chromosome involved!
gene=read.table(file.choose(),header=T)

# Create a column with the size of each gene!
size=(gene[,"end"]-gene[,"start"])

# Bind the gene dataframe with the new size vector!
gene=cbind(gene,size)

# Rename the column elements by removing "chr"!
gene[,"Chrom"]=gsub("chr","",as.character(gene[,"Chrom"]))

# Create a barplot with the gene proportion per each chromosome!
ggplot(data=gene,aes(x=Chrom,y=size,fill=gene)) + 
  geom_bar(stat="identity",position="fill") +
  theme_bw() +
# Have the x axis appear better!
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Percentage of chromosome")+
  xlab("Chromosomes")

# - Assign to each start and end point in the NEW_CoMMpass_CN_data_collapsed the correct gene by creating 
# - a new data frame!
complete=(setDT(NEW_CoMMpass_CN_data_collapsed)[setDT(gene), on = c("Chrom", "start<=start", "end>=end")])

# Remove the size column in the complete data frame!
complete$size=NULL

# Renaming the columns of the complete data frame!
colnames(complete)=c("sample","chr","start","end","major","minor","total","abnormal","gene")

##################################################
### Clinical Data Visualization
##################################################

# Always name the element depending on your file name!
CoMMpass_CN_data_collapsed=read.table(file.choose(),header=T)