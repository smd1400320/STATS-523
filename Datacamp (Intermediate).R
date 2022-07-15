##################################################
### chunk: printEx
##################################################
Clinic_CoMMpass=read.table(file.choose(),header=T)
library(ggplot2)
NO_NA_Clinic_CoMMpass=subset(Clinic_CoMMpass,!is.na(ISS))
ggplot(data=subset(NO_NA_Clinic_CoMMpass,!is.na(age)),aes(x=age,fill=ISS))+
  geom_bar()
MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants=read.table(file.choose(),header=T)
APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants=MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants[str_detect(MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants$GENE,"APOBEC"),]
FIRST_APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants=APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants[str_detect(APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants$sample,"_1_"),]
ggplot(FIRST_APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants,aes(x=GENE,fill=EFFECT))+
  geom_bar(aes(x=forcats::fct_infreq(GENE)))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
SECOND_APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants=APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants[str_detect(APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants$sample,"_2_"),]
ggplot(SECOND_APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants,aes(x=GENE,fill=EFFECT))+
  geom_bar(aes(x=forcats::fct_infreq(GENE)))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
THIRD_APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants=APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants[str_detect(APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants$sample,"_3_"),]
ggplot(THIRD_APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants,aes(x=GENE,fill=EFFECT))+
  geom_bar(aes(x=forcats::fct_infreq(GENE)))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants,aes(x=GENE,fill=EFFECT))+
  geom_bar(aes(x=forcats::fct_infreq(GENE)))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
mean(APOBEC_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants$TUMOR_DP)
ggplot(MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants,aes(x=NORMAL_DP))+
  geom_bar()
mean(MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants$TUMOR_DP)
MAF_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants=MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants[str_detect(MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants$GENE,"MAF"),]
FIRST_MAF_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants=MAF_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants[str_detect(MAF_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants$sample,"_1_"),]
ggplot(FIRST_MAF_MMRF_CoMMpass_IA11a_IGV_All_Canonical_Variants,aes(x=GENE,fill=EFFECT))+
  geom_bar(aes(x=forcats::fct_infreq(GENE)))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
