#Tables and Figures to accompany the manuscript 'High Mycobacterium bovis exposure but low IGRA positivity in UK farm workers' Thomas et al 2025, published in Zoonoses and Public Health  

# load packages 
library(tidyverse)
library(table1)
library(cowplot)
library(readxl) 

#import data 
data=read.csv("zTBstudyfinaldata_clean.csv")


#Table 1 
#Note includes linked APHA data for bTB burden over 10 year period

#Collapse Farm Manager/Director and Farmer/Farm worker into one group
data <- data %>% mutate(qs3_occupation_clean = ifelse(qs3_occupation_clean == "Farm Manager/Director" | qs3_occupation_clean == "Farmer/Farm worker", "Farmer/Farm worker", qs3_occupation_clean))


#Clean self-reported reactors in last 2 years 
data$qs3_reactors_numeric = as.numeric(data$qs3_reactors)
data[,c("qs3_reactors","qs3_reactors_numeric")]
data$qs3_reactors_numeric[data$qs3_reactors=="No more than 10, no fewer than 4"] = (10+4)/2
data$qs3_reactors_numeric[data$qs3_reactors=="at least 50"] = 50
data$qs3_reactors_numeric[data$qs3_reactors=="1 R, 10 IRs, "] = 1
data$qs3_reactors_numeric[data$qs3_reactors=="100+"] = 100
data$qs3_reactors_numeric[data$qs3_reactors=="3, 5 test positive"] = 5
data$qs3_reactors_numeric[data$qs3_reactors=="150+"] = 150
                
data %>%
  #group_by(ps_id) %>%
  mutate(medianreactors=median(reactors_10years,na.rm=T),quart=IQR(reactors_10years,na.rm=T)) %>%
  #ungroup() %>%
  mutate(totaltested_10years = ifelse(is.na(totaltested_10years),0,totaltested_10years)) %>%
  mutate(riskgroup = ifelse(totaltested_10years==0,"No CPH",ifelse(reactors_10years<medianreactors,"Low",ifelse(reactors_10years<quart,"Medium","High")))) %>%
  #select(reactors, lesions, totaltested,medianreactors, quart, riskgroup) 
  mutate(riskgroup = fct_relevel(riskgroup, "High", after = Inf)) %>%
  mutate(riskgroup = fct_relevel(riskgroup, "No CPH", after = Inf)) %>%
  table1(~ age_years + 
           qs3_occupation_clean + 
           qs1_gender_3.factor  + 
           ethnicity1.factor  + 
           qs2_bcg.factor + 
           qs2_healthscore + 
           farmtype.factor +
           totaltested_10years + 
           reactors_10years + 
           lesions_10years + 
           medianherdsize_10years +
           qs3_reactors_numeric +
           qs2_milk.factor + 
           qs2_exposurerisk_tb.factor +
           qs2_precautions.factor + 
           qs4_contact_cattle.factor + 
           IGRA_result_posneg + 
           IGRA_TB1MinusNil_units +
           IGRA_TB2MinusNil_units
         | riskgroup, overall=F, format_number = list(big.mark = ","),
         caption = "Risk group", data = .)



# Negatives only for IGRA TB1 and TB2
data %>%
  #group_by(ps_id) %>%
  mutate(medianreactors=median(reactors_10years,na.rm=T),quart=IQR(reactors_10years,na.rm=T)) %>%
  #ungroup() %>%
  mutate(totaltested_10years = ifelse(is.na(totaltested_10years),0,totaltested_10years)) %>%
  mutate(riskgroup = ifelse(totaltested_10years==0,"No CPH",ifelse(reactors_10years<medianreactors,"Low",ifelse(reactors_10years<quart,"Medium","High")))) %>%
  #select(reactors, lesions, totaltested,medianreactors, quart, riskgroup) 
  mutate(riskgroup = fct_relevel(riskgroup, "High", after = Inf)) %>%
  mutate(riskgroup = fct_relevel(riskgroup, "No CPH", after = Inf)) %>%
  filter(IGRA_result_posneg == "Negative") %>%
  table1(~ IGRA_TB1MinusNil_units +
           IGRA_TB2MinusNil_units
         | riskgroup, overall=F, format_number = list(big.mark = ","),
         caption = "Risk group", data = .)

#prop vaccinated 
table(data$qs2_bcg.factor)
#60/90=66.7%

#Overall table, not split by risk group 
data %>%
  mutate(all = "all") %>%
  table1(~ age_years + 
           qs3_occupation_clean + 
           qs1_gender_3.factor  + 
           ethnicity1.factor  + 
           qs2_bcg.factor + 
           qs2_healthscore + 
           farmtype.factor +
           totaltested_10years + 
           reactors_10years + 
           lesions_10years + 
           qs2_milk.factor + 
           qs2_exposurerisk_tb.factor +
           qs2_precautions.factor + 
           qs4_contact_cattle.factor + 
           IGRA_result_posneg + 
           IGRA_TB1MinusNil_units +
           IGRA_TB2MinusNil_units
         | all, overall=F, format_number = list(big.mark = ","),
         caption = "All", data = .)

#herd size - questionnaire data 
table(data$herdsize)
data$herdsize = factor(data$herdsize, levels = c("1-10","11-50","51-100","101-200","201-300","300+"))

#herd size as reported via linked APHA data over last 10 years 
data$medianherdsize_10years
summary(data$medianherdsize_10years)

#duration in occupation 
table(data$duration_occ)
#48/90 (53.3%) in occupation for >20 years 

#median household size 
summary(data$household_size3)

#edit household size of 44 to 4 - data entry mistake 
data$household_size3[data$household_size3 == "44"] <- 4
summary(data$household_size3)

#BCG vaccination 
table(data$qs2_bcg.factor)

#time since vaccination 
summary(data$time_since_bcg)

#travel to TB high risk area in last 12 months 
table(data$qs1_travel)
#3/90

#median number of reactors since 2011
summary(data$reactors_10years)

#median number of reactors with lesions since 2011
summary(data$lesions_10years)



######
#Figures 

#Figure 1
#mbovis=read_excel("release/data/M.bovis_tables_and_figures.xlsx",sheet = 4,skip=4)
mbovis=read_excel("data/M.bovis_tables_and_figures.xlsx",sheet = 4,skip=4)

names(mbovis)[1] = "year"
mbovis=mbovis[1:23,]
view(mbovis)

mbovis %>%
  mutate(year=as.factor(year)) %>%
  mutate(England = as.numeric(England)) %>%
  ggplot(aes(x=year,y=England)) + 
  geom_bar(stat='identity') +
  theme(axis.text = element_text(size = 10),axis.title = element_text(size = 20)) + 
  xlab("Year") + 
  ylab("Culture-positive M. bovis cases, England") 


#ggsave("~/University of Bristol/grp-zoo - Documents/zTB/Manuscripts/zooTBstudy/Mbovis.png")



#Figure 2 
#apha vs questionnaire data 
#Filter APHA data to last 2 years to match self-reported reactor data as per questionnaire 

#Note plot looks slightly different to the manuscript because for participants with more than one CPH, an average is plotted for them. 
data %>%
  ggplot(aes(x=qs3_reactors_numeric,y=reactors_2years)) + 
  geom_abline(intercept = 0,col="grey") + 
  geom_point(size=2,aes(shape="Reactors",color="Reactors")) + 
  geom_point(aes(x=qs3_lesions,y=lesions_2years,shape="Lesions",color="Lesions"),size=2) + 
  xlab("# Reactors/lesions (questionnaire)") + 
  ylab("# Reactors/lesions (APHA data)") +
  theme_bw(base_size = 14) +
  theme(legend.position = c(.99, .99),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(-5, 5, 0, 0), 
        legend.box.background = element_rect(size=0,fill="black")) +
  labs(shape=NULL,color=NULL)

 

#Figure 3
igra_data <- data %>% select(ps_id, IGRA_result_posneg, IGRA_Nil_units, IGRA_TB1MinusNil_units, IGRA_TB2MinusNil_units, IGRA_MitogenMinusNil_units, testingwave,
                             TB1, TB2, TB2minusTB1, Nil25per, TB1aboveNil25per, TB2aboveNil25per, TB1PosNeg, TB2PosNeg)


head(igra_data)

igra_long2 <- pivot_longer(igra_data, cols = c(IGRA_Nil_units, IGRA_TB1MinusNil_units, IGRA_TB2MinusNil_units, IGRA_MitogenMinusNil_units, 
                                               TB1, TB2, TB2minusTB1),
                          names_to = "IGRA_category", values_to = "IU", names_prefix = "IGRA_")



igra_long2 %>% 
  #filter(!IGRA_category == "MitogenMinusNil_units") %>% #! drops variable 
  filter(IGRA_category == "TB1MinusNil_units" | IGRA_category == "TB2MinusNil_units") %>% 
  mutate(IGRA_category = recode(IGRA_category,"TB1MinusNil_units"="TB Antigen 1","TB2MinusNil_units"="TB Antigen 2")) %>%
  mutate(minIU = -min(IU,na.rm = TRUE)*1.5) %>% #ADDED na.rm=TRUE
  mutate(IU = IU + minIU) %>%
  arrange(IU)%>%
  mutate(count1=1) %>%
  group_by(IGRA_category) %>%
  mutate(count=cumsum(count1)) %>%
  ungroup() %>%
  ggplot(aes(x=IU,y=count)) + 
  #geom_line() +
  geom_point(aes(shape=IGRA_result_posneg),size=3,alpha=0.7) + 
  facet_grid(.~IGRA_category) +
  xlab("IFN-gamma (International Units/ml)") + 
  ylab("Number of participants")+
  theme(axis.text.x=element_text(size=10, angle=0, vjust=0.3),
        axis.text.y=element_text(size=10),
        plot.title=element_text(size=10)) + #changes angle and size of your x-axis tick labels 
  theme(axis.title.y = element_text(size = 8)) +
  theme(axis.title.x = element_blank()) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_text(angle=-40, hjust=0, vjust = 1, size =10)) +
  theme(axis.title.y = element_text(angle=90, vjust = 0.5)) + 
  geom_vline(xintercept = c(0.35), linetype = "dashed") + 
  scale_x_continuous(breaks=c(-0.1,0,0.1,1,4)+0.12,labels=c(-0.1,0,0.1,1,4),trans='log10') + 
  theme_bw(base_size = 14) + 
  theme(legend.position = "none",axis.text = element_text(size = 14),axis.title = element_text(size = 20),strip.text = element_text(size = 20)) 
