###Validate 
# 1. PLCOM2012 6y--------------Lcmodels
# 2. LCRAT 5y -----------------Lcmodels
# 3. LCDRAT 5y-----------------Lcmodels
# 4. Bach 5y ------------------adjusted from Lcmodels, change 10-y to 5-y
# 5. HUNT 6y-------------------manual, parameters were provided by HUNT team
# 6. UCLD 5y---------------------publicly available script in Python
# 7. UCLI 5y---------------------publicly available script in Python
# 8. LLPV2 5y -----------------manual, parameters were provided by UK team
# 9. LLPV3 5y -----------------manual, parameters were provided by UK team
# 10. OWL 5y ------------------OWL function, choose the 5-y prediction


#1. PLCOM2012 6y: Tammemagi MC, Katki HA, Hocking WG, et al. Selection criteria for lung-cancer screening. N Engl J Med 2013
#2-3. LCRAT/LCDRAT:Katki HA, Kovalchik SA, Berg CD, Cheung LC, Chaturvedi AK. Development and validation of risk models to select ever-smokers for CT lung cancer screening. JAMA 2016
#4. Bach: Bach PB, Kattan MW, Thornquist MD, et al. Variations in lung cancer risk among smokers. J Natl Cancer Inst 2003
#Summary paper: Katki HA, Kovalchik SA, Petito LC, Cheung LC, Jacobs E, Jemal A, Berg CD, Chaturvedi AK. Implications of nine risk prediction models for selecting ever-smokers for computed tomography lung cancer screening. Ann Intern Med 2018
# Lcmodels download website: https://dceg.cancer.gov/tools/risk-assessment/lcmodels

# 5 HUNT model
#Markaki M, Tsamardinos I, Langhammer A, Lagani V, Hveem K, Røe OD. A Validated Clinical Risk Prediction Model for Lung Cancer in Smokers of All Ages and Exposure Types: A HUNT Study. EBioMedicine 2018
#Calculator website: http://mensxmachina.org/en/hunt-ntnu-lung-cancer-risk-calculator/

data<-data %>% mutate(gender_hunt=case_when(gender==1~0,gender==0~1),
                                        age_hunt=100/age,
                                        packyears_hunt=log(packyears+1),
                                        quit_years_hunt=log(quit_years+1),
                                        bmi_hunt=log(bmi+1),
                                        shs_hunt=log(shs_hours+1))

data<-data %>% mutate(XB_logistic=1.18203062+0.31573217*gender_hunt-1.98496138*age_hunt+
                        1.11994217*packyears_hunt-0.24019955*quit_years_hunt-1.70238304*bmi_hunt+
                                          0.49212668*cough+ 0.0807242*shs_hunt-0.04002877*intensity)
data<-data %>% mutate(riskHUNT_logistic=1/(1+exp(-XB_logistic)))


# 6-7 UCLD UCLI
#Callender T, Imrie F, Cebere B, et al. Assessing eligibility for lung cancer screening using parsimonious ensemble machine learning models: A development and validation study. PLoS medicine 2023
#Script website: https://github.com/callta/lung-cancer-models

# 8-9 LLPV2 LLPV3
#Field JK, Vulkan D, Davies MPA, Duffy SW, Gabe R. Liverpool Lung Project lung cancer risk stratification model: calibration and prospective validation. Thorax 2021

data<-data %>%mutate(smkyears.cat1=ifelse(years_smoked<20,1,0), 
                                           smkyears.cat2=ifelse(years_smoked>=20 & years_smoked<40,1,0),
                                           smkyears.cat3=ifelse(years_smoked>=40 &years_smoked<60,1,0),
                                           smkyears.cat4=ifelse(years_smoked>=60,1,0),
                                           early_onset1=ifelse(family_history_late==1,1,0),
                                           early_onset2=ifelse(family_history_late==2,1,0)) 

data <- data%>%mutate(sumB=0.7692*smkyears.cat1+1.4516*smkyears.cat2+2.5072*smkyears.cat3+2.7243*smkyears.cat4+
                                              0.6025*copd+
                                              0.6754*history_cancer+
                                              0.6343*asbestos+
                                              0.7034*early_onset1+0.1677*early_onset2)

data<-data %>% mutate(agecat= as.numeric(cut(age, breaks = c(0,45,50,55,60,65,70,75,80,Inf),right = FALSE,labels = c(1,2,3,4,5,6,7,8,9))))
table(data$agecat,exclude = NULL) 

data<-data %>%  mutate(alpha_age_llpv2=case_when(
  (sex==1 & agecat==1)~(-9.06),
  (sex==1 & agecat==2)~(-8.16),
  (sex==1 & agecat==3)~(-7.31),
  (sex==1 & agecat==4)~(-6.63),
  (sex==1 & agecat==5)~(-5.97),
  (sex==1 & agecat==6)~(-5.56),
  (sex==1 & agecat==7)~(-5.31),
  (sex==1 & agecat==8)~(-4.83),
  (sex==1 & agecat==9)~(-4.68),
  
  (sex==2 & agecat==1)~(-9.9),
  (sex==2 & agecat==2)~(-8.06),
  (sex==2 & agecat==3)~(-7.46),
  (sex==2 & agecat==4)~(-6.50),
  (sex==2 & agecat==5)~(-6.22),
  (sex==2 & agecat==6)~(-5.99),
  (sex==2 & agecat==7)~(-5.49),
  (sex==2 & agecat==8)~(-5.23),
  (sex==2 & agecat==9)~(-5.42)))

data<-data %>%  mutate(alpha_age_llpv3=case_when(
  (sex==1 & agecat==1)~(-9.84),
  (sex==1 & agecat==2)~(-8.94),
  (sex==1 & agecat==3)~(-8.09),
  (sex==1 & agecat==4)~(-7.41),
  (sex==1 & agecat==5)~(-6.75),
  (sex==1 & agecat==6)~(-6.34),
  (sex==1 & agecat==7)~(-6.09),
  (sex==1 & agecat==8)~(-5.61),
  (sex==1 & agecat==9)~(-5.46),
  
  (sex==2 & agecat==1)~(-10.37),
  (sex==2 & agecat==2)~(-8.53),
  (sex==2 & agecat==3)~(-7.93),
  (sex==2 & agecat==4)~(-6.97),
  (sex==2 & agecat==5)~(-6.69),
  (sex==2 & agecat==6)~ (-6.46),
  (sex==2 & agecat==7)~(-5.96),
  (sex==2 & agecat==8)~(-5.70),
  (sex==2 & agecat==9)~(-5.89)))

data$t1 <- 5-data$age%%5-0.5
data$t2 <- data$age%%5+0.5
data$age_t1 <- data$age+data$t1
data$age_t2 <- data$age_t1+data$t2
data<-data %>% mutate(agecat_t2= as.numeric(cut(age_t2, breaks = c(0,45,50,55,60,65,70,75,80,Inf),right = FALSE,labels = c(1,2,3,4,5,6,7,8,9))))
table(data$agecat_t2,exclude = NULL) 
data<-data %>%  mutate(alpha_age_llpv2_t2=case_when(
  (sex==1 & agecat_t2==1)~(-9.06),
  (sex==1 & agecat_t2==2)~(-8.16),
  (sex==1 & agecat_t2==3)~(-7.31),
  (sex==1 & agecat_t2==4)~(-6.63),
  (sex==1 & agecat_t2==5)~(-5.97),
  (sex==1 & agecat_t2==6)~(-5.56),
  (sex==1 & agecat_t2==7)~(-5.31),
  (sex==1 & agecat_t2==8)~(-4.83),
  (sex==1 & agecat_t2==9)~(-4.68),
  
  (sex==2 & agecat_t2==1)~(-9.9),
  (sex==2 & agecat_t2==2)~(-8.06),
  (sex==2 & agecat_t2==3)~(-7.46),
  (sex==2 & agecat_t2==4)~(-6.50),
  (sex==2 & agecat_t2==5)~(-6.22),
  (sex==2 & agecat_t2==6)~(-5.99),
  (sex==2 & agecat_t2==7)~(-5.49),
  (sex==2 & agecat_t2==8)~(-5.23),
  (sex==2 & agecat_t2==9)~(-5.42)))

data<-data %>%  mutate(alpha_age_llpv3_t2=case_when(
  (sex==1 & agecat_t2==1)~(-9.84),
  (sex==1 & agecat_t2==2)~(-8.94),
  (sex==1 & agecat_t2==3)~(-8.09),
  (sex==1 & agecat_t2==4)~(-7.41),
  (sex==1 & agecat_t2==5)~(-6.75),
  (sex==1 & agecat_t2==6)~(-6.34),
  (sex==1 & agecat_t2==7)~(-6.09),
  (sex==1 & agecat_t2==8)~(-5.61),
  (sex==1 & agecat_t2==9)~(-5.46),
  
  (sex==2 & agecat_t2==1)~(-10.37),
  (sex==2 & agecat_t2==2)~(-8.53),
  (sex==2 & agecat_t2==3)~(-7.93),
  (sex==2 & agecat_t2==4)~(-6.97),
  (sex==2 & agecat_t2==5)~(-6.69),
  (sex==2 & agecat_t2==6)~ (-6.46),
  (sex==2 & agecat_t2==7)~(-5.96),
  (sex==2 & agecat_t2==8)~(-5.70),
  (sex==2 & agecat_t2==9)~(-5.89)))

data$sum_alpha_llpv2<-(data$t1*data$alpha_age_llpv2+data$t2*data$alpha_age_llpv2_t2)/5
data$sum_alpha_llpv3<-(data$t1*data$alpha_age_llpv3+data$t2*data$alpha_age_llpv3_t2)/5

data <- data%>%mutate(riskLLPV2=1/(1+exp(-(sum_alpha_llpv2+sumB))),
                      riskLLPV3=1/(1+exp(-(sum_alpha_llpv3+sumB)))) 

# 10 OWL
#Pan Z, Zhang R, Shen S, et al. OWL: an optimized and independently validated machine learning prediction model for lung cancer screening based on the UK Biobank, PLCO, and NLST populations. EBioMedicine 2023; 88: 104443.
#Script website: https://github.com/WeiLab4Research/OWL



