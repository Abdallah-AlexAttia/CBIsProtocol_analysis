# CBIsProtocol_analysis
This was a prospective, observational Eastern Association for the Surgery of Trauma-sponsored multicenter trial. 
# Libraries
library(haven)
library(dplyr)
library(finalfit)
library(ggplot2)
library(lme4)
# read sav file 
dta <- read_sav(file.choose())


dta$bedsize <- ""

# Removed step in compliance with policies.

table(dta$bedsize,dta$redcap_data_access_group,useNA = "always")


dta$unos_region <- as.factor(dta$unos_region)
dta$bedsize <- as.factor(dta$bedsize)
dta$protocol <- as.factor(dta$protocol)
dta$registered_donor <- as.factor(dta$registered_donor)
dta$organ_donation <- as.factor(dta$organ_donation)



dep <- "organ_donation"
explanatory <- c("bedsize","protocol","registered_donor")

dta1 <- as.data.frame(dta)
# drop unused levels in bedsize
dta1$bedsize <- droplevels(dta1$bedsize)
class(dta1)
mod <- finalfit(.data = dta1,dependent  = dep,explanatory = explanatory,random_effect = "unos_region",metrics = T)
plot <- finalfit_plot(.data = dta1,dependent  = dep,explanatory = explanatory,
                      dependent_label= "Organ Donation",random_effect = "unos_region")
plot2 <- finalfit_plot(.data = dta1,dependent  = dep,explanatory = explanatory,
                      dependent_label= "Organ Donation")

# print column names as list 
print(paste0('"',names(dta1),'"',collapse = ","))

dta1$protocol <- as.factor(dta1$protocol)
table(dta1$protocol,dta1$bedsize)
dta1$bedsize <- as.factor(dta1$bedsize)
dta1$bedsize <- droplevels(dta1$bedsize)
# drop empty observation from bedsize
dta1 <- dta1 %>% filter(bedsize != "")
# make NA in bedsize empty 
dta1$bedsize[is.na(dta1$bedsize)] <- ""
d <- "bedsize"

ex <- c("age","race___1","registered_donor","protocol","age","sex","race___1","race___2","race___3",
        "race___4","race___5","race___6","race___7","race___8","race___9","race___10",
        "comorbidities___1","comorbidities___2","comorbidities___3","comorbidities___4",
        "comorbidities___5","comorbidities___6","comorbidities___7","comorbidities___8","comorbidities___9",
        "comorbidities___10","comorbidities___11","comorbidities___12","comorbidities___13","comorbidities___14",
        "comorbidities___15","comorbidities___16","comorbidities___17","comorbidities___18","comorbidities___19",
        "comorbidities___20","covid","advance_directive","dnr","care_withdrawal","complications___1",
        "complications___2","complications___3","complications___17","complications___18","complications___4",
        "complications___5","complications___6","complications___7","complications___8","complications___9",
        "complications___10","complications___11","complications___15","complications___16","complications___12",
        "complications___13","blunt_pen_mixed","moi___1","moi___11","moi___2","moi___3","moi___4","moi___5","moi___6",
        "moi___7","moi___8","moi___9","moi___10","intent","iss","n_iss","ais_head","ais_face","ais_thorax","ais_abdomen",
        "ais_extremity","injury_complete","transport_type","bls_or_als","EMS_air_or_grd","phgcs","phgcs_e","phgcs_v","phgcs_m",
        "phsbp","phdbs","phmap","phhr","phrr","phtemp","ph_meds___0","ph_meds___1","ph_meds_2","ph_meds___3","ph_meds___4",
        "ph_procedures___1","ph_procedures___2","ph_procedures___3","ph_procedures___4","ph_procedures___5","ph_procedures___6",
        "ph_procedures___7","ph_procedures___8","ph_procedures___9","ph_procedures___10","ph_procedures___11","ph_procedures___12",
        "ph_down_time","transporttime","prehospital_complete","edgcs","edgcs_e","edgcs_v","edgcs_m","edsbp","eddbp","edmap","edhr",
        "edrr","edtemp","marshall_classification","ed_interventions___0","ed_interventions___1","ed_interventions___2",
        "ed_interventions___3","ed_interventions___4","ed_interventions___5","ed_interventions___6","ed_interventions___7",
        "ed_interventions___8","ed_interventions___9","ed_interventions___10","ed_interventions___11","ed_interventions___12",
        "ed_interventions___13","ed_interventions___14","ed_interventions___15","ed_interventions___16","ed_interventions___17",
        "emergency_department_complete","neurosurgery___0","neurosurgery___1","neurosurgery___2","neurosurgery___3","neurosurgery___4",
        "neurosurgery___5","vascular_surgery___0","vascular_surgery___1",
        "vascular_surgery___2","vascular_surgery___3","vascular_surgery___4","surgeries___0",
        "surgeries___1","surgeries___2","surgeries___3","surgeries___4","surgeries___5","laparotomy_open",
        "open_abd_management","first_surgery_day","time_to_surgery","intraop_bloodloss","intraop_fluids","intraop_bloodproducts",
        "or_fluid_balance_absolute_value","negative_fluid_balance","surgical_management_complete","abx","periop_abx","abx_days",
        "abx_which___1","abx_which___2","abx_which___3","abx_which___5","abx_which___6","other_abx","vitamink","vitamink_total",
        "mannitol","mtp_activated","mtp_transfused","mtp_activated_day","mtp_initated_mins","wholeblood",
        "wholeblood8hrs","wholeblood24hrs","wholebloodmins","prbcs","prbcs8hrs","prbcs24hrs","prbcsmins",
        "platelets","platelets8hrs","platelets24hrs","plateletsmins","ffp","ffp8hrs","ffp24hrs","ffpmins",
        "cryo","cryo8hrs","cryo24hrs","cryomins","pcc3","pcc4","pcc4_8hrs",
        "pcc4_24hrs","pcc4_mins","fluids","fluids8hrs","fluids24hrs","fluidsmins","fluid_name___1","fluid_name___2",
        "fluid_name___3","fluid_name___4","fluid_name___5","methylprednisone_administered","methylprednisone_drip",
        "methylprednisone_dosage","methylprednisone_freq","methylprednisone_days","vasopressin_administered",
        "desmopressin_administered",
        "desmopressin_drip","desmopressin_days")

tb1 <- summary_factorlist(.data= dta1,dependent = d,explanatory = ex,p=T,add_col_totals = T,add_row_totals = T,column = T,cont = "median")
openxlsx::write.xlsx(tb1, file = "bedsize.xlsx")



# Fit a mixed-effects logistic regression model
model <- glmer(organ_donation ~ bedsize + protocol + registered_donor + (1 | unos_region),
               family = binomial, data = dta1)
summary(model)
# Extract fixed effects (log odds ratios) and confidence intervals
fixed_effects <- fixef(model)
conf_intervals <- confint(model)

# Transform log odds ratios to relative risks
relative_risks <- exp(fixed_effects)

library(sjPlot)
plot_model(model, type = "est", show.ci = TRUE,ri.nr = "re")


