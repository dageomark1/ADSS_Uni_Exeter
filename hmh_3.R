library(tidyverse)
library(foreign)
hmh <- read.spss("D:/Bureau/University of Exeter UK/Modules/Dissertation Project/Dataset/fulfilling_lives_cdf_noids.sav")

hmh <- as.data.frame(hmh)
dim(hmh)

####
convert_quarter_to_date <- function(x) {
  if (is.na(x) || trimws(x) == "") return(NA)
  parts <- unlist(strsplit(x, "Q|Q\\s|\\sQ"))
  parts <- trimws(parts)
  if (length(parts) != 2) return(NA)
  q <- as.integer(parts[1])
  y <- as.integer(parts[2])
  if (is.na(q) || is.na(y)) return(NA)
  month <- c("01", "04", "07", "10")[q]
  as.Date(paste0(y, "-", month, "-01"))
}


# Add dates back to hmh_clean (for merging)
hmh <- hmh %>%
  mutate(
    MNSTART_date = sapply(MNSTART, convert_quarter_to_date),
    MNEND_date = sapply(MNEND, convert_quarter_to_date)
  ) %>%
  mutate(
    MNSTART_date = as.Date(MNSTART_date, origin = "1970-01-01"),
    MNEND_date = as.Date(MNEND_date, origin = "1970-01-01"),
    across(
      matches("^NDTDATE_\\d+$"),
      ~ as.Date(sapply(., convert_quarter_to_date), origin = "1970-01-01")
    ),
    across(
      matches("^OSDATE_\\d+$"),
      ~ as.Date(sapply(., convert_quarter_to_date), origin = "1970-01-01")
    )
  )

## selection of unique individuals for the analysis
hmh_data <- hmh %>%
  filter(enrolment_num == 1)

## integrated_model from support service accessed variables
vars_treatment <- hmh_data %>%
  select(starts_with("S3_"),  # Housing support
         starts_with("S23_"), # Substance misuse support
         starts_with("S36_"), # Mental health support
         starts_with("ACCM_OTSH_"), # Housing Stability
         starts_with("ACCM_OTPS_"),
         starts_with("ACCM_RS_"),
         starts_with("ACCM_FF_"),
         starts_with("ACCM_TA_"),
         starts_with("ACCM_SA_"),
         starts_with("ACCM_RSP_"),
         starts_with("ACCM_OTH_"),
         starts_with("S2_"),starts_with("S4_"),
         starts_with("S1_"),starts_with("S6_"),starts_with("S7_"), 
         starts_with("S8_"), starts_with("S9_"), starts_with("S10_"), 
         starts_with("S11_"), starts_with("S12_"), starts_with("S13_"),
         starts_with("S14_"), starts_with("S15_"), starts_with("S16_"), 
         starts_with("S17_"), starts_with("S18_"), starts_with("S19_"), 
         starts_with("S20_"), starts_with("S21_"), starts_with("S22_"), 
         starts_with("S24_"), starts_with("S25_"), starts_with("S5_"),
         starts_with("S26_"), starts_with("S27_"), starts_with("S28_"), 
         starts_with("S29_"), starts_with("S30_"), starts_with("S31_"),
         starts_with("S32_"), starts_with("S33_"), starts_with("S34_"), 
         starts_with("S35_"), starts_with("S37_"))

# Experience of disadvantage at baseline
exp_disadv_baseline <- hmh_data %>%
  select(HLNS, # homeless when engage with the programme
         OFFD, # history of offending
         SUBS, # misusing substances
         MTHL #  mental health need
         )

# Service use
serv_use <- hmh_data %>%
  select(starts_with("EVIC0"), # Number of evictions from tenancies this quarter
         starts_with("ARR0"), # Number of arrests this quarter
         starts_with("PRIS0"), # Number of nights spent in prison this quarter
         starts_with("PAE0"), # Number of presentations at A&E this quarter
         starts_with("CMHT0"), # Number of face to face contacts with community mental health team this quarter
         starts_with("MHOPA0"), # Number of mental health outpatient attendances this quarter
         starts_with("MHINP0"), # Number of days spent as a mental health inpatient this quarter
         starts_with("DAS0"), # Number of face to face contacts with drug and alcohol services this quarter
         DEST
         )


## other COVARIATES
covariates <- hmh_data %>%
  select(SEX, ETHBANDED, DIS,
         EDUCBANDED, AGEBANDED)

# TIME STRUCTURE
vars_time <- hmh_data %>%
  select(MNSTART_date, MNEND_date,YEARQUARTEREND,starts_with("NDTDATE_"),
         starts_with("OSDATE_"))  # dates of engagement reviews

#### Homelessness Outcomes Star
hos_vars <- hmh_data %>%
  select(starts_with("OSMOT_"),
         starts_with("OSSELF_"),
         starts_with("OSMM_"),
         starts_with("OSSOC_"),
         starts_with("OSSUB_"),
         starts_with("OSEMH_"),
         starts_with("OSPHY_"),
         starts_with("OSMTM_"),
         starts_with("OSACC_"),
         starts_with("OSOFF_"))

## New Directions Team Assessment (NDTA)
ndta_vars <- hmh_data %>%
  select(starts_with("NDTENG_"),
         starts_with("NDTISH_"),
         starts_with("NDTUSH_"),
         starts_with("NDTRTO_"),
         starts_with("NDTRFO_"),
         starts_with("NDTSAX_"),
         starts_with("NDTSE_"),
         starts_with("NDTADA_"),
         starts_with("NDTIC_"),
         starts_with("NDTHNG_"))

# COMBINE all selected variables
hmh_vars <- bind_cols(
  hmh_data %>% select(enrolment_num),  # unique identifier
  vars_treatment,
  serv_use,
  exp_disadv_baseline,
  covariates,
  vars_time,
  ndta_vars,
  hos_vars
)





# Save to inspect
write_csv(hmh_vars, "hmh_vars.csv")
###

library(dplyr)

clean_labels <- function(x) {
  x <- as.character(x)
  x[x %in% c("Not collected/refused", "Not collected/not known", 
             "Not collected","NULL","Not known","999","998",
             "Not applicable - no end date",
             "Unknown destination")] <- NA
  
  # Gender
  x[x == "Male"] <- 1
  x[x == "Female"] <- 2
  
  # Ethnicity
  x[x == "White British"] <- 1
  x[x == "Minority ethnic group"] <- 2
  
  # Disability
  x[x == "Disabled"] <- 1
  x[x == "Not disabled"] <- 2
  
  # Age band
  x[x == "18-29"] <- 1
  x[x == "30-39"] <- 2
  x[x == "40-49"] <- 3
  x[x == "50-59"] <- 4
  x[x == "60+"]   <- 5
  
  ## Homelessness outcomes star
  
  # OSMOT
  # Journey of Change motivational scale
  x[x == "I am not interested in talking to workers or making changes"] <- 1
  x[x == "I am fed up with how my life is, but nothing can be done about it"] <- 2
  x[x == "I have had enough of living like this and want things to change"] <- 3
  x[x == "I will go along with help if other people can stop my life being like this"] <- 4
  x[x == "I can see that I need to do things by myself to get to where I want to be"] <- 5
  x[x == "I am doing things to help me get to where I want to be  - with help"] <- 6
  x[x == "I am seeing some benefits from the positive choices I am making"] <- 7
  x[x == "I am getting there and I know it is down to me, but I want support to keep it up"] <- 8
  x[x == "Mostly I feel confident in my choices, I just need a bit of hep now and then"] <- 9
  x[x == "I am motivated and take responsibility for myself, independent of the service"] <- 10
  
  # OSSELF
  # Journey of Change – Self-Care / Daily Living
  x[x == "I don't keep myself warm, clean and fed, but I don't want to talk about it"] <- 1
  x[x == "I don't look after myself well. Occassionally I worry about that"] <- 2
  x[x == "I don’t want to live like this any more - I need help"] <- 3
  x[x == "If others can help me look after myself better, I'll go along with it"] <- 4
  x[x == "I want to be able to do more for myself"] <- 5
  x[x == "I am doing some things to look after my home for myself"] <- 6
  x[x == "I'm building my living skills"] <- 7
  x[x == "I can look after myself and my home well enough but need support to keep it going"] <- 8
  x[x == "I can look after my home and myself well, with occasional help"] <- 9
  x[x == "I can look after my home and myself without outside help"] <- 10
  
  
  # OSMM
  # Journey of Change – Managing Money
  x[x == "My money is in crisis but I am ignoring the situation"] <- 1
  x[x == "My money is a mess but nothing can be done about it"] <- 2
  x[x == "I don’t want these money problems"] <- 3
  x[x == "I will go along with help if workers can sort out my money for me"] <- 4
  x[x == "I need to sort out my money, debts and benefits, and have plans in place"] <- 5
  x[x == "I am sorting out my money and trying to manage with what I've got. It's difficult"] <- 6
  x[x == "I have no major issues with money and am learning to manage, with support"] <- 7
  x[x == "I manage my money OK but sometimes need help"] <- 8
  x[x == "I manage my money well enough, with occasional support"] <- 9
  x[x == "I can manage my money well enough and don’t need support with it"] <- 10
  
  
  # OSSOC
  # Journey of Change – Social Networks and Relationships
  x[x == "I am always alone or with people who are a negative influence and I will not discuss this"] <- 1
  x[x == "I am occasionally fed up with being alone or with people who don’t help me"] <- 2
  x[x == "I am isolated or the people around me aren't good for me and I want some help"] <- 3
  x[x == "I am talking to one or more people I can trust"] <- 4
  x[x == "I know I need to find positive relationships, but it feels hard"] <- 5
  x[x == "I am doing things to build a positive social network"] <- 6
  x[x == "I am learning what works for me with social networks, friends and family"] <- 7
  x[x == "I have positive contact with people but need help to maintain this"] <- 8
  x[x == "I mostly feel connected and supported but occasionally need support"] <- 9
  x[x == "I feel connected and supported and I don’t need help in this area"] <- 10
  
  # OSPHY
  # Journey of Change – Physical Health
  x[x == "My physical health is bad but I don’t want to talk about it"] <- 1
  x[x == "My health is bad but nothing will help"] <- 2
  x[x == "I want help for pain or illness"] <- 3
  x[x == "I will go along with treatment provided"] <- 4
  x[x == "I need to take some responsibility for looking after my health"] <- 5
  x[x == "I am doing some things to look after my physical health"] <- 6
  x[x == "I am learning to look after my health"] <- 7
  x[x == "I mostly look after my health but need support to maintain this"] <- 8
  x[x == "I am learning to maintain healthy habits and taking care of my physical health"] <- 9
  x[x == "I look after my physical health and my lifestyle is reasonably healthy"] <- 10
  
  # OSSUB
  x[x == "I don’t have a problem with drugs or alcohol - although others think I do"] <- 1
  x[x == "Maybe my drug use or drinking is a problem but that's just the way it is"] <- 2
  x[x == "I need some help with my drug use or my drinking"] <- 3
  x[x == "I go along with some things to reduce the risk or harm from alcohol or drugs"] <- 4
  x[x == "I see that I need to make changes myself to tackle my drink use or drug use"] <- 5
  x[x == "I am doing some things myself to address my drug use or drinking"] <- 6
  x[x == "I am learning to manage my alcohol and/or drug use but there are a few issues"] <- 7
  x[x == "I am not using drugs or drinking problematically but need support to maintain this"] <- 8
  x[x == "I am not using drugs or drinking problematically with occasional support"] <- 9
  x[x == "I don’t have a problem with drugs or alcohol or I manage without support from a service"] <- 10
  
  
  # OSEMH
  x[x == "I often feel pretty bad but I don’t want to talk about it"] <- 1
  x[x == "I don’t like feeling like this but there is nothing anyone can do about it"] <- 2
  x[x == "I want to get out of this and feel better"] <- 3
  x[x == "I am going along with help in relation to my mental or emotional health"] <- 4
  x[x == "I believe that there are things I can do to feel better"] <- 5
  x[x == "I am trying ways to improve my emotional well-being"] <- 6
  x[x == "I am mostly on an even keel. I have ways to help myself when things feel tough"] <- 7
  x[x == "I am getting on with my life, with support"] <- 8
  x[x == "I mostly feel fine - I just need support now and then"] <- 9
  x[x == "I feel fine - emotional and mental health are not a problem for me"] <- 10
  
  # OSMTM
  # Journey of Change – Meaningful Use of Time
  x[x == "I am doing nothing with my time, or it revolves around drugs, alcohol or criminal activity"] <- 1
  x[x == "Occasionally I wish I was doing something meaningful but there's no way I could"] <- 2
  x[x == "I don’t want to spend my days doing nothing or in chaos anymore"] <- 3
  x[x == "I am going along with actions that others suggest"] <- 4
  x[x == "I know I need to take the initiative to change things"] <- 5
  x[x == "I am getting clear about what to do and taking steps towards that"] <- 6
  x[x == "I am learning what works for me and getting closer to where I want to be"] <- 7
  x[x == "I am using my time well but need support to maintain that"] <- 8
  x[x == "I use my time well but occasionally need support with it"] <- 9
  x[x == "I use my time well and don’t need any extra help"] <- 10
  
  # OSACC
  # Managing Tenancy and Accommodation
  x[x == "I am not able or not willing to comply with rules and regulations"] <- 1
  x[x == "I am not complying with the rules and occasionally worry about being evicted"] <- 2
  x[x == "I don’t want to lose my accommodation"] <- 3
  x[x == "I am going along with things to keep my accommodation"] <- 4
  x[x == "I want to make the changes I need so I can live independently"] <- 5
  x[x == "I am taking steps to be able to live independently and find a home"] <- 6
  x[x == "I am learning how to manage my tenancy and be self-reliant"] <- 7
  x[x == "I can live independently, with ongoing support"] <- 8
  x[x == "I live independently, with occasional support"] <- 9
  x[x == "I live independently and manage my own tenancy without support"] <- 10
  
  # OSOFF
  # Journey of Change – Offending
  x[x == "I am not able or willing to discuss offending"] <- 1
  x[x == "The courts or police are causing me problems"] <- 2
  x[x == "I wish I didn’t have these problems"] <- 3
  x[x == "I am going along with help to sort out issues with the law"] <- 4
  x[x == "I want to stop offending"] <- 5
  x[x == "I am taking steps to stop offending and/or deal with the consequences of offending"] <- 6
  x[x == "I understand how and why I get into trouble and how to stop it for good"] <- 7
  x[x == "I am staying within the law, with support"] <- 8
  x[x == "I am not offending, with occasional support to maintain this"] <- 9
  x[x == "I am not offending and don’t need support in this area"] <- 10
  
  ## New Directions Team Assessment (NDTA)
  # NDTENG
  x[x == "Rarely misses appointments or routine activities; always complies with reasonable requests; actively engaged in tenancy/treatment"] <- 0
  x[x == "Usually keeps appointments and routine activities; usually complies with reasonable requests; involved in tenancy/treatment"] <- 1
  x[x == "Follows through some of the time in daily routines or other activities; usually complies with reasonable requests; is sufficiently involved in tenancy/treatment to avoid sanctions but not wholly engaging with services."] <- 2
  x[x == "Non-compliant with support work or reasonable requests; does not follow daily routine, though may keep some appointments."] <- 3
  x[x == "Does not engage at all or keep appointments"] <- 4
  
  # NDTISH (Intentional Self-Harm)
  # NDTA – Risk of Deliberate Self-Harm or Suicide
  x[x == "No concerns about risk of deliberate self-harm or suicide attempt"] <- 0
  x[x == "Minor concerns about risk of deliberate self-harm or suicide attempt"] <- 1
  x[x == "Definite indicators of risk of deliberate self-harm or suicide attempt, including long term, destructive behaviour whilst aware of risks, such as heavy alcohol intake"] <- 2
  x[x == "High risk to physical safety as a result of deliberate self-harm or suicide attempt"] <- 3
  x[x == "Immediate risk to physical safety as a result of deliberate self-harm or suicide attempt"] <- 4
  
  # NDTUSH
  # NDTA – Unintentional Risk to Physical Safety
  x[x == "No concerns about unintentional risk to physical safety"] <- 0
  x[x == "Minor concerns about unintentional risk to physical safety"] <- 1
  x[x == "Definite indicators of unintentional risk to physical safety"] <- 2
  x[x == "High risk to physical safety as a result of self-neglect, unsafe behaviour or inability to maintain a safe environment"] <- 3
  x[x == "Immediate risk to physical safety as a result of self-neglect, unsafe behaviour or inability to maintain a safe environment"] <- 4
  
  # NDTRTO
  # NDTA – Risk to Others (double-weighted)
  x[x == "No concerns about risk to physical safety or property of others"] <- 0
  x[x == "Antisocial behaviour e.g. street drinking, begging, noisy neighbours"] <- 2
  x[x == "Risk to property and/or minor risk to physical safety of others"] <- 4
  x[x == "High risk to physical safety of others as a result of dangerous behaviour or offending/criminal behaviour"] <- 6
  x[x == "Immediate risk to physical safety of others as a result of dangerous behaviour or offending/criminal behaviour"] <- 8
  
  # NDTRFO
  # NDTA – Risk from Others (double-weighted)
  x[x == "No concerns about risk of abuse or exploitation from other individuals or society"] <- 0
  x[x == "Minor concerns about risk of abuse or exploitation from other individuals or society"] <- 2
  x[x == "Definite risk of abuse or exploitation from other individuals or society"] <- 4
  x[x == "Probably occurrence of abuse or exploitation from other individuals or society"] <- 6
  x[x == "Evidence of abuse or exploitation from other individuals or society"] <- 8
  
  # NDTSAX
  # NDTA – Stress and Emotional Wellbeing
  x[x == "Normal response to stressors"] <- 0
  x[x == "Somewhat reactive to stress, has some coping skills, responsive to limited intervention"] <- 1
  x[x == "Moderately reactive to stress; needs support in order to cope"] <- 2
  x[x == "Obvious reactiveness; very limited problem solving in response to stress; becomes hostile and aggressive to others"] <- 3
  x[x == "Severe reactiveness to stressors, self-destructive, antisocial, or have other outward manifestations"] <- 4
  
  # NDTSE
  # NDTA – Social Effectiveness / Social Skills
  x[x == "Social skills are within the normal range"] <- 0
  x[x == "Is generally able to carry out social interactions with minor deficits, can generally engage in give-and-take conversation with only minor disruption"] <- 1
  x[x == "Marginal social skills, sometimes creates interpersonal friction; sometimes inappropriate"] <- 2
  x[x == "Uses only minimal social skills, cannot engage in give-and-take of instrumental or social conversations; limited response to social cues; inappropriate"] <- 3
  x[x == "Lacking in almost any social skills; inappropriate response to social cues; aggressive"] <- 4
  
  #NDTADA
  # Alcohol and Drug Use Scale
  x[x == "Abstinence; no use of alcohol or drugs during rating period"] <- 0
  x[x == "Occasional use of alcohol or abuse of drugs without impairment"] <- 1
  x[x == "Some use of alcohol or abuse of drugs with some effect on functioning; sometimes inappropriate to others"] <- 2
  x[x == "Recurrent use of alcohol or abuse of drugs which causes significant effect on functioning; aggressive behaviour to others"] <- 3
  x[x == "Drug/alcohol dependence; daily abuse of alcohol or drugs which causes severe impairment of functioning; inability to function in community secondary to alcohol/drug abuse; aggressive behaviour to others; criminal activity to support alcohol or drug use"] <- 4
  
  # NDTIC
  # Impulse Control and Aggressive Behaviour Scale
  x[x == "No noteworthy incidents"] <- 0
  x[x == "Maybe one or two lapses of impulse control; minor temper outbursts/aggressive actions, such as attention-seeking behaviour which is not threatening or dangerous"] <- 1
  x[x == "Some temper outbursts/aggressive behaviour; moderate severity; at least one episode of behaviour that is dangerous or threatening"] <- 2
  x[x == "Impulsive acts which are fairly often and/or of moderate severity"] <- 3
  x[x == "Frequent and/or severe outbursts/aggressive behaviour, e.g., behaviours which could lead to criminal charges / Anti-Social Behaviour Orders / risk to or from others / property"] <- 4
  
  # NDTHNG
  # Housing Stability and Support Needs Scale
  x[x == "Settled accommodation; very low housing support needs"] <- 0
  x[x == "Settled accommodation; Living in short-term / temporary accommodation; low to medium housing support needs"] <- 1
  x[x == "Living in short-term / temporary accommodation; medium to high housing support needs"] <- 2
  x[x == "Immediate risk of loss of accommodation; living in short-term /temporary accommodation; squatting; 'sofa surfing'; high housing support needs"] <- 3
  x[x == "Rough sleeping; living in high-risk exploitative accommodation under coercive arrangements"] <- 4
  
  # DEST
  x[x=="Moved to other support (not funded through this project)"] <- 1
  x[x=="No longer requires support"] <- 2
  x[x=="Client disengaged from project"] <- 3
  x[x=="Prison"] <- 4
  x[x=="Hospital"] <- 5
  x[x=="Deceased"] <- 6
  x[x=="Moved out of area"] <- 7
  x[x=="Other"] <- 8
  x[x=="Excluded from the programme"] <- 9
  x[x=="Unknown destination"] <- 10
  x[x=="Not applicable - no end date"] <- 11
  
  
  # Education band
  x[x == "No or entry-level qualifications only"] <- 1
  x[x == "GCSEs grades A*-G and equivalents"] <- 2
  x[x == "A levels/equivalent or higher"] <- 3
  
  # Ordinal frequency buckets
  x[x == "1 to 2"] <- 1
  x[x == "3 to 4"] <- 3
  x[x == "5 to 9"] <- 5
  x[x == "10 or more"] <- 10
  x[x == "5 or more"] <- 2
  x[x == "1 or more"] <- 1
  x[x == "2 or more"] <- 2
  x[x == "3 or more"] <- 3
  x[x == "4 or more"] <- 4
  x[x == "1 to 31 (i.e. up to one month)"] <- 1
  x[x == "32 to 62 (i.e. one to two months)"] <- 2
  x[x == "63 to 93 (i.e. two to three months)"] <- 3
  x[x == "3 to 5"] <- 3
  x[x == "6 or more"] <- 4
  
  # Return as numeric
  suppressWarnings(as.numeric(x))
}

## Apply cleaning
# Generate the regex pattern for S1_ to S37_ followed by three digits
#prefixes <- paste0("S", 1:37, "_")
#pattern <- paste0("^(", paste(prefixes, collapse = "|"), ")\\d{3}$")

# Replace Yes/No with 1/0 for support and baseline binary variables
hmh_vars_transformed <- hmh_vars %>%
  mutate(
    across(matches("^(|S1_|S2_|S3_|S4_|S5_|S6_|S7_|S8_|S9_|S10_|S11_|S12_|S13_|S14_|S15_|S16_|S17_|S18_|S19_|S20_|S21_|S22_|S23_|S24_|S25_|S26_|S27_|S28_|S29_|S30_|S31_|S32_|S33_|S34_|S35_|S36_|S37_)\\d{3}$"), ~ case_when(
      . == "Yes" ~ 1,
      . == "No" ~ 0
    )),
    across(c(HLNS, OFFD, SUBS, MTHL), ~ case_when(
      . == "Yes" ~ 1,
      . == "No" ~ 0
    )),
    
    # For HOS variables, set 0s to NA 
    across(matches("^(OSSELF_|OSSOC_|OSMM_|OSSUB_|OSEMH_|OSPHY_|OSMTM_|OSACC_|
                   OSOFF_)\\d{3}$"),
           ~ na_if(as.character(.), "0"))
  )

# apply label cleaning to everything else
hmh_clean <- hmh_vars_transformed %>%
  mutate(across(-c(MNSTART_date, MNEND_date, YEARQUARTEREND, 
                   matches("^(OSDATE|NDTDATE)_")),
                clean_labels))



#####
summary(hmh_clean$DAS001)
table(hmh_clean$MNSTART_date, useNA = "ifany")


#
library(tidyverse)
hmh_long_values <- hmh_clean %>%
  pivot_longer(
    cols = matches("^(OSMOT_|OSSELF_|OSMM_|OSSOC_|OSSUB_|OSPHY_|OSEMH_|OSMTM_|OSACC_|OSOFF_|NDTENG_|NDTISH_|NDTUSH_|NDTRTO_|NDTRFO_|NDTSAX_|NDTSE_|NDTADA_|NDTIC_|NDTHNG_|ACCM_OTSH_|ACCM_OTPS_|ACCM_RS_|ACCM_FF_|ACCM_TA_|ACCM_SA_|ACCM_RSP_|ACCM_OTH_|S1_|S2_|S3_|S4_|S5_|S6_|S7_|S8_|S9_|S10_|S11_|S12_|S13_|S14_|S15_|S16_|S17_|S18_|S19_|S20_|S21_|S22_|S23_|S24_|S25_|S26_|S27_|S28_|S29_|S30_|S31_|S32_|S33_|S34_|S35_|S36_|S37_|OSSUB_|OSEMH_|DAS|MHOPA|MHINP|PAE|ARR|PRIS|EVIC|CMHT)\\d{3}$"),
    names_to = c("var", "wave"),
    names_pattern = "^([A-Z0-9_]+?)[_]?([0-9]{3})$",  # fixed pattern
    values_to = "value"
  ) %>%
  mutate(wave = as.integer(wave)) %>%
  pivot_wider(
    names_from = var,
    values_from = value,
    values_fn = list
  ) %>%
  select(-matches("^(OSDATE|NDTDATE)"))

  



#
# Pivot date columns in long format to preserve 3,552 rows
hmh_long_dates <- hmh_clean %>%
  select(enrolment_num, matches("^(OSDATE_|NDTDATE_)\\d{3}$")) %>%
  pivot_longer(
    cols = -enrolment_num,
    names_to = c("var", "wave"),
    names_pattern = "^([A-Z0-9]+)_([0-9]{3})$",
    values_to = "date"
  ) %>%
  mutate(wave = as.integer(wave)) %>%
  group_by(enrolment_num, wave, var) %>%
  summarise(date = first(date), .groups = "drop") %>%  # keeps NA, avoids list-cols
  pivot_wider(
    id_cols = c(enrolment_num, wave),
    names_from = var,
    values_from = date
  )

## Combine long format
hmh_long <- hmh_long_values %>%
  left_join(hmh_long_dates, by = c("enrolment_num", "wave")) %>%
  mutate(wave = as.integer(wave)) %>%
  filter(!is.na(OSDATE) | !is.na(NDTDATE))

# Unlist all list-columns to convert NULLs to NA
hmh_long <- hmh_long %>%
  mutate(across(where(is.list), ~ ifelse(lengths(.) == 0, NA, unlist(.))))

#
# Convert YEARQUARTEREND to Date using first month of each quarter
hmh_long <- hmh_long %>%
  mutate(YEARQUARTEREND = as.character(YEARQUARTEREND),  # convert factor to string
         YEARQUARTEREND_date = as.Date(paste0(
           substr(YEARQUARTEREND, 1, 4), "-",
           case_when(
             grepl("-1$", YEARQUARTEREND) ~ "01",
             grepl("-2$", YEARQUARTEREND) ~ "04",
             grepl("-3$", YEARQUARTEREND) ~ "07",
             grepl("-4$", YEARQUARTEREND) ~ "10",
             TRUE ~ NA_character_
           ),
           "-01"
         )))


### Filtering rows with start and end dates
hmh_long_model <- hmh_long %>%
  filter(!is.na(MNSTART_date) & !is.na(MNEND_date) & !is.na(OSDATE) &
         !is.na(NDTDATE) & !is.na(YEARQUARTEREND_date))

##
library(lubridate)

hmh_long_model <- hmh_long_model %>%
  mutate(
    # Define the actual calendar date of each wave
    wave_date = as.Date("2014-01-01") + months((wave - 1) * 3),
    
    # Create time indicator: 0 = before program start, 1 = during/after
    time = if_else(wave_date < MNSTART_date, 0, 1, missing = NA_integer_)
  ) %>%
  mutate(OSOFF = na_if(OSOFF, 0))



##
hmh_final <- hmh_long_model %>%
  mutate(across(c(ACCM_OTSH,ACCM_OTPS,ACCM_RS,ACCM_FF,ACCM_TA,ACCM_SA,
                  ACCM_RSP,ACCM_OTH), as.numeric),
         across(any_of(c(paste0("S", 1:37), "DIS", "AGEBANDED", "DAS", "PAE",
                         "PRIS", "ARR", "EVIC", "CMHT", "MHINP", "MHOPA",
                         "OSEMH", "OSSUB", "DEST", "NDTENG", "NDTISH", "NDTUSH",
                         "NDTRTO", "NDTRFO", "NDTSAX", "NDTSE", "NDTADA",
                         "NDTIC", "NDTHNG", "OSMOT", "OSSELF", "OSMM", "OSSOC",
                         "OSPHY", "OSMTM", "OSACC", "OSOFF")), as.factor),
         across(c(enrolment_num),as.integer),
         across(c(EDUCBANDED, ETHBANDED, SEX, MTHL, SUBS, OFFD, HLNS), as.factor),
         across(c(NDTDATE, OSDATE, YEARQUARTEREND_date, MNSTART_date, MNEND_date), 
                ~ as.Date(.)))

############################################################
# HOS domains
hos_domains <- c(
  "Motivation", "SelfCare", "SocialNetworks",
  "SubstanceMisuse", "PhysicalHealth", "EmotionalHealth",
  "MeaningfulTime", "Accommodation", "Offending", "ManagingMoney"
)

# NDTA domains
ndta_domains <- c(
  "Engagement", "IntentionalSelfHarm", "UnintentionalSelfHarm",
  "RiskToOthers", "RiskFromOthers", "StressAnxiety",
  "SocialEffectiveness", "AlcoholDrugAbuse", "ImpulseControl",
  "Housing"
)

##### rename hos to match hos_domains
rename_and_convert <- function(data) {
  data %>%
    rename(
      # HOS domains
      Motivation = OSMOT,
      SelfCare = OSSELF,
      ManagingMoney = OSMM,
      SocialNetworks = OSSOC,
      SubstanceMisuse = OSSUB,
      PhysicalHealth = OSPHY,
      EmotionalHealth = OSEMH,
      MeaningfulTime = OSMTM,
      Accommodation = OSACC,
      Offending = OSOFF,
      
      # NDTA domains
      Engagement = NDTENG,
      IntentionalSelfHarm = NDTISH,
      UnintentionalSelfHarm = NDTUSH,
      RiskToOthers = NDTRTO,
      RiskFromOthers = NDTRFO,
      StressAnxiety = NDTSAX,
      SocialEffectiveness = NDTSE,
      AlcoholDrugAbuse = NDTADA,
      ImpulseControl = NDTIC,
      Housing = NDTHNG
    ) %>%
    mutate(across(all_of(hos_domains), ~ as.numeric(as.character(.)))) %>%
    mutate(across(all_of(ndta_domains), ~ as.numeric(as.character(.)))) %>%
    mutate(HOS_total = rowSums(across(all_of(hos_domains)), na.rm = FALSE),
           NDT_total = rowSums(across(all_of(ndta_domains)), na.rm = FALSE))
}


############## 
# All support service variables: S1 to S37 except S27
support_vars <- paste0("S", setdiff(1:37, 27))

# Create composite support score
create_support_score <- function(data, support_vars) {
  data %>%
    mutate(across(all_of(support_vars), ~ as.numeric(as.character(.)))) %>%
    mutate(
      support_score = rowSums(across(all_of(support_vars)), na.rm = FALSE)
    )
}


# Apply both transformations and save the result
hmh_final <- hmh_final %>%
  rename_and_convert() %>%
  create_support_score(support_vars)

### transform back to original structure
hmh_final <- hmh_final %>%
  mutate(across(all_of(hos_domains), ~ factor(.))) %>%
  mutate(across(all_of(ndta_domains), ~ factor(.))) %>%
  mutate(across(all_of(support_vars), ~ factor(.))) %>%
  mutate(time = factor(time)
  )




############ Data Imputation
library(mice)

# Step 1: Initial imputation setup
ini <- mice(hmh_final, maxit = 0)
meth <- ini$method

# Step 2: Variables to exclude from imputation
exclude_vars <- c("wave", "wave_date", "MNSTART_date", "MNEND_date", 
                  "enrolment_num", "time", "OSDATE", "NDTDATE", "YEARQUARTEREND")

meth[exclude_vars] <- ""

# Step 3: Assign "rf" method to remaining variables (except for IDs, dates, etc.)
vars_to_impute <- setdiff(names(hmh_final), exclude_vars)
meth[vars_to_impute] <- "rf"

# (Optional) Set method only for factor/numeric variables
# meth[sapply(hmh_final, is.factor)] <- "rf"
# meth[sapply(hmh_final, is.numeric)] <- "rf"

# Step 4: Run the imputation
imp_rf <- mice(hmh_final, method = meth, m = 20, maxit = 10, seed = 1234)


## imputed dataset 1
imp1 <- complete(imp_rf, 1)
## imputed dataset 2


## all the imputed dataset in long format
imp_all <- complete(imp, action = "long")



### imputation datasets long format
imp_all <- rename_and_convert(imp_all)

######### support level
library(mice)
library(dplyr)

# Step 1: Convert imp_rf to long format
imp_rf_long <- complete(imp_rf, "long", include = TRUE)

# Step 2: Calculate support_level in each .imp group using median
imp_rf_long <- imp_rf_long %>%
  group_by(.imp) %>%
  mutate(
    support_level = factor(
      if_else(
        support_score >= median(support_score),
        "High", "Low"
      )
    )
  ) %>%
  ungroup()

# Step 3: Convert back to mids object
imp_rf_final <- as.mids(imp_rf_long)

## imputed dataset 1
imp1 <- complete(imp_rf_final, 1)

############## 
# Remove all rows with NA from hmh_complete
hmh_no_na <- hmh_final %>%
  filter(complete.cases(.))


## convert the HOS to numeric
hmh_no_na <- hmh_no_na %>%
  mutate(across(all_of(hos_domains), ~ as.numeric(as.character(.)))) %>%
  mutate(across(all_of(ndta_domains), ~ as.numeric(as.character(.))))

###
hmh_no_na <- hmh_no_na %>%
  mutate(
    support_level = factor(
      if_else(
        support_score >= median(support_score),
        "High", "Low"
      )
    )
  )

############### Missing Values Summary Table ###########################
library(dplyr)
library(tidyr)
library(gt)

# Calculate missing counts and percentages
missing_summary <- hmh_final %>%
  summarise(across(all_of(c(hos_domains, ndta_domains)),
                   list(
                     MissingCount = ~ sum(is.na(.)),
                     MissingPercent = ~ mean(is.na(.)) * 100
                   ),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("Domain", ".value"),
               names_sep = "_") %>%
  mutate(MissingPercent = round(MissingPercent, 3))

# Split and tag groups
hos_block  <- filter(missing_summary, Domain %in% hos_domains)
ndta_block <- filter(missing_summary, Domain %in% ndta_domains)

hos_block$Group  <- "HOS Domains"
ndta_block$Group <- "NDTA Domains"

# Combine and arrange by group
final_table <- bind_rows(hos_block, ndta_block) %>%
  arrange(Group, desc(MissingPercent))

# Build gt table
final_table %>%
  gt(groupname_col = "Group") %>%
  cols_label(
    Domain = "Domain",
    MissingCount = "Missing Count",
    MissingPercent = "Missing (%)"
  ) %>%
  fmt_number(
    columns = c(MissingPercent),  # updated syntax
    decimals = 3
  ) %>%
  tab_options(
    table.font.size = "small",
    column_labels.font.weight = "bold",
    data_row.padding = px(3)
  )

######################### imputed dataset 1 #################################


#### Proportion of Beneficiaries' needs and support(imputed vs completed cases)
library(tidyverse)
library(gt)

# 1. Compute Needs Summary: imp1 (imputed data)
# 1. Compute Needs Summary: imp1 (imputed data)
needs_summary_imp1 <- imp1 %>%
  mutate(total_needs = as.numeric(as.character(HLNS)) +
           as.numeric(as.character(OFFD)) +
           as.numeric(as.character(SUBS)) +
           as.numeric(as.character(MTHL))) %>%
  count(total_needs) %>%
  mutate(percent = n / sum(n),
         Dataset = "Imputed",
         Type = "Total Needs",
         Category = as.character(total_needs)) %>%
  dplyr::select(Dataset, Type, Category, Count = n, Percent = percent)

# 2. Support summary for imp1
support_summary_imp1 <- imp1 %>%
  count(support_level) %>%
  mutate(percent = n / sum(n),
         Dataset = "Imputed",
         Type = "Support Level",
         Category = support_level) %>%
  dplyr::select(Dataset, Type, Category, Count = n, Percent = percent)

# 3. Needs summary for hmh_no_na
needs_summary_cc <- hmh_no_na %>%
  mutate(total_needs = as.numeric(as.character(HLNS)) +
           as.numeric(as.character(OFFD)) +
           as.numeric(as.character(SUBS)) +
           as.numeric(as.character(MTHL))) %>%
  count(total_needs) %>%
  mutate(percent = n / sum(n),
         Dataset = "Complete Case",
         Type = "Total Needs",
         Category = as.character(total_needs)) %>%
 dplyr::select(Dataset, Type, Category, Count = n, Percent = percent)

# 4. Support summary for hmh_no_na
support_summary_cc <- hmh_no_na %>%
  count(support_level) %>%
  mutate(percent = n / sum(n),
         Dataset = "Complete Case",
         Type = "Support Level",
         Category = support_level) %>%
  dplyr::select(Dataset, Type, Category, Count = n, Percent = percent)

# 5. Combine everything
## Create the clean summary table with explicit ordering
combined_all <- bind_rows(
  needs_summary_cc %>% mutate(group_label = "1_Complete Case: Total Needs"),
  support_summary_cc %>% mutate(group_label = "2_Complete Case: Support Level"),
  needs_summary_imp1 %>% mutate(group_label = "3_Imputed: Total Needs"),
  support_summary_imp1 %>% mutate(group_label = "4_Imputed: Support Level")
) %>%
  mutate(
    Percent = scales::percent(Percent, accuracy = 0.1),
    # Clean up the group labels for display
    display_label = gsub("^\\d_", "", group_label)
  ) %>%
  arrange(group_label)  # Sort by our ordered labels

# Define the order we want for row groups
group_order <- c(
  "1_Complete Case: Total Needs",
  "2_Complete Case: Support Level", 
  "3_Imputed: Total Needs",
  "4_Imputed: Support Level"
)

# Create clean display labels (without the numbers)
display_labels <- setNames(
  c("Complete Case: Total Needs", 
    "Complete Case: Support Level",
    "Imputed: Total Needs",
    "Imputed: Support Level"),
  group_order
)

# Final GT table with enforced ordering
combined_all %>%
  dplyr::select(display_label, Category, Count, Percent) %>%
  gt(groupname_col = "display_label") %>%
  tab_header(
    title = "Comparison of Needs and Support Levels (Imputed vs Complete Case)"
  ) %>%
  cols_label(
    Category = "Category",
    Count = "Count",
    Percent = "Percent"
  ) %>%
  tab_options(
    table.font.size = "small",
    table.align = "center"
  ) %>%
  opt_row_striping() %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )




#############################  Star plots #############################
######
library(ggradar)
library(fmsb)
library(scales)
library(tidyverse)
library(tibble)
library(ggiraphExtra)
###
make_star_plot <- function(data, domains, title, subtitle = NULL,
                           group_var = "time",
                           colors = c("#1b9e77", "#d95f02"), 
                           labels = c("Baseline", "Follow-up"),
                           draw_legend = TRUE,
                           cex.main = 1.2,
                           vlcex = 0.8,
                           calcex = 0.8,# NEW (for NDTA 0–8):
                           max_score = 10,
                           axis_breaks = NULL) {
  
  row_data <- data %>%
    group_by(.data[[group_var]]) %>%
    summarise(across(all_of(domains), ~ mean(as.numeric(as.character(.x)), na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(cols = -all_of(group_var), names_to = "Domain", values_to = "Score") %>%
    mutate(Group = factor(.data[[group_var]], labels = labels))
  
  wide <- row_data %>%
    dplyr::select(Group, Domain, Score) %>%
    pivot_wider(names_from = Domain, values_from = Score) %>%
    as.data.frame()
  
  radar_df <- rbind(rep(max_score, length(domains)),
                    rep(0, length(domains)),
                    wide[, -1, drop=FALSE])
  
  lab_seq <- if (!is.null(axis_breaks)) axis_breaks else {
    if (max_score == 10) seq(0, 10, 2) else 0:max_score
  }
  
  radarchart(
    radar_df,
    axistype = 1,
    pcol = colors[1:nrow(wide)],
    pfcol = adjustcolor(colors[1:nrow(wide)], alpha.f = 0.2),
    plwd = 2,
    plty = 1,
    cglcol = "grey", cglty = 1, axislabcol = "grey30",
    caxislabels = lab_seq,
    vlcex = vlcex,
    calcex = calcex
  )
  
  mtext(title, side = 3, line = 2, cex = cex.main, font = 2)
  
  if (!is.null(subtitle)) {
    mtext(subtitle, side = 3, line = 0.5, cex = 0.9, col = "gray40")
  }
  
  if (draw_legend && (length(labels) > 1 || labels[1] != "")) {
    legend("bottomright", legend = labels[1:nrow(wide)], 
           col = colors[1:nrow(wide)], lty = 1, lwd = 2,
           bty = "n", horiz = FALSE, cex = 0.8)
  }
}



# Calculate and print mean HOS scores
# Compute total HOS score per person
hos_total_baseline <- imp1 %>%
  filter(time == 0) %>%
  filter(if_all(all_of(hos_domains), ~ !is.na(.)))

# Calculate mean total score
mean_total_hos <- mean(hos_total_baseline$HOS_total, na.rm = TRUE)

# Print result
print(round(mean_total_hos,1))

# Calculate sample size
nrow(hos_total_baseline)


# Calculate and print mean HOS scores
# Compute total ndta score per person
ndta_total_baseline <- imp1 %>%
  filter(time == 0) %>%
  filter(if_all(all_of(ndta_domains), ~ !is.na(.)))

# Calculate mean ndta total score
mean_total_ndta <- mean(ndta_total_baseline$NDT_total, na.rm = TRUE)

# Print result
print(round(mean_total_ndta,1))

# Calculate sample size
nrow(ndta_total_baseline)


# Calculate and print mean HOS scores
# Compute total HOS score per person
hos_total_na_baseline <- hmh_no_na %>%
  filter(time == 0) %>%
  filter(if_all(all_of(hos_domains), ~ !is.na(.)))

# Calculate mean total score
mean_total_hos_na <- mean(hos_total_na_baseline$HOS_total, na.rm = TRUE)

# Print result
print(round(mean_total_hos_na,1))

# Calculate sample size
nrow(hos_total_na_baseline)

# Calculate and print mean ndta scores for complete cases
# Compute total ndta score per person
ndta_total_na_baseline <- hmh_no_na %>%
  filter(time == 0) %>%
  filter(if_all(all_of(ndta_domains), ~ !is.na(.)))

# Calculate mean total score
mean_total_ndta_na <- mean(ndta_total_na_baseline$NDT_total, na.rm = TRUE)

# Print result
print(round(mean_total_ndta_na,1))

# Calculate sample size
nrow(ndta_total_na_baseline)

############################### BASELINE PLOTS ########################
#### HOS Baseline plot with imp1 and complete case

# 1. Close all existing graphics devices
# Close existing devices
while (dev.cur() > 1) dev.off()

pdf("hos_star_plot_two_panel.pdf", width = 14, height = 7, pointsize = 12)

# Layout: 2 plots on top, legend spanning bottom
layout(matrix(c(1, 2,
                3, 3), 
              nrow = 2, byrow = TRUE),
       heights = c(8, 2)) # top row plots taller than bottom row

par(oma = c(1, 3, 3, 3), mar = c(2, 0, 3, 0))

# Panel 1 - Imputed data
make_star_plot(
  data = imp1 %>% filter(time == 0),
  domains = hos_domains,
  title = "HOS Scores at Baseline",
  subtitle = "Imputed Data (n = 15,967)",
  labels = " ",
  draw_legend = FALSE,
  cex.main = 1.5,
  vlcex = 1.1,
  calcex = 1.0,
  max_score = 10
)

# Panel 2 - Complete case data
make_star_plot(
  data = hmh_no_na %>% filter(time == 0),
  domains = hos_domains,
  title = "HOS Scores at Baseline",
  subtitle = "Complete Case (n = 648)",
  labels = " ",
  draw_legend = FALSE,
  cex.main = 1.5,
  vlcex = 1.1,
  calcex = 1.0,
  max_score = 10
)

# Legend panel
par(mar = c(0, 2, 0, 2))
plot.new()
legend("center",
       legend = c(
         "Baseline",
         "Journey of Change:",
         "1-2.4 = Stuck | 2.5-4.4 = Accepting Help",
         "4.5-6.4 = Believing | 6.5-8.4 = Learning",
         "8.5-10 = Self-Reliance"
       ),
       col = c("#1b9e77", NA, NA, NA, NA),
       lty = c(1, NA, NA, NA, NA), 
       lwd = c(2, NA, NA, NA, NA),
       bty = "n", 
       horiz = FALSE, 
       cex = 1.2,
       text.col = "black",
       xpd = NA)

dev.off()

############### NDTA Baseline plot with imp1 and complete case ###########

# Now create the plot with proper text sizing
# 1. Close all existing graphics devices
# Close existing devices
while (dev.cur() > 1) dev.off()

pdf("ndt_star_plot_two_panel.pdf", width = 14, height = 7, pointsize = 12)

# Layout: 2 plots on top, legend spanning bottom
layout(matrix(c(1, 2,
                3, 3), 
              nrow = 2, byrow = TRUE),
       heights = c(8, 2)) # top row plots taller than bottom row

par(oma = c(1, 3, 3, 3), mar = c(2, 0, 3, 0))

# Panel 1 - Imputed data
make_star_plot(
  data = imp1 %>% filter(time == 0),
  domains = ndta_domains,
  title = "NDTA Scores at Baseline",
  subtitle = "Imputed Data (n = 15,967)",
  labels = " ",
  draw_legend = FALSE,
  cex.main = 1.5,
  vlcex = 1.1,
  calcex = 1.0,
  max_score = 8,            # <-- different from HOS
  axis_breaks = 0:8
)

# Panel 2 - Complete case data
make_star_plot(
  data = hmh_no_na %>% filter(time == 0),
  domains = ndta_domains,
  title = "NDTA Scores at Baseline",
  subtitle = "Complete Case (n = 648)",
  labels = " ",
  draw_legend = FALSE,
  cex.main = 1.5,
  vlcex = 1.1,
  calcex = 1.0,
  max_score = 8,            # <-- different from HOS
  axis_breaks = 0:8
)

# Legend panel
par(mar = c(0, 2, 0, 2))
plot.new()
legend("center",
       legend = c(
         "Baseline"
       ),
       col = c("#1b9e77", NA, NA, NA),
       lty = c(1, NA, NA, NA), 
       lwd = c(2, NA, NA, NA),
       bty = "n", 
       horiz = FALSE, 
       cex = 1.2,
       text.col = "black",
       xpd = NA)

dev.off()

##############################################################################
####################### Baseline vs 2 year Follow up #########################
#############################################################################
# Baseline
prepare_plot_data <- function(imp_data) {
  baseline_data <- imp_data %>% 
    filter(time == 0)
  
  followup_data <- imp_data %>%
    filter(time == 1) %>%
    mutate(
      diff_days = abs(as.numeric(difftime(wave_date, MNSTART_date, units = "days")) - 730)
    ) %>%
    group_by(enrolment_num) %>%
    slice_min(order_by = diff_days, n = 1) %>%
    ungroup()
  
  # Combine baseline and 2 year follow-up
  hmh_2yr_flwup <- bind_rows(
    baseline_data %>% mutate(period = 0),
    followup_data %>% mutate(period = 1)
  )
  
  return(hmh_2yr_flwup)
}

### plot data from imp1 to imp20
for (i in 1:20) {
  imp_data <- complete(imp_rf_final, i)
  assign(paste0("hmh_2yr_flwup_imp", i), prepare_plot_data(imp_data))
}


###
# Calculate and print mean HOS scores
# Compute total HOS score per person
hos_total_baseline.2yrs <- hmh_2yr_flwup_imp1 %>%
  filter(time == 0) %>%
  filter(if_all(all_of(hos_domains), ~ !is.na(.)))

# Calculate mean total score
mean_total_hos.2yrs <- mean(hos_total_baseline.2yrs$HOS_total, na.rm = TRUE)

# Print result
print(round(mean_total_hos.2yrs,1))

# Calculate sample size
nrow(hos_total_baseline.2yrs)


# Calculate and print mean HOS scores
# Compute total ndta score per person
ndta_total_baseline.2yrs <- hmh_2yr_flwup_imp1 %>%
  filter(time == 0) %>%
  filter(if_all(all_of(ndta_domains), ~ !is.na(.)))

# Calculate mean ndta total score
mean_total_ndta.2yrs <- mean(ndta_total_baseline.2yrs$NDT_total, na.rm = TRUE)

# Print result
print(round(mean_total_ndta.2yrs,1))

# Calculate sample size
nrow(ndta_total_baseline.2yrs)

############################# Complete cases #################################
# Baseline for complete cases
baseline_data_na <- hmh_no_na %>% filter(time == 0)

# Approx. 2 years after MNSTART_date
# Find follow-up wave closest to 2 years per person
followup_data_na <- hmh_no_na %>%
  filter(time == 1) %>%
  mutate(
    diff_days = abs(as.numeric(difftime(wave_date, MNSTART_date, units = "days")) - 730)
  ) %>%
  group_by(enrolment_num) %>%
  slice_min(order_by = diff_days, n = 1) %>%
  ungroup()

# Combine
hmh_2yrs_flwup_na <- bind_rows(
  baseline_data_na %>% mutate(period = 0),
  followup_data_na %>% mutate(period = 1)
)

# Calculate and print mean HOS scores
# Compute total HOS score per person
hos_total.2yrs_na_baseline <- hmh_2yrs_flwup_na %>%
  filter(time == 0) %>%
  filter(if_all(all_of(hos_domains), ~ !is.na(.)))

# Calculate mean total score
mean_total_hos.2yrs_na <- mean(hos_total.2yrs_na_baseline$HOS_total, na.rm = TRUE)

# Print result
print(round(mean_total_hos.2yrs_na,1))

# Calculate sample size
nrow(hos_total.2yrs_na_baseline)

# Calculate and print mean ndta scores for complete cases
# Compute total ndta score per person
ndta_total_na_baseline <- hmh_no_na %>%
  filter(time == 0) %>%
  filter(if_all(all_of(ndta_domains), ~ !is.na(.)))

# Calculate mean total score
mean_total_ndta_na <- mean(ndta_total_na_baseline$NDT_total, na.rm = TRUE)

# Print result
print(round(mean_total_ndta_na,1))

# Calculate sample size
nrow(ndta_total_na_baseline)


########################### 2 year followup plots ###############

#### HOS Baseline plot with imp1 and complete case

# 1. Close all existing graphics devices
# Close existing devices
while (dev.cur() > 1) dev.off()

pdf("hos_star_plot_2yr.pdf", width = 14, height = 7, pointsize = 12)

# Layout: 2 plots on top, legend spanning bottom
layout(matrix(c(1, 2,
                3, 3), 
              nrow = 2, byrow = TRUE),
       heights = c(8, 2)) # top row plots taller than bottom row

par(oma = c(1, 3, 3, 3), mar = c(2, 0, 3, 0))

# Panel 1 - Imputed data
make_star_plot(
  data = hmh_2yr_flwup_imp1,
  domains = hos_domains,
  title = "HOS Scores: Baseline vs 2-Year Follow-up",
  subtitle = "Imputed Data (n = 15,967)",
  labels = " ",
  draw_legend = FALSE,
  cex.main = 1.5,
  vlcex = 1.1,
  calcex = 1.0,
  max_score = 10
)

# Panel 2 - Complete case data
make_star_plot(
  data = hmh_2yrs_flwup_na,
  domains = hos_domains,
  title = "HOS Scores: Baseline vs 2-Year Follow-up",
  subtitle = "Complete Case (n = 648)",
  labels = " ",
  draw_legend = FALSE,
  cex.main = 1.5,
  vlcex = 1.1,
  calcex = 1.0,
  max_score = 10
)

# Legend panel
par(mar = c(0, 2, 0, 2))
plot.new()
legend("center",
       legend = c(
         "Baseline","2-Year Follow-up",
         "Journey of Change:",
         "1-2.4 = Stuck | 2.5-4.4 = Accepting Help",
         "4.5-6.4 = Believing | 6.5-8.4 = Learning",
         "8.5-10 = Self-Reliance"
       ),
       col = c("#1b9e77", "#d95f02", NA, NA, NA,NA),
       lty = c(1, 1, NA, NA, NA,NA), 
       lwd = c(2, 2, NA, NA, NA,NA),
       bty = "n", 
       horiz = FALSE, 
       cex = 1.2,
       text.col = "black",
       xpd = NA)

dev.off()

############### NDTA Baseline plot with imp1 and complete case ###########

# Now create the plot with proper text sizing
# 1. Close all existing graphics devices
# Close existing devices
while (dev.cur() > 1) dev.off()

pdf("ndt_star_plot_2yr.pdf", width = 14, height = 7, pointsize = 12)

# Layout: 2 plots on top, legend spanning bottom
layout(matrix(c(1, 2,
                3, 3), 
              nrow = 2, byrow = TRUE),
       heights = c(8, 2)) # top row plots taller than bottom row

par(oma = c(1, 3, 3, 3), mar = c(2, 0, 3, 0))

# Panel 1 - Imputed data
make_star_plot(
  data = hmh_2yr_flwup_imp1,
  domains = ndta_domains,
  title = "NDTA Scores: Baseline vs 2-Year Follow-up",
  subtitle = "Imputed Data (n = 15,967)",
  labels = " ",
  draw_legend = FALSE,
  cex.main = 1.5,
  vlcex = 1.1,
  calcex = 1.0,
  max_score = 8,            # <-- different from HOS
  axis_breaks = 0:8
)

# Panel 2 - Complete case data
make_star_plot(
  data = hmh_2yrs_flwup_na,
  domains = ndta_domains,
  title = "NDTA Scores: Baseline vs 2-Year Follow-up",
  subtitle = "Complete Case (n = 648)",
  labels = " ",
  draw_legend = FALSE,
  cex.main = 1.5,
  vlcex = 1.1,
  calcex = 1.0,
  max_score = 8,            # <-- different from HOS
  axis_breaks = 0:8
)

# Legend panel
par(mar = c(0, 2, 0, 2))
plot.new()
legend("center",
       legend = c(
         "Baseline","2-Year Follow-up"
       ),
       col = c("#1b9e77", "#d95f02", NA, NA, NA),
       lty = c(1, 1, NA, NA, NA), 
       lwd = c(2, 2, NA, NA, NA),
       bty = "n", 
       horiz = FALSE, 
       cex = 1.2,
       text.col = "black",
       xpd = NA)

dev.off()

################## Baseline and full duration ##########################
# Calculate and print mean HOS scores
# Compute total HOS score per person
hos_total_full <- imp1 %>%
  filter(if_all(all_of(hos_domains), ~ !is.na(.)))

# Calculate mean total score
mean_total_hos_full <- mean(hos_total_full$HOS_total, na.rm = TRUE)

# Print result
print(round(mean_total_hos_full,1))

# Calculate sample size
nrow(hos_total_full)


# Calculate and print mean NDTA scores
# Compute total ndta score per person
ndta_total_full <- imp1 %>%
  filter(if_all(all_of(ndta_domains), ~ !is.na(.)))

# Calculate mean ndta total score
mean_total_ndta_full <- mean(ndta_total_full$NDT_total, na.rm = TRUE)

# Print result
print(round(mean_total_ndta_full,1))

# Calculate sample size
nrow(ndta_total_full)


# Calculate and print mean HOS scores for Complete Case
# Compute total HOS score per person
hos_total_na_full <- hmh_no_na %>%
  filter(if_all(all_of(hos_domains), ~ !is.na(.)))

# Calculate mean total score
mean_total_hos_na_full <- mean(hos_total_na_full$HOS_total, na.rm = TRUE)

# Print result
print(round(mean_total_hos_na_full,1))

# Calculate sample size
nrow(hos_total_na_full)

# Calculate and print mean ndta scores for complete cases
# Compute total ndta score per person
ndta_total_na_full <- hmh_no_na %>%
  filter(if_all(all_of(ndta_domains), ~ !is.na(.)))

# Calculate mean total score
mean_total_ndta_na_full <- mean(ndta_total_na_full$NDT_total, na.rm = TRUE)

# Print result
print(round(mean_total_ndta_na_full,1))

# Calculate sample size
nrow(ndta_total_na_full)


#####################################################################
################## PLOTS BASELINE vs FOLLOW-UP ######################
#####################################################################
#### HOS Baseline plot with imp and complete case

# 1. Close all existing graphics devices
# Close existing devices
while (dev.cur() > 1) dev.off()

pdf("hos_star_plot_full.pdf", width = 14, height = 7, pointsize = 12)

# Layout: 2 plots on top, legend spanning bottom
layout(matrix(c(1, 2,
                3, 3), 
              nrow = 2, byrow = TRUE),
       heights = c(8, 2)) # top row plots taller than bottom row

par(oma = c(1, 3, 3, 3), mar = c(2, 0, 3, 0))

# Panel 1 - Imputed data
make_star_plot(
  data = imp1,
  domains = hos_domains,
  title = "HOS Scores: Baseline vs Follow-up",
  subtitle = "Imputed Data (n = 19,840)",
  labels = " ",
  draw_legend = FALSE,
  cex.main = 1.5,
  vlcex = 1.1,
  calcex = 1.0,
  max_score = 10
)

# Panel 2 - Complete case data
make_star_plot(
  data = hmh_no_na,
  domains = hos_domains,
  title = "HOS Scores: Baseline vs Follow-up",
  subtitle = "Complete Case (n = 741)",
  labels = " ",
  draw_legend = FALSE,
  cex.main = 1.5,
  vlcex = 1.1,
  calcex = 1.0,
  max_score = 10
)

# Legend panel
par(mar = c(0, 2, 0, 2))
plot.new()
legend("center",
       legend = c(
         "Baseline","Follow-up",
         "Journey of Change:",
         "1-2.4 = Stuck | 2.5-4.4 = Accepting Help",
         "4.5-6.4 = Believing | 6.5-8.4 = Learning",
         "8.5-10 = Self-Reliance"
       ),
       col = c("#1b9e77", "#d95f02", NA, NA, NA,NA),
       lty = c(1, 1, NA, NA, NA,NA), 
       lwd = c(2, 2, NA, NA, NA,NA),
       bty = "n", 
       horiz = FALSE, 
       cex = 1.2,
       text.col = "black",
       xpd = NA)

dev.off()

############### NDTA Baseline plot with imp1 and complete case ###########

# Now create the plot with proper text sizing
# 1. Close all existing graphics devices
# Close existing devices
while (dev.cur() > 1) dev.off()

pdf("ndt_star_plot_full.pdf", width = 14, height = 7, pointsize = 12)

# Layout: 2 plots on top, legend spanning bottom
layout(matrix(c(1, 2,
                3, 3), 
              nrow = 2, byrow = TRUE),
       heights = c(8, 2)) # top row plots taller than bottom row

par(oma = c(1, 3, 3, 3), mar = c(2, 0, 3, 0))

# Panel 1 - Imputed data
make_star_plot(
  data = imp1,
  domains = ndta_domains,
  title = "NDTA Scores: Baseline vs Follow-up",
  subtitle = "Imputed Data (n = 19,840)",
  labels = " ",
  draw_legend = FALSE,
  cex.main = 1.5,
  vlcex = 1.1,
  calcex = 1.0,
  max_score = 8,            # <-- different from HOS
  axis_breaks = 0:8
)

# Panel 2 - Complete case data
make_star_plot(
  data = hmh_no_na,
  domains = ndta_domains,
  title = "NDTA Scores: Baseline vs Follow-up",
  subtitle = "Complete Case (n = 741)",
  labels = " ",
  draw_legend = FALSE,
  cex.main = 1.5,
  vlcex = 1.1,
  calcex = 1.0,
  max_score = 8,            # <-- different from HOS
  axis_breaks = 0:8
)

# Legend panel
par(mar = c(0, 2, 0, 2))
plot.new()
legend("center",
       legend = c(
         "Baseline","Follow-up"
       ),
       col = c("#1b9e77", "#d95f02", NA, NA, NA),
       lty = c(1, 1, NA, NA, NA), 
       lwd = c(2, 2, NA, NA, NA),
       bty = "n", 
       horiz = FALSE, 
       cex = 1.2,
       text.col = "black",
       xpd = NA)

dev.off()
########################################################################


##############################################################################
###################### BASELINE PLOTS all imputations ########################

# Calculate and print mean HOS scores
# Compute total HOS score per person
hos_total_baseline_all <- imp_all %>%
  filter(time == 0) %>%
  filter(if_all(all_of(hos_domains), ~ !is.na(.)))
  

# Calculate mean total score
mean_total_hos_all <- mean(hos_total_baseline_all$HOS_total, na.rm = TRUE)

# Print result
print(round(mean_total_hos_all,1))

# Calculate sample size
nrow(hos_total_baseline_all)

# Calculate and print mean NDTA scores
# Compute total NDTA score per person
ndta_total_baseline_all <- imp_all %>%
  filter(time == 0) %>%
  filter(if_all(all_of(ndta_domains), ~ !is.na(.)))


# Calculate mean total score
mean_total_ndta_all <- mean(ndta_total_baseline_all$HOS_total, na.rm = TRUE)

# Print result
print(round(mean_total_ndta_all,1))

# Calculate sample size
nrow(ndta_total_baseline_all)


#### HOS Baseline plot with all the imputations
make_star_plot(
  data = imp_all %>% filter(time == 0),
  domains = hos_domains,
  title = "HOS Scores at Baseline",
  subtitle = "All Imputations",
  labels = "Baseline"
)

#### NDTA Baseline plot with all the imputations
make_star_plot(
  data = imp_all %>% filter(time == 0),
  domains = ndta_domains,
  title = "NDTA Scores at Baseline",
  subtitle = "All Imputations",
  labels = "Baseline"
)



###################### Error Bars plots #################################
#############################################################################
########################## error bar-plots #####################
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

################ barplot at baseline and 2 years follow-up #################
###################### HOS Domaine ######################
##  Summary with SE – Imputation data
hos_summary <- hmh_2yr_flwup_imp1 %>%
  mutate(across(all_of(hos_domains), ~ as.numeric(as.character(.)))) %>%
  group_by(period) %>%
  summarise(across(all_of(hos_domains),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(-period, names_to = "metric", values_to = "value") %>%
  separate(metric, into = c("domain", "stat"), sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(period = factor(period, labels = c("Baseline", "Follow-up")),
         lower = mean - 1.96 * se,
         upper = mean + 1.96 * se)


## 2. Plot 1
plot1 <- ggplot(hos_summary, aes(x = domain, y = mean, fill = period)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "HOS Domain Scores at Baseline vs 2 Year Follow-up (95% CIs)",
       subtitle = "Imputation",
       x = "Domain", y = "Average Score (± SE)") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 3. Summary with SE – Complete cases
hos_summary_na <- hmh_2yrs_flwup_na %>%
  mutate(across(all_of(hos_domains), ~ as.numeric(as.character(.)))) %>%
  group_by(period) %>%
  summarise(across(all_of(hos_domains),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(-period, names_to = "metric", values_to = "value") %>%
  separate(metric, into = c("domain", "stat"), sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(period = factor(period, labels = c("Baseline", "Follow-up")),
         lower = mean - 1.96 * se,
         upper = mean + 1.96 * se)



## 4. Plot 2
plot2 <- ggplot(hos_summary_na, aes(x = domain, y = mean, fill = period)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "HOS Domain Scores at Baseline vs 2 Year Follow-up (95% CIs)",
       subtitle = "Complete Cases",
       x = "Domain", y = "Average Score (± SE)") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 5. Combine plots on the same page
hos_error_bars.2yr <- plot1 + plot2 + plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")

### export plots
ggsave("hos_error_bars.2yr.pdf", dpi = 300, width = 13, height = 6)

############# full duration ########################
library(dplyr)
library(tidyr)

# Function to calculate mean + CI
summary_stats <- function(data, domains, group_var = "time") {
  data %>%
    mutate(across(all_of(domains), ~ as.numeric(as.character(.)))) %>%  # Convert to numeric
    group_by(.data[[group_var]]) %>%
    summarise(across(all_of(domains), list(
      mean = ~mean(.x, na.rm = TRUE),
      se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
    ), .names = "{.col}_{.fn}"), .groups = "drop") %>%
    pivot_longer(-all_of(group_var), names_to = c("Domain", ".value"),
                 names_sep = "_") %>%
    mutate(
      lower = mean - 1.96 * se,
      upper = mean + 1.96 * se,
      period = factor(.data[[group_var]], labels = c("Baseline", "Follow-up"))
    )
}



## imp1
hos_stats <- summary_stats(imp1, hos_domains, group_var = "time")

##
library(ggplot2)

plot3 <- ggplot(hos_stats, aes(x = Domain, y = mean, fill = period)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(0.8), width = 0.2) +
  labs(title = "HOS Domain Scores at Baseline vs Follow-up (95% CIs)",
       subtitle = "Imputation",
       y = "Mean Score", x = "Domain") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##
## complete cases 
hos_stats_na <- summary_stats(hmh_no_na, hos_domains, group_var = "time")

##
library(ggplot2)

plot4 <- ggplot(hos_stats_na, aes(x = Domain, y = mean, fill = period)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(0.8), width = 0.2) +
  labs(title = "HOS Domain Scores at Baseline vs Follow-up (95% CIs)",
       subtitle = "Complete Cases",
       y = "Mean Score", x = "Domain") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




hos_error_bars_full <- plot3 + plot4 + plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom")

### export plots
ggsave("hos_error_bars_full.pdf", dpi = 300, width = 13, height = 6)

############### NDTA Domaine ##########################
##  Summary with SE – Imputation data
ndta_summary <- hmh_2yr_flwup_imp1 %>%
  mutate(across(all_of(ndta_domains), ~ as.numeric(as.character(.)))) %>%
  group_by(period) %>%
  summarise(across(all_of(ndta_domains),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(-period, names_to = "metric", values_to = "value") %>%
  separate(metric, into = c("domain", "stat"), sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(period = factor(period, labels = c("Baseline", "Follow-up")),
         lower = mean - 1.96 * se,
         upper = mean + 1.96 * se)


## 2. Plot 1
plot5 <- ggplot(ndta_summary, aes(x = domain, y = mean, fill = period)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "NDTA Domain Scores at Baseline vs 2 Year Follow-up (95% CIs)",
       subtitle = "Imputation",
       x = "Domain", y = "Average Score (± SE)") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 3. Summary with SE – Complete cases
ndta_summary_na <- hmh_2yrs_flwup_na %>%
  mutate(across(all_of(ndta_domains), ~ as.numeric(as.character(.)))) %>%
  group_by(period) %>%
  summarise(across(all_of(ndta_domains),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(-period, names_to = "metric", values_to = "value") %>%
  separate(metric, into = c("domain", "stat"), sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(period = factor(period, labels = c("Baseline", "Follow-up")),
         lower = mean - 1.96 * se,
         upper = mean + 1.96 * se)



## 4. Plot 2
plot6 <- ggplot(ndta_summary_na, aes(x = domain, y = mean, fill = period)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "NDTA Domain Scores at Baseline vs 2 Year Follow-up (95% CIs)",
       subtitle = "Complete Cases",
       x = "Domain", y = "Average Score (± SE)") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 5. Combine plots on the same page
ndta_error_bars.2yr <- plot5 + plot6 + plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")

### export plots
ggsave("ndta_error_bars.2yr.pdf", dpi = 300, width = 13, height = 6)

############# full duration ########################
library(dplyr)
library(tidyr)

# Function to calculate mean + CI
summary_stats <- function(data, domains, group_var = "time") {
  data %>%
    mutate(across(all_of(domains), ~ as.numeric(as.character(.)))) %>%  # Convert to numeric
    group_by(.data[[group_var]]) %>%
    summarise(across(all_of(domains), list(
      mean = ~mean(.x, na.rm = TRUE),
      se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
    ), .names = "{.col}_{.fn}"), .groups = "drop") %>%
    pivot_longer(-all_of(group_var), names_to = c("Domain", ".value"),
                 names_sep = "_") %>%
    mutate(
      lower = mean - 1.96 * se,
      upper = mean + 1.96 * se,
      period = factor(.data[[group_var]], labels = c("Baseline", "Follow-up"))
    )
}



## imp1
ndta_stats <- summary_stats(imp1, ndta_domains, group_var = "time")

##
library(ggplot2)

plot7 <- ggplot(ndta_stats, aes(x = Domain, y = mean, fill = period)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(0.8), width = 0.2) +
  labs(title = "NDTA Domain Scores at Baseline vs Follow-up (95% CIs)",
       subtitle = "Imputation",
       y = "Mean Score", x = "Domain") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##
## complete cases 
ndta_stats_na <- summary_stats(hmh_no_na, ndta_domains, group_var = "time")

##
library(ggplot2)

plot8 <- ggplot(ndta_stats_na, aes(x = Domain, y = mean, fill = period)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(0.8), width = 0.2) +
  labs(title = "NDTA Domain Scores at Baseline vs Follow-up (95% CIs)",
       subtitle = "Complete Cases",
       y = "Mean Score", x = "Domain") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




ndta_error_bars_full <- plot7 + plot8 + plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom")


### export plots
ggsave("ndta_error_bars_full.pdf", dpi = 300, width = 13, height = 6)

#####===================================================================
#################### Multivariate Models ############################
#####===================================================================

####### Multilinear Model 2 year followup ################
library(mice)
library(dplyr)
library(purrr)

# 1. Define outcome variables and predictors
ndt_items <- c("Engagement", "IntentionalSelfHarm", "UnintentionalSelfHarm", 
               "RiskToOthers", "RiskFromOthers", "StressAnxiety", "SocialEffectiveness", 
               "AlcoholDrugAbuse", "ImpulseControl", "Housing")

hos_items <- c("Motivation", "SelfCare", "ManagingMoney", "SocialNetworks", 
               "SubstanceMisuse", "EmotionalHealth", "PhysicalHealth", 
               "MeaningfulTime", "Accommodation", "Offending")

outcome_vars <- c("NDT_total", "HOS_total", ndt_items, hos_items)
predictors.2yr <- c("AGEBANDED", "SEX", "ETHBANDED", "HLNS", "OFFD", "SUBS",
                "MTHL","EDUCBANDED","support_level","time")

# Step 1: Convert to long and extract 2-year follow-up
imp_long <- complete(imp_rf_final, action = "long", include = TRUE)

extract2_2yr_data <- function(df) {
  baseline <- df %>% filter(time == 0)
  
  followup <- df %>%
    filter(time == 1) %>%
    mutate(diff_days = abs(as.numeric(difftime(wave_date, MNSTART_date, units = "days")) - 730)) %>%
    group_by(enrolment_num) %>%
    slice_min(order_by = diff_days, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  bind_rows(baseline, followup) %>%
    mutate(time = factor(time, levels = c(0, 1), labels = c("baseline", "followup")))
}

imp_2yr_long <- imp_long %>%
  group_split(.imp) %>%
  map_dfr(extract2_2yr_data)

imp_2yr_long <- imp_2yr_long %>%
  mutate(across(all_of(outcome_vars), ~ as.numeric(as.character(.))))


# 2. Split into list of imputed datasets
imp_list <- split(imp_2yr_long, imp_2yr_long$.imp)

# 3. Run linear models on each imputed dataset
fit_models <- function(outcome, data_list, predictors.2yr) {
  models <- map(data_list, function(df) {
    formula <- as.formula(paste(outcome, "~", paste(predictors.2yr, collapse = " + ")))
    lm(formula, data = df)
  })
  pooled <- mice::pool(models)
  summary(pooled)
}

# Filter out datasets where support_level has less than 2 levels
valid_imp_list <- imp_list %>% 
  keep(~ n_distinct(.x$support_level) > 1)

# 4. Run models for all outcomes and store results
# Run models only on valid datasets
all_model_results <- map(outcome_vars, ~ fit_models(.x, valid_imp_list, predictors.2yr))
names(all_model_results) <- outcome_vars

#######
# Step 2: Combine all results into one data frame
combined_results <- bind_rows(
  lapply(names(all_model_results), function(outcome) {
    df <- all_model_results[[outcome]]
    df$outcome <- outcome
    df
  }),
  .id = "model"
)

# Step 3: Reorder and round numeric columns
combined_results <- combined_results %>%
  dplyr::select(outcome, term, estimate, std.error, statistic, p.value, everything()) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))


# Suppose your combined results df is called `combined_results`

combined_results_grouped_imp.2yr <- combined_results %>%
  group_by(outcome) %>%
  mutate(
    outcome = if_else(row_number() == 1, outcome, "")
  ) %>%
  ungroup()

# Now print or export combined_results_grouped, e.g. with kable or flextable
print(combined_results_grouped_imp.2yr)
############ display
library(flextable)
flextable(combined_results_grouped_imp.2yr) %>%
  autofit()

############# R square for 2 year followup:imputed data #########
get_pooled_r2 <- function(outcome, data_list, predictors.2yr) {
  # Fit lm in each imputed dataset
  models <- purrr::map(data_list, function(df) {
    fml <- as.formula(paste(outcome, "~", paste(predictors.2yr, collapse = " + ")))
    lm(fml, data = df)
  })
  
  # Convert to mira and pool R^2
  mira_obj <- mice::as.mira(models)
  out      <- mice::pool.r.squared(mira_obj)
  
  # Make a clean data frame with Metric column from row names
  out_df <- as.data.frame(out)
  out_df$Metric <- rownames(out_df)
  rownames(out_df) <- NULL
  
  # Add outcome tag
  out_df$outcome <- outcome
  
  # Return with consistent column names
  out_df
}

# Run for all outcomes
r2_list <- purrr::map(outcome_vars, get_pooled_r2,
                      data_list = valid_imp_list,
                      predictors.2yr = predictors.2yr)

# Identify CI column names robustly (different mice versions use slightly different labels)
all_names <- unique(unlist(purrr::map(r2_list, names)))
lo_col <- intersect(c("lo 95", "lo95", "lo"), all_names)[1]
hi_col <- intersect(c("hi 95", "hi95", "hi"), all_names)[1]

# Bind and format
r2_table_imp_2yr <- dplyr::bind_rows(r2_list) %>%
  dplyr::mutate(
    est     = round(est, 3),
    `lo 95` = round(`lo 95`, 3),
    `hi 95` = round(`hi 95`, 3)
  ) %>%
  dplyr::rename(
    `R²`    = est,
    CI_low  = `lo 95`,
    CI_high = `hi 95`
  ) %>%
  dplyr::select(outcome,  `R²`, CI_low, CI_high)



r2_table_imp_2yr

# Step 4: Export to CSV
write.csv(combined_results_grouped_imp.2yr, "combined_results_grouped.2yr.csv",
          row.names = FALSE)

# Step 4: Export R^2 to CSV
write.csv(r2_table_imp_2yr, "r2_table_imp_2yr.csv",
          row.names = FALSE)


#############################################
#================ Model Validation ===============
##################################################
check_residuals_2yr <- function(outcome, data_list, predictors, imputation_num = 1) {
  df <- data_list[[imputation_num]]
  model <- lm(as.formula(paste(outcome, "~", paste(predictors, collapse = " + "))), data = df)
  
  par(mfrow = c(2, 2))
  plot(model)  # Residuals vs fitted, Q-Q, Scale-location, Residuals vs leverage
  
  # Add title at the top of the multi-panel plot
  mtext(paste("Residual Diagnostics for", outcome, "- Imputation", imputation_num),
        outer = TRUE, cex = 1.2, line = -2, font = 2)
}


# Example
### export residuals plots
while (dev.cur() > 1) dev.off()
pdf("residual_NDTA_imp_2yr.pdf", width = 13, height = 6)
check_residuals_2yr("NDT_total", valid_imp_list, predictors.2yr, 
                    imputation_num = 1)
dev.off()

while (dev.cur() > 1) dev.off()
pdf("residual_HOS_imp_2yr.pdf", width = 13, height = 6)
check_residuals_2yr("HOS_total", valid_imp_list, predictors.2yr, 
                    imputation_num = 1)
dev.off()

############## Homoscedasticity #############
library(lmtest)
library(purrr)
library(dplyr)
library(gt)

# helper to run BP for any outcome
check_bp.2yr <- function(outcome) {
  check_homoscedasticity(outcome, valid_imp_list, predictors.2yr) %>%
    mutate(outcome = outcome)
}

# run for all outcomes you want to display
outcomes_to_check <- c("NDT_total", "HOS_total",ndt_items,hos_items)  # add domains if you like

bp_all.2yr <- map_df(outcomes_to_check, check_bp.2yr)

# per-outcome summary across imputations
bp_summary.2yr <- bp_all.2yr %>%
  group_by(outcome) %>%
  summarise(
    prop_sig = mean(BP_pval < 0.05, na.rm = TRUE),
    .groups = "drop"
  )

# pretty gt tables
bp_all_gt.2yr <- bp_all.2yr %>%
  arrange(outcome, Imputation) %>%
  gt(groupname_col = "outcome") %>%
  cols_label(
    Imputation = "Imputation",
    BP_pval    = "Breusch–Pagan p-value"
  ) %>%
  fmt_number(columns = BP_pval, decimals = 4) %>%
  data_color(
    columns = BP_pval,
    colors = scales::col_bin(
      palette = c("#fde0dd", "#e0f3db"),
      bins = c(-Inf, 0.05, Inf)
    )
  ) %>%
  tab_header(title = "Breusch–Pagan Tests by Outcome and Imputation") %>%
  tab_source_note("Red cells: p < 0.05 (evidence of heteroskedasticity).")

library(dplyr)
library(gt)

threshold <- 0.90  # 75%

bp_summary_gt.2yr <- bp_summary.2yr %>%
  gt() %>%
  cols_label(
    outcome  = "Outcome",
    prop_sig = "Prop. p<0.05"
  ) %>%
  fmt_percent(columns = prop_sig, decimals = 0) %>%
  tab_header(
    title = "Breusch–Pagan Summary Across Imputations",
    subtitle = "Baseline vs Two-Year Follow-up"
  ) %>%
  # Style only the prop_sig column based on threshold
  tab_style(
    style = cell_fill(color = "#e0f3db"),
    locations = cells_body(
      columns = prop_sig,
      rows = prop_sig < threshold
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#fde0dd"),
    locations = cells_body(
      columns = prop_sig,
      rows = prop_sig >= threshold
    )
  ) %>%
  tab_source_note(md(paste0(
    "Cell shading: **green** if Prop. p<0.05 ≤ ", 
    scales::percent(threshold, accuracy = 1),
    "; **red** if Prop. p<0.05 ≥ ", 
    scales::percent(threshold, accuracy = 1), "."
  )))

bp_summary_gt.2yr



###################### Imputed data ##########################
################################################################
############# Multi-linear model: full duration  ##############
###############################################################

################ 2nd method best #############
# 1. Define outcome variables and predictors (same as before)
ndt_items <- c("Engagement", "IntentionalSelfHarm", "UnintentionalSelfHarm", 
               "RiskToOthers", "RiskFromOthers", "StressAnxiety", "SocialEffectiveness", 
               "AlcoholDrugAbuse", "ImpulseControl", "Housing")

hos_items <- c("Motivation", "SelfCare", "ManagingMoney", "SocialNetworks", 
               "SubstanceMisuse", "EmotionalHealth", "PhysicalHealth", 
               "MeaningfulTime", "Accommodation", "Offending")

outcome_vars <- c("NDT_total", "HOS_total", ndt_items, hos_items)
predictors <- c("AGEBANDED", "SEX", "ETHBANDED", "HLNS", "OFFD", "SUBS",
                "MTHL","EDUCBANDED","time","support_level")

# Step 1: Convert to long format (include all time points)
imp_long_full <- complete(imp_rf_final, action = "long", include = TRUE)

# Function to process full duration data (keeps all observations)
process_full_data <- function(df) {
  df %>%
    # Convert time to factor if desired (optional)
    mutate(time = factor(time, levels = c(0, 1), labels = c("baseline", "followup"))) %>%
    # Ensure outcomes are numeric
    mutate(across(all_of(outcome_vars), ~ as.numeric(as.character(.))))
}

# Process all imputations
imp_full_long <- imp_long_full %>%
  group_split(.imp) %>%
  map_dfr(process_full_data)

# 2. Split into list of imputed datasets
imp_list_full <- split(imp_full_long, imp_full_long$.imp)

# 3. Use the same modeling function as before
fit_models <- function(outcome, data_list, predictors) {
  models <- map(data_list, function(df) {
    formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
    lm(formula, data = df)
  })
  pooled <- mice::pool(models)
  summary(pooled)
}

# Filter out datasets where support_level has less than 2 levels
valid_imp_list.full <- imp_list_full %>% 
  keep(~ n_distinct(.x$support_level) > 1)

# 4. Run models for all outcomes
all_model_results_full <- map(outcome_vars, ~ fit_models(.x, valid_imp_list.full,
                                                         predictors))
names(all_model_results_full) <- outcome_vars


# Step 2: Combine all results into one data frame
combined_results.full <- bind_rows(
  lapply(names(all_model_results_full), function(outcome) {
    df <- all_model_results_full[[outcome]]
    df$outcome <- outcome
    df
  }),
  .id = "model"
)

# Step 3: Reorder and round numeric columns
combined_results.full <- combined_results.full %>%
  select(outcome, term, estimate, std.error, statistic, p.value, everything()) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# Suppose your combined results df is called `combined_results`

combined_results_grouped_imp.full <- combined_results.full %>%
  group_by(outcome) %>%
  mutate(
    outcome = if_else(row_number() == 1, outcome, "")
  ) %>%
  ungroup()

# Now print or export combined_results_grouped, e.g. with kable or flextable
print(combined_results_grouped.full)
############ display
library(flextable)
flextable(combined_results_grouped_imp.full) %>%
  autofit()


############# R square for full duration: imputed data #########
get_pooled_r2_imp_full <- function(outcome, data_list, predictors) {
  # Fit lm in each imputed dataset
  models <- purrr::map(data_list, function(df) {
    fml <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
    lm(fml, data = df)
  })
  
  # Convert to mira and pool R^2
  mira_obj <- mice::as.mira(models)
  out      <- mice::pool.r.squared(mira_obj)
  
  # Make a clean data frame with Metric column from row names
  out_df <- as.data.frame(out)
  out_df$Metric <- rownames(out_df)
  rownames(out_df) <- NULL
  
  # Add outcome tag
  out_df$outcome <- outcome
  
  # Return with consistent column names
  out_df
}

# Run for all outcomes
r2_list_imp_full <- purrr::map(outcome_vars, get_pooled_r2_imp_full,
                      data_list = valid_imp_list.full,
                      predictors = predictors)

# Identify CI column names robustly (different mice versions use slightly different labels)
all_names <- unique(unlist(purrr::map(r2_list_imp_full, names)))
lo_col <- intersect(c("lo 95", "lo95", "lo"), all_names)[1]
hi_col <- intersect(c("hi 95", "hi95", "hi"), all_names)[1]

# Bind and format
r2_table_imp_full <- dplyr::bind_rows(r2_list_imp_full) %>%
  dplyr::mutate(
    est     = round(est, 3),
    `lo 95` = round(`lo 95`, 3),
    `hi 95` = round(`hi 95`, 3)
  ) %>%
  dplyr::rename(
    `R²`    = est,
    CI_low  = `lo 95`,
    CI_high = `hi 95`
  ) %>%
  dplyr::select(outcome,  `R²`, CI_low, CI_high)



r2_table_imp_full

# Export R2 for imp full duration to CSV
write.csv(r2_table_imp_full, "r2_table_imp_full.csv",
          row.names = FALSE)

# Export to CSV
write.csv(combined_results_grouped_imp.full, "combined_results_grouped_imp.full.csv",
          row.names = FALSE)

####################### Imputed data ###############
#================ Model Validation: full  ===============
##################################################
check_residuals_imp_full <- function(outcome, data_list, predictors, imputation_num = 1) {
  df <- data_list[[imputation_num]]
  model <- lm(as.formula(paste(outcome, "~", paste(predictors, collapse = " + "))), data = df)
  
  par(mfrow = c(2, 2))
  plot(model)  # Residuals vs fitted, Q-Q, Scale-location, Residuals vs leverage
  
  # Add title at the top of the multi-panel plot
  mtext(paste("Residual Diagnostics for", outcome, "- Imputation", imputation_num),
        outer = TRUE, cex = 1.2, line = -2, font = 2)
}


# Example
residual_NDTA_imp_full <- check_residuals_imp_full("NDT_total", valid_imp_list.full, predictors, 
                                        imputation_num = 1)

residual_HOS_imp_full <- check_residuals_imp_full("HOS_total", valid_imp_list.full, predictors, 
                                                   imputation_num = 1)

### export residuals plots
while (dev.cur() > 1) dev.off()
pdf("residual_NDTA_imp_full.pdf", width = 13, height = 6)
check_residuals_imp_full("NDT_total", valid_imp_list.full, predictors, imputation_num = 1)
dev.off()

while (dev.cur() > 1) dev.off()
pdf("residual_HOS_imp_full.pdf", width = 13, height = 6)
check_residuals_imp_full("HOS_total", valid_imp_list.full, predictors, imputation_num = 1)
dev.off()


############## Homoscedasticity #############
library(lmtest)
library(purrr)
library(dplyr)
library(gt)

# helper to run BP for any outcome
check_bp.full <- function(outcome) {
  check_homoscedasticity(outcome, valid_imp_list.full, predictors) %>%
    mutate(outcome = outcome)
}

# run for all outcomes you want to display
outcomes_to_check <- c("NDT_total", "HOS_total",ndt_items,hos_items)  # add domains if you like

bp_all.full <- map_df(outcomes_to_check, check_bp.full)

# per-outcome summary across imputations
bp_summary.full <- bp_all.full %>%
  group_by(outcome) %>%
  summarise(
    prop_sig = mean(BP_pval < 0.05, na.rm = TRUE),
    .groups = "drop"
  )

# pretty gt tables
bp_all_gt.full <- bp_all.full %>%
  arrange(outcome, Imputation) %>%
  gt(groupname_col = "outcome") %>%
  cols_label(
    Imputation = "Imputation",
    BP_pval    = "Breusch–Pagan p-value"
  ) %>%
  fmt_number(columns = BP_pval, decimals = 4) %>%
  data_color(
    columns = BP_pval,
    colors = scales::col_bin(
      palette = c("#fde0dd", "#e0f3db"),
      bins = c(-Inf, 0.05, Inf)
    )
  ) %>%
  tab_header(title = "Breusch–Pagan Tests by Outcome and Imputation") %>%
  tab_source_note("Red cells: p < 0.05 (evidence of heteroskedasticity).")

library(dplyr)
library(gt)

threshold <- 0.90  # 75%

bp_summary_gt.full <- bp_summary.full %>%
  gt() %>%
  cols_label(
    outcome  = "Outcome",
    prop_sig = "Prop. p<0.05"
  ) %>%
  fmt_percent(columns = prop_sig, decimals = 0) %>%
  tab_header(
    title = "Breusch–Pagan Summary Across Imputations",
    subtitle = "Baseline vs Follow-up"
  ) %>%
  # Style only the prop_sig column based on threshold
  tab_style(
    style = cell_fill(color = "#e0f3db"),
    locations = cells_body(
      columns = prop_sig,
      rows = prop_sig < threshold
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#fde0dd"),
    locations = cells_body(
      columns = prop_sig,
      rows = prop_sig >= threshold
    )
  ) %>%
  tab_source_note(md(paste0(
    "Cell shading: **green** if Prop. p<0.05 ≤ ", 
    scales::percent(threshold, accuracy = 1),
    "; **red** if Prop. p<0.05 ≥ ", 
    scales::percent(threshold, accuracy = 1), "."
  )))

bp_summary_gt.full


#====================================================================
####################### Complete Case #####################################
#=====================================================================

###################### Baseline vs 2 year followup ################
# 1. Define outcome variables and predictors
ndt_items <- c("Engagement", "IntentionalSelfHarm", "UnintentionalSelfHarm", 
               "RiskToOthers", "RiskFromOthers", "StressAnxiety", "SocialEffectiveness", 
               "AlcoholDrugAbuse", "ImpulseControl", "Housing")

hos_items <- c("Motivation", "SelfCare", "ManagingMoney", "SocialNetworks", 
               "SubstanceMisuse", "EmotionalHealth", "PhysicalHealth", 
               "MeaningfulTime", "Accommodation", "Offending")

outcome_vars <- c("NDT_total", "HOS_total", ndt_items, hos_items)
predictors_cc.2yr <- c("AGEBANDED", "SEX", "ETHBANDED", "HLNS", "OFFD", "SUBS",
                    "MTHL","EDUCBANDED","support_level","time")

# Step 1: Convert to long and extract 2-year follow-up
complete_case_df.2yr <- hmh_no_na

extract2_2yr_data <- function(df) {
  baseline <- df %>% filter(time == 0)
  
  followup <- df %>%
    filter(time == 1) %>%
    mutate(diff_days = abs(as.numeric(difftime(wave_date, MNSTART_date, units = "days")) - 730)) %>%
    group_by(enrolment_num) %>%
    slice_min(order_by = diff_days, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  bind_rows(baseline, followup) %>%
    mutate(time = factor(time, levels = c(0, 1), labels = c("baseline", "followup")))
}

complete_case_df.2yr <- complete_case_df.2yr %>%
  group_split(enrolment_num) %>%
  map_dfr(extract2_2yr_data)

# Step 3: Keep only rows with complete data for the outcome and predictors
complete_case_df.2yr <- complete_case_df.2yr %>%
  mutate(across(all_of(outcome_vars), ~ as.numeric(as.character(.)))) %>%
  filter(if_all(all_of(c(outcome_vars, predictors_cc.2yr)), ~ !is.na(.)))

# Step 4: Run regressions
cc_model_results.2yr <- map(outcome_vars, function(outcome) {
  formula <- as.formula(paste(outcome, "~", paste(predictors_cc.2yr, collapse = " + ")))
  model <- lm(formula, data = complete_case_df.2yr)
  tidy(model) %>%
    mutate(outcome = outcome)
})

# Step 5: Combine and clean the results
cc_combined_results.2yr <- bind_rows(cc_model_results.2yr) %>%
  relocate(outcome) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

############ display
library(dplyr)
library(flextable)

# Step 2: Reorder and round numeric columns
cc_combined_results.2yr <- cc_combined_results.2yr %>%
  dplyr::select(outcome, term, estimate, std.error, statistic, p.value, everything()) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# Step 3: Format for grouped table display
cc_combined_results_grouped.2yr <- cc_combined_results.2yr %>%
  group_by(outcome) %>%
  mutate(
    outcome = if_else(row_number() == 1, outcome, "")
  ) %>%
  ungroup()

# Step 4: Display with flextable
flextable(cc_combined_results_grouped.2yr) %>%
  autofit()

############# R square for 2 year followup: CC data #########
get_pooled_cc.2yr_r2 <- function(outcome, data, predictors) {
    formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
    model <- lm(formula, data = data)
    r2_val <- summary(model)$r.squared
    
    tibble(
      outcome = outcome,
      Metric = "R²",
      R2 = r2_val
    )
  }

# Run for all outcomes
r2_cc.2yr_list <- map(outcome_vars, ~ get_pooled_cc.2yr_r2(.x, 
                                                           complete_case_df.2yr, 
                                                           predictors_cc.2yr))

# Combine into a single table
r2_cc.2yr_table <- bind_rows(r2_cc.2yr_list) %>%
  mutate(R2 = round(R2, 3))

# View table
r2_cc.2yr_table

# Export R2 for CC 2 year progress outcomes to CSV
write.csv(r2_cc.2yr_table, "r2_cc.2yr_table.csv",
          row.names = FALSE)

# Export to CSV
write.csv(cc_combined_results_grouped.2yr, "cc_combined_results_grouped.2yr.csv",
          row.names = FALSE)



####################### Complete Case ###############
#================ Model Validation: baseline vs 2 year followup  ===============
##################################################
check_residuals_cc_2yr <- function(outcome, data_list, predictors_cc.2yr) {
  df <- complete_case_df.2yr
  model <- lm(as.formula(paste(outcome, "~", paste(predictors_cc.2yr, collapse = " + "))), data = df)
  
  par(mfrow = c(2, 2))
  plot(model)  # Residuals vs fitted, Q-Q, Scale-location, Residuals vs leverage
  
  # Add title at the top of the multi-panel plot
  mtext(paste("Residual Diagnostics for", outcome, "- Complete Case"),
        outer = TRUE, cex = 1.2, line = -2, font = 2)
}


# Example
### export ndta residuals plots
while (dev.cur() > 1) dev.off()
pdf("residual_NDTA_cc_2yr.pdf", width = 13, height = 6)
check_residuals_cc_2yr("NDT_total", complete_case_df.2yr,
                       predictors_cc.2yr)
dev.off()

while (dev.cur() > 1) dev.off()
pdf("residual_NDTA_imp_2yr.pdf", width = 13, height = 6)
check_residuals_2yr("NDT_total", valid_imp_list, predictors.2yr, 
                    imputation_num = 1)
dev.off()


### export hos residuals plots
while (dev.cur() > 1) dev.off()
pdf("residual_HOS_cc_2yr.pdf", width = 13, height = 6)
check_residuals_cc_2yr("HOS_total", complete_case_df.2yr,
                       predictors_cc.2yr)
dev.off()

while (dev.cur() > 1) dev.off()
pdf("residual_HOS_imp_2yr.pdf", width = 13, height = 6)
check_residuals_2yr("HOS_total", valid_imp_list, predictors.2yr, 
                    imputation_num = 1)
dev.off()

#####################

while (dev.cur() > 1) dev.off()
pdf("residual_RiskFromOthers_cc_2yr.pdf", width = 13, height = 6)
check_residuals_cc_2yr("RiskFromOthers", complete_case_df.2yr,
                       predictors_cc.2yr)
dev.off()

############## Homoscedasticity #############
library(lmtest)
library(dplyr)
library(gt)
library(purrr)

# Function for complete case: run BP test once per outcome
check_bp_cc.2yr <- function(outcome) {
  formula <- as.formula(paste(outcome, "~", paste(predictors_cc.2yr, collapse = " + ")))
  model <- lm(formula, data = complete_case_df.2yr)
  bp <- bptest(model)
  tibble(outcome = outcome, BP_pval = bp$p.value)
}

# Run for all outcomes
outcomes_to_check <- c("NDT_total", "HOS_total", ndt_items, hos_items)

bp_all_cc.2yr <- map_df(outcomes_to_check, check_bp_cc.2yr)

# Summarise results (mean = same as single value here, but keeps format consistent)
bp_summary_cc.2yr <- bp_all_cc.2yr %>%
  group_by(outcome) %>%
  summarise(
    prop_sig = mean(BP_pval < 0.05, na.rm = TRUE),  # will be 0 or 1 here
    .groups = "drop"
  )

# Pretty table for all outcomes (single-test version)
bp_all_gt_cc.2yr <- bp_all_cc.2yr %>%
  gt() %>%
  cols_label(
    outcome  = "Outcome",
    BP_pval  = "Breusch–Pagan p-value"
  ) %>%
  fmt_number(columns = BP_pval, decimals = 4) %>%
  data_color(
    columns = BP_pval,
    colors = scales::col_bin(
      palette = c("#fde0dd", "#e0f3db"),
      bins = c(-Inf, 0.05, Inf)
    )
  ) %>%
  tab_header(title = "Breusch–Pagan Tests (Complete Case)") %>%
  tab_source_note("Red = p < 0.05 (evidence of heteroskedasticity); Green = p ≥ 0.05.")

# Summary table with threshold coloring
threshold <- 0.90

bp_summary_gt_cc.2yr <- bp_summary_cc.2yr %>%
  gt() %>%
  cols_label(
    outcome  = "Outcome",
    prop_sig = "Prop. p<0.05"
  ) %>%
  fmt_percent(columns = prop_sig, decimals = 0) %>%
  tab_header(
    title = "Breusch–Pagan Summary for Complete Case",
    subtitle = "Baseline vs Two-Year Follow-up"
  ) %>%
  tab_style(
    style = cell_fill(color = "#e0f3db"),
    locations = cells_body(columns = prop_sig, rows = prop_sig <= threshold)
  ) %>%
  tab_style(
    style = cell_fill(color = "#fde0dd"),
    locations = cells_body(columns = prop_sig, rows = prop_sig > threshold)
  ) %>%
  tab_source_note(md(paste0(
    "Cell shading: **green** if Prop. p<0.05 ≤ ",
    scales::percent(threshold, accuracy = 1),
    "; **red** if Prop. p<0.05 > ",
    scales::percent(threshold, accuracy = 1), "."
  )))

bp_summary_gt_cc.2yr

########### Baseline vs Full Duration: Complete case ###################
library(dplyr)
library(purrr)
library(broom)
# Step 1: Define outcomes and predictors (already done before)
ndt_items <- c("Engagement", "IntentionalSelfHarm", "UnintentionalSelfHarm", 
               "RiskToOthers", "RiskFromOthers", "StressAnxiety", "SocialEffectiveness", 
               "AlcoholDrugAbuse", "ImpulseControl", "Housing")

hos_items <- c("Motivation", "SelfCare", "ManagingMoney", "SocialNetworks", 
               "SubstanceMisuse", "EmotionalHealth", "PhysicalHealth", 
               "MeaningfulTime", "Accommodation", "Offending")

outcome_vars <- c("NDT_total", "HOS_total", ndt_items, hos_items)
predictors_cc.full <- c("AGEBANDED", "SEX", "ETHBANDED", "HLNS", "OFFD", "SUBS", 
                "MTHL", "EDUCBANDED", "time","support_level")

# Step 2: Convert imp_rf_final (first imputed dataset) to a complete data.frame
complete_case_df <- hmh_no_na  # or choose another number if needed

# Step 3: Keep only rows with complete data for the outcome and predictors
complete_case_df <- complete_case_df %>%
  mutate(across(all_of(outcome_vars), ~ as.numeric(as.character(.)))) %>%
  filter(if_all(all_of(c(outcome_vars, predictors_cc.full)), ~ !is.na(.)))

# Step 4: Run regressions
cc_model_results <- map(outcome_vars, function(outcome) {
  formula <- as.formula(paste(outcome, "~", paste(predictors_cc.full, collapse = " + ")))
  model <- lm(formula, data = complete_case_df)
  tidy(model) %>%
    mutate(outcome = outcome)
})

# Step 5: Combine and clean the results
cc_combined_results.full <- bind_rows(cc_model_results) %>%
  relocate(outcome) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

############ display
library(dplyr)
library(flextable)

# Step 6: Reorder and round numeric columns
cc_combined_results.full <- cc_combined_results.full %>%
  dplyr::select(outcome, term, estimate, std.error, statistic, p.value, everything()) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# Step 7: Format for grouped table display
cc_combined_results_grouped.full <- cc_combined_results.full %>%
  group_by(outcome) %>%
  mutate(
    outcome = if_else(row_number() == 1, outcome, "")
  ) %>%
  ungroup()

# Step 8: Display with flextable
flextable(cc_combined_results_grouped.full) %>%
  autofit()

############# R square for followup: CC data #########
get_pooled_cc_r2 <- function(outcome, data, predictors_cc.full) {
  formula <- as.formula(paste(outcome, "~", paste(predictors_cc.full, collapse = " + ")))
  model <- lm(formula, data = data)
  r2_val <- summary(model)$r.squared
  
  tibble(
    outcome = outcome,
    Metric = "R²",
    R2 = r2_val
  )
}

# Run for all outcomes
r2_cc_list <- map(outcome_vars, ~ get_pooled_cc_r2(.x, complete_case_df, 
                                                   predictors_cc.full))

# Combine into a single table
r2_cc_full_table <- bind_rows(r2_cc_list) %>%
  mutate(R2 = round(R2, 3))

# View table
r2_cc_full_table

# Export R2 for CC 2 year progress outcomes to CSV
write.csv(r2_cc_full_table, "r2_cc_full_table.csv",
          row.names = FALSE)

# Export to CSV
write.csv(cc_combined_results_grouped.full, "cc_combined_results_grouped.full.csv",
          row.names = FALSE)


#####################################################################
#================ Model Validation: baseline vs followup =============
##################################################
check_residuals_cc.full <- function(outcome, data_list, predictors_cc.full) {
  df <- complete_case_df
  model <- lm(as.formula(paste(outcome, "~", paste(predictors_cc.2yr, collapse = " + "))), data = df)
  
  par(mfrow = c(2, 2))
  plot(model)  # Residuals vs fitted, Q-Q, Scale-location, Residuals vs leverage
  
  # Add title at the top of the multi-panel plot
  mtext(paste("Residual Diagnostics for", outcome, "- Complete Case"),
        outer = TRUE, cex = 1.2, line = -2, font = 2)
}


# Example
### export ndta residuals plots
while (dev.cur() > 1) dev.off()
pdf("residual_NDTA_cc_full.pdf", width = 13, height = 6)
check_residuals_cc.full("NDT_total", complete_case_df,
                       predictors_cc.full)
dev.off()

while (dev.cur() > 1) dev.off()
pdf("residual_NDTA_imp_full.pdf", width = 13, height = 6)
check_residuals_imp_full("NDT_total", valid_imp_list.full, predictors,
                         imputation_num = 1)
dev.off()

### export hos residuals plots
while (dev.cur() > 1) dev.off()
pdf("residual_HOS_cc_full.pdf", width = 13, height = 6)
check_residuals_cc.full("HOS_total", complete_case_df,
                       predictors_cc.full)
dev.off()

while (dev.cur() > 1) dev.off()
pdf("residual_HOS_imp_full.pdf", width = 13, height = 6)
check_residuals_imp_full("HOS_total", valid_imp_list.full, predictors, 
                         imputation_num = 1)
dev.off()


while (dev.cur() > 1) dev.off()
pdf("residual_RiskFromOthers_cc_full.pdf", width = 13, height = 6)
check_residuals_cc_2yr("RiskFromOthers", complete_case_df,
                       predictors_cc.full)
dev.off()

############## Homoscedasticity #############
library(lmtest)
library(dplyr)
library(gt)
library(purrr)

# Function for complete case: run BP test once per outcome
check_bp_cc.full <- function(outcome) {
  formula <- as.formula(paste(outcome, "~", paste(predictors_cc.full,
                                                  collapse = " + ")))
  model <- lm(formula, data = complete_case_df)
  bp <- bptest(model)
  tibble(outcome = outcome, BP_pval = bp$p.value)
}

# Run for all outcomes
outcomes_to_check <- c("NDT_total", "HOS_total", ndt_items, hos_items)

bp_all_cc.full <- map_df(outcomes_to_check, check_bp_cc.full)

# Summarise results (mean = same as single value here, but keeps format consistent)
bp_summary_cc.full <- bp_all_cc.full %>%
  group_by(outcome) %>%
  summarise(
    prop_sig = mean(BP_pval < 0.05, na.rm = TRUE),  # will be 0 or 1 here
    .groups = "drop"
  )

# Pretty table for all outcomes (single-test version)
bp_all_gt_cc.full <- bp_all_cc.full %>%
  gt() %>%
  cols_label(
    outcome  = "Outcome",
    BP_pval  = "Breusch–Pagan p-value"
  ) %>%
  fmt_number(columns = BP_pval, decimals = 4) %>%
  data_color(
    columns = BP_pval,
    colors = scales::col_bin(
      palette = c("#fde0dd", "#e0f3db"),
      bins = c(-Inf, 0.05, Inf)
    )
  ) %>%
  tab_header(title = "Breusch–Pagan Tests (Complete Case)") %>%
  tab_source_note("Red = p < 0.05 (evidence of heteroskedasticity); Green = p ≥ 0.05.")

# Summary table with threshold coloring
threshold <- 0.90

bp_summary_gt_cc.full <- bp_summary_cc.full %>%
  gt() %>%
  cols_label(
    outcome  = "Outcome",
    prop_sig = "Prop. p<0.05"
  ) %>%
  fmt_percent(columns = prop_sig, decimals = 0) %>%
  tab_header(
    title = "Breusch–Pagan Summary for Complete Case",
    subtitle = "Baseline vs Follow-up"
  ) %>%
  tab_style(
    style = cell_fill(color = "#e0f3db"),
    locations = cells_body(columns = prop_sig, rows = prop_sig <= threshold)
  ) %>%
  tab_style(
    style = cell_fill(color = "#fde0dd"),
    locations = cells_body(columns = prop_sig, rows = prop_sig > threshold)
  ) %>%
  tab_source_note(md(paste0(
    "Cell shading: **green** if Prop. p<0.05 ≤ ",
    scales::percent(threshold, accuracy = 1),
    "; **red** if Prop. p<0.05 > ",
    scales::percent(threshold, accuracy = 1), "."
  )))

bp_summary_gt_cc.full

bp_summary.2yr
bp_summary_gt_cc.2yr

bp_summary_gt_cc.full
bp_summary_gt.full

# Convert gt objects back to data frames before saving
df_imp_full <- as.data.frame(bp_summary_gt.full)
df_cc_2yr  <- as.data.frame(bp_summary_gt_cc.2yr)
df_cc_full <- as.data.frame(bp_summary_gt_cc.full)
df_imp_2yr <- as.data.frame(bp_summary.2yr)
# Save each table separately as CSV
write.csv(df_imp_full,    "bp_summary_full.csv",    row.names = FALSE)
write.csv(df_cc_2yr,  "bp_summary_cc_2yr.csv",  row.names = FALSE)
write.csv(df_cc_full, "bp_summary_cc_full.csv", row.names = FALSE)
write.csv(df_imp_2yr, "bp_summary.2yr.csv",row.names = FALSE)
##########################################################################
########################### Propensity Score Analysis ####################
##########################################################################
###################### imputed data: Baseline vs full duration #############################
library(mice)
library(survey)
library(cobalt)
library(dplyr)
library(purrr)

# --- Inputs ---
ndt_items <- c("Engagement", "IntentionalSelfHarm", "UnintentionalSelfHarm", 
               "RiskToOthers", "RiskFromOthers", "StressAnxiety", "SocialEffectiveness", 
               "AlcoholDrugAbuse", "ImpulseControl", "Housing")

hos_items <- c("Motivation", "SelfCare", "ManagingMoney", "SocialNetworks", 
               "SubstanceMisuse", "EmotionalHealth", "PhysicalHealth", 
               "MeaningfulTime", "Accommodation", "Offending")

outcome_vars <- c("NDT_total", "HOS_total", ndt_items, hos_items)
predictors.psa <- c("AGEBANDED", "SEX", "ETHBANDED", "HLNS", "OFFD", "SUBS",
                "MTHL", "EDUCBANDED", "time", "support_level")



# Convert imputed data to long format (include original data)
imp_long_full.psa <- complete(imp_rf_final, action = "long", include = TRUE)

# Function to process each imputed dataset
process_full_data <- function(df) {
  df %>%
    mutate(
      time = factor(time, levels = c(0, 1), labels = c("baseline", "followup")),
      NDT_total = as.numeric(as.character(NDT_total)),
      HOS_total = as.numeric(as.character(HOS_total)),
      across(setdiff(outcome_vars, c("NDT_total", "HOS_total")),
             ~ ordered(.)),
      support_level = as.factor(support_level)
    )
}

# Process all imputations
imp_full_long.psa <- imp_long_full.psa %>%
  group_split(.imp) %>%
  map_dfr(process_full_data)

# Split into list of imputed datasets
imp_list_full.psa <- split(imp_full_long.psa, imp_full_long.psa$.imp)

# Filter out datasets where support_level has insufficient variation
valid_imp_list.psa <- imp_list_full.psa %>% 
  keep(~ n_distinct(.x$support_level) > 1)


# Function to calculate IPW weights
get_ipw_weights <- function(df) {
  ps_model <- glm(
    support_level ~ AGEBANDED + SEX + ETHBANDED + HLNS + OFFD + 
      SUBS + MTHL + EDUCBANDED + time,
    family = binomial(link = "logit"),
    data = df
  )
  df$ps <- predict(ps_model, type = "response")
  df$ipw <- ifelse(df$support_level == "high", 1/df$ps, 1/(1 - df$ps))
  return(df)
}

# Apply IPW to all valid imputed datasets
imp_ipw_list <- valid_imp_list.psa %>%
  map(get_ipw_weights)


# Check balance for the first imputed dataset
bal.tab(
  support_level ~ AGEBANDED + SEX + ETHBANDED + HLNS + OFFD + 
    SUBS + MTHL + EDUCBANDED + time,
  data = imp_ipw_list[[1]], 
  weights = imp_ipw_list[[1]]$ipw,
  method = "weighting"
)


# Function to fit survey-weighted models
library(ordinal)  # For ordinal regression
library(MASS)     # For polr()

# 1. Define outcome types (ordinal vs continuous)
outcome_types <- list(
  "NDT_total" = "gaussian",
  "HOS_total" = "gaussian",
  # NDT items (5-level ordinal)
  "Engagement" = "ordinal", "IntentionalSelfHarm" = "ordinal",
  "UnintentionalSelfHarm" = "ordinal", "RiskToOthers" = "ordinal",
  "RiskFromOthers" = "ordinal", "StressAnxiety" = "ordinal",
  "SocialEffectiveness" = "ordinal", "AlcoholDrugAbuse" = "ordinal",
  "ImpulseControl" = "ordinal", "Housing" = "ordinal",
  # HOS items (10-level ordinal)
  "Motivation" = "ordinal", "SelfCare" = "ordinal",
  "ManagingMoney" = "ordinal", "SocialNetworks" = "ordinal",
  "SubstanceMisuse" = "ordinal", "EmotionalHealth" = "ordinal",
  "PhysicalHealth" = "ordinal", "MeaningfulTime" = "ordinal",
  "Accommodation" = "ordinal", "Offending" = "ordinal"
)

# 2. Modified modeling function
# 2. Modified modeling function with clm() for ordinal outcomes
fit_ipw_model <- function(df, outcome) {
  tryCatch({
    if(outcome_types[[outcome]] == "gaussian") {
      design <- svydesign(ids = ~1, weights = ~ipw, data = df)
      svyglm(
        reformulate(predictors.psa, response = outcome),
        design = design,
        family = gaussian()
      )
    } else if(outcome_types[[outcome]] == "ordinal") {
      df[[outcome]] <- ordered(df[[outcome]])
      ordinal::clm(
        reformulate(predictors.psa, response = outcome),
        data = df,
        weights = ipw,
        link = "logit"
      )
    }
  }, error = function(e) {
    message(paste("Error in", outcome, ":", e$message))
    NULL
  })
}

# 3. CORRECTED MODEL RESULTS WITH MANUAL POOLING
library(furrr)
plan(multisession)

all_model_results <- future_map(outcome_vars, function(outcome) {
  models <- map(imp_ipw_list, ~ fit_ipw_model(.x, outcome))
  models <- compact(models)
  
  if(length(models) == 0) return(NULL)
  
  if(outcome_types[[outcome]] == "ordinal") {
    coefs <- map(models, coef)
    vars <- map(models, ~ diag(vcov(.x)))
    
    m <- length(models)
    coef_mat <- do.call(rbind, coefs)
    var_mat <- do.call(rbind, vars)
    
    Q_bar <- colMeans(coef_mat)
    U_bar <- colMeans(var_mat)
    B <- apply(coef_mat, 2, var)
    T <- U_bar + (1 + 1/m) * B
    
    # t-statistics
    statistic <- Q_bar / sqrt(T)
    
    # Confidence intervals (normal approx)
    alpha <- 0.05
    crit <- qnorm(1 - alpha/2)
    conf.low <- Q_bar - crit * sqrt(T)
    conf.high <- Q_bar + crit * sqrt(T)
    
    tibble(
      term = names(Q_bar),
      estimate = Q_bar,
      std.error = sqrt(T),
      statistic = statistic,
      p.value = 2 * pnorm(-abs(estimate / std.error)),
      conf.low = conf.low,
      conf.high = conf.high
    )
  } else {
    broom::tidy(pool(as.mira(models)))
  }
}, .options = furrr_options(seed = TRUE)) %>% 
  setNames(outcome_vars)

# 4. Process results
successful_results <- compact(all_model_results)

# 5. Create unified results data frame
###################################
library(dplyr)
library(purrr)
library(tibble)

all_results_df <- map_dfr(names(all_model_results), function(name) {
  df <- all_model_results[[name]]
  
  if (inherits(df, "data.frame")) {
    df %>%
      dplyr::select(term, estimate,std.error,statistic, p.value) %>%
      mutate(outcome = name)
  } else {
    tibble()
  }
})

write.csv(all_results_df, "IPW_CA_final_results.csv", row.names = FALSE)
############### all results with full parameters ###################
library(dplyr)
library(purrr)
library(tibble)

wanted_cols <- c(
  "term", "estimate", "std.error", "statistic", "p.value","conf.low","conf.high", 
  "b", "df", "dfcom", "fmi", "lambda", "m", "riv", "ubar"
)

all_ipw_imp_results_df <- map_dfr(names(all_model_results), function(name) {
  df <- all_model_results[[name]]
  
  if (inherits(df, "data.frame")) {
    df %>%
      # Select only available columns, add NAs for missing ones
      dplyr::select(dplyr::any_of(wanted_cols)) %>%
      tibble::add_column(outcome = name, .before = 1)
  } else {
    tibble()
  }
})


write.csv(all_ipw_imp_results_df, "IPW_CA_full_results.csv", row.names = FALSE)

#####################################################################
##################### Results Validation #############################
####################################################################
############## Standardized Mean Differences (SMDs) Before/After Weighting
################### all imputations
library(cobalt)
library(tidyverse)

# 1. Combine all imputed datasets into one dataframe with imputation indicator
combined_imp_ipw <- bind_rows(imp_ipw_list, .id = "imp_id") %>%
  mutate(imp_id = as.numeric(imp_id))

# Convert binary factors to dummy variables
combined_imp_ipw_dummies <- combined_imp_ipw %>%
  mutate(
    HLNS_1 = as.numeric(HLNS == 1),
    SUBS_1 = as.numeric(SUBS == 1),
    OFFD_1 = as.numeric(OFFD == 1),
    MTHL_1 = as.numeric(MTHL == 1)
  )

# Create covariate name mapping
clear_names <- c(
  "SEX_2" = "Sex (Female)",
  "AGEBANDED_1" = "Age Band: 18-29",
  "AGEBANDED_2" = "Age Band: 30-39",
  "AGEBANDED_3" = "Age Band: 40+",
  "EDUCBANDED_2" = "Education: Higher",
  "ETHBANDED_2" = "Ethnicity: Minority ethnic group",
  "time_followup" = "Time: Follow-up",
  "SUBS_1" = "Substance Misuse: Yes",
  "OFFD_1" = "Offending History: Yes",
  "MTHL_1" = "Mental Health Issues: Yes",
  "HLNS_1" = "Housing Need: Yes"
)

# 2. Check balance across all imputed datasets
balance_stats_all <- bal.tab(
  support_level ~ AGEBANDED + SEX + ETHBANDED + HLNS_1 + OFFD_1 + 
    SUBS_1 + MTHL_1 + EDUCBANDED + time,  # Use the dummy variables
  data = combined_imp_ipw_dummies,
  weights = "ipw",
  method = "weighting",
  imp = "imp_id",       # Specify imputation ID variable
  un = TRUE,            # Show unadjusted balance
  stats = c("mean.diffs", "variance.ratios"),
  family = binomial(link = "logit")
)

# 3. Generate Love Plot for all imputations
balance_plot_imp <- love.plot(balance_stats_all,
          thresholds = c(m = 0.1),
          abs = TRUE,
          stats = "mean.diffs",
          binary = "std",
          var.order = "unadjusted",
          title = "Covariate Balance Before/After IPW (All Imputations)",
          sample.names = c("Unadjusted", "Adjusted"),
          which.factors = c("HLNS", "SUBS", "OFFD", "MTHL"),  
          position = "top",colors = c("#E69F00", "#56B4E9"),
          var.names = clear_names)

### export plots
ggsave("balance_plot_imp.pdf", dpi = 300, width = 8, height = 6)


# 4. Generate detailed balance report (optional)
print(balance_stats_all)


############# # Weight Distribution Checks ##############
summary(balance_stats_all)
library(ggplot2)

# Weight distribution plot
weight_dist_plot_imp <- ggplot(combined_imp_ipw_dummies, aes(x = ipw, fill = support_level)) +
  geom_histogram(alpha = 0.6, bins = 50) +
  labs(title = "IP Weight Distribution by Support Level",
       subtitle = "All Imputations",
       x = "Inverse Probability Weight",
       y = "Count") +
  theme_minimal()

# Summary statistics
weight_summary <- combined_imp_ipw_dummies %>%
  group_by(support_level) %>%
  summarise(
    n = n(),
    mean_weight = mean(ipw),
    sd_weight = sd(ipw),
    min_weight = min(ipw),
    max_weight = max(ipw),
    median_weight = median(ipw)
  )

# ROC curve
library(pROC)
# 2. Fit a new propensity score model on the combined data
# (Or use an existing one if you saved it)
ps_model_combined <- glm(
  support_level ~ AGEBANDED + SEX + ETHBANDED + HLNS + OFFD + 
    SUBS + MTHL + EDUCBANDED + time,
  family = binomial(link = "logit"),
  data = combined_imp_ipw_dummies
)

# 3. Calculate predictions
combined_imp_ipw_dummies$ps_pred <- predict(ps_model_combined, type = "response")

# 4. Generate ROC curve
roc_curve <- roc(
  response = as.numeric(combined_imp_ipw_dummies$support_level == "Low"),
  predictor = combined_imp_ipw_dummies$ps_pred
)

# 5. Plot ROC curve
roc_imp <- plot(roc_curve, main = "ROC Curve for Propensity Score Model: Imputations")
text(0.5, 0.3, paste("AUC =", round(roc_curve$auc, 3)), cex = 1.2)

#############################  Assess Model Fit #######################
# Calibration plot
library(ggplot2)
calibration_data <- combined_imp_ipw_dummies %>%
  mutate(
    ps = predict(ps_model_combined, type = "response"),
    ps_group = cut(ps, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
  ) %>%
  group_by(ps_group) %>%
  summarise(
    mean_ps = mean(ps, na.rm = TRUE),
    observed = mean(support_level == "Low", na.rm = TRUE),
    n = n()
  )

calibr_plot_imp <- ggplot(calibration_data, aes(x = mean_ps, y = observed)) +
  geom_point(aes(size = n), color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_text(aes(label = sprintf("n=%d", n)), vjust = -1, size = 3) +
  labs(
    title = "Propensity Score Model Calibration",
    subtitle = "Predicted vs Observed Probabilities of Low Support Level",
    x = "Predicted Probability (Mean by Bin)",
    y = "Observed Proportion",
    size = "Number of Cases"
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")


############ Residual Analysis (for Continuous Outcomes) #########
########## For NDT_total and hos_total 
# For NDT_total
design_ndt <- svydesign(ids = ~1, weights = ~ipw, data = imp_ipw_list[[1]])
model_ndt <- svyglm(
  NDT_total ~ AGEBANDED + SEX + ETHBANDED + HLNS + OFFD + 
    SUBS + MTHL + EDUCBANDED + time + support_level,
  design = design_ndt
)

# For HOS_total
design_hos <- svydesign(ids = ~1, weights = ~ipw, data = imp_ipw_list[[1]])
model_hos <- svyglm(
  HOS_total ~ AGEBANDED + SEX + ETHBANDED + HLNS + OFFD + 
    SUBS + MTHL + EDUCBANDED + time + support_level,
  design = design_hos
)

#######
# Select first imputed dataset
imp1_ipw <- imp_ipw_list[[1]]
imp2_ipw <- imp_ipw_list[[2]]
# Function for continuous outcome diagnostics
diagnose_continuous <- function(outcome) {
  design <- svydesign(ids = ~1, weights = ~ipw, data = imp2_ipw)
  model <- svyglm(
    reformulate(predictors.psa, response = outcome),
    design = design,
    family = gaussian()
  )
  
  # Create diagnostic plots
  par(mfrow = c(2, 2))
  plot(model, which = 1:4)
  title(paste(outcome, "- Imputation 1"), outer = TRUE, line = -1)
}

# Run for total scores
diagnose_continuous("NDT_total")
diagnose_continuous("HOS_total")


# Function for ordinal outcome diagnostics
diagnose_ordinal <- function(outcome) {
  df <- imp1_ipw
  df[[outcome]] <- ordered(df[[outcome]])
  
  # Fit model
  model <- clm(
    reformulate(predictors.psa, response = outcome),
    data = df,
    weights = ipw
  )
  
  # Formal tests
  cat("\n=== Proportional Odds Tests for", outcome, "===\n")
  print(nominal_test(model))
  print(scale_test(model))
  
  # Graphical check
  plot_data <- df %>%
    group_by(support_level, .data[[outcome]]) %>%
    summarise(count = n()) %>%
    mutate(prop = count/sum(count))
  
  ggplot(plot_data, aes(x = .data[[outcome]], y = prop, fill = support_level)) +
    geom_bar(position = "dodge", stat = "identity") +
    labs(title = paste("Proportional Odds Check for", outcome),
         subtitle = "Distribution by Support Level: Imputation",
         y = "Proportion") +
    theme_minimal()
}

# Run for key outcomes
diagnose_ordinal("Engagement")
diagnose_ordinal("IntentionalSelfHarm")
diagnose_ordinal("Motivation")


#### Proportional Odds Assumption (for Ordinal Outcomes)

# For Engagement
engagement_model <- clm(
  Engagement ~ AGEBANDED + SEX + ETHBANDED + HLNS + OFFD + 
    SUBS + MTHL + EDUCBANDED + time + support_level,
  data = imp1_ipw,
  weights = ipw,
  link = "logit"
)

# Test proportional odds assumption
library(VGAM)
nominal_test(engagement_model)
scale_test(engagement_model)

###############################
library(tidyverse)

# Prepare calibration data
calibration_data.ordinal <- imp1_ipw %>%
  mutate(
    predicted_prob = fitted(engagement_model),
    outcome = as.numeric(Engagement)
  ) %>%
  arrange(predicted_prob) %>%
  mutate(quantile = cut(predicted_prob, breaks = 10, include.lowest = TRUE)) %>%
  group_by(quantile) %>%
  summarise(
    mean_pred = mean(predicted_prob),
    mean_obs = mean(outcome),
    n = n()
  )

# Plot calibration
ggplot(calibration_data.ordinal, aes(x = mean_pred, y = mean_obs)) +
  geom_point(aes(size = n), color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = T, color = "darkgreen") +
  labs(title = "Engagement Model Calibration",
       subtitle = "Predicted vs Observed Engagement Scores",
       x = "Predicted Probability",
       y = "Observed Proportion") +
  theme_minimal()

# Method 3: Hosmer-Lemeshow Test for Ordinal Models

######################## gt table #################################
library(dplyr)
library(tidyr)

table_df <- all_results_df %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    se_fmt = sprintf("(%.3f)", std.error),
    cell = paste0(estimate_fmt, "\n", se_fmt)
  ) %>%
  dplyr::select(term, outcome, cell) %>%  # ensure correct select
  pivot_wider(names_from = outcome, values_from = cell)

# Make it look nice
gt_table <- table_df %>%
  gt(rowname_col = "term") %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body()
  )

gt_table

########### gt table ###############
# Prepare the data
table_data <- all_results_df %>%
  # Format estimates and SEs
  dplyr::mutate(
    formatted = sprintf("%.3f (%.3f)", estimate, std.error),
    # Add significance stars
    formatted = dplyr::case_when(
      p.value < 0.001 ~ paste0(formatted, "***"),
      p.value < 0.01 ~ paste0(formatted, "**"),
      p.value < 0.05 ~ paste0(formatted, "*"),
      TRUE ~ formatted
    )
  ) %>%
  # Reshape to wide format
  tidyr::pivot_wider(
    id_cols = term,
    names_from = outcome,
    values_from = formatted,
    values_fill = "-"
  ) %>%
  # Clean term names
  dplyr::mutate(
    term = dplyr::recode(
      term,
      "(Intercept)" = "Constant",
      "AGEBANDED2" = "Age band 2",
      "AGEBANDED3" = "Age band 3",
      "SEX2" = "Sex (female)",
      "ETHBANDED2" = "Ethnicity band 2",
      "HLNS1" = "Housing need",
      "OFFD1" = "Offending history",
      "SUBS1" = "Substance misuse",
      "MTHL1" = "Mental health",
      "EDUCBANDED2" = "Education band 2",
      "timefollowup" = "Time in program",
      "support_levelLow" = "Support level (Low)"
    )
  )

# Create gt table with safe row styling
results_table <- gt::gt(table_data) %>%
  gt::tab_header(
    title = "Regression Results Across Outcomes",
    subtitle = "Estimates with standard errors in parentheses"
  ) %>%
  gt::cols_label(term = "Predictor") %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels()
  ) %>%
  gt::tab_style(
    style = gt::cell_fill(color = "gray95"),
    locations = gt::cells_body(rows = which(1:nrow(table_data) %% 2 == 1))
  ) %>%
  gt::tab_footnote(
    footnote = "*** p < 0.001; ** p < 0.01; * p < 0.05",
    locations = gt::cells_title()
  ) %>%
  gt::tab_options(
    table.font.size = 14,
    column_labels.font.size = 16,
    heading.title.font.size = 20,
    table.width = "100%",
    data_row.padding = gt::px(8)
  )

results_table

#=========================================================================
############### imputed data: Baseline vs 2 year followup ###############
#==========================================================================





#############################################################################
########################### Complete Case IPW+CA #########################
######################################################################
# ---------------------------------------------
# COMPLETE-CASE: Baseline vs Full Duration
# ----------------------------------------------

# Packages
library(dplyr)
library(purrr)
library(survey)
library(ordinal)
library(MASS)    # if you need polr elsewhere
library(broom)
library(tibble)
library(cobalt)  # for bal.tab (optional)

ndt_items <- c("Engagement", "IntentionalSelfHarm", "UnintentionalSelfHarm", 
               "RiskToOthers", "RiskFromOthers", "StressAnxiety", "SocialEffectiveness", 
               "AlcoholDrugAbuse", "ImpulseControl", "Housing")

hos_items <- c("Motivation", "SelfCare", "ManagingMoney", "SocialNetworks", 
               "SubstanceMisuse", "EmotionalHealth", "PhysicalHealth", 
               "MeaningfulTime", "Accommodation", "Offending")

outcome_vars <- c("NDT_total", "HOS_total", ndt_items, hos_items)
predictors.psa <- c("AGEBANDED", "SEX", "ETHBANDED", "HLNS", "OFFD", "SUBS",
                    "MTHL", "EDUCBANDED", "time", "support_level")


# --- 0. prep cc dataset ---
cc_full.psa <- hmh_no_na %>%
  mutate(
    time = factor(time, levels = c(0, 1), labels = c("baseline", "followup")),
    NDT_total = as.numeric(as.character(NDT_total)),
    HOS_total = as.numeric(as.character(HOS_total)),
    across(setdiff(outcome_vars, c("NDT_total", "HOS_total")),
           ~ ordered(.)),
    support_level = as.factor(support_level)
  )

# --- 1. IPW function (same as you used) ---
get_ipw_weights <- function(df) {
  ps_model <- glm(
    support_level ~ AGEBANDED + SEX + ETHBANDED + HLNS + OFFD + 
      SUBS + MTHL + EDUCBANDED + time,
    family = binomial(link = "logit"),
    data = df
  )
  df$ps <- predict(ps_model, type = "response")
  df$ipw <- ifelse(df$support_level == "high", 1/df$ps, 1/(1 - df$ps))
  return(df)
}

# Apply IPW
cc_full.psa <- get_ipw_weights(cc_full.psa)

# Optional: check covariate balance
bal.tab(
  support_level ~ AGEBANDED + SEX + ETHBANDED + HLNS + OFFD + 
    SUBS + MTHL + EDUCBANDED + time,
  data = cc_full.psa,
  weights = cc_full.psa$ipw,
  method = "weighting"
)

# --- 2. outcome types (copy from yours) ---
outcome_types <- list(
  "NDT_total" = "gaussian",
  "HOS_total" = "gaussian",
  "Engagement" = "ordinal", "IntentionalSelfHarm" = "ordinal",
  "UnintentionalSelfHarm" = "ordinal", "RiskToOthers" = "ordinal",
  "RiskFromOthers" = "ordinal", "StressAnxiety" = "ordinal",
  "SocialEffectiveness" = "ordinal", "AlcoholDrugAbuse" = "ordinal",
  "ImpulseControl" = "ordinal", "Housing" = "ordinal",
  "Motivation" = "ordinal", "SelfCare" = "ordinal",
  "ManagingMoney" = "ordinal", "SocialNetworks" = "ordinal",
  "SubstanceMisuse" = "ordinal", "EmotionalHealth" = "ordinal",
  "PhysicalHealth" = "ordinal", "MeaningfulTime" = "ordinal",
  "Accommodation" = "ordinal", "Offending" = "ordinal"
)

# --- 1. Modeling function (svyglm for gaussian; clm for ordinal) ---
fit_ipw_model <- function(df, outcome) {
  tryCatch({
    if (outcome_types[[outcome]] == "gaussian") {
      design <- svydesign(ids = ~1, weights = ~ipw, data = df)
      svyglm(
        reformulate(predictors.psa, response = outcome),
        design = design,
        family = gaussian()
      )
    } else if (outcome_types[[outcome]] == "ordinal") {
      df[[outcome]] <- ordered(df[[outcome]])
      ordinal::clm(
        reformulate(predictors.psa, response = outcome),
        data = df,
        weights = ipw,
        link = "logit"
      )
    } else {
      stop("Unknown outcome type for: ", outcome)
    }
  }, error = function(e) {
    message("Error fitting model for ", outcome, ": ", e$message)
    NULL
  })
}

# --- 2. Run on complete-case data ---
library(furrr)
plan(multisession)

library(furrr)
plan(multisession)

all_model_results.cc <- future_map(outcome_vars, function(outcome) {
  model <- fit_ipw_model(cc_full.psa, outcome)
  if (is.null(model)) return(NULL)
  
  if (outcome_types[[outcome]] == "ordinal") {
    coefs <- coef(model)
    vars <- diag(vcov(model))
    statistic <- coefs / sqrt(vars)
    
    alpha <- 0.05
    crit <- qnorm(1 - alpha/2)
    conf.low <- coefs - crit * sqrt(vars)
    conf.high <- coefs + crit * sqrt(vars)
    
    tibble(
      term = names(coefs),
      estimate = coefs,
      std.error = sqrt(vars),
      statistic = statistic,
      p.value = 2 * pnorm(-abs(statistic)),
      conf.low = conf.low,
      conf.high = conf.high
    )
  } else {
    broom::tidy(model) |>
      mutate(
        df = df.residual(model),
        dfcom = nobs(model)
      )
  }
}, .options = furrr_options(seed = TRUE)) |>
  setNames(outcome_vars)

successful_results_cc <- compact(all_model_results.cc)


# 5. Create unified results data frame
###################################
library(dplyr)
library(purrr)
library(tibble)

all_results_cc_df <- map_dfr(names(all_model_results.cc), function(name) {
  df <- all_model_results.cc[[name]]
  
  if (inherits(df, "data.frame")) {
    df %>%
      dplyr::select(term, estimate,std.error,statistic, p.value) %>%
      mutate(outcome = name)
  } else {
    tibble()
  }
})


write.csv(all_results_cc_df, "IPW_CA_final_results_cc.csv", row.names = FALSE)

############### all results with full parameters ###################
library(dplyr)
library(purrr)
library(tibble)

wanted_cols.cc <- c(
  "term", "estimate", "std.error", "statistic", "p.value","conf.low","conf.high", 
  "b", "df", "dfcom"
)

all_ipw_imp_results_cc_df <- map_dfr(names(all_model_results.cc), function(name) {
  df <- all_model_results.cc[[name]]
  
  if (inherits(df, "data.frame")) {
    df %>%
      # Select only available columns, add NAs for missing ones
      dplyr::select(dplyr::any_of(wanted_cols.cc)) %>%
      tibble::add_column(outcome = name, .before = 1)
  } else {
    tibble()
  }
})


write.csv(all_ipw_imp_results_cc_df, "IPW_CA_full_results_cc.csv", row.names = FALSE)

###############################################################
################ Results validation ###########################
##############################################################

############## Standardized Mean Differences (SMDs) Before/After Weighting
# Convert binary factors to dummy variables
combined_cc_full.psa_dummies <- cc_full.psa %>%
  mutate(
    HLNS_1 = as.numeric(HLNS == 1),
    SUBS_1 = as.numeric(SUBS == 1),
    OFFD_1 = as.numeric(OFFD == 1),
    MTHL_1 = as.numeric(MTHL == 1)
  )
# Check balance for the first imputed dataset
library(cobalt)

balance_cc <- bal.tab(
  support_level ~ AGEBANDED + SEX + ETHBANDED + HLNS_1 + OFFD_1 + 
    SUBS_1 + MTHL_1 + EDUCBANDED + time,
  data = combined_cc_full.psa_dummies,
  weights = combined_cc_full.psa_dummies$ipw,
  method = "weighting",
  un = TRUE,
  stats = c("mean.diffs", "variance.ratios")
)

# Create Love plot with clear names
clear_names_cc <- c(
  "SEX_2" = "Sex (Female)",
  "AGEBANDED_1" = "Age Band: 18-29",
  "AGEBANDED_2" = "Age Band: 30-39",
  "AGEBANDED_3" = "Age Band: 40+",
  "EDUCBANDED_2" = "Education: Higher",
  "ETHBANDED_2" = "Ethnicity: Minority ethnic group",
  "time_followup" = "Time: Follow-up",
  "SUBS_1" = "Substance Misuse: Yes",
  "OFFD_1" = "Offending History: Yes",
  "MTHL_1" = "Mental Health Issues: Yes",
  "HLNS_1" = "Housing Need: Yes"
)

balance_plot_cc <- love.plot(balance_cc,
          thresholds = c(m = 0.1),
          abs = TRUE,
          stats = "mean.diffs",
          binary = "std",
          var.order = "unadjusted",
          title = "Covariate Balance Before/After IPW (Complete Case)",
          sample.names = c("Unadjusted", "Adjusted"),
          position = "top",
          var.names = clear_names,
          which.factors = c("HLNS", "SUBS", "OFFD", "MTHL"))

library(patchwork)


## 5. Combine plots on the same page
balance_plot_comparison <- balance_plot_imp + balance_plot_cc + 
  plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")

### export plots
ggsave("balance_plot_comparison.pdf", dpi = 300, width = 15, height = 6)


# Weight distribution plot
weight_dist_plot_cc <- ggplot(combined_cc_full.psa_dummies, 
                              aes(x = ipw, fill = support_level)) +
  geom_histogram(alpha = 0.6, bins = 50) +
  labs(title = "IP Weight Distribution by Support Level",
       subtitle = "Complete Case",
       x = "Inverse Probability Weight",
       y = "Count") +
  theme_minimal()

## 5. Combine plots on the same page
Weight_dist_plot_comparison <- weight_dist_plot_imp + weight_dist_plot_cc + 
  plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")

### export plots
ggsave("Weight_dist_plot_comparison.pdf", dpi = 300, width = 15, height = 6)

## Propensity Score Model Diagnostics
# ROC Curve
library(pROC)
roc_cc <- roc(
  response = as.numeric(cc_full.psa$support_level == "Low"),
  predictor = cc_full.psa$ps
)


# Close existing devices
while (dev.cur() > 1) dev.off()
# Open PDF device
pdf("roc_curves_comparison.pdf", width = 12, height = 6)

par(mfrow = c(1, 2))  # 1 row, 2 columns

# Left panel: Imputed
plot(roc_curve, main = "ROC Curve for Propensity Score Model (Imputation)")
text(0.5, 0.3, paste("AUC =", round(roc_curve$auc, 3)), cex = 1.2)

# Right panel: Complete case
plot(roc_cc, main = "ROC Curve for Propensity Score Model (Complete Case)")
text(0.5, 0.3, paste("AUC =", round(auc(roc_cc), 3)), cex = 1.2)

par(mfrow = c(1,1))  # reset layout

# Close PDF device
dev.off()

     
###### Calibration Plot
     cc_full.psa %>%
       mutate(ps_group = cut(ps, breaks = quantile(ps, probs = seq(0, 1, 0.1)), 
                             include.lowest = TRUE)) %>%
       group_by(ps_group) %>%
       summarise(
         mean_ps = mean(ps),
         observed = mean(support_level == "Low", na.rm = TRUE),
         n = n()
       ) %>%
       ggplot(aes(x = mean_ps, y = observed)) +
       geom_point(aes(size = n), color = "steelblue") +
       geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
       labs(title = "Propensity Score Calibration (Complete Case)",
            x = "Predicted Probability", y = "Observed Proportion")


####### Residual Diagnostics for Continuous Outcomes
# --- Continuous Outcomes Diagnostics ---
library(survey)
library(ggplot2)
library(patchwork)

# 1. Create survey design
design_cc <- svydesign(ids = ~1, weights = ~ipw, data = cc_full.psa)

##########
diagnose_continuous_cc <- function(outcome) {
  # Fit the model
  model.cc <- svyglm(
    reformulate(predictors.psa, response = outcome),
    design = design_cc,
    family = gaussian()
  )
  
  # Create diagnostic plots
  par(mfrow = c(2, 2))
  plot(model.cc, which = 1:4)  # Fixed: changed 'model' to 'model.cc'
  title(paste(outcome, "- Complete Case"), outer = TRUE, line = -1)
}


# 3. Generate diagnostics
ndt_diag_cc <- diagnose_continuous_cc("NDT_total")
hos_diag_cc <- diagnose_continuous_cc("HOS_total")

# 4. Display plots
ndt_diag_cc
hos_diag_cc 

# Run for total scores: imputation

ndt_diag_imp <- diagnose_continuous("NDT_total")
hos_diag_imp <- diagnose_continuous("HOS_total")

#============= NDTA ====================
while (dev.cur() > 1) dev.off()
pdf("diagnostics_ndt_cc_vs_imp.pdf", width = 12, height = 6)  # landscape
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Right: imputed
diagnose_continuous("NDT_total")
mtext("NDTA Total — Imputed", line = 1, cex = 1, font = 2)

# Left: complete case (NDT & HOS, if each function draws a panel)
diagnose_continuous_cc("NDT_total")
mtext("NDTA Total — Complete Case", line = 1, cex = 1, font = 2)

dev.off()

#================ HOS ====================
while (dev.cur() > 1) dev.off()
pdf("diagnostics_hos_cc_vs_imp.pdf", width = 12, height = 6)  # landscape
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Right: imputed
diagnose_continuous("HOS_total")
mtext("HOS Total — Imputed", line = 1, cex = 1, font = 2)

# Left: complete case (NDT & HOS, if each function draws a panel)
diagnose_continuous_cc("HOS_total")
mtext("HOS Total — Complete Case", line = 1, cex = 1, font = 2)

dev.off()

################# Proportional Odds Assumption for Ordinal Outcomes
# --- Ordinal Outcomes Diagnostics ---
diagnose_ordinal_cc <- function(outcome) {
  # Fit model
  model <- clm(
    reformulate(predictors.psa, response = outcome),
    data = cc_full.psa,
    weights = ipw
  )
  
  # Formal tests
  cat("\n==== Proportional Odds Tests for", outcome, "(CCA) ====\n")
  print(nominal_test(model))
  print(scale_test(model))
  
  # Graphical check
  plot_data <- cc_full.psa %>%
    group_by(support_level, .data[[outcome]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(support_level) %>%
    mutate(prop = n / sum(n))
  
  ggplot(plot_data, aes(x = factor(.data[[outcome]]), y = prop, fill = support_level)) +
    geom_bar(position = "dodge", stat = "identity") +
    labs(title = paste("Proportional Odds Check for", outcome, "(CCA)"),
         x = outcome, y = "Proportion") +
    theme_minimal()
}

# Run for key outcomes:CC

diagnose_ordinal_cc("Engagement")
diagnose_ordinal_cc("IntentionalSelfHarm")
diagnose_ordinal_cc("Motivation")

# Run for key outcomes: Imputation

diagnose_ordinal("Engagement")
diagnose_ordinal("IntentionalSelfHarm")
diagnose_ordinal("Motivation")

#======================================================
# ndta ordinal key outcomes: imputed vs complete case
#======================================================
############## Engagement #################
library(patchwork)

engag_imp <- diagnose_ordinal("Engagement")    + ggtitle("Engagement (NDTA): Imputed")
engag_cc  <- diagnose_ordinal_cc("Engagement") + ggtitle("Engagement (NDTA): Complete Case")

combined_engag <- engag_imp + engag_cc + plot_layout(ncol = 2) +
  plot_annotation(title = "NDTA – Engagement: Ordinal Diagnostics")

ggsave("diag_ndta_ordinal_Engagement.pdf", combined_engag, width = 12, height = 6, dpi = 300)

############## Housing #################
Housing_imp <- diagnose_ordinal("Housing")    + ggtitle("Housing (NDTA): Imputed")
Housing_cc  <- diagnose_ordinal_cc("Housing") + ggtitle("Housing (NDTA): Complete Case")

combined_Housing <- Housing_imp + Housing_cc + plot_layout(ncol = 2) +
  plot_annotation(title = "NDTA – Housing: Ordinal Diagnostics")

ggsave("diag_ndta_ordinal_Housing.pdf", combined_Housing, width = 12, height = 6, dpi = 300)


#====================================================
# hos ordinal key outcomes: imputed vs complete case 
#====================================================
############ Accommodation ##############
accommo_imp <- diagnose_ordinal("Accommodation")    + 
  ggtitle("Accommodation (HOS): Imputed")
accommo_cc  <- diagnose_ordinal_cc("Accommodation") + 
  ggtitle("Accommodation (HOS): Complete Case")

combined_Accommodation <- accommo_imp + accommo_cc + plot_layout(ncol = 2) +
  plot_annotation(title = "HOS – Accommodation: Ordinal Diagnostics")

ggsave("diag_hos_ordinal_Accommodation.pdf",
       combined_Accommodation, width = 12, height = 6, dpi = 300)


############ Offending ##############
offd_imp <- diagnose_ordinal("Offending")    + 
  ggtitle("Offending (HOS): Imputed")
offd_cc  <- diagnose_ordinal_cc("Offending") + 
  ggtitle("Offending (HOS): Complete Case")

combined_Offending <- offd_imp + offd_cc + plot_layout(ncol = 2) +
  plot_annotation(title = "HOS – Offending: Ordinal Diagnostics")

ggsave("diag_hos_ordinal_Offending.pdf",
       combined_Offending, width = 12, height = 6, dpi = 300)

#=====================================================================
#======================= END END END END =============================
#=====================================================================



#






















###################################
#----------------------------------------------------------
####        Complete case: Baseline vs 2 year followup ??????????????
#------------------------------------------------------------
# =========================================================
# COMPLETE-CASE: Baseline vs 2-year Follow-up (IPW + CA) ??????????????
# =========================================================

library(dplyr)
library(purrr)
library(survey)
library(ordinal)
library(MASS)
library(broom)
library(tibble)
library(cobalt)
library(furrr)

# ---------------------------------------------------------
# 0. Variables
# ---------------------------------------------------------
ndt_items <- c("Engagement", "IntentionalSelfHarm", "UnintentionalSelfHarm", 
               "RiskToOthers", "RiskFromOthers", "StressAnxiety", "SocialEffectiveness", 
               "AlcoholDrugAbuse", "ImpulseControl", "Housing")

hos_items <- c("Motivation", "SelfCare", "ManagingMoney", "SocialNetworks", 
               "SubstanceMisuse", "EmotionalHealth", "PhysicalHealth", 
               "MeaningfulTime", "Accommodation", "Offending")

outcome_vars <- c("NDT_total", "HOS_total", ndt_items, hos_items)
predictors_cc.2yr <- c("AGEBANDED", "SEX", "ETHBANDED", "HLNS", "OFFD", 
                       "SUBS", "MTHL", "EDUCBANDED", "time", "support_level")

# ---------------------------------------------------------
# 1. Data prep: Baseline + nearest-to-2-year follow-up
# ---------------------------------------------------------
extract2yr_data <- function(df) {
  baseline <- df %>% filter(time == 0)
  
  followup <- df %>%
    filter(time != 0) %>%
    mutate(diff_days = abs(as.numeric(difftime(wave_date, MNSTART_date, units = "days")) - 730)) %>%
    group_by(enrolment_num) %>%
    slice_min(order_by = diff_days, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  bind_rows(baseline, followup) %>%
    mutate(time = factor(time, levels = c(0, 1),
                         labels = c("baseline", "followup")))
}

complete_case_ipw_df.2yr <- hmh_no_na %>%
  group_split(enrolment_num) %>%
  map_dfr(extract2yr_data) %>%
  mutate(
    NDT_total = as.numeric(as.character(NDT_total)),
    HOS_total = as.numeric(as.character(HOS_total)),
    across(setdiff(outcome_vars, c("NDT_total", "HOS_total")), ordered),
    support_level = as.factor(support_level)
  ) %>%
  filter(if_all(all_of(c(outcome_vars, predictors_cc.2yr)), ~ !is.na(.)))

# ---------------------------------------------------------
# 2. IPW calculation
# ---------------------------------------------------------
get_ipw_weights <- function(df) {
  ps_model <- glm(
    support_level ~ AGEBANDED + SEX + ETHBANDED + HLNS + OFFD + 
      SUBS + MTHL + EDUCBANDED + time,
    family = binomial(link = "logit"),
    data = df
  )
  df$ps <- predict(ps_model, type = "response")
  df$ipw <- ifelse(df$support_level == "high", 1 / df$ps, 1 / (1 - df$ps))
  return(df)
}

complete_case_ipw_df.2yr <- get_ipw_weights(complete_case_ipw_df.2yr)

# Optional: covariate balance check
bal.tab(
  support_level ~ AGEBANDED + SEX + ETHBANDED + HLNS + OFFD + 
    SUBS + MTHL + EDUCBANDED + time,
  data = complete_case_ipw_df.2yr,
  weights = complete_case_ipw_df.2yr$ipw,
  method = "weighting"
)

# ---------------------------------------------------------
# 3. Outcome types
# ---------------------------------------------------------
outcome_types <- list(
  "NDT_total" = "gaussian",
  "HOS_total" = "gaussian",
  "Engagement" = "ordinal", "IntentionalSelfHarm" = "ordinal",
  "UnintentionalSelfHarm" = "ordinal", "RiskToOthers" = "ordinal",
  "RiskFromOthers" = "ordinal", "StressAnxiety" = "ordinal",
  "SocialEffectiveness" = "ordinal", "AlcoholDrugAbuse" = "ordinal",
  "ImpulseControl" = "ordinal", "Housing" = "ordinal",
  "Motivation" = "ordinal", "SelfCare" = "ordinal",
  "ManagingMoney" = "ordinal", "SocialNetworks" = "ordinal",
  "SubstanceMisuse" = "ordinal", "EmotionalHealth" = "ordinal",
  "PhysicalHealth" = "ordinal", "MeaningfulTime" = "ordinal",
  "Accommodation" = "ordinal", "Offending" = "ordinal"
)

# ---------------------------------------------------------
# 4. Model fitting
# ---------------------------------------------------------
fit_ipw_model <- function(df, outcome) {
  tryCatch({
    if (outcome_types[[outcome]] == "gaussian") {
      design <- svydesign(ids = ~1, weights = ~ipw, data = df)
      svyglm(
        reformulate(predictors_cc.2yr, response = outcome),
        design = design,
        family = gaussian()
      )
    } else if (outcome_types[[outcome]] == "ordinal") {
      df[[outcome]] <- ordered(df[[outcome]])
      ordinal::clm(
        reformulate(predictors_cc.2yr, response = outcome),
        data = df,
        weights = ipw,
        link = "logit"
      )
    } else {
      stop("Unknown outcome type for: ", outcome)
    }
  }, error = function(e) {
    message("Error fitting model for ", outcome, ": ", e$message)
    NULL
  })
}

# ---------------------------------------------------------
# 5. Run models
# ---------------------------------------------------------
plan(multisession)

all_model_results.cc_ipw.2yr <- future_map(outcome_vars, function(outcome) {
  model <- fit_ipw_model(complete_case_ipw_df.2yr, outcome)
  if (is.null(model)) return(NULL)
  
  if (outcome_types[[outcome]] == "ordinal") {
    coefs <- coef(model)
    vars <- diag(vcov(model))
    statistic <- coefs / sqrt(vars)
    
    alpha <- 0.05
    crit <- qnorm(1 - alpha/2)
    conf.low <- coefs - crit * sqrt(vars)
    conf.high <- coefs + crit * sqrt(vars)
    
    tibble(
      term = names(coefs),
      estimate = coefs,
      std.error = sqrt(vars),
      statistic = statistic,
      p.value = 2 * pnorm(-abs(statistic)),
      conf.low = conf.low,
      conf.high = conf.high
    )
  } else {
    broom::tidy(model) |>
      mutate(
        df = df.residual(model),
        dfcom = nobs(model)
      )
  }
}, .options = furrr_options(seed = TRUE)) |>
  setNames(outcome_vars)

successful_results_cc_ipw.2yr <- compact(all_model_results.cc_ipw.2yr)

# ---------------------------------------------------------
# 6. Combine results and save
# ---------------------------------------------------------
all_results_cc_ipw_df.2yr <- map_dfr(names(all_model_results.cc_ipw.2yr), function(name) {
  df <- all_model_results.cc_ipw.2yr[[name]]
  
  if (inherits(df, "data.frame")) {
    df %>%
      dplyr::select(term, estimate, std.error, statistic, p.value) %>%
      mutate(outcome = name)
  } else {
    tibble()
  }
})

write.csv(all_results_cc_df.2yr, "IPW_CA_results_cc_2yr.csv", row.names = FALSE)




















































##











library(dplyr)
library(survey)
library(mitools)

# 1. Define outcome variables and predictors
ndt_items <- c("Engagement", "IntentionalSelfHarm", "UnintentionalSelfHarm", 
               "RiskToOthers", "RiskFromOthers", "StressAnxiety", "SocialEffectiveness", 
               "AlcoholDrugAbuse", "ImpulseControl", "Housing")

hos_items <- c("Motivation", "SelfCare", "ManagingMoney", "SocialNetworks", 
               "SubstanceMisuse", "EmotionalHealth", "PhysicalHealth", 
               "MeaningfulTime", "Accommodation", "Offending")

# Define predictors and outcome
predictors_psa <- c("AGEBANDED", "SEX", "ETHBANDED", "HLNS", "OFFD", "SUBS",
                "MTHL", "EDUCBANDED", ndt_items, hos_items)

outcome_psa <- "support_binary"

# Step 1: Convert to long and extract 2-year follow-up
imp_long <- complete(imp_rf_final, action = "long", include = TRUE)

extract2_2yr_data <- function(df) {
  baseline <- df %>% filter(time == 0)
  
  followup <- df %>%
    filter(time == 1) %>%
    mutate(diff_days = abs(as.numeric(difftime(wave_date, MNSTART_date, units = "days")) - 730)) %>%
    group_by(enrolment_num) %>%
    slice_min(order_by = diff_days, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  bind_rows(baseline, followup) %>%
    mutate(time = factor(time, levels = c(0, 1), labels = c("baseline", "followup")))
}

imp_2yr_long <- imp_long %>%
  group_split(.imp) %>%
  map_dfr(extract2_2yr_data)


# Ensure support_level is a factor for logistic model
imp_2yr_long <- imp_2yr_long %>%
  mutate(support_binary = if_else(support_level == "high", 1, 0))


# Split into list by imputation
imp_list <- split(imp_2yr_long, imp_2yr_long$.imp)

# Define IPW + CA function
run_ipw_ca <- function(df) {
  # Propensity score model
  formula_ps <- as.formula(paste(outcome_psa, "~", paste(predictors_psa, collapse = " + ")))
  ps_model <- glm(formula_ps, data = df, family = binomial)
  df$ps <- predict(ps_model, type = "response")
  
  # Stabilized weights
  p_treat <- mean(df[[outcome_psa]] == 1)
  df$weight <- ifelse(
    df[[outcome_psa]] == 1,
    p_treat / df$ps,
    (1 - p_treat) / (1 - df$ps)
  )
  
  # Outcome model
  formula_outcome <- as.formula(paste(outcome_psa, "~", paste(predictors_psa, collapse = " + ")))
  outcome_model <- svyglm(
    formula_outcome,
    design = svydesign(ids = ~1, weights = ~weight, data = df),
    family = binomial()
  )
  
  return(summary(outcome_model))
}

# Run across all imputed datasets
results_list <- map(imp_list, run_ipw_ca)

# Pool results manually (mice::pool doesn't support svyglm), so use Rubin’s Rules manually
# For now, extract coefficients and standard errors
coef_mat <- map_dfr(results_list, ~ as.data.frame(coef(summary(.))), .id = "imp")
coef_mat$term <- rownames(coef(summary(results_list[[1]])))

# Aggregate using Rubin's Rules
rubins_pool <- coef_mat %>%
  group_by(term) %>%
  summarise(
    estimate = mean(Estimate),
    within_var = mean(`Std. Error`^2),
    between_var = var(Estimate),
    total_var = within_var + (1 + 1/length(results_list)) * between_var,
    se_total = sqrt(total_var),
    t_stat = estimate / se_total,
    df = (length(results_list) - 1) * (1 + within_var / ((1 + 1/length(results_list)) * between_var))^2,
    p_value = 2 * pt(-abs(t_stat), df)
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  select(term, estimate, se_total, t_stat, p_value, df)

# Display
library(flextable)
flextable(rubins_pool) %>% autofit()


######### PSA : IPW + Covariate Adjustment (Doubly Robust) ###################


###########################################################################
############################# Full Duration ###################################

# Step 1: Create list of imputed datasets
imp_list <- mget(paste0("imp", 1:20))

# Step 2: Estimate PS and weights
imp_list <- mget(paste0("imp", 1:20))

# Ensure support_level is a factor with 2 levels (0 and 1)
for (i in 1:20) {
  imp_list[[i]] <- imp_list[[i]] %>%
    mutate(
      support_binary = ifelse(support_level == "High", 1, 0),
      ps = glm(support_binary ~ AGEBANDED + SEX + ETHBANDED + HLNS + OFFD + SUBS + MTHL,
               family = binomial(), data = .)$fitted.values,
      weight = ifelse(support_binary == 1, 1 / ps, 1 / (1 - ps))
    )
}



# Step 3: Fit weighted regression models using survey design
library(survey)

library(miceadds)
library(survey)

# Your model results from above
models.full <- lapply(imp_list, function(data) {
  des <- svydesign(ids = ~1, weights = ~weight, data = data)
  svyglm(HOS_total ~ support_level + AGEBANDED + SEX + ETHBANDED,
         design = des)
})

# Pool using miceadds
pooled_results <- miceadds::pool_mi(models.full)
summary(pooled_results)



################################################################
################## Complete Case ##############################

########################## 2 year followup ###################
# Load required libraries
library(dplyr)
library(purrr)
library(broom)
library(flextable)

# 1. Define outcome variables and predictors
ndt_items <- c("Engagement", "IntentionalSelfHarm", "UnintentionalSelfHarm", 
               "RiskToOthers", "RiskFromOthers", "StressAnxiety", "SocialEffectiveness", 
               "AlcoholDrugAbuse", "ImpulseControl", "Housing")

hos_items <- c("Motivation", "SelfCare", "ManagingMoney", "SocialNetworks", 
               "SubstanceMisuse", "EmotionalHealth", "PhysicalHealth", 
               "MeaningfulTime", "Accommodation", "Offending")

ipw_outcome_vars <- "support_level"

ipw_predictors.2yr <- c("AGEBANDED", "SEX", "ETHBANDED", "HLNS", "OFFD", "SUBS",
                    "MTHL", "EDUCBANDED", "NDT_total", "HOS_total", ndt_items,
                    hos_items)

# 2. Extract baseline and closest-to-2-year follow-up
extract2_2yr_data <- function(df) {
  baseline <- df %>% filter(time == 0)
  
  followup <- df %>%
    filter(time == 1) %>%
    mutate(diff_days = abs(as.numeric(difftime(wave_date, MNSTART_date, units = "days")) - 730)) %>%
    group_by(enrolment_num) %>%
    slice_min(order_by = diff_days, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  bind_rows(baseline, followup) %>%
    mutate(time = factor(time, levels = c(0, 1), labels = c("baseline", "followup")))
}

complete_case_df.2yr <- hmh_no_na %>%
  group_split(enrolment_num) %>%
  map_dfr(extract2_2yr_data)

# Recode support_level: High = 1, Low = 0
complete_case_df.2yr <- complete_case_df.2yr %>%
  mutate(support_level_binary = if_else(support_level == "High", 1, 0))


# 3. Keep complete cases for outcome and predictors
complete_case_df.2yr <- complete_case_df.2yr %>%
  mutate(across(all_of(ipw_outcome_vars), ~ as.factor(as.character(.)))) %>%
  filter(if_all(all_of(c(ipw_outcome_vars, ipw_predictors.2yr)), ~ !is.na(.)))

# 4. Estimate IPW model (probability of being observed at 2yr)
ipw_model_2yr <- glm(support_level_binary ~ AGEBANDED + SEX + ETHBANDED + HLNS + OFFD + 
                       SUBS + MTHL, family = binomial(link = "logit"), 
                     data = complete_case_df.2yr)

# 5. Predict probabilities and compute weights
predicted_probs_2yr <- predict(ipw_model_2yr, type = "response")
ipw_weights_2yr <- 1 / predicted_probs_2yr
complete_case_df.2yr$ipw_weights_2yr <- ipw_weights_2yr

# 6. Run weighted regressions for each outcome
# Re-run weighted logistic regression using binary outcome
ipw_model_results_2yr <- map(ipw_outcome_vars, function(outcome) {
  formula <- as.formula(paste("support_level_binary ~", paste(ipw_predictors.2yr, collapse = " + ")))
  model <- glm(formula, data = complete_case_df.2yr, family = binomial(), weights = ipw_weights_2yr)
  tidy(model) %>% mutate(outcome = outcome)
})


# 7. Combine and clean results
ipw_combined_results_2yr <- bind_rows(ipw_model_results_2yr) %>%
  relocate(outcome) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# 8. Format output: group rows by outcome
ipw_combined_results_grouped <- ipw_combined_results_2yr %>%
  group_by(outcome) %>%
  mutate(outcome = if_else(row_number() == 1, outcome, "")) %>%
  ungroup()

# 9. Display table using flextable
flextable(ipw_combined_results_grouped) %>%
  autofit()


###########################
















