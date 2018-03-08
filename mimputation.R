#this script will do multiple imputation and create new variables needed for the
#full analysis


# Load Data and Libraries -------------------------------------------------

library(mice)
load("output/nlsy_processed.RData")


# Variables to Impute -----------------------------------------------------

#Put all variables that should be imputed onto this chain. Don't put mixedrace_parent here
variables_to_impute <- c("gender","age","urban97","region97","family","fage",
                         "mage","migration","informant","moved_out","highparented",
                         "hhinc","hhsize","asvab","gpa_overall","enrollment02","multirace02")


# Run Imputation ----------------------------------------------------------

nlsy_mi <- mice(subset(nlsy, select=variables_to_impute),5)
#put results back into a list
analytical_samples <- lapply(list(1,2,3,4,5), function(i) {complete(nlsy_mi,i)})


# Construct Analytical Sample ---------------------------------------------

#this will use a custom function in lapply to create variables that will be used in the actual analysis

analytical_samples <- lapply(analytical_samples, function(analytical_data) {
  #add back in race of parent
  analytical_data <- cbind(analytical_data, mixedrace_parent=nlsy$mixedrace_parent)
  #define DV of racially consistent self-report
  analytical_data$race.consistent <- as.character(analytical_data$mixedrace_parent)==
    as.character(analytical_data$multirace02)
  #allow for two unusual cases
  analytical_data$race.consistent[(analytical_data$multirace02=="WBI" 
                                   & (analytical_data$mixedrace_parent=="WB" |
                                        analytical_data$mixedrace_parent=="WI" |
                                        analytical_data$mixedrace_parent=="BI")) |
                                    (analytical_data$multirace02=="WIH" 
                                     & (analytical_data$mixedrace_parent=="WI" |
                                          analytical_data$mixedrace_parent=="WH" |
                                          analytical_data$mixedrace_parent=="IH"))] <- TRUE
  
  #grand mean center all independent variables
  analytical_data$female.ctr <- as.numeric(analytical_data$gender=="Female")-
    mean(as.numeric(analytical_data$gender=="Female"))
  analytical_data$age.ctr <- analytical_data$age-mean(analytical_data$age)
  analytical_data$urban.ctr <- as.numeric(analytical_data$urban97=="Urban")-
    mean(as.numeric(analytical_data$urban97=="Urban"))
  analytical_data$twobio.ctr <- as.numeric(analytical_data$family=="Two bio p")-
    mean(as.numeric(analytical_data$family=="Two bio p"))
  analytical_data$moved_out.ctr <- as.numeric(analytical_data$moved_out)-
    mean(as.numeric(analytical_data$moved_out))
  analytical_data$parented.ctr <- analytical_data$highparented-mean(analytical_data$highparented)
  analytical_data$loghhinc.ctr <- log(analytical_data$hhinc)-mean(log(analytical_data$hhinc))
  analytical_data$moved_county.ctr <- as.numeric(as.numeric(analytical_data$migration)>=3)-
    mean(as.numeric(analytical_data$migration)>=3)
  analytical_data$moved_state.ctr <- as.numeric(as.numeric(analytical_data$migration)>=4)-
    mean(as.numeric(analytical_data$migration)>=4)
  analytical_data$noDiploma.ctr <- as.numeric(as.numeric(analytical_data$enrollment02)<=2)-
    mean(as.numeric(analytical_data$enrollment02)<=2)
  temp <- analytical_data$enrollment02=="Not enrolled, 4-yr college grad"  | 
    analytical_data$enrollment02=="Not enrolled, grad degree" |
    analytical_data$enrollment02=="Enrolled in 2-yr college" | 
    analytical_data$enrollment02=="Enrolled in 4-yr college" | 
    analytical_data$enrollment02=="Enrolled in grad program" | 
    analytical_data$enrollment02=="Enrolled in 2-yr college"
  analytical_data$college.ctr <- as.numeric(temp)-mean(temp)
  analytical_data$hs.center <- as.numeric(analytical_data$enrollment02=="Enrolled in HS")-
    mean(as.numeric(analytical_data$enrollment02=="Enrolled in HS"))
  analytical_data$gpa.ctr <- analytical_data$gpa_overall-mean(analytical_data$gpa_overall)

  analytical_data$logpcinc.ctr <- log(analytical_data$hhinc/analytical_data$hhsize)-
    mean(log(analytical_data$hhinc/analytical_data$hhsize))
  
  #create dummy for either parent being id'ed as hispanic or white
  analytical_data$anyhisp <- grepl("H", analytical_data$mixedrace_parent)
  analytical_data$anywhite <- grepl("W", analytical_data$mixedrace_parent)
  analytical_data$anyblack <- grepl("B", analytical_data$mixedrace_parent)
  
  #code a dummy for being multiracial by parent id
  analytical_data$multi.parent <- nchar(as.character(analytical_data$mixedrace_parent))>1
  
  #collapse some very small multiracial categories
  #BI and BA into Black, non-white, non-hisp
  #analytical_data$mixedrace_parent_orig <- analytical_data$mixedrace_parent
  #analytical_data$mixedrace_parent <- as.character(analytical_data$mixedrace_parent)
  #analytical_data$mixedrace_parent[analytical_data$mixedrace_parent=="BI" | 
  #                                   analytical_data$mixedrace_parent=="BA"] <- "BO"
  #analytical_data$mixedrace_parent[analytical_data$mixedrace_parent=="IH" | 
  #                                   analytical_data$mixedrace_parent=="AH"] <- "OH"
  #analytical_data$mixedrace_parent <- factor(analytical_data$mixedrace_parent,
  #                                           levels=c("W","B","I","A","H",
  #                                                    "WB","WI","WA","WH","BO","BH","OH"))
  
  return(analytical_data)
})


# Save Analytical Data ----------------------------------------------------

save(analytical_samples, file="output/analytical_samples.RData")