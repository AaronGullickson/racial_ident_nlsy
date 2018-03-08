#' ---
#' title: "processRawData.R"
#' author: ""
#' ---

# Introduction ------------------------------------------------------------

# This script will read in the raw data from the NLSY97 based on extracts from
# NLS Explorer. Part of this process will be linking the roster data which 
# contains information on parental race with individual records. The final data
# will be saved as a CSV/RData.


# Read in the Data --------------------------------------------------------
demog <- read.csv("input/demographic/demographic.csv")
roster <- read.csv("input/roster/roster.csv")

#how many cases are missing due to non-interview in 2002?
sum(demog$S1531300==-5)
#remove all of these cases
demog <- subset(demog, S1531300!=-5)

# Code Demographic Variables ----------------------------------------------
demog$id <- demog$R0000100

# GENDER
demog$gender <- factor(demog$R0536300, levels=c(1,2),
                       labels=c("Male","Female"))
table(demog$R0536300, demog$gender, exclude=NULL)

# BIRTH COHORT/AGE
demog$birthyear <- demog$R0536402
summary(demog$birthyear)

#age is recorded in months
demog$age <- ifelse(demog$S1531300==-5, NA, demog$S1531300/12)
summary(demog$age)

plot(demog$birthyear, demog$age)

# HOUSEHOLD STRUCTURE
demog$family <- factor(demog$R1205300,
                       levels=1:10,
                       labels=c("Two bio p","Two p, bio mom","Two p, bio dad",
                                "Bio mom", "Bio dad","Adoptive","Foster",
                                "Grandparents","Other relatives",
                                "Something else"))
table(demog$R1205300, demog$family, exclude=NULL)

#compare this to HH structure in 2002
demog$hh2002 <- factor(demog$S1542000,
                       levels=1:10,
                       labels=c("Two bio p","Two p, bio mom","Two p, bio dad",
                                "Bio mom", "Bio dad","Adoptive","Foster",
                                "Grandparents","Other relatives",
                                "Something else")) 
table(demog$S1542000, demog$hh2002, exclude=NULL)
table(demog$family, demog$hh2002)
demog$moved_out <- demog$family!="Something else" & demog$hh2002=="Something else"
summary(demog$moved_out)

# HOUSEHOLD SIZE
demog$hhsize <- demog$R1205400

# PARENTAL EDUCATION
demog$biodaded <- ifelse(demog$R1302400<0 | demog$R1302400>20,NA,demog$R1302400)
demog$biomomed <- ifelse(demog$R1302500<0 | demog$R1302500>20,NA,demog$R1302500)
demog$resdaded <- ifelse(demog$R1302600<0 | demog$R1302600>20,NA,demog$R1302600)
demog$resmomed <- ifelse(demog$R1302700<0 | demog$R1302700>20,NA,demog$R1302700)
summary(demog[,c("biodaded","biomomed","resdaded","resmomed")])
#get highest parental education level - add a column of -4 values so I don't 
#get warning when all are missing
demog$highparented <- apply(cbind(rep(-4,nrow(demog)),
                                  demog[,c("biodaded","biomomed",
                                           "resmomed","resdaded")]),
                            1,max,na.rm=TRUE)
demog$highparented[demog$highparented<0] <- NA
summary(demog$highparented)

# HOUSEHOLD INCOME
#For household income we are going to make valid 
#negative values and zero the smallest non-neg number (5)
demog$hhinc <- ifelse(demog$R1204500>=-4 & demog$R1204500<0, NA,demog$R1204500)
demog$hhinc <- ifelse(demog$hhinc<=0, min(demog$hhinc[demog$hhinc>0]), 
                      demog$hhinc)
summary(demog$hhinc)

demog$hhnetworth <- ifelse(demog$R1204700>=-4 & demog$R1204700<0, NA,
                           demog$R1204700)
summary(demog$hhnetworth)

# URBANICITY
demog$urban97 <- factor(demog$R1217500, levels=0:1,
                        labels=c("Rural","Urban"))
table(demog$R1217500, demog$urban97, exclude=NULL)

# REGION
demog$region97 <- factor(demog$R1200300, levels=1:4,
                         labels=c("Northeast","North Central","South", "West"))
table(demog$R1200300, demog$region97, exclude=NULL)

# MIGRATION BETWEEN 1997 and 2002
demog$migration <- factor(demog$S1530100, levels=c(-4, 1:4),
                         labels=c("Non-movers","Within county",
                                  "Different county","Different state",
                                  "Different country"))
table(demog$S1530100, demog$migration, exclude=NULL)

# ASVAB score - lets standardize it
demog$asvab <- scale(ifelse(demog$R9829600<0, NA, demog$R9829600))
summary(demog$asvab)

demog$gpa_overall <- ifelse(demog$R9871900<0, NA, demog$R9871900/100)
summary(demog$gpa_overall)

#ENROLLMENT
demog$enrollment02 <- factor(demog$S1538001, levels=1:11,
               labels=c("Not enrolled, no HS degree",
                        "Not enrolled, GED",
                        "Not enrolled, HS Degree",
                        "Not enrolled, some college",
                        "Not enrolled, 2-yr college grad",
                        "Not enrolled, 4-yr college grad",
                        "Not enrolled, grad degree",
                        "Enrolled in HS", 
                        "Enrolled in 2-yr college",
                        "Enrolled in 4-yr college",
                        "Enrolled in grad program"))
table(demog$enrollment02, demog$S1538001, exclude=NULL)
#how many respondents are still in HS by age?
table(demog$enrollment02, floor(demog$age))


# Code Multiple Race Responses --------------------------------------------

#Separate Yes/No variables for each race option. Turn into binaries and
#code missing values
white02 <- ifelse(demog$S1224900<0,NA,demog$S1224900)==1
black02 <- ifelse(demog$S1224901<0,NA,demog$S1224901)==1
#we can't distinguish Asians and PI on parental ID, so collapse them. 
asian02 <- ifelse(demog$S1224902<0,NA,demog$S1224902)==1 | 
  ifelse(demog$S1224903<0,NA,demog$S1224903)==1
indian02 <- ifelse(demog$S1224904<0,NA,demog$S1224904)==1
other02 <- ifelse(demog$S1224905<0,NA,demog$S1224905)==1
hispanic02 <- ifelse(demog$S1224906<0,NA,demog$S1224906)==1
summary(cbind(white02,black02,asian02,indian02,other02,hispanic02))

#create multiracial categories from these responses
#ignore non-hispanic other
demog$multirace02 <- paste(ifelse(white02,"W",""),
                           ifelse(black02,"B",""),
                           ifelse(indian02,"I",""),
                           ifelse(asian02,"A",""),
                           ifelse(hispanic02,"H",""), sep="")
demog$multirace02 <- factor(demog$multirace02,
                            levels=c("W","B","I","A","H",
                                     "WB","WI","WA","WH","BI","BA","BH","IA","IH","AH",
                                     "WBI","WBA","WBH","WIA","WIH","WAH","BIA","BIH","BAH","IAH",
                                     "WBIA","WBIH","WBAH","WIAH","BIAH",
                                     "WBIAH"))
table(demog$multirace02)

# how does this look if add in the other category?
multirace.other <- paste(ifelse(white02,"W",""),
                         ifelse(black02,"B",""),
                         ifelse(indian02,"I",""),
                         ifelse(asian02,"A",""),
                         ifelse(hispanic02,"H",""),
                         ifelse(other02,"O",""), sep="")
multirace.other <- factor(multirace.other,
                            levels=c("W","B","I","A","H","O",
                                     "WB","WI","WA","WH","WO","BI","BA","BH","BO","IA","IH","IO","AH","AO","HO",
                                     "WBI","WBA","WBH","WBO","WIA","WIH","WIO","WAH","WAO","WHO","BIA","BIH","BIO","BAH","BAO","BHO","IAH","IAO","IHO","AHO",
                                     "WBIA","WBIH","WBIO","WBAH","WBAO","WBHO","WIAH","WIAO","WIHO","WAHO","BIAH","BIAO","BAHO","BIHO","IAHO",
                                     "WBIAH","WBIAO","WBIHO","WBAHO","WIAHO","BIAHO",
                                     "WBIAHO"))
table(multirace.other, droplevels(demog$multirace02), exclude=NULL)

#lets also look at the distribution of race for those who reported other
#alone or in combination
table(multirace.other[!is.na(other02) & other02==TRUE], exclude=NULL)

#compare to hispanic question in 2002
demog$hispanic <- ifelse(demog$S1224800<0, NA, demog$S1224800)==1

table(demog$multirace02, demog$hispanic)

# Collect Roster Data -----------------------------------------------------

# The demographic information is listed in columns for each household member and
# then each non-HH member. First I need to collect these arrays for specific 
# demographic characteristics, then loop through and pull out the bio mom and
# dad based on the indicated relationship to the respondent of that column.

#how deep to go in the household roster. Some dads as deep as #16.
hhdepth <- 16
#how deep to go in the non-HH roster. Deepest parent is at #8
nhhdepth <- 8

#add an NA column to each roster in order to easily include missing parents
age <- cbind(roster[,c(paste("R",seq(from=1080300, by=100, length=hhdepth), sep=""))],
             roster[,c(paste("R",seq(from=1163700, by=100, length=nhhdepth), sep=""))],
             NA)
ethnic <- cbind(roster[,c(paste("R",seq(from=1094600, by=100, length=hhdepth), sep=""))],
                roster[,c(paste("R",seq(from=1172500, by=100, length=nhhdepth), sep=""))],
                NA)
grade <- cbind(roster[,c(paste("R",seq(from=1099400, by=100, length=hhdepth), sep=""))],
               roster[,c(paste("R",seq(from=1176900, by=100, length=nhhdepth), sep=""))],
               NA)
race <- cbind(roster[,c(paste("R",seq(from=1115400, by=100, length=hhdepth), sep=""))],
              roster[,c(paste("R",seq(from=1184500, by=100, length=nhhdepth), sep=""))],
               NA)
relate <- cbind(roster[,c(paste("R",seq(from=1315800, by=100, length=hhdepth), sep=""))],
              roster[,c(paste("R",seq(from=1186600, by=100, length=nhhdepth), sep=""))])
informant <- roster[,c(paste("R",seq(from=1102600, by=100, length=hhdepth), sep=""))]==1

#get informant relationship
#check to make sure there is always one and only one informant
summary(apply(informant,1,sum))
#damn it!
sum(apply(informant,1,sum)==0)
sum(apply(informant,1,sum)>1)
#8 cases of no informant, 3 cases of multiple informants. 

#ok, so lets figure out informant. note that 0 is a real value for respondent,
#so need to add one to relationship values and then subtract in final product
informant_relationship <- (relate[,1:hhdepth]+1)*informant
informant_relationship[informant_relationship<=0] <- 100
#for cases of double values, take the relationship that has a smaller value
temp <- apply(informant_relationship, 1, min)-1
informant_relationship <- cut(temp, c(0,1,3,5,88,100), right=FALSE, 
                              labels=c("Self","Spouse","Bio Parent",
                                       "Other","Unknown"))
summary(informant_relationship)
round(prop.table(table(informant_relationship))*100,1)

#lets combine spouse, other, unknown together for parsimony
temp <- factor(ifelse(informant_relationship=="Self","Self",
                      ifelse(informant_relationship=="Bio Parent",
                             "Bio Parent",
                             "Other")),
               levels=c("Self","Bio Parent","Other"))
table(informant_relationship, temp, exclude=NULL)
informant_relationship <- temp

#use which to quickly extract the index of all bio parents. Fathers are 4, mothers are 3.
dadid <- momid <- rep(ncol(relate)+1, nrow(relate))
temp <- which(relate==4, arr.ind=TRUE)
dadid[temp[,1]] <- temp[,2]
temp <- which(relate==3, arr.ind=TRUE)
momid[temp[,1]] <- temp[,2]

#How do I pull out variable columns of a matrix? Thank you StackOverflow! 
#https://stackoverflow.com/questions/25584039/how-to-extract-different-columns-from-each-row-of-a-data-frame

#one addition to this is that I need to deal with the zero dadid and momid
#because they will be dropped by routine making my vectors too small. So I
#replace zeros by the last column of the matrices which is just an NA column.
dadid[dadid==0] <- ncol(age)
momid[momid==0] <- ncol(age)

parents <- data.frame(id=roster$R0000100,
                      informant=informant_relationship,
                      fage=age[cbind(seq_along(dadid), dadid)],
                      feduc=grade[cbind(seq_along(dadid), dadid)],
                      fethnic=ethnic[cbind(seq_along(dadid), dadid)],
                      frace=race[cbind(seq_along(dadid), dadid)],
                      mage=age[cbind(seq_along(momid), momid)],
                      meduc=grade[cbind(seq_along(momid), momid)],
                      methnic=ethnic[cbind(seq_along(momid), momid)],
                      mrace=race[cbind(seq_along(momid), momid)])

#now recode variables, fix missing values, etc.
parents$fage <- ifelse(!is.na(parents$fage) & parents$fage<0, NA, parents$fage)
parents$mage <- ifelse(!is.na(parents$mage) & parents$mage<0, NA, parents$mage)
parents$feduc <- ifelse(!is.na(parents$feduc) & (parents$feduc<0 | parents$feduc>20), 
                        NA, parents$feduc)
parents$meduc <- ifelse(!is.na(parents$meduc) & (parents$meduc<0 | parents$meduc>20), 
                        NA, parents$meduc)
parents$fethnic <- factor(parents$fethnic, levels=0:1, labels=c("Not Hispanic","Hispanic"))
parents$methnic <- factor(parents$methnic, levels=0:1, labels=c("Not Hispanic","Hispanic"))
parents$frace <- factor(parents$frace, levels=1:7,
                        labels=c("White","Black","AmIndian","Asian","Other","Hispanic","Mixed"))
parents$mrace <- factor(parents$mrace, levels=1:7,
                        labels=c("White","Black","AmIndian","Asian","Other","Hispanic","Mixed"))
#replace race with hispanic if ethnicity variable hispanic
temp <- factor(ifelse(!is.na(parents$fethnic) & parents$fethnic=="Hispanic",
                      "Hispanic", as.character(parents$frace)),
               levels=c("White","Black","AmIndian","Asian","Other","Hispanic","Mixed"))
table(parents$frace, temp, exclude=NULL)
parents$frace <- temp
temp <- factor(ifelse(!is.na(parents$methnic) & parents$methnic=="Hispanic",
                      "Hispanic", as.character(parents$mrace)),
               levels=c("White","Black","AmIndian","Asian","Other","Hispanic","Mixed"))
table(parents$mrace, temp, exclude=NULL)
parents$mrace <- temp

with(parents, table(frace, mrace, exclude=NULL))

#create parent mixed race variable. Ignore gender of parent because DF
temp <- paste(parents$frace, parents$mrace, sep=".")
TF <- !is.na(parents$frace) & !is.na(parents$mrace) & parents$frace==parents$mrace
temp[TF] <- as.character(parents$frace)[TF]
TF <- !is.na(parents$frace) & !is.na(parents$mrace) & 
  as.numeric(parents$frace)>as.numeric(parents$mrace)
temp[TF] <- paste(parents$mrace, parents$frace, sep=".")[TF]
temp <- gsub("White","W",temp) 
temp <- gsub("Black","B",temp) 
temp <- gsub("AmIndian","I",temp) 
temp <- gsub("Asian","A",temp) 
temp <- gsub("Hispanic","H",temp) 
#mixed, other, or missing parents are NA for our purposes
temp[grepl("NA|Mixed|Other", temp)] <- NA
parents$mixedrace_parent <- factor(gsub("\\.","", temp),
                                   levels=c("W","B","I","A","H",
                                            "WB","WI","WA","WH",
                                            "BI","BA","BH",
                                            "IA","IH","AH"))
table(parents$mixedrace_parent, exclude=NULL)

#code residential parents
#parents$dadres <- dadid<=hhdepth
#parents$momres <- momid<=hhdepth

summary(parents)

#remove cases that do not have a result for parentally based combined race

# Merge Datasets and Save -------------------------------------------------------

# First limit variables to just the key ones for analysis
demog <- subset(demog,
                select = c("id","gender","age","urban97","region97","migration",
                           "moved_out","family","hhinc","highparented","asvab",
                           "gpa_overall","enrollment02","multirace02","hhsize",
                           "hispanic"))

parents <- subset(parents,
                  select=c("id","informant","fage","mage","mixedrace_parent",
                           "frace","mrace"))

#now merge by id
nlsy <- merge(demog, parents, by="id", all.x=FALSE, all.y=FALSE)

#how many cases are missing on each race variable and combined
sum(is.na(nlsy$multirace02))
sum(is.na(nlsy$mixedrace_parent))
sum(is.na(nlsy$multirace02) | is.na(nlsy$mixedrace_parent))

#remove cases missing on either race variable
nlsy <- subset(nlsy, !is.na(multirace02) & !is.na(mixedrace_parent))
nrow(nlsy)

#drop any unused factor levels to simplify multiple imputation later
nlsy <- droplevels(nlsy)

#summary to check everything
summary(nlsy)

#table of multiple race response by parental race response
with(nlsy, table(multirace02, mixedrace_parent, exclude=NULL))

#table of parents race
with(nlsy, table(frace, mrace, exclude=NULL))

#table of hispanic by mixedrace_parent
with(nlsy, table(mixedrace_parent, hispanic, exclude=NULL))

#what proportion of those who identified as Hispanic had at least
#one Hispanic-identified parent.
with(subset(nlsy, hispanic), mean(grepl("H",mixedrace_parent)))

save(nlsy, file="output/nlsy_processed.RData")
