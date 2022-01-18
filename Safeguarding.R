# set working directory and import file, recode new variable 'Area'  
# create dataframe from the original file
safeguarding <- orders

#Create Area variable and set to 0
safeguarding$Area <- 0
#run "recode Area" function and save output in Area; important to write your variable names in colons (but not the data frame name)
safeguarding$Area <- recodeArea(DF=safeguarding,varname="Area",varname2="Site",varname3 = "LA.of.residence", varname4="Referred.from",varname5="Default.LA")

#pick the columns we want, creating a new data frame called 'SelectedColumns':
SelectedColumns <- safeguarding[,(c("SH24.UID","Customer.ID","Created.at","Created.at.month.year","Dispatched.at",'Dispatched.at.month.year',"Notified.at","Area",
                  "Notified.at.month.year","Age","Gender","Genitals","Ethnicity","Sexual.preference","Sexuality","Sites.tested",
                  "Sexually.assaulted.risk.assessment","Pressured.into.sex","Paid.for.sex","Drink.or.Drugs","Depression.or.low.mood","Older.or.younger.partner",
                  "Syphilis","HIV","Chlamydia","Gonorrhoea","Hep.B","Hep.C"))]

#'table' gives a table with the selected variable and the number of occurrences
table(SelectedColumns$Dispatched.at.month.year=="2021-11")


#Create subsets (data frames) for the two safeguarding reports:
#one subset for young people (age < 18), and another for adults. First check what type of data is 'Age'
class(SelectedColumns$Age)
#the funciton 'structure' gives all the info on each variable
str(SelectedColumns$Age)

#create a subset with people under 18 ***CHANGE MONTH*** ----
#There's a comma at the end of the command because we are selecting certain rows but all the columns.
YoungPeople <- SelectedColumns [(SelectedColumns$Age<18 
                            & SelectedColumns$Created.at.month.year=="2021-11"),]
nrow(YoungPeople)

ZeroArea <- YoungPeople[(YoungPeople$Area=="0"),] 
rm(ZeroArea)

# PAULA'S REQUEST: date fields are imported as factor, convert factor to date----
# class(SelectedColumns$Created.at)
# SelectedColumns$Created.at <- as.Date(SelectedColumns$Created.at, format = "%Y-%m-%d")
# YoungPeople <- SelectedColumns[(SelectedColumns$Age<18 & (SelectedColumns$Created.at >= "2019-10-01" & SelectedColumns$Created.at <= "2020-07-31")),]

str(YoungPeople$Sexually.assaulted.risk.assessment)
summary(YoungPeople$Sexually.assaulted.risk.assessment)
summary(YoungPeople$Pressured.into.sex)
summary(YoungPeople$Paid.for.sex)
summary(YoungPeople$Drink.or.Drugs)
summary(YoungPeople$Depression.or.low.mood)
summary(YoungPeople$Older.or.younger.partner)

table(YoungPeople$Sexually.assaulted.risk.assessment)

#create new variable (columns) flags that - first - equal the safeguarding columns and - then - assign 1 or 0 to those safeguarding flags
YoungPeople$Flag_SA <- YoungPeople$Sexually.assaulted.risk.assessment
YoungPeople$Flag_PS <- YoungPeople$Pressured.into.sex
YoungPeople$Flag_PAID <- YoungPeople$Paid.for.sex
YoungPeople$Flag_DD <- YoungPeople$Drink.or.Drugs
YoungPeople$Flag_DEP <- YoungPeople$Depression.or.low.mood
YoungPeople$Flag_PARTNER <- YoungPeople$Older.or.younger.partner
#Code the safeguarding-flag variables just created by giving them a number, 1 for true/yes, and 0 for false/no:
YoungPeople$Flag_SA = ifelse(YoungPeople$Flag_SA=="yes", 1,0)
YoungPeople$Flag_PS = ifelse(YoungPeople$Flag_PS=="yes", 1,0)
YoungPeople$Flag_PAID = ifelse(YoungPeople$Flag_PAID=="yes",1,0)
YoungPeople$Flag_DD = ifelse(YoungPeople$Flag_DD=="yes",1,0)
YoungPeople$Flag_DEP = ifelse(YoungPeople$Flag_DEP=="yes",1,0)
YoungPeople$Flag_PARTNER = ifelse(YoungPeople$Flag_PARTNER=="yes",1,0)

#table the results to double check flag against the original variable:
table(YoungPeople$Sexually.assaulted.risk.assessment, YoungPeople$Flag_SA)
table(YoungPeople$Pressured.into.sex, YoungPeople$Flag_PS)
table(YoungPeople$Paid.for.sex, YoungPeople$Flag_PAID)
table(YoungPeople$Drink.or.Drugs, YoungPeople$Flag_DD)
table(YoungPeople$Depression.or.low.mood, YoungPeople$Flag_DEP)
table(YoungPeople$Older.or.younger.partner, YoungPeople$Flag_PARTNER)


#create a new variable that gives a summary of all the other flags:
YoungPeople$Any_Flag <- 0
YoungPeople$Any_Flag = ifelse((YoungPeople$Flag_SA==1 | YoungPeople$Flag_PS==1 | YoungPeople$Flag_PAID==1|
                                 YoungPeople$Flag_DD==1| YoungPeople$Flag_DEP==1| YoungPeople$Flag_PARTNER), 1,0)
table(YoungPeople$Any_Flag)


#create a column that sums up flags
YoungPeople$Sum.Flags <- rowSums(YoungPeople[,c("Flag_SA",
                                                        "Flag_PS",
                                                        "Flag_PAID",
                                                        "Flag_DD",
                                                        "Flag_DEP",
                                                        "Flag_PARTNER")])
table(YoungPeople$Sum.Flags)

#Sophie Sep.2021: create new variables that - first - equal the safeguarding columns and - then - assign a string to flags
YoungPeople$Flag_Assaulted = ifelse(YoungPeople$Sexually.assaulted.risk.assessment=="yes","Assaulted","") 
YoungPeople$Flag_Pressured = ifelse(YoungPeople$Pressured.into.sex=="yes","Pressured","")
YoungPeople$Flag_Paid <- ifelse(YoungPeople$Paid.for.sex=="yes","Paid","")
YoungPeople$Flag_Drink <- ifelse(YoungPeople$Drink.or.Drugs=="yes","Drink","")
YoungPeople$Flag_Depression <- ifelse(YoungPeople$Depression.or.low.mood=="yes","Depression","")
YoungPeople$Flag_Partner <- ifelse(YoungPeople$Older.or.younger.partner=="yes","Partner","")
# concatenate all the flags in one column
YoungPeople$FlagsTogether <- paste(YoungPeople$Flag_Assaulted,"" ,YoungPeople$Flag_Pressured,"" , YoungPeople$Flag_Paid,"" , 
                                   YoungPeople$Flag_Drink,"" , YoungPeople$Flag_Depression,"" , YoungPeople$Flag_Partner)


#create new subset with only users with flags
YoungPeopleFlags <- YoungPeople [YoungPeople$Any_Flag==1,]

#order columns:
YoungPeopleFlagsOrdered <- YoungPeopleFlags [c("Created.at","Dispatched.at","Notified.at","Customer.ID","SH24.UID","Gender","Genitals","Age","Ethnicity",
            "Sexual.preference","Sexuality","Area","FlagsTogether","Sum.Flags","Flag_SA","Flag_PS","Flag_PAID","Flag_DD","Flag_DEP","Flag_PARTNER",
            "Syphilis","HIV","Chlamydia","Gonorrhoea","Hep.B","Hep.C")]


#create subset for Adults ***CHANGE MONTH***
Adults <- SelectedColumns [(SelectedColumns$Age>17 
                           & SelectedColumns$Created.at.month.year=="2021-11"),]

ZeroArea <- Adults[(Adults$Area=="0"),] 
rm(ZeroArea)


#In ADULTS there's only one safeguarding flag----
Adults$Flag_SA <- Adults$Sexually.assaulted.risk.assessment

#give value 1 if there is a flag, and 0 if there's no flag
Adults$Flag_SA = ifelse(Adults$Flag_SA=="yes",1,0)
table(Adults$Sexually.assaulted.risk.assessment, Adults$Flag_SA)

#create new subset with only adult users with flags
AdultsFlags <- Adults [Adults$Flag_SA==1,]
AdultsFlagOrdered <- AdultsFlags [c("Created.at","Dispatched.at","Notified.at","Customer.ID","SH24.UID","Gender","Genitals","Age","Ethnicity","Sexual.preference",
                                    "Sexuality","Area","Syphilis","HIV","Chlamydia","Gonorrhoea","Hep.B","Hep.C")]

#export data to csv (export the data of the ordered file). Use double \\ when setting destination file----
write.table (YoungPeopleFlagsOrdered, file="/Users/ElenaArdines1/Documents/Reports/1. Monthly Reports/Clinical Team Reports/Safeguarding/YoungPeople.csv", row.names=F, sep=",")
write.table(AdultsFlagOrdered, file="/Users/ElenaArdines1/Documents/Reports/1. Monthly Reports/Clinical Team Reports/Safeguarding/Adults.csv", row.names=F, sep=",")

write.table (Adults, file="/Users/ElenaArdines1/Documents/Reports/1. Monthly Reports/Clinical Team Reports/Safeguarding/Adults.csv", row.names=F, sep=",")


#Clean the environment
rm(list = ls())
