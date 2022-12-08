# Script to produce Xero file
# read files for invoicing from Backing_Data tab

# set date variables 
v1 <- '2022-11'               #v1: reporting month
v2 <- '01.11.2022-30.11.2022' #v2: activity period being invoiced
v3 <- "30/11/2022"            #v3: InvoiceDate
v4 <- "31/12/2022"            #v4: DueDate

# convert character to date, first set the format the date is shown 
invSTI$processed_at <- as.Date(invSTI$processed_at,"%Y-%m-%d")
# extract month from day date
invSTI$MonthYear <- format(as.Date(invSTI$processed_at),"%Y-%m")

# summarise month results to double check with Performance Summary 
STImonth <- invSTI[(invSTI$MonthYear == v1) , ]
table(STImonth$overall_type)



# Count of RPR kits processed as per invoicing query (https://github.com/sh24/sql_reports/blob/master/MIME_KEEP/sti_invoice_detail.sql)
# returns that include Syphilis RPR in reporting month = v1----
# no need to count kits dispatched with RPR as we don't invoice extra for those
RPR.Returned <- invSTI[(invSTI$overall_type == "kits_tested" & invSTI$MonthYear == v1),c("default_la","MonthYear","includes_syphilis_rpr")]
RPR.Returned <- RPR.Returned[(RPR.Returned$includes_syphilis_rpr == 1),]



# RPR.Dispatched <- orders[(orders$Dispatched.at.month.year == v1), c("Default.LA","Test.regime","Dispatched.at.month.year") ]
# RPR.Dispatched <- RPR.Dispatched[grep('RPR',RPR.Dispatched$Test.regime),]
# # Kits returned that include Syphilis RPR
# RPR.Returned <- orders[(orders$Lab.results.at.month.year == v1), c("Default.LA","Test.regime","Lab.results.at.month.year") ]
# RPR.Returned <- RPR.Returned[grep('RPR',RPR.Returned$Test.regime),]


# add column with description
#RPR.Dispatched$Description <- "RPR Syphilis tests dispatched"
RPR.Returned$Description <- "RPR Syphilis tests processed"

# remove columns not needed any more
#RPR.Dispatched$Test.regime = NULL
RPR.Returned$Test.regime = NULL
RPR.Returned$includes_syphilis_rpr = NULL


# rename (new variable name = existing variable name) to have same names in all data frames----
# RPR.Dispatched <- rename(RPR.Dispatched, default_la = Default.LA, MonthYear = Dispatched.at.month.year)
# RPR.Returned <- rename(RPR.Returned, default_la = Default.LA, MonthYear = Lab.results.at.month.year)
# END count of Syphilis RPR----








names(invSTI)


# extract the columns we need for invoicing
invSTI <- invSTI[,c("overall_type","default_la","MonthYear","invoice_category_billable","referred_from_token")]

# assign values to 'overall_type' that align with invoicing
invSTI$overall_type[invSTI$overall_type == 'kits_sent'] <- 'Orders'
invSTI$overall_type[invSTI$overall_type == 'kits_tested'] <- 'Returns'
table(invSTI$overall_type, invSTI$MonthYear == v1)

# concatenate values of both variables (type and category) to create the invoicing Description
invSTI$Description <- paste(invSTI$overall_type, invSTI$invoice_category_billable, sep=" - ")

# some returns are blank, showing in data.frame as 'Returns -'
# they relate to lab_results not interpreted by the mapping table in the DB. We are not charging for these.
# code blanks to be able to identify them with grep later
invSTI$Description[invSTI$invoice_category_billable == ""] <- "blank"
table(invSTI$Description)
        

# remove variables not needed anymore
invSTI$overall_type = NULL
invSTI$invoice_category_billable = NULL

# account for Hertfordshire hertstestyourself Council, invoiced at a different fee than Hertfordshire
# change name of default_LA
invSTI$default_la[invSTI$default_la == "Hertfordshire" & invSTI$referred_from_token == "BsLmCj8iKsTYxML0"] <- "Hertfordshire hertstestyourself Council"
# drop column not needed any more
invSTI$referred_from_token = NULL

# check 
test1 <- invSTI[(invSTI$MonthYear == v1),]
test1 <- test1[grep('Orders',test1$Description),]

test2 <- invSTI[(invSTI$MonthYear == v1),]
test2 <- test2[grep('Returns',test2$Description),]


# Include CT treatments. Read the file. Get needed columns
invTreatments <- Treatments[ , c("Region","Dispatched.MonthYear")]

# create variable Description to rbind with STI data frame
invTreatments$Description <- "CT Treatments"
# rename (new variable name = existing variable name) to have same names in all data frames
invTreatments <- rename(invTreatments, default_la = Region, MonthYear = Dispatched.MonthYear)


# Include contraception
invCOC <- COC[ , c("Dispatched.at.month.year","Months.prescribed","Drug","Region")]
invPOP <- POP[ , c("Dispatched.at.month.year","Months.prescribed","Drug","Region")]
invEC <- ECNow[ , c("Region","Dispatched.at.month.year","Drug")]
invInjectable <- Injectable[ , c("region","Injectable.months.prescribed","Dispatched.Month.Year")]
invPatch <- Patch[ , c("region","Patch.months.prescribed","Dispatched.Month.Year")]
invRing <- Ring[ , c("region","Ring.months.prescribed","Dispatched.Month.Year")]



# concatenate values to create 'Description'
invCOC$Description <- paste("Contraception COC",invCOC$Months.prescribed,"mth",invCOC$Drug)
invPOP$Description <- paste("Contraception POP",invPOP$Months.prescribed,"mth",invPOP$Drug)
invEC$Description <- paste("Contraception EC",invEC$Drug)
invInjectable$Description <- paste('Contraception Sayana Press 104mg / 0.65ml',invInjectable$Injectable.months.prescribed,"mth")
invPatch$Description <- paste('Contraception Patch',invPatch$Patch.months.prescribed,"mth")
invRing$Description <- paste('Contraception Ring',invRing$Ring.months.prescribed,"mth")

              
# rename (new variable name = existing variable name) to have same names in all data frames----
invCOC <- rename(invCOC, default_la = Region, MonthYear = Dispatched.at.month.year)
invPOP <- rename(invPOP, default_la = Region, MonthYear = Dispatched.at.month.year)
invEC <- rename(invEC, default_la = Region, MonthYear = Dispatched.at.month.year)
invInjectable <- rename(invInjectable, default_la = region, MonthYear = Dispatched.Month.Year)
invPatch <- rename(invPatch, default_la = region, MonthYear = Dispatched.Month.Year)
invRing <- rename(invRing, default_la = region, MonthYear = Dispatched.Month.Year)

# remove variables not needed 
invCOC$Months.prescribed = NULL
invCOC$Drug = NULL
invPOP$Months.prescribed = NULL
invPOP$Drug = NULL
invEC$Drug = NULL
invInjectable$Injectable.months.prescribed = NULL
invPatch$Patch.months.prescribed = NULL
invRing$Ring.months.prescribed = NULL

# Photo diagnosis consultations and treatments
# invoice consultations as per the number of consultations with a diagnose time stamp in the relevant month
invPDConsult <- PhotoConsult[ , c("diagnosed_month_year","region")]
# rename (new variable name = existing variable name) to have same names in all data frames
invPDConsult <- rename(invPDConsult, default_la = region, MonthYear = diagnosed_month_year)
# create variable Description
invPDConsult$Description <- "Photo Diagnosis Consultations"

# invoice PD treatments depending on the drug posted
invPDTreatm <- PhotoTreatm[,c('dispatched_month_year','name',"Drug")]
# include the name of the drug, related to the drug code
invPDTreatm$DrugName <- ""
invPDTreatm$DrugName[invPDTreatm$Drug == "3005"] <- "Imiquimod"
invPDTreatm$DrugName[invPDTreatm$Drug == "3006"] <- "Podophyllotoxin"
invPDTreatm$DrugName[invPDTreatm$Drug == "3007"] <- "Aciclovir_episodic 800mg"
invPDTreatm$DrugName[invPDTreatm$Drug == "3008"] <- "Aciclovir_suppressive 400mg"
invPDTreatm$DrugName[invPDTreatm$Drug == "3030"] <- "Condyline"
# concatenate values to create 'Description'
invPDTreatm$Description <- paste("Photo Treatments",invPDTreatm$Drug,invPDTreatm$DrugName)
# rename (new variable name = existing variable name) to have same names in all data frames
invPDTreatm <- rename(invPDTreatm, default_la = name, MonthYear = dispatched_month_year)
# drop columns no needed
invPDTreatm$Drug = NULL
invPDTreatm$DrugName = NULL


names(invSTI)
names(invCOC)
names(invPOP)
names(invEC)
names(invInjectable)
names(invPatch)
names(invTreatments)
names(RPR.Returned)
names(invPDConsult)
names(invPDTreatm)
#names(invBolts)


# Stack data sets one on top of the other ----
invoicing <- rbind(invSTI,invTreatments,invCOC,invPOP,invEC,invInjectable,invPatch,invRing,RPR.Returned,invPDConsult,invPDTreatm)


# check
#### testinvoicing <- as.data.frame(table(invoicing$Description, invoicing$MonthYear == v1))  
#### write.table (testinvoicing, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2022\\2022_04\\Xero_Quantities_2022.April_v9.csv", row.names=F, sep=",")


# Create variable to group freetesting, Ireland etc together
        invoicing$Service <- invoicing$default_la
        invoicing$Service[grepl("Freetesting -", invoicing$default_la)] <- "Freetesting"
        invoicing$Service[grepl("Ireland -", invoicing$default_la)] <- "Ireland"
        invoicing$Service[grepl("Northern Ireland ", invoicing$default_la)] <- "Northern Ireland"
        invoicing$Service[grepl("PrEP Trial -", invoicing$default_la)] <- "PrEP Trial"
        invoicing$Service[(invoicing$default_la == "Bury" | invoicing$default_la == "Rochdale" | invoicing$default_la == "Oldham" )] <- "Orbish"

# Create ContactName, to name regions as per invoicing requirements
invoicing$ContactName <- invoicing$Service
invoicing$ContactName[invoicing$default_la == "Blackburn with Darwen"] <- "Blackburn"
invoicing$ContactName[invoicing$default_la == "Cornwall and Isles of Scilly PCT"] <- "Cornwall and Isles of Scilly"
invoicing$ContactName[invoicing$default_la == "County Durham"] <- "County Durham and Darlington NHS Foundation Trust"
invoicing$ContactName[invoicing$default_la == "Derby"] <- "Derby City"
invoicing$ContactName[invoicing$default_la == "Derbyshire"] <- "Derbyshire Community Health Services NHS Foundation Trust"
invoicing$ContactName[invoicing$default_la == "East Berkshire"] <- "Berkshire"
invoicing$ContactName[invoicing$default_la == "Hertfordshire"] <- "Hertfordshire"
invoicing$ContactName[invoicing$default_la == "Hertfordshire hertstestyourself Council"] <- "Hertfordshire hertstestyourself Council"
invoicing$ContactName[invoicing$default_la == "Hillingdon"] <- "London Northwest Healthcare"
invoicing$ContactName[invoicing$default_la == "Leicester"] <- "Leicester City"
invoicing$ContactName[invoicing$default_la == "Nottingham"] <- "Nottingham City Council"
invoicing$ContactName[invoicing$default_la == "PHE"] <- "UKHSA"
invoicing$ContactName[invoicing$default_la == "PrEP Trial"] <- "UKHSA PrEP Trial"
invoicing$ContactName[invoicing$default_la == "Southend-on-Sea"] <- "Southend"
invoicing$ContactName[invoicing$default_la == "Staffordshire"] <- "South Staffordshire and Shropshire Healthcare NHS Foundation Trust"
invoicing$ContactName[invoicing$default_la == "Stoke-on-Trent"] <- "Stoke on Trent"



# extract data for the relevant month, and columns needed for invoicing: use variable v1 defined above
invMonth <- invoicing[(invoicing$MonthYear == v1),c("ContactName","MonthYear",'Description')]

# check
test3 <- invMonth[grep('Orders',invMonth$Description),]
test4 <- invMonth[grep('Returns',invMonth$Description),]

# convert to data.frame to get the count of the number of tests
invMonth_1 = as.data.frame(table (invMonth$ContactName,invMonth$Description))
#name the columns
colnames(invMonth_1)[1] <- "ContactName"
colnames(invMonth_1)[2] <- "Description"
colnames(invMonth_1)[3] <- "Quantity"


# Include bolt-ons ----
invBolts <- bolts
# concatenate year and month
invBolts$MonthYear <- paste(invBolts$year, invBolts$month, sep="-")
# extract MonthYear = relevant month
invBolts <- invBolts[(invBolts$MonthYear == v1),]
# rename (new variable name = existing variable name) to have same names in all data frames
invBolts <- rename(invBolts, ContactName = region_name, Description = product_name, Quantity = count)
# name bolt-ons as per invoicing categories
invBolts$Description[invBolts$Description == "pop"] <- "POP bolt-on Desogestrel 3 months"
invBolts$Description[invBolts$Description == "condoms"] <- "Bolt-on condoms"
invBolts$Description[invBolts$Description == "lube"] <- "Bolt-on lube"
invBolts$Description[invBolts$Description == "pregnancy test"] <- "Bolt-on pregnancy test"
invBolts$Description[invBolts$Description == "sti test"] <- "Bolt-on STI test"
# delete not-needed variables
invBolts$year = NULL
invBolts$month = NULL
invBolts$parent_order_type = NULL
invBolts$MonthYear = NULL
# END Include bolt-ons ----

# Stack the boltons to the rest of the invoicing ----
invMonth_2 <- rbind(invMonth_1,invBolts)


# create invoice number column----
# first half of the invoice number is the month
invMonth_2$Invoice <- 'Inv '
# second half of the invoice number is associated to the specific Trust/Region, which we call ContactName in this script
invMonth_2$Number <- ""
invMonth_2$Number[invMonth_2$ContactName == "Essex"] <- "0001"
invMonth_2$Number[invMonth_2$ContactName == "Shropshire"] <- "0002"
invMonth_2$Number[invMonth_2$ContactName == "Telford and Wrekin"] <- "0003"
invMonth_2$Number[invMonth_2$ContactName == "Worcestershire"] <- "0005"
invMonth_2$Number[invMonth_2$ContactName == "London Northwest Healthcare"] <- "0006"
invMonth_2$Number[invMonth_2$ContactName == "County Durham and Darlington NHS Foundation Trust"] <- "0007"
invMonth_2$Number[invMonth_2$ContactName == "Derbyshire Community Health Services NHS Foundation Trust"] <- "0008"
invMonth_2$Number[invMonth_2$ContactName == "South Staffordshire and Shropshire Healthcare NHS Foundation Trust"] <- "0009"
invMonth_2$Number[invMonth_2$ContactName == "Hertfordshire"] <- "00010"
invMonth_2$Number[invMonth_2$ContactName == "Hertfordshire hertstestyourself Council"] <- "00011"
invMonth_2$Number[invMonth_2$ContactName == "Berkshire"] <- "00012"
invMonth_2$Number[invMonth_2$ContactName == "Bradford"] <- "00013"
invMonth_2$Number[invMonth_2$ContactName == "Knowsley"] <- "00014"
invMonth_2$Number[invMonth_2$ContactName == "Darlington"] <- "00015"
invMonth_2$Number[invMonth_2$ContactName == "Leicester City"] <- "00016"
invMonth_2$Number[invMonth_2$ContactName == "Rutland"] <- "00017"
invMonth_2$Number[invMonth_2$ContactName == "Leicestershire"] <- "00018"
invMonth_2$Number[invMonth_2$ContactName == "Derby City"] <- "00019"
invMonth_2$Number[invMonth_2$ContactName == "North Staffordshire"] <- "00020"
invMonth_2$Number[invMonth_2$ContactName == "Gateshead"] <- "00021"
invMonth_2$Number[invMonth_2$ContactName == "Cheshire East"] <- "00022"
invMonth_2$Number[invMonth_2$ContactName == "Halton"] <- "00023"
invMonth_2$Number[invMonth_2$ContactName == "Warrington"] <- "00024"
invMonth_2$Number[invMonth_2$ContactName == "Northern Ireland"] <- "00025"
invMonth_2$Number[invMonth_2$ContactName == "UKHSA"] <- "00026"
invMonth_2$Number[invMonth_2$ContactName == "Cornwall and Isles of Scilly"] <- "00027"
invMonth_2$Number[invMonth_2$ContactName == "Liverpool"] <- "00028"
invMonth_2$Number[invMonth_2$ContactName == "Stoke on Trent"] <- "00029"
invMonth_2$Number[invMonth_2$ContactName == "Sunderland"] <- "00030"
invMonth_2$Number[invMonth_2$ContactName == "Nottingham City Council"] <- "00031"
invMonth_2$Number[invMonth_2$ContactName == "Buckinghamshire"] <- "00032"
invMonth_2$Number[invMonth_2$ContactName == "Dorset"] <- "00033"
invMonth_2$Number[invMonth_2$ContactName == "Kirklees"] <- "00035"
invMonth_2$Number[invMonth_2$ContactName == "South Tyneside"] <- "00036"
invMonth_2$Number[invMonth_2$ContactName == "East Sussex"] <- "00037"
invMonth_2$Number[invMonth_2$ContactName == "UKHSA PrEP Trial"] <- "00038"
invMonth_2$Number[invMonth_2$ContactName == "Bromley"] <- "00039"
invMonth_2$Number[invMonth_2$ContactName == "Blackburn"] <- "00040"
invMonth_2$Number[invMonth_2$ContactName == "Southend"] <- "00041"
invMonth_2$Number[invMonth_2$ContactName == "Teesside"] <- "00042"
invMonth_2$Number[invMonth_2$ContactName == "Thurrock"] <- "00043"
invMonth_2$Number[invMonth_2$ContactName == "Wirral"] <- "0056"
invMonth_2$Number[invMonth_2$ContactName == "Rotherham"] <- "0057"
invMonth_2$Number[invMonth_2$ContactName == "Stockport"] <- "0058"
invMonth_2$Number[invMonth_2$ContactName == "Tameside"] <- "0059"
invMonth_2$Number[invMonth_2$ContactName == "Orbish"] <- "0060"


# create InvoiceNumber as concatenate of 'Invoice' and 'Number'
invMonth_2$InvoiceNumber <- paste(invMonth_2$Invoice, v1, invMonth_2$Number, sep=" ")

# remove columns no needed anymore
invMonth_2$Invoice = NULL
invMonth_2$Number = NULL


# create reference. Use 'Region' instead of 'ContactName' as a better reference to our records
invMonth_2$Reference <- paste("SH:24 ",invMonth_2$ContactName, "activity", v2)
# invoice dates----
invMonth_2$InvoiceDate <- v3
invMonth_2$DueDate <- v4

        
# remove dataframe rows based on zero values in one column
invMonth_3 <- invMonth_2[invMonth_2$Quantity != 0, ]
# export to check figures 
# use ~ to avoid portability issues when defining Home Directory
#### DELETE? 5th.Sep.20222: write.table (invMonth_3, file="~/Reports/1.Monthly_Reports/Invoicing/2022_07/Check_Xero_Quantities_2022.07.csv", row.names=F, sep=",")



#remove Description = blank (those are the returns that came with a lab_error, we don't charge for them)
invMonth_3 <- invMonth_3[invMonth_3$Description != 'blank', ]
#remove Medway invoice
invMonth_3 <- invMonth_3[(invMonth_3$ContactName != 'Medway'), ]


# create price data frames
Description <- c('Orders - All STIs (dual site)','Orders - All STIs (single site)','Orders - All STIs (triple site)',
                'Orders - CT/GC (dual site)','Orders - CT/GC (single site)','Orders - CT/GC (triple site)',
                'Orders - HIV',
                'Orders - HIV & CT/GC (dual site)','Orders - HIV & CT/GC (single site)','Orders - HIV & CT/GC (triple site)',
                'Orders - HIV & Syphilis',
                'Orders - Syphilis',
                'Orders - Syphilis & CT/GC (dual site)','Orders - Syphilis & CT/GC (single site)','Orders - Syphilis & CT/GC (triple site)',
                'Orders - Hep: 1 blood',
                'Orders - Hep: 1 blood & CT/GC (dual site)','Orders - Hep: 1 blood & CT/GC (single site)','Orders - Hep: 1 blood & CT/GC (triple site)',
                'Orders - Hep: 2 bloods',
                'Orders - Hep: 2 bloods & CT/GC (dual site)','Orders - Hep: 2 bloods & CT/GC (single site)','Orders - Hep: 2 bloods & CT/GC (triple site)',
                'Orders - Hep: 3 bloods',
                'Orders - Hep: 3 bloods & CT/GC (dual site)','Orders - Hep: 3 bloods & CT/GC (single site)','Orders - Hep: 3 bloods & CT/GC (triple site)',
                'Orders - Hep: 4 bloods',
                'Orders - Hep: 4 bloods & CT/GC (dual site)',
                'Orders - Hep: 4 bloods & CT/GC (single site)',
                'Orders - Hep: 4 bloods & CT/GC (triple site)',
                'Returns - All STIs (dual site)','Returns - All STIs (single site)','Returns - All STIs (triple site)',
                'Returns - CT/GC (dual site)','Returns - CT/GC (single site)','Returns - CT/GC (triple site)',
                'Returns - HIV',
                'Returns - HIV & CT/GC (dual site)','Returns - HIV & CT/GC (single site)','Returns - HIV & CT/GC (triple site)',
                'Returns - HIV & Syphilis',
                'Returns - Syphilis',
                'Returns - Syphilis & CT/GC (dual site)','Returns - Syphilis & CT/GC (single site)','Returns - Syphilis & CT/GC (triple site)',
                'Returns - Hep: 1 blood',
                'Returns - Hep: 1 blood & CT/GC (dual site)','Returns - Hep: 1 blood & CT/GC (single site)','Returns - Hep: 1 blood & CT/GC (triple site)',
                'Returns - Hep: 2 bloods',
                'Returns - Hep: 2 bloods & CT/GC (dual site)','Returns - Hep: 2 bloods & CT/GC (single site)','Returns - Hep: 2 bloods & CT/GC (triple site)',
                'Returns - Hep: 3 bloods',
                'Returns - Hep: 3 bloods & CT/GC (dual site)','Returns - Hep: 3 bloods & CT/GC (single site)','Returns - Hep: 3 bloods & CT/GC (triple site)',
                'Returns - Hep: 4 bloods',
                'Returns - Hep: 4 bloods & CT/GC (dual site)',
                'Returns - Hep: 4 bloods & CT/GC (single site)',
                'Returns - Hep: 4 bloods & CT/GC (triple site)',
              'CT Treatments',
              'Contraception COC 3 mth Levonorgestrel/Ethinylestradiol',
              'Contraception COC 3 mth Desogestrel / Ethinyloestradiol 20',
              'Contraception COC 3 mth Desogestrel / Ethinyloestradiol 30',
              'Contraception COC 3 mth Gestodene / Ethinyloestradiol',
              'Contraception COC 3 mth Norgestimate / Ethinyloestradiol',
              'Contraception COC 6 mth Levonorgestrel/Ethinylestradiol',
              'Contraception COC 6 mth Desogestrel / Ethinyloestradiol 20',
              'Contraception COC 6 mth Desogestrel / Ethinyloestradiol 30',
              'Contraception COC 6 mth Gestodene / Ethinyloestradiol',
              'Contraception COC 6 mth Norgestimate / Ethinyloestradiol',
              'Contraception COC 12 mth Levonorgestrel/Ethinylestradiol',
              'Contraception COC 12 mth Desogestrel / Ethinyloestradiol 20',
              'Contraception COC 12 mth Desogestrel / Ethinyloestradiol 30',
              'Contraception COC 12 mth Gestodene / Ethinyloestradiol',
              'Contraception COC 12 mth Norgestimate / Ethinyloestradiol',
              'Contraception POP 3 mth Desogestrel',
              'Contraception POP 3 mth Levonorgestrel',
              'Contraception POP 3 mth Noriday',
              'Contraception POP 6 mth Desogestrel',
              'Contraception POP 6 mth Levonorgestrel',
              'Contraception POP 6 mth Noriday',
              'Contraception POP 12 mth Desogestrel',
              'Contraception POP 12 mth Levonorgestrel',
              'Contraception POP 12 mth Noriday',
              'Contraception POP bolt-on Desogestrel 3 months',
              'Bolt on condoms',
              'Bolt on lube',
              'Bolt on pregnancy test',
              'Bolt on STI test',
              'Contraception EC EllaOne',
              'Contraception EC Levonelle',
              'Contraception EC Levonorgestrel 1.5mg',
              'Photo Diagnosis Consultations',
              'Photo Treatments 3005 Imiquimod',
              'Photo Treatments 3006 Podophyllotoxin',
              'Photo Treatments 3007 Aciclovir_episodic 800mg',
              'Photo Treatments 3008 Aciclovir_suppressive 400mg',
              'Photo Treatments 3030 Condyline',
              'RPR Syphilis tests processed',
              'Contraception Ring 3 mth',
              'Contraception Ring 6 mth',
              'Contraception Ring 12 mth',
              'Contraception Patch 3 mth',
              'Contraception Patch 6 mth',
              'Contraception Patch 12 mth',
              'Contraception Sayana Press 104mg / 0.65ml 3 mth',
              'Contraception Sayana Press 104mg / 0.65ml 6 mth',
              'Contraception Sayana Press 104mg / 0.65ml 12 mth')

# define prices per tariff
Fee1DiscountRM <-c( 3.60, 3.04, 4.18, 2.99, 2.34, 3.16,2.72, 3.60, 3.04, 4.18, 2.72,2.72, 3.60, 3.04, 4.18, 2.72,3.60, 3.04, 4.18, 2.72, 3.60, 3.04, 4.18, 2.72, 3.60, 3.04, 4.18, 2.72, 3.60, 3.04, 4.18,#orders
                   46.13,31.60,51.10,29.06,13.71,34.03,9.64,37.60,23.62,42.56,18.17,9.64,37.60,23.62,42.56,9.64,37.60,23.62,42.56,18.17,46.13,31.60,51.10,26.70,54.66,40.13,59.63,35.23,63.19,48.66,68.16,#returns
                   21.11, #CT treatments 
                   16.30,16.65,16.65,16.65,16.65,19.61,26.50,26.50,26.50,26.50,26.22,36.35,36.35,36.35,36.35,#COC prices
                   17.92,14.92,17.61,22.85,20.00,20.37,32.70,25.08,23.13,#POP prices
                    3.48, 0.52, 0.79, 0.65, 4.25,#Bolt-ons prices: POP bolt-on, condoms, lube, preg test
                   28.50,15.00,15.00,#EC prices
                   18.95,64.54,33.77,22.87,18.79,33.77,#Photo diagnosis: consultations first, then treatments
                   12.10,#Syphilis RPR returned
                   34.36,53.87,73.38,44.55,74.25,103.95,29.75,36.65,43.55)#Ring (3,6, 12 months), Patch, Injectable

Fee2Standard <- c( 6.54, 5.52, 7.60, 5.44, 4.25, 5.85, 4.94, 6.54, 5.52, 7.60, 4.94, 4.94, 6.54, 5.52, 7.60, 4.94, 6.54, 5.52, 7.60, 4.94, 6.54, 5.52, 7.60, 4.94, 5.52, 6.54, 7.60, 4.94, 5.52, 6.54, 7.60,
                  66.60,38.48,72.82,37.22,19.16,55.28,16.62,54.19,33.85,61.41,22.82,13.52,54.19,33.85,61.41,16.62,54.19,33.85,61.41,22.82,66.60,38.48,72.82,33.54,44.68,72.80,79.02,39.74,50.88,79.00,85.22,
                  20.05,
                  16.30,16.65,16.65,16.65,16.65,19.61,26.50,26.50,26.50,26.50,26.22,36.35,36.35,36.35,36.35,
                  17.92,14.92,17.61,22.85,20.00,20.37,32.70,25.08,23.13,
                   3.48, 0.52, 0.79, 0.65, 4.25,
                  28.50,15.00,15.00,
                  18.95,64.54,33.77,22.87,18.79,33.77,
                  12.10,
                  34.36,53.87,73.38,44.55,74.25,103.95,29.75,36.65,43.55)

Fee3Discount <- c( 3.60, 3.04, 4.18, 2.99, 2.34, 3.16,2.72, 3.60, 3.04, 4.18, 2.72,2.72, 3.60, 3.04, 4.18,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
                  45.03,30.50,50.00,27.96,12.61,32.93,8.54,36.50,22.52,41.46,17.07,8.54,36.50,22.52,41.46,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
                  21.11,
                  16.30,16.65,16.65,16.65,16.65,19.61,26.50,26.50,26.50,26.50,26.22,36.35,36.35,36.35,36.35,
                  17.92,14.92,17.61,22.85,20.00,20.37,32.70,25.08,23.13,
                   3.48, 0.52, 0.79, 0.65, 4.25,
                  28.50,15.00,15.00,
                  18.95,64.54,33.77,22.87,18.79,33.77,
                  12.10,
                  34.36,53.87,73.38,44.55,74.25,103.95,29.75,36.65,43.55)

Fee4DiscountRM5 <- c( 3.42, 2.89, 3.97, 2.84, 2.22, 3.00,2.58, 3.42, 2.89, 3.97, 2.58,2.58, 3.42, 2.89, 3.97,2.72, 3.60, 3.04, 4.18, 2.72, 3.60, 3.04, 4.18, 2.72, 3.60, 3.04, 4.18, 2.72, 3.60, 3.04, 4.18,
                     43.88,30.08,48.60,27.66,13.08,32.38,9.21,35.78,22.49,40.49,17.32,9.21,35.78,22.49,40.49,9.64,37.60,23.62,42.56,18.17,46.13,31.60,51.10,26.70,54.66,40.13,59.63,35.23,63.19,48.66,68.16,
                     20.05,
                     16.30,16.65,16.65,16.65,16.65,19.61,26.50,26.50,26.50,26.50,26.22,36.35,36.35,36.35,36.35,
                     17.92,14.92,17.61,22.85,20.00,20.37,32.70,25.08,23.13,
                      3.48, 0.52, 0.79, 0.65, 4.25,
                     28.50,15.00,15.00,
                     18.95,64.54,33.77,22.87,18.79,33.77,
                     12.10,
                     34.36,53.87,73.38,44.55,74.25,103.95,29.75,36.65,43.55)

# freetesting invoices are quarterly and done separately
Fee5Freetesting <- c(0.00,0.00,0.00,0.00,0.00,0.00,2.72,0.00,0.00,0.00, 2.72,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
                     0.00,0.00,0.00,0.00,0.00,0.00,9.64,0.00,0.00,0.00,18.18,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
                     0.00,
                     0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
                     0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
                     0.00,0.00,0.00,0.00,0.00,
                     0.00,0.00,0.00,
                     0.00,0.00,0.00,0.00,0.00,0.00,
                     0.00,
                     0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)

eSRH <- c( 6.54, 5.52, 7.60, 5.44, 4.25, 5.85, 4.94, 6.54, 5.52, 7.60, 4.94, 4.94, 6.54, 5.52, 7.60,4.94,6.54,5.52,7.60,4.94,6.54,5.52,7.60,4.94,5.52,6.54,7.60,4.94,5.52,6.54,7.60,
          46.45,30.33,49.26,37.07,19.01,46.13,16.47,39.04,33.70,46.26,20.67,13.37,39.04,33.70,46.26,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
          21.11,
          16.30,16.65,16.65,16.65,16.65,19.61,26.50,26.50,26.50,26.50,26.22,36.35,36.35,36.35,36.35,
          17.92,14.92,17.61,22.85,20.00,20.37,32.70,25.08,23.13,
           3.48, 0.52, 0.79, 0.65, 4.25,
          28.50,15.00,15.00,
          18.95,64.54,33.77,22.87,18.79,33.77,
          12.10,
          34.36,53.87,73.38,44.55,74.25,103.95,29.75,36.65,43.55)

eSRH5 <- c( 6.21, 5.24, 7.22, 5.17, 4.04, 5.56, 4.69, 6.21, 5.24, 7.22, 4.69, 4.69, 6.21, 5.24, 7.22,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
           44.13,28.81,46.80,35.22,18.06,43.82,15.65,37.09,32.02,43.95,19.64,12.70,37.09,32.02,43.95,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
           21.11,
           16.30,16.65,16.65,16.65,16.65,19.61,26.50,26.50,26.50,26.50,26.22,36.35,36.35,36.35,36.35,
           17.92,14.92,17.61,22.85,20.00,20.37,32.70,25.08,23.13,
            3.48, 0.52, 0.79, 0.65, 4.25,
           28.50,15.00,15.00,
           18.95,64.54,33.77,22.87,18.79,33.77,
           12.10,
           34.36,53.87,73.38,44.55,74.25,103.95,29.75,36.65,43.55)

FeeZero <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
            0.00,
            0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
            0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
            0.00,0.00,0.00,0.00,0.00,
            0.00,0.00,0.00,
            0.00,0.00,0.00,0.00,0.00,0.00,
            0.00,
            0,0,0,0,0,0,0,0,0)

# create data frame with prices per invoicing category
fee1 <- data.frame(Description, Fee1DiscountRM)
fee2 <- data.frame(Description, Fee2Standard)
fee3 <- data.frame(Description, Fee3Discount)
fee4 <- data.frame(Description, Fee4DiscountRM5)
fee5 <- data.frame(Description, Fee5Freetesting) # freetesting done separately
fee6 <- data.frame(Description, eSRH)
fee7 <- data.frame(Description, eSRH5)
fee0 <- data.frame(Description, FeeZero)


# break down the main invoicing data set as per the fee contracted in each area 
InvFee1 <- invMonth_3 [(invMonth_3$ContactName=="Blackburn" | invMonth_3$ContactName=="Bromley"
                        | invMonth_3$ContactName=="Cheshire East" | invMonth_3$ContactName=="Cornwall and Isles of Scilly"
                        | invMonth_3$ContactName=="County Durham and Darlington NHS Foundation Trust" | invMonth_3$ContactName=="Darlington"
                        | invMonth_3$ContactName=="Derby City" | invMonth_3$ContactName=="Derbyshire Community Health Services NHS Foundation Trust"
                        | invMonth_3$ContactName=="Dorset" | invMonth_3$ContactName=="Halton"
                        | invMonth_3$ContactName=="Berkshire"
                        | invMonth_3$ContactName=="Knowsley" | invMonth_3$ContactName=="Liverpool"
                        | invMonth_3$ContactName=="UKHSA" | invMonth_3$ContactName=="Southend"  
                        | invMonth_3$ContactName=="Warrington" | invMonth_3$ContactName=="East Sussex" | invMonth_3$ContactName=="Teesside"  | invMonth_3$ContactName=="Orbish"
                        | invMonth_3$ContactName=="Hertfordshire" 
                        | invMonth_3$ContactName=="Bury" | invMonth_3$ContactName=="Rochdale" | invMonth_3$ContactName=="Oldham"),]

InvFee2 <- invMonth_3 [(invMonth_3$ContactName=="Gateshead" | invMonth_3$ContactName=="Glasgow" 
                        | invMonth_3$ContactName=="London Northwest Healthcare" | invMonth_3$ContactName=="South Tyneside" 
                        | invMonth_3$ContactName=="Sunderland" | invMonth_3$ContactName=="Worcestershire"
                        ) , ]

    ### DELETE? 5th.Sep.2022 Berkshire changing to Discount + Royal Mail in August invoicing
    ### InvFee3 <- invMonth_3 [(invMonth_3$ContactName=="Berkshire"),]

InvFee4 <- invMonth_3 [(invMonth_3$ContactName=="Leicester City" | invMonth_3$ContactName=="Leicestershire"
                        | invMonth_3$ContactName=="North Staffordshire" | invMonth_3$ContactName=="Rutland"
                        | invMonth_3$ContactName=="Shropshire" | invMonth_3$ContactName=="South Staffordshire and Shropshire Healthcare NHS Foundation Trust"
                        | invMonth_3$ContactName=="Stoke on Trent" | invMonth_3$ContactName=="Telford and Wrekin") ,]

# freetesting done separately
InvFee5 <- invMonth_3 [(invMonth_3$ContactName=="Freetesting"),] 

InvFee6 <- invMonth_3 [(invMonth_3$ContactName=="Buckinghamshire" | invMonth_3$ContactName=="Northern Ireland"
                        | invMonth_3$ContactName=="Rotherham" | invMonth_3$ContactName=="Stockport" | invMonth_3$ContactName=="Tameside"),]

InvFee7 <- invMonth_3 [(invMonth_3$ContactName=="Essex" | invMonth_3$ContactName=="Thurrock" | invMonth_3$ContactName=="Wirral"
                        | invMonth_3$ContactName=="Bradford" | invMonth_3$ContactName=="Kirklees" 
                          | invMonth_3$ContactName=="Nottingham City Council"),]

# some areas not invoiced. Count them anyway to check whole picture
InvFeeZero <- invMonth_3 [(invMonth_3$ContactName=="Ireland") | (invMonth_3$ContactName=="Romania") | (invMonth_3$ContactName=="Fettle")
                        | (invMonth_3$ContactName == "Medway"),]

# check that sum of the invoices grouped equals total of invoicing in file invMonth_3:
nrow(InvFee1)
nrow(InvFee2)
   ### DELETE? 5th.Sep.2022 Berkshire was the only Trust on this tariff - not since August: nrow(InvFee3)
nrow(InvFee4)
nrow(InvFee5)
nrow(InvFee6)
nrow(InvFee7)
nrow(InvFeeZero)

# merge each price data set with its correspondent areas
InvoicesFee1 = merge(x = InvFee1, y = fee1, by = "Description", all.x = TRUE)
InvoicesFee2 = merge(x = InvFee2, y = fee2, by = "Description", all.x = TRUE)
# Berkshire tariff doesn't exist since Sep 2022: InvoicesFee3 = merge(x = InvFee3, y = fee3, by = "Description", all.x = TRUE)
InvoicesFee4 = merge(x = InvFee4, y = fee4, by = "Description", all.x = TRUE)
# freetesting done separately
#InvoicesFee5 = merge(x = InvFee5, y = fee5, by = "Description", all.x = TRUE)
InvoicesFee6 = merge(x = InvFee6, y = fee6, by = "Description", all.x = TRUE)
InvoicesFee7 = merge(x = InvFee7, y = fee7, by = "Description", all.x = TRUE)
InvoicesFeeZero = merge(x = InvFeeZero, y = fee0, by = "Description", all.x = TRUE)

# rename variables in the data frames that include prices. Variables have to be called the same to be able to stack data frames
#rename(new variable name = existing variable name)
InvoicesFee1 <- rename(InvoicesFee1, UnitAmount = Fee1DiscountRM)
InvoicesFee2 <- rename(InvoicesFee2, UnitAmount = Fee2Standard)
# Berkshire tariff doesn't exist since Sep 2022: InvoicesFee3 <- rename(InvoicesFee3, UnitAmount = Fee3Discount)
InvoicesFee4 <- rename(InvoicesFee4, UnitAmount = Fee4DiscountRM5)
# freetesting done separately
#InvoicesFee5 <- rename(InvoicesFee5, UnitAmount = Fee5Freetesting)
InvoicesFee6 <- rename(InvoicesFee6, UnitAmount = eSRH)
InvoicesFee7 <- rename(InvoicesFee7, UnitAmount = eSRH5)
InvoicesFeeZero <- rename(InvoicesFeeZero, UnitAmount = FeeZero)

# Stack data sets one on top of the other 
InvoicesStack <- rbind(InvoicesFee1, InvoicesFee2, InvoicesFee4, InvoicesFee6, InvoicesFee7, InvoicesFeeZero)  


# create the rest of the variables needed for the Xero file
InvoicesStack$EmailAddress <- ""
InvoicesStack$POAddressLine1 <- ""
InvoicesStack$POAddressLine2 <- ""
InvoicesStack$POAddressLine3 <- ""
InvoicesStack$POAddressLine4 <- ""
InvoicesStack$POCity <- ""
InvoicesStack$PORegion <- ""
InvoicesStack$POPostalCode <- ""
InvoicesStack$POCountry <- ""
InvoicesStack$Total <- ""
InvoicesStack$InventoryItemCode <- ""
InvoicesStack$Discount <- ""
InvoicesStack$AccountCode <- "200"
InvoicesStack$AccountCode[grepl("Contraception", InvoicesStack$Description)] <- "206"
table(InvoicesStack$AccountCode)

InvoicesStack$TaxType <- 0
InvoicesStack$TaxType = ifelse(InvoicesStack$AccountCode =='200' ,"Exempt Income","Zero Rated Income")
table(InvoicesStack$TaxType)

InvoicesStack$TaxAmount <- ""
InvoicesStack$TrackingName1 <- "Workstream"
InvoicesStack$TrackingOption1 <- "Operations"
InvoicesStack$TrackingName2 <- ""
InvoicesStack$TrackingOption2 <- ""
InvoicesStack$Currency <- "GBP"
InvoicesStack$BrandingTheme <- "Standard SH24 (Accounts)"

# select and order the columns we need
InvoicesStack_Ordered <- InvoicesStack [c("ContactName","EmailAddress","POAddressLine1","POAddressLine2","POAddressLine3","POAddressLine4",
                                          "POCity","PORegion","POPostalCode","POCountry",
                                          "InvoiceNumber","Reference","InvoiceDate","DueDate","Total","InventoryItemCode","Description",
                                          "Quantity","UnitAmount","Discount","AccountCode","TaxType","TaxAmount","TrackingName1","TrackingOption1",
                                          "TrackingName2","TrackingOption2","Currency","BrandingTheme")]
# order data first by Area (=ContactName) and second by the invoicing category (=Description)
InvoicesStack_Ordered <- InvoicesStack_Ordered[order(InvoicesStack_Ordered$ContactName),]


# Replace <NA> in Unit.Amount with zero ----
InvoicesStack_Ordered[is.na(InvoicesStack_Ordered)] <- "0"

write.table (InvoicesStack_Ordered, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_11/20221207_Xero_Nov.csv", row.names=F, sep=",")

