# producing Xero file

# read STI orders, CT treatment, contraception and photo diagnosis files from backing data

# specific files for invoicing:
invSTI = read.csv("20220606_Invoicing.csv")
bolts = read.csv("20220606_EC_Now_boltons_Disaggregated.csv")

# set date variables 
v1 <- '2022-05'               #v1: reporting month
v2 <- '01.05.2022-31.05.2022' #v2: activity period being invoiced
v3 <- "31/05/2022"            #v3: InvoiceDate
v4 <- "30/06/2022"            #v4: DueDate

# convert character to date, first set the format the date is shown 
invSTI$processed_at <- as.Date(invSTI$processed_at,"%Y-%m-%d")
# extract month from day date
invSTI$Dispatched.MonthYear <- format(as.Date(invSTI$processed_at),"%Y-%m")

# summarise month results to double check with Performance Summary 
STImonth <- invSTI[(invSTI$Dispatched.MonthYear == v1) , ]
table(STImonth$overall_type, STImonth$repeat_kit)
table(STImonth$overall_type)


# extract the columns we need for invoicing
invSTI <- invSTI[,c("overall_type","default_la","Dispatched.MonthYear","invoice_category_billable","referred_from_token")]

# assign values to 'overall_type' that align with invoicing
invSTI$overall_type[invSTI$overall_type == 'kits_sent'] <- 'Orders'
invSTI$overall_type[invSTI$overall_type == 'kits_tested'] <- 'Returns'
table(invSTI$overall_type, invSTI$Dispatched.MonthYear == v1)

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
# rename (new variable name = existing variable name) to have same names in all data frames----
invSTI  <- rename(invSTI, MonthYear = Dispatched.MonthYear)

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
invCOC <- ContCOC[ , c("Dispatched.at.month.year","Months.prescribed","Drug","Region")]
invPOP <- ContPOP[ , c("Dispatched.at.month.year","Months.prescribed","Drug","Region")]
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

# Include bolt-ons
invBolts <- bolts[ , c("Dispatched.MthYr.EC.Now","Region","Any.bolt.ons.")]
# count only prescriptions that have been dispatched
invBolts <- invBolts[(invBolts$Dispatched.MthYr.EC.Now != "" & invBolts$Any.bolt.ons. != ""),]
# create variable 'Description'
table(invBolts$Any.bolt.ons.)

invBolts$Description <- 0
invBolts$Description[invBolts$Any.bolt.ons. == "pop"] <- "POP bolt-on Desogestrel 3 months"
invBolts$Description[invBolts$Any.bolt.ons. == "condoms"] <- "Bolt-on condoms"
invBolts$Description[invBolts$Any.bolt.ons. == "lube"] <- "Bolt-on lube"
invBolts$Description[invBolts$Any.bolt.ons. == "pregnancy test"] <- "Bolt-on pregnancy test"
invBolts$Description[invBolts$Any.bolt.ons. == "sti test"] <- "Bolt-on STI test"
# rename (new variable name = existing variable name) to have same names in all data frames
invBolts <- rename(invBolts, default_la = Region, MonthYear = Dispatched.MthYr.EC.Now)
# drop columns no needed
invBolts$Any.bolt.ons. = NULL

# Count of RPR kits
# kits dispatched that include Syphilis RPR in reporting month = v1----
RPR.Dispatched <- orders[(orders$Dispatched.at.month.year == v1), c("Default.LA","Test.regime","Dispatched.at.month.year") ]
RPR.Dispatched <- RPR.Dispatched[grep('RPR',RPR.Dispatched$Test.regime),]
# Kits returned that include Syphilis RPR
RPR.Returned <- orders[(orders$Lab.results.at.month.year == v1), c("Default.LA","Test.regime","Lab.results.at.month.year") ]
RPR.Returned <- RPR.Returned[grep('RPR',RPR.Returned$Test.regime),]

# add column with description
RPR.Dispatched$Description <- "RPR Syphilis tests dispatched"
RPR.Returned$Description <- "RPR Syphilis tests processed"

# remove columns not needed any more
RPR.Dispatched$Test.regime = NULL
RPR.Returned$Test.regime = NULL

# rename (new variable name = existing variable name) to have same names in all data frames----
RPR.Dispatched <- rename(RPR.Dispatched, default_la = Default.LA, MonthYear = Dispatched.at.month.year)
RPR.Returned <- rename(RPR.Returned, default_la = Default.LA, MonthYear = Lab.results.at.month.year)
# END count of Syphilis RPR----


names(invSTI)
names(invCOC)
names(invPOP)
names(invEC)
names(invInjectable)
names(invPatch)
names(invTreatments)
names(RPR.Dispatched)
names(RPR.Returned)
names(invPDConsult)
names(invPDTreatm)
names(invBolts)


# Stack data sets one on top of the other ----
invoicing <- rbind(invSTI,invTreatments,invCOC,invPOP,invEC,invInjectable,invPatch,invRing,RPR.Dispatched,RPR.Returned,invPDConsult,invPDTreatm,invBolts)

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


# create invoice number column----
# first half of the invoice number is the month
invMonth_1$Invoice <- 'Inv '
# second half of the invoice number is associated to the specific Trust/Region, which we call ContactName in this script
invMonth_1$Number <- ""
invMonth_1$Number[invMonth_1$ContactName == "Essex"] <- "0001"
invMonth_1$Number[invMonth_1$ContactName == "Shropshire"] <- "0002"
invMonth_1$Number[invMonth_1$ContactName == "Telford and Wrekin"] <- "0003"
invMonth_1$Number[invMonth_1$ContactName == "Worcestershire"] <- "0005"
invMonth_1$Number[invMonth_1$ContactName == "London Northwest Healthcare"] <- "0006"
invMonth_1$Number[invMonth_1$ContactName == "County Durham and Darlington NHS Foundation Trust"] <- "0007"
invMonth_1$Number[invMonth_1$ContactName == "Derbyshire Community Health Services NHS Foundation Trust"] <- "0008"
invMonth_1$Number[invMonth_1$ContactName == "South Staffordshire and Shropshire Healthcare NHS Foundation Trust"] <- "0009"
invMonth_1$Number[invMonth_1$ContactName == "Hertfordshire"] <- "00010"
invMonth_1$Number[invMonth_1$ContactName == "Hertfordshire hertstestyourself Council"] <- "00011"
invMonth_1$Number[invMonth_1$ContactName == "Berkshire"] <- "00012"
invMonth_1$Number[invMonth_1$ContactName == "Bradford"] <- "00013"
invMonth_1$Number[invMonth_1$ContactName == "Knowsley"] <- "00014"
invMonth_1$Number[invMonth_1$ContactName == "Darlington"] <- "00015"
invMonth_1$Number[invMonth_1$ContactName == "Leicester City"] <- "00016"
invMonth_1$Number[invMonth_1$ContactName == "Rutland"] <- "00017"
invMonth_1$Number[invMonth_1$ContactName == "Leicestershire"] <- "00018"
invMonth_1$Number[invMonth_1$ContactName == "Derby City"] <- "00019"
invMonth_1$Number[invMonth_1$ContactName == "North Staffordshire"] <- "00020"
invMonth_1$Number[invMonth_1$ContactName == "Gateshead"] <- "00021"
invMonth_1$Number[invMonth_1$ContactName == "Cheshire East"] <- "00022"
invMonth_1$Number[invMonth_1$ContactName == "Halton"] <- "00023"
invMonth_1$Number[invMonth_1$ContactName == "Warrington"] <- "00024"
invMonth_1$Number[invMonth_1$ContactName == "Northern Ireland"] <- "00025"
invMonth_1$Number[invMonth_1$ContactName == "UKHSA"] <- "00026"
invMonth_1$Number[invMonth_1$ContactName == "Cornwall and Isles of Scilly"] <- "00027"
invMonth_1$Number[invMonth_1$ContactName == "Liverpool"] <- "00028"
invMonth_1$Number[invMonth_1$ContactName == "Stoke on Trent"] <- "00029"
invMonth_1$Number[invMonth_1$ContactName == "Sunderland"] <- "00030"
invMonth_1$Number[invMonth_1$ContactName == "Nottingham City Council"] <- "00031"
invMonth_1$Number[invMonth_1$ContactName == "Buckinghamshire"] <- "00032"
invMonth_1$Number[invMonth_1$ContactName == "Dorset"] <- "00033"
invMonth_1$Number[invMonth_1$ContactName == "Kirklees"] <- "00035"
invMonth_1$Number[invMonth_1$ContactName == "South Tyneside"] <- "00036"
invMonth_1$Number[invMonth_1$ContactName == "East Sussex"] <- "00037"
invMonth_1$Number[invMonth_1$ContactName == "UKHSA PrEP Trial"] <- "00038"
invMonth_1$Number[invMonth_1$ContactName == "Bromley"] <- "00039"
invMonth_1$Number[invMonth_1$ContactName == "Blackburn"] <- "00040"
invMonth_1$Number[invMonth_1$ContactName == "Southend"] <- "00041"
invMonth_1$Number[invMonth_1$ContactName == "Teesside"] <- "00042"
invMonth_1$Number[invMonth_1$ContactName == "Thurrock"] <- "00043"
invMonth_1$Number[invMonth_1$ContactName == "Wirral"] <- "0056"
invMonth_1$Number[invMonth_1$ContactName == "Rotherham"] <- "0057"
invMonth_1$Number[invMonth_1$ContactName == "Stockport"] <- "0058"
invMonth_1$Number[invMonth_1$ContactName == "Tameside"] <- "0059"
invMonth_1$Number[invMonth_1$ContactName == "Orbish"] <- "0060"


# create InvoiceNumber as concatenate of 'Invoice' and 'Number'
invMonth_1$InvoiceNumber <- paste(invMonth_1$Invoice, v1, invMonth_1$Number, sep=" ")

# remove columns no needed anymore
invMonth_1$Invoice = NULL
invMonth_1$Number = NULL


# REMOVE AS TARAN INCLUDES THE PO IN EACH INVOICE - DISCUSS WITH BLAKE
#some Trusts have a Purchase Order
# invMonth_1$PO <- ""
# invMonth_1$PO[invMonth_1$ContactName == "Berkshire"] <- "PO 40157114"
# invMonth_1$PO[invMonth_1$ContactName == "County Durham and Darlington NHS Foundation Trust"] <- "PO RXP0003834180"
# invMonth_1$PO[invMonth_1$ContactName == "Bradford"] <- "PO L024942"
# invMonth_1$PO[invMonth_1$ContactName == "Cheshire East"] <- "PO RQ6N400039417"
# invMonth_1$PO[invMonth_1$ContactName == "Gateshead"] <- "PO RLNN400008230"
# invMonth_1$PO[invMonth_1$ContactName == "Halton"] <- "PO RQ6N400039417"
# invMonth_1$PO[invMonth_1$ContactName == "Hertfordshire"] <- "PO 000037852"
# invMonth_1$PO[invMonth_1$ContactName == "Kirklees"] <- "PO L024941"
# invMonth_1$PO[invMonth_1$ContactName == "Knowsley"] <- "PO RQ6N400039417"
# invMonth_1$PO[invMonth_1$ContactName == "Leicester City"] <- "PO SS139020"
# invMonth_1$PO[invMonth_1$ContactName == "Leicestershire"] <- "PO SS139020"
# invMonth_1$PO[invMonth_1$ContactName == "North Staffordshire"] <- "PO SS139114"
# invMonth_1$PO[invMonth_1$ContactName == "Nottingham City Council"] <- "PO NCC7156225"
# invMonth_1$PO[invMonth_1$ContactName == "UKHSA PrEP Trial"] <- "PO 6695926"
# invMonth_1$PO[invMonth_1$ContactName == "Rutland"] <- "PO SS139020"
# invMonth_1$PO[invMonth_1$ContactName == "Shropshire"] <- "PO SS105992"
# invMonth_1$PO[invMonth_1$ContactName == "South Staffordshire and Shropshire Healthcare NHS Foundation Trust"] <- "PO SS124036"
# invMonth_1$PO[invMonth_1$ContactName == "Stoke on Trent"] <- "PO SS139114"
# invMonth_1$PO[invMonth_1$ContactName == "Telford and Wrekin"] <- "PO SS80163"
# invMonth_1$PO[invMonth_1$ContactName == "Warrington"] <- "PO RQ6N400039417"

# create reference. Use 'Region' instead of 'ContactName' as a better reference to our records
invMonth_1$Reference <- paste("SH:24 ",invMonth_1$ContactName, "activity", v2)
# invoice dates----
invMonth_1$InvoiceDate <- v3
invMonth_1$DueDate <- v4

        
# remove dataframe rows based on zero values in one column
invMonth_2 <- invMonth_1[invMonth_1$Quantity != 0, ]
# export to check figures 
write.table (invMonth_2, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2022\\2022_05\\Check_Xero_Quantities_2022.May_v1.csv", row.names=F, sep=",")



#remove Description = blank (those are the returns that came with a lab_error, we don't charge for them)
invMonth_2 <- invMonth_2[invMonth_2$Description != 'blank', ]
#remove Medway invoice
invMonth_2 <- invMonth_2[(invMonth_2$ContactName != 'Medway'), ]


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
              'RPR Syphilis tests dispatched',
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

Fee1DiscountRM <-c( 3.60, 3.04, 4.18, 2.99, 2.34, 3.16,2.72, 3.60, 3.04, 4.18, 2.72,2.72, 3.60, 3.04, 4.18, 2.72,3.60, 3.04, 4.18, 2.72, 3.60, 3.04, 4.18, 2.72, 3.60, 3.04, 4.18, 2.72, 3.60, 3.04, 4.18,
                   46.13,31.60,51.10,29.06,13.71,34.03,9.64,37.60,23.62,42.56,18.17,9.64,37.60,23.62,42.56,9.64,37.60,23.62,42.56,18.17,46.13,31.60,51.10,26.70,54.66,40.13,59.63,35.23,63.19,48.66,68.16,
                   21.11,
                   16.30,16.65,16.65,16.65,16.65,19.61,26.50,26.50,26.50,26.50,26.22,36.35,36.35,36.35,36.35,
                   17.92,14.92,17.61,22.85,20.00,20.37,32.70,25.08,23.13,
                    3.48, 0.52, 0.79, 0.65, 4.25,
                   28.50,15.00,15.00,
                   18.95,64.54,33.77,22.87,18.79,33.77,
                   0.00,12.10,
                   34.36,53.87,73.38,44.55,74.25,103.95,29.75,36.65,43.55)

Fee2Standard <- c( 6.54, 5.52, 7.60, 5.44, 4.25, 5.85, 4.94, 6.54, 5.52, 7.60, 4.94, 4.94, 6.54, 5.52, 7.60, 4.94, 6.54, 5.52, 7.60, 4.94, 6.54, 5.52, 7.60, 4.94, 5.52, 6.54, 7.60, 4.94, 5.52, 6.54, 7.60,
                  66.60,38.48,72.82,37.22,19.16,55.28,16.62,54.19,33.85,61.41,22.82,13.52,54.19,33.85,61.41,16.62,54.19,33.85,61.41,22.82,66.60,38.48,72.82,33.54,44.68,72.80,79.02,39.74,50.88,79.00,85.22,
                  20.05,
                  16.30,16.65,16.65,16.65,16.65,19.61,26.50,26.50,26.50,26.50,26.22,36.35,36.35,36.35,36.35,
                  17.92,14.92,17.61,22.85,20.00,20.37,32.70,25.08,23.13,
                   3.48, 0.52, 0.79, 0.65, 4.25,
                  28.50,15.00,15.00,
                  18.95,64.54,33.77,22.87,18.79,33.77,
                  0.00,12.10,
                  34.36,53.87,73.38,44.55,74.25,103.95,29.75,36.65,43.55)

Fee3Discount <- c( 3.60, 3.04, 4.18, 2.99, 2.34, 3.16,2.72, 3.60, 3.04, 4.18, 2.72,2.72, 3.60, 3.04, 4.18,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
                  45.03,30.50,50.00,27.96,12.61,32.93,8.54,36.50,22.52,41.46,17.07,8.54,36.50,22.52,41.46,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
                  21.11,
                  16.30,16.65,16.65,16.65,16.65,19.61,26.50,26.50,26.50,26.50,26.22,36.35,36.35,36.35,36.35,
                  17.92,14.92,17.61,22.85,20.00,20.37,32.70,25.08,23.13,
                   3.48, 0.52, 0.79, 0.65, 4.25,
                  28.50,15.00,15.00,
                  18.95,64.54,33.77,22.87,18.79,33.77,
                  0.00,12.10,
                  34.36,53.87,73.38,44.55,74.25,103.95,29.75,36.65,43.55)

Fee4DiscountRM5 <- c( 3.42, 2.89, 3.97, 2.84, 2.22, 3.00,2.58, 3.42, 2.89, 3.97, 2.58,2.58, 3.42, 2.89, 3.97,2.72, 3.60, 3.04, 4.18, 2.72, 3.60, 3.04, 4.18, 2.72, 3.60, 3.04, 4.18, 2.72, 3.60, 3.04, 4.18,
                     43.88,30.08,48.60,27.66,13.08,32.38,9.21,35.78,22.49,40.49,17.32,9.21,35.78,22.49,40.49,9.64,37.60,23.62,42.56,18.17,46.13,31.60,51.10,26.70,54.66,40.13,59.63,35.23,63.19,48.66,68.16,
                     20.05,
                     16.30,16.65,16.65,16.65,16.65,19.61,26.50,26.50,26.50,26.50,26.22,36.35,36.35,36.35,36.35,
                     17.92,14.92,17.61,22.85,20.00,20.37,32.70,25.08,23.13,
                      3.48, 0.52, 0.79, 0.65, 4.25,
                     28.50,15.00,15.00,
                     18.95,64.54,33.77,22.87,18.79,33.77,
                     0.00,12.10,
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
                     0.00,0.00,
                     0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)

eSRH <- c( 6.54, 5.52, 7.60, 5.44, 4.25, 5.85, 4.94, 6.54, 5.52, 7.60, 4.94, 4.94, 6.54, 5.52, 7.60,4.94,6.54,5.52,7.60,4.94,6.54,5.52,7.60,4.94,5.52,6.54,7.60,4.94,5.52,6.54,7.60,
          46.45,30.33,49.26,37.07,19.01,46.13,16.47,39.04,33.70,46.26,20.67,13.37,39.04,33.70,46.26,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
          21.11,
          16.30,16.65,16.65,16.65,16.65,19.61,26.50,26.50,26.50,26.50,26.22,36.35,36.35,36.35,36.35,
          17.92,14.92,17.61,22.85,20.00,20.37,32.70,25.08,23.13,
           3.48, 0.52, 0.79, 0.65, 4.25,
          28.50,15.00,15.00,
          18.95,64.54,33.77,22.87,18.79,33.77,
          0.00,12.10,
          34.36,53.87,73.38,44.55,74.25,103.95,29.75,36.65,43.55)

eSRH5 <- c( 6.21, 5.24, 7.22, 5.17, 4.04, 5.56, 4.69, 6.21, 5.24, 7.22, 4.69, 4.69, 6.21, 5.24, 7.22,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
           44.13,28.81,46.80,35.22,18.06,43.82,15.65,37.09,32.02,43.95,19.64,12.70,37.09,32.02,43.95,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
           21.11,
           16.30,16.65,16.65,16.65,16.65,19.61,26.50,26.50,26.50,26.50,26.22,36.35,36.35,36.35,36.35,
           17.92,14.92,17.61,22.85,20.00,20.37,32.70,25.08,23.13,
            3.48, 0.52, 0.79, 0.65, 4.25,
           28.50,15.00,15.00,
           18.95,64.54,33.77,22.87,18.79,33.77,
           0.00,12.10,
           34.36,53.87,73.38,44.55,74.25,103.95,29.75,36.65,43.55)

FeeZero <- c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
            0.00,
            0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
            0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
            0.00,0.00,0.00,0.00,0.00,
            0.00,0.00,0.00,
            0.00,0.00,0.00,0.00,0.00,0.00,
            0.00,0.00,
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
InvFee1 <- invMonth_2 [(invMonth_2$ContactName=="Blackburn" | invMonth_2$ContactName=="Bromley"
                        | invMonth_2$ContactName=="Cheshire East" | invMonth_2$ContactName=="Cornwall and Isles of Scilly"
                        | invMonth_2$ContactName=="County Durham and Darlington NHS Foundation Trust" | invMonth_2$ContactName=="Darlington"
                        | invMonth_2$ContactName=="Derby City" | invMonth_2$ContactName=="Derbyshire Community Health Services NHS Foundation Trust"
                        | invMonth_2$ContactName=="Dorset" | invMonth_2$ContactName=="Halton"
                        | invMonth_2$ContactName=="Knowsley" | invMonth_2$ContactName=="Liverpool"
                        | invMonth_2$ContactName=="UKHSA" | invMonth_2$ContactName=="Southend"  
                        | invMonth_2$ContactName=="Warrington" | invMonth_2$ContactName=="East Sussex" | invMonth_2$ContactName=="Teesside"  | invMonth_2$ContactName=="Orbish"
                        | invMonth_2$ContactName=="Hertfordshire" 
                        | invMonth_2$ContactName=="Bury" | invMonth_2$ContactName=="Rochdale" | invMonth_2$ContactName=="Oldham"),]

InvFee2 <- invMonth_2 [(invMonth_2$ContactName=="Gateshead" | invMonth_2$ContactName=="Glasgow" 
                        | invMonth_2$ContactName=="London Northwest Healthcare" | invMonth_2$ContactName=="South Tyneside" 
                        | invMonth_2$ContactName=="Sunderland" | invMonth_2$ContactName=="Worcestershire"
                        ) , ]

InvFee3 <- invMonth_2 [(invMonth_2$ContactName=="Berkshire"),]

InvFee4 <- invMonth_2 [(invMonth_2$ContactName=="Leicester City" | invMonth_2$ContactName=="Leicestershire"
                        | invMonth_2$ContactName=="North Staffordshire" | invMonth_2$ContactName=="Rutland"
                        | invMonth_2$ContactName=="Shropshire" | invMonth_2$ContactName=="South Staffordshire and Shropshire Healthcare NHS Foundation Trust"
                        | invMonth_2$ContactName=="Stoke on Trent" | invMonth_2$ContactName=="Telford and Wrekin") ,]

# freetesting done separately
InvFee5 <- invMonth_2 [(invMonth_2$ContactName=="Freetesting"),] 

InvFee6 <- invMonth_2 [(invMonth_2$ContactName=="Buckinghamshire" | invMonth_2$ContactName=="Northern Ireland"
                        | invMonth_2$ContactName=="Rotherham" | invMonth_2$ContactName=="Stockport" | invMonth_2$ContactName=="Tameside"),]

InvFee7 <- invMonth_2 [(invMonth_2$ContactName=="Essex" | invMonth_2$ContactName=="Thurrock" | invMonth_2$ContactName=="Wirral"
                        | invMonth_2$ContactName=="Bradford" | invMonth_2$ContactName=="Kirklees" 
                          | invMonth_2$ContactName=="Nottingham City Council"),]

# some areas not invoiced. Count them anyway to check whole picture
InvFeeZero <- invMonth_2 [(invMonth_2$ContactName=="Ireland") | (invMonth_2$ContactName=="Romania") | (invMonth_2$ContactName=="Fettle")
                        | (invMonth_2$ContactName == "Medway"),]

# check that sum of the invoices grouped equals total of invoicing in file invMonth_2:
nrow(InvFee1)
nrow(InvFee2)
nrow(InvFee3)
nrow(InvFee4)
nrow(InvFee5)
nrow(InvFee6)
nrow(InvFee7)
nrow(InvFeeZero)

# merge each price data set with its correspondent areas
InvoicesFee1 = merge(x = InvFee1, y = fee1, by = "Description", all.x = TRUE)
InvoicesFee2 = merge(x = InvFee2, y = fee2, by = "Description", all.x = TRUE)
InvoicesFee3 = merge(x = InvFee3, y = fee3, by = "Description", all.x = TRUE)
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
InvoicesFee3 <- rename(InvoicesFee3, UnitAmount = Fee3Discount)
InvoicesFee4 <- rename(InvoicesFee4, UnitAmount = Fee4DiscountRM5)
# freetesting done separately
#InvoicesFee5 <- rename(InvoicesFee5, UnitAmount = Fee5Freetesting)
InvoicesFee6 <- rename(InvoicesFee6, UnitAmount = eSRH)
InvoicesFee7 <- rename(InvoicesFee7, UnitAmount = eSRH5)
InvoicesFeeZero <- rename(InvoicesFeeZero, UnitAmount = FeeZero)

# Stack data sets one on top of the other 
InvoicesStack <- rbind(InvoicesFee1, InvoicesFee2, InvoicesFee3, InvoicesFee4, InvoicesFee6, InvoicesFee7, InvoicesFeeZero)  


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
InvoicesStack$TaxType <- "No VAT"
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

write.table (InvoicesStack_Ordered, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2022\\2022_05\\20220613_Xero_May_test.csv", row.names=F, sep=",")
