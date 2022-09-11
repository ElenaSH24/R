# producing Xero quarterly file for freetesting activity
# read generic invoicing file (same we use for SH:24 invoicing)
invSTI = read.csv("20220905_invoicing.csv")
invoicing <- invSTI

# extract freetesting activity, and the variables needed for invoicing
invFre <- invoicing[grep('Freetesting -', invoicing$default_la)
                     ,c("overall_type","default_la","processed_at","invoice_category_billable")]


# convert character to date, first set the format the date is shown 
invFre$processed_at <- as.Date(invFre$processed_at,"%Y-%m-%d")
# extract month from day date
invFre$Dispatched.MonthYear <- format(as.Date(invFre$processed_at),"%Y-%m")
# extract data for the relevant quarter
invFreet <- invFre[(invFre$Dispatched.MonthYear == "2022-04" | invFre$Dispatched.MonthYear == "2022-05" | invFre$Dispatched.MonthYear == "2022-06"),]


# assign values to 'overall_type' that align with invoicing
invFreet$overall_type[invFreet$overall_type == 'kits_sent'] <- 'Orders'
invFreet$overall_type[invFreet$overall_type == 'kits_tested'] <- 'Returns'
table(invFreet$overall_type,invFreet$Dispatched.MonthYear)

# concatenate values of both variables (type and category) to create the invoicing Description
invFreet$Description <- paste(invFreet$overall_type, invFreet$invoice_category_billable, sep=" - ")

table(invFreet$Description, invFreet$overall_type=='Returns')
# some returns are blank, showing in data.frame as 'Returns -'
# we don't invoice those 'blank' - only a few per month that haven't been processed by lab
# they relate to categories not interpreted by the mapping table in the DB
invFreet <- invFreet[(invFreet$Description != "Returns - "),]

# freetesting scheme is designed for 'HIV' and 'HIV & Syphilis'
# Allocate any 'Syphilis' order or return to 'HIV' (charge for 1 blood). Again, only a few per month - DISCUSS WITH TEAM
invFreet$Description[invFreet$Description == "Orders - Syphilis"] <- "Orders - HIV"
invFreet$Description[invFreet$Description == "Returns - Syphilis"] <- "Returns - HIV"

# remove variables not needed anymore
invFreet$processed_at = NULL
invFreet$overall_type = NULL
invFreet$invoice_category_billable = NULL

table(invFreet$Description)

invFreet_1 <- invFreet

##### create matrix or group by - install dplyr

invFreet_1 %>%
  dplyr::group_by(default_la,Description)%>%
  dplyr::summarise(frequency()) # <-- dplyr

names(invFreet)

d %>%
  dplyr::group_by(A,B)%>%
  dplyr::summarise(UNIQUE_COUNT = n_distinct(C)) # <-- dplyr









# Create ContactName, to name regions as per invoicing requirements
invFreet$ContactName <- invFreet$default_la
invFreet$ContactName[invFreet$default_la == "Freetesting - Bedford"] <- "Bedford Borough Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Blackburn with Darwen"] <- "Lancashire and South Cumbria NHS Foundation Trust"
invFreet$ContactName[invFreet$default_la == "Freetesting - Blackpool"] <- "Blackpool Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Bolton"] <- "Bolton Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Brighton and Hove"] <- "Brighton & Hove City Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Bury"] <- "Bury Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Camden"] <- "London Borough of Islington"
invFreet$ContactName[invFreet$default_la == "Freetesting - Central Bedfordshire"] <- "Bedford Borough Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - County Durham"] <- "County Durham"
invFreet$ContactName[invFreet$default_la == "Freetesting - Coventry"] <- "Coventry City Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Darlington"] <- "Darlington Borough Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Doncaster"] <- "Doncaster Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Gateshead"] <- "Gateshead Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Hampshire"] <- "Hampshire County Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Hartlepool"] <- "Hartlepool Borough Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Hertfordshire"] <- "Hertfordshire County Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Islington"] <- "London Borough of Islington"
invFreet$ContactName[invFreet$default_la == "Freetesting - Kent"] <- "Kent County Council"
invFreet$ContactName[invFreet$default_la == "The Royal Borough of Kingston Upon Thames"] <- "Kingston upon Thames"
invFreet$ContactName[invFreet$default_la == "Freetesting - Lancashire"] <- "The Royal Borough of Kingston Upon Thames"
invFreet$ContactName[invFreet$default_la == "Freetesting - Leeds"] <- "Leeds"
invFreet$ContactName[invFreet$default_la == "Freetesting - Leicester"] <- "Leicester City Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Leicestershire"] <- "Leicestershire County Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Manchester"] <- "Manchester City Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Middlesbrough"] <- "Middlesbrough Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Milton Keynes"] <- "Milton Keynes"
invFreet$ContactName[invFreet$default_la == "Freetesting - Newcastle upon Tyne"] <- "Newcastle City Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Norfolk"] <- "Norfolk County Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - North Tyneside"] <- "North Tyneside Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Northumberland"] <- "Northumbria Healthcare Foundation Trust"
invFreet$ContactName[invFreet$default_la == "Freetesting - Nottingham"] <- "Nottingham City Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Nottinghamshire"] <- "Nottinghamshire"
invFreet$ContactName[invFreet$default_la == "Freetesting - Oldham"] <- "Oldham Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Oxfordshire"] <- "Oxfordshire County Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Portsmouth"] <- "Portsmouth City Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Redbridge"] <- "London Borough of Redbridge"
invFreet$ContactName[invFreet$default_la == "Freetesting - Redcar and Cleveland"] <- "Redcar and Cleveland Borough Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Richmond upon Thames"] <- "London Borough of Richmond-Upon-Thames"
invFreet$ContactName[invFreet$default_la == "Freetesting - Rochdale"] <- "Rochdale Borough Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Rotherham"] <- "Rotherham"
invFreet$ContactName[invFreet$default_la == "Freetesting - Salford"] <- "Salford City Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - South Tyneside"] <- "South Tyneside Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Southampton"] <- "Southampton City Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Stockport"] <- "Stockport Metropolitan Borough Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Stockton-on-Tees"] <- "Stockton-on-Tees Borough Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Suffolk"] <- "Suffolk County Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Sunderland"] <- "Sunderland City Counc"
invFreet$ContactName[invFreet$default_la == "Freetesting - Swindon"] <- "Swindon Borough Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Tameside"] <- "Tameside Metropolitan Borough Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Telford and Wrekin"] <- "Telford & Wrekin Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Trafford"] <- "Trafford Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Wakefield"] <- "Wakefield Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Wandsworth"] <- "Wandsworth"
invFreet$ContactName[invFreet$default_la == "Freetesting - Warwickshire"] <- "Warwickshire County Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - West Sussex"] <- "West Sussex County Council"
invFreet$ContactName[invFreet$default_la == "Freetesting - Wigan"] <- "Wigan Council"

# Create EmailAddress
invFreet$EmailAddress <- ""
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Bedford"] <- "invoices@bedford.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Blackburn with Darwen"] <- "LCT-FAP@LANCASHIRECARE.NHS.UK"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Blackpool"] <- "Deborah.Willetts@blackpool.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Bolton"] <- "lee.houghton2@bolton.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Brighton and Hove"] <- "Publichealth@brighton-hov.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Bury"] <- "SDUinvoices@bury.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Camden"] <- "Nancy.Padwick@islington.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Central Bedfordshire"] <- "invoices@bedford.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - County Durham"] <- "creditors@durham.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Coventry"] <- "PublicHealthandInsightBusiness@coventry.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Darlington"] <- "ken.ross@darlington.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Doncaster"] <- ""
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Gateshead"] <- "davidbrady@gateshead.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Hampshire"] <- "VIMENQUIRES@HANTS.GOV.UK"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Hartlepool"] <- "supplier.invoices@hartlepool.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Hertfordshire"] <- "Hertfordshire.Invoices@proactiscapture.com"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Islington"] <- "Nancy.Padwick@islington.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Kent"] <- "PHfinance@kent.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "The Royal Borough of Kingston Upon Thames"] <- ""
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Lancashire"] <- "contractmgmt.care@lancashire.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Leeds"] <- "Hannah.Sowerbutts@leeds.gov.uk "
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Leicester"] <- "Meenaxi.Pattni@leicester.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Leicestershire"] <- "Josh.Gamble@leics.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Manchester"] <- "R.Taylor2@manchester.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Middlesbrough"] <- ""
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Milton Keynes"] <- "mkc.invoices@cambridgeshire.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Newcastle upon Tyne"] <- ""
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Norfolk"] <- "invoices@norfolk.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - North Tyneside"] <- ""
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Northumberland"] <- ""
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Nottingham"] <- "NCCinvoices@emss.org.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Nottinghamshire"] <- "Nottinghamshire"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Oldham"] <- "julie.burgess@oldham.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Oxfordshire"] <- "VIMEnquiries@hants.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Portsmouth"] <- "PHContracts@portsmouthcc.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Redbridge"] <- ""
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Redcar and Cleveland"] <- "redcaraccountspayable@redcar-cleveland.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Richmond upon Thames"] <- "Lea.siba@richmondandwandsworth.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Rochdale"] <- "invoices@rochdale.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Rotherham"] <- ""
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Salford"] <- "helen.dugdale@salford.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - South Tyneside"] <- "Lisa.Longstaff@southtyneside.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Southampton"] <- "Invoicesonly@southampton.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Stockport"] <- ""
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Stockton-on-Tees"] <- ""
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Suffolk"] <- "iproc@suffolk.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Sunderland"] <- "apinvoices@sunderland.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Swindon"] <- "publichealth@swindon.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Tameside"] <- "supplierinvoice@tameside.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Telford and Wrekin"] <- "Elliot.Bromley@telford.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Trafford"] <- "accounts.payable@trafford.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Wakefield"] <- ""
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Wandsworth"] <- ""
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Warwickshire"] <- "Ettymartin@warwickshire.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - West Sussex"] <- "Paul.Woodcock@westsussex.gov.uk"
invFreet$EmailAddress[invFreet$default_la == "Freetesting - Wigan"] <- "P.Jamieson@wigan.gov.uk"

# Create POAddressLine1
invFreet$POAddressLine1 <- ""

# Create POPostalCode
invFreet$POPostalCode <- ""

# Define quarter to be invoiced 
v2 <- 'InvQ2.2022'
invFreet$InvoiceNumber <- ""
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Bedford"] <- paste(v2,"000001", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Blackburn with Darwen"] <- paste(v2,"000002", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Blackpool"] <- paste(v2,"000003", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Bolton"] <- paste(v2,"000004", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Brighton and Hove"] <- paste(v2,"000005", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Bury"] <- paste(v2,"000006", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Camden"] <- paste(v2,"000007", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Central Bedfordshire"] <- paste(v2,"000008", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - County Durham"] <- paste(v2,"000009", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Coventry"] <- paste(v2,"000010", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Darlington"] <- paste(v2,"000011", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Doncaster"] <- paste(v2,"000011", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Gateshead"] <- paste(v2,"000012", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Hampshire"] <- paste(v2,"000013", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Hartlepool"] <- paste(v2,"000014", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Hertfordshire"] <- paste(v2,"000015", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Islington"] <- paste(v2,"000016", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Kent"] <- paste(v2,"000017", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "The Royal Borough of Kingston Upon Thames"] <- paste(v2,"000018", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Lancashire"] <- paste(v2,"000019", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Leeds"] <- paste(v2,"000020", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Leicester"] <- paste(v2,"000021", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Leicestershire"] <- paste(v2,"000022", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Manchester"] <- paste(v2,"000023", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Middlesbrough"] <- paste(v2,"000024", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Milton Keynes"] <- paste(v2,"000025", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Newcastle upon Tyne"] <- paste(v2,"000026", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Norfolk"] <- paste(v2,"000027", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - North Tyneside"] <- paste(v2,"000028", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Northumberland"] <- paste(v2,"000029", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Nottingham"] <- paste(v2,"000030", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Nottinghamshire"] <- paste(v2,"??????", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Oldham"] <- paste(v2,"000032", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Oxfordshire"] <- paste(v2,"000033", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Portsmouth"] <- paste(v2,"000034", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Redbridge"] <- paste(v2,"000035", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Redcar and Cleveland"] <- paste(v2,"000036", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Richmond upon Thames"] <- paste(v2,"000037", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Rochdale"] <- paste(v2,"000038", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Rotherham"] <- paste(v2,"000039", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Salford"] <- paste(v2,"000040", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - South Tyneside"] <- paste(v2,"000041", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Southampton"] <- paste(v2,"000042", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Stockport"] <- paste(v2,"000043", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Stockton-on-Tees"] <- paste(v2,"000044", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Suffolk"] <- paste(v2,"000045", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Sunderland"] <- paste(v2,"000046", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Swindon"] <- paste(v2,"000047", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Tameside"] <- paste(v2,"000048", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Telford and Wrekin"] <- paste(v2,"000049", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Trafford"] <- paste(v2,"000050", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Wakefield"] <- paste(v2,"000051", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Wandsworth"] <- paste(v2,"000052", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Warwickshire"] <- paste(v2,"000053", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - West Sussex"] <- paste(v2,"000054", sep="-")
invFreet$InvoiceNumber[invFreet$default_la == "Freetesting - Wigan"] <- paste(v2,"000055", sep="-")

# Create Reference
invFreet$Reference <- ""
v3 <- '01.04.2022-30.06.2022'
invFreet$Reference[invFreet$default_la == "Freetesting - Bedford"] <- paste("SH24 Freetesting","Bedford",v3,"PO: 5160031", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Blackburn with Darwen"] <- paste("SH24 Freetesting","Blackburn with Darwen",v3,"PO: P049854", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Blackpool"] <- paste("SH24 Freetesting","Blackpool",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Bolton"] <- paste("SH24 Freetesting","Bolton",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Brighton and Hove"] <- paste("SH24 Freetesting","Brighton and Hove",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Bury"] <- paste("SH24 Freetesting","Bury",v3,"PO: 30482707", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Camden"] <- paste("SH24 Freetesting","Camden",v3,"PO: 10607806", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Central Bedfordshire"] <- paste("SH24 Freetesting","Central Bedfordshire",v3,"PO: 5160031", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - County Durham"] <- paste("SH24 Freetesting","County Durham",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Coventry"] <- paste("SH24 Freetesting","Coventry",v3,"PO: 8211080", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Darlington"] <- paste("SH24 Freetesting","Darlington",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Doncaster"] <- paste("SH24 Freetesting","Doncaster",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Gateshead"] <- paste("SH24 Freetesting","Gateshead",v3,"PO: 260146998", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Hampshire"] <- paste("SH24 Freetesting","Hampshire",v3,"PO: 9004519115", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Hartlepool"] <- paste("SH24 Freetesting","Hartlepool",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Hertfordshire"] <- paste("SH24 Freetesting","Hertfordshire",v3,"PO: 3500335967", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Islington"] <- paste("SH24 Freetesting","Islington",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Kent"] <- paste("SH24 Freetesting","Kent",v3,"PO: 1319015", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Kingston Upon Thames"] <- paste("SH24 Freetesting","Kingston Upon Thames",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Lancashire"] <- paste("SH24 Freetesting","Lancashire",v3,"Ref: JR/PH/LCC/20/1064", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Leeds"] <- paste("SH24 Freetesting","Leeds",v3,"PO: Z221143", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Leicester"] <- paste("SH24 Freetesting","Leicester",v3,"PO: 50443057", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Leicestershire"] <- paste("SH24 Freetesting","Leicestershire",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Manchester"] <- paste("SH24 Freetesting","Manchester",v3,"PO: 4100973256", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Middlesbrough"] <- paste("SH24 Freetesting","Middlesbrough",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Milton Keynes"] <- paste("SH24 Freetesting","Milton Keynes",v3,"PO: 5160021", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Newcastle upon Tyne"] <- paste("SH24 Freetesting","Newcastle upon Tyne",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Norfolk"] <- paste("SH24 Freetesting","Norfolk",v3,"PO: 655508", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - North Tyneside"] <- paste("SH24 Freetesting","North Tyneside",v3,"PO: 1153347", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Northumberland"] <- paste("SH24 Freetesting","Northumberland",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Nottingham"] <- paste("SH24 Freetesting","Nottingham",v3,"PO: 7151052", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Nottinghamshire"] <- paste("SH24 Freetesting","Nottinghamshire",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Oldham"] <- paste("SH24 Freetesting","Oldham",v3,"PO: OL20400740", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Oxfordshire"] <- paste("SH24 Freetesting","Oxfordshire",v3,"PO: 9004150478", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Portsmouth"] <- paste("SH24 Freetesting","Portsmouth",v3,"PO: 295 0729", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Redbridge"] <- paste("SH24 Freetesting","Redbridge",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Redcar and Cleveland"] <- paste("SH24 Freetesting","Redcar and Cleveland",v3,"PO: 8112095", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Richmond upon Thames"] <- paste("SH24 Freetesting","Richmond upon Thames",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Rochdale"] <- paste("SH24 Freetesting","Rochdale",v3,"PO: PH104120", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Rotherham"] <- paste("SH24 Freetesting","Rotherham",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Salford"] <- paste("SH24 Freetesting","Salford",v3,"PO: 4500364535", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - South Tyneside"] <- paste("SH24 Freetesting","South Tyneside",v3,"PO: 20299805", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Southampton"] <- paste("SH24 Freetesting","Southampton",v3,"PO: 20011796", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Stockport"] <- paste("SH24 Freetesting","Stockport",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Stockton-on-Tees"] <- paste("SH24 Freetesting","Stockton-on-Tees",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Suffolk"] <- paste("SH24 Freetesting","Suffolk",v3,"PO: 866559", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Sunderland"] <- paste("SH24 Freetesting","Sunderland",v3,"PO: 4900364803", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Swindon"] <- paste("SH24 Freetesting","Swindon",v3,"Ref: PH2019", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Tameside"] <- paste("SH24 Freetesting","Tameside",v3,"PO: 40100831", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Telford and Wrekin"] <- paste("SH24 Freetesting","Telford and Wrekin",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Trafford"] <- paste("SH24 Freetesting","Trafford",v3,"PO: 7500242001", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Wakefield"] <- paste("SH24 Freetesting","Wakefield",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Wandsworth"] <- paste("SH24 Freetesting","Wandsworth",v3,"", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Warwickshire"] <- paste("SH24 Freetesting","Warwickshire",v3,"PO: 13586286", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - West Sussex"] <- paste("SH24 Freetesting","West Sussex",v3,"PO: 4100168727", sep=" - ")
invFreet$Reference[invFreet$default_la == "Freetesting - Wigan"] <- paste("SH24 Freetesting","Wigan",v3,"", sep=" - ")

names(invFreet)

write.table (invFreet, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_08/2022.09.08_invoic_freetesting.csv", row.names=F, sep=",")






############# Continue from here 08.09.2022

invMonth <- invoicing[(invoicing$Processed_Month == x),c("ContactName","Processed_Month",'Description')]

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
invMonth_1$Number[invMonth_1$ContactName == "Wirral"] <- "0056"


# create Reference as concatenate of 'Invoice' and 'Number'
invMonth_1$Reference <- paste(invMonth_1$Invoice,x,invMonth_1$Number, sep=" ")

# some Trusts have a Purchase Order
invMonth_1$PO <- ""
invMonth_1$PO[invMonth_1$ContactName == "Berkshire"] <- "PO 40157114"
invMonth_1$PO[invMonth_1$ContactName == "Warrington"] <- "PO RQ6N400039417"

# create reference. Use 'Region' instead of 'ContactName' as a better reference to our records
invMonth_1$Reference <- paste("SH:24 ",invMonth_1$ContactName, " activity 01.02.2022-28.02.2022", invMonth_1$PO)
# invoice dates----
invMonth_1$InvoiceDate <- "28/02/2022"
invMonth_1$DueDate <- "31/03/2022"

        
# remove dataframe rows based on zero values in one column
invMonth_2 <- invMonth_1[invMonth_1$Quantity != 0, ]


# create price data frames
Description <- c('Orders - HIV',
                'Orders - HIV & Syphilis',
                'Orders - Syphilis',
                'Returns - HIV',
                'Returns - HIV & Syphilis',
                'Returns - Syphilis')


Fee5Freetesting

# create data frame with prices per invoicing category
fee1 <- data.frame(Description, Fee1DiscountRM)


# break down the main invoicing data set as per the fee contracted in each area 
InvFee1 <- invMonth_2 [(invMonth_2$ContactName=="Blackburn" | invMonth_2$ContactName=="Bradford" | invMonth_2$ContactName=="Bromley"
                        | invMonth_2$ContactName=="Cheshire East" | invMonth_2$ContactName=="Cornwall and Isles of Scilly"
                        | invMonth_2$ContactName=="County Durham and Darlington NHS Foundation Trust" | invMonth_2$ContactName=="Darlington"
                        | invMonth_2$ContactName=="Derby City" | invMonth_2$ContactName=="Derbyshire Community Health Services NHS Foundation Trust"
                        | invMonth_2$ContactName=="Dorset" | invMonth_2$ContactName=="Halton"
                        | invMonth_2$ContactName=="Kirklees" | invMonth_2$ContactName=="Knowsley" | invMonth_2$ContactName=="Liverpool"),]



# merge each price data set with its correspondent areas
InvoicesFee1 = merge(x = InvFee1, y = fee1, by = "Description", all.x = TRUE)


# rename variables in the data frames that include prices. Variables have to be called the same to be able to stack data frames
#rename(new variable name = existing variable name)
InvoicesFee1 <- rename(InvoicesFee1, Unit.Price = Fee1DiscountRM)

# Stack data sets one on top of the other 
InvoicesStack <- rbind(InvoicesFee1, InvoicesFee2, InvoicesFee3)  


# select and order the columns we need
InvoicesStack <- InvoicesStack [c("ContactName","Reference","Reference","InvoiceDate","DueDate","Description","Quantity","Unit.Price")]
# order data first by Area (=ContactName) and second by the invoicing category (=Description)
InvoicesStack <- InvoicesStack[order(InvoicesStack$ContactName,InvoicesStack$Description),]

# create blank variables
InvoicesStack$Total <- ""

