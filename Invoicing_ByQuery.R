#producing Xero file


invoicing = read.csv("20220303_Invoicing_AprToFeb.csv")

names(invoicing)
table(invoicing$Region)

invoicing$Region <- invoicing$default_la
invoicing$Region[grepl("Freetesting -", invoicing$default_la)] <- "Freetesting"
invoicing$Region[grepl("Ireland -", invoicing$default_la)] <- "Ireland"
invoicing$Region[grepl("Northern Ireland ", invoicing$default_la)] <- "Northern Ireland"
invoicing$Region[grepl("PrEP Trial -", invoicing$default_la)] <- "PrEP Trial"

class(invoicing$order_created_at)
class(invoicing$processed_at)

# convert character to date, first set the format the date is shown 
invoicing$order_created_at <- as.Date(invoicing$order_created_at,"%Y-%m-%d")
invoicing$processed_at <- as.Date(invoicing$processed_at,"%Y-%m-%d")

# extract month from day date
invoicing$Created_Month <- format(as.Date(invoicing$order_created_at),"%Y-%m")
invoicing$Processed_Month <- format(as.Date(invoicing$processed_at),"%Y-%m")

table(invoicing$Processed_Month, invoicing$overall_type=='kits_sent')
table(invoicing$Processed_Month, invoicing$overall_type=='kits_tested')



