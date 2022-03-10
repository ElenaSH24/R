#producing Xero file


invoicing = read.csv("20220303_Invoicing_AprToFeb.csv")

invoicing$Region <- invoicing$default_la
names(invoicing)
table(invoicing$Region)

invoicing$Region[grepl("Freetesting -", invoicing$default_la)] <- "Freetesting"
invoicing$Region[grepl("Ireland -", invoicing$default_la)] <- "Ireland"
invoicing$Region[grepl("Northern Ireland ", invoicing$default_la)] <- "Northern Ireland"
invoicing$Region[grepl("PrEP Trial -", invoicing$default_la)] <- "PrEP Trial"





orders1$Dispatched_day <- as.Date(orders1$dispatched_at, "%Y-%m-%d")
orders1$DispatchedMonthYr <- format(as.Date(orders1$dispatched_at), "%Y-%m")



