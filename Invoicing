# working directory: Masters for sharing
# read files from 'Backing.Data' Tab, OrdersMonth from Orders_Total (inc 'Area' as per Saskia code)
#install packages: dplyr, data.table (for function setDT)

OrdersSitesTested <- OrdersMonth
#Generate intermediate variable 1 as replica of Sites.Tested, and then asign values showing kind of test allocated to 'Sites Tested' for invoicing.
#Generate intermediate variable 2 as replica of Sites.Tested, and then asign values showing kind of test with detail.
OrdersSitesTested$Sites.tested1 <- OrdersSitesTested$Sites.tested

# Replace blank with a value (zmissing) in the variable 'Sites.tested1' with function 'levels'----
# LEVELS is for categorical values
#levels(OrdersSitesTested$Sites.tested1)[levels(OrdersSitesTested$Sites.tested1) == ""] <- "zmissing"


# If the Sites.tested attribute is not a factor, use this square bracket notation.
OrdersSitesTested$Sites.tested1[OrdersSitesTested$Sites.tested1 == ""] <- 'zmissing'

# see how many orders don't have a Sites.tested category
table(OrdersSitesTested$Sites.tested1, useNA = "always")

# Convert N/A into character, to be able to substitute later (otherwise Warning message: invalid factor level, NA generated)
OrdersSitesTested$Sites.tested1 <- sapply(OrdersSitesTested$Sites.tested1, as.character)


# Categorise all the Sites.Tested1 = <blank> within the categories used for invoicing----
# WE DON'T DO THIS FROM 13.APR.2021:Categorise all tests with 'Hepatitis' as "T6/TT: All STIs MSM" for easy, using 'grepl'
# OrdersSitesTested$Sites.tested1 <- ifelse(grepl("Hepatitis", OrdersSitesTested$Test.regime), "T6/TT: All STIs MSM", OrdersSitesTested$Sites.tested1)

# categorise tests with 'Hepatitis'
OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" &                            
      (OrdersSitesTested$Test.regime=="HepatitisB::Blood" 
      | OrdersSitesTested$Test.regime=="HepatitisC::Blood")
      ] <- "zHep: 1 blood Hep B or Hep C"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" &                            
      (OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisB::Blood")
      ] <- "z Hep: 1 blood + CT/GC triple" 

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" &                            
       (OrdersSitesTested$Test.regime=="HepatitisB::Blood,HepatitisC::Blood" 
       | OrdersSitesTested$Test.regime=="HepatitisB::Blood,Hiv::Blood"
       | OrdersSitesTested$Test.regime=="HepatitisB::Blood,Syphilis::Treponemal"
       | OrdersSitesTested$Test.regime=="HepatitisC::Blood,Hiv::Blood")
       ] <- "zHep: 2 bloods (Hep B & Hep C, Hep & HIV/Syphilis)"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" &                            
       (OrdersSitesTested$Test.regime=="Chlamydia::Vaginal,Gonorrhoea::Vaginal,HepatitisB::Blood,HepatitisC::Blood"
       | OrdersSitesTested$Test.regime=="Chlamydia::Oral,Gonorrhoea::Oral,HepatitisB::Blood,HepatitisC::Blood"
       | OrdersSitesTested$Test.regime=="Chlamydia::Oral,Gonorrhoea::Oral,HepatitisC::Blood,Hiv::Blood")
       ] <- "zHep: 2 bloods + CT/GC single"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" &                            
       (OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisC::Blood,Syphilis::RPR"
       | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisC::Blood,Syphilis::Treponemal"
       | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisB::Blood,Syphilis::RPR"
       | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisB::Blood,HepatitisC::Blood"
       )
       ] <- "zHep: 2 bloods + CT/GC triple"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" &                            
       (OrdersSitesTested$Test.regime=="HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal"
       | OrdersSitesTested$Test.regime=="HepatitisB::Blood,HepatitisC::Blood,Hiv::Blood"
       | OrdersSitesTested$Test.regime=="HepatitisB::Blood,HepatitisC::Blood,Syphilis::Treponemal")
       ] <- "zHep: 3 bloods"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" &                            
      (OrdersSitesTested$Test.regime=="Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisB::Blood,HepatitisC::Blood,Syphilis::Treponemal"
      | OrdersSitesTested$Test.regime=="Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal"
      | OrdersSitesTested$Test.regime=="Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisB::Blood,Hiv::Blood,Syphilis::Treponemal")
      ] <- "zHep: 3 bloods + CT/GC dual"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" &                            
       (OrdersSitesTested$Test.regime=="Chlamydia::Vaginal,Gonorrhoea::Vaginal,HepatitisB::Blood,HepatitisC::Blood,Hiv::Blood"
       | OrdersSitesTested$Test.regime=="Chlamydia::Vaginal,Gonorrhoea::Vaginal,HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal"
       | OrdersSitesTested$Test.regime=="Chlamydia::Urine,Gonorrhoea::Urine,HepatitisB::Blood,Hiv::Blood,Syphilis::Treponemal"
       | OrdersSitesTested$Test.regime=="Chlamydia::Urine,Gonorrhoea::Urine,HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal"
       | OrdersSitesTested$Test.regime=="Chlamydia::Urine,Gonorrhoea::Urine,HepatitisB::Blood,HepatitisC::Blood,Hiv::Blood"
       | OrdersSitesTested$Test.regime=="Chlamydia::Urine,Gonorrhoea::Urine,HepatitisB::Blood,HepatitisC::Blood,Syphilis::Treponemal")
       ] <- "zHep: 3 bloods + CT/GC single"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" & 
       (OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisB::Blood,Hiv::Blood,Syphilis::Treponemal" 
       | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisB::Blood,Hiv::Blood,Syphilis::RPR"
       | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Vaginal,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Vaginal,HepatitisB::Blood,Hiv::Blood,Syphilis::Treponemal"
       | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Vaginal,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Vaginal,HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal"
       | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal"
       | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisC::Blood,Hiv::Blood,Syphilis::RPR"
       | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisB::Blood,HepatitisC::Blood,Syphilis::Treponemal"
       | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisB::Blood,HepatitisC::Blood,Syphilis::RPR")
       ] <- "zHep: 3 bloods + CT/GC triple"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" &                            
       (OrdersSitesTested$Test.regime=="HepatitisB::Blood,HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal")
       ] <- "zHep: 4 bloods"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" &                            
      (OrdersSitesTested$Test.regime=="Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisB::Blood,HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal" 
      | OrdersSitesTested$Test.regime=="Chlamydia::Oral,Chlamydia::Vaginal,Gonorrhoea::Oral,Gonorrhoea::Vaginal,HepatitisB::Blood,HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal")
      ] <- "zHep: 4 bloods + CT/GC dual"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" &                            
      (OrdersSitesTested$Test.regime=="Chlamydia::Urine,Gonorrhoea::Urine,HepatitisB::Blood,HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal" 
      | OrdersSitesTested$Test.regime=="Chlamydia::Vaginal,Gonorrhoea::Vaginal,HepatitisB::Blood,HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal"
      | OrdersSitesTested$Test.regime=="Chlamydia::Oral,Gonorrhoea::Oral,HepatitisB::Blood,HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal")
      ] <- "zHep: 4 bloods + CT/GC single"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" &                            
      (OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisB::Blood,HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal" 
      | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,HepatitisB::Blood,HepatitisC::Blood,Hiv::Blood,Syphilis::RPR"
      | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Vaginal,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Vaginal,HepatitisB::Blood,HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal")
      | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Chlamydia::Vaginal,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,Gonorrhoea::Vaginal,HepatitisB::Blood,HepatitisC::Blood,Hiv::Blood,Syphilis::Treponemal"
      ] <- "zHep: 4 bloods + CT/GC triple"

# and then categorise the rest of N/A
OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=='zmissing' & 
     (OrdersSitesTested$Test.regime=="Chlamydia::Oral,Chlamydia::Vaginal,Gonorrhoea::Oral,Gonorrhoea::Vaginal" 
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Vaginal,Gonorrhoea::Anal,Gonorrhoea::Vaginal"
     | OrdersSitesTested$Test.regime=="Chlamydia::Urine,Chlamydia::Vaginal,Gonorrhoea::Urine,Gonorrhoea::Vaginal"
     | OrdersSitesTested$Test.regime=="Gonorrhoea::Anal,Gonorrhoea::Oral" | OrdersSitesTested$Test.regime=="Gonorrhoea::Oral,Gonorrhoea::Urine" 
     | OrdersSitesTested$Test.regime=="Gonorrhoea::Anal,Gonorrhoea::Urine" | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Gonorrhoea::Anal,Gonorrhoea::Oral")
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral"
     | OrdersSitesTested$Test.regime=='Gonorrhoea::Anal,Gonorrhoea::Vaginal'
     | OrdersSitesTested$Test.regime=='Chlamydia::Oral,Gonorrhoea::Oral,Gonorrhoea::Vaginal'
     ] <- "CT/GC (dual site)"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" & 
    (OrdersSitesTested$Test.regime=="Gonorrhoea::Vaginal" | OrdersSitesTested$Test.regime=="Gonorrhoea::Urine" 
       | OrdersSitesTested$Test.regime=="Gonorrhoea::Oral" | OrdersSitesTested$Test.regime=="Gonorrhoea::Anal"
       | OrdersSitesTested$Test.regime=="Chlamydia::Vaginal" | OrdersSitesTested$Test.regime=="Chlamydia::Urine"
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal" )
    ] <- "CT/GC (single site)"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" & 
     (OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Vaginal,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Vaginal"
       | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine"
       | OrdersSitesTested$Test.regime=="Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine"
       | OrdersSitesTested$Test.regime=="Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Vaginal"
      )] <- "CT/GC (triple site)"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" & 
     (OrdersSitesTested$Test.regime=="Chlamydia::Oral,Chlamydia::Vaginal,Gonorrhoea::Oral,Gonorrhoea::Vaginal,Hiv::Blood,Syphilis::Treponemal" 
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Vaginal,Gonorrhoea::Anal,Gonorrhoea::Vaginal,Hiv::Blood,Syphilis::Treponemal"
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,Hiv::Blood,Syphilis::Treponemal"
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Urine,Hiv::Blood,Syphilis::RPR,Syphilis::Treponemal"
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Urine,Hiv::Blood,Syphilis::RPR"
     | OrdersSitesTested$Test.regime=="Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Oral,Gonorrhoea::Urine,Hiv::Blood,Syphilis::RPR"
     | OrdersSitesTested$Test.regime=="Chlamydia::Oral,Chlamydia::Vaginal,Gonorrhoea::Vaginal,Hiv::Blood,Syphilis::Treponemal"
     | OrdersSitesTested$Test.regime=="Gonorrhoea::Anal,Gonorrhoea::Oral,Hiv::Blood,Syphilis::Treponemal"
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Vaginal,Gonorrhoea::Urine,Gonorrhoea::Vaginal,Hiv::Blood,Syphilis::Treponemal"
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Gonorrhoea::Anal,Gonorrhoea::Oral,Hiv::Blood,Syphilis::RPR"
     )] <- "All STIs (dual site)"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" & 
    (OrdersSitesTested$Test.regime=="Chlamydia::Vaginal,Gonorrhoea::Vaginal,Hiv::Blood,Syphilis::RPR" 
    | OrdersSitesTested$Test.regime=="Chlamydia::Urine,Gonorrhoea::Urine,Hiv::Blood,Syphilis::RPR"
    | OrdersSitesTested$Test.regime=="Chlamydia::Urine,Gonorrhoea::Urine,Hiv::Blood,Syphilis::RPR,Syphilis::Treponemal"
    | OrdersSitesTested$Test.regime=="Chlamydia::Oral,Gonorrhoea::Oral,Hiv::Blood,Syphilis::RPR"
    | OrdersSitesTested$Test.regime=="Chlamydia::Vaginal,Gonorrhoea::Vaginal,Hiv::Blood,Syphilis::RPR,Syphilis::Treponemal"
    | OrdersSitesTested$Test.regime=="Gonorrhoea::Vaginal,Hiv::Blood,Syphilis::Treponemal"
    | OrdersSitesTested$Test.regime=="Gonorrhoea::Anal,Hiv::Blood,Syphilis::Treponemal"
    | OrdersSitesTested$Test.regime=="Gonorrhoea::Urine,Hiv::Blood,Syphilis::RPR"
    | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Gonorrhoea::Anal,Hiv::Blood,Syphilis::RPR"
    | OrdersSitesTested$Test.regime=="Gonorrhoea::Urine,Hiv::Blood,Syphilis::Treponemal"
      )] <- "All STIs (single site)"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" & 
     (OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,Hiv::Blood,Syphilis::RPR,Syphilis::Treponemal"
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,Hiv::Blood,Syphilis::Treponemal"
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,Hiv::Blood,Syphilis::RPR"
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Vaginal,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Vaginal,Hiv::Blood,Syphilis::Treponemal"
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Chlamydia::Vaginal,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,Gonorrhoea::Vaginal,Hiv::Blood,Syphilis::Treponemal"
     | OrdersSitesTested$Test.regime=="Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,Hiv::Blood,Syphilis::Treponemal"
     | OrdersSitesTested$Test.regime=='Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,Hiv::Blood,Syphilis::Treponemal'
     )] <- "All STIs (triple site)"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" & 
     (OrdersSitesTested$Test.regime=="Chlamydia::Oral,Chlamydia::Vaginal,Gonorrhoea::Oral,Gonorrhoea::Vaginal,Syphilis::Treponemal" 
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Vaginal,Gonorrhoea::Anal,Gonorrhoea::Vaginal,Syphilis::Treponemal"
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Urine,Syphilis::RPR"
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Gonorrhoea::Anal,Gonorrhoea::Oral,Syphilis::RPR"
     | OrdersSitesTested$Test.regime=="Chlamydia::Oral,Chlamydia::Vaginal,Gonorrhoea::Oral,Gonorrhoea::Vaginal,Hiv::Blood,Syphilis::RPR"
     | OrdersSitesTested$Test.regime=="Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Oral,Gonorrhoea::Urine,Syphilis::RPR"
      )] <- "Syphilis & CT/GC (dual site)"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" & 
    (OrdersSitesTested$Test.regime=="Chlamydia::Vaginal,Gonorrhoea::Vaginal,Syphilis::RPR" 
    | OrdersSitesTested$Test.regime=="Gonorrhoea::Vaginal,Syphilis::Treponemal"
    | OrdersSitesTested$Test.regime=="Chlamydia::Urine,Gonorrhoea::Urine,Syphilis::RPR"
    | OrdersSitesTested$Test.regime=="Chlamydia::Oral,Gonorrhoea::Oral,Syphilis::RPR"
    | OrdersSitesTested$Test.regime=="Gonorrhoea::Oral,Hiv::Blood,Syphilis::Treponemal"
    )] <- "Syphilis & CT/GC (single site)"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" & 
     (OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,Syphilis::RPR" 
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Vaginal,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Vaginal,Syphilis::Treponemal"
     | OrdersSitesTested$Test.regime=="Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,Syphilis::RPR"
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Urine,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,Syphilis::RPR,Syphilis::Treponemal"
       )] <- "Syphilis & CT/GC (triple site)"


OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" & 
     (OrdersSitesTested$Test.regime=="Chlamydia::Oral,Chlamydia::Vaginal,Gonorrhoea::Oral,Gonorrhoea::Vaginal,Hiv::Blood" 
     | OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Vaginal,Gonorrhoea::Anal,Gonorrhoea::Vaginal,Hiv::Blood")
     ] <- "HIV & CT/GC (dual site)"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" & 
                                   (OrdersSitesTested$Test.regime=="Chlamydia::Oral,Hiv::Blood")
                                 ] <- "HIV & CT/GC (single site)"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" & 
     (OrdersSitesTested$Test.regime=="Chlamydia::Anal,Chlamydia::Oral,Chlamydia::Vaginal,Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Vaginal,Hiv::Blood" 
    | OrdersSitesTested$Test.regime=="Gonorrhoea::Anal,Gonorrhoea::Oral,Gonorrhoea::Urine,Hiv::Blood"
     )] <- "HIV & CT/GC (triple site)"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" & 
     (OrdersSitesTested$Test.regime=="Hiv::Blood,Syphilis::RPR,Syphilis::Treponemal" 
     |  OrdersSitesTested$Test.regime=="Hiv::Blood,Syphilis::Treponemal"
     | OrdersSitesTested$Test.regime=="Hiv::Blood,Syphilis::RPR")
     ] <- "HIV & Syphilis"

OrdersSitesTested$Sites.tested1 [OrdersSitesTested$Sites.tested1=="zmissing" & OrdersSitesTested$Test.regime=="Syphilis::RPR" 
    ] <- "Syphilis"

table(OrdersSitesTested$Sites.tested1=="zmissing")
# END Sites.Tested1 ----


# ORDERS DISPATCHED per Sites.tested----
# create dataframe for relevant month. Exclude Fettle: we don't invoice Fettle, and the categorisation of Sites.tested=N/A would take forever, there's a huge variety of tests combinations.
InvoicOrders <- OrdersSitesTested[(OrdersSitesTested$Dispatched.at.month.year=="2021-03"),]
class(InvoicOrders)
# check that there are no orders posted with a Sites.tested that is <blank>
Zero <- InvoicOrders[(InvoicOrders$Sites.tested1 == "zmissing"),]
# use 'droplevels' to remove the rows from the table function output which have 0 counts 
# NO USE: table(droplevels(Zero$Test.regime))
rm(Zero)


# table per area and Convert the table into a data frame matrix, so it's an object I can export later
InvoicOrders = as.data.frame.matrix(table(InvoicOrders$Sites.tested1, InvoicOrders$Area))
# remove the white spaces from the header with 'gsub'----
colnames(InvoicOrders) <- gsub(" ","",colnames(InvoicOrders))
# remove the columns not needed for invoicing (i.e. Gilead, etc) with subset and "-c"
InvoicOrders <- subset(InvoicOrders, select=-c(Gilead,Gogodoc,Herefordshire,London,MTVevent))
# add column with the total of rows with 'rowSums' 
InvoicOrders$Total.Orders <- rowSums(InvoicOrders[,-1])
# add row with total of the columns
InvoicOrders["Total InvoicOrders",(2:45)] <- colSums(InvoicOrders[,2:45], na.rm=TRUE)
