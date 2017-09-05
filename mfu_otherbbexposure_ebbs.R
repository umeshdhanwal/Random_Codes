library(dplyr)
library(tidyr)

## Reading commandline arguments.
args <- commandArgs(trailingOnly = TRUE)

## Exit if number of argument is wrong
if (length(args) != 4) {
    stop("Incorrect number of arguments.")
} else {
    ebbs_source_file <- as.character(args[1])
    ebbs_map_file <- as.character(args[2])
    error_file <- as.character(args[3])
    ebbs_output_file <- as.character(args[4])
}

## Read EBBS file
ebbs_data <- read.csv(ebbs_source_file,
                      stringsAsFactors = FALSE)
ebbs_data$startdate <- as.Date(ebbs_data$startdate, format = "%m/%d/%Y")

## Read EBBS product mapping file
ebbs_product <- read.csv(ebbs_map_file,
                         stringsAsFactors = FALSE)

## Getting only the unique product
ebbs_product <- unique(ebbs_product)

## Filter away LTP, NACC, and UNKNOWN
ebbs_data <- ebbs_data[ebbs_data$tradesourcesystem != 'LTP',]
ebbs_data <- ebbs_data[ebbs_data$leid != 'UNKNOWN',]
ebbs_data <- ebbs_data[ebbs_data$tradesourcesystem != 'NACC',]

nrow(ebbs_data)

## clean up the product key
ebbs_data <- ebbs_data %>% separate(productcode, c("productcode1", 
                                                   "productcode2"), "_")
ebbs_data$mapkey <- with(ebbs_data, 
                         paste0(bookinglocation, productcode1))
ebbs_data <- ebbs_data %>% left_join(ebbs_product, 
                                     by = c("mapkey" = "ProductMapping"))

## Check for duplicate produc mapping
duplicate_product <- ebbs_product %>% group_by(ProductMapping) %>% filter(n()>1)
duplicate_product <- duplicate_product %>%
    left_join(ebbs_data, by = c("ProductMapping" = "mapkey"))

nrow(ebbs_data)
nrow(duplicate_product)
write.csv(duplicate_product, 
          file = error_file,
          row.names = FALSE)

## Add final product code
ebbs_data$finalproduct <- ebbs_data$BaselProductCode

## Map product using CMS Limit Product Code
ebbs_data <- within(ebbs_data, 
                    finalproduct[cmslimitproductcode != 'UNKNOWN'] <- 
                        cmslimitproductcode[cmslimitproductcode != 'UNKNOWN'])

## Update Korea product with second productcode
ebbs_data <- within(ebbs_data, 
                    finalproduct[tradesourcesystem == 'KOREAKR' & 
                                     !is.na(productcode2) &
                                     cmslimitproductcode == 'UNKNOWN'] <- 
                        productcode2[tradesourcesystem == 'KOREAKR' & 
                                         !is.na(productcode2) &
                                         cmslimitproductcode == 'UNKNOWN'])

## Update Korea product with first productcode if second is NA
ebbs_data <- within(ebbs_data, 
                    finalproduct[tradesourcesystem == 'KOREAKR' & 
                                     is.na(productcode2) &
                                     cmslimitproductcode == 'UNKNOWN'] <- 
                        productcode1[tradesourcesystem == 'KOREAKR' & 
                                         is.na(productcode2) &
                                         cmslimitproductcode == 'UNKNOWN'])

## Default productcode1 009 to GMGMKTNBAL
ebbs_data <- within(ebbs_data, 
                    finalproduct[productcode1 == '009'] <- 'GMGMKTNBAL')

## Default productcode1 015 to GMGMKTNBAL
ebbs_data <- within(ebbs_data, 
                    finalproduct[productcode1 == '015'] <- 'GMGMKTNBAL')

## Default productcode1 NOSTRO-A to GMGMKTNBAL
#ebbs_data <- within(ebbs_data, 
#                    finalproduct[productcode1 == 'NOSTRO-A'] <- 'GMGMKTNBAL')

## Default all deal with tradesourcesystem HOGANHK to LEODREOD
ebbs_data <- within(ebbs_data, 
                    finalproduct[tradesourcesystem == 'HOGANHK'] <- 'LEODREOD')

## Default all BaselProductCode NA to LEODREOD
ebbs_data[is.na(ebbs_data$finalproduct),]$finalproduct <- 'LEODREOD'

## Default all BaselProductCode that is UNMAPPED to LEODREOD
#ebbs_data[ebbs_data$finalproduct == 'UNMAPPED',]$finalproduct <- 'LEODREOD'

## Default transactionid containing opics to GMGMKTNBAL
ebbs_data[grep("opics", 
               ebbs_data$transactionid,
               ignore.case = TRUE),]$finalproduct <- 'GMGMKTNBAL'

## Default all null string to Unknown EBBS
ebbs_data[ebbs_data$finalproduct == "null",]$finalproduct <- 'Unknown EBBS'

## Default maturity date to current day + 10 days
maturity <- as.Date(Sys.Date()) + 10

## Create final data frame for export to csv
csv_data <- data.frame(ebbs_data$transactionid,
                       ebbs_data$bookinglocation,
                       ebbs_data$leid,
                       ebbs_data$limitid,
                       ebbs_data$startdate,
                       maturity,
                       ebbs_data$outstanding,
                       ebbs_data$txnccy,
                       ebbs_data$finalproduct,
                       stringsAsFactors = FALSE)

names(csv_data) <- make.names(c("TransactionID",
                                "BookingLocation",
                                "LEID",
                                "LimitID",
                                "StartDate",
                                "MaturityDate",
                                "Outstanding",
                                "TxnCCY",
                                "Product"))

write.csv(csv_data, 
          file = ebbs_output_file,
          row.names = FALSE)