library(dplyr)
library(tidyr)

CleanData <- function(file_name) {
# Converts the cross tab EE and PFE profile data file into a tall table.
#
# Args:
# file_name: Full path of the EE or PFE profile file. 
#
# Returns:
# A cleaned up data frame containing the EE or PFE by tenor
    
    # Read CSV file into a data frame.
    ee <- read.csv(file_name,
                   sep = "|", 
                   stringsAsFactors = FALSE)
    
    # Remove branch level information.
    ee <- ee[ee$SCB_Entity == "",]
    
    # Ensure only OTC pfe and ee is used. Blank measure name refers
    # to the row containing the time point.
    ee <- ee[ee$Measure_Name == "PFE (End of Day Control)" |
                 ee$Measure_Name == "EE (End of Day Control)" |
                 ee$Measure_Name == "",]
    
    
    # Clean up the header and split up the date header.
    header <- names(ee)
    header <- c(header, unlist(strsplit(header[15], 
                                        split = "[.]")))
    
    # Clean up the data and split the EE column data.
    ee <- cbind(ee, data.frame(do.call('rbind',
                                       strsplit(as.character(ee[,15]),
                                                split = ","))))
    
    # Set the column names.
    names(ee) <- make.names(header)
    
    # Get the leid and EE data into a data frame.
    ee <- data.frame("Code"=ee$Code, 
                     ee[,16:113], 
                     stringsAsFactors = FALSE)
    ee[,2:99] <- sapply(ee[,2:99], 
                        as.character)
    
    # Using tidyr to change EE data from wide to long data.
    ee_value <- gather(ee[2:nrow(ee),], tenor, eevalue, 2:99)
    names(ee_value) <- make.names(c("code","tenor","eevalue"))
    
    # Using tidyr to change EE date from wide to long data.
    ee_date <- gather(ee[1,], tenor, eedate, 2:99)
    ee_date$eedate <- as.Date(ee_date$eedate, format = "%d%B%Y")
    ee_date$Code <- NULL
    
    # Merging eevalue and eedate. Using dplyr package for performance.
    ee_clean <- inner_join(ee_value, ee_date, by = "tenor")
    ee_clean$eevalue <- as.numeric(ee_clean$eevalue)
    ee_clean$code <- as.character(ee_clean$code)
    ee_clean$tenor <- NULL
    
    ee_clean
}

# Reading commandline arguments.
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 4) {
    # Read the full path of the EE and PFE profile source file from
    # the parameters.
    ee_file_path <- as.character(args[1])
    pfe_file_path <- as.character(args[2])
    
    # Read the destination path for the combined file.
    profile_destination_path <- as.character(args[3])
    eepe_destination_path <- as.character(args[4])
    
    # Process the EE profile data.
    ee_data <- CleanData(ee_file_path)
    names(ee_data) <- make.names(c("code", "eevalue", "date"))
    
    # Process the PFE profile data.
    pfe_data <- CleanData(pfe_file_path)
    names(pfe_data) <- make.names(c("code", "pfevalue", "date"))
    
    # Combine both EE and PFE profile data.
    profile_data <- full_join(ee_data, pfe_data, by = c("code","date"))
    
    # Default all NA to 0.
    profile_data[is.na(profile_data)] <- 0
    
    # Default convert thousands to actual.
    profile_data$eevalue <- profile_data$eevalue * 1000
    profile_data$pfevalue <- profile_data$pfevalue * 1000
    
    # Extract the EE profile for EEPE calculation
    eepe_data <- data.frame(profile_data$code,
                   profile_data$date,
                   profile_data$eevalue, stringsAsFactors = FALSE)
    names(eepe_data) <- make.names(c("code", "date", "eevalue"))
    
    # Format the date so it can be imported into ACCESS.
    profile_data$date <- format(profile_data$date, "%m/%d/%Y")
    
    # Export counterparty PFE and EE profile.
    write.csv(profile_data, 
              file = profile_destination_path, 
              row.names = FALSE)
    
    # Add a cummulative max calcuation column which is the EEE.
    eepe_data <- eepe_data %>% 
        group_by(code) %>% 
        arrange(code, date) %>%
        mutate(eeevalue = cummax(eevalue))
    
    # Add a lagged date column (lag by 1 day) used to calculate
    # day difference between 2 consecutive profile date.
    eepe_data <- mutate(eepe_data, 
                        lagged_date = lag(date, default = min(date)))
    
    # Add a column with day difference between 2 consecutive 
    # profile date.
    eepe_data$date_diff <- as.numeric(eepe_data$date - 
                                          eepe_data$lagged_date)
    
    # Calculate the weighted EEE
    eepe_data$weighted_eee <- as.numeric(eepe_data$eeevalue * 
                                             eepe_data$date_diff)
    
    # Limit the EEPE date to 1 year (including) from the start date.
    date_year_lag <- as.Date(min(eepe_data$date) + 365)
    eepe_data <- eepe_data[eepe_data$date <= date_year_lag,]
    
    # Calculate the EEPE by counterparty
    cpty <- eepe_data %>% 
        group_by(code) %>% 
        summarise(eepe = (sum(weighted_eee)/sum(date_diff)))
    
    # Export the calculated counterparty EEPE.
    write.csv(cpty, 
              file = eepe_destination_path, 
              row.names = FALSE)
} else {
    print("Incorrect number of parameters")
}