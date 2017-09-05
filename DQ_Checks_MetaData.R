library(gtools)
library(dplyr)

finaloutput<-function(mfu_path,date,savedfilename)
{

#Relevant Columns is the data frame obtained from file.info 
#and ordered by the date modified

#Set Working Directory
setwd(mfu_path)

date<-as.Date(date,"%Y
%m%d")

print(date)

#Create an empty DataFrame
bbbonds_col=0
#bbbonds_col=data.frame(filename=character(),cobdate=date,size=numeric(0),filemodifieddatetime=character(),numberofrecords=numeric(0))


##Assigning the Relevant values to the columns of the metadata csv
relevent_columns1<-file.info(list.files(path=mfu_path,pattern="\\.csv$", full.names = T ,recursive=TRUE))
relevent_columns2<-file.info(list.files(path=mfu_path,pattern="\\.txt$", full.names = T ,recursive=TRUE))
relevent_columns<-rbind(relevent_columns1,relevent_columns2)
relevent_columns<-relevent_columns[with(relevent_columns, rev(order(as.POSIXct(mtime)))), ]
bbbonds_col<-data.frame(relevent_columns$mtime,relevent_columns$size,basename(rownames(relevent_columns)))
colnames(bbbonds_col)<-c('filemodifieddatetime','size','filename')

#Filtered File name
relevanttext=eval(parse(text=paste0("bbbonds_col","$","filename")))
relevanttext=gsub("ALL_CRISKREPMFU", "", relevanttext)
relevanttext=gsub("_D_", "", relevanttext)
relevanttext=gsub(".csv", "", relevanttext)
relevanttext=gsub('[0-9]+', "", relevanttext)
relevanttext=gsub('W', "", relevanttext)
relevanttext=gsub('_M', "", relevanttext)
relevanttext=gsub('_', "", relevanttext)

bbbonds_col$FilteredFilename=relevanttext


#input the Cob Date
bbbonds_col$cobdate=date

#Convert Size in KB
bbbonds_col$size=bbbonds_col$size/1024
cat(str(bbbonds_col))

#Reading each of rows to get the records count
filenames=data.frame(rownames(relevent_columns))
bbbonds_col$numberofrecords=apply(filenames,1,function(x)length(readLines(x)))

#Extract relevant columns
subsetbbbonds=subset(bbbonds_col, select = c(filename,FilteredFilename,filemodifieddatetime,size,numberofrecords,
cobdate))

#Writing to the csv file which can be used for creating html
write.csv(subsetbbbonds,savedfilename,quote = FALSE,sep=",",col.names=T,row.names=F)

}


# Reading commandline arguments.
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 3) {
    # Read the arguments for source parameters: 
    # mfu_path with folder name,date,savedfilename

#Put the path for the folder where MFU's output files are located
mfu_path<-args[1]
setwd(mfu_path)

#Input the COB date which would be used for calculation
date<-args[2]

#Input the file path for creation of metadata csv DQ File
namefilename=args[3]

#Input the parameters in function
checktt=finaloutput(mfu_path,date,namefilename)

} else {
    print("Incorrect number of parameters")
    
}

    




