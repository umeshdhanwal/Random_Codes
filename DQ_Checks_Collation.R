library(gtools)
library(dplyr)

finaloutput<-function(filename,mfu_path,date,nameoffile,savedfilename)
{

#Relevant Columns is the data frame obtained from file.info 
#and ordered by the date modified

#Return subsetbonds is the dataset where size,percentage is mentioned 
#for analysis


#Set Working Directory
setwd(mfu_path)

filename=filename

date<-as.Date(date,"%Y%m%d")

print(date)

relevent_columns<-file.info(list.files(path=mfu_path,pattern=filename, full.names = T ))

relevent_columns<-relevent_columns[with(relevent_columns, rev(order(as.POSIXct(mtime)))), ]

bbbonds_col<-data.frame(relevent_columns$mtime,relevent_columns$size,rownames(relevent_columns))
colnames(bbbonds_col)<-c('filemodifieddatetime','size','fullpath')

bbbonds_col$numberofrecords=length(readLines(filename))

#Get name of File for Table
bbbonds_col$filename=nameoffile 

#input the Cob Date
bbbonds_col$cobdate=date

#Convert Size in KB
bbbonds_col$size=bbbonds_col$size/1024

#Extract relevant columns
subsetbbbonds=subset(bbbonds_col, select = c(filename,size,numberofrecords,
cobdate))

savedfile=read.csv(savedfilename,header=TRUE,stringsAsFactors=FALSE)

#savedfile$cobdate<-as.Date(savedfile$cobdate,"%m/%d/%Y")

savedfile<-savedfile[!(savedfile$filename==nameoffile & savedfile$cobdate==date),]

finalsubset=smartbind(savedfile,subsetbbbonds)

#Ordering by COB date in increasing order
finalsubset<-finalsubset[with(finalsubset
, order(cobdate)), ]

#Calculating the Percentage change for Size
pct <- (function(x) abs({x/lag(x)}-1)*100)
x=finalsubset %>% group_by(filename) %>% mutate_each(funs(pct), c(size))
finalsubset$pct_size=x$size

#Calculating the Percentage change for number of records
y=finalsubset%>% group_by(filename) %>% mutate_each(funs(pct), c(numberofrecords))
finalsubset$pct_numberofrecords=y$numberofrecords
 
 #Export the Calculated percentage changes to File
 write.table(finalsubset,savedfilename,quote = FALSE,sep=",",col.names=T,row.names=F)
}

# Reading commandline arguments.
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 5) {
    # Read the arguments for source parameters: 
    # filename,mfu_path,date,nameoffile,savedfilename

filename=args[1]


#Put the path for the folder where MFU's output files are located
mfu_path<-args[2]
setwd(mfu_path)

#Input the COB date which would be used for calculation
date<-args[3]


#Input the file name for MFU
namefile=args[4]

#Input the file path for DQ File
namefilename=args[5]

#Input the parameters in function
checktt=finaloutput(filename,mfu_path,date,namefile,namefilename)

} else {
    print("Incorrect number of parameters")
    
}

    




