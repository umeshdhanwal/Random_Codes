library(dplyr)


source("C:\\MFU\\RScript\\KmeansOD.R")

multmerge = function(mypath,mypattern){
#Function for reading and merging files
#example: mypath=C://MFU//Data
#example: mypattern="^Data_Metadata" 
#example: mymergeddata = multmerge("C://MFU//Data","^Data_Metadata")

filenames=list.files(path=mypath,pattern=mypattern, full.names = T ,recursive=TRUE)
datalist = do.call(rbind,lapply(filenames,read.csv))
return(datalist)}

#multmerge("C://MFU//Source","^Source_Metadata.*\\.csv$")

finaloutput<-function(mfu_path,mypattern,record1,record2,savedfilename,kval,numdays,dateofrunning)
{
#Relevant Columns from the data frame is passed to K means
#example: mfu_path->"C:/MFU/Source/"
#example: mypattern->"^Data_Metadata$"
#example: record1->size
#example: record2->numberofrecords
#example: savedfilename->paste0(savedfilename,cobdate)
#example: Kvals=3
#example: numdays=10
#example: dateofrunning="2017-06-21"

#Set Working Directory
setwd(mfu_path)

#Convert numdays into numeric
numdays=as.numeric(numdays)
kval=as.numeric(kval)

#Create an empty DataFrame
bbbonds_col=0
full_outliersdata = data.frame()

##Assigning the Relevant values to the columns of the metadata csv
relevent_columns<-multmerge(mfu_path,mypattern)

#Writing consolidated file to see the data
consolfilename=paste0(savedfilename,"_Consol_",dateofrunning,".csv") 


#Calculating the Percentage change for Size
pct <- (function(x) abs({x/lag(x)}-1)*100)
x=relevent_columns %>% group_by(FilteredFilename) %>% mutate_each(funs(pct), c(size))
relevent_columns$pct_size=x$size

#Calculating the Percentage change for number of records
y=relevent_columns%>% group_by(FilteredFilename) %>% mutate_each(funs(pct), c(numberofrecords))
relevent_columns$pct_numberofrecords=y$numberofrecords

write.csv(relevent_columns,consolfilename,quote = FALSE,sep=",",col.names=T,row.names=F)

savedfilename=paste0(savedfilename,dateofrunning,".csv")

#The below represents the logic for K-MOD
for ( i in unique(relevent_columns$FilteredFilename)){
outliersdata=data.frame(relevent_columns[which(relevent_columns$FilteredFilename==i),])

if(nrow(outliersdata)>=numdays){
   
   subset_Kmod=as.data.frame(subset(outliersdata, select = c(size,numberofrecords)))
   df_total = data.frame()
   for (i in 1:numdays){
   z<- kmod(subset_Kmod, kval,l=1)
   # add vector to a dataframe
   df_total <- rbind(df_total,as.numeric(row.names(z$L)))
   }
   #Trying to find the index for outlier
   valueofl=as.data.frame(table(unlist(df_total)))
   b=valueofl$Var1[valueofl$Freq==max(valueofl$Freq)]
   #levels_split <- strsplit(levels(b), ",")
   #splitcol=data.frame(lapply(levels_split, as.numeric))
   Index_Kmod=as.numeric(levels(b))[b]
   
   #print(splitcol)  
   #if(length(splitcol)==1){
   #colnames(splitcol)=c("col_index")
   #}else{
   #colnames(splitcol)=c("row_index","col_index")
   #}
   FoundedKmod=data.frame(relevent_columns[Index_Kmod,])
   #print(Index_Kmod)
   #print(FoundedKmod)
   matchcheck=sum(match(outliersdata$numberofrecords,FoundedKmod$numberofrecords),na.rm=T)
   FoundedKmod=FoundedKmod[matchcheck<=2]
   FoundedKmod=FoundedKmod[FoundedKmod$pct_size>=5,]
   FoundedKmod=FoundedKmod[FoundedKmod$pct_numberofrecords>=5,]
   full_outliersdata=rbind(full_outliersdata,FoundedKmod)
        
  } 
}
 

full_outliersdata = full_outliersdata %>% na.omit()
full_outliersdata=subset(full_outliersdata, select=c(filename,filemodifieddatetime,size,numberofrecords,cobdate,FilteredFilename))

#Adding the constraint for cobdate to be equal to cobdate
full_outliersdata=full_outliersdata[full_outliersdata$cobdate==dateofrunning,]

#Writing to the csv file which can be used for creating html
write.csv(full_outliersdata,savedfilename,quote = FALSE,sep=",",col.names=T,row.names=F)
}

#Testing function for output:
finaloutput("C://MFU//Source","^Source_Metadata.*\\.csv$",size,numberofrecords,"Kmod_source",2,8,"2017-07-21")

# Reading commandline arguments.
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 8) {
    # Read the arguments for source parameters: 
    # mfu_path with folder name,date,savedfilename

#Put the path for the folder where MFU's output files are located
mfu_path<-args[1]
setwd(mfu_path)

#Input the pattern for the file name
patternoffile<-args[2]

#Input the column for the 1st argument
argument1=args[3]

#Input the column for the 1st argument
argument2=args[4]

#Input the filename for the output file
argument3=args[5]

#Input the parameter for k
argument4=args[6]

#Input the parameter for number of the minimum days parameter 
#which needs to be looked for building the series
argument5=args[7]

#Input the parameter for subsetting the dates 
argument6=args[8]

#Input the parameters in function
checktt=finaloutput(mfu_path,patternoffile,argument1,argument2,argument3,argument4,argument5,argument6)
} else {
    print("Incorrect number of parameters")
    
}