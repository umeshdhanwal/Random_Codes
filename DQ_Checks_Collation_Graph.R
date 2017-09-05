start.time <- Sys.time()

library(RGraphics)
library(ggplot2)
library(R.utils)
library(dplyr)
library(reshape)
library(plotly)

finalgraph<-function(Source_File,numberofdays){
#Parameters for Source file as well look up for last n days needs to be passed
#for graphs

fullset<-read.csv(Source_File,header=TRUE,stringsAsFactors=FALSE)

print(fullset)

fullset$cobdate<-as.Date(fullset$cobdate,"%Y-%m-%d")

print(fullset)

ss=arrange(distinct(fullset,cobdate),desc(cobdate))

datestokeep<-arrange(distinct(fullset,cobdate),desc(cobdate))[1:numberofdays,]

finalset<-subset(fullset,cobdate%in%datestokeep)

is.num <- sapply(finalset, is.numeric)
finalset[is.num] <- lapply(finalset[is.num], round, 2)

finalset$size<- round(finalset$size,0)

#Extracting the Number of Records and Percentage Change
arrangedtable2<- data.frame(finalset$filename,finalset$cobdate,finalset$numberofrecords,
finalset$pct_numberofrecords)

colnames(arrangedtable2)=c("Filename","COBDate","Number_of_Records","Pct_Records")

#Using the Melt from Reshape package for Tabular Form

mdata1 <- melt(arrangedtable2, id=c("Filename","COBDate","Number_of_Records"))

nn1<-reshape(mdata1,timevar="COBDate",idvar="Filename",direction="wide")
nn1[is.na(nn1)]<-0

#Removing the Variable column
nn1=nn1[, -grep("variable", colnames(nn1))]

#Replacing the .Value with Pct_Records column
names(nn1)<- gsub("value","Pct_Records",names(nn1))

#Extracting the File Size and Percentage Change

arrangedtable<- data.frame(finalset$filename,finalset$cobdate,finalset$size,
finalset$pct_size)

colnames(arrangedtable)=c("Filename","COBDate","File_Size_KB","Pct_Size")

#Extracting the File Size and Percentage Change
arrangedtable<- data.frame(finalset$filename,finalset$cobdate,finalset$size,
finalset$pct_size)

colnames(arrangedtable)=c("Filename","COBDate","File_Size_KB","Pct_Size")

#Using the Melt from Reshape package for Tabular Form

mdata <- melt(arrangedtable, id=c("Filename","COBDate","File_Size_KB"))

nn<-reshape(mdata,timevar="COBDate",idvar="Filename",direction="wide")
nn[is.na(nn)]<-0

#Removing the Variable column
nn=nn[, -grep("variable", colnames(nn))]

#Replacing the .Value with Pct_Size column
names(nn)<- gsub("value","Pct_Size",names(nn))

return(list(finalset,nn,nn1))
}


# Reading commandline arguments.
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 2) {
    # Read the arguments for source parameters: 
    # source_file_path,numberofdays

#Put the path for the file where MFU's Source files are located
value=args[1]

print(value)

#Put the path for the folder where MFU's output files are located
num_days=args[2]

print(num_days)


#value="Z:\\Hadoop\\MFU\\DQ\\Source_Data\\Source.csv"
#num_days=3
zz=finalgraph(value,num_days)

} else {
    print("Incorrect number of parameters")
    
}

#Getting the value from List to data frame
graphset=as.data.frame(zz[1])

#plotting the Graph for Size
g <- ggplot(graphset, aes(cobdate,filename)) + geom_point(aes(size = pct_size), colour = "green") + theme_bw() + xlab("COBDates") + ylab("MFUFiles(% Size)")
g + scale_size_continuous(range=c(0,10)) 
g + guides(fill=FALSE)
size_graph=g + ggtitle("File Size Check")

ggsave("C:\\MFU\\RScript\\SizeGraph.png")
size_graph
dev.off()

nn=as.data.frame(zz[2])
nn1=as.data.frame(zz[3])


#plotting the Graph for Records
g <- ggplot(graphset, aes(cobdate,filename)) + geom_point(aes(size = pct_numberofrecords), colour = "green") + theme_bw() + xlab("COBDates") + ylab("MFUFiles(% Records)")
g + scale_size_continuous(range=c(0,10)) 
g + guides(fill=FALSE)
records_graph=g + ggtitle("File Number of Records Check")

ggsave("C:\\MFU\\RScript\\RecordCount.png")
records_graph
dev.off()


#Saving the image for usage for RMarkdown
save.image("C:\\MFU\\RScript\\DQ_Checks.RData")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken