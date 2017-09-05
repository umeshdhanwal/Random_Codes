library(R2HTML)
options(scipen = 999)


# Create the HTML output for table and plot graphs 

load("C:\\MFU\\RScript\\DQ_Checks.RData")

HTMLStart(outdir="Z:/Hadoop/MFU/DQ", file="Source_File_check",
  	extension="html", echo=FALSE, HTMLframe=FALSE)

HTML.title("Source Report", HR=1)

HTML.title("Data Quality Check for Source Files", HR=2)

HTML.title("#Number of Records with Percentage Change(Absolute)", HR=3)

HTMLInsertGraph(GraphFileName="C:/MFU/RScript/RecordCount.png")

HTML.title("Size of Source File (in KB) with Percentage Change(Absolute)", HR=3)

HTMLInsertGraph(GraphFileName="C:/MFU/RScript/SizeGraph.png")

HTML.title("Table Showing File Size", HR=3)

HTML(nn,asDF=TRUE,,nsmall=c(0,2),row.names = FALSE)

HTML.title("Table Showing Number of Records", HR=3)

HTML(nn1,asDF=TRUE,,nsmall=c(0,2),row.names = FALSE)

HTMLhr()

HTMLStop()


