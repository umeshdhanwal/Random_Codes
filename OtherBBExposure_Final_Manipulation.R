start.time <- Sys.time()

if("sqldf" %in% rownames(installed.packages()) == FALSE) {install.packages("sqldf")}

library('sqldf')


#####Loading all the Files#####
OTP_AIRB=data.frame(read.csv("C:\\OtherBBExposure\\Processed\\OTP_AIRB_Trade_Details.csv",na.strings=''))
FX=data.frame(read.csv("C:\\OtherBBExposure\\Processed\\FX.csv",na.strings=''))
DTP=data.frame(read.csv("C:\\OtherBBExposure\\Processed\\DTP_Trade_Details.csv",na.strings=''))
OTP_Bal=data.frame(read.csv("C:\\OtherBBExposure\\Processed\\OTP_Bal_Trade_Details.csv",na.strings=''))
SCI=data.frame(read.csv("C:\\OtherBBExposure\\Processed\\SCI.csv",header=FALSE))
LTP_Bank=data.frame(read.csv("C:\\OtherBBExposure\\Processed\\LTP_Bank.csv",na.strings=''))
LTP_Non_Bank=data.frame(read.csv("C:\\OtherBBExposure\\Processed\\LTP_Non_Bank.csv",na.strings=''))
SPC_OTP=data.frame(read.csv("C:\\OtherBBExposure\\Source\\SPC_Codes_OTP.csv",na.strings=''))
SPC_DTP=data.frame(read.csv("C:\\OtherBBExposure\\Source\\SPC_Codes_DTP.csv",na.strings=''))

##Defining Key for Non Bank
LTP_Non_Bank[]=lapply(LTP_Non_Bank,as.character)
LTP_Non_Bank[is.na(LTP_Non_Bank)] <- ""

str(LTP_Non_Bank)
LTP_Non_Bank$DTP_Key1=paste0(LTP_Non_Bank$LTP,LTP_Non_Bank$Co12
,LTP_Non_Bank$Lookup,LTP_Non_Bank$Co16,LTP_Non_Bank$Co15)

LTP_Non_Bank$OTP_Key1=paste0(LTP_Non_Bank$LTP,LTP_Non_Bank$Co12
,LTP_Non_Bank$Co13,LTP_Non_Bank$Co14,LTP_Non_Bank$Co16,LTP_Non_Bank$Co15)

LTP_Non_Bank$OTP_Key2=paste0(LTP_Non_Bank$LTP,LTP_Non_Bank$Co12,"*"
,LTP_Non_Bank$Co14,LTP_Non_Bank$Co16,LTP_Non_Bank$Co15)

head(LTP_Non_Bank)

#####Manipulating data frame for AIRB
nrow(OTP_AIRB)

AIRB_1=subset(OTP_AIRB,(OTP_AIRB$Exposure.Type!=6))
nrow(AIRB_1)
head(AIRB_1)
tail(AIRB_1)

AIRB_1[]=lapply(AIRB_1,as.character)
AIRB_1[is.na(AIRB_1)] <- ""

#Defining relevant key for Joining with SCI Banking Book

AIRB_1$CustID_BK_Location_or_CustID_CPtyID_BK_Location=
ifelse(AIRB_1$Exposure.Type== 1, 
paste(AIRB_1$Supplier.ID,AIRB_1$Country.Code,sep=""),
ifelse(AIRB_1$Exposure.Type== 5,
paste(AIRB_1$Supplier.ID,AIRB_1$Country.Code,sep=""),
paste(AIRB_1$Supplier.ID,"-",AIRB_1$Buyer.ID,AIRB_1$Country.Code,sep="")))


##Adding Headers for SCI since its not having headers

head(SCI)
tail(SCI)

colnames(SCI)=c("Header","SCI_LEID","Sub_Profile_ID","Booking_Location"
,"Source_system","Counterparty_Name","SCI_Cust_ID")

SCI$Booking_Location<- gsub('SG/SCSP','SP/SCSP',SCI$Booking_Location) 
SCI$Booking_Location<- gsub('MY/SCOL','LN/SCOL',SCI$Booking_Location)
SCI$Booking_Location<- gsub('AE/SCDF','DF/SCDF',SCI$Booking_Location)
SCI$SCIID_BK=paste0(SCI$SCI_Cust_ID,substr(SCI$Booking_Location,1,2))

SCI_Lookup=SCI[c("SCI_LEID","SCIID_BK")]
head(SCI_Lookup)

#Joining with SCI banking Book for LEID

AIRB_2=sqldf("SELECT t1.*, (select MIN(SCI_LEID) from SCI_Lookup t2 
where t1.[CustID_BK_Location_or_CustID_CPtyID_BK_Location]=t2.SCIID_BK) 
AS SCID_Lookup
FROM AIRB_1 AS t1")

AIRB_2[is.na(AIRB_2)] <- ""

head(AIRB_2)
head(FX)

str(AIRB_2)
tail(AIRB_2)
nrow(AIRB_2)

#Joining with FX file for Lookup

AIRB_3=sqldf("SELECT t1.*, (select Spot from FX t2 
where t1.[Exposure.Ccy]=t2.Code) 
AS Exposure_CCY_Rate
FROM AIRB_2 AS t1")

head(AIRB_3)
tail(AIRB_3)

#Getting the Exposure Value

AIRB_3$Exposure_CCY_Rate=as.numeric(AIRB_3$Exposure_CCY_Rate)
AIRB_3$Exposure.Amount=as.numeric(AIRB_3$Exposure.Amount)

AIRB_3$Exposure_USD_Amt=abs(AIRB_3$Exposure_CCY_Rate*AIRB_3$Exposure.Amount)

str(AIRB_3)

AIRB_3$Key1=paste0(AIRB_3$Country.Code,AIRB_3$Supplier.ID,AIRB_3$Buyer.ID
,AIRB_3$Recourse.Indicator,
AIRB_3$Limit.Group.Id,AIRB_3$Limit.Product.Code)

AIRB_3$Key2=paste0(AIRB_3$Country.Code,AIRB_3$Supplier.ID,"*"
,AIRB_3$Recourse.Indicator,
AIRB_3$Limit.Group.Id,AIRB_3$Limit.Product.Code)

nrow(AIRB_3)
str(AIRB_3)

#Getting the Limit ID from L20 Non Bank file

OTP_AIRB_1=
sqldf("SELECT t1.*, (select MIN(LimitID) from LTP_Non_Bank t2 
where t1.Key1 = t2.OTP_Key1) 
AS Lookup_Non_bank_SCI_Approved_LimitID
FROM AIRB_3 AS t1")

head(OTP_AIRB_1)
str(OTP_AIRB_1)
nrow(OTP_AIRB_1)

#Convert NA into blanks

OTP_AIRB_1[is.na(OTP_AIRB_1)] <- ""

OTP_AIRB_2=sqldf("SELECT t1.*, (select MIN(LimitID) from LTP_Non_Bank t2 
where t1.Key2 = t2.OTP_Key2) AS Lookup2_Non_bank_SCI_Approved_LimitID
FROM OTP_AIRB_1 AS t1")
head(OTP_AIRB_2)
nrow(OTP_AIRB_2)

OTP_AIRB_2[is.na(OTP_AIRB_2)] <- ""

#Getting the Final value of Limit ID from L20 Non Bank file

OTP_AIRB_2$Final_Lookup_Non_bank_SCI_Approved_LimitID=0

OTP_AIRB_2$Final_Lookup_Non_bank_SCI_Approved_LimitID =
ifelse(OTP_AIRB_1$Lookup_Non_bank_SCI_Approved_LimitID == ""
,OTP_AIRB_2$Lookup2_Non_bank_SCI_Approved_LimitID,
OTP_AIRB_1$Lookup_Non_bank_SCI_Approved_LimitID)

OTP_AIRB_3=sqldf("SELECT t1.*, (select min(SCI_LimitID) from LTP_Non_Bank t2 
where t1.Key1 = t2.OTP_Key1) AS Lookup1_Non_bank_SCI_LEID
FROM OTP_AIRB_2 AS t1")
nrow(OTP_AIRB_3)
head(OTP_AIRB_3)
str(OTP_AIRB_3)

OTP_AIRB_3[is.na(OTP_AIRB_3)] <- ""

OTP_AIRB_4=sqldf("SELECT t1.*, (select min(SCI_LimitID) from LTP_Non_Bank t2 
where t1.Key2 = t2.OTP_Key2) AS Lookup2_Non_bank_SCI_LEID
FROM OTP_AIRB_3 AS t1")
nrow(OTP_AIRB_4)
head(OTP_AIRB_4)

OTP_AIRB_4[is.na(OTP_AIRB_4)] <- ""


OTP_AIRB_4$Final_Lookup_Non_bank_SCI_LEID=0

OTP_AIRB_4$Final_Lookup_Non_bank_SCI_LEID=
ifelse(OTP_AIRB_4$Lookup1_Non_bank_SCI_LEID== ""
,OTP_AIRB_4$Lookup2_Non_bank_SCI_LEID,
OTP_AIRB_4$Lookup1_Non_bank_SCI_LEID)

#Lookup for the Standard Product Code 
OTP_AIRB_5=sqldf("SELECT t1.*, (select t2.[Standard.Product.Code] 
from SPC_OTP t2 where t1.[Limit.Product.Code] = t2.Field2) AS SPC
FROM OTP_AIRB_4 AS t1")

OTP_AIRB_5[is.na(OTP_AIRB_5)] <- ""

str(OTP_AIRB_5)
head(OTP_AIRB_5)
nrow(OTP_AIRB_5)

#Cleaning up the output for MFU
 
OTP_AIRB_7=subset(OTP_AIRB_5,(OTP_AIRB_5$Exposure_USD_Amt!=""))
OTP_AIRB_7=subset(OTP_AIRB_7,(OTP_AIRB_7$Exposure_USD_Amt!=0))
str(OTP_AIRB_7)
tail(OTP_AIRB_7)
head(OTP_AIRB_7)

#Getting the Final output for MFU

OTP_AIRB_8=sqldf("SELECT t1.[TXN.ID] AS TransactionID, t1.SCID_Lookup AS LEID, t1.Final_Lookup_Non_bank_SCI_Approved_LimitID AS LimitID, t1.SPC AS Product, t1.TRANS_START_DATE AS StartDate, t1.[Maturity.date] AS MaturityDate, t1.[Exposure.Ccy] AS TxnCCY, t1.Exposure_USD_Amt AS Outstanding,
[Country.Code] as BookingLocation
FROM OTP_AIRB_7 AS t1")

OTP_AIRB_8$Outstanding=abs(as.numeric(OTP_AIRB_8$Outstanding))
OTP_AIRB_8[is.na(OTP_AIRB_8)] <- ""

##The below output for AIRB for checking purposes as MFU doesnt give individual breakup
write.table(OTP_AIRB_7,"C:\\OtherBBExposure\\data\\OTP_AIRB.csv",sep=",",col.names=T,row.names=F)



####Manipulating for OTP_Bal

OTP_Bal[]=lapply(OTP_Bal,as.character)

OTP_Bal[is.na(OTP_Bal)] <- ""

head(OTP_Bal)
str(OTP_Bal)
nrow(OTP_Bal)

#Removing the rows where product already present in AIRB file
OAF_Bal_1=sqldf("select * from OTP_Bal where [Limit.Product.Code] 
not in('RSRV','PADS','PFAC','TRDS','BBTP')")

head(OAF_Bal_1)
str(OAF_Bal_1)
nrow(OAF_Bal_1)

#Getting the key for SCI

OAF_BAL_1=OAF_Bal_1
OAF_BAL_1$Risk_Party_ID_BK_Location=paste0(OAF_Bal_1$Risk.Party.ID,OAF_Bal_1$Booking.Location)

head(OAF_BAL_1)

OAF_BAL_1[is.na(OAF_BAL_1)] <- ""

#Joining with SCI banking book for LEID
OAF_BAL_2=sqldf("SELECT t1.*, (select MIN(SCI_LEID) from SCI_Lookup t2 
where t1.[Risk_Party_ID_BK_Location]=t2.SCIID_BK) 
AS SCID_Lookup
FROM OAF_BAL_1 AS t1")

head(OAF_BAL_2)
nrow(OAF_BAL_2)
str(OAF_BAL_2)
OTP_Bal[is.na(OTP_Bal)] <- ""

#Joining with FX file for Lookup
OAF_BAL_3=sqldf("SELECT t1.*, (select Spot from FX t2 
where t1.[Finance.CCY]=t2.Code) 
AS Finance_CCY_Rate
FROM OAF_BAL_2 AS t1")

head(OAF_BAL_3)
tail(OAF_BAL_3)
OAF_BAL_3[is.na(OAF_BAL_3)] <- ""

#Getting the Exposure Value
OAF_BAL_3$Finance_CCY_Rate=as.numeric(OAF_BAL_3$Finance_CCY_Rate)
OAF_BAL_3$Finance.Outstanding.amount=as.numeric(OAF_BAL_3$Finance.Outstanding.amount)
OAF_BAL_3$Finance_USD_OS=abs(OAF_BAL_3$Finance_CCY_Rate*OAF_BAL_3$Finance.Outstanding.amount)
OAF_BAL_3[is.na(OAF_BAL_3)] <- ""


OAF_BAL_4=sqldf("SELECT t1.*, (select Spot from FX t2 
where t1.[Pastdue.amount.Currency]=t2.Code) 
AS PastDue_CCY_Rate
FROM OAF_BAL_3 AS t1")

head(OAF_BAL_4)
OAF_BAL_4[is.na(OAF_BAL_4)] <- ""

OAF_BAL_4$PastDue_CCY_Rate=as.numeric(OAF_BAL_4$PastDue_CCY_Rate)
OAF_BAL_4$Past.Due.Amount=as.numeric(OAF_BAL_4$Past.Due.Amount)

OAF_BAL_4$PastDue_Amount=abs(OAF_BAL_4$PastDue_CCY_Rate*OAF_BAL_4$Past.Due.Amount)
OAF_BAL_4[is.na(OAF_BAL_4)] <- ""

str(OAF_BAL_4)

#Defining key for L20 Non bank and bank file

OAF_BAL_4$Key1=paste0(OAF_BAL_4$Booking.Location ,OAF_BAL_4$Risk.Party.ID
,OAF_BAL_4$Counterparty.ID,
OAF_BAL_4$Recourse.Indicator,OAF_BAL_4$Limit.Group.ID
,OAF_BAL_4$Limit.Product.Code )

OAF_BAL_4$Key2=paste0(OAF_BAL_4$Booking.Location ,OAF_BAL_4$Risk.Party.ID
,"*",
OAF_BAL_4$Recourse.Indicator,OAF_BAL_4$Limit.Group.ID
,OAF_BAL_4$Limit.Product.Code )


#Joining with L20 Non bank and bank file to get Limit ID

OTP_Bal=sqldf("SELECT t1.*, (select min(limitID) from LTP_Non_Bank t2 
where t1.Key1 = t2.OTP_Key1) AS Lookup1_Approved_SCI_LimitID
FROM OAF_BAL_4 AS t1")
head(OTP_Bal)
nrow(OTP_Bal)
tail(OTP_Bal)


OTP_Bal_1=sqldf("SELECT t1.*, (select min(limitID) from LTP_Non_Bank t2 
where t1.Key2 = t2.OTP_Key2) AS Lookup2_Approved_SCI_LimitID
FROM OTP_Bal AS t1")
head(OTP_Bal_1)
nrow(OTP_Bal_1)
tail(OTP_Bal_1)

#Cleaning the output
OTP_Bal_1$Finance.CCY <- sapply(OTP_Bal_1$Finance.CCY, as.character)
OTP_Bal_1$Pastdue.amount.Currency<- sapply(OTP_Bal_1$Pastdue.amount.Currency, as.character)

OTP_Bal_1$CCY=
ifelse(OTP_Bal_1$Finance.CCY== ""
,OTP_Bal_1$Pastdue.amount.Currency,
OTP_Bal_1$Finance.CCY)

OTP_Bal_1$CCY=gsub('[[:digit:]]+', '', OTP_Bal_1$CCY)

OTP_Bal_1[is.na(OTP_Bal_1)] <- ""

#Getting the final values for L20 Non Bank and Bank file

OTP_Bal_1$Final_Lookup_Non_Bank_Approved_SCI_LimitID=0

OTP_Bal_1$Final_Lookup_Non_Bank_Approved_SCI_LimitID=
ifelse(OTP_Bal_1$Lookup1_Approved_SCI_LimitID== ""
,OTP_Bal_1$Lookup2_Approved_SCI_LimitID,
OTP_Bal_1$Lookup1_Approved_SCI_LimitID)

OTP_Bal_2=sqldf("SELECT t1.*, (select min(LimitID) from LTP_Bank t2 
where t1.[Limit.Reservation.ID] = t2.Key) 
AS Final_Lookup_Bank_Approved_SCI_LimitID
FROM OTP_Bal_1 AS t1")

OTP_Bal_2[is.na(OTP_Bal_2)] <- ""

OTP_Bal_3=
sqldf("SELECT t1.*, (select min(SCI_LimitID) from LTP_Non_Bank t2 
where t1.Key1 = t2.OTP_Key1) 
AS Lookup1_SCI_LEID
FROM OTP_Bal_2 AS t1")

OTP_Bal_3[is.na(OTP_Bal_3)] <- ""
head(OTP_Bal_3)
tail(OTP_Bal_3)
nrow(OTP_Bal_3)

OTP_Bal_4=
sqldf("SELECT t1.*, (select min(SCI_LimitID) from LTP_Non_Bank t2 
where t1.Key2 = t2.OTP_Key2) AS Lookup2_SCI_LEID
FROM OTP_Bal_3 AS t1")
head(OTP_Bal_4)
tail(OTP_Bal_4)
nrow(OTP_Bal_4)

OTP_Bal_4[is.na(OTP_Bal_4)] <- ""

OTP_Bal_4$Final_Lookup_Non_Bank_SCI_LEID=
ifelse(OTP_Bal_4$Lookup1_SCI_LEID== ""
,OTP_Bal_4$Lookup2_SCI_LEID,
OTP_Bal_4$Lookup1_SCI_LEID)

OTP_Bal_5=
sqldf("SELECT t1.*, (select min(LimitID) from LTP_Bank t2 
where t1.[Limit.Reservation.ID] = t2.Key) 
AS Final_Lookup_Bank_SCI_LEID
FROM OTP_Bal_4 AS t1")

OTP_Bal_5[is.na(OTP_Bal_5)] <- ""
head(OTP_Bal_5)
tail(OTP_Bal_5)
nrow(OTP_Bal_5)

OTP_Bal_5$Final_Limit_ID=
ifelse(OTP_Bal_5$Final_Lookup_Non_Bank_Approved_SCI_LimitID== ""
,OTP_Bal_5$Final_Lookup_Bank_Approved_SCI_LimitID,
OTP_Bal_5$Final_Lookup_Non_Bank_Approved_SCI_LimitID)


OTP_Bal_5$Final_Limit_ID=substr(OTP_Bal_5$Final_Limit_ID, 1, 8)


#Outstanding is either past due or Finance USD
OTP_Bal_5$Outstanding=
ifelse(OTP_Bal_5$Finance_USD_OS== 0
,OTP_Bal_5$PastDue_Amount,
OTP_Bal_5$Finance_USD_OS)

OTP_Bal_5$Outstanding=
ifelse(OTP_Bal_5$Finance_USD_OS==""
,OTP_Bal_5$PastDue_Amount,
OTP_Bal_5$Finance_USD_OS)

#Doing lookup for the Standard Product Code

OTP_Bal_6=
sqldf("SELECT t1.*, (select t2.[Standard.Product.Code] 
from SPC_OTP t2 where t1.[Limit.Product.Code] = t2.Field2) AS SPC
FROM OTP_Bal_5 AS t1")

OTP_Bal_6[is.na(OTP_Bal_6)] <- ""
OTP_Bal_6=subset(OTP_Bal_6,(OTP_Bal_6$Outstanding!=0))
OTP_Bal_6=subset(OTP_Bal_6,(OTP_Bal_6$Outstanding>1))

head(OTP_Bal_6)
tail(OTP_Bal_6)
str(OTP_Bal_6)
nrow(OTP_Bal_6)

#Getting the relevant columns for MFU
OTP_Bal_7=
sqldf("SELECT [Transaction.ID] AS TransactionID, SCID_Lookup AS LEID, Final_Limit_ID AS LimitID, SPC as Product, [Transaction...Trade.Date] AS StartDate, [Maturity.Date] AS MaturityDate, [CCY] as TxnCCY, Outstanding
,[Booking.Location] as BookingLocation FROM OTP_Bal_6")
head(OTP_Bal_7)
tail(OTP_Bal_7)
nrow(OTP_Bal_7)

##The below output for OTP Bal for checking purposes as MFU doesnt give individual breakup
write.table(OTP_Bal_6,"C:\\OtherBBExposure\\data\\OTP_bal.csv",sep=",",col.names=T,row.names=F)


####Manipulating for DTP

DTP[]=lapply(DTP,as.character)

DTP[is.na(DTP)] <- ""


head(DTP)
tail(DTP)
str(DTP)

#Removing the derivatives product 
DTP_col=sqldf("select * from DTP where [Product.Code.Local] Not LIKE '%MAR%'")

head(DTP_col)

#Getting the key for SCI banking book
 
DTP_col$TPID_Booking_Location=paste0(DTP_col$Transaction.Processing.System.ID
,DTP_col$Booking.location)

DTP_col$RiskCpty_Booking_Location=paste0(DTP_col$Risk.Counterparty.ID...TP,
DTP_col$Booking.location)

DTP_col$TLS_Risk_Party_ID_Booking_Location=paste0(DTP_col$TLS.Risk.Party.ID,
DTP_col$Booking.location)

DTP_col$Check_TPID_RiskCpty=ifelse(DTP_col$Risk.Counterparty.ID...TP=="",
DTP_col$TPID_Booking_Location,DTP_col$RiskCpty_Booking_Location)

DTP_col$Check_TPID_RiskCpty=ifelse(DTP_col$Product.Code.Local=="CGTSO",
DTP_col$TLS_Risk_Party_ID_Booking_Location,DTP_col$Check_TPID_RiskCpty)

DTP_col$Check_TPID_RiskCpty=ifelse(DTP_col$Product.Code.Local=="CGTSN",
DTP_col$TLS_Risk_Party_ID_Booking_Location,DTP_col$Check_TPID_RiskCpty)


#Joining with SCI banking book for Lookup
DTP_col2=sqldf("SELECT t1.*, (select MIN(SCI_LEID) from SCI_Lookup t2 
where t1.[Check_TPID_RiskCpty]=t2.SCIID_BK) 
AS SCID_Lookup
FROM DTP_col AS t1")

head(DTP_col2)
str(DTP_col2)

#Joining with FX for the exchange rates

DTP_col2=sqldf("SELECT t1.*, (select Spot from FX t2 
where t1.[Currency]=t2.Code) 
AS Principal_CCY
FROM DTP_col2 AS t1")

#Putting Principal Amount in absolute terms
DTP_col2$Principal_Amount_Absolute=abs(as.numeric(DTP_col2$Principal.Amount.Current))

DTP_col2$Principal_USD_Amt=DTP_col2$Principal_Amount_Absolute/100*
as.numeric(DTP_col2$Principal_CCY)

##Getting the key for L20 Non bank
DTP_col2$Key=paste0(DTP_col2$Booking.location,DTP_col2$TLS.Customer.ID
,substr(DTP_col2$transaction.id,1,12),DTP_col2$Limit.group.ID,DTP_col2$Limit.Product.Code)

#Joining with the L20 Non bank file for Limit ID
DTP_1=sqldf("SELECT t1.*, (select min(limitID) from LTP_Non_Bank t2 where 
t1.Key = t2.DTP_Key1) AS Lookup_Non_bank_SCI_Approved_LimitID
FROM DTP_col2 AS t1")
head(DTP_1)
nrow(DTP_1)
tail(DTP_1)

DTP_1[is.na(DTP_1)] <- ""

#Joining with the L20 bank file for Limit ID
DTP_2=sqldf("SELECT t1.*, (select min(LimitID) from LTP_Bank t2 where 
t1.[Limit.Reservation.ID] = t2.Key) AS Lookup_bank_SCI_Approved_LimitID
FROM DTP_1 AS t1")
DTP_2[is.na(DTP_2)] <- ""

head(DTP_2)
nrow(DTP_2)
tail(DTP_2)

#Getting the Final Limit ID for MFU
DTP_2$Final_Limit_ID=
ifelse(DTP_2$Lookup_Non_bank_SCI_Approved_LimitID==""
,DTP_2$Lookup_bank_SCI_Approved_LimitID,
DTP_2$Lookup_Non_bank_SCI_Approved_LimitID)

#Getting the LEID for LTP Non Bank
DTP_3=sqldf("SELECT t1.*, (select min(SCI_LimitID) from LTP_Non_Bank t2 where 
t1.Key = t2.DTP_Key1) AS Lookup_Non_bank_LimitID_SCILEID
FROM DTP_2 AS t1")
DTP_3[is.na(DTP_3)] <- ""

head(DTP_3)
nrow(DTP_3)
tail(DTP_3)

#Getting the Standard Product Code
DTP_4=sqldf("SELECT t1.*, (select t2.[Standard.Product.Code] from SPC_DTP t2 
where t1.[Product.Code.Local] = t2.[Product.Variant.Code]) AS SPC
FROM DTP_3 AS t1")
DTP_4[is.na(DTP_4)] <- ""

head(DTP_4)
nrow(DTP_4)
tail(DTP_4)

DTP_4=subset(DTP_4,(DTP_4$Principal_USD_Amt!=0))
DTP_4=subset(DTP_4,(DTP_4$Principal_USD_Amt!=""))


#Final Output for MFU for DTP
DTP_5=sqldf("SELECT [transaction.id] AS TransactionID, [SCID_Lookup] AS LEID
, [Final_Limit_ID] AS LimitID, SPC AS Product, 
[Start.Date..GDS..Trade.Date..Value.Date.] AS StartDate, 
[Maturity.Date]  AS MaturityDate, [Currency] AS TxnCCY, 
[Principal_USD_Amt] AS Outstanding,[Booking.location] as BookingLocation
FROM DTP_4")
head(DTP_5)
nrow(DTP_5)
tail(DTP_5)

##The below output for DTP for checking purposes as MFU doesnt give individual breakup
write.table(DTP_4,"C:\\OtherBBExposure\\data\\DTP.csv",sep=",",col.names=T,row.names=F)


##Combining for MFU for OTp bal,AIRB and DTP

FinalMFU1=data.frame(rbind(OTP_AIRB_8,OTP_Bal_7))
FinalMFU2=rbind(FinalMFU1,DTP_5)

FinalMFU2$Product[FinalMFU2$Product==""]="Unknown_LTP"

str(FinalMFU2)

head(FinalMFU2)


##Final MaturityDate when blank

Mdate=as.Date(Sys.time())+10
Mdate=format(strptime(Mdate,"%Y-%m-%d"),"%d%m%Y")
FinalMFU2$MaturityDate=
ifelse(FinalMFU2$MaturityDate==""
,Mdate,
FinalMFU2$MaturityDate)

Fdate=as.Date(Sys.time())-1
Fdate=format(strptime(Fdate,"%Y-%m-%d"),"%Y%m%d")
filename= paste ("C:\\OtherBBExposure\\Final_MFU_File\\OtherBBExposure_LTP_",Fdate, ".csv", sep="")

#Final output is stored in the below location
write.table(FinalMFU2,filename,quote = FALSE,sep=",",col.names=T,row.names=F)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



