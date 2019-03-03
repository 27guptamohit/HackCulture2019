#Mohit Gupta
#Team: Crypto Crew
#Project Part Submission for Hack-Culture Spring 2019
#University of Illinois, Urbana Champaign
#Github link: https://github.com/27guptamohit/HackCulture2019.git

library(readxl)

#Below I am loading the downloaded stock files as data frames which I will later concatenate.
#Instead of writing the functions to load all the files at once, I am writing all the steps individually
#so that the team members with non-technical background can understand what is being done.
AMZN = read.csv(file.choose())
BAC = read.csv(file.choose())
C = read.csv(file.choose())
CAG  = read.csv(file.choose()) 
CAT = read.csv(file.choose())
EA = read.csv(file.choose())
FDX = read.csv(file.choose())
GIS = read.csv(file.choose())
GS = read.csv(file.choose())
HSBC = read.csv(file.choose())
JPM = read.csv(file.choose())
MKC = read.csv(file.choose())
ML = read.csv(file.choose())
MS = read.csv(file.choose())
NTDOY = read.csv(file.choose())
ORCL = read.csv(file.choose())
PIY = read.csv(file.choose())
SAP = read.csv(file.choose())
SIEGY = read.csv(file.choose())
UPS = read.csv(file.choose())
WMT = read.csv(file.choose())



#========================================================================
#I am now creating a dataframe which contains the columns with the closing price of the stocks.



master_close = data.frame(Date = HSBC$Date) 

master_close$EA_Close = EA$Adj.Close
master_close$NTDOY_Close = NTDOY$Adj.Close
master_close$AMZN_Close = AMZN$Adj.Close
master_close$CAT_Close = CAT$Adj.Close
master_close$FDX_Close = FDX$Adj.Close
master_close$ORCL_Close = ORCL$Adj.Close
master_close$SAP_Close = SAP$Adj.Close
master_close$SIEGY_Close = SIEGY$Adj.Close
master_close$UPS_Close = UPS$Adj.Close
master_close$WMT_Close = WMT$Adj.Close
master_close$CAG_Close = CAG$Adj.Close
master_close$GIS_Close = GIS$Adj.Close
master_close$MKC_Close = MKC$Adj.Close
master_close$BAC_Close = BAC$Adj.Close
master_close$C_Close = C$Adj.Close
master_close$GS_Close = GS$Adj.Close
master_close$HSBC_Close = HSBC$Adj.Close
master_close$JPM_Close = JPM$Adj.Close
master_close$MS_Close = MS$Adj.Close

#Saving the data frame as a csv file.

#If you want to save the above dataframe, un-comment the below line.
#write.csv(master_close[order(master_close$Date, decreasing = FALSE),], file = "master_close.csv")


#========================================================

#Now reading the crypto currency's data which was provided by the team members.
#I observed that the crypto currency traded on all the seven days of the week
#and moreover the date format of the crypto currency was in the form of "2019-02-28T22:59:59.000Z"
#which was required to be converted in the form of "yyyy-mm-dd" and the weekends were also required to be removed.

crypto = read.csv(file.choose()) #File name: Bitcoin&ETH.csv

crypto$Date = levels(droplevels(crypto$Date))

crypto$Date = format(as.Date(substr(crypto$Date, 1,10)), "%Y/%m/%d")

#Using the below function, only the rows with common dates will be selected and rest rows would be discarded.
#Thus we will have all the data for the crypto currencies and stock prices with only common dates.
#The weekend values of the crypto currencies will also be removed in the process.

final_df = merge(crypto, master_close)

#Saving the above data frame as csv file for future use.
write.csv(final_df, file = "master_closing.csv")


#========================================================

#Now choosing few of the stocks to analyze the correlation against each other and bitcoin.

master_group1 = subset(final_df, select = c(BitCoin, Apple_Close, JPM_Close, WMT_Close))


group1_return = data.frame(diff(as.matrix(log(master_group1))))


cor(group1_return)


plot(group1_return)


#===================
#Choosing another group of stocks.

master_group2 = subset(final_df, select = c(BitCoin, Pfizer_Close, EA_Close, ORCL_Close))

group2_return = data.frame(diff(as.matrix(log(master_group2))))


cor(group2_return)


plot(group2_return)






