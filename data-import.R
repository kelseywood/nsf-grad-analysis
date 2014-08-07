#data import and clean up
#data downloaded from https://www.fastlane.nsf.gov/grfp/AwardeeList.do?method=loadAwardeeList
#converted from xls to tab-delimited in Open Office (I know it says csv but it's tab-delim)

#read datasets
#options: tab sep, fill blanks with NA, header=column names, no quotation marks, stringsAsFactors off (important for downstream analysis)
nsf2009=read.table("2009-AwardeeList.csv",sep="\t",fill=T,header=T,quote="",stringsAsFactors=FALSE)
nsf2010=read.table("2010-AwardeeList.csv",sep="\t",fill=T,header=T,quote="",stringsAsFactors=FALSE)
nsf2011=read.table("2011-AwardeeList.csv",sep="\t",fill=T,header=T,quote="",stringsAsFactors=FALSE)
nsf2012=read.table("2012-AwardeeList.csv",sep="\t",fill=T,header=T,quote="",stringsAsFactors=FALSE)
nsf2013=read.table("2013-AwardeeList.csv",sep="\t",fill=T,header=T,quote="",stringsAsFactors=FALSE)
nsf2014=read.table("2014-AwardeeList.csv",sep="\t",fill=T,header=T,quote="",stringsAsFactors=FALSE)

#2014 data is formatted differently from previous years
nsf2014$Name=paste(nsf2014$Last, nsf2014$First) #combine first and last name into one column
nsf2014$Email.Address=NA #set email to NA (so can use rbind)
nsf2014$Proposed.Graduate.Institution=NA #set email to NA (so can use rbind)
nsf2014=nsf2014[,3:8] #remove first and last name columns

#add column for year
nsf2009$year=2009
nsf2010$year=2010
nsf2011$year=2011
nsf2012$year=2012
nsf2013$year=2013
nsf2014$year=2014

#rbind to combine rows for each year into one dataset
nsf5yr=rbind(nsf2009,nsf2010,nsf2011,nsf2012,nsf2013,nsf2014)

#since there are some weird characters in there may have to change locale
Sys.setlocale('LC_ALL','C')
