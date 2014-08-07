# nsf-grfp-analysis.R by Kelsey Wood
# data downloaded from nsf website and converted from xls to tab-delimited in Open Office (I know it says csv but it's tab-delim)

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

#since there are some weird characters in there have to change locale
Sys.setlocale('LC_ALL','C')

#get data from UC schools using grep
UC=subset(nsf5yr,grepl("University of California",nsf5yr$Current.Institution,ignore.case=T))

#examine data
t=as.data.frame(table(UC$Current.Institution))

#lots of inconsistent naming
#replace values using grep - if any part of string contains "Berkeley" replace whole string with "University of California - Berkeley"
UC$Current.Institution[grepl("Berkeley",UC$Current.Institution,ignore.case=T)]<-"University of California-Berkeley"
UC$Current.Institution[grepl("San Di",UC$Current.Institution,ignore.case=T)]<-"University of California-San Diego"
UC$Current.Institution[grepl("San Francisco",UC$Current.Institution,ignore.case=T)]<-"University of California-San Francisco"
UC$Current.Institution[grepl("Santa Barbara",UC$Current.Institution,ignore.case=T)]<-"University of California-Santa Barbara"
UC$Current.Institution[grepl("Davis",UC$Current.Institution,ignore.case=T)]<-"University of California-Davis"
UC$Current.Institution[grepl("Los Angeles",UC$Current.Institution,ignore.case=T)]<-"University of California-Los Angeles"
UC$Current.Institution[grepl("Riverside",UC$Current.Institution,ignore.case=T)]<-"University of California-Riverside"
UC$Current.Institution[grepl("Irvine",UC$Current.Institution,ignore.case=T)]<-"University of California-Irvine"
UC$Current.Institution[grepl("Santa Cruz",UC$Current.Institution,ignore.case=T)]<-"University of California-Santa Cruz"
UC$Current.Institution[grepl("Merced",UC$Current.Institution,ignore.case=T)]<-"University of California-Merced"

#re-examine data and make frequency table for number of awards by institution and year
t=as.data.frame(table(UC$Current.Institution,UC$year))
#rename columns to something useful
names(t)=c("University","Year","Freq")
#realize I dont need "University of California..." so replacing that with nothing using gsub (grep sub)
t$University=gsub("University of California-","",t2$University)

#since number of awards changes per year..
#add data on total number of awards to calculate percent of total
> as.data.frame(table(nsf5yr$year))
Var1 Freq
1 2009 1248
2 2010 2051
3 2011 2077
4 2012 2067
5 2013 2064
6 2014 2000

#calculate proportion by dividing by number of winners for each year
t$Prop=NA   #just to create column called Prop
t[t$Year==2009,]$Prop=t[t$Year==2009,]$Freq/1248
t[t$Year==2010,]$Prop=t[t$Year==2010,]$Freq/2051
t[t$Year==2011,]$Prop=t[t$Year==2011,]$Freq/2077
t[t$Year==2012,]$Prop=t[t$Year==2012,]$Freq/2067
t[t$Year==2013,]$Prop=t[t$Year==2013,]$Freq/2064
t[t$Year==2014,]$Prop=t[t$Year==2014,]$Freq/2000

#plot data by frequency
plot=ggplot(data=t, aes(x=Year, y=Freq, group=University, color=University)) 
plot + geom_line()

#plot data by percent
plot=ggplot(data=t, aes(x=Year, y=Percent, group=University, color=University)) 
plot + geom_line()

#custom palette made using colorbrewer (colorbrewer2.org)
cbPalette <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")

#plot Percent
plot=ggplot(data=t, aes(x=Year, y=Percent, fill=University)) 
plot + geom_bar(stat="identity") + 
  scale_fill_manual(guide = guide_legend(reverse=TRUE), values=cbPalette) +
  theme(text=element_text(size=14)) +
  scale_y_continuous(name="NSF-GRFP Awards (Percent of Total)", expand = c(0,0), limits = c(0,18))

#publish to plotly
py <- plotly()
py$ggplotly()

############################################
####Analysis of awards by Field of Study####
############################################

#making a new dataset to play with Field
field=UC 
#combine subfields into main fields using grep
field$Field.of.Study[grepl("Chemistry",field$Field.of.Study,ignore.case=T)]<-"Chemistry"
field$Field.of.Study[grepl("Comp/IS/Eng",field$Field.of.Study,ignore.case=T)]<-"Comp/IS/Eng"
field$Field.of.Study[grepl("Engineering - Bioengineering and Biomedical",field$Field.of.Study,ignore.case=T)]<-"Biomedical Engineering"
field$Field.of.Study[grepl("Engineering - ",field$Field.of.Study,ignore.case=T)]<-"Engineering - Other"
field$Field.of.Study[grepl("Geosciences",field$Field.of.Study,ignore.case=T)]<-"Geosciences"
field$Field.of.Study[grepl("Life Sciences",field$Field.of.Study,ignore.case=T)]<-"Life Sciences"
field$Field.of.Study[grepl("Mathematical Sciences",field$Field.of.Study,ignore.case=T)]<-"Math"
field$Field.of.Study[grepl("Physics and Astronomy",field$Field.of.Study,ignore.case=T)]<-"Physics"
field$Field.of.Study[grepl("Psychology",field$Field.of.Study,ignore.case=T)]<-"Psychology"
field$Field.of.Study[grepl("Social Sciences",field$Field.of.Study,ignore.case=T)]<-"Social Sciences"
field$Field.of.Study[grepl("Materials Research",field$Field.of.Study,ignore.case=T)]<-"Materials Research"
field$Field.of.Study[grepl("STEM",field$Field.of.Study,ignore.case=T)]<-"STEM Education"

#examine data
as.data.frame(table(field$Field.of.Study))

#still a couple of weird ones - from the 2014 of course
#1                 East Bay    1
#2                     Inc.    1
#3   PRESIDENT & FELLOWS OF    3
#4                San Diego    1

> nsf5yr[nsf5yr$Field.of.Study==" East Bay",]
#Name Email.Address   Baccalaureate.Institution Field.of.Study
#10939 Pyatt  Julie Alanna          <NA> California State University       East Bay
#Proposed.Graduate.Institution
#10939                          <NA>
#  Current.Institution year
#10939 Social Sciences - Cultural AnthropologyUNIVERSITY OF CALIFORNIA BERKELEY 2014

#went back to the original data to figure out what the field was supposed to be for these few rows
#replace weird values with real field
field$Field.of.Study[grepl("East Bay",field$Field.of.Study,ignore.case=T)]<-"Social Sciences"
field$Field.of.Study[grepl("Inc",field$Field.of.Study,ignore.case=T)]<-"Engineering - Other"
field$Field.of.Study[grepl("PRESIDENT & FELLOWS OF",field$Field.of.Study,ignore.case=T)]<-"Social Sciences"
field$Field.of.Study[grepl("San Diego",field$Field.of.Study,ignore.case=T)]<-"Engineering - Other"

#check results
f=as.data.frame(table(field$Field.of.Study))
sum(f$Freq)

#make frequency table for graphing
f=as.data.frame(table(field$Current.Institution,field$Field.of.Study,field$year))
names(f)=c("University","Field","Year","Freq")
f$University=gsub("University of California-","",f$University)

#get data from Davis only
f_davis=f[f$University=="Davis",]

#plot Davis data with Fields as different lines
plot=ggplot(data=f_davis, aes(x=Year, y=Freq, group=Field, color=Field)) 
plot + geom_line()

#pull out life science data
lifesci=f[f$Field=="Life Sciences",]

#look at life science data by university
plot=ggplot(data=lifesci, aes(x=Year, y=Freq, group=University, color=University)) 
plot + geom_line()