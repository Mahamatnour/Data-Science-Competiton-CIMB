if (!file.exists("data")){
  dir.create("data")
}
#DATA SOURCES: GETTING DATA FROM DATABASES, WEB, XML AND JSON IN R

##read sas
install.packages("sas7bdat") 
library(sas7bdat)

# download.file('https://github.com/ramnathv/RDataPkg/blob/master/sas/airline.sas7bdat?raw=true', method='auto',destfile = 'data/airline.sas7bdat')
read.sas7bdat('data/airline.sas7bdat')

##read xl
install.packages('readxl')
library(readxl)
excel1 <- readxl_example('clippy.xls')
read_excel(excel1)
xlsx_example <- readxl_example('datasets.xlsx')
read_excel(xlsx_example)

excel_sheets(xlsx_example)
read_excel(xlsx_example, sheet = 'chickwts')

read_excel(xlsx_example, sheet = 2)

read_excel(xlsx_example, n_max = 3)
class(xlsx_example)

read_excel(xlsx_example,  range = cell_rows(1:4))
read_excel(xlsx_example,  range = cell_cols(1:4))
read_excel(xlsx_example,  range = cell_cols("B:D"))

## read mysql
install.packages('RMySQL')
library(RMySQL)
ucscDb<-dbConnect(MySQL(),user="genome", host="genome-mysql.soe.ucsc.edu")

#show all databases
result <- dbGetQuery(ucscDb,'show databases')

#connect to hg18 database
hg18 <- dbConnect(MySQL(),db='hg18',user='genome',host="genome-mysql.cse.ucsc.edu")

#list all table in hg18
(allTables <- dbListTables(hg18))
length(allTables)
head(allTables)
#show first 5 tables
allTables[1:5]

#list fields for 'HInvGeneMrna' table - dbListFields(hg18,'HInvGeneMrna')
HInvData <- dbReadTable(hg18,'HInvGeneMrna')
head(HInvData)

#select subset
query <- dbSendQuery(hg18,"Select * from HInvGeneMrna where qNumInsert between 2 and 3")
queryFetch <- fetch(query)
dbDisconnect(hg18) #disconnect after getting the data from server

## Download from quotemedia (finance)
URL1 <- 'https://app.quotemedia.com/quotetools/getHistoryDownload.csv?&webmasterId=501&startDay=02&startMonth=02&startYear=2002&endDay=30&endMonth=07&endYear=2018&isRanged=false&symbol=TSLA'
download.file(URL1, destfile = 'data/tesla.csv')
list.files("./data")

## exercise lapangan terbang
url2 <- 'http://www.data.gov.my/data/ms_MY/dataset/f72882f5-9954-47f1-aa64-4851bd12a69b/resource/29d366b0-bce7-46c6-8c44-74eed2506e94/download/penumpangmengikutlapanganterbangtidaktermasukpenumpangtransit.xlsx'
download.file(url2, destfile = 'data/passenger.xlsx', mode = 'wb')
list.files('./data')

## Reading from XML
install.packages('XML')
library('XML')
doc <- xmlTreeParse('./Data/simple.xml', useInternalNodes = TRUE)
rootNode <- xmlRoot(doc)
rootNode
xmlName(rootNode)
names(rootNode)
rootNode[[1]][[1]] #name of 1st food item
rootNode[[1]][[2]] #price of 1st food item
xmlSApply(rootNode,xmlValue) # Programatically extract parts of the file

xpathApply(rootNode, "//name", xmlValue) #apply to extact all food names

xpathApply(rootNode, "//price", xmlValue) ##apply to extact all food price

xpathApply(rootNode, "//description", xmlValue) ##apply to extact all food description

#extracting data from uitm segamat
con=url("https://johor.uitm.edu.my/v3/index.php/direktorij/pejabat-bendahari")
htmlCode=readLines(con)
close(con)

doc <- htmlTreeParse(htmlCode, useInternalNodes = TRUE)
print(doc)

trim <- function(x) gsub("^\\s+|\\s+$", "", x)
xpath <- "(//*[@id='adminForm']/ul/li[1]/div[1]/a | //*[@id='adminForm']/ul/li[1]/div[2]/text())"

txt <- trim(xpathSApply(doc,xpath, xmlValue))
print(txt)

##Reading JSON
library(jsonlite)
dt <- fromJSON('http://www.data.gov.my/data/ms_MY/dataset/b3a0b3cf-55c0-4926-9110-5c57703997ba/resource/3fa5de2a-4402-45d1-b13b-0f632a27c392/download/jumlah-pekerja-asing-plks-aktif-mengikut-sektor-2011---2016.json')
class(dt)
dt
dt$'2011'
dt['2011'] #get dataframe format
dt[['2011']]
dt[['2011']][2]

#RESHAPING DATA, SUB-SETTING OBSERVATIONS & VARIABLES, SUMMARIZING DATA

# Sub-setting Data
set.seed(1)
x <- data.frame("var1"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11:15))
(x$var2[c(1,3)]=NA)

x[,1]
x[1:2, "var2"]
x[x$var1<=3 & x$var3 >10,]
x[x$var1>2 | x$var3 >10,]

x[is.na(x$var2),] # extract all rows with "NA"
x[x$var2>1,] #Data for var1 and var3 not presented correctly for row with NA
x[which(x$var2>1),] #correct way
x[which.max(x$var1),]#find max for var1

x[which.min(x$var3),]  #look for row with minimum value for var3
sort(x$var1) #To sort values for a particular column
sort(x$var1,decreasing=TRUE) #to sort decreasingly
sort(x$var2,na.last=TRUE ) #What if I have “NA”

x[order(x$var1),]#sort whole dataframe
x[order(x$var1, x$var3),] #To sort data based on multiple columns

# Sorting Data: dplyr package
library(dplyr)
arrange(x, var1)
arrange(x, desc(var1))

#Merging data
set.seed(1)
x<- sample(1:20,10)
y<- sample(30:50,10)
dt.1 <- data.frame(x,y)

set.seed(2)
x<- sample(1:20,10)
y<- sample(30:50,10)
dt.2 <- data.frame(x,y)
dt.1
dt.2
merge(dt.1, dt.2, by="x")
merge(dt.1, dt.2, by="x", all =TRUE)
merge(dt.1, dt.2, by=c("x","y"))

## exercise Genting Malaysia Berhad
# url4 <- 'http://www.google.com/finance/historical?cid=146406397243039&startdate=Aug+4%2C+2012&enddate=Aug+3%2C+2017&num=30&ei=sbqCWaiVOozguQTz3IewAg&output=csv'
url4 <- 'https://app.quotemedia.com/quotetools/getHistoryDownload.csv?&webmasterId=501&startDay=02&startMonth=02&startYear=2002&endDay=30&endMonth=07&endYear=2018&isRanged=false&symbol=GMALF'
download.file(url4, destfile = './data/genting.csv')
genting <- read.csv('./data/genting.csv')

library(dplyr)
genting %>% nrow #nrow(genting)
genting %>% ncol #ncol(genting)
genting %>% names #names(genting)
genting %>% summarize(mean(Close)) #mean(genting$Close)
genting %>% filter(Close>8.35) %>% nrow #nrow(genting[genting$Close>8.35,]) 

#Merging Data: Hands-on
df1<-data.frame(CustomerId = c(1:6),Product = c(rep("Honda", 3),rep("Chevrolet", 3)))
df2<-data.frame(CustomerId = c(2, 4, 7),State =c("Selangor","Sarawak","Kelantan"))

merge(df1, df2, by='CustomerId')
merge(df1, df2, by='CustomerId', all=TRUE)

library(readxl)
URL7<-"http://www.data.gov.my/data/dataset/b5cd948f-cffb-4439-ae08-e508ff073a93/resource/1f2d5629-ac8d-449a-a4c1-9d269d625d84/download/lokalitihotspot2015.xlsx"
download.file(URL7, destfile = "./data/dengue.xlsx",mode='wb')
denggi <- read_excel('./data/dengue.xlsx')
str(denggi)
dt.split.1 <- split(denggi, denggi$Negeri)
dt.split.1

dt.split.2 <- split(denggi$`Jumlah Kes Terkumpul`, denggi$Negeri)
dt.split.2

lapply(dt.split.2, sum)

tapply(denggi$'Jumlah Kes Terkumpul',denggi$'Daerah/Zon/PBT', sum)

names(denggi) <- gsub(".", "", names(denggi),fixed=TRUE)
names(denggi)
names(denggi) <- gsub("/", "", names(denggi),fixed=TRUE)

names(denggi) <- c("Year","Week","State","District","Location","Total","Outbreak Duration")
names(denggi)[2] <- 'Week No'

names(denggi)<-gsub(" ", "_", names(denggi))

denggi$Location <-gsub("Lndah", "Indah",denggi$Location)
denggi$Location <-gsub("Kg", "Kampung",denggi$Location)
denggi$Location <-gsub("[kK]g", "Kampung",denggi$Location)

dt_Taman <- denggi[grepl("Taman", denggi$Location),]
str(dt_Taman)
nrow(dt_Taman)

#String split
strsplit("hello Malaysia !", " ")
paste('Hello,','Good','Morning')

denggi$Location[[10]]
strsplit(as.character(denggi$Location), " ")[[10]]

grep("Taman",denggi$Location, value = TRUE)
class(grep("Taman",denggi$Location))
grep("Kampung",denggi$Location, value = TRUE)

denggi$Location[[3266]]
table(grepl("[Tt]aman",denggi$Location)) #table function to categorise false true

cnt<-table(grepl("Taman",denggi$Location))
barplot(cnt)

nrow(denggi[grepl("[Tt]aman|[Tt][Mm][Nn]", denggi$Location),])
nrow(denggi[grepl("[Tt][Mm][Nn]", denggi$Location),])
dt_taman <- denggi[grepl("[Tt][Mm][Nn]|[Tt]aman", denggi$Location),]
dt_taman <- denggi[grepl("[Tt][Mm][Nn]|[Tt]aman", denggi$Location),]

denggi[grepl("Seksyen|Medan", denggi$Location),]$Location

denggi[grep("^T(.*)1", denggi$Location),"Location"]
denggi[grep(".*([Jj]alan)(.*)(Jaya)", denggi$Location),"Location"]

denggi[grepl("[Tt][Mm][Nn]|[Tt]aman", denggi$Location) & grepl("[Pp]etaling", denggi$District),"Location"]

kg_denggi <- denggi[(grepl("[Kk]g|[Kk]ampung", denggi$Location) & grepl("[Ss]elangor", denggi$State))|(grepl("[Kk]g|[Kk]ampung", denggi$Location) & grepl("[Pp]erak", denggi$State)),]
kg_plot <- table(grepl("[Ss]elangor",kg_denggi$State))
barplot(kg_plot, names.arg=c("Perak","Selangor"), col=c("blue", "red"))

nrow(denggi[grepl("[Kk]g|[Kk]ampung", denggi$Location) & grepl("[Ss]elangor", denggi$State),])
nrow(denggi[grepl("[Kk]g|[Kk]ampung", denggi$Location) & grepl("[Pp]erak", denggi$State),])

#data.table package
install.packages('data.table')
library(data.table)
DF <- data.frame(x=rep(c("a","b","c"),each=3),y=c(1,3,6), v=1:9)

DT <- data.table(x=rep(c("a","b","c"),each=3),y=c(1,3,6), v=1:9)

#check class
class(DT)
class(DF)

#subset row 3:5
DF[3:5,]
DT[3:5]

#Show all the records where x==a
DF[DF$x=='a',]
DT[DT$x=='a']

#Show all the records for row 1,3,7
DF[c(1,3,7),]
DT[c(1,3,7)]

DT[,list(mean(y),sum(v))]
DT[,.(length(x),sum(y))]

#Find the max for column y and mean for column v
DT[,.(max(y),mean(v))]

#Find the square root of column y and store in column w, show all the elements in w
DT$w <- DT[,.(sqrt(y))] #DT[,w:=(y)^.5]
DT$w

#Find the mean of column v and store in column w
DT$w <-DT[,.(mean(v)),] #DT[,w:=mean(v)]
DT$w

DT[,plot(y,v)]
DT[, for(x in 1:10) print(x)]

is.data.table(DT)[,.(hist(y),z:=log(v))]
names(DT)

as.data.table(DT)[,.(hist(y),z=log(v))]

#calculate the mean for column y and
# group the result by even and odd rows
DT[,mean(y),by=v%%2]

library(datasets)
irisDT <- data.table(iris)
class(irisDT)
irisDT

#mean for Sepal.Width and group the results by Species
irisDT[,mean(Sepal.Width),by=Species]

tapply(irisDT$Sepal.Width)
library(dplyr)
irisDT %>% group_by(Species) %>% summarise(mean(Sepal.Width))

# Determine the time to compute mean for Sepal.Length group by Species

fileURL10<-"https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/datasets/iris.csv"
download.file(fileURL10, destfile="./Data/iris.csv")
irisDT <- fread("./Data/iris.csv")
system.time(irisDT[,mean(Sepal.Length),by=Species])

#Find the computation time for the followings:
system.time(mean(irisDT$Sepal.Length,by=DT$Species))
