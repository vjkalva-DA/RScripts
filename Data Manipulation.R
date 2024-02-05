df <- read.csv("houses.csv")
df <- read.csv("dm.csv")
df[1,3]
df[c(1,2,500),c(2,6,7)]
df[c(1:5), "Gender"]
head(df$Gender)
own <- df[df$OwnHome=="Own",]
rent <- df[df$OwnHome=="Rent",]
history_H_L <- df[(df$History=="High"|df$History=="Low"),]
sum(table(df$History))
colSums(is.na(df))
x <- 1000-212
male_high <- df[(df$Gender=="Male"&df$History=="High"),]
table(df$Gender)
494-315
df$History <- ifelse(is.na(df$History),"Missing",as.character(df$History))
df$History <- as.factor(df$History)

index <- which(df$History=="High")
high <- df[index,]
sales <- c(255,600,500,400,500,700,800,NA,200,NA,500)
s600 <- sales[sales>=600]
s6001 <- sales[which(sales>=600)] 
hist_gender <- df[,c("Gender", "History")]
x <- df[df$History=="High"|df$History=="Low",c("Gender", "Age")]
df$logINC <- log(df$AmountSpent)
#ORDER
amountspent <- df[order(-df$AmountSpent),]
max(df$AmountSpent)

# Aggreagte variable to be summarized, by categorical cariable then mean, median or mode
aggregate(df$AmountSpent, by=list(df$History),mean)
library(dplyr)
group_by(df$History)%>%summarise(mean)
df%>%group_by(History)%>%summarise(mean=mean(AmountSpent))

class(aggregate(df$AmountSpent, by=list(df$History),mean))
class(df%>%group_by(History)%>%summarise(mean=mean(AmountSpent)))
tapply(df$AmountSpent, df$History,mean)
class(tapply(df$AmountSpent, df$History,mean))

# contigency table

table(df$Gender,df$History)
class(table(df$Gender,df$History))
xtabs(df$AmountSpent~df$Gender+df$OwnHome)

# filter

male <- filter(df,Gender=="Male"&Married=="Single"&OwnHome=="Own"&Location=="Close"& between(Salary,45000,55000))%>%arrange(desc(Salary))

# Select

G_A_M <- select(df,Age,Gender,Married)
otherthan_GAM <- select(df,-Age,-Gender,-Married)

# Mutate
df <- mutate(df,logincome=log(AmountSpent))
library(dplyr)
# Arrange

data <- arrange(df,desc(AmountSpent))

# groupby

gr <- group_by(df, History)
summarise(gr,mean(AmountSpent), sd(AmountSpent))

# functional pipelines %>%

# find the mean amount spent of people whose age is >=28

am28 <- df%>%filter(Age=="Young")%>%summarise(Mean=mean(AmountSpent))

salary_desc <- arrange(df,amountspent)
salary <- df[order(-df$Salary),]
max(df$Salary)
least_Saalry <- df[order(df$Salary),]
head(least_Saalry)
min(df$Salary)

# Manipulating date objects

fda <- read.csv("fda.csv")
sapply(fda,class)
fda$issued <- as.Date(fda$issued)
head(fda$issued)
str(fda$issued)
head(months(fda$issued))
unique(months(fda$issued))
unique(weekdays(fda$issued))
fda$issued[2]-fda$issued[3]

difftime(fda$issued[1],fda$issued[2],units = "days")
difftime(fda$issued[3],fda$issued[4], units = "weeks")

day1 <- fda%>%filter(weekdays(fda$issued)=="Sunday")
day2 <- fda%>%filter(weekdays(fda$issued)=="Monday")
day3 <- fda%>%filter(weekdays(fda$issued)=="Tuesday")
day4 <- fda%>%filter(weekdays(fda$issued)=="Wednesday")
day5 <- fda%>%filter(weekdays(fda$issued)=="Thursday")
day6 <- fda%>%filter(weekdays(fda$issued)=="Friday")
day7 <- fda%>%filter(weekdays(fda$issued)=="Saturday")
2+46+49+66+51+58
fda_issued <- arrange(fda,fda$issued)

day66 <- fda%>%filter(weekdays(issued)=="Friday"&office=="Center for Drug Evaluation and Research")%>%nrow()
day66
today <- Sys.time()
class(today)
weekdays(today)
months(today)

library(lubridate)

str(fda)
fda$issued <- ymd(fda$issued)
str(fda$issued)

# Merging Tables

pf <- read.csv("pfizer.csv")
sapply(pf,class)

colnames(fda)[2] <- "first_name"
colnames(fda)[1] <- "last_name"

person_letter_issued <- merge(x=pf, y=fda, by="last_name", all.y = TRUE)


df1 <- data.frame(cust_id=c(1,2,3,4,5,6), product=c("Icecream", "Snakes", "Chicken", "Cookies", "Levista", "continental"))
df2 <- data.frame(cust_id=c(2,4,6), state=c("NY", "IL","FL"))

df_inner <- merge(df1,df2,"cust_id")
df_outer <- merge(df1,df2,"cust_id", all=TRUE)
df_left <- merge(df1,df2,"cust_id", all.x = TRUE)
df_right <- merge(df1,df2,"cust_id", all.y = TRUE)


# Missing value treatment

air <- airquality
head(air)
colSums(is.na(air))
dim(air)
summary(air)
air$Ozone[is.na(air$Ozone)] <- 45
summary(air)

air$Solar.R[is.na(air$Solar.R)] <- mean(air$Solar.R,na.rm = TRUE)
summary(air)

# Transporting data using reshape2()
library(reshape2)
wide <- data.frame(person=c("Shankar","Ajay","Ramesh"), age=c(26,27,29), weight=c(65,78,69))
long <- melt(wide,id.vars = "person", value.name = "Age_Weight")
dcast(long, person~variable,value.var = "Age_Weight")

# Manipulating character strings and using sqldf

a <- "Batman"
substr(a,start = 2,stop = 6)
nchar(a)
toupper(a)
tolower(a)
substr(wide$person,start = 2, stop = 5)
b <- "Bat-Man"
strsplit(b,split = "-")
c <- "Bat/Man"
strsplit(c,split = "/")

grepl("a",c)
b
sub("-","/",b)
d <- "Ba-t-Man"
sub("-","/",d)
gsub("-","/",d)


# SQL commands

library(sqldf)
