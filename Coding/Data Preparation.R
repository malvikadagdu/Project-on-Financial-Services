getwd()
setwd("/Users/malvikadagdu/Desktop/R Programming/Dataset for sec 2")
getwd()

#Basic Import fin <- read.csv("Future 500.csv")

fin <- read.csv("Future 500.csv", na.strings = "")
fin
head(fin)
tail(fin,10)
str(fin)
summary(fin)

#Changing from non-factor to factor

fin$ID <-factor(fin$ID)
summary(fin)
str(fin)

fin$Inception <- factor(fin$Inception)
summary(fin)
str(fin)

#Factor Variable Trap (FVT)
#For Numeric into characters
a <- c("12","13","14","12","12")
a
typeof(a)
b <- as.numeric(a)
b
typeof(b)

#For Numeric into Factors

z <- factor(c("12","13","14","12","12"))
z
typeof(z)
y <- as.numeric(z)
y
typeof(y)

x <- as.numeric(as.character(z))
x
typeof(x)


#FVT Example

head(fin)
str(fin)
#fin$Profit <- factor(fin$Profit)
head(fin)
str(fin)

?sub
fin$Expenses <- gsub(" Dollars","",fin$Expenses)
fin$Expenses <- gsub(",","",fin$Expenses)
head(fin)
str(fin)


fin$Revenue <- gsub("\\$","",fin$Revenue)
fin$Revenue <- gsub(",","",fin$Revenue)
head(fin)
str(fin)

fin$Growth <- gsub("%","",fin$Growth)
head(fin)
str(fin)

fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Growth <- as.numeric(fin$Growth)
str(fin)
summary(fin)

# Locating missing data
# Updated Import :fin <- read.csv("Future 500.csv", na.strings = "") 
head(fin,24)
fin[!complete.cases(fin),]
str(fin)


#Filtering: Using which() for non-missing data

head(fin)
fin[fin$Revenue == 9746272,]
fin[which(fin$Revenue == 9746272),]

head(fin)
fin[fin$Employees == 45,]
fin[which(fin$Employees == 45),]

## Filtering: Using is.na() for missing data
head(fin,24)

is.na()
a <- c(1,24,543,NA,76,45,NA)
is.na(a)

is.na(fin$Expenses)
fin[is.na(fin$Expenses),]

fin[is.na(fin$State),]

#Removing records with missing data

fin_backup <- fin
fin[!complete.cases(fin),]
fin[is.na(fin$Industry),]
fin[!is.na(fin$Industry),]
fin <- fin[!is.na(fin$Industry),]

fin[!complete.cases(fin),]

#Resetting the dataframe index
fin
rownames(fin) <- 1:nrow(fin)
fin

fin
rownames(fin) <- NULL
fin

#Replacing Missing Data: Factual Analysis Method

fin[!complete.cases(fin),]
fin[is.na(fin$State),]
fin[is.na(fin$State) & fin$City == "New York","State"] <- "NY"
#check:
fin[c(11,377),]

fin[is.na(fin$State) & fin$City == "San Francisco","State"] <- "CA"
#check
fin[c(82,265),]

fin[!complete.cases(fin),]

#Replacing Missing Data: Median Imputation

fin[!complete.cases(fin),]

med_empl_retail <- median(fin[fin$Industry == "Retail","Employees"], na.rm = TRUE)
med_empl_retail

fin[is.na(fin$Employees) & fin$Industry=="Retail","Employees"] <- med_empl_retail

#check:

fin[3,]

fin[is.na(fin$Employees),]

med_empl_fin_ind <- median(fin[fin$Industry == "Financial Services","Employees"], na.rm = TRUE)
med_empl_fin_ind

fin[is.na(fin$Employees) & fin$Industry == "Financial Services","Employees"] <- med_empl_fin_ind

fin[330,]

fin[!complete.cases(fin),]


med_growth <- median(fin[fin$Industry == "Construction","Growth"], na.rm = TRUE)

fin[is.na(fin$Growth) & fin$Industry == "Construction", "Growth"] <- med_growth

fin[8,]

fin[!complete.cases(fin),]

med_rev <- median(fin[fin$Industry == "Construction","Revenue"], na.rm = TRUE)

fin[is.na(fin$Revenue) & fin$Industry == "Construction","Revenue"] <- med_rev
fin[c(8,42),]

fin[!complete.cases(fin),]

med_cons <- median(fin[fin$Industry == "Construction","Expenses"], na.rm = TRUE)

fin[is.na(fin$Expenses) & fin$Industry == "Construction", "Expenses"] <- med_cons

fin[is.na(fin$Profit), "Profit"] <- fin[is.na(fin$Profit),"Revenue"] - fin[is.na(fin$Profit),"Expenses"]
fin[c(8,42),]

fin[is.na(fin$Expenses),"Expenses"] <- fin[is.na(fin$Expenses),"Revenue"] - fin[is.na(fin$Expenses),"Profit"]
fin[15,]

fin[!complete.cases(fin),]


#Visualizaton:
install.packages("ggplot2")
library(ggplot2)

# A scatterplot classified by industry showing revenue, expenses. profit
p <- ggplot(data =  fin)
p
p + geom_point(aes(x=Revenue, y =Expenses, colour = Industry, size = Profit))


# A scatterplot classified by industry trends for the expenses ~ revenue relationship

d <- ggplot(data = fin, aes(x = Revenue, y = Expenses, colour = Industry))
d + geom_point() + geom_smooth(fill = NA, size = 1.2)


# A boxplot showing growth by industry
f <- ggplot(data = fin, aes(x = Industry, y = Growth, colour = Industry))
f + geom_boxplot(size = 1)

# Extra:
f + geom_jitter() + geom_boxplot(size = 1, alpha = 0.5, outlier.colour = NA)


