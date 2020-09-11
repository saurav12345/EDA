#Library to be used

library(readxl)
library(stringr)
library(gsubfn)
library(dplyr)
library(xlsx)
library(lubridate)
library(expss)

# set the data dir
getwd()
#setwd("C:/Users/saurav.an.kumar/Documents/Perdiem/meal_vs_perdiem/Raw Data/Apr 20/")

# #import file
# filename <- file.choose()
# sheets <- excel_sheets(filename)
# ##path <- system.file("ME Workings - Oct.xlsx", package = "readxl")
# path <- system.file(filename, package = "readxl")
# data_full <- lapply(excel_sheets(filename), read_excel, path = filename)
# data_files <- data_full[[1]]

data <- read_xlsx(file.choose(),sheet = "Expense Details",col_types = rep("text", 86))

colnames(data) <- gsubfn("\\ ", "_",colnames(data))
colnames(data) <- gsubfn("\\ #", "",colnames(data))
colnames(data) <- gsubfn("\\/", "_",colnames(data))
colnames(data) <- gsubfn("\\?", "",colnames(data))
colnames(data) <- gsubfn("\\_#", "",colnames(data))

#convert to date format
data$Expense_From_On_Date <- as.Date(as.numeric(data$Expense_From_On_Date),
                                     origin="1899/12/30")

data$Expense_To_Date <- as.Date(as.numeric(data$Expense_To_Date),
                                origin="1899/12/30")

data$TR_Period <- as.Date(as.numeric(data$TR_Period),
                          origin="1899/12/30")

data$Processed_Period <- as.Date(as.numeric(data$Processed_Period),
                                 origin = "1899/12/30")


#Business_Rules_Implemetaion
data_files <- filter(data, Charge_Type != "PPA")
data_files <- distinct(data_files,Expense_ID,.keep_all = TRUE)
meals_entertainment <- filter(data_files, Expense_Type == "Meals and Entertainment")
per_diem <- filter(data_files,Expense_Type =="Per Diem - International" | Expense_Type =="Per Diem - Local")
per_diem$Total_Employee_Home_Expense_Amount <- round(as.numeric(per_diem$Total_Employee_Home_Expense_Amount),digit = 2)
per_diem$Total_Employee_Home_Expense_Amount_2 <- paste(per_diem$Total_Employee_Home_Expense_Amount,per_diem$Reimbursable_Currency)
#meals_entertainment<-meals_entertainment[,c(1,3,4,7,10,13,15,19,20,22:26,34,70:72,85)]
meals_entertainment_may<-meals_entertainment[,c(1,3,4,7,10,13,15,19,20,22:26,35,68:70,83)]

#per_diem <- per_diem[,c(1,3,4,7,10,13,15,19,20,22:26,34,70:72,85)]
per_diem_may <- per_diem[,c(1,3,4,7,10,13,15,19,20,22:26,35,68:70,83,87)]

meals_entertainment_may$Concatenate <- paste(meals_entertainment_may$Personnel_ID,
                                             meals_entertainment_may$Expense_From_On_Date, sep = " ")

regexp <- "[[:digit:]]+"
#str_extract(per_diem_may$, regexp)

per_diem_may_1 <- filter(per_diem_may,
                         str_extract(per_diem_may$Country_Specific_Information_2, regexp) == 0 &
                           str_extract(per_diem_may$Country_Specific_Information_3, regexp) == 0)
per_diem_may_2 <- filter(per_diem_may_1,
                         per_diem_may_1$Country_Specific_Information_16 != "AssignmentType - oneDay" |
                           is.na(per_diem_may_1$Country_Specific_Information_16))

per_diem_may_3 <- filter(per_diem_may_2,per_diem_may_2$Total_Employee_Home_Expense_Amount_2 != "12 EUR" &
                           per_diem_may_2$Total_Employee_Home_Expense_Amount_2 != "22 CHF" )

per_diem_may_3 <- filter(per_diem_may_3,as.numeric(per_diem_may_3$Number_of_Days) <= 16.00)


# re - assigning file
pp <- per_diem_may_3
mm <- meals_entertainment_may

# Adding 16 days from Expense_from_on_Date
pp$day_1 <- as.Date(pp$Expense_From_On_Date,"%d/%m/%Y")
pp$day_2 <- as.Date(pp$day_1 + days(1))
pp$day_3 <- as.Date(pp$day_2 + days(1))
pp$day_4 <- as.Date(pp$day_3 + days(1))
pp$day_5 <- as.Date(pp$day_4 + days(1))
pp$day_6 <- as.Date(pp$day_5 + days(1))
pp$day_7 <- as.Date(pp$day_6 + days(1))
pp$day_8 <- as.Date(pp$day_7 + days(1))
pp$day_9 <- as.Date(pp$day_8 + days(1))
pp$day_10 <- as.Date(pp$day_9 + days(1))
pp$day_11 <- as.Date(pp$day_10 + days(1))
pp$day_12 <- as.Date(pp$day_11 + days(1))
pp$day_13 <- as.Date(pp$day_12 + days(1))
pp$day_14 <- as.Date(pp$day_13 + days(1))
pp$day_15 <- as.Date(pp$day_14 + days(1))
pp$day_16 <- as.Date(pp$day_15 + days(1))


# condetioning the data
pp$day_1 <- ifelse(pp$Expense_To_Date == pp$day_1 | pp$Expense_To_Date > pp$day_1, as.Date(pp$day_1), NA)
pp$day_2 <- ifelse(pp$Expense_To_Date == pp$day_2 | pp$Expense_To_Date > pp$day_2, pp$day_2, NA)
pp$day_3 <- ifelse(pp$Expense_To_Date == pp$day_3 | pp$Expense_To_Date > pp$day_3, pp$day_3, NA)
pp$day_4 <- ifelse(pp$Expense_To_Date == pp$day_4 | pp$Expense_To_Date > pp$day_4, pp$day_4, NA)
pp$day_5 <- ifelse(pp$Expense_To_Date == pp$day_5 | pp$Expense_To_Date > pp$day_5, pp$day_5, NA)
pp$day_6 <- ifelse(pp$Expense_To_Date == pp$day_6 | pp$Expense_To_Date > pp$day_6, pp$day_6, NA)
pp$day_7 <- ifelse(pp$Expense_To_Date == pp$day_7 | pp$Expense_To_Date > pp$day_7, pp$day_7, NA)
pp$day_8 <- ifelse(pp$Expense_To_Date == pp$day_8 | pp$Expense_To_Date > pp$day_8, pp$day_8, NA)
pp$day_9 <- ifelse(pp$Expense_To_Date == pp$day_9 | pp$Expense_To_Date > pp$day_9, pp$day_9, NA)
pp$day_10 <- ifelse(pp$Expense_To_Date == pp$day_10 | pp$Expense_To_Date > pp$day_10, pp$day_10, NA)
pp$day_11 <- ifelse(pp$Expense_To_Date == pp$day_11 | pp$Expense_To_Date > pp$day_11, pp$day_11, NA)
pp$day_12 <- ifelse(pp$Expense_To_Date == pp$day_12 | pp$Expense_To_Date > pp$day_12, pp$day_12, NA)
pp$day_13 <- ifelse(pp$Expense_To_Date == pp$day_13 | pp$Expense_To_Date > pp$day_13, pp$day_13, NA)
pp$day_14 <- ifelse(pp$Expense_To_Date == pp$day_14 | pp$Expense_To_Date > pp$day_14, pp$day_14, NA)
pp$day_15 <- ifelse(pp$Expense_To_Date == pp$day_15 | pp$Expense_To_Date > pp$day_15, pp$day_15, NA)
pp$day_16 <- ifelse(pp$Expense_To_Date == pp$day_16 | pp$Expense_To_Date > pp$day_16, pp$day_16, NA)



pp$Expense_From_On_Date <- as.Date(pp$Expense_From_On_Date,"%d/%m/%Y")

pp$Expense_To_Date <- as.Date(pp$Expense_To_Date,"%d/%m/%Y")




# converting to date format from number  format.
pp$day_1 <- as.Date.numeric(pp$day_1, is.na=TRUE, origin = origin)
pp$day_2 <- as.Date.numeric(pp$day_2, is.na=TRUE, origin = origin)
pp$day_3 <- as.Date.numeric(pp$day_3, is.na=TRUE, origin = origin)
pp$day_4 <- as.Date.numeric(pp$day_4, is.na=TRUE, origin = origin)
pp$day_5 <- as.Date.numeric(pp$day_5, is.na=TRUE, origin = origin)
pp$day_6 <- as.Date.numeric(pp$day_6, is.na=TRUE, origin = origin)
pp$day_7 <- as.Date.numeric(pp$day_7, is.na=TRUE, origin = origin)
pp$day_8 <- as.Date.numeric(pp$day_8, is.na=TRUE, origin = origin)
pp$day_9 <- as.Date.numeric(pp$day_9, is.na=TRUE, origin = origin)
pp$day_10 <- as.Date.numeric(pp$day_10, is.na=TRUE, origin = origin)
pp$day_11 <- as.Date.numeric(pp$day_11, is.na=TRUE, origin = origin)
pp$day_12 <- as.Date.numeric(pp$day_12, is.na=TRUE, origin = origin)
pp$day_13 <- as.Date.numeric(pp$day_13, is.na=TRUE, origin = origin)
pp$day_14 <- as.Date.numeric(pp$day_14, is.na=TRUE, origin = origin)
pp$day_15 <- as.Date.numeric(pp$day_15, is.na=TRUE, origin = origin)
pp$day_16 <- as.Date.numeric(pp$day_16, is.na=TRUE, origin = origin)


# concatinate the id's with date
pp$Concatenate_1 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_1 != is.na(pp$day_1), paste(pp$Personnel_ID,pp$day_1, sep=" "), NA)
pp$Concatenate_2 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_2 != is.na(pp$day_2), paste(pp$Personnel_ID,pp$day_2, sep=" "), NA)
pp$Concatenate_3 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_3 != is.na(pp$day_3), paste(pp$Personnel_ID,pp$day_3, sep=" "), NA)
pp$Concatenate_4 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_4 != is.na(pp$day_4), paste(pp$Personnel_ID,pp$day_4, sep=" "), NA)
pp$Concatenate_5 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_5 != is.na(pp$day_5), paste(pp$Personnel_ID,pp$day_5, sep=" "), NA)
pp$Concatenate_6 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_6 != is.na(pp$day_6), paste(pp$Personnel_ID,pp$day_6, sep=" "), NA)
pp$Concatenate_7 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_7 != is.na(pp$day_7), paste(pp$Personnel_ID,pp$day_7, sep=" "), NA)
pp$Concatenate_8 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_8 != is.na(pp$day_8), paste(pp$Personnel_ID,pp$day_8, sep=" "), NA)
pp$Concatenate_9 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_9 != is.na(pp$day_9), paste(pp$Personnel_ID,pp$day_9, sep=" "), NA)
pp$Concatenate_10 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_10 != is.na(pp$day_10), paste(pp$Personnel_ID,pp$day_10, sep=" "), NA)
pp$Concatenate_11 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_11 != is.na(pp$day_11), paste(pp$Personnel_ID,pp$day_11, sep=" "), NA)
pp$Concatenate_12 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_12 != is.na(pp$day_12), paste(pp$Personnel_ID,pp$day_12, sep=" "), NA)
pp$Concatenate_13 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_13 != is.na(pp$day_13), paste(pp$Personnel_ID,pp$day_13, sep=" "), NA)
pp$Concatenate_14 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_14 != is.na(pp$day_14), paste(pp$Personnel_ID,pp$day_14, sep=" "), NA)
pp$Concatenate_15 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_15 != is.na(pp$day_15), paste(pp$Personnel_ID,pp$day_15, sep=" "), NA)
pp$Concatenate_16 <- ifelse(pp$Personnel_ID != is.na(pp$Personnel_ID) & pp$day_16 != is.na(pp$day_16), paste(pp$Personnel_ID,pp$day_16, sep=" "), NA)

#Lookup with Concatenate values
pp$Vlookup_1  <- vlookup(pp$Concatenate_1,  mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_2  <- vlookup(pp$Concatenate_2,  mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_3  <- vlookup(pp$Concatenate_3,  mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_4  <- vlookup(pp$Concatenate_4,  mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_5  <- vlookup(pp$Concatenate_5,  mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_6  <- vlookup(pp$Concatenate_6,  mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_7  <- vlookup(pp$Concatenate_7,  mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_8  <- vlookup(pp$Concatenate_8,  mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_9  <- vlookup(pp$Concatenate_9,  mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_10 <- vlookup(pp$Concatenate_10, mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_11 <- vlookup(pp$Concatenate_11, mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_12 <- vlookup(pp$Concatenate_12, mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_13 <- vlookup(pp$Concatenate_13, mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_14 <- vlookup(pp$Concatenate_14, mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_15 <- vlookup(pp$Concatenate_15, mm, result_column = "Concatenate", lookup_column = "Concatenate")
pp$Vlookup_16 <- vlookup(pp$Concatenate_16, mm, result_column = "Concatenate", lookup_column = "Concatenate")


# same as above for mm file.
mm$Vlookup_1  <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_1",  lookup_column = "Concatenate_1")
mm$Vlookup_2  <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_2",  lookup_column = "Concatenate_2")
mm$Vlookup_3  <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_3",  lookup_column = "Concatenate_3")
mm$Vlookup_4  <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_4",  lookup_column = "Concatenate_4")
mm$Vlookup_5  <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_5",  lookup_column = "Concatenate_5")
mm$Vlookup_6  <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_6",  lookup_column = "Concatenate_6")
mm$Vlookup_7  <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_7",  lookup_column = "Concatenate_7")
mm$Vlookup_8  <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_8",  lookup_column = "Concatenate_8")
mm$Vlookup_9  <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_9",  lookup_column = "Concatenate_9")
mm$Vlookup_10 <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_10", lookup_column = "Concatenate_10")
mm$Vlookup_11 <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_11", lookup_column = "Concatenate_11")
mm$Vlookup_12 <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_12", lookup_column = "Concatenate_12")
mm$Vlookup_13 <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_13", lookup_column = "Concatenate_13")
mm$Vlookup_14 <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_14", lookup_column = "Concatenate_14")
mm$Vlookup_15 <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_15", lookup_column = "Concatenate_15")
mm$Vlookup_16 <- vlookup(mm$Concatenate,  pp, result_column = "Concatenate_16", lookup_column = "Concatenate_16")

# converting into dataframe
pp =as.data.frame(pp)
mm =as.data.frame(mm)

#Count the number of Lookups
pp$Count <- rowSums(!is.na(pp[ , c("Vlookup_1", "Vlookup_2", "Vlookup_3", "Vlookup_4", "Vlookup_5", "Vlookup_6", "Vlookup_7", "Vlookup_8", "Vlookup_9", "Vlookup_10", "Vlookup_11", "Vlookup_12", "Vlookup_13", "Vlookup_14", "Vlookup_15", "Vlookup_16")]))
mm$Count <- rowSums(!is.na(mm[ , c("Vlookup_1", "Vlookup_2", "Vlookup_3", "Vlookup_4", "Vlookup_5", "Vlookup_6", "Vlookup_7", "Vlookup_8", "Vlookup_9", "Vlookup_10", "Vlookup_11", "Vlookup_12", "Vlookup_13", "Vlookup_14", "Vlookup_15", "Vlookup_16")]))

# againg renaming the data sets
a <- pp
b <- mm

# filter the data (>0)
a <- subset(a, a$Count > 0)
b <- subset(b, b$Count > 0)

# select new columns
a <- a[ , c("Enterprise_ID", "First_Name", "Personnel_ID", "Employee_Country", "TR_Period","Expense_ID","Expense_Type", "Expense_From_On_Date", "Expense_To_Date", "Total_Employee_Home_Expense_Amount")]
b <- b[ , c("Enterprise_ID", "First_Name", "Personnel_ID", "Employee_Country", "TR_Period","Expense_ID","Expense_Type", "Expense_From_On_Date", "Expense_To_Date", "Total_Employee_Home_Expense_Amount")]

#Joing the tables
Meals_Vs_Perdiem <- as.data.frame(rbind(a,b))

#Rename the file
Meals <- mm
PerDiem <- pp

#Keep only the required files
rm(data,data_files,per_diem,per_diem_may,per_diem_may_1,per_diem_may_2,per_diem_may_3,mm,pp,meals_entertainment)

# saving the final data
write.csv(Meals, "meals.csv",row.names = F)
write.csv(PerDiem, "PerDiem.csv",row.names = F)
Meals_Vs_Perdiem$Expense_To_Date <- ifelse(is.na(Meals_Vs_Perdiem$Expense_To_Date),str_replace_na(Meals_Vs_Perdiem$Expense_To_Date),format(Meals_Vs_Perdiem$Expense_To_Date,format = "%d/%m/%Y"))
Meals_Vs_Perdiem$Total_Employee_Home_Expense_Amount <- str_replace_na(Meals_Vs_Perdiem$Total_Employee_Home_Expense_Amount)
Meals_Vs_Perdiem$Expense_From_On_Date <- format(Meals_Vs_Perdiem$Expense_From_On_Date,format = "%d/%m/%Y")
Meals_Vs_Perdiem$TR_Period <- format(Meals_Vs_Perdiem$TR_Period,format = "%d/%m/%Y")

Meals_Vs_Perdiem$Concatenate <- ifelse(Meals_Vs_Perdiem$Expense_Type == "Per Diem - Local",
                                       str_c(Meals_Vs_Perdiem$Expense_Type,format(Meals_Vs_Perdiem$Expense_From_On_Date,format = "%d/%m/%Y"),sep = "                      "),
                                       ifelse(Meals_Vs_Perdiem$Expense_Type == "Per Diem - International",
                                              str_c(Meals_Vs_Perdiem$Expense_Type,format(Meals_Vs_Perdiem$Expense_From_On_Date,format = "%d/%m/%Y"),sep = "     "),
                                              str_c(Meals_Vs_Perdiem$Expense_Type,format(Meals_Vs_Perdiem$Expense_From_On_Date,format = "%d/%m/%Y"),sep = "   ")))


Meals_Vs_Perdiem$Concatenate_2 <- ifelse(Meals_Vs_Perdiem$Expense_To_Date == "NA",
                                         str_c(Meals_Vs_Perdiem$Expense_To_Date,
                                               round(as.numeric(Meals_Vs_Perdiem$Total_Employee_Home_Expense_Amount),digit = 2),sep = "                   "),
                                         str_c(format(Meals_Vs_Perdiem$Expense_To_Date,format = "%d/%m/%Y"),
                                               round(as.numeric(Meals_Vs_Perdiem$Total_Employee_Home_Expense_Amount),digit = 2),sep = "   "))


Meals_Vs_Perdiem$Concatenate <- str_c(Meals_Vs_Perdiem$Concatenate,Meals_Vs_Perdiem$Concatenate_2,sep = "   ")

Meals_Vs_Perdiem$Concatenate <- str_c(format(Meals_Vs_Perdiem$TR_Period,format = "%d/%m/%Y"),Meals_Vs_Perdiem$Concatenate,sep = "   ")

Meals_Vs_Perdiem <- Meals_Vs_Perdiem[ , c("Enterprise_ID", "First_Name", "Personnel_ID", "Employee_Country", "TR_Period","Expense_ID","Expense_Type",
                                          "Expense_From_On_Date", "Expense_To_Date", "Total_Employee_Home_Expense_Amount","Concatenate")]


Meals_Vs_Perdiem$Concatenate <- gsubfn("NA","       ", Meals_Vs_Perdiem$Concatenate)
Meals_Vs_Perdiem$Expense_To_Date <- gsubfn("NA"," ", Meals_Vs_Perdiem$Expense_To_Date)

write.csv(Meals_Vs_Perdiem,"Meals_Vs_Perdiem.csv",row.names = F)