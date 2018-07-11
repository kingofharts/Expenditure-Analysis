## Daily Spend Box Plots
## 4 April 2018
## Nicholas Paul Hartman Ponce, for same

DSD <- read.delim("~/Test/Test/Daily_Spend_Data.txt", row.names=1)

View(DSD)

boxplot(NetSpend ~ DoW.M.1, data = DSD, xlab = "Day of Week", ylab = "Spend")
