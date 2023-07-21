library(deaR)
library(readxl)


Bank_Data_2020 <- read_excel("D:\\SEM 4\\MGT1042-IAAPM PROJECT\\Bankdata2020.xlsx")

View(Bank_Data_2020)

Bank_Data_2020
##Converting cells if the value is negative or zero because negative and zero value can not be considered in DEA
Bank_Data_2020$Deposits[Bank_Data_2020$Deposits == "-"] <- 0
Bank_Data_2020$Borrowings[Bank_Data_2020$Borrowings == "-"] <- 0
Bank_Data_2020$Investments[Bank_Data_2020$Investments == "-"] <- 0

##Converting character such as coma into numeric (1,000 to 1000)
Bank_Data_2020$Deposits <- as.numeric(Bank_Data_2020$Deposits)  
Bank_Data_2020$Capital <- as.numeric(Bank_Data_2020$Capital)  
Bank_Data_2020$`Reserves and Surplus` <- as.numeric(Bank_Data_2020$`Reserves and Surplus`)  
Bank_Data_2020$Borrowings <- as.numeric(Bank_Data_2020$Borrowings)  
Bank_Data_2020$Investments <- as.numeric(Bank_Data_2020$Investments)

#view Bank_Data_2020
View(Bank_Data_2020)
means = c(mean(Bank_Data_2020$Capital, na.rm = TRUE),mean(Bank_Data_2020$`Reserves and Surplus`, na.rm = TRUE),mean(Bank_Data_2020$Deposits, na.rm = TRUE),mean(Bank_Data_2020$Borrowings, na.rm = TRUE),mean(Bank_Data_2020$Investments, na.rm = TRUE))

#changing of negative values to the mean value of the column 
for(j in 1:nrow(Bank_Data_2020))
{
  for(k in 2:ncol(Bank_Data_2020))
  {
    if(Bank_Data_2020[j,k] <= 0){
      Bank_Data_2020[j,k]  <- Bank_Data_2020[j,k] + means[k-2]
    }
  }
}


View(Bank_Data_2020)
Bank_Data_2020

# Define the input variables in the DEA model(first column will be treated as DMU by default, ni is no. of input and no is no. of output)
data_input = read_data(Bank_Data_2020,ni = 4,no = 1)
data_input

#Final model
res<-model_basic(data_input,dmu_eval = 1:97,dmu_ref = 1:97,orientation = "io",rts="crs")
res


efficiencies(res)
summary(res)
plot(res)

