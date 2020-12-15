setwd("C:/Users/QJ/Desktop/FYP/Data/BTC")
library(dplyr)
data1 = read.csv("11.8.1.csv", header = FALSE)
data2 = read.csv("11.8.2.csv", header = FALSE)

#########Change

######### Things to change : 1. Days, hours, minutes to expiry
#: 2. risk free rates
#: 3. check the underyling prices for safety measure
# 4. file name ofc


txt1 = as.character(data1[1,1])
txt1 = strsplit(txt1, "")[[1]]

txt2 = as.character(data2[1,1])
txt2 = strsplit(txt2, "")[[1]]

get_underlying <- function(text){
  output = ""
  bool = FALSE
  
  for( i in 1: length(text)){
    if(text[i] == ")"){
      bool = FALSE
    }
    if(bool){
      output = paste(output, text[i], sep = "")
    }
    if(text[i] == "$"){
      bool = TRUE
    }
  }
  output
}

# get_days <- function(text){
#   #text = strsplit(text, "")[[1]]
#   text = text[ (length(text) - 30) : length(text) ]
#   
#   for( i in 1:length(text)){
#     if ( text[i] == "n"){
#       output = paste(text[i+2], text[i+3], sep = "")
#       print(i)
#       break
#     }
#   }
#   output
#   
# }



u1 = as.numeric(get_underlying(txt1)) #underlying price
u2 = as.numeric(get_underlying(txt2))

get_minutes<- function(days,hours,minutes){
  (days-1)*24*60 + hours*60 + minutes
}

# x1 = as.numeric(get_days(txt1))


x1 = 18
y1 = 46
x2 = 3
x3 = 29
M_other_days1 = get_minutes(x1,x2,x3)
M_other_days2 = get_minutes(y1,x2,x3)

#Assuming calculation is done on 9:46am of the observed time in the exchange, then M_current_day = 854 (same as example in the CBOE sheet)
#Since the options listed on Deribit expire at 08:00 UTC, M_settlement_day = 8*60 = 480
#
T1 = (854 + 480 + M_other_days1)/525600
T2 = (854 + 480 + M_other_days2)/525600

########################Change
R1 = x1/365 * 2.05
R2 = y1/365 * 2.06
# taken from https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=billrates as of July

data1 = data1[3:nrow(data1),]
data2 = data2[3:nrow(data2),]

names(data1) = c("Last_Call", "Bid_Call" , "Ask_Call" , "Strike" , "Last_Put", "Bid_Put", "Ask_Put")
names(data2) = c("Last_Call", "Bid_Call" , "Ask_Call" , "Strike" , "Last_Put", "Bid_Put", "Ask_Put")

tidyFactors <- function(some_data,u){
  some_data = as.numeric(substring(some_data,1,6))*u
}

data1$Bid_Call = tidyFactors(data1$Bid_Call,u1)
data1$Ask_Call = tidyFactors(data1$Ask_Call,u1)
data1$Bid_Put = tidyFactors(data1$Bid_Put,u1)
data1$Ask_Put = tidyFactors(data1$Ask_Put,u1)

data2$Bid_Call = tidyFactors(data2$Bid_Call,u2)
data2$Ask_Call = tidyFactors(data2$Ask_Call,u2)
data2$Bid_Put = tidyFactors(data2$Bid_Put,u2)
data2$Ask_Put = tidyFactors(data2$Ask_Put,u2)