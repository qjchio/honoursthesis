setwd("C:/Users/QJ/Desktop/FYP/Data/BTC/")
source("main1.r")
files = list.files(path = "C:/Users/QJ/Desktop/FYP/Data/BTC", pattern = "*.csv" , full.names = TRUE , recursive = FALSE)


interpol = function(data, target,f,t){
  for( i in 2:nrow(data)){
    if(data$d2[i-1] < target & data$d2[i] > target){
      eye = i-1
    }
  }
  
  # print(eye)
  
  ai = data$IV[eye]
  bi = data$yprime[eye]
  
  deltaY = data$IV[eye+1] - data$IV[eye]
  
  
  deltaX = (data$d2[eye+1] - data$d2[eye])
  # x2 = (-log(data$Strike[eye+1]/f) - 0.5 * t * (data$IV[eye+1])^2)   /
  #              (data$IV[eye+1] * sqrt(t))
  # x1 = (-log(data$Strike[eye]/f) - 0.5 * t * (data$IV[eye])^2)   /
  #   (data$IV[eye] * sqrt(t))
  # deltaX = x2 - x1
  
  ci = (3 * deltaY  - deltaX *data$yprime[eye+1] - 2 * deltaX * data$yprime[eye] )/(deltaX^2)
  di = ( -2 * deltaY + deltaX * data$yprime[eye+1] + deltaX *data$yprime[eye] )/(deltaX^3)
  
  
  # target = (-log(target)/f) - 0.5 * t * (data$IV[eye+1])^2)   /
  #   (data$IV[eye+1] * sqrt(t))
  
  # print(ai)
  # print(bi)
  # print(ci)
  # print(di)
  # print(deltaX)
  # print(deltaY)
  
  # print(xi)
  
  targetdiff = (target- data$d2[eye])
  
  answer = ai + bi*targetdiff + ci*targetdiff^2 + di*(targetdiff)^3
  answer
}


df = data.frame(matrix(ncol= 2, nrow= 1))
colnames(df) = c("Date", "VIX")

for(i in seq(1,length(files),2)){
  row = mainFunction(files[i],files[i+1])
  # print(row)
  # print("d")
  df = rbind(df, row)  
}


write.csv(df, file = "VIX_Output.csv")

