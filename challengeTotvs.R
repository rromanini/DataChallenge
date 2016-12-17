
if(!require(jsonlite)){
  install.packages("jsonlite", repos ="http://cran.us.r-project.org")
  library(jsonlite)
}

if(!require(plyr)){
  install.packages("plyr", repos ="http://cran.us.r-project.org")
  library(plyr)
}


if(!require(lubridate)){
  install.packages("lubridate", repos ="http://cran.us.r-project.org")
  library(lubridate)
}

if(!require(neuralnet)){
  install.packages("neuralnet", repos ="http://cran.us.r-project.org")
  library(neuralnet)
}





#Getting data
temp <- "./challengeTotvs.zip"

fileURL = "https://github.com/TOTVS/MDMStatic/blob/master/code-challenge/TOTVS%20Labs%20-%20AI%20Challenge%20-%20Dataset.zip?raw=true"

#download original file from github
download.file(fileURL,destfile=temp, mode="wb")

#unzip file sample.txt
td <- tempdir()
fname <- unzip(temp, list=TRUE)$Name[1]
unzip(zipfile = temp, files=fname, exdir=td,overwrite=TRUE)

#read the file sample.txt
setwd(td)
raw.data <- readLines("sample.txt", warn = "F")

#Parsing the data
rd <- fromJSON(raw.data)

#Exploring the data

# Document size / Number of records from file sample.txt 
nrow(rd$ide)

# Number of null values
length(which(is.na(rd) == TRUE))

#summary from valorTotal, in complemento section
summary(rd$complemento)

#Conversion necessary for plot a histogram
rd$complemento <- data.matrix(rd$complemento)

#Distribution of Totals Sales (rd$complemento)
hist(rd$complemento)

par(mfrow=c(1,2))

#Resume about main statistics related to Totals (rd$complemento)
boxplot(rd$complemento,ylab="Total Price",main= toupper("Totals - With outliers"),col=c("red"))

boxplot(rd$complemento,ylab="Total Price",main= toupper("Totals - Without outliers"),col=c("blue"),outline=FALSE)
```

#Identify a pattern that can help predict how much a customer will spend.

qtdSalesByDay <- strptime(as.matrix(rd$ide[,1]),format = "%Y-%m-%d")
tab <- table(cut(qtdSalesByDay, 'day'))
tab2 <- table(cut(qtdSalesByDay, 'week'))

barplot(tab, col=cm(5), las=3,main = "Qtd Sales by Date")

barplot(tab2, col=cm(5), las=3,main = "Qtd Sales by Week")


qtdSalesByHour <- strptime(as.matrix(rd$ide[,1]),format = "%Y-%m-%dT%H:%M")
count(hour(qtdSalesByHour))

#Fitting Models

#Create an unique data frame with the prods for help in linear regression and neural network
x = 0
                
#Create an unique data frame with the prods for help in linear regression and neural network
for (obj in rd$dets){
  if (x<1) {
    dfProd <- obj$prod
    x = 1
  } else {
    dfProd <- rbind(dfProd,obj$prod)
  }
}
                
#First model, using regression 
#vUnCom explained by qCom,vProd and xProd variables
firstModel <- lm(vUnCom~qCom+vProd+xProd,dfProd)
summary(firstModel)

secondModel <- nnet(vUnCom~qCom+vProd+xProd,data=dfProd,size=2)
summary(secondModel)


#Mean squared error
                
#Using multiple linear regression
firstModel.predict <- predict(firstModel)
                
# mean squared error
mean((firstModel.predict - dfProd$vUnCom)^2) 
                
#Using neural network
secondModel.predict <- predict(secondModel)
                
# mean squared error
mean((secondModel.predict - dfProd$vUnCom)^2) 
                
#Sales forecast for the next week
                
#Building the dataframe
day <- c(5,6,7,8,9,11,12,13,14,15,16,18,19,20,21,22)
values = c()
i = 1
valueRestaurant = 0
currentDate = substr(rd$ide[i,1]$`$date`,1,10)

#summarize by day
                
for (obj in rd$complemento){
if(substr(rd$ide[i,1]$`$date`,1,10) != currentDate)
{
    values <- c(values,valueRestaurant)
    valueRestaurant = 0
    currentDate = substr(rd$ide[i,1]$`$date`,1,10)
}
valueRestaurant = valueRestaurant + rd$complemento[i]
i = i+1
}
                
df <- data.frame(values,day)
                
lm.fit <- lm(values~day,df)
                
#Predicting the last week of January
new.df <- data.frame(day=c(25,26,27,28,29,30,31))
                
#Predict using the linear Model
#Days 25 to 31 January
predict(lm.fit, new.df)
                

                
                