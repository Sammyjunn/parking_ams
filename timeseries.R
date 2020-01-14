#Making the path and global variables filenames
PATH = 'C:/Users/Sammy/Downloads/parkingdata'
filenames <- list.files(PATH, full.names=TRUE)
filenames2 <- list.files(PATH, full.names=FALSE)

#This function reads the data of a specific garage and returns it
startReading <- function(i){
  showConnections(all=TRUE)
  zz = gzfile(filenames[i], 'rt' )  
  dat = read.csv(zz,header=TRUE, sep = ';')
  check = colnames(dat)
  head(dat)
  check == colnames(dat)
  close(zz)
  length(filenames)
  dat['NameOfParkingSpace'] = filenames2[1]
  return (dat)
}
startReading(54)

#This creates the data of garage 1
zz = gzfile(filenames[1], 'rt' )
df = read.csv(zz,header=TRUE, sep = ';')
close(zz)
df['NameOfParkingSpace'] = filenames2[1]
n = nrow(df)

#This merges all the other garages to garage 1
dataMerge <- function(){
  for (i in range(2:57)){
    zz = gzfile(filenames[i] )  
    dfi = read.csv(zz,header=TRUE, sep = ';')
    dfi['NameOfParkingSpace'] = filenames2[i]
    df = rbind(df,dfi)
    n=nrow(dfi) + n
  } 
  return(df)
}  
df = dataMerge() 

#This saves a desired dataframe into Excel, can be altered
inExcel <- function(dataFrame){
  write.csv(dataFrame, 'dataFrame.csv', row.names = TRUE)
}
inExcel(df)

#This functions shows all the error values, and a list of where they are
detectErrors <- function(dataFrame){
  errorindex = dataFrame[which(dataFrame$state == 'error'),]
  index = which(dataFrame$state == 'error')
  shortcapacity = dataFrame$shortcapacity
  states = dataFrame$state
  return (list(errorindex, index))
}
detectErrors(df)

#This functions gives the list of the "Okay rates"
okayRate <- function(){
  for (j in (1:57)){
    zz = gzfile(filenames[j] )  
    dfi = read.csv(zz,header=TRUE, sep = ';')
    print(nrow(dfi[which(dfi$state == 'ok'),])/nrow(dfi))
    print(j)
  }
}  
okayRate()

#This function gives us some basic statistics and the occupancy rate
descriptiveStatistics <- function(dataFrame){
  stat = summary(dataFrame$shortcapacity)
  st.err = sd(dataFrame$shortcapacity)
  dataFrame['occupancyRate'] = 1 - dataFrame$freespaceshort/dataFrame$shortcapacity
  occupancy = dataFrame['occupancyRate']
  return(list(stat, st.err, occupancy))
  #occupancy[1:50,] to make it shorty
}  
descriptiveStatistics(startReading(54))

#id = 900000003_parkinglocation for example
#This function can plot the freespaceshort
plotFunction <- function(id){
df[which(df$id == "id"),]
plot(ts(df[which(df$id == "id"),]$shortcapacity), ts(df[which(df$id == "id"),]$freespaceshort), gpars = list(col = c("black", "red")))
}

#install.packages(c("zoo", "dplyr"))
#need to install these packages as 1
library(zoo)
library(dplyr)
dfi['shortcapacity']
simpleNanSolver<-function(dfp){
  #removes the Nas at the head and replaces all other Nas for the last observation in the respective column   (the Last Observation Carried Forward)
  #inputs dataframe with data of 1 station
  drop <- c("longcapacity","freespacelong")
  dfp = dfp[,!(names(dfp) %in% drop)]
  dfp %>% 
    do(na.locf(.))
}
which(is.na(simpleNanSolver(startReading(54))))



#test change
