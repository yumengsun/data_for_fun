#########################################################
#Task 1: data processing                                #
#Details: get the data into R as a data frame of        #
#character variables, where one row/record in the input #
#will populate n rows in the data frame. Here n is the  #
#number of MAC responses in that record.                #
#########################################################

#Step 1: Use readLines() to read each line of the offline data into R as a character string. 
txt = readLines("/Users/yumengsun/Documents/133/offline.final.trace.txt")

#Step 2: use regular expressions to split each line up into fields, and create a set of 10 variables: time, scanMac, posX, posY, posZ, orientation, mac, signal, channel, type, where one record in the input file results in multiple records in the data frame, one for each MAC, with the information such as time, scanMAC, posX, etc. repeated for each of these records.
processLine = function(x){
  #####################################################################
  # Input: a string  that corresponds to one line from the data file  #
  # Output: a character matrix with 10 columns                        #
  #                                                                   #
  # If the input cannot be transformed into 10 columns, return NA     #
  # Handle the NAs when creating the data frame 'offline'             #
  #####################################################################
  
  # Case 1: Return NA if x is an "extra line" which begins with a '#':
  if((!grepl("^#",x ))){
    x.split = unlist(strsplit(x, split = ";"))
    if(length(x.split) >= 5){
      time = gsub("^t=", "",x.split[1])
      scanMac = gsub("^id=", "",x.split[2])
      pos = gsub("^pos=", "",x.split[3])
      pos.split = unlist(strsplit(pos, split = ","))
      posX = pos.split[1]
      posY = pos.split[2]
      posZ = pos.split[3]
      orientation = gsub("^degree=", "",x.split[4])
      # subset the input line to extract the MAC addresses of all the responses
      MACs = x.split[-(1:4)] 
      # split the extracted lines with "=" and form it as a matrix with the first row being all the MAC addresses and the second row with all the other info(i.e. signal, channel and mode type)
      MACs.split = matrix(unlist(strsplit(MACs, split = "=")), nrow = 2)
      # mac is a vector contains all the MAC addresses
      mac = MACs.split[1,]
      # now we further split the second row of MACs.split, to separate signal, channel and mode type using ",". Form it as a matrix with three rows, with the first row storing all the signals, the second row with all the channels and the third row with all the types. Store the matrix into "siChTy".
      siChTy = matrix(unlist(strsplit(MACs.split[2,], split = ",")),nrow = 3)
      signal = siChTy[1,]
      channel= siChTy[2,]
      type = siChTy[3, ]
      
      # Form a character matrix with 10 columns
      n = length(mac)
      result = cbind(rep(time,n), rep(scanMac,n), 
                     rep(posX,n), rep(posY,n), rep(posZ,n),
                     rep(orientation,n), mac, signal, channel, type)
      return(result)
    }
  }else{ 
    #Case 2: if x does not begin with "#":
    # Split the line using ";"
    # Some of the lines do not have MAC addresses of any responding peers
    # Check if the input is such a line by checking its length
    # If the length is smaller than 10, drop this line by returning NA.
    return (NA)
    
    
  }
}
tmp = lapply(txt, processLine)
#omit the lines with NAs and save the rest as a data frame
offline = na.omit(as.data.frame(do.call("rbind", tmp),
                                stringsAsFactors=FALSE))
#give meaningful names to the dataframe
names(offline) = c("time", "scanMac", "posX", "posY", 
                   "posZ","orientation", "mac", 
                   "signal", "channel", "type")

##########################################################
#Task 2: clean and process these character variables into# 
#formats that can be more easily analyzed                #
##########################################################

# Choose which 6 MAC addresses to keep:
# After dropping all records that correspond to adhoc devices, there are 12 MAC addresses left.
# According to the online database, Linksys/Cisco has MAC addresses starting with "00:14:bf". Therefore, we have found 5 MAC addresses to keep: 00:14:bf:3b:c7:c6, 00:14:bf:b1:97:81, 00:14:bf:b1:97:8a, 00:14:bf:b1:97:8d and 00:14:bf:b1:97:90. 

#> summary(offline$mac)
#00:04:0e:5c:23:fc   00:0f:a3:39:dd:cd   00:0f:a3:39:e0:4b 
#418                     145619             43508 
#00:0f:a3:39:e1:c0  00:0f:a3:39:e2:10 00:14:bf:3b:c7:c6 
#145862                  19162            126529 
#00:14:bf:b1:97:81  00:14:bf:b1:97:8a 00:14:bf:b1:97:8d 
#120339                132962            121325 
#00:14:bf:b1:97:90  00:30:bd:f8:7f:c5 00:e0:63:82:8b:a9 
#122315                 301               103 

#We also noticed that 00:0f:a3:39:dd:cd and 00:0f:a3:39:e1:c0 have similar frequencies as the chosen addresses. We therefore have to decide which one to keep. 

#We then checked the summary statistics of the signal strenghs of these 7 MAC addresses. As we know, the 6 access points were on the same floor and signal strenghts were measured at 166 different points; thus the signal strenghs must have a similar range among the 6 access points.

#> summary(offline$signal[offline$mac=="00:0f:a3:39:dd:cd"])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-98.00  -77.00  -71.00  -70.46  -64.00  -46.00 
#> summary(offline$signal[offline$mac=="00:0f:a3:39:e1:c0"])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-92.0   -58.0   -54.0   -53.7   -50.0   -34.0 
#> summary(offline$signal[offline$mac=="00:14:bf:b1:97:90"])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-93.00  -76.00  -69.00  -67.03  -58.00  -34.00 
#> summary(offline$signal[offline$mac=="00:14:bf:b1:97:8a"])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-90.00  -64.00  -58.00  -56.65  -50.00  -26.00 
#> summary(offline$signal[offline$mac=="00:14:bf:b1:97:8d"])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-93.00  -60.00  -54.00  -53.74  -48.00  -25.00 
#> summary(offline$signal[offline$mac=="00:14:bf:3b:c7:c6"])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-90.00  -66.00  -62.00  -60.76  -56.00  -38.00 

#From the above comparation, we observed that 00:0f:a3:39:dd:cd is overall too weak, compared with the others; on the other hand, 00:0f:a3:39:e1:c0 has a similar range as the other five access points.

#So far, we have chosen the following 6 MAC addresses to keep:
#00:0f:a3:39:e1:c0, 00:14:bf:3b:c7:c6, 00:14:bf:b1:97:81, 
#00:14:bf:b1:97:8a, 00:14:bf:b1:97:8d and 00:14:bf:b1:97:90. 


cleanData = function(data, keepMacs = c(
  "00:0f:a3:39:e1:c0",
  "00:14:bf:3b:c7:c6", 
  "00:14:bf:b1:97:81", 
  "00:14:bf:b1:97:8a", 
  "00:14:bf:b1:97:8d",
  "00:14:bf:b1:97:90")) {
  #########################################################
  # Input:                                                #
  #  -data: the output from the above processing          #
  #  -keepMacs: a character vector of the 6 MAC addresses #
  #                                                       #
  # Output:                                               #
  #  - a cleaned dataframe                                #
  #########################################################
  
  #Convert everything except mac addresses and type into numeric
  dataframe = data
  dataframe[,-c(1,2,7,10)]= sapply(c(3:6,8:9),
                                   function(x) 
                                     as.numeric(dataframe[,x]))
  #Convert type and mac addresses into factor.
  dataframe$mac = as.factor(dataframe$mac)
  dataframe$scanMac = as.factor(dataframe$scanMac)
  dataframe$type = as.factor(dataframe$type)
  dataframe$time = as.factor(dataframe$time)
  #> sapply(offline, class)
  #time        scanMac        posX        posY        posZ 
  #"numeric"   "factor"   "numeric"   "numeric"   "numeric" 
  #orientation     mac      signal     channel        type 
  #"numeric"   "factor"   "numeric"   "numeric"   "factor" 
  
  #drop unnecessary columns where all the elements are equal 
  checkEqual = sapply(1:10, 
                      function(i) 
                        all(dataframe[i,i]==dataframe[,i]))
  # > names(offline)[checkEqual]
  #[1] "scanMac" "posZ"   
  #So, scanMac and posZ need to be dropped.
  dataframe$scanMac = NULL
  dataframe$posZ = NULL
  #Since all the channels are close to 2.4G, drop that column as well.
  dataframe$channel = NULL
  #Drop all records that correspond to adhoc devices, i.e. type = 1
  dataframe = dataframe[(dataframe$type!=1),]
  #"type" is no longer useful, so drop that column.
  dataframe$type = NULL  
  #Round the values for orientation to the nearest 45 degrees, and store in a new column named 'ortn.rounded'
  dataframe$ortn.rounded = round(dataframe$orientation/45)*45
  dataframe$ortn.rounded[dataframe$ortn.rounded==360] = 0
  dataframe = dataframe[dataframe$mac%in%keepMacs,]
  return(dataframe)
}

offline2 = cleanData(offline)

#> names(offline2)
#[1] "time"         "posX"         "posY"         "orientation" 
#[5] "mac"          "signal"       "ortn.rounded"

#> dim(offline2)
#[1] 769332      7

#######################
#Part 2: Visualization# 
#######################

##plot1- Distribution of Signal by Angle for Each Router

#In order to study how the signal strength changes when the router has different orientations (8 angles),
#we pick a specific location (posX = 24  & posY = 4) and investigate the effect of orientation on this 
#fixed location.
par(mar=c(1,1,1,1))
 macName=c(
   "00:0f:a3:39:e1:c0",
   "00:14:bf:3b:c7:c6", 
   "00:14:bf:b1:97:81", 
   "00:14:bf:b1:97:8a", 
   "00:14:bf:b1:97:8d",
   "00:14:bf:b1:97:90")
 angle=unique(offline2$ortn.rounded)
 draw=function(i,j,xaxt='n',...){
 plot( density(offline2$signal[offline2$posX == 24 & offline2$posY == 4 & offline2$mac==macName[i] & offline2$ortn.rounded==angle[j]]),
       col=i, main=c(macName[i],angle[j]),cex.main=0.8, xaxt=xaxt,
       xlim=c(-100,-40),ylim=c(0,0.65),...)
 }
 par(mar=c(2,2,2,2))
 par(mfrow=c(6,8))
 mapply(function(i,j) draw(i,j), rep(1:5,each=8), 1:8)
 mapply(function(i,j) draw(i,j,xaxt="s"), rep(6,each=8), 1:8)

#There are 48 density plots shown here for the signal strengths. 6 colors were used to identify the 6 routers, 
#and the coloumns are showing the angles. Since there are roughly 110 observations in each panel, based on LLN, 
#most of the curves should look roughly normal. In our plot, some of them do show our predicted pattern, but many 
#other curves are still skewed to the left with serious departures of secondary modes. Also, the center of the 
#distribution are different for each plot, meaning that angles and MAC address do affect the signal strength in most cases. 

###plot2-Matching MAC with Position

#To indentify the location of each router, we pick 6 locations which are closest
#to six routers in the png file. These locations are(1,13), (2,0), (7,7), (16,3), (33,3)
#and (33,8), which is showed in the matrix (AppPos) below. We also set the angle as 0 to
#control the effect of orientation.Since the signal strength should be strong when the 
#location is close to the router, we can match the mac address with its approximate position.

 AppPos= matrix( c( 1,13,2,0,7,7,16,3,33,3,33,8),ncol = 2, byrow = TRUE)
 print(AppPos)

 par(mar=c(2,2,2,2))
 par(mfrow=c(6,6))
 Locate=function(i,j,xaxt='n',...){
 plot(density(offline2$signal[offline2$posX == AppPos[j,1] & offline2$posY == AppPos[j,2] & offline2$mac==macName[i] & offline2$ortn.rounded==angle[1]]),
      col=i, main=c(macName[i],c(AppPos[j,1] , AppPos[j,2])),cex.main=0.7, xaxt=xaxt,
      xlim=c(-90,-40),ylim=c(0,0.65),...)
 }
 mapply(function(i,j) Locate(i,j), rep(1:5,each=6),1:6)
 mapply(function(i,j) Locate(i,j,xaxt="s"), rep(6,each=6),1:6)

#Accodring to the plot above, the location of each can be shown below. The middle-bottom
#one on the png file is tricky since there is no tested location close to it. Therefore, 
#we decide the other five first and assign it the one left over. 
  ## "00:0f:a3:39:e1:c0"    (7.5,6.5)
  ## "00:14:bf:3b:c7:c6"    (13,-2)   "left over"
  ## "00:14:bf:b1:97:81"    (33.5,3)
  ## "00:14:bf:b1:97:8a"    (2.5,-0.5)
  ## "00:14:bf:b1:97:8d"    (33.5,9)
  ## "00:14:bf:b1:97:90"    (1,14)

###plot3-Signal Strength by Distance for Each Router
 
#After maching the mac adress with the the closest testing location, we can create a more 
#accurate position matrix for each router based on the png file.

 AppPos2=matrix( c( 7.5,6.5,13,-2,33.5,3,2.5,-0.5,33.5,9,1,14),ncol = 2, byrow = TRUE,
               dimnames = list(macName, c("x", "y") ))
 print(AppPos2)

#Now we can test the effect of distance on signal strength for each router. Since different 
#set of locations are tested for 110 times for each router, we drew 6 scatter plots, whose 
#y-axis is the signal strength and x-axis is the distance.We did not exclude the effect of 
#orientation here since we want to  see their distribution in the plots as well. 

 par(mfrow=c(2,3))
 DistPlot=function(i){
   MAC=offline2[offline2$mac==macName[i],]
   MAC$dist=sqrt((MAC$posX-AppPos2[i,1])^2+(MAC$posY-AppPos2[i,2])^2)
   plot(y=jitter(MAC$signal,amount=1.5),x=jitter(MAC$dist,amount=1.5),cex=0.02,pch=19,
        main=macName[i],xlab="Distance",ylab="Signal")
 }
 mapply(function(i) DistPlot(i),1:6)

#The plots generally follows a linear model that shorter distance leads to higher signal,
#which is the same as we predicted and the slope can be seen in the plots. Morever, the 
#vertical change of the plots indicate the effect of orientation as well. 

############################
# Part 3                   #
# Nearest Neighbors        #
# Cross-validation         #
# and assessment           #
############################
#function: re-organize the data frame to prepare for the prediction 
prepare = function(df, MACs =c(
  "00:0f:a3:39:e1:c0",
  "00:14:bf:3b:c7:c6", 
  "00:14:bf:b1:97:81", 
  "00:14:bf:b1:97:8a", 
  "00:14:bf:b1:97:8d",
  "00:14:bf:b1:97:90") ){
  #########################################
  # Input:                                #
  #       df: the data frame to be        #
  #           reshaped.                   #
  #       MACs: the MAC addresses to keep #
  # Output:                               #
  #       newdf: a data frame with each   #
  #             row being a unique combi- #
  #             nation of position and    #
  #             orientation and its colunms
  #             being: posCombo, posX,posY#
  #             ortn.rounded, orientation,#
  #             S1, S2, S3, S4, S5, S6.   #
  #########################################
  
  #combine posX, posY, ortn.rounded into one variable - posCombo
  df$posCombo = paste(df$posX, df$posY, df$ortn.rounded, sep=",")
  #get the unique combinations for all the position and orientation
  pos = unique(df$posCombo)
  comboSplit = matrix(as.numeric(unlist(strsplit(pos, split = ","))),ncol = 3, byrow = T)
  #add posCombo, posX,posY and orientation to the new data frame
  newdf = data.frame(pos,comboSplit)
  #add S1...S6 into the new data frame
  for (i in 1:6){
    S = sapply(1:length(pos), function(x) getSignal(df=df, pos = pos[x],MAC=MACs[i]))   
    newdf = cbind(newdf, S)
  }
  newdf$orientation = sapply(1:length(pos), function(x) df$orientation[df$posCombo==pos[x]][1])
  
  #give meaningful names to the new data frame
  names(newdf) = c("posCombo","posX","posY","ortn.rounded",paste("S",1:6,sep=""),"orientation")
  return (newdf)
}
offline3 = prepare(offline2)
save(offline3, file ="offline3.rda")

#process online data
txt = readLines("/Users/yumengsun/Documents/133/proj/online.final.trace.txt")
tmp = lapply(txt, processLine)
#omit the lines with NAs and save the rest as a data frame
online = na.omit(as.data.frame(do.call("rbind", tmp),
                               stringsAsFactors=FALSE))
#give meaningful names to the dataframe
names(online) = c("time", "scanMac", "posX", "posY", 
                  "posZ","orientation", "mac", 
                  "signal", "channel", "type")
online2 = cleanData(online)
online3 = prepare(online2)
#find k nearest neighbours to the newSignal
findKNN = function(train, newSignal,k){
  trainSignal = train[,paste("S",1:6,sep="")]
  diff = apply(trainSignal, 1, 
               function(x) x-newSignal)
  dist = unlist(lapply(1:length(diff),function(x) sqrt(sum(diff[[x]]^2))))
  KNN = (train[order(dist)[1:k], paste("pos",c("X","Y"),sep="")])
  KNN$weight = 1/(dist[order(dist)][1:k])
  KNN$weight = KNN$weight/sum(KNN$weight)
  return(KNN)
}
#helper: get n nearest angles to the input angle
getNearAngle = function(n, angle){
  factor = round(angle/45)
  rounded = factor*45
  if(n%%2 ==1){
    angles = rounded + seq(-45*(n-1)/2, 45*(n-1)/2,length=n)
  }else{
    n = n+1
    angles = rounded + seq(-45*(n-1)/2, 45*(n-1)/2,length=n)
    if(rounded>angle){
      angles = angles[-n]
    }else{
      angles = angles[-1]
    }
  }
  angles[angles<0]=angles[angles<0]+360
  angles[angles>=360]=angles[angles>=360]-360
  return (angles)
}
#helper: get a train set with only the n nearest angles
getTrain = function(df, n, angle){
  angles = getNearAngle(n,angle)
  return(df[df$ortn.rounded%in%angles,])
}
predict = function(newData, train, n=1, k=3){
  newSignal = newData[,paste("S",1:6,sep="")]
  angle = newData$orientation
  pred = list(length=nrow(newData))
  for(i in 1:nrow(newData)){
    newTrain = getTrain(train, n, angle[i])
    KNN = findKNN(newTrain, newSignal[i,], k)
    pred.x = weighted.mean(x = KNN$posX, w = KNN$weight)
    pred.y = weighted.mean(x = KNN$posY, w = KNN$weight)
    pred[[i]] = c(pred.x, pred.y)
  }
  pred = do.call("rbind",pred)
  return(pred) 
}
getError = function(pred, true){
  sum(rowSums((pred-true)^2))
}

###########
#cross validation
offline3$posXY =  as.factor(paste(offline3$posX, offline3$posY, sep=","))
online3$posXY =  as.factor(paste(online3$posX, online3$posY, sep=","))

#>length(levels(offline3$posXY)) 
#[1] 166
#So we use 5-fold CV


cvKNN = function(trainDF, v = 5, k){
  #convert this random permutation into a matrix with 5 columns of indices, one for each fold
  trainXY = matrix(sample(unique(trainDF$posXY),165), ncol=v)
  err = matrix(0, nrow = 8, ncol = length(k))
  for(i in 1:v){
    
    train = subset(trainDF, posXY%in%trainXY[,-i])
    test = subset(trainDF, posXY%in%trainXY[,i])
    for(nAngle in 1:8){
      print(nAngle)
      for(j in 1:length(k)){
        pred = predict(newData=test, train, n=nAngle, k=k[j])
        err[nAngle,j] = err[nAngle,j]+
          getError(pred, true = test[,c("posX","posY")])       
      }
    } 
  }
  return(err)
}
CVerror = cvKNN(trainDF=offline3, v = 5, k=1:10)

plot(CVerror[1,], type = "l", ylim =c(10000,20000), xlab="Number of nearest neighbours",ylab="Sums of squared error",main = "5-fold Cross Validation on Offline Data to Choose Besk k-nAngle combination")
sapply(2:8, function(x) lines(CVerror[x,], col = x))
abline(v=10,lty=2)
abline(h=10150.42,lty=2)
points(x = 10, y = 10150.42, col = "red")
legend("topright",legend=paste("nAngle = ",1:8,sep=""),lty=1,col = 1:10,cex = .6,bty="n")
# We choose k = 10, numAngle = 4 as the best k-nAngle combination, because it gives the smallest CV error.
pred = predict(online3, offline3, n=4, k=10)
error = getError(pred = pred, true = online3[,c("posX","posY")])
#> error
#[1] 261.418