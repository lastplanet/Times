#If new files scrapped
# run bindandclean.R in Scrapped Data Files
# Then source this file
# Takes awhile as it does a bit of clean up line by line
# and gets the birth year
basedir = "~/"
setwd(paste0(basedir,"Dropbox/__MyPers Archive/Sprint Kayak/Times/"))

library(stringr)
paddlers=read.csv("Paddlers.csv",stringsAsFactors=FALSE)
#usa=read.csv("USA.csv")
#elite is the elite comparison paddlers
elite=read.csv("Elite.csv",stringsAsFactors=FALSE) #make sure no col is all blank
#fullresults.csv is created by bindandclean.R (above)
usa=read.csv("fullresults.csv",stringsAsFactors=FALSE)
usa$time[which(usa$time<10)]=NA #fix bad times

#building the whole data frame anew takes a long time
#if you add new birth years, then it is necessary to build anew
#but otherwise you can just fix the new entries
ans = readline("Rebuild whole mydata? y/n (takes a long time)")
if(ans=="y"){
mydata=rbind(usa,elite)
names=as.character(mydata$name)
pnames=as.character(paddlers$name)
mydata$by=paddlers$age[match(names, paddlers$name)]
mydata$country=paddlers$country[match(names, paddlers$name)]
mydata$gender=paddlers$gender[match(names, paddlers$name)]
for(j in c("K2","K4","C2","C4")){
  for(i in which(mydata$boat==j)){
    n=as.numeric(str_sub(j,2))
    val=which(str_detect(names[i],pnames))
    if(length(val)!=n){
      mydata$by[i]=NA}else{
    mydata$by[i]=sum(paddlers$age[val])/n
      }
    name1 = str_split(mydata$name[i], ";")[[1]][1]
    mydata$country[i]=paddlers$country[match(name1, paddlers$name)]
  }
}
# names[which(is.na(mydata$by))]
mydata$age=mydata$year-mydata$by-1+mydata$month/12
mydata$time[which(mydata$time<10)]=NA #fix bad times
mydata = mydata[,c("name","boat","time","event","level","type","finish","venue","month","day","year","by","age","country","gender","certified")]
write.csv(mydata, file="mydata.csv", row.names=FALSE)
}
if(ans=="n"){
  newmydata=rbind(usa,elite)
  oldmydata=read.csv("mydata.csv",stringsAsFactors=FALSE)
  tmpdat=apply(oldmydata[,c("name","boat","time","venue")],1,function(x){paste(x, collapse=" ")})
  tmpdat2=apply(newmydata[,c("name","boat","time","venue")],1,function(x){paste(x, collapse=" ")})
  mydata=newmydata[which(!(tmpdat2 %in% tmpdat)),]
  
  names=as.character(mydata$name)
  pnames=as.character(paddlers$name)
  mydata$by=paddlers$age[match(names, paddlers$name)]
  mydata$country=paddlers$country[match(names, paddlers$name)]
  mydata$gender=paddlers$gender[match(names, paddlers$name)]
  for(j in c("K2","K4","C2","C4")){
    for(i in which(mydata$boat==j)){
      n=as.numeric(str_sub(j,2))
      val=which(str_detect(names[i],pnames))
      if(length(val)!=n){
        mydata$by[i]=NA}else{
          mydata$by[i]=sum(paddlers$age[val])/n
        }
      name1 = str_split(mydata$name[i], ";")[[1]][1]
      mydata$country[i]=paddlers$country[match(name1, paddlers$name)]
    }
  }
  # names[which(is.na(mydata$by))]
  mydata$age=mydata$year-mydata$by-1+mydata$month/12
  mydata$time[which(mydata$time<10)]=NA #fix bad times
  mydata = mydata[,c("name","boat","time","event","level","type","finish","venue","month","day","year","by","age","country","gender","certified")]
  #only include old data that is in the new file in case a file was deleted
  mydata = rbind(mydata, oldmydata[which(tmpdat %in% tmpdat2),])
  write.csv(mydata, file="mydata.csv", row.names=FALSE)
}
if(ans!="y" & ans!="n") cat("Input y or n.\n")