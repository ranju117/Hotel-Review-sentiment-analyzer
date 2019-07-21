library(xml2)
library(rvest)
library(stringr)
library(sentimentr)
overallsum<-0
for(k in 3:7){
  querstr<-paste("div:nth-child(",k,sep="")
  querstr<-paste(querstr,")",sep="")
  querstr<-paste("#component_24 > div:nth-child(3) > div:nth-child(1) >",querstr)
  querstr<-paste(querstr,"> div:nth-child(1) > div:nth-child(3) > div:nth-child(3) > div:nth-child(1)")
  print(querstr)
  
  url <-"https://www.tripadvisor.in/Hotel_Review-g1568568-d12940995-Reviews-Hotel_Harshali_Park-Khopoli_Raigad_District_Maharashtra.html"
  webpage <- read_html(url)
  title_html <- html_nodes(webpage, querstr)
  title <- html_text(title_html)
  print(title)
  
  sum<-0
  count<-0
  tab<-sentiment(title)
  print(tab)
  for(i in 1:nrow(tab)){
    sent<-as.numeric(tab[i,"sentiment"])
    sum<-sum+sent*100
    #sum<-sum/nrow(title)
    
  }
  rows<-nrow(tab)
  print(rows)
  sum<-(sum/rows)
  print(sum)
  if(sum==0)print("Neutral")
  if(sum<0&&sum>(-25))print("Mildly bad")
  if(sum<(-25)&&sum>(-50))print("Bad")
  if(sum<(-50)&&sum>(-75))print("Very Bad")
  if(sum<(-75))print("Worst!!")
  if(sum>0&&sum<25)print("Not bad")
  if(sum>25&&sum<50)print("Good")
  if(sum>50&&sum<75)print("Very Good")
  if(sum>75)print("Excellent!")
  overallsum<-overallsum+sum
}
print("The overall happiness score for this hotel")
print(overallsum/nrow(tab))