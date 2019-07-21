library(xml2)
library(rvest)
library(stringr)
library(sentimentr)
library(tidyverse)
df<-read.csv(file="/home/ranjani/Downloads/b.csv", sep=",", colClasses=c("NULL", NA, "NULL"))
#df <- read.csv("/home/ranjani/Downloads/b1.csv",header = FALSE)
n<-0
mb<-0
vb<-0
w<-0
nb<-0
g<-0
vg<-0
e<-0
b<-0
sum<-0
vectind<-0
review_array<-array()
scoreind<-0
score_array<-array()
sent<-as.numeric(tab[1,"sentiment"])
sum<-sum+sent*100
min1<-sum
min2<-sum
min3<-sum
max<-sum
for(row in 1:5){
  
  
  sum<-0
  count<-0
  title<-df[row,"Review"]
  r<-toString(title)
  print(title)
  tab<-sentiment(r)
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
  
  if(sum==0){
    print("Neutral") 
    n<-n+1}
  if(sum<0&&sum>(-25)){
    print("Mildly bad")
    mb<-mb+1}
  if(sum<(-25)&&sum>(-50)){
    print("Bad")
    b<-b+1}
  if(sum<(-50)&&sum>(-75))
  {print("Very Bad")
    vb<-vb+1}
  if(sum<(-75)){
    print("Worst!!")
    w<-w+1}
  if(sum>0&&sum<25){
    print("Not bad")
    nb<-nb+1}
  if(sum>25&&sum<50){
    print("Good")
    g<-g+1}
  if(sum>50&&sum<75){
    print("Very Good")
    vg<-vg+1}
  if(sum>75){
    print("Excellent!")
    e<-e+1}
  
  
  if(sum>max)
  { max<-sum
  best_review<-title}
  
  if(sum<min1)
  { min3<-min2
  worst_review3<-worst_review2
  min2<-min1
  min1<-sum
  worst_review2<-worst_review1
  
  worst_review1<-title}
  else if(sum<min2)
  {
    min3<-min2
    min2<-sum
    worst_review3<-worst_review2  
    worst_review2<-title 
  }
  
  else if(sum<min3)
  {
    
    min3<-sum
    worst_review3<-title 
  }
}
print("Best Review")
print(best_review)
print("Reviews you got to concentrate on")
print(worst_review1)
print(worst_review2)
print(worst_review3)
t<-c(n,mb,b,vb,w,nb,g,vg,e)
labels<-c("Neutral","Mildly Bad","Bad","Very Bad","Worst","Not Bad","Good","Very Good","Excellent")
pie(t,labels)
#print(score_array)
