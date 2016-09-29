# open file 
library(tidyr)
kirby<-read.csv("/Users/bryanxwu/Desktop/bryanxwu/Documents/Extra- Curricular/Research/Poldrack Lab/Practice/kirby.csv")

# extract IDs 
kirby<-separate(kirby,X, c("Kirby", "idtrial"), sep="kirby_")
kirby<-kirby[,-1]
kirby<-separate(kirby,idtrial, c("id", "trial"), sep="_")

kirby2<-kirby

# there are 36 participants, and 27 trials per 

# check $ bounds and separate into reward groups
kirby2$large_amt[kirby2$large_amt>65 & kirby2$large_amt<70]
kirby2$large_amt[kirby2$large_amt>85]
kirby2$large_amt[kirby2$large_amt<30]
kirby2$large_amt[kirby2$large_amt>35 & kirby2$large_amt<55]

# looks like there are some amounts (25 and 50) that are outside of the ranges, and so I will regroup so that it's even...

kirby2$reward[kirby2$large_amt>65]<-3
kirby2$reward[kirby2$large_amt>=50 & kirby2$large_amt<65]<-2
kirby2$reward[kirby2$large_amt<50]<-1
kirby2$reward[is.na(kirby2$large_amt)==TRUE]<-NA

# check frequencies 
library(dplyr)
kirby2 %>%
  filter(!is.na(reward)) %>%
  group_by(reward) %>%
  summarise(freq=n())

# good! So there's 9 of each small, medium, and large 

# calculate the discount rate for hyperbolic and exponential models 
kirby2<-kirby2 %>%
  filter(!is.na(reward)) %>%
  mutate(exponential=log(small_amt/large_amt)/(-1*later_del))

kirby2<-kirby2 %>%
  filter(!is.na(reward)) %>%
  mutate(hyperbolic=(((large_amt/small_amt)-1)/later_del))
  
# time to somehow rank/bin? 
kirby3<-kirby2[1:27,]

kirby3<-arrange(kirby3, hyperbolic)



# 9 groups of 3 

kirby2$rank[kirby2$hyperbolic<0.0003854351]<-1
kirby2$rank[kirby2$hyperbolic>0.0001568303 & kirby2$hyperbolic<0.0009399513]<-2
kirby2$rank[kirby2$hyperbolic>0.0003867213 & kirby2$hyperbolic<0.0022755535]<-3
kirby2$rank[kirby2$hyperbolic>0.0009491939 & kirby2$hyperbolic<0.0050845070]<-4
kirby2$rank[kirby2$hyperbolic>0.0022917006 & kirby2$hyperbolic<0.0128554160]<-5
kirby2$rank[kirby2$hyperbolic>0.0051780537 & kirby2$hyperbolic<0.0293421971]<-6
kirby2$rank[kirby2$hyperbolic>0.0131014197 & kirby2$hyperbolic<0.0625334812]<-7
kirby2$rank[kirby2$hyperbolic>0.0305167630 & kirby2$hyperbolic<0.1433288727]<-8
kirby2$rank[kirby2$hyperbolic>0.0651767585]<-9

kirby2<-arrange(kirby2, id,rank, reward)

View(kirby2)

# now to analyze!

kirby3$SSpercent[1]<-sum(kirby2$key_press==80 & kirby2$reward==1 & kirby2$rank==1)/36
kirby3$SSpercent[2]<-sum(kirby2$key_press==80 & kirby2$reward==2 & kirby2$rank==1)/36
kirby3$SSpercent[3]<-sum(kirby2$key_press==80 & kirby2$reward==3 & kirby2$rank==1)/36
kirby3$SSpercent[4]<-sum(kirby2$key_press==80 & kirby2$reward==1 & kirby2$rank==2)/36
kirby3$SSpercent[5]<-sum(kirby2$key_press==80 & kirby2$reward==2 & kirby2$rank==2)/36
kirby3$SSpercent[6]<-sum(kirby2$key_press==80 & kirby2$reward==3 & kirby2$rank==2)/36
kirby3$SSpercent[7]<-sum(kirby2$key_press==80 & kirby2$reward==1 & kirby2$rank==3)/36
kirby3$SSpercent[8]<-sum(kirby2$key_press==80 & kirby2$reward==2 & kirby2$rank==3)/36
kirby3$SSpercent[9]<-sum(kirby2$key_press==80 & kirby2$reward==3 & kirby2$rank==3)/36
kirby3$SSpercent[10]<-sum(kirby2$key_press==80 & kirby2$reward==1 & kirby2$rank==4)/36
kirby3$SSpercent[11]<-sum(kirby2$key_press==80 & kirby2$reward==2 & kirby2$rank==4)/36
kirby3$SSpercent[12]<-sum(kirby2$key_press==80 & kirby2$reward==3 & kirby2$rank==4)/36
kirby3$SSpercent[13]<-sum(kirby2$key_press==80 & kirby2$reward==1 & kirby2$rank==5)/36
kirby3$SSpercent[14]<-sum(kirby2$key_press==80 & kirby2$reward==2 & kirby2$rank==5)/36
kirby3$SSpercent[15]<-sum(kirby2$key_press==80 & kirby2$reward==3 & kirby2$rank==5)/36
kirby3$SSpercent[16]<-sum(kirby2$key_press==80 & kirby2$reward==1 & kirby2$rank==6)/36
kirby3$SSpercent[17]<-sum(kirby2$key_press==80 & kirby2$reward==2 & kirby2$rank==6)/36
kirby3$SSpercent[18]<-sum(kirby2$key_press==80 & kirby2$reward==3 & kirby2$rank==6)/36
kirby3$SSpercent[19]<-sum(kirby2$key_press==80 & kirby2$reward==1 & kirby2$rank==7)/36
kirby3$SSpercent[20]<-sum(kirby2$key_press==80 & kirby2$reward==2 & kirby2$rank==7)/36
kirby3$SSpercent[21]<-sum(kirby2$key_press==80 & kirby2$reward==3 & kirby2$rank==7)/36
kirby3$SSpercent[22]<-sum(kirby2$key_press==80 & kirby2$reward==1 & kirby2$rank==8)/36
kirby3$SSpercent[23]<-sum(kirby2$key_press==80 & kirby2$reward==2 & kirby2$rank==8)/36
kirby3$SSpercent[24]<-sum(kirby2$key_press==80 & kirby2$reward==3 & kirby2$rank==8)/36
kirby3$SSpercent[25]<-sum(kirby2$key_press==80 & kirby2$reward==1 & kirby2$rank==9)/36
kirby3$SSpercent[26]<-sum(kirby2$key_press==80 & kirby2$reward==2 & kirby2$rank==9)/36
kirby3$SSpercent[27]<-sum(kirby2$key_press==80 & kirby2$reward==3 & kirby2$rank==9)/36

