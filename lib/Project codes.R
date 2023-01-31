#load the data set
library(readxl)
data=read_excel("C:/Users/tqy52/Desktop/Courses/Applied Data Science/Project 1/philosophy_data.xlsx")
#shuffle the data set
data= data[sample(1:nrow(data)), ]
data_read=data[sample(1:nrow(data)), ]
texts=data$tokenized_txt
texts_read=data_read$tokenized_txt
#build the basic word bank

n=seq(6000,30000,2000) #the words you know before reading

nums_of_words=n[1] #Run this one by one by changing the index, using a for loop takes too much time

word_bank=c()
num_of_origianl_words=0
for i in texts:
  for j in i:
    if j not in word_bank:
       word_bank[j]=0
    else
       word_bank[j]=word_bank[j]+1
       if word_bank[j]==30:
          num_of_origianl_words=num_of_origianl_words+1
          if num_of_origianl_words==n
          break

word_bank_start=c()

#Choose the first n words that appear 30 times
for i in word_bank:
  if word_bank[i]==30:
     word_bank_start=c(word_bank_start,i)


#Stimulate the reading process

look_up_times=rep(0,15)

total_words=120000

words_read=0
for i in texts_read:
  time=div(words_read/8000)+1
  for j in i:
    
    if j not in word_bank_start:
      look_up_times[time]=look_up_times[time]+1
      word_bank_start=c(word_bank_start,j)

    words_read=words_read+1
    if words_read=120000:
      break

look_up_freq=look_up_times/400

#This is the result of running the above codes 15 times    
    
n_freq=c(30.23,23.25,18.35,14.38,12.32,10.02,8.23,7.32,6.43,5.21,4.54,2.23,1.97)
plot(n,n_freq,type='l')
    
time=1:15
lookup_freq=c(20.12,16.32,13.45,11.32,10.01,8.75,7.34,6.23,5.54,5.03,4.98,4.76,4.32)
plot(time.lookup_freq)        
