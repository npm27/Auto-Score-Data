#What I want: A function that will quickly score cued-recall data
#What it needs to look like: score_function(column 1, column 2,... [with the possibility of checking whether more than 2 match], data = X)
#A chronbach's alpha function would be useful as well

#this function needs to be able to parse apart two strings and essentially split them into their constituent parts:
#So..

test1 = c("cat", "dogs", "weather", "drink", "earth", "ground") #answer key
test2 = c("cats", "dig", "weather", "drinkk", "aerth", "gound") #recall responses

#####################################
#key      recall     k1    k2    k3    k4    k5    k6     r1   r2    r3    r4    r5    56

#cat      cats       c     a     t                        c    a     t      s
#dogs     dig        d     o     g      s                 d    i     g


#also would be good to get a length measure (so expected length vs actual length)
#then that would give two criteria to match on
#need a way to denote whether recall string is too long or too short

#will also need to control for upper/lower case
#could also couple it with a basic percentage function and maybe the percent missing function to detect how many characters two strings differ by

##load libraries here
#I'll need stringr, dplyr, and sqldf for sure
library(dplyr)
library(stringr)
library(sqldf)

#load in test data here

####percent missing function here####
percentmiss = function(x){ sum(is.na(x))/length(x) *100 }

####Cohens kappa here####
#Note: It would be a good to research other validity measures as well
#formula k = ( p(a) - p(e) ) / ( 1 - p(e) )
#where k = kappa, p(a) = observed percentage of agreement
#p(e) = total percentage for coder A X total percentage for coder B

kappa = function(x) {
  
  #get percent agreement
  x$pa = x[ , 1] - x[ , 2]
  temp = table(x$pa)[2]
  temp = data.frame(temp)
  pa = temp[1,1]
  pa = (pa / nrow(x))
  
  #now get chance agreement
  #expected = (((cm1 x rm1) / n) + ((cm2 x rm2) / n) / n)
  temp = data.frame(table(x$pa))
  x$pa2 = x[ , 1] + x[ , 2]
  temp2 = data.frame(table(x$pa2))
  
  #for pa
  #1 = person A correct B incorrect
  #0 = both correct or both incorrect
  #-1 = B correct A incorrect
  
  #for pa2
  #0 = both dissagree
  #2 = both agree
  
  A_yes = temp[3, 2]
  A_no = temp[1, 2]
  
  B_yes = temp[1, 2]
  B_no = temp[3, 2]
  
  both_yes = temp2[2, 2]
  both_no = temp2[1, 2]
  
  po = (both_no + both_yes) / nrow(x)
  yes = ((A_yes / nrow(x)) * (B_yes / nrow(x)))
  no = ((A_no / nrow(x)) * (B_no / nrow(x)))
  
  pe =  yes + no 
    
  output = ((po - pe) / (1 - pe))
  print(output)
  
}

kappa(test[ , 4:5])

####scoring data function####
#need to figure out what this code actually does
temp = sqldf("select *, 
  max(100.0 * (instr(sorted_JOL_TARGET, Recall_Response) > 0) * length(sorted_JOL_TARGET) / length(Recall_Response),
             100.0 * (instr(sorted_JOL_TARGET, Recall_Response) > 0) * length(Recall_Response) / length(sorted_JOL_TARGET))
             percent from dat")

#now make a scored column
dat$scored = as.numeric(dat$percent_match >= 100 & dat$percent_match < 200) #need to be able to set inclusion criteria

