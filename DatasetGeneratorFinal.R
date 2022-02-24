#Run this code to produce a dataset which uses the parameters specified in the Params.csv file.
require(pacman)
pacman::p_load(pacman, ggplot2, rio, plyr)
p <- import("Params.csv")
Cat1 <- c(p[16,3], p[16,4]);Cat2 <- c(p[17,3], p[17,4], p[17,5], p[17,6], p[17,7]);Cat3 <- c(p[18,3], p[18,4],p[18,5], p[18,6])
cN <- c(p[16,2],p[17,2],p[18,2],p[22,2],p[23,2],p[24,2],p[25,2],p[26,2]);jitA <- as.numeric(p[3,2])
#Program
jit <- round(runif(1000, -(jitA), jitA))
i = 0
blank.base <- merge(data.frame(Cat1),data.frame(Cat2),all = TRUE)
blank.base <- merge(blank.base,data.frame(Cat3),all = TRUE)
blank.base["Num1"] <- 0
blank.base["Num2"] <- 0
blank.base["Num3"] <- 0
blank.base["Num4"] <- 0
blank.base["Num5"] <- 0
blank <- blank.base[rep(1:nrow(blank.base),each=250),]
colnames(blank) <- c("Cat1","Cat2","Cat3","Num1","Num2","Num3","Num4","Num5")
# make it a dataframe
df <- data.frame(blank)
# make the numeric columns numeric
df$Num1 <- sapply(df$Num1,as.numeric);df$Num2 <- sapply(df$Num2,as.numeric);df$Num3 <- sapply(df$Num3,as.numeric);df$Num4 <- sapply(df$Num4,as.numeric);df$Num5 <- sapply(df$Num5,as.numeric)
#Randomly distribute everything
df$Num1 <- round(runif(10000,0,100));df$Num2 <- round(runif(10000,0,100));df$Num3 <- round(runif(10000,0,100));df$Num4 <- round(runif(10000,0,100))
#First relationship
J <- jit[i];i <- i + 1; J2 <- jit[1000 - i];i <- i + 1
df$Num4[df$Cat3 == Cat3[1]] <- abs(round(runif(2500,min((as.numeric(p[31,2]) + J)%%100,100),min(as.numeric(p[31,3]) + J2, 100))))
J <- jit[i];i <- i + 1; J2 <- jit[1000 - i];i <- i + 1
df$Num4[df$Cat3 == Cat3[1]] <- abs(round(runif(2500,min((as.numeric(p[31,2]) + J)%%100,100),min(as.numeric(p[31,3]) + J2, 100))))
J <- jit[i];i <- i + 1; J2 <- jit[1000 - i];i <- i + 1
df$Num4[df$Cat3 == Cat3[2]] <- abs(round(runif(2500,min((as.numeric(p[31,4]) + J)%%100,100),min(as.numeric(p[31,5]) + J2, 100))))
J <- jit[i];i <- i + 1; J2 <- jit[1000 - i];i <- i + 1
df$Num4[df$Cat3 == Cat3[2]] <- abs(round(runif(2500,min((as.numeric(p[31,4]) + J)%%100,100),min(as.numeric(p[31,5]) + J2, 100))))
J <- jit[i];i <- i + 1; J2 <- jit[1000 - i];i <- i + 1
df$Num4[df$Cat3 == Cat3[3]] <- abs(round(runif(2500,min((as.numeric(p[31,6]) + J)%%100,100),min(as.numeric(p[31,7]) + J2, 100))))
J <- jit[i];i <- i + 1; J2 <- jit[1000 - i];i <- i + 1
df$Num4[df$Cat3 == Cat3[4]] <- abs(round(runif(2500,min((as.numeric(p[31,8]) + J)%%100,100),min(as.numeric(p[31,9]) + J2, 100))))
J <- jit[i];i <- i + 1; J2 <- jit[1000 - i];i <- i + 1
# Second relationship
df[df$Cat1 == Cat1[as.numeric(p[32,11])] & df$Cat3 == Cat3[1],]$Num2 <- round(runif(1250,max(as.numeric(p[32,2]) + J,0),min(as.numeric(p[32,3]) + J2,100)))
J <- jit[i];i <- i + 1; J2 <- jit[1000 - i];i <- i + 1
df[df$Cat1 == Cat1[as.numeric(p[32,11])] & df$Cat3 == Cat3[2],]$Num2 <- round(runif(1250,max(as.numeric(p[32,4]) + J,0),min(as.numeric(p[32,5]) + J2,100)))
J <- jit[i];i <- i + 1; J2 <- jit[1000 - i];i <- i + 1
df[df$Cat1 == Cat1[as.numeric(p[32,11])] & df$Cat3 == Cat3[3],]$Num2 <- round(runif(1250,max(as.numeric(p[32,6]) + J,0),min(as.numeric(p[32,7]) + J2,100)))
J <- jit[i];i <- i + 1; J2 <- jit[1000 - i];i <- i + 1
df[df$Cat1 == Cat1[as.numeric(p[32,11])] & df$Cat3 == Cat3[4],]$Num2 <- round(runif(1250,max(as.numeric(p[32,8]) + J,0),min(as.numeric(p[32,9]) + J2,100)))
J <- jit[i];i <- i + 1; J2 <- jit[1000 - i];i <- i + 1

##### Third relationship - parabolic relationship between Num4 and Num2 
score <- df[df$Cat1 == Cat1[as.numeric(p[34,3])] & df$Cat2 == Cat2[as.numeric(p[34,5])],]$Num4
score <- rnorm(1000, 1, 0.05) * score
df[df$Cat1 == Cat1[as.numeric(p[34,3])] & df$Cat2 == Cat2[as.numeric(p[34,5])],]$Num2 <- round(0.02*(score-50)^2+30 + J)
J <- jit[i];i <- i + 1; J2 <- jit[1000 - i];i <- i + 1

#Fourth relationship
score2 <- df[df$Cat1 == Cat1[as.numeric(p[36,3])] & df$Cat2 == Cat2[as.numeric(p[36,5])] & df$Cat3 == Cat3[as.numeric(p[36,7])],]$Num2
df[df$Cat1 == Cat1[as.numeric(p[36,3])] & df$Cat2 == Cat2[as.numeric(p[36,5])] & df$Cat3 == Cat3[as.numeric(p[36,7])],]$Num1 <- round(abs(2*jitter(score2,100)))
cor(x = df$Num1[df$Cat1 == Cat1[as.numeric(p[36,3])] & df$Cat2 == Cat2[as.numeric(p[36,5])] & df$Cat3 == Cat3[as.numeric(p[36,7])]], y = score2)

#Fifth relationship
ssA <- df[df$Cat2 == Cat2[as.numeric(p[38,3])],]$Num3
ssC <- df[df$Cat2 == Cat2[as.numeric(p[38,5])],]$Num3

df[df$Cat2 == Cat2[as.numeric(p[38,3])],]$Num1 <- round(((100-jitter(ssA,50)) *rnorm(250,1, as.numeric(p[38,7])))%%100)
df[df$Cat2 == Cat2[as.numeric(p[38,5])],]$Num1 <- round(((100-jitter(ssC,50)) *rnorm(250,1, as.numeric(p[38,7])))%%100)

#calculate overall scores
df$Num5 <- round(0.25*df$Num1 + 0.25*df$Num2 + 0.25*df$Num3 + 0.25*df$Num4)

#Sixth relationship
Num1.1 <- df[df$Cat1 == Cat1[2],]$Num1
Num2.1 <- df[df$Cat1 == Cat1[2],]$Num2
Num3.1 <- df[df$Cat1 == Cat1[2],]$Num3
Num4.1 <- df[df$Cat1 == Cat1[2],]$Num4

df[df$Cat1 == Cat1[as.numeric(p[40,3])],]$Num5 <- round((as.numeric(p[40,5]))*Num1.1 + (as.numeric(p[40,7]))*Num2.1 + (as.numeric(p[40,9]))*Num4.1 + (as.numeric(p[40,11]))*Num3.1)

#Seventh Relationship - boost one value in CatD by a specified amount
soD <- df[df$Cat2 == Cat2[as.numeric(p[42,3])],]$Num5
# write a function to pass to sapply
min100 <- function(x){min(x,100)}
Amount <- 1 + as.numeric(p[42,5]) /100
df[df$Cat2 == Cat2[4],]$Num5 <- round(sapply(Amount*soD + as.numeric(p[42,5]),min100))
colnames(df) <- c(p[16,2],p[17,2],p[18,2],p[22,2],p[23,2],p[24,2],p[25,2],p[26,2])
if(p[7,2] != "")
{
  df <- df[sample(nrow(df)),]
}
export(df, "cwdata.csv")
