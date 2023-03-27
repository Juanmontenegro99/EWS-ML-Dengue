source("code/01exploredata.R")
library(dplyr)

#Read Rt
Rt <- read.csv("data/rt_dengue.csv")[20:795,]
google_W <- google_W[20:795,]
dengue_nal_W <- dengue_nal_W[20:795,]

#Convert digital data in binary data
lambda <- 25
variables <- colnames(google_W)[c(-1,-2,-19,-20)]
google_W1 <- google_W %>% mutate(across(.cols = variables,
                                       function(x) as.numeric(x>lambda)))
#Rt$R <- as.numeric(Rt$R>1)

#Calculate True Positive, False Positive, False Negative and True Negative.
# 1 TP; 2 FP; 3 FN; 4 TN.
cont = 1
lab <- matrix(0, nrow = nrow(Rt), ncol = length(variables)+1)
for(j in variables){
  for(i in 7:776){
    if(Rt$R[i]>1){
      if(sum(google_W1[(i-6):i, j])>3){
        lab[(i-6), cont] = 1
      }else{
        lab[(i-6), cont] = 3
      }
    }else{
      if(sum(google_W1[(i-6):i, j])>3){
        lab[(i-6), cont] = 2 
      }else{
        lab[(i-6), cont] = 4
      }
    }
  }
  cont = cont+1
}

for(i in 7:776){
  if(Rt$R[i]>1){
    if(sum(as.numeric(dengue_nal_W[(i-6):i, "CASES"]>1080))>3){
      lab[(i-6), cont] = 1
    }else{
      lab[(i-6), cont] = 3
    }
  }else{
    if(sum(as.numeric(dengue_nal_W[(i-6):i, "CASES"]>1080))>3){
      lab[(i-6), cont] = 2 
    }else{
      lab[(i-6), cont] = 4
    }
  }
}

#Order by more TP and less FP
Mconf <- matrix(0, nrow = 4, ncol = 17)
colnames(Mconf) <- c(variables, "cases")
rownames(Mconf) <- c("TP", "FP", "FN", "TN")
for(i in 1:4){Mconf[i,] = apply(lab==i,2,sum)}
Mconf <- data.frame(t(Mconf))
Mconf <- Mconf[order(-Mconf$TP, Mconf$FP),][1:6,]

#Subset the final variables
google_top <- rownames(Mconf)[1:5]
final_data <- google_W1[1:770,google_top]
final_data$cases <- as.numeric(dengue_nal_W[1:770, "CASES"]>1080)

#Calculate the Mt
gt <- rowSums(final_data)
Mt <- matrix(0, nrow = nrow(final_data), ncol = 1)
for(i in 1:nrow(Mt)){
  S = 1/(1+exp(-(gt[i])))
  Mt[i] = (2*S)-1
}

plot(Mt, type = "l")
