#Import libraries
library(R0)
library(dplyr)
library(zoo)
library(lubridate)
library(ggplot2)
source("code/01exploredata.R")

#Read dataset
dengue<- read.csv("data/data_dengue.csv")

#Preprocessing data
dengue_nal_W <- data_dengue(dengue)

#Create Moment Generation Function (MGF)
mGT<-R0::generation.time("gamma", c(16.27, 20.3))
#mGT1 <- function(t)((sign(1 - 13.57*t)*abs(1 - 13.57*t)^(-4.3))*(
#  sign(1 - 2.7*t)*abs(1 - 2.7*t)^(-16))*((1-t)^-1)*((1-t)^-1))
#mGT$GT <- lapply(mGT$time, mGT1)

#Calculate and save Rt
TD <- R0::est.R0.TD(dengue_nal_W$CASES, mGT, begin=1, end=795, nsim=1000, correct=FALSE)
df.static <- data.frame(R= TD$R, ymin=TD$conf.int[1],
                        ymax=TD$conf.int[2], date=dengue_nal_W$DATE, 
                        incidence=dengue_nal_W$CASES, dt.inicio =dengue_nal_W$DATE, 
                        type="temperature-independent")

#Plot Rt in time
ggplot(data=df.static, aes(x=dt.inicio, y=R, ymin=lower, ymax=upper)) +
  geom_ribbon(aes(alpha=0.5), show.legend = F) +
  geom_line() +
  geom_line(aes(y=1), linetype=2) +
  geom_line(data=df.static, aes(x=dt.inicio, y=incidence/2000), color="red") +
  scale_y_continuous(
    
    # Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*2000, name="Second Axis")
  )

#Save Rt
write.csv(df.static, "data/rt_dengue.csv", row.names = F)