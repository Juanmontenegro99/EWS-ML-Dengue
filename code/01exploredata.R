#Import libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)

#Read datasets
dengue <- read.csv("data/data_dengue.csv")
google <- read.csv("data/daily_google.csv", sep = ";")

#Preprocessing functions
data_dengue <- function(dengue){
dengue_nal <- data.frame(table(dengue$FEC_NOT))
colnames(dengue_nal) <- c("DATE", "CASES")
dengue_nal$DATE <- as.Date(dengue_nal$DATE)
#dengue_nal$YEAR_WEEK <- paste(year(dengue_nal$DATE), week(dengue_nal$DATE), sep="-")
dengue_nal$YEAR <- year(dengue_nal$DATE)
dengue_nal$WEEK <- week(dengue_nal$DATE)
dengue_nal_W <- dengue_nal %>% group_by(WEEK, YEAR) %>% 
  summarise(CASES = sum(CASES)) %>% ungroup()
dengue_nal_W$DATE <- as.Date(paste(dengue_nal_W$YEAR, dengue_nal_W$WEEK, 1, sep="-"), "%Y-%U-%u")
dengue_nal_W <- dengue_nal_W[order(dengue_nal_W$DATE),]

}

data_google <- function(google){
google$date <- as.Date(google$date, "%d/%m/%Y")
#google$YEAR_WEEK <- paste(year(google$date), week(google$date), sep="-")
google$YEAR <- year(google$date)
google$WEEK <- week(google$date)
google <- google[,-1]

google_W <- google %>% group_by(WEEK, YEAR) %>% summarise_all(mean)
google_W$YEAR_WEEK <- paste(google_W$YEAR, google_W$WEEK, sep="-")
google_W$DATE <- as.Date(paste(google_W$YEAR, google_W$WEEK, 1, sep="-"), "%Y-%U-%u")
google_W <- google_W[order(google_W$DATE),]
}

#Preprocessing data
dengue_nal_W <- data_dengue(dengue)
google_W <- data_google(google)

#Plot Digital data vs Cases
ggplot()+
  geom_line(aes(x=google_W$DATE, y=google_W$dengue), color="gray")+ 
  geom_line(aes(x=dengue_nal_W$DATE, y=dengue_nal_W$CASES/50), color="red")+
  scale_y_continuous(name = "Google_dengue",
                     sec.axis = sec_axis(~.*50, name="Cases"))
ggplot()+
  geom_line(aes(x=google_W$DATE, y=google_W$sintomas.dengue), color="gray")+ 
  geom_line(aes(x=dengue_nal_W$DATE, y=dengue_nal_W$CASES/50), color="red")+
  scale_y_continuous(name = "Google_sintomas_dengue",
                     sec.axis = sec_axis(~.*50, name="Cases"))
ggplot()+
  geom_line(aes(x=google_W$DATE, y=google_W$dolor.de.cabeza), color="gray")+ 
  geom_line(aes(x=dengue_nal_W$DATE, y=dengue_nal_W$CASES/50), color="red")+
  scale_y_continuous(name = "Google_dolor_cabeza",
                     sec.axis = sec_axis(~.*50, name="Cases"))
ggplot()+
  geom_line(aes(x=google_W$DATE, y=google_W$migrana), color="gray")+ 
  geom_line(aes(x=dengue_nal_W$DATE, y=dengue_nal_W$CASES/50), color="red")+
  scale_y_continuous(name = "Google_migraña",
                     sec.axis = sec_axis(~.*50, name="Cases"))
ggplot()+
  geom_line(aes(x=google_W$DATE, y=google_W$dolor.muscular), color="gray")+ 
  geom_line(aes(x=dengue_nal_W$DATE, y=dengue_nal_W$CASES/50), color="red")+
  scale_y_continuous(name = "Google_dolor_muscular",
                     sec.axis = sec_axis(~.*50, name="Cases"))
ggplot()+
  geom_line(aes(x=google_W$DATE, y=google_W$dolor.ojos), color="gray")+ 
  geom_line(aes(x=dengue_nal_W$DATE, y=dengue_nal_W$CASES/50), color="red")+
  scale_y_continuous(name = "Google_dolor_ojos",
                     sec.axis = sec_axis(~.*50, name="Cases"))
ggplot()+
  geom_line(aes(x=google_W$DATE, y=google_W$nauseas), color="gray")+ 
  geom_line(aes(x=dengue_nal_W$DATE, y=dengue_nal_W$CASES/50), color="red")+
  scale_y_continuous(name = "Google_nauseas",
                     sec.axis = sec_axis(~.*50, name="Cases"))
ggplot()+
  geom_line(aes(x=google_W$DATE, y=google_W$vomito), color="gray")+ 
  geom_line(aes(x=dengue_nal_W$DATE, y=dengue_nal_W$CASES/50), color="red")+
  scale_y_continuous(name = "Google_vomito",
                     sec.axis = sec_axis(~.*50, name="Cases"))
ggplot()+
  geom_line(aes(x=google_W$DATE, y=google_W$mosquito), color="gray")+ 
  geom_line(aes(x=dengue_nal_W$DATE, y=dengue_nal_W$CASES/50), color="red")+
  scale_y_continuous(name = "Google_mosquito",
                     sec.axis = sec_axis(~.*50, name="Cases"))
ggplot()+
  geom_line(aes(x=google_W$DATE, y=google_W$picadura.mosquito), color="gray")+ 
  geom_line(aes(x=dengue_nal_W$DATE, y=dengue_nal_W$CASES/50), color="red")+
  scale_y_continuous(name = "Google_picadura_mosquito",
                     sec.axis = sec_axis(~.*50, name="Cases"))
