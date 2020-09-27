# course project question: does the spin rate of a pitch increase a pitchers
# strikeout percentage?


# load libraries
library(dplyr); library(ggplot2); library(cowplot); library(corrplot); library(ggpubr)


# remove variables saved in environment
rm(list = ls())


# load dataset and rename columns
x <- read.csv("stats.csv")
colnames(x) <- c("last_name", "first_name", "year", "player_age", "games", "innings_pitched", "hits", 
                 "single", "double", "triple", "home_run", "strikeouts", "walks", "percent_strikeouts",
                 "percent_walks", "batting_avg", "slg_percent", "on_base_percent", "on_base_plus_slug",
                 "wins", "loss", "quality_starts", "starts", "no_slider", "speed_slider", "spin_slider", 
                 "no_changeup", "speed_changeup", "spin_changeup", "no_curve", "speed_curve", "spin_curve",
                 "no_fastball", "speed_fastball", 'spin_fastball')


# summary statistics
summary(x)


# evaluate range of starts for all pitchers
i<-ggplot(x,aes(strikeouts))+geom_histogram(color="black", fill="white", bins = 40, na.rm = TRUE)
o<-ggplot(x,aes(innings_pitched)) + geom_histogram(color="black", fill="white", bins = 40,na.rm = TRUE)
u<-ggplot(x,aes(quality_starts)) + geom_histogram(color="black", fill="white", bins = 40,na.rm = TRUE)
p<-ggplot(x,aes(games)) + geom_histogram(color="black", fill="white", bins = 40,na.rm = TRUE)

plot_grid(i,o,u,p)


q <- ggplot(x,aes(spin_slider))+geom_histogram(color="black", fill="white", bins = 40, na.rm = TRUE)
r <- ggplot(x,aes(spin_changeup))+geom_histogram(color="black", fill="white", bins = 40, na.rm = TRUE)
s <- ggplot(x,aes(spin_curve))+geom_histogram(color="black", fill="white", bins = 40, na.rm = TRUE)
t <- ggplot(x,aes(spin_fastball))+geom_histogram(color="black", fill="white", bins = 40, na.rm = TRUE)

plot_grid(q,r,s,t)


# subset the dataset for analysis
#y <- x[x$innings_pitched <= 100,c(12:19,22:35)]
y <- x[,c(12:19,22:35)]


# correlation table
corrplot(cor(y,use = "complete.obs"), type = "upper", tl.col = "black", 
         tl.srt = 45, tl.cex = 1.2, cl.cex = 1.2, col=colorRampPalette(c("blue","white","red"))(200))



# EDA plots
a <- ggplot(y,aes(spin_slider,percent_strikeouts))+geom_point(na.rm = TRUE) +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE,na.rm = TRUE)

b <- ggplot(y,aes(spin_changeup,percent_strikeouts))+geom_point(na.rm = TRUE) +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE,na.rm = TRUE)

c <- ggplot(y,aes(spin_curve,percent_strikeouts))+geom_point(na.rm = TRUE) +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE,na.rm = TRUE)

d <- ggplot(y,aes(spin_fastball,percent_strikeouts))+geom_point(na.rm = TRUE) +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE,na.rm = TRUE)

plot_grid(a,b,c,d)



