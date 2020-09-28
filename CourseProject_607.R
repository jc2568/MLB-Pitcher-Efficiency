# course project question: does the spin rate of a pitch increase a pitchers
# strikeout percentage?


# load libraries
library(dplyr); library(ggplot2); library(cowplot); library(corrplot); library(ggpubr);
library(tidyr)


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


# histograms for strikeouts, innings pitched, quality starts, and games
i <- ggplot(x,aes(strikeouts)) + geom_histogram(color="black", fill="white", bins = 40, na.rm = TRUE) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

o <- ggplot(x,aes(innings_pitched)) + geom_histogram(color="black", fill="white", bins = 40,na.rm = TRUE) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

u <- ggplot(x,aes(quality_starts)) + geom_histogram(color="black", fill="white", bins = 40,na.rm = TRUE) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

p <- ggplot(x,aes(games)) + geom_histogram(color="black", fill="white", bins = 40,na.rm = TRUE) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

plot_grid(i,o,u,p)


# examine spin rates for pitches
q <- ggplot(x,aes(spin_slider))+geom_histogram(color="black", fill="white", bins = 40, na.rm = TRUE) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

r <- ggplot(x,aes(spin_changeup))+geom_histogram(color="black", fill="white", bins = 40, na.rm = TRUE) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

s <- ggplot(x,aes(spin_curve))+geom_histogram(color="black", fill="white", bins = 40, na.rm = TRUE) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

t <- ggplot(x,aes(spin_fastball))+geom_histogram(color="black", fill="white", bins = 40, na.rm = TRUE) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

plot_grid(q,r,s,t)


# subset the dataset for analysis
#y <- x[x$innings_pitched <= 100,c(12:19,22:35)]
y <- x[,c(12:19,22:35)]


# correlation table
corrplot(cor(y[,c(1:4,11:22)],use = "complete.obs"), type = "upper", tl.col = "black", 
         tl.srt = 45, tl.cex = 1.2, cl.cex = 1.2, col=colorRampPalette(c("blue","white","red"))(200))


# EDA plots
d <- y[,c(3,13,16,19,22)]
c <- d[,1:2]
c1 <- d[,c(1,3)]
c2 <- d[,c(1,4)]
c3 <- d[,c(1,5)]

c<-mutate(c,"spin_slider")
colnames(c) <- c("percent_strikeouts", "spin_rate", "pitch")
c1<-mutate(c1,"spin_changeup")
colnames(c1) <- c("percent_strikeouts", "spin_rate", "pitch")
c2<-mutate(c2,"spin_curve")
colnames(c2) <- c("percent_strikeouts", "spin_rate", "pitch")
c3<-mutate(c3,"spin_fastball")
colnames(c3) <- c("percent_strikeouts", "spin_rate", "pitch")
e<-rbind(c,c1,c2,c3)

ggplot(e,aes(spin_rate,percent_strikeouts,color=pitch,shape=pitch)) + geom_point(na.rm = TRUE) +
        theme_bw() + theme(axis.title = element_text(face = "bold",size = 20),
                           axis.text = element_text(size = 12),
                           legend.text = element_text(size = 12),
                           legend.title = element_text(face = "bold", size = 16)) +
        guides(colour = guide_legend(override.aes = list(size=3, stroke=1.5)))
        

a <- ggplot(y,aes(spin_slider,percent_strikeouts))+geom_point(na.rm = TRUE) +
        stat_smooth(method = "lm", se = FALSE, na.rm = TRUE, color = "red", size = 1.5) +
        stat_cor(label.x = 1750, label.y = 45, na.rm = TRUE, size = 5) +
        stat_regline_equation(label.x = 1750, label.y = 42, na.rm = TRUE, size = 5) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

b <- ggplot(y,aes(spin_changeup,percent_strikeouts))+geom_point(na.rm = TRUE) +
        stat_smooth(method = "lm", se = FALSE, na.rm = TRUE, color = "red", size = 1.5) +
        stat_cor(label.x = 2250, label.y = 45, na.rm = TRUE, size = 5) +
        stat_regline_equation(label.x = 2250, label.y = 42, na.rm = TRUE, size = 5) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

c <- ggplot(y,aes(spin_curve,percent_strikeouts))+geom_point(na.rm = TRUE) +
        stat_smooth(method = "lm", se = FALSE, na.rm = TRUE, color = "red", size = 1.5) +
        stat_cor(label.x = 1500, label.y = 45, na.rm = TRUE, size = 5) +
        stat_regline_equation(label.x = 1500, label.y = 42, na.rm = TRUE, size = 5) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

d <- ggplot(y,aes(spin_fastball,percent_strikeouts))+geom_point(na.rm = TRUE) +
        stat_smooth(method = "lm", se = FALSE, na.rm = TRUE, color = "red", size = 1.5) +
        stat_cor(label.x = 1750, label.y = 45, na.rm = TRUE, size = 5) +
        stat_regline_equation(label.x = 1750, label.y = 42, na.rm = TRUE, size = 5) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))


plot_grid(a,b,c,d)













