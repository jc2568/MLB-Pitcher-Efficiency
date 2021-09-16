# course project question: does the spin rate of a pitch increase a pitchers
# strikeout percentage?


## load libraries
#library(dplyr); library(ggplot2); library(cowplot); library(corrplot); library(ggpubr);
#library(tidyr)
library(dplyr); library(ggplot2); library(cowplot); library(corrplot); 
library(ggpubr); library(factoextra); library(e1071); library(tidyverse);
library(caret)


## Data Loading and Summary
# remove variables saved in environment
rm(list = ls())

# load dataset and rename columns
x <- read.csv("stats.csv")

# subset dataset by pitch spin rates
df  <- x[,c(14,26,29,32,35)]

# summary statistics for the dataset
summary(df)


## EDA
# subset the dataset for analysis
y <- x[,c(12:19,22:35)]

# correlation table
corrplot(cor(y[,c(1:4,11:22)],use = "complete.obs"), type = "upper", tl.col = "black", 
         tl.srt = 45, tl.cex = 1, cl.cex = 1, col=colorRampPalette(c("blue","white","red"))(200))


ggplot(df,aes(p_k_percent)) + geom_histogram(color="black", fill="white", bins = 40,na.rm = TRUE) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

# examine spin rates for pitches
q <- ggplot(df,aes(sl_avg_spin))+geom_histogram(color="black", fill="white", bins = 40, na.rm = TRUE) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

r <- ggplot(df,aes(ch_avg_spin))+geom_histogram(color="black", fill="white", bins = 40, na.rm = TRUE) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

s <- ggplot(df,aes(cu_avg_spin))+geom_histogram(color="black", fill="white", bins = 40, na.rm = TRUE) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

t <- ggplot(df,aes(fastball_avg_spin))+geom_histogram(color="black", fill="white", bins = 40, na.rm = TRUE) +
        theme_bw()+theme(axis.title = element_text(face = "bold",size = 20),
                         axis.text = element_text(size = 12))

plot_grid(q,r,s,t)

# eliminate NAs from dataset and replace with 0
df[is.na(df)] <- 0

# convert to dataframe
df  <- data.frame(df)

# scale data
df <- scale(df)

# determine the number of clusters using the elbow method
fviz_nbclust(df, kmeans, method = "wss")

# cluster analysis
k <- kmeans(df, centers = 5, nstart = 25)
fviz_cluster(k, data = df)

# subset dataset by pitch spin rates
df  <- x[1:706,c(14,26,29,32,35)]

# replace NAs with average of each feature
df$sl_avg_spin[is.na(df$sl_avg_spin)] <- 2401
df$ch_avg_spin[is.na(df$ch_avg_spin)] <- 1774
df$cu_avg_spin[is.na(df$cu_avg_spin)] <- 2490
df$fastball_avg_spin[is.na(df$fastball_avg_spin)] <- 2259

# convert to dataframe
df  <- data.frame(df)

# scale data
df1 <- scale(df)

# cluster analysis
k1 <- kmeans(df1, centers = 4, nstart = 25)
fviz_cluster(k1, data = df1)

# cluster analysis
k1 <- kmeans(df1, centers = 3, nstart = 25)
fviz_cluster(k1, data = df1)

# cluster analysis
k1 <- kmeans(df1, centers = 2, nstart = 25)
fviz_cluster(k1, data = df1)

# use cmeans to resolve the overlap between clusters 1 and 2
k2 <- cmeans(df1, centers = 2)
fviz_cluster(list(data = df1, cluster=k2$cluster), 
             ellipse.type = "norm",
             ellipse.level = 0.85,
             palette = "jco",
             ggtheme = theme_minimal())

# convert to dataframe
df2  <- data.frame(df)

# prepare dataset for analysis
c  <- df2[,1:2]
c1 <- df2[,c(1,3)]
c2 <- df2[,c(1,4)]
c3 <- df2[,c(1,5)]

c<-mutate(c,"spin_slider")
colnames(c) <- c("percent_strikeouts", "spin_rate", "pitch")
c1<-mutate(c1,"spin_changeup")
colnames(c1) <- c("percent_strikeouts", "spin_rate", "pitch")
c2<-mutate(c2,"spin_curve")
colnames(c2) <- c("percent_strikeouts", "spin_rate", "pitch")
c3<-mutate(c3,"spin_fastball")
colnames(c3) <- c("percent_strikeouts", "spin_rate", "pitch")
e<-rbind(c,c1,c2,c3)

# assign each pitch to a category; categories were determined using the percent
# strikeout quartiles
summary(e)
e <- mutate(e,pitch_assignment = 0)
e$pitch_assignment[e$percent_strikeouts <= mean(e$percent_strikeouts)] <- "Below_avg"
e$pitch_assignment[e$percent_strikeouts > mean(e$percent_strikeouts)] <- "Above_avg"

# construct final dataset
sub_e <- e[,c(2:4)]
final_e <- sub_e
final_e$spin_rate <- as.factor(final_e$spin_rate)
final_e$pitch <- as.factor(final_e$pitch)
final_e$pitch_assignment <- as.factor(final_e$pitch_assignment)


# according to ref (###) the createDataPartition does a stratified random split
# dividing the data into 75% training and 25% testing
trn <- createDataPartition(y = final_e$pitch_assignment, p = 0.85, list = FALSE)
training <- final_e[trn,]
test <- final_e[-trn,]


prop.table(table(training$pitch_assignment))
prop.table(table(test$pitch_assignment))
table(training$pitch_assignment)
table(test$pitch_assignment)
nrow(training)
nrow(test)


# train the model
Naive_Bayes_Model <- naiveBayes(pitch_assignment ~., data=training)


# predicitons
NB_Predictions <- predict(Naive_Bayes_Model,test)
table(NB_Predictions,test$pitch_assignment)