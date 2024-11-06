#STAT0023 ICA2
#Group 36"
#Student numbers: 22012694 22022606

# Input the data and separate them to modelling data and predicting data
anemia <- read.csv("AnemiaData.csv")
train <- anemia[anemia$Haemoglobin!=-1, ]
test <- anemia[anemia$Haemoglobin==-1, ]
library(ggplot2)

#EDA

#Choose similar covariate
length(train[train$CleanWater == "No" & train$TreatedWater == "Yes", 1]) 
#TreatWater is included in CleanWater, prefer CleanWater
length(train[train$AgricLandOwn == "Yes" & train$AgricArea == 0, 1]) 
#Num of household own AgricArea less than 1
table(train$AgricLandOwn)

#Scatter plot of Hb and numeric covariate
plot1 <- ggplot(train, aes(x = Age, y = Haemoglobin)) +
  geom_point(alpha = 0.2) +  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Haemoglobin by Age",
       x = "Age",
       y = "Haemoglobin")
print(plot1)
plot2 <- ggplot(train, aes(x = HHSize, y = Haemoglobin)) +
  geom_point(alpha = 0.2) +  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Haemoglobin by HHSize",
       x = "HHSize",
       y = "Haemoglobin")
print(plot2)
plot3 <- ggplot(train, aes(x = HHUnder5s, y = Haemoglobin)) +
  geom_point(alpha = 0.2) +  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Haemoglobin by HHUnder5s",
       x = "HHUnder5s",
       y = "Haemoglobin")
print(plot3)
plot4 <- ggplot(train, aes(x = TotalChildren, y = Haemoglobin)) +
  geom_point(alpha = 0.2) +  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Haemoglobin by TotalChildren",
       x = "TotalChildren",
       y = "Haemoglobin")
print(plot4)
png("WealthScore.png")
plot5 <- ggplot(train, aes(x = WealthScore, y = Haemoglobin)) +
  geom_point(alpha = 0.2) +  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Haemoglobin by WealthScore",
       x = "WealthScore",
       y = "Haemoglobin")
print(plot5)
dev.off()
plot6 <- ggplot(train, aes(x = AgricArea, y = Haemoglobin)) +
  geom_point(alpha = 0.2) +  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Haemoglobin by AgricArea",
       x = "AgricArea",
       y = "Haemoglobin")
print(plot6)

#boxplot of Hb and categorical covariate
#Health condition
par(mfrow = c(1,2))
boxplot(Haemoglobin ~ RecentBirth, data = train, main = "Boxplot of Haemoglobin by RecentBirth",
        xlab = "RecentBirth", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
boxplot(Haemoglobin ~ Pregnant, data = train, main = "Boxplot of Haemoglobin by Pregnant",
        xlab = "Pregnant", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
#Household condition
par(mfrow = c(2,2))
boxplot(Haemoglobin ~ CleanWater, data = train, main = "Boxplot of Haemoglobin by CleanWater",
        xlab = "CleanWater", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
boxplot(Haemoglobin ~ TreatedWater, data = train, main = "Boxplot of Haemoglobin by TreatedWater",
        xlab = "TreatedWater", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
boxplot(Haemoglobin ~ Electricity, data = train, main = "Boxplot of Haemoglobin by Electricity",
        xlab = "Electricity", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
boxplot(Haemoglobin ~ Toilet, data = train, main = "Boxplot of Haemoglobin by Toilet",
        xlab = "Toilet", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
#Agric and animals
par(mfrow = c(3,3))
boxplot(Haemoglobin ~ AgricLandOwn, data = train, main = "Boxplot of Haemoglobin by AgriclandOwn",
        xlab = "AgriclandOwn", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
boxplot(Haemoglobin ~ AnimCart, data = train, main = "Boxplot of Haemoglobin by AnimCart",
        xlab = "AnimCart", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
boxplot(Haemoglobin ~ Cows, data = train, main = "Boxplot of Haemoglobin by Cows",
        xlab = "Cows", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
boxplot(Haemoglobin ~ Horses, data = train, main = "Boxplot of Haemoglobin by Horses",
        xlab = "Horses", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
boxplot(Haemoglobin ~ Goats, data = train, main = "Boxplot of Haemoglobin by Goats",
        xlab = "Goats", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
boxplot(Haemoglobin ~ Sheep, data = train, main = "Boxplot of Haemoglobin by Sheep",
        xlab = "Sheep", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
boxplot(Haemoglobin ~ Chickens, data = train, main = "Boxplot of Haemoglobin by Chickens",
        xlab = "Chickens", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
#Education
par(mfrow = c(1,2))
boxplot(Haemoglobin ~ Education, data = train, main = "Boxplot of Haemoglobin by Education",
        xlab = "Education", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
boxplot(Haemoglobin ~ HHEducation, data = train, main = "Boxplot of Haemoglobin by HHEducation",
        xlab = "HHEducation", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
#Demographic
png("Boxplot_1.png")
par(mfrow = c(2,2))
boxplot(Haemoglobin ~ Province, data = train, main = "Boxplot of Haemoglobin by Province",
        xlab = "Province", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
boxplot(Haemoglobin ~ Region, data = train, main = "Boxplot of Haemoglobin by Region",
        xlab = "Region", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
boxplot(Haemoglobin ~ Ethnicity, data = train, main = "Boxplot of Haemoglobin by Ethnicity",
        xlab = "Ethnicity", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
boxplot(Haemoglobin ~ Rural, data = train, main = "Boxplot of Haemoglobin by Rural",
        xlab = "Rural", ylab = "Haemoglobin", col = c("lightblue", "salmon"))
dev.off()

#rearrange age variables into three catalogs
train$AgeGroup <- cut(train$Age, breaks = c(15, 18, 40, 49), include.lowest = TRUE,
                      labels = c("15-18", "19-40", "41-49"))
par(mfrow = c(1,1))
boxplot(WealthScore ~ AgeGroup, data = train, main = "WealthScore and AgeGroup",
        xlab = "AgeGroup", ylab = "WealthScore", col = c("lightblue", "salmon"))

#The possible correlation with WealthScore
#boxplot
png("WealthScore_corr.png")
par(mfrow = c(2,2))
boxplot(WealthScore ~ Electricity, data = train, main = "WealthScore and Electricity Availability",
        xlab = "Electricity", ylab = "WealthScore", col = c("lightblue", "salmon"))
boxplot(WealthScore ~ Toilet, data = train, main = "WealthScore and Toilet Availability",
        xlab = "Toilet", ylab = "WealthScore", col = c("lightblue", "salmon"))
boxplot(WealthScore ~ CleanWater, data = train, main = "WealthScore and CleanWater Availability",
        xlab = "CleanWater", ylab = "WealthScore", col = c("lightblue", "salmon"))
boxplot(WealthScore ~ TreatedWater, data = train, main = "WealthScore and TreatedWater Availability",
        xlab = "TreatedWater", ylab = "WealthScore", col = c("lightblue", "salmon"))
dev.off()
#correlation coefficient matrix
train$WealthScore2 <- as.numeric(as.factor(train$WealthScore))
train$Electricity2 <- as.numeric(as.factor(train$Electricity))
train$CleanWater2 <- as.numeric(as.factor(train$CleanWater))
train$TreatedWater2 <- as.numeric(as.factor(train$TreatedWater))
cor_matrix1 <- cor(train[c("WealthScore2", "Electricity2", "CleanWater2", "TreatedWater2")],
                   use = "complete.obs")
print(cor_matrix1)
train$WealthScore2 <- NULL #delete the new covariate from dataset
train$Electricity2 <- NULL
train$CleanWater2 <- NULL
train$TreatedWater2 <- NULL

#The possible correlation among demograpgic covariate
#correlation coefficient matrix
train$Province2 <- as.numeric(as.factor(train$Province))
train$Region2 <- as.numeric(as.factor(train$Region))
train$Ethnicity2 <- as.numeric(as.factor(train$Ethnicity))
train$Rural2 <- as.numeric(as.factor(train$Rural))
cor_matrix2 <- cor(train[c("Province2", "Region2", "Ethnicity2", "Rural2")], use = "complete.obs")
print(cor_matrix2)
train$Province2 <- NULL
train$Region2 <- NULL
train$Ethnicity2 <- NULL
train$Rural2 <- NULL

#univariate analysis of Hb
summary(train$Haemoglobin)
length(train[train[,2]<6.5,1]) #Extremely low Hb
length(train[train[,2]<11,1]) 
par(mfrow = c(1,1))
hist(train$Haemoglobin, breaks = 25)

#regroup province using hierarchical clustering
NumVars <- c(6,21,22,23)
Summaries <- aggregate(train[,NumVars],by=list(train$Province),FUN=function(x) {c(Mean=mean(x), SD=sd(x))}) 
rownames (Summaries) <- Summaries[,1]
Summaries <- scale(Summaries[,-1])
Distances <- dist(Summaries)
ClusTree <- hclust(Distances, method="complete")

png("Hierarchical_clustering.png")
plot(ClusTree, xlab="group", ylab="Separation")
abline(h=5, col="red", lty=2)
dev.off()

NewGroups <- paste("ProvGrp", cutree(ClusTree, h=5),sep="")
table(rownames(Summaries), NewGroups)
province_group_data <- data.frame(Province = rownames(Summaries), ProvinceGroup5 = NewGroups)
train <- merge(train, province_group_data, by = "Province", all.x = TRUE)
boxplot(Haemoglobin ~ ProvinceGroup5, data = train, main = "Haemoglobin and Province in group",
        xlab = "Province Group", ylab = "Haemoglobin", col = c("lightblue", "salmon"))

#PCA of all numeric covariate for EDA
pc <- prcomp(train[,c(4,6,7,21,22,23)],scale. = TRUE)
print(pc)
summary(pc)

png("pca_biplot.png")
biplot(pc,col=c("skyblue3","darkred"),xlabs = rownames(train),cex=c(0.75,1),lwd=2, xlim = c(-0.08, 0.04))
dev.off()

#potential interaction
#WealthScore and HHEducation
png("interaction_1.png")
custom_colors1 <- c("orange", "green3", "blue")  
interaction_plot1 <- ggplot(train, aes(x = WealthScore, y = Haemoglobin, color = HHEducation)) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.6) +
  geom_smooth(method = "lm", aes(group = HHEducation), se = FALSE) +
  scale_color_manual(values = custom_colors1) +  
  labs(title = "Interaction of WealthScore and HHEducation on Haemoglobin", x = "WealthScore", 
       y = "Haemoglobin") + 
  theme_minimal() 
print(interaction_plot1)
dev.off()
#Pregnant and RecentBirth
png("interaction_2.png")
interaction_plot2 <- ggplot(train, aes(x = RecentBirth, y = Haemoglobin, color = Pregnant)) +
  geom_point() +  # Plot points
  geom_line(aes(group = Pregnant)) +  # Plot lines
  labs(x = "Recent Birth", y = "Haemoglobin", color = "Pregnant", 
       title = "Interaction Plot for Recent Birth and Pregnancy in Terms of Haemoglobin")
print(interaction_plot2)
dev.off()
#Rural and Ethnicity
png("interaction_3.png")
interaction_plot3 <- ggplot(train, aes(x = Rural, y = Haemoglobin, color = Ethnicity)) +
  geom_point() +  
  geom_line(aes(group = Ethnicity)) +  
  labs(x = "Rural", y = "Haemoglobin", color = "Ethnicity",
       title = "Interaction Plot of Rural and Ethnicity in Terms of Haemoglobin")
facet_wrap(~Rural)
print(interaction_plot3)
dev.off()
#TotalChildren and AgeGroup
png("interaction_4.png")
custom_colors2 <- c("lightgreen", "darkred", "skyblue2")  
interaction_plot4 <- ggplot(train, aes(x = TotalChildren, y = Haemoglobin, color = AgeGroup)) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.6) +
  geom_smooth(method = "lm", aes(group = AgeGroup), se = FALSE) +
  scale_color_manual(values = custom_colors2) +  
  labs(title = "Interaction of TotalChildren and AgeGroup on Haemoglobin", 
       x = "TotalChildren", y = "Haemoglobin") +
  theme_minimal()
print(interaction_plot4)
dev.off()
#WealthScore and Goats
interaction_plot5 <- ggplot(train, aes(x = WealthScore, y = Haemoglobin, color = Goats)) + 
  geom_point(position = position_jitter(width = 0.1), alpha = 0.6) +  
  geom_smooth(method = "lm", aes(group = Goats), se = FALSE) + 
  labs(title = "Interaction of WealthScore and Goats on Haemoglobin", x = "WealthScore",
       y = "Haemoglobin") +   theme_minimal() 
print(interaction_plot5)


#Model building
# Histogram
png("Histogram.png")
histogram <- ggplot(train, aes(x = Haemoglobin)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(x = "Haemoglobin", y = "Frequency", title = "Histogram of Haemoglobin")
print(histogram)
dev.off()
# QQ plot
qqplot <- ggplot(train, aes(sample = Haemoglobin)) +
  geom_qq() +
  geom_abline(intercept = mean(train$Haemoglobin), slope = sd(train$Haemoglobin), color = "darkred") +
  labs(title = "QQ Plot of Haemoglobin")
print(qqplot)

#Initial model
lm0_groupprov <- lm(Haemoglobin~.-ID-Age-Province,data=train)
summary(lm0_groupprov)

#Assumption checking
par(mfrow=c(2,2))
plot(lm0_groupprov)
train$ProvinceGroup5 <- NULL

#Compare R sq with the ungrouped Province
lm0<- lm(Haemoglobin~.-ID-Age,data=train)
summary(lm0)

png("lm0_assumption.png")
par(mfrow=c(2,2))
plot(lm0)
dev.off()

#covariate backwards elimination
#overlaping of data in Province and Region
lm1 <- lm(Haemoglobin~. -ID - Age - Province ,data=train)
summary(lm1)
lm2 <- lm(Haemoglobin~. -ID - Age - Region ,data=train)
summary(lm2) #lm2 has higher R sq
#Remove covariate with very big p value and not significant as mentioned in EDA
lm3 <- lm(Haemoglobin~. - ID - Age - HHSize - CleanWater - Toilet - Electricity 
          - BikeScootCar - AnimCart - Horses - Education - Cows - Sheep - Chickens - Region ,data=train)
summary(lm3)
anova(lm0,lm3)
plot(lm3)

#Remove covariate with relatively large p value step by step
lm4 <- update(lm3,.~.- TreatedWater)
summary(lm4)
anova(lm3,lm4)

lm5 <- update(lm4,.~.- AgricArea)
summary(lm5)
anova(lm4,lm5)

lm6 <- update(lm5,.~.- AgricLandOwn)
summary(lm6)
anova(lm5,lm6)

lm7 <- update(lm6,.~.- Goats)
summary(lm7)
anova(lm6,lm7)

plot(lm7)
#Adding potential interaction
lm8 <- update(lm7,.~. + Ethnicity*Rural + RecentBirth*Pregnant*AgeGroup*TotalChildren 
              + HHEducation*WealthScore)
summary(lm8)
anova(lm7,lm8)

lm9 <- update(lm7,.~. + Ethnicity*Rural + RecentBirth*Pregnant + HHEducation*WealthScore
              + AgeGroup*TotalChildren)
summary(lm9)
anova(lm8,lm9)

png("lm9_assumption.png")
par(mfrow=c(2,2))
plot(lm9)
dev.off()

#Final lm model
lm_final <- lm(Haemoglobin~RecentBirth+HHUnder5s+Rural+TotalChildren+WealthScore
               +Pregnant+HHEducation+Ethnicity+
                 AgeGroup+Province+Rural*Ethnicity+RecentBirth*Pregnant+WealthScore*HHEducation
               +TotalChildren*AgeGroup,data=train)
summary(lm_final)

#Try glm
glm1 <- glm(Haemoglobin ~ RecentBirth+HHUnder5s+Rural+TotalChildren+WealthScore
            +Pregnant+HHEducation+Ethnicity+
              AgeGroup+Province+Rural*Ethnicity+RecentBirth*Pregnant+WealthScore*HHEducation
            +TotalChildren*AgeGroup, data = train, family=inverse.gaussian())
summary(glm1)
glm2 <- glm(Haemoglobin ~ RecentBirth+HHUnder5s+Rural+TotalChildren+WealthScore
            +Pregnant+HHEducation+Ethnicity+
              AgeGroup+Province+Rural*Ethnicity+RecentBirth*Pregnant+WealthScore
            *HHEducation+TotalChildren*AgeGroup, data = train, family=Gamma())
summary(glm2)
plot(glm1)

#Pearson's residual
ps <- sum(resid(glm1,type="pearson")^2 ) / glm1$df.residual
print(ps)


#Prediction
#Adjust covariate for test data
test$AgeGroup <- cut(test$Age, breaks = c(15, 18, 40, 49), include.lowest = TRUE, 
                     labels = c("15-18", "19-40", "41-49"))

prediction <- predict(lm9, newdata = test, se.fit = TRUE)
# Extract the standard errors and compute variance of predictions
se_pred <- prediction$se.fit
var_pred<- se_pred^2
residual_var <- summary(lm9)$sigma^2  
total_variance <- residual_var + var_pred
# Standard deviation of the prediction error
sde_pred <- sqrt(total_variance)
test$pred_error <- sde_pred
test$pred_haemoglobin <- predict(lm9, newdata = test)

output <- test[c("ID", "pred_haemoglobin")]
output$standard_error <- test$pred_error
write.table(output, "ICA2_Group36_pred.dat", sep = " ", row.names = FALSE, col.names = FALSE, quote = FALSE)

