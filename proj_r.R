############## PACKAGES ################

library(tidyverse)
library(vtable)
library(cluster)
library(naniar)
library(Boruta)
library(MASS)
library(knitr)
library(kableExtra)
library(factoextra)
library(gridExtra)
library(caTools)

############## DATA FETCH #################

setwd("C:/Files/UoE/Modules/Summer/MA335-7-SU Modelling experimental and observational data/Final project")
proj_dat <- read.csv("project data.csv")

############## DATA CLEANING #############

# Visualization on missing values
vis_miss(proj_dat) + labs(title = "Missing Values")
gg_miss_fct(x = proj_dat, fct = Group)

# Converting the m,f values

proj_dat$M.F <- ifelse(proj_dat$M.F == "M", 1, 0)

# Removing the rows with group = "converted"

proj_dat <- proj_dat %>%
  filter(Group != "Converted")

# Removing rows with NA

proj_dat <- proj_dat %>%
  filter(!is.na(SES) & !is.na(MMSE))


############ DESCRIPTIVE ANALYSIS ###########

summary_of_data <- st(proj_dat)

par(mfrow=c(2,2))
b1 <- boxplot(proj_dat$Age, main = "Age", ylab = "Age")

h1 <- hist(proj_dat$EDUC, main = "Education", xlab= "Education")
h2 <- hist(proj_dat$SES, main = "SES", xlab= "SES")
h3 <- hist(proj_dat$eTIV, main = "eTIV", xlab= "eTIV")

cor_matrix <- cor(proj_dat[, c("Age", "EDUC", "MMSE", "eTIV", "nWBV")])
print(cor_matrix)
kable(cor_matrix, format = "html", table.attr = "class='table'", caption = "Correlation Table") %>%
  kable_styling(full_width = FALSE)

############### CLUSTERING #################

# Using K-means clustering
cluster_dat <- subset(proj_dat, select = -c(Group))
cluster_dat1 <- subset(proj_dat, select = -c(Group))

kmeans_out <- kmeans(cluster_dat, centers = 2, nstart = 20)
cluster_kmean <- kmeans_out$cluster

c1 <- fviz_cluster(kmeans_out, data = cluster_dat, geom = "point", title = "k-means Clusters")
table(cluster_kmean)


# Using Hierarchical Clustering

hierarc_out <- hclust(dist(cluster_dat1))
hierarchical_out <- cutree(hierarc_out, k=2)
table(hierarchical_out)
c2 <- fviz_cluster(list(data = cluster_dat1, cluster = hierarchical_out), geom = "point", stand = FALSE, main = "Hierarchical Clusters")
grid.arrange(c1, c2, nrow = 1)

############### Logistic Regression #############

unique(proj_dat$Group)

dat <- proj_dat
dat$Group <- as.factor(dat$Group)

# Logistic reg
set.seed(123)
train_ind <- sample(1:nrow(dat), 0.7*nrow(dat)) # 70%
train_dat <- dat[train_ind, ]
test_dat <- dat[-train_ind, ]

logi_model <- glm(Group~., data=train_dat, family=binomial)
pred <- predict(logi_model, newdata = test_dat, type = "response")
pred_class <- ifelse(pred > 0.5, 1, 0)

act_class <- test_dat$Group
accuracy <- sum(pred_class == act_class) / length(act_class)

print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

##############################
dat <- proj_dat
dat$Group <- as.factor(dat$Group)
glm.fit<-glm(Group~M.F + Age + EDUC + SES + MMSE + CDR + eTIV + nWBV + ASF,
             data = dat, family = binomial)
summary(glm.fit)
contrasts(as.factor(dat$Group))

glm.probs <- predict(glm.fit,type="response") #Pr(Y=1|X)
glm.predicted <- rep("Demented",317)
glm.predicted[glm.probs>0.5]="Nondemented"
table(glm.predicted, dat$Group)
mean(glm.predicted==dat$Group)



##############################

# Group column is binary
logi_model <- glm(as.factor(Group) ~ M.F + Age + EDUC + SES + MMSE + CDR + eTIV + nWBV + ASF,
                  data = proj_dat, family = binomial)

summary(logi_model)

mod.prob <- predict(logi_model, type="response")
mod.predicted <- rep("Demented", 317)
mod.predicted[mod.prob > 0.5] = "Nondemented"
table(mod.predicted, proj_dat$Group)
mean(mod.predicted==proj_dat$Group)

#################################################
data1 <- proj_dat
data1$Group <- as.factor(data1$Group) 

set.seed(42)
splitted_dat <- sample.split(data1$Group, SplitRatio = 0.7)
train_dat <- data1[splitted_dat, ]
test_dat <- data1[!splitted_dat, ]

logi_model <- glm(Group ~ ., data = train_dat, family = binomial)
summary(logi_model)

preds <- predict(logi_model, newdata = test_dat, type = "response")
predicted_lbls <- ifelse(preds > 0.5, "Nondemented", "Demented")
accuracy <- sum(predicted_lbls == test_dat$Group) / nrow(test_dat)
print(accuracy)
############ Feature Selection Method############

dat <- proj_dat
dat$Group <- ifelse(proj_dat$Group == "Demented", 1, 0)

par(mfrow=c(1,1))
# Using boruta
boruta_out <- Boruta(dat$Group ~., data=dat, doTrace=1)
decision <- boruta_out$finalDecision
signif <- decision[boruta_out$finalDecision %in% c("Confirmed")]
print(signif)
plot(boruta_out, xlab="Features", main="Feature Importance")
att1 <- attStats(boruta_out)


# Using wrapper - forward
model1 <- lm(Group~1, data = dat)
step1 <- step(model1, scope=~ M.F + Age + EDUC + SES + MMSE + CDR + eTIV + nWBV + ASF,
              method='forward')
summary(step1)



