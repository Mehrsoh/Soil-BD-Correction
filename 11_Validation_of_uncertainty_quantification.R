# Validating uncertainty estimates for the best-performing pedotransfer function (PTF)
# (Note that random forest proved to be the best)
#
# Author: Gabor Szatmari
# E-mail: szatmari.gabor@atk.hun-ren.hu


# 1. Load packages ----
library(ranger)
library(caret)
library(dplyr)
library(ggplot2)
library(doParallel)
library(gridExtra)


# 2. Set working directory ----

setwd("d:/Dokumentumok/Kutatas/2025/Mehr_BD_TIM/data_with_EnvCov/")


# 3. Load the MARTHA dataset ----
martha <- readRDS("MARTHA_with_EnvCov.rds")


# 4. Data Cleaning ----
martha_clean <- martha %>%
  select(-CaCO3, -SOM, -Profile_ID) %>%
  na.omit() %>%
  mutate(
    Geology_02 = as.factor(Geology_02),
    Soil_type  = as.factor(Soil_type)
  )


# 5. Five times repeated 10-fold cross-validation ----
set.seed(1234)
folds <- createMultiFolds(martha_clean$BD, k = 10, times = 5)

container <- list()

for(i in 1:length(folds)){
  
  train.set <- martha_clean[folds[[i]], ]
  test.set  <- martha_clean[-folds[[i]], ]
  
  t.grid <- expand.grid(mtry=c(1, seq(5,55,by=5), 58),
                        splitrule="variance",
                        min.node.size=c(5))
  
  rf.model <- train(
    x=train.set[,c(1,3:59)],
    y=train.set$BD, 
    method = "ranger",
    tuneGrid = t.grid,
    trControl = trainControl(method = "cv"),
    importance = "impurity",
    quantreg = TRUE,
    num.trees = 200,
    num.threads = detectCores()-4)
  
  temp <- data.frame(predict(rf.model$finalModel, test.set, num.threads=(detectCores()-4), type="quantiles", quantiles=seq(1,0, by=-0.01)[2:100])$predictions) # quantiles
  names(temp) <- paste0("q.", seq(1,0, by=-0.01)[2:100])
  temp$fold <- i
  temp$obs <- test.set$BD
  
  container[[i]] <- temp
  
  message(paste0("Fold ", i, " Done!"))
  
  if(i==length(folds)){
    container <- do.call(rbind.data.frame, container)
    saveRDS(container, file="Container.rds")
  }
  
}


# 6. Check the results ----
q <- seq(1,0, by=-0.01)[2:100] # quantiles used

for(i in 1:49){ # symmetric prediction intervals
  if(i==1){
    pi <- q[i] - q[(length(q) + 1) - i]
  } else {
    pi <- c(pi, q[i] - q[(length(q) + 1) - i])
  }
}


## 6.1. Prediction interval coverage probability (PICP) plot ====
picp <- data.frame(matrix(NA, ncol=51, nrow=length(pi)))
picp[,1] <- pi

names(picp) <- c("theo", paste0("Fold", 1:50))


for(i in 1:50){
  
  temp <- container[container$fold==i,]
  
  for(j in 1:length(pi)){
    
    picp[j,i+1] <- sum(ifelse(temp[,j] >= temp$obs & temp$obs > temp[,(ncol(temp)-1) - j], yes=1, no=0))/nrow(temp)
    
  }
  
}

plot(picp$theo, apply(picp[,2:51], 1, mean), ylab="Observed", xlab="Theoretical", main="PICP"); abline(0,1, col="red") # plot PICP


## 6.2. Quantile coverage probability (QCP) plot ====
index <- subset(1:99, 1:99 %% 2 == 0)

qcp <- data.frame(matrix(NA, ncol=51, nrow=length(pi)))
qcp[,1] <- pi

names(qcp) <- c("theo", paste0("Fold", 1:50))

for(i in 1:50){
  
  temp <- container[container$fold==i,]
  
  for(j in 1:length(pi)){
    m <- index[j]
    qcp[j,i+1] <- sum(ifelse(temp[,m] >= temp$obs, yes=1, no=0))/nrow(temp)
    
  }
  
}

plot(qcp$theo, apply(qcp[,2:51], 1, mean), ylab="Observed", xlab="Theoretical", main="QCP"); abline(0,1, col="red") # plot QCP



# 7. Create some nice figures ----
picp.df <- data.frame(theo=picp$theo,
                      obs=apply(picp[,2:51], 1, mean),
                      upper=apply(picp[,2:51], 1, function(x) quantile(x, probs=0.025)),
                      lower=apply(picp[,2:51], 1, function(x) quantile(x, probs=0.975)))

qcp.df <- data.frame(theo=qcp$theo,
                     obs=apply(qcp[,2:51], 1, mean),
                     upper=apply(qcp[,2:51], 1, function(x) quantile(x, probs=0.95)),
                     lower=apply(qcp[,2:51], 1, function(x) quantile(x, probs=0.05)))

picp.fig <- ggplot(picp.df, aes(x=theo, y=obs)) + 
  geom_point(size=1)+
  geom_abline(col="red3", lwd=0.5)+
  #geom_pointrange(aes(ymin=lower, ymax=upper), size=0.1)+
  xlim(0,1)+
  ylim(0,1)+
  labs(x="Expected fraction", y="Observed fraction", title="PICP")+
  theme(plot.title=element_text(hjust=0.5, face='bold'))

qcp.fig <- ggplot(qcp.df, aes(x=theo, y=obs)) + 
  geom_point(size=1)+
  geom_abline(col="red3", lwd=0.5)+
  #geom_pointrange(aes(ymin=lower, ymax=upper), size=0.1)+
  xlim(0,1)+
  ylim(0,1)+
  labs(x="Expected fraction", y="Observed fraction", title="QCP")+
  theme(plot.title=element_text(hjust=0.5, face='bold'))

fig <- arrangeGrob(picp.fig, qcp.fig, layout_matrix = rbind(c(1,2)))

ggsave(filename="Fig_PICP_QCP.jpg",
       plot = fig,
       width=20,
       height=10,
       units="cm",
       dpi=300)
