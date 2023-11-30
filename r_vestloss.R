library(dplyr)
library(sp)
library(leaflet)
library(lubridate)
library(ggplot2)
library(purrr)
library(tidyverse)
library(stringr)
library(naniar)
library(reticulate)
library(proxy)
library(pracma)
library(reshape2)
library(readxl)
library(janitor)
library(tibble)
library(viridis)
library(scales)
library(blandr)
library(irr)
library(MASS)
library(emg)
library(blandr)
library(Rtsne)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(kernlab)
library(randomForest)
library(e1071)   
library(varImp)
library(cluster)
library(factoextra)
library(magrittr)
library(tsne)
library(rgl)
library(NbClust)
library(RcppAlgos)
library(RoDiCE)
library(boot)
library(rgl)
library(ggpubr)
library(car)
library(naivebayes)
library(psych)
library(GGally)
library(ggcorrplot)
library(vcd)
library(e1071)
library(caTools)
library(caret)
library(multcomp)
library(ROCR)
library(PRROC)
library(nnet)


###############################
# SET DIRECTORY AND READ FILE #
###############################

figs_dir = "/data/figs/"

mydir<- "/data/"



data_et_sum_category<- read_excel(paste(mydir, 
                         "data_etiologies_summary.xlsx", 
                          sep = ""),
                          sheet = 2,
                         col_names = T) 

data_et_sum<- read_excel(paste(mydir, 
                               "data_etiologies_summary.xlsx", 
                               sep = ""),
                         sheet = 3,
                         col_names = T) 


data_et_all<- read_excel(paste(mydir, 
                         "etiol_1503.xlsx", 
                          sep = ""),
                          sheet = 3,
                          col_names = T) 


data_vb_age<- read_excel(paste(mydir, 
                        "etiol_1503.xlsx", 
                        sep = ""),
                        sheet = 5,
                        col_names = T) 

################
# MISSING DATA #
################


vis_miss(data_et_sum)
vis_miss(data_et_all)
vis_miss(data_vb_age)


#################
# INITIAL PLOTS #  
#################

ggplot(data_et_sum,
       aes(x = Etiologies, y = N)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw() +
  xlab("") + ylab("")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  
mlted_data_et_sum = melt(data_et_sum, id.vars = c("Etiologies", "N"))  


ggplot(mlted_data_et_sum,
       aes(x = Etiologies, y = value)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw() + facet_wrap(~variable) +
xlab("") + ylab("")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




###############
# AGE WALKING #
###########################
# NORMALITY TEST & others #
###########################

df_melted = melt(data_vb_age)
df_melted = na.omit(df_melted)

ggqqplot(df_melted, x = "value")
ggqqplot(df_melted, x = "value", color = "variable")
shapiro.test(df_melted$value) #not normal


ggqqplot(na.omit(data_vb_age$NVF))
ggqqplot(na.omit(data_vb_age$CBVL))
ggqqplot(na.omit(data_vb_age$PVF))
qqPlot(data_vb_age$NVF)
qqPlot(data_vb_age$CBVL)
qqPlot(data_vb_age$PVF)

shapiro.test(df_melted$value)
shapiro.test(data_vb_age$NVF)
shapiro.test(data_vb_age$NVF[-39])
shapiro.test(data_vb_age$CBVL)
shapiro.test(data_vb_age$PVF)


ggplot(df_melted , aes(x=variable, y=value, color=variable)) +
  geom_boxplot()  +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text( size=10, angle = 90 ),
        axis.text.y = element_text( size=9.5),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.position = "none") +ylab("")  + xlab("Vestibular Function") + ylab("Age of walking")




var.test(x = data_vb_age$CBVL, y = data_vb_age$PVF) #equal variance
t.test(x = data_vb_age$CBVL, y = data_vb_age$PVF, var.equal = TRUE) #different in means
ks.test(x = data_vb_age$CBVL, y = data_vb_age$PVF) #different in distributions


var.test(x = data_vb_age$NVF, y = data_vb_age$PVF) #different variance
t.test(x = data_vb_age$NVF, y = data_vb_age$PVF, var.equal = TRUE) #different in means
ks.test(x = data_vb_age$NVF, y = data_vb_age$PVF) #different in distributions


var.test(x = data_vb_age$NVF, y = data_vb_age$CBVL) #equal variance
t.test(x = data_vb_age$NVF, y = data_vb_age$CBVL, var.equal = TRUE) #different in means
ks.test(x = data_vb_age$NVF, y = data_vb_age$CBVL) #different in distributions


desired_order <- c("NVF", "PVF", "CBVL")
df_melted$variable <- factor(df_melted$variable, levels = desired_order)

ggdensity(df_melted, x = "value",
          add = "mean", rug = TRUE,
          color = "variable", fill = "variable")


ggplot(data_vb_age, aes(NVF)) + stat_ecdf(geom = "step",   na.rm = T) +
  labs(title="Empirical Cumulative \n Density Function",
       y = "F(NVF)", x="NVF")+
  theme_classic()

ggplot(data_vb_age, aes(CBVL)) + stat_ecdf(geom = "step", na.rm = T) +
  labs(title="Empirical Cumulative \n Density Function",
       y = "F(Areflexia)", x="Areflexia")+
  theme_classic()

ggplot(data_vb_age, aes(PVF)) + stat_ecdf(geom = "step", na.rm = T) +
  labs(title="Empirical Cumulative \n Density Function",
       y = "F(PVF)", x="PVF")+
  theme_classic()




##############################################################################
# ANOVA for understanding if specific factors are predictive of VFs from HL #
#############################################################################

new_data = data_et_all
new_data_NV_PVF = filter(new_data, VB %in% c('2','1'))
new_data_NV_CBVL = filter(new_data, VB %in% c('2','0'))


new_data$VB = as.factor(new_data$VB)
levels(new_data$VB) <- c('CBVL', 'PIVF', 'NVF')

new_data_NV_PVF$VB = as.factor(new_data_NV_PVF$VB)
new_data_NV_CBVL$VB = as.factor(new_data_NV_CBVL$VB)


levels(new_data_NV_PVF$VB) <- c('PIVF','NVF')
levels(new_data_NV_CBVL$VB) <- c('CBVL', 'NVF')


new_data_NV_PVF$VB2 = new_data_NV_PVF$VB
levels(new_data_NV_PVF$VB2) = c(1,0)

new_data_NV_CBVL$VB2 = new_data_NV_CBVL$VB
levels(new_data_NV_CBVL$VB2) = c(1,0)


new_data$Etiology2 = as.factor(new_data$Etiology2)




ggviolin(new_data, x = "VB", y = "age_head", fill = "VB", 
          color = "VB", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("NVF", "PIVF", "CBVL"),
          ylab = "Age-head (months)", xlab = "Vestibular Function") + 
  theme(legend.position = "none", 
        axis.text=element_text(size=17),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20)) + 
  stat_summary(fun = "mean",
               geom = "point",
               color = "red") +
  scale_x_discrete(labels = c("NVF", "PVF", "CBVL"))


ggviolin(new_data, x = "VB", y = "age_sit", fill = "VB", 
          color = "VB", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("NVF", "PIVF", "CBVL"),
          ylab = "Age-sit (months)", xlab = "Vestibular Function") + 
  theme(legend.position = "none", 
        axis.text=element_text(size=17),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20)) + 
  stat_summary(fun = "mean",
               geom = "point",
               color = "red") +
  scale_x_discrete(labels = c("NVF", "PVF", "CBVL"))

ggviolin(new_data, x = "VB", y = "age_stand_support", fill = "VB", 
          color = "VB", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("NVF", "PIVF", "CBVL"),
          ylab = "Age-stand-support (months)", xlab = "Vestibular Function") + 
  theme(legend.position = "none", 
        axis.text=element_text(size=17),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20)) + 
  stat_summary(fun = "mean",
               geom = "point",
               color = "red") +
  scale_x_discrete(labels = c("NVF", "PVF", "CBVL"))


ggviolin(new_data, x = "VB", y = "age_walking", fill = "VB", 
          color = "VB", #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("NVF", "PIVF", "CBVL"),
          ylab = "Age-walking (months)", xlab = "Vestibular Function") + 
  theme(legend.position = "none", 
        axis.text=element_text(size=17),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20)) + 
  stat_summary(fun = "mean",
               geom = "point",
               color = "red") +
  scale_x_discrete(labels = c("NVF", "PVF", "CBVL"))



na.omit(new_data[,c(2,8:11)] %>%
  group_by(VB)) %>% 
  summarise_at(vars('age_head','age_sit','age_stand_support','age_walking'), mean)



# Compute the analysis of variance
res.aov_age_head <- aov(age_head ~ VB, data = new_data)
# Summary of the analysis
summary(res.aov_age_head)
res.aov_age_head$coefficients

# Compute the analysis of variance
res.aov_age_sit <- aov(age_sit ~ VB, data = new_data)
# Summary of the analysis
summary(res.aov_age_sit)
res.aov_age_sit$coefficients


# Compute the analysis of variance
res.aov_age_stand_support <- aov(age_stand_support ~ VB, data = new_data)
# Summary of the analysis
summary(res.aov_age_stand_support)


# Compute the analysis of variance
res.aov_age_walking <- aov(age_walking ~ VB, data = new_data)
# Summary of the analysis
summary(res.aov_age_walking)


prova <- aov(cbind(age_walking, age_stand_support, age_sit, age_head) ~ VB, data = new_data)
summary(prova)

table(new_data$VB, new_data$Etiology2)
chisq.test(new_data$VB, new_data$Etiology2)




#########
# Logit #
#########

new_data2 = new_data
new_data2$VB2 = new_data2$VB
levels(new_data2$VB2) = c(2,1,0)

set.seed(1)
index1 <- sample(nrow(new_data2),nrow(new_data2)*0.80)
new_data2_train = new_data2[index1,]
new_data2_test = new_data2[-index1,]

new_data2_train$VB2 <- as.factor(new_data2_train$VB2)
new_data2_test$VB2 <- as.factor(new_data2_test$VB2)


model1 <- multinom(VB2 ~ age_head, 
                   data = new_data2_train)

model2 <- multinom(VB2 ~ age_sit, 
                   data = new_data2_train)

model3 <- multinom(VB2 ~ age_stand_support, 
                   data = new_data2_train)

model4 <- multinom(VB2 ~ age_walking, 
                   data = new_data2_train)

model5 <- multinom(VB2 ~ Etiology2, 
                   data = new_data2_train)




summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)



#############
# ROC Curve #
#############


test_prob1 <- predict(model1, newdata = new_data2_test[, 'age_head'])
test_roc1 = multiclass.roc(new_data2_test$VB2, test_prob1, type = "probs")


test_prob2 = predict(model2, newdata = new_data2_test[,'age_sit'])
test_roc2 = multiclass.roc(new_data2_test$VB2, test_prob2, type = "probs")

test_prob3 = predict(model3, newdata = new_data2_test[,'age_stand_support'])
test_roc3 = multiclass.roc(new_data2_test$VB2, test_prob3, type = "probs")


test_prob4 = predict(model4, newdata = new_data2_test[,'age_walking'])
test_roc4 = multiclass.roc(new_data2_test$VB2, test_prob4, type = "probs")

test_prob5 = predict(model5, newdata = new_data2_test[,'Etiology2'])
test_roc5 = multiclass.roc(new_data2_test$VB2, test_prob5, type = "probs")



###########
# CUT OFF #
###########

# Create a sequence of cutoff values
cutoff_values <- seq(0, 1, by = 0.05)

# Create an empty data frame to store results
cutoff_data <- data.frame(Cutoff = numeric(length(cutoff_values)),
                          Sensitivity = numeric(length(cutoff_values)),
                          Specificity = numeric(length(cutoff_values)))

# Loop through cutoff values
for (i in seq_along(cutoff_values)) {
  cutoff <- cutoff_values[i]
  
  # Create a binary outcome based on the current cutoff
  predicted_class <- ifelse(test_prob2 > cutoff, 1, 0)
  
  # Create confusion matrix
  confusion_matrix <- table(predicted_class, new_data_NV_PVF_test$VB2)
  
  # Ensure that confusion matrix has both rows and columns
  if (nrow(confusion_matrix) == 2 && ncol(confusion_matrix) == 2) {
    # Calculate sensitivity and specificity
    sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
    specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
    
    # Store results in the data frame
    cutoff_data[i, ] <- c(cutoff, sensitivity, specificity)
  } else {
    # Handle the case where the confusion matrix does not have the expected structure
    cat("Warning: Confusion matrix does not have the expected structure for cutoff =", cutoff, "\n")
  }
}

# Find the optimal cutoff based on a criterion (e.g., maximizing sensitivity + specificity)
optimal_cutoff_index <- which.max(cutoff_data$Sensitivity + cutoff_data$Specificity)
optimal_cutoff <- cutoff_data$Cutoff[optimal_cutoff_index]

# Print the optimal cutoff and corresponding sensitivity and specificity
cat("Optimal Cutoff:", optimal_cutoff, "\n")
cat("Sensitivity at Optimal Cutoff:", cutoff_data$Sensitivity[optimal_cutoff_index], "\n")
cat("Specificity at Optimal Cutoff:", cutoff_data$Specificity[optimal_cutoff_index], "\n")


