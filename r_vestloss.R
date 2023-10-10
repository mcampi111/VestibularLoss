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

###############################
# SET DIRECTORY AND READ FILE #
###############################

figs_dir = "C:\\Users\\mcampi\\Desktop\\Sylvette\\paper_ethiologies_HL_VF\\data\\figs\\"

mydir<- "C:\\Users\\mcampi\\Desktop\\Sylvette\\paper_ethiologies_HL_VF\\data\\"



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

set.seed(1)
index1 <- sample(nrow(new_data_NV_PVF),nrow(new_data_NV_PVF)*0.80)
new_data_NV_PVF_train = new_data_NV_PVF[index1,]
new_data_NV_PVF_test = new_data_NV_PVF[-index1,]


fit <- glm(VB ~ age_head + age_sit + age_stand_support + age_walking + Etiology2,
                weights =age_walking,
                family = binomial(link = "logit"),
                data = new_data_NV_PVF_train) #new_data_NV_PVF

summary(fit)

## odds ratios and 95% CI
coef(fit)
exp(cbind(OR = coef(fit), confint(fit)))


index2 <- sample(nrow(new_data_NV_CBVL),nrow(new_data_NV_CBVL)*0.80)
new_new_data_NV_CBVL_train = new_data_NV_CBVL[index2,]
new_new_data_NV_CBVL_test = new_data_NV_CBVL[-index2,]



fit2 = glm(VB ~  age_sit + age_stand_support + age_walking + Etiology2,
           weights = age_walking,
           family = binomial(link = "logit"),
           data = new_new_data_NV_CBVL_train)#new_new_data_NV_CBVL

summary(fit2 )
coef(fit2)
confint(fit2)

#############
# ROC Curve #
#############

#automatic
library(pROC)
test_prob = predict(fit, newdata = new_data_NV_PVF_test[-c(2:4),], type = "response")
test_roc = roc(new_data_NV_PVF_test[-c(2:4),]$VB2 ~ test_prob, plot = TRUE, print.auc = TRUE)

#AUC
as.numeric(test_roc$auc)
# Get the cutoff value at the optimal point on the ROC curve
coords(test_roc, "best", ret = "threshold")


test_prob = predict(fit, newdata = new_new_data_NV_CBVL_test, type = "response")
test_roc = roc(new_new_data_NV_CBVL_test$VB2 ~ test_prob,
               plot = TRUE,
               print.auc = TRUE)
#AUC
as.numeric(test_roc$auc)
# Get the cutoff value at the optimal point on the ROC curve
coords(test_roc, "best", ret = "threshold")



