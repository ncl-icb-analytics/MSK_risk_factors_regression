library(tidyverse)
library(DBI)
library(ModelMetrics)
library(data.table)
library(NCLRtemplates)

con <- dbConnect(odbc::odbc(), .connection_string = Sys.getenv("SANDBOX"))

MSK <- dbGetQuery(con, "select * from [data_lab_SBI].[dbo].[MSKRegression]")

MSK <- as.data.table(MSK)

# format

# Trying to do in tidyverse what I'd rather do in base R. It's over-elaborate.
MSK <-
  MSK %>% 
  select(starts_with("Age")) %>% 
  mutate(Age85plus = ifelse(rowSums(., na.rm=TRUE)>0, 0, 1)) %>% 
  select(Age85plus) %>% 
  bind_cols(MSK, .name_repair = "universal") %>% 
  as.data.frame()

MSK$Age <-
  MSK %>% 
  select(starts_with("Age")) %>% 
  pivot_longer(cols = everything()
               , values_transform = 
               , values_drop_na = TRUE) %>% 
  filter(value ==1) %>% 
  select(name) %>% 
  pull()

names(MSK)

MSK$Age <- factor(MSK$Age, levels = c("Age16_24", "Age25_44", "Age45_64", "Age65_74", "Age75_84", "Age85plus"))
MSK$GenderCode <- factor(MSK$GenderCode)
MSK$IMD <- factor(MSK$IMD, ordered = TRUE)


#
ggplot(MSK, aes(x=Co_morbidities))+
  geom_histogram()

# Treat comorbidities as factor
MSK$Co_morbidities <- factor(MSK$Co_morbidities, ordered = TRUE)

# How many predictors can we have, based on common rule of thumb (Frank Harrel, Regression Modelling Strategies/RMS) 1:20
# Riley et al. 2019 https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6519266/

sum(MSK$MSK)/20  #=611.  We should be fine.


##### Models####
# Go big or go home...

glm_big <- glm(MSK~., data=MSK[6:26], family="binomial")
summary(glm_big)

anova(glm_big, test='Chisq')

#  Van Houwelingen and Le Cessie shrinkage estimate

null_model <- glm(MSK~1, data=MSK[6:26], family="binomial")

# comes out at 0.95, which is > than the 0.9 minimum in the Riley et al. paper above.
1 - ((glm_big$df.null- glm_big$df.residual) / (-2*(as.numeric(logLik(null_model)) - as.numeric(logLik(glm_big)))))



auc(null_model)
auc(glm_big)

# So, even the big model's auc is only 0.571 (explains 57.1% of variation).  It's a bit rubbish.
# Need to further examine the parametrisation as well as whether we have the right predictors.

MSK$preds <- predict(glm_big, type="response")

MSK %>% 
  group_by(Age) %>% 
  summarise(MSK = sum(MSK),
            Predicted = sum(preds)) %>% 
  ggplot(aes(y=MSK,x=Predicted, col= Age))+
  geom_point()+
  geom_abline(intercept=0, slope=1, col="black")+
  scale_colour_ncl()+
  theme_nclicb()
