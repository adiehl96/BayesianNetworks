library(dplyr)
library(tidyverse)
library(dagitty)
library(lavaan)

# As everyone has their own folder for this, better to just set it manually.
# setwd("C:/Users/soeve/Downloads")
df <- read.csv("student-por.csv", sep = ";")



selected_df <- df %>%
  dplyr::select(Dalc, G3, Pstatus, Walc, absences, activities, age, failures, famrel,
                famsup, freetime, goout, health, higher, paid, schoolsup, sex,
                studytime)

selected_df <-  drop_na(selected_df) #Needed? G3 column contains NA


# --------------
# Creating the network
g <- dagitty(' dag {
Dalc [pos="-0.908,0.332"]
G3 [pos="0.039,1.478"]
Pstatus [pos="-1.174,-1.119"]
Walc [pos="-1.243,1.001"]
absences [pos="-0.651,-0.640"]
activities [pos="-0.217,-1.406"]
age [pos="0.650,0.932"]
failures [pos="0.531,0.332"]
famrel [pos="-1.256,-0.492"]
famsup [pos="-0.739,-1.038"]
freetime [pos="0.020,-0.619"]
goout [pos="-0.992,-0.225"]
health [pos="-0.781,1.484"]
higher [pos="0.764,-0.897"]
paid [pos="0.210,-1.341"]
schoolsup [pos="0.449,-1.237"]
sex [pos="-0.418,1.127"]
studytime [pos="0.131,0.296"]
Dalc -> Walc
Dalc -> health
Dalc -> studytime
Pstatus -> famrel
Walc -> health
Walc -> studytime
absences -> G3
activities -> freetime
age -> G3
age -> failures
age -> goout
age -> higher
failures -> G3
famrel -> absences
famrel -> famsup
famrel -> health
famsup -> freetime
famsup -> studytime
freetime -> health
goout -> Dalc
goout -> Walc
goout -> freetime
health -> G3
higher -> Dalc
higher -> G3
higher -> failures
higher -> studytime
paid -> freetime
paid -> studytime
schoolsup -> freetime
schoolsup -> studytime
sex -> Dalc
sex -> G3
sex -> Walc
sex -> studytime
studytime -> G3
studytime -> freetime
}
')
plot(g)

# Ordered variables needed for poly
selected_df$sex <- as.numeric(ordered(selected_df$sex, c("F", "M")))
selected_df$Pstatus <- as.numeric(ordered(selected_df$Pstatus, c("T", "A")))
selected_df$schoolsup <- as.numeric(ordered(selected_df$schoolsup, c("no", "yes")))
selected_df$famsup <- as.numeric(ordered(selected_df$famsup, c("no", "yes")))
selected_df$paid <- as.numeric(ordered(selected_df$paid, c("no", "yes")))
selected_df$activities <- as.numeric(ordered(selected_df$activities, c("no", "yes")))
selected_df$higher <- as.numeric(ordered(selected_df$higher, c("no", "yes")))

# selected_df$age <- ordered(selected_df$age,
#                            levels=as_vector(unique(sort(selected_df$age))))
# selected_df$studytime <- ordered(selected_df$studytime,
#                            levels=as_vector(unique(sort(selected_df$studytime))))
# selected_df$famrel <- ordered(selected_df$famrel,
#                            levels=as_vector(unique(sort(selected_df$famrel))))
# selected_df$freetime <- ordered(selected_df$freetime,
#                            levels=as_vector(unique(sort(selected_df$freetime))))
# selected_df$goout <- ordered(selected_df$goout,
#                            levels=as_vector(unique(sort(selected_df$goout))))
# selected_df$Dalc <- ordered(selected_df$Dalc,
#                            levels=as_vector(unique(sort(selected_df$Dalc))))
# selected_df$Walc <- ordered(selected_df$Walc,
#                            levels=as_vector(unique(sort(selected_df$Walc))))
# selected_df$health <- ordered(selected_df$health,
#                            levels=as_vector(unique(sort(selected_df$health))))
# selected_df$absences <- ordered(selected_df$absences,
#                             levels=as_vector(unique(sort(selected_df$absences))))
# selected_df$G3 <- ordered(selected_df$G3,
#                            levels=as_vector(unique(sort(selected_df$G3))))

# Extract polychoric correlation matrix
m <- lavCor(selected_df)

#  Test model using polychoric correlation matrix
localTests(g, sample.cov=m, sample.nobs=nrow(selected_df))

### PATH COEFFICIENTS ###
fit <- sem( toString(g,"lavaan"), sample.cov=m, sample.nobs=nrow(selected_df) )
summary(fit)
fg <- lavaanToGraph(fit, digits=2)
cg <- coordinates(g)
coordinates(fg) <- cg
plot(fg, show.coefficients=TRUE)

### ISOLATED EFFECTS
library(bnlearn)
net1 <- model2network(toString(g,"bnlearn"))
df_scaled <- as.data.frame(scale(selected_df))
fit1 <- bn.fit( net1, df_scaled )

# FAMILY

# adjustmentSets(g,"famrel","G3")
summary( lm( G3 ~ famrel + famsup + Pstatus, as.data.frame(scale(selected_df)) ) )
## Attempting to predict G3 from a single variable. Doesn't work great hahah.
# predicted.G3 <- predict(fit1,node="G3",data=df_scaled[,"famrel",drop=F],method="bayes-lw")
# plot(df_scaled[,"G3"],predicted.G3)
# predicted.G3 <- predict(fit1,node="G3",data=df_scaled[,"famsup",drop=F],method="bayes-lw")
# plot(df_scaled[,"G3"],predicted.G3)
# predicted.G3 <- predict(fit1,node="G3",data=df_scaled[,"Pstatus",drop=F],method="bayes-lw")
# plot(df_scaled[,"G3"],predicted.G3)

## Attempting to predict G3 from a group of variables. No idea if this approach is valid.
predicted.G3 <- predict(fit1,node="G3",data=df_scaled[,c("Pstatus","famrel","famsup"),drop=F],method="bayes-lw")
plot(df_scaled[,"G3"],predicted.G3)


# SCHOOL

summary( lm( G3 ~ schoolsup + absences + failures, as.data.frame(scale(selected_df)) ) )
predicted.G3 <- predict(fit1,node="G3",data=df_scaled[,c("schoolsup","absences","failures"),drop=F],method="bayes-lw")
plot(df_scaled[,"G3"],predicted.G3)

# SOCIAL

summary( lm( G3 ~ Dalc + Walc + activities + freetime + goout, as.data.frame(scale(selected_df)) ) )
predicted.G3 <- predict(fit1,node="G3",data=df_scaled[,c("Dalc","Walc","activities","freetime","goout"),drop=F],method="bayes-lw")
plot(df_scaled[,"G3"],predicted.G3)

# INTRINSIC

summary( lm( G3 ~ higher, as.data.frame(scale(selected_df)) ) )
predicted.G3 <- predict(fit1,node="G3",data=df_scaled[,"higher",drop=F],method="bayes-lw")
plot(df_scaled[,"G3"],predicted.G3)
