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
fg
coordinates(fg) <- cg
fg <- dagitty('dag {
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
  Dalc -> Walc [beta="0.51"]
  Dalc -> health [beta="-0.019"]
  Dalc -> studytime [beta="0.025"]
  Pstatus -> famrel [beta="-0.051"]
  Walc -> health [beta="0.13"]
  Walc -> studytime [beta="-0.16"]
  absences -> G3 [beta="-0.016"]
  activities -> freetime [beta="0.13"]
  age -> G3 [beta="0.052"]
  age -> failures [beta="0.26"]
  age -> goout [beta="0.11"]
  age -> higher [beta="-0.27"]
  failures -> G3 [beta="-0.31"]
  famrel -> absences [beta="-0.09"]
  famrel -> famsup [beta="0.015"]
  famrel -> health [beta="0.11"]
  famsup -> freetime [beta="0.011"]
  famsup -> studytime [beta="0.1"]
  freetime -> health [beta="0.056"]
  goout -> Dalc [beta="0.22"]
  goout -> Walc [beta="0.25"]
  goout -> freetime [beta="0.33"]
  health -> G3 [beta="-0.076"]
  higher -> Dalc [beta="-0.1"]
  higher -> G3 [beta="0.22"]
  higher -> failures [beta="-0.24"]
  higher -> studytime [beta="0.16"]
  paid -> freetime [beta="-0.058"]
  paid -> studytime [beta="-0.0019"]
  schoolsup -> freetime [beta="0.014"]
  schoolsup -> studytime [beta="0.038"]
  sex -> Dalc [beta="0.26"]
  sex -> G3 [beta="-0.05"]
  sex -> Walc [beta="0.16"]
  sex -> studytime [beta="-0.13"]
  studytime -> G3 [beta="0.15"]
  studytime -> freetime [beta="-0.056"]}')
plot(fg, show.coefficients=TRUE)
# TODO kan ik rondjes maken?

# -----------------------------------
### ISOLATED EFFECTS
# -----------------------------------
# On the basis of adjustment sets
library(bnlearn)
net1 <- model2network(toString(g,"bnlearn"))
df_scaled <- as.data.frame(scale(selected_df))
fit1 <- bn.fit( net1, df_scaled )

# FAMILY -----------------------------------

adjustmentSets(g,"famrel","G3") # No adjustment set- isolated effect cannot be reliably quantified

adjustmentSets(g,"famsup","G3")
summary( lm( G3 ~ famsup + famrel, as.data.frame(scale(selected_df)) ) )
# Coefficient is 5.825e-02 (not significant)

adjustmentSets(g,"Pstatus","G3") # No adjustment set- isolated effect cannot be reliably quantified

## Old attempts we probably won't use, but just in case
# summary( lm( G3 ~ famrel + famsup + Pstatus, as.data.frame(scale(selected_df)) ) )

## Attempting to predict G3 from a group of variables. No idea if this approach is valid.
# predicted.G3 <- predict(fit1,node="G3",data=df_scaled[,c("Pstatus","famrel","famsup"),drop=F],method="bayes-lw")
# plot(df_scaled[,"G3"],predicted.G3)


# SCHOOL -----------------------------------

adjustmentSets(g,"schoolsup","G3") # No adjustment set- isolated effect cannot be reliably quantified

adjustmentSets(g,"absences","G3")
summary( lm( G3 ~ absences + famrel, as.data.frame(scale(selected_df)) ) )
# Coefficient is -8.640e-02 (*)

adjustmentSets(g,"failures","G3")
summary( lm( G3 ~ failures + age + higher, as.data.frame(scale(selected_df)) ) )
# Coefficient is -3.389e-01 (***)

## Old attempts we probably won't use, but just in case
# summary( lm( G3 ~ schoolsup + absences + failures, as.data.frame(scale(selected_df)) ) )
# predicted.G3 <- predict(fit1,node="G3",data=df_scaled[,c("schoolsup","absences","failures"),drop=F],method="bayes-lw")
# plot(df_scaled[,"G3"],predicted.G3)

# SOCIAL -----------------------------------

adjustmentSets(g,"Dalc","G3")
summary( lm( G3 ~ Dalc + goout + higher + sex, as.data.frame(scale(selected_df)) ) )
# Coefficient is -1.372e-01 (***)

adjustmentSets(g,"Walc","G3")
summary( lm( G3 ~ Walc + Dalc +  goout + sex, as.data.frame(scale(selected_df)) ) )
# Coefficient is -5.471e-02 (not significant)

adjustmentSets(g,"activities","G3") # No adjustment set- isolated effect cannot be reliably quantified

adjustmentSets(g,"freetime","G3")
summary( lm( G3 ~ freetime + famsup + goout + paid + schoolsup + studytime, as.data.frame(scale(selected_df)) ) )
# Coefficient is -9.577e-02 (*)

adjustmentSets(g,"goout","G3")
summary( lm( G3 ~ goout + age, as.data.frame(scale(selected_df)) ) )
# Coefficient is -7.660e-02 (.)

## Old attempts we probably won't use, but just in case
# summary( lm( G3 ~ Dalc + Walc + activities + freetime + goout, as.data.frame(scale(selected_df)) ) )
# predicted.G3 <- predict(fit1,node="G3",data=df_scaled[,c("Dalc","Walc","activities","freetime","goout"),drop=F],method="bayes-lw")
# plot(df_scaled[,"G3"],predicted.G3)

# INTRINSIC -----------------------------------

adjustmentSets(g,"higher","G3")
summary( lm( G3 ~ higher + age, as.data.frame(scale(selected_df)) ) )
# Coefficient is 3.269e-01 (***)

## Old attempts we probably won't use, but just in case
# summary( lm( G3 ~ higher, as.data.frame(scale(selected_df)) ) )
# predicted.G3 <- predict(fit1,node="G3",data=df_scaled[,"higher",drop=F],method="bayes-lw")
# plot(df_scaled[,"G3"],predicted.G3)
