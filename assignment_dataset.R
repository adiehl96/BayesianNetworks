library(dplyr)
library(tidyverse)
library(dagitty)
library(lavaan)

setwd("C:/Users/soeve/Downloads")
df <- read.csv("student/student-mat.csv", sep = ";")
View(df)



selected_df <- df %>%
  dplyr::select(Dalc, G3, Pstatus, Walc, absences, activities, age, failures, famrel,
                famsup, freetime, goout, health, higher, paid, schoolsup, sex,
                studytime)

# selected_df <-  drop_na(selected_df) #Needed? G3 column contains NA

View(selected_df)


# --------------
# Creating the network
g <- dagitty(' dag {
Dalc [pos="-0.931,0.588"]
G3 [pos="-0.021,1.387"]
Pstatus [pos="-1.174,-1.119"]
Walc [pos="-1.162,0.998"]
absences [pos="-0.787,-0.678"]
activities [pos="0.129,-1.391"]
age [pos="-0.547,0.636"]
failures [pos="0.427,1.036"]
famrel [pos="-1.338,-0.528"]
famsup [pos="-0.306,-1.226"]
freetime [pos="-0.109,0.508"]
goout [pos="-1.060,-0.183"]
health [pos="-0.781,1.484"]
higher [pos="0.768,-0.508"]
paid [pos="0.507,-1.107"]
schoolsup [pos="-0.446,-0.436"]
sex [pos="-0.691,0.922"]
studytime [pos="0.563,0.614"]
Dalc -> health
Pstatus -> famrel
Walc -> health
absences -> G3
activities -> freetime
age -> G3
age -> goout
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
selected_df$schoolsup <- as.numeric(ordered(selected_df$schoolsup, c("yes", "no")))
selected_df$famsup <- as.numeric(ordered(selected_df$famsup, c("yes", "no")))
selected_df$paid <- as.numeric(ordered(selected_df$paid, c("yes", "no")))
selected_df$activities <- as.numeric(ordered(selected_df$activities, c("yes", "no")))
selected_df$higher <- as.numeric(ordered(selected_df$higher, c("yes", "no")))

selected_df$age <- ordered(selected_df$age, 
                           levels=as_vector(unique(sort(selected_df$age))))
selected_df$studytime <- ordered(selected_df$studytime,
                           levels=as_vector(unique(sort(selected_df$studytime))))
selected_df$famrel <- ordered(selected_df$famrel,
                           levels=as_vector(unique(sort(selected_df$famrel))))
selected_df$freetime <- ordered(selected_df$freetime,
                           levels=as_vector(unique(sort(selected_df$freetime))))
selected_df$goout <- ordered(selected_df$goout,
                           levels=as_vector(unique(sort(selected_df$goout))))
selected_df$Dalc <- ordered(selected_df$Dalc,
                           levels=as_vector(unique(sort(selected_df$Dalc))))
selected_df$Walc <- ordered(selected_df$Walc,
                           levels=as_vector(unique(sort(selected_df$Walc))))
selected_df$health <- ordered(selected_df$health,
                           levels=as_vector(unique(sort(selected_df$health))))
selected_df$absences <- ordered(selected_df$absences,
                            levels=as_vector(unique(sort(selected_df$absences))))
selected_df$G3 <- ordered(selected_df$G3,
                           levels=as_vector(unique(sort(selected_df$G3))))



View(selected_df)

# Extract polychoric correlation matrix
m <- lavCor(selected_df)
m

#  Test model using polychoric correlation matrix
localTests(g, sample.cov=m, sample.nobs=nrow(selected_df))


