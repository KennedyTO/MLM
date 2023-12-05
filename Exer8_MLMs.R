## PSYC 4330 - Seminar in Statistics
## Exercise 8: Multilevel Modeling


# Open the dataset
d <- read.csv(file.choose()) #heck2011.csv
names(d)
head(d)
car::some(d)

# Let's start by explore math grades, noting that 
# the students are nested/clustered within schools

# Q1
# Run the null/intercept only model
# Y(ij) = γ(00) + u(0j) + e(ij)  #Composite model
library(lme4)
length(unique(d$schcode)) #number of schools
nrow(d) #number of students
m <- lmer(math ~ 1 + (1|schcode),
          data = d)
summary(m) #find γ(00), SD(u(0j)), SD(e(ij))
#γ(00)
fixef(m)
#u(0j)
ranef(m)
#e(ij)
head(residuals(m))
#Intraclass Correlation Coefficient
library(performance)
icc(m)
# Approximately 14% of the variance is due to 
# between school variance


# Q2
# Add the level 1 predictor 'female'
# But still only have a random effect for the intercept
# Composite model:
# Y(ij) = γ(00) + γ(10)*FEMALE(i,j) + u(0j) + e(ij) 
m2 <- lmer(math ~ female +
             (1|schcode), data = d)
summary(m2)  #find γ(00), γ(10), SD(u(0j)), SD(e(ij))
summary(m) #only slightly reduced the level 1 variance
#γ(00) and γ(10)
fixef(m2)
#u(0j)
ranef(m2)
#e(ij)
head(residuals(m2))


# Q3
# Add 'ses_mean' as a level 2 predictor (no level 1 predictor)
# Composite model:
# Y(ij) = γ(00) + γ(01)*SES_MEAN(j) + u(0j) + e(ij) 
m3 <- lmer(math ~ ses_mean + (1|schcode),
           data = d)
summary(m3) #find γ(00), γ(01), SD(u(0j)), SD(e(ij))
summary(m) #note the reduced level 2 variance
#γ(00) and γ(01)
fixef(m3)
#u(0j)
ranef(m3)
#e(ij)
head(residuals(m3))


#Change in ICC
icc(m)
icc(m2) #added level 1 pred, little reduction in Bet-Grp var
icc(m3) #large reduction in the ICC (Bet-Grp var)

