#########################
# Clase 3: 2024
# Ejercicio en clases
#########################

# See: https://egap.org/resource/10-things-to-know-about-covariate-adjustment/

rm(list=ls())

set.seed(20140714)
N = 2000
N.treated = 1000
Replications = 10000

true.treatment.effect = 1

# Create pre-treatment covariates
owns.id.card = rbinom(n = N, size = 1, prob = .18)
has.formal.schooling = rbinom(n = N, size = 1, prob = .6)
age = round(rnorm(n = N, mean = 37, sd = 16))
age[age<18] = 18
age[age>65] = 65
TV.access = rbinom(n = N, size = 1, prob = .7)
epsilon = rnorm(n = N, mean = 0, sd = 2)

# Create potential outcomes correlated with pre-treatment covariates
Y0 = round(owns.id.card + 2*has.formal.schooling + 3*TV.access + log(age) + epsilon)
Y1 = Y0 + true.treatment.effect

# Assign treatment repeatedly
Z.mat = replicate(Replications, ifelse(1:N %in% sample(1:N, N.treated), 1, 0))

# Generate observed outcomes
Y.mat = Y1 * Z.mat + Y0 * (1 - Z.mat)

diff.in.means = function(Y, Z) {
  coef(lm(Y ~ Z))[2]
}

ols.adjust = function(Y, Z) {
  coef(lm(Y ~ Z + owns.id.card + has.formal.schooling + age + TV.access))[2]
}

unadjusted.estimates = rep(NA, Replications)
adjusted.estimates   = rep(NA, Replications)

for (i in 1:Replications) {
  unadjusted.estimates[i]  =  diff.in.means(Y.mat[,i], Z.mat[,i])
  adjusted.estimates[i]    =  ols.adjust(Y.mat[,i], Z.mat[,i])
}

# Estimated variability (standard deviation) of each estimator
hist(unadjusted.estimates)
hist(adjusted.estimates)

sd.of.unadj = sd(unadjusted.estimates)
sd.of.unadj
sd.of.adj   = sd(adjusted.estimates)
sd.of.adj


# Estimated bias of each estimator
mean(unadjusted.estimates) - true.treatment.effect
mean(adjusted.estimates) - true.treatment.effect

# Margin of error (at 95% confidence level) for each estimated bias
1.96 * sd.of.unadj / sqrt(Replications)
1.96 * sd.of.adj   / sqrt(Replications)


# ---------------------- Randomization inference --------------------- #

#Limpiar los objetos en R
rm(list = ls())

# Cracion de vectores
Y <- c(23,27,58,61) #Creamos Variable Y
Y

Z <- c(0,1, 0, 1) #Creamos Variable X
Z

# ATE via regresion OLS
# -----------------------------
coef(lm(Y~Z))["Z"]

# Simulación de repetidos experimentos
# ----------------------------------------

# Primero ponemos nuestros datos en dataframe

data1 = data.frame(Y,Z)
data1


# Response schedule para hip= no hay efecto
make_RS <- function(rate) {
	with(data1, data.frame(yt = Y + (1-Z)*rate, 
	yc= Y + Z*rate))	
}

RS0=make_RS(0)
RS0 

newdata=data.frame(RS0, z=data1$Z)
newdata
coef(lm(I(yt*z+yc*(1-z))~z,data=newdata))["z"]


# Simulacion
simD = replicate(100,{
	newdata = data.frame(RS0, z=sample(data1$Z))
	coef(lm(I(yt*z+yc*(1-z))~z,data=newdata))["z"]
	})

simD

# Sampling distribution
hist(simD)

# Test de hipotesis aproximado
mean(simD>=3.5) 








