#####################
# Ayudantia 1, 2023 #
#####################

library(foreign)
library(car)
library(dplyr)

setwd('/Users/luismaldonado2/Dropbox/Sol3026_3063/2024/Ayudantias/Ayudantia 1')
base <- read.dta(file = here("base_ej23.dta"))

# Newman, B. J. (2022). Economic Inequality, the Working Poor, and 
# Belief in the American Dream. Public Opinion Quarterly, 86(4), 944-954.

# Tratamiento
# El experimento tiene 3 condiciones: un grupo de control, 
# un tratamiento que presenta información factual sobre 
# la desigualdad económica en USA, y un tratamiento que entrega 
# la misma información de desigualdad económica pero agrega información 
# que indica que muchas personas de bajos recursos en USA son trabajdores pobres.  

# ------------- Actividad 1: Balance ------------ #
# Examine el balance de las condiciones experimentales respecto 
# de las covariables. Al respecto:	
  # a. Analice solo el experimento 1 (surveyid==2016).	
  # b. Para el análisis de balance, use las siguientes covariables: age, income5, male. 
  # c. Concluya si hay o no balance respecto de estas covariables. 
  # En caso de no existir balance, sugiera una solución.	

# Tratamiento
#################

# Variable condition: 1=control, 2=ineq, 3=ineqWP
# Filtro base del 2016
base_16 = base %>%
  filter(surveyid == 2016)

# Medias y ANOVA
# Edad
tabla_age = base_16 %>% 
  group_by(condition) %>% 
  summarize(Media = mean(age, na.rm = TRUE))

tabla_age$condition = car::recode(tabla_age$condition, "1 = 'Control'; 2='Ineq'; 3='IneqWP'")
tabla_age$Media=round(tabla_age$Media, digits = 2)
tabla_age

m1<-lm(age~factor(condition),data=base,subset=surveyid==2016)
summary(m1) # valor p de Anova test

# Ingreso
tabla_income = base_16 %>% 
  group_by(condition) %>% 
  summarize(Media = mean(income5, na.rm = TRUE))

tabla_income$condition = car::recode(tabla_income$condition, "1 = 'Control'; 2='Ineq'; 3='IneqWP'")
tabla_income$Media=round(tabla_income$Media, digits = 2)
tabla_income

m2 <-lm(income5~factor(condition),data=base,subset=surveyid==2016)
summary(m2) # valor p de Anova test

# Sexo
tabla_male = base_16 %>% 
  group_by(condition) %>% 
  summarize(Media = mean(male, na.rm = TRUE))

tabla_male$condition = car::recode(tabla_male$condition, "1 = 'Control'; 2='Ineq'; 3='IneqWP'")
tabla_male$Media=round(tabla_male$Media, digits = 2)
tabla_male

m3 <- lm(male~factor(condition),data=base,subset=surveyid==2016)
summary(m3) # valor p de Anova test

# -------------- Actividad 2: ATE -------------- #
# Vamos a replicar algunos de los resultados reportados en Figura 1 del artículo 
# que evalúan la hipótesis 1:

  # H1: learning about inequality in conjunction with information 
  # about the prevalence of the WP will erode be- lief in the American dream. 

#a.	Use como variable dependiente el indicador de apoyo a la meritocracia (meritocracy_01). 
#b.	No use covariables.
#c.	Estime los seis ATEs de la Figura 1, usando HC2 SE. 
#d.	Interprete los ATEs para el base que combina experimento 1 y 2. 
#e.	Concluya respecto de hipótesis 1 (H1).

# Variable dependiente
summary(base$meritocracy_01)

# Experiment 1
library(estimatr) #SE

m1 <- lm_robust(meritocracy_01~ineq + ineqWP, data=base, subset=surveyid==2016)

# Experiment 2
m2 <- lm_robust(meritocracy_01~ineq + ineqWP, data=base, subset=surveyid==2017)

# Experimento 1 and 2 combined
m3 <- lm_robust(meritocracy_01~ineq + ineqWP, data=base)

library(texreg)
screenreg(l=list(m1,m2,m3), single.row = TRUE, stars = c(0.01, 0.05))

# --------------Actividad 3: Covariables--------------#
# Evalué los resultados en punto 4 incluyendo covariables.
# Use la siguientes variables independientes: age, income5, male.

# Experiment 1
m4 <- lm_robust(meritocracy_01~ineq + ineqWP + age + income5 + male, data=base, subset=surveyid==2016)

# Experiment 2
m5 <- lm_robust(meritocracy_01~ineq + ineqWP + age + income5 + male, data=base, subset=surveyid==2017)

# Experimento 1 and 2 combined
m6 <- lm_robust(meritocracy_01~ineq + ineqWP + age + income5 + male, data=base)

screenreg(l=list(m4,m5,m6), single.row = TRUE, stars = c(0.01, 0.05))










