library("lmtest")
library("GGally")
library("rlang")
library("rlms")
library("vctrs")
library("dplyr")
library("car")

# install.packages("devtools")
# devtools::install_github("bdemeshev/rlms")
data <- read.csv("D:\\R_projects\\r18i_os26b.csv")
#
data2 = select(data, nj13.2, n_marst, nh5, n_diplom, n_age, status, nj6.2)
# 13.2 - зп, n_marst - семейное положение, nh5 - пол, n_diplom  - законченное образование, n_age - возраст, 
# status - тип населенного пункта, nj36.2 - число рабочих часов

# подготавливаем к выкидыванию спецкоды
data2$nj13.2[which(data2$nj13.2 >= 99999990)] = NaN
data2$n_marst[which(data2$n_marst >= 99999990)] = NaN
data2$nh5[which(data2$nh5 >= 99999990)] = NaN
data2$n_diplom[which(data2$n_diplom >= 99999990)] = NaN
data2$n_age[which(data2$n_age >= 99999990)] = NaN
data2$status[which(data2$status >= 99999990)] = NaN
data2$nj6.2[which(data2$nj6.2 >= 99999990)] = NaN

data2 = na.omit(data2)

sal1 = as.character(data2$nj13.2)
sal2 = lapply(sal1, as.integer)
sal = as.numeric(unlist(sal2))
mean(sal)
data2["salary"] = (sal - mean(sal)) / sqrt(var(sal))

age1 = as.character(data2$n_age)
age2 = lapply(age1, as.integer)
age3 = as.numeric(unlist(age2))
data2["age"]= (age3 - mean(age3)) / sqrt(var(age3))

data2["sex"]=data2$nh5
data2["sex"] = lapply(data2["sex"], as.character)
data2$sex[which(data2$sex!='1')] <- 0
data2$sex[which(data2$sex=='1')] <- 1
data2$sex = as.numeric(data2$sex)

data2["h_educ"] = data2$n_diplom
data2["h_educ"] = lapply(data2["h_educ"], as.character)
data2["higher_educ"] = data2$n_diplom
data2["higher_educ"] = 0
data2$higher_educ[which(data2$h_educ=='6')] <- 1

data2["status1"]=data2$status
data2["status1"] = lapply(data2["status1"], as.character)
data2["status2"] = 0
data2$status2[which(data2$status1=='1')] <- 1
data2$status2[which(data2$status1=='2')] <- 1
data2$status2 = as.numeric(data2$status2)

dur1 = as.character(data2$nj6.2)
dur2 = lapply(dur1, as.integer)
dur3 = as.numeric(unlist(dur2))
data2["dur"] = (dur3 - mean(dur3)) / sqrt(var(dur3))


data2["wed"]= data2$n_marst
data2["wed"] = lapply(data2["wed"], as.character)
data2$wed1 = 0
data2$wed1[which(data2$wed=='1')] <- 1
data2$wed1[which(data2$wed=='3')] <- 1
data2$wed1 = as.numeric(data2$wed1)

data2["wed2"] = lapply(data2["wed"], as.character)
data2$wed2 = 0
data2$wed2[which(data2$wed=='2')] <- 1
data2$wed2 = as.numeric(data2$wed2)

data2["wed3"]=data2$n_marst
data2$wed3 = 0
data2$wed3[which(data2$wed=='4')] <- 1
data2$wed3 = as.numeric(data2$wed3)

# Данные обработаны
data2

model1 = lm(salary~age+dur+sex+higher_educ+status2+wed1+wed2+wed3, data=data2)
model1
summary(model1)
vif(model1)

# Переменные гражданского состояния образуют достаточно сильную связь, но не выше порога 5, что значит, что они не являются мультиколлинеарными
best = 0
for (x in seq(0.1, 5, 0.1))
{
  for (y in seq(0.1, 5, 0.1))
  {
    modelx = lm(salary~sex+higher_educ+status2+wed1+wed2+wed3+I(age^x)+I(dur^y), data=data2)
    if (best<summary(modelx)$adj.r.squared)
    {
      best_x = x
      best_y = y
      best = summary(modelx)$adj.r.squared
      best_model = modelx
    }
  }
}
best_x
best_y
summary(best_model)
# Лучшая модель с квадратами (норм отработала)

model2 = lm(salary~sex+higher_educ+status2+wed1+wed2+wed3+I(log(abs(age)))+I(log(abs(dur))), data=data2)
summary(model2)

model3 = lm(salary~dur+sex+higher_educ+status2+wed1+wed2+wed3+I(log(abs(age))), data=data2)
summary(model3)

model4 = lm(salary~age+sex+higher_educ+status2+wed1+wed2+wed3+I(log(abs(dur))), data=data2)
summary(model4)

#Модели с логарифмами (чуть хуже отработали)

data2[which(data2$salary>4),]
# Большая часть в браке, возраст в среднем 40-50 лет, число рабочих часов - 40 - 50 в неделю, 
# образование - высшее, живут в городе, притом чаще всего в райцентре (скорее всего в Москве)
# Соотношение по половому признаку - 42/58, с небольшим перевесом в сторону мужчин

data3 = select(data2, salary, age, dur, sex, higher_educ, status2, wed1)
data3$wed1[which(data3$wed1 == 0)] = NaN
data3$sex[which(data3$sex == 1)] = NaN

data3 = na.omit(data3)

model1 = lm(salary~age+dur+higher_educ+status2, data=data3)
model1
summary(model1)
vif(model1)

best = 0
for (x in seq(0.1, 5, 0.1))
{
  for (y in seq(0.1, 5, 0.1))
  {
    modelx = lm(salary~higher_educ+status2+I(age^x)+I(dur^y), data=data3)
    if (best<summary(modelx)$adj.r.squared)
    {
      best_x = x
      best_y = y
      best = summary(modelx)$adj.r.squared
      best_model = modelx
    }
  }
}
best_x
best_y
summary(best_model)
# Лучшая модель с квадратами (норм отработала)

model2 = lm(salary~higher_educ+status2+I(log(abs(age)))+I(log(abs(dur))), data=data3)
summary(model2)

model3 = lm(salary~dur+higher_educ+status2+I(log(abs(age))), data=data3)
summary(model3)

model4 = lm(salary~age+higher_educ+status2+I(log(abs(dur))), data=data3)
summary(model4)

# вторая группа

data3 = select(data2, salary, age, dur, sex, higher_educ, status2, wed3)
data3$wed3[which(data3$wed3 == 0)] = NaN
data3$sex[which(data3$sex == 1)] = NaN
data3
data3$status2[which(data3$status2 == 0)] = NaN
data3
data3 = na.omit(data3)

model1 = lm(salary~age+dur+higher_educ, data=data3)
model1
summary(model1)
vif(model1)

best = 0
for (x in seq(0.1, 5, 0.1))
{
  for (y in seq(0.1, 5, 0.1))
  {
    modelx = lm(salary~higher_educ+I(age^x)+I(dur^y), data=data3)
    if (best<summary(modelx)$adj.r.squared)
    {
      best_x = x
      best_y = y
      best = summary(modelx)$adj.r.squared
      best_model = modelx
    }
  }
}
best_x
best_y
summary(best_model)
# Лучшая модель с квадратами (норм отработала)

model2 = lm(salary~higher_educ+I(log(abs(age)))+I(log(abs(dur))), data=data3)
summary(model2)

model3 = lm(salary~dur+higher_educ+I(log(abs(age))), data=data3)
summary(model3)

model4 = lm(salary~age+higher_educ+I(log(abs(dur))), data=data3)
summary(model4)

