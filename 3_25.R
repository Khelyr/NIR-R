library("lmtest")
library("rlms")
library("dplyr")
library("GGally")
library("foreign")
library("car")
library("sandwich")
library("devtools")

data = read.csv("r26i_os26b.csv")
glimpse(data)


data2 = select(data, vj13.2, v_age, vh5, v_educ, status, vj6.2, v_marst)

#исключаем строки с отсутствующими значениями NA
data2 = na.omit(data2)

#зарплата c элементами нормализации
data2$vj13.2
sal = as.numeric(data2$vj13.2)
#summary(sal)
mean(sal)
data2["salary"] = (sal - mean(sal)) / sqrt(var(sal))
data2["salary"]
summary(data2["salary"])

#пол
data2["sex"]=data2$vh5
data2$sex[which(data2$sex!='1')] <- 0
data2$sex[which(data2$sex=='1')] <- 1
data2$sex = as.numeric(data2$sex)
summary(data2["sex"])


#семейное положение
data2["wed"]= data2$v_marst
data2$wed1 = 0
data2$wed1[which(data2$wed=='2')] <- 1
data2$wed1 = as.numeric(data2$wed1)
summary(data2["wed"])

data2["wed2"] = lapply(data2["wed"], as.character)
data2$wed2 = 0
data2$wed2[which(data2$wed=='4')] <- 1
data2$wed2[which(data2$wed=='5')] <- 1
data2$wed2 = as.numeric(data2$wed2)
summary(data2["wed2"])
data2["wed3"]=data2$v_marst
data2$wed3 = 0
data2$wed3[which(data2$wed=='1')] <- 1
data2$wed3 = as.numeric(data2$wed3)

#образование
data2["h_educ"] = data2$v_educ
data2["higher_educ"] = data2$v_educ
data2["higher_educ"] = 0
data2$higher_educ[which(data2$h_educ=='21')] <- 1
data2$higher_educ[which(data2$h_educ=='22')] <- 1
data2$higher_educ[which(data2$h_educ=='23')] <- 1

#возраст c элементами нормализации
age1 = as.character(data2$v_age)
age2 = lapply(age1, as.integer)
age3 = as.numeric(unlist(age2))
data2["age"] = (age3 - mean(age3)) / sqrt(var(age3))
data2["age"]
summary(data2["age"])

#населенный пункт
data2["status1"]=data2$status
data2["status2"] = 0
data2$status2[which(data2$status1=='1')] <- 1
data2$status2[which(data2$status1=='2')] <- 1
data2$status2 = as.numeric(data2$status2)

#продолжительность рабочей недели
dur1 = as.character(data2$vj6.2)
dur2 = lapply(dur1, as.integer)
dur3 = as.numeric(unlist(dur2))
data2["dur"] = (dur3 - mean(dur3)) / sqrt(var(dur3))



data3 = select(data2, salary, age, sex, higher_educ, status2, dur, wed1,wed2,wed3)
glimpse(data3)

#графики парных зависимостей
ggpairs(data3)

#уровни факторных переменных
levels(data2$v_educ)
qplot(data = data3, salary)


# Пункт 1
model1 = lm(data = data3, salary ~ age + sex + higher_educ + status2 + dur + wed1 + wed2 + wed3)
summary(model1)
vif(model1)
# У всех коэффициентов vif меньше 10 => показатель хороший.

# Пункт 2
model2 = lm(data = data3, salary ~ log(age) + sex + higher_educ + status2 + dur + wed1 + wed2 + wed3)
summary(model2)
vif(model2)


model3 = lm(data = data3, salary ~ age + sex + higher_educ + status2 + dur  + wed1 + wed2 + wed3 + age*dur)
summary(model3)
vif(model3)


model4 = lm(data = data3, salary ~ age + sex + higher_educ + status2 + log(dur) + wed1 + wed2 + wed3)
summary(model4)
vif(model4)

model5 = lm(data = data3, salary ~ age + log(age) + sex + higher_educ + status2 + log(dur) + wed1 + wed2 + wed3 + age*dur)
summary(model5)
vif(model5)

model5.1 = lm(data = data3, salary ~ I(age^2) + sex + higher_educ + status2 + dur + wed1 + wed2 + wed3)
summary(model5.1)#R^2 = 0.023
vif(model5.1)

model5.2 = lm(data = data3, salary ~ age + I(sex^2) + I(higher_educ^2) + status2 + dur + wed1 + wed2 + wed3)
summary(model5.2)#R^2 = 0.0255
vif(model5.2)

model5.3 = lm(data = data3, salary ~ age + sex + I(higher_educ^2) + status2 + dur + wed1 + wed2 + wed3)
summary(model5.3)#R^2 = 0.0255
vif(model5.3)

model5.4 = lm(data = data3, salary ~ age + sex + higher_educ + I(status2^0.1) + I(dur^2) + wed1 + wed2 + wed3)
summary(model5.4)#R^2 = 0.0255
vif(model5.4)

model6 = lm(data = data3, salary~age + sex + higher_educ + status2 + I(dur^2) + dur + wed1 + wed2 + wed3)
summary(model6)
vif(model6)
# Лучшая модель R^2 = 0.02608

model7 = lm(data = data3, salary^0.5~age + sex + higher_educ + status2 + I(dur^2) + dur + wed1 + wed2 + wed3)
summary(model7)
vif(model7)
#R^2 = 0.08, однако половина коэффициентов не значима.

model8 = lm(data = data3, salary^2~age + sex + higher_educ + status2 + I(dur^2) + dur + wed1 + wed2 + wed3)
summary(model8)
vif(model8)
#Модель по данным похожа на модель 6, однако модель номер 6 чуть лучше.

# Пункт 3

model6 = lm(data = data3, salary~age + sex + higher_educ + status2 + I(dur^2) + dur + wed1 + wed2 + wed3)
summary(model6)
# Лучшая модель R^2 = 0.0252


# Пункт 4

#Можно сделать вывод, что индивиды, чья рабочая неделя в (dur + 1) раз больше , получают большую зарплату.

# Пункт 5

data4 = read.csv("r21i_os26c.csv")
glimpse(data4)

data4$qj13.2[which(data4$qj13.2 >= 99999990)] = NaN
data4$q_age[which(data4$q_age >= 99999990)] = NaN
data4$qh5[which(data4$qh5 >= 99999990)] = NaN
data4$q_educ[which(data4$q_educ >= 99999990)] = NaN
data4$status[which(data4$status >= 99999990)] = NaN
data4$qj6.2[which(data4$qj6.2 >= 99999990)] = NaN

data4$q_marst[which(data4$q_marst >= 99999990)] = NaN
data4$region[which(data4$region >= 99999990)] = NaN
data4$qj1.1.2[which(data4$qj1.1.2 >= 99999990)] = NaN
data4$q_occup08[which(data4$q_occup08 >= 99999990)] = NaN
data4$qj23[which(data4$qj23 >= 99999990)] = NaN

data5 = select(data4, qj13.2, q_age, qh5, q_educ, status, qj6.2, q_marst)

#исключаем строки с отсутствующими значениями NA
data5 = na.omit(data5)
glimpse(data5)

#зарплата c элементами нормализации
data5$qj13.2
sal = as.numeric(data5$qj13.2)
#summary(sal)
mean(sal)
data5["salary"] = (sal - mean(sal)) / sqrt(var(sal))
data5["salary"]
summary(data5["salary"])

#пол
data5["sex"]=data5$qh5
data5$sex[which(data5$sex!='1')] <- 0
data5$sex[which(data5$sex=='1')] <- 1
data5$sex = as.numeric(data5$sex)
summary(data5["sex"])


#семейное положение
data5["wed2"]= data5$q_marst
data5$wed1 = 0
data5$wed1[which(data5$wed=='4')] <- 1
data5$wed1 = as.numeric(data5$wed1)
summary(data5["wed2"])

data5["wed3"]=data5$q_marst
data5$wed3 = 0
data5$wed3[which(data5$wed=='1')] <- 1
data5$wed3 = as.numeric(data5$wed3)
summary(data5["wed3"])

#образование
data5["h_educ"] = data5$q_educ
data5["higher_educ"] = data5$h_educ
data5["higher_educ"] = 0
data5$higher_educ[which(data5$q_educ=='21')] <- 1
data5$higher_educ[which(data5$q_educ=='22')] <- 1
data5$higher_educ[which(data5$q_educ=='23')] <- 1

#возраст c элементами нормализации
age1 = as.character(data5$q_age)
age2 = lapply(age1, as.integer)
age3 = as.numeric(unlist(age2))
data5["age"] = (age3 - mean(age3)) / sqrt(var(age3))
data5["age"]
summary(data5["age"])

#населенный пункт
data5["status1"]=data5$status
data5["status2"] = 0
data5$status2[which(data5$status1=='2')] <- 1
data5$status2 = as.numeric(data5$status2)

#продолжительность рабочей недели
dur1 = as.character(data5$qj6.2)
dur2 = lapply(dur1, as.integer)
dur3 = as.numeric(unlist(dur2))
data5["dur"] = (dur3 - mean(dur3)) / sqrt(var(dur3))
summary(data5["dur"])


# Выделил подмножество индивидов, не вступавших в брак.
data6 = subset(data5, wed3 == 1)
data6

# Выделил подмножество индивидов, которые живут в городе.
data7 = subset(data6, status2 == 1)
data7

# Модель для подмножества индивидом, которые не вступили в брак и которые живут в городе.
model9 = lm(salary ~ age + sex + higher_educ + status2 + I(dur^2) + dur + wed3 , data = data7)
summary(model9)
# salary = 9 - 2.65*age + 6.4*higher_educ - 2*I(dur^2) + 7.2*dur

# Выделил подмножество индивидов, которые разведены.
data8 = subset(data5, wed2 == 1)
data8

# Выделил подмножество индивидов, которые имеют высшее образование.
data9 = subset(data8, higher_educ == 1)
data9

# Выделил подмножество женщин.
data10 = subset(data9, sex == 0)
data10

# Модель для подмножества женщин, которые разведены и которые имеют высшее образование.
model10 = lm(salary ~ age + sex + higher_educ + status2 + I(dur^2) + dur + wed2, data = data10)
summary(model10)
# salary = 9.6 - 2.65*age + 5.2*status2 - 2*I(dur^2) + 7.2*dur

# model9
# salary = 9 - 2.65*age + 6.4*higher_educ - 2*I(dur^2) + 7.2*dur

# model10
# salary = 9.6 - 2.65*age + 5.2*status2 - 2*I(dur^2) + 7.2*dur

# Сравнивая модели 9 и 10, можно сделать вывод, что наибольшую зарплату получают индивиды, которые живут в городе и не состоят в браке,
# чем женщины, которые разведены и имеют высшее образование.


