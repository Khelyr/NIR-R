library('lmtest')

data = swiss
help(swiss)

#1
#Среднее значение
mean(data$Infant.Mortality) #19.94255
mean(data$Agriculture) #50.65957
mean(data$Examination) #16.48936

#Дисперсия
var(data$Infant.Mortality) #8.483802
var(data$Agriculture) #515.7994
var(data$Examination) #63.64662

#Средне-квадратическое отклонение (СКО)
sd(data$Infant.Mortality) #2.912697
sd(data$Agriculture) #22.71122
sd(data$Examination) #7.977883

#2
#Infant.Mortality~Agriculture
model1 = lm(Infant.Mortality~Agriculture, data)
model1
summary(model1)
#Infant.Mortality = 20.337955 - 0.007805*Agriculture

#Infant.Mortality~Examination
model2 = lm(Infant.Mortality~Examination, data)
model2
summary(model2)
#Infant.Mortality = 20.62899 - 0.04163*Examination

#3
#model1: R^2 = 0.003704 < 0.3 => модель плоха, зависимость практически отсутствует
#model2: R^2 = 0.013 < 0.3 => модель плоха, зависимость практически отсутствует

#4
#model1: так как p-статистика модели >0.05 (=0.684) => причинно-следственная связь 
#между регрессором и объясняемой переменной крайне мала (или вовсе отсутствует)

##model2: так как p-статистика модели >0.05 (=0.445) => причинно-следственная связь 
#между регрессором и объясняемой переменной крайне мала (или вовсе отсутствует)

