library('lmtest')
library("car")
data = attitude
help(attitude)
data = na.omit(attitude)


#------------------------------------------------------------------------------------------------------
#1
#------------------------------------------------------------------------------------------------------

#Для начала проверим датасет на наличие линейной зависимости
#summary(lm(rating~complaints, data)) #R^2 = 0.6813
#summary(lm(rating~privileges, data)) #R^2 = 0.1816
#summary(lm(rating~learning, data)) #R^2 = 0.389
#summary(lm(rating~raises, data)) #R^2 = 0.3483
#summary(lm(rating~critical, data)) #R^2 = 0.02447
#summary(lm(rating~advance, data)) #R^2 = 0.02405
#summary(lm(complaints~privileges, data)) #R^2 = 0.3117
#summary(lm(complaints~learning, data)) #R^2 = 0.3561
#summary(lm(complaints~raises, data)) #R^2 = 0.4478
#summary(lm(complaints~critical, data)) #R^2 = 0.03524
#summary(lm(complaints~advance, data)) #R^2 = 0.05044
#summary(lm(privileges~critical, data)) #R^2 = 0.02168
#summary(lm(privileges~advance, data)) #R^2 = 0.1179
#summary(lm(learning~critical, data)) #R^2 = 0.01345
#summary(lm(learning~advance, data)) #R^2 = 0.2826
#summary(lm(raises~critical, data)) #R^2 = 0.142
#summary(lm(raises~advance, data)) #R^2 = 0.3297
#summary(lm(critical~advance, data)) #R^2 = 0.08028

summary(lm(privileges~learning, data)) #R^2 = 0.2434 < 0.3 => нет линейной зависимости
summary(lm(privileges~raises, data)) #R^2 = 0.1985 < 0.3 => нет линейной зависимости
summary(lm(learning~raises, data)) #R^2 = 0.41 => VIF = 1,67 = > линейная зависимость есть, но незначительная

#------------------------------------------------------------------------------------------------------
#2
#------------------------------------------------------------------------------------------------------

#Построим модель rating~privileges+learning+raises

model1.1 = lm(rating~privileges+learning+raises, data)
model1.1
summary(model1.1)
#R^2 = 0.3963 , модель плоха, но не безнадёжна
#p-значение privileges очень велико, попробуем исключить его в следующей модели

model1.2 = lm(rating~learning+raises, data)
model1.2
summary(model1.2)
#R^2 > 0.3963 (=0.41) модель стала лучше, хотя всё ещё плоха
#p-значения регрессоров приемлемы

#Таким образом модель rating~learning+raises является лучшей лмнейной моделью, R^2 = 0.41

#------------------------------------------------------------------------------------------------------
#3
#------------------------------------------------------------------------------------------------------

#Введём в модель логарифмы регрессоров

vif(lm(rating~privileges+learning+log(raises), data)) #зависимости слабые
model2.1 = lm(rating~privileges+learning+log(raises), data)
model2.1
summary(model2.1)
#R^2 = 0.4055 , модель плоха, но не безнадёжна
#p-значение privileges очень велико, попробуем исключить его в следующей модели

vif(lm(rating~learning+log(raises), data)) #зависимости слабые
model2.2 = lm(rating~learning+log(raises), data)
model2.2
summary(model2.2)
#R^2 > 0.4055 (=0.4198), модель стала лучше, но всё ещё плоха
#p-значения регрессоров приемлемы


vif(lm(rating~privileges+log(learning)+raises, data)) #зависимости слабые
model3.1 = lm(rating~privileges+log(learning)+raises, data)
model3.1
summary(model3.1)
#R^2 = 0.382 , модель плоха, но не безнадёжна
#p-значение privileges очень велико, попробуем исключить его в следующей модели

vif(lm(rating~log(learning)+raises, data)) #зависимости слабые
model3.2 = lm(rating~log(learning)+raises, data)
model3.2
summary(model3.2)
#R^2 > 0.382 (= 0.3948), модель стала лучше, но всё ещё плоха
#p-значения регрессоров приемлемы


vif(lm(rating~privileges+log(learning)+log(raises), data)) #зависимости слабые
model4.1 = lm(rating~privileges+log(learning)+log(raises), data)
model4.1
summary(model4.1)
#R^2 = 0.3902 , модель плоха, но не безнадёжна
#p-значение privileges очень велико, попробуем исключить его в следующей модели

vif(lm(rating~log(learning)+log(raises), data)) #зависимости слабые
model4.2 = lm(rating~log(learning)+log(raises), data)
model4.2
summary(model4.2)
#R^2 > 0.3902 (=0,4033), модель стала лучше, но всё ещё плоха
#p-значения регрессоров приемлемы


vif(lm(rating~log(privileges)+learning+raises, data)) #зависимости слабые
model5.1 = lm(rating~log(privileges)+learning+raises, data)
model5.1
summary(model5.1)
#R^2 = 0.3982 , модель плоха, но не безнадёжна
#p-значение и std error у log(privileges) очень велики, попробуем исключить его в следующей модели

vif(lm(rating~learning+raises, data)) #зависимости слабые
model5.2 = lm(rating~learning+raises, data)
model5.2
summary(model5.2)
#R^2 > 0.3982 (=0.41), модель стала лучше, но всё ещё плоха
#p-значения регрессоров приемлемы


vif(lm(rating~log(privileges)+learning+log(raises), data)) #зависимости слабые
model6.1 = lm(rating~log(privileges)+learning+log(raises), data)
model6.1
summary(model6.1)
#R^2 = 0.4071 , модель плоха, но не безнадёжна
#p-значение и std error у log(privileges) очень велики, 
#модель rating~learning+log(raises) уже была рассмотрена (model2.2)
#в model2.2 R^2 > 0.4071 (=0.4198) => model2.2 лучше model6.1, хотя всё ещё плохая


vif(lm(rating~log(privileges)+log(learning)+raises, data)) #зависимости слабые
model7.1 = lm(rating~log(privileges)+log(learning)+raises, data)
model7.1
summary(model7.1)
#R^2 = 0.3839 , модель плоха, но не безнадёжна
#p-значение и std error у log(privileges) очень велики,
#модель rating~log(learning)+raises уже была рассмотрена (model3.2)
#в model3.2 R^2 > 0.3839 (=0.3948) => model3.2 лучше model7.1, хотя всё ещё плохая


vif(lm(rating~log(privileges)+log(learning)+log(raises), data)) #зависимости слабые
model8.1 = lm(rating~log(privileges)+log(learning)+log(raises), data)
model8.1
summary(model8.1)
#R^2 = 0.3918
#p-значение и std error у log(privileges) очень велики,
#модель rating~log(learning)+log(raises) уже была рассмотрена (model4.2)
#в model4.2 R^2 > 0.3918 (=0.4033) => model4.2 лучше model8.1, хотя всё ещё плохая

#Таким образом зависимость rating~learning+log(raises) является лучшей: R^2 = 0.4198 и все регрессоры значимы

#------------------------------------------------------------------------------------------------------
#4
#------------------------------------------------------------------------------------------------------

#Рассмотрим модели построенные на произведениях пар регрессоров

model9 = lm(rating~I(privileges^2), data)
model9
summary(model9) 
#R^2 = 0.1206

model10 = lm(rating~I(learning^2), data)
model10
summary(model10)
#R^2 = 0.3753

model11 = lm(rating~I(raises^2), data)
model11
summary(model11)
#R^2 = 0.3006

model12 = lm(rating~I(privileges*learning), data)
model12
summary(model12)
#R^2 = 0.3254

model13 = lm(rating~I(privileges*raises), data)
model13
summary(model13)
#R^2 = 0.2933

model14 = lm(rating~I(learning*raises), data)
model14
summary(model14)
#R^2 = 0.435

vif(lm(rating~I(privileges^2)+I(learning^2), data))
model15 = lm(rating~I(privileges^2)+I(learning^2), data)
model15
summary(model15)
#R^2 = 0.3698

vif(lm(rating~I(privileges^2)+I(raises^2), data))
model16 = lm(rating~I(privileges^2)+I(raises^2), data)
model16
summary(model16)
#R^2 = 0.3068

vif(lm(rating~I(privileges^2)+I(privileges*learning), data))
model17 = lm(rating~I(privileges^2)+I(privileges*learning), data)
model17
summary(model17)
#R^2 = 0.3309

vif(lm(rating~I(privileges^2)+I(privileges*raises), data)) #VIF = 4
model18 = lm(rating~I(privileges^2)+I(privileges*raises), data)
model18
summary(model18)
#R^2 = 0.3104

vif(lm(rating~I(privileges^2)+I(learning*raises), data))
model19 = lm(rating~I(privileges^2)+I(learning*raises), data)
model19
summary(model19)
#R^2 = 0.4216

vif(lm(rating~I(learning^2)+I(raises^2), data))
model20 = lm(rating~I(learning^2)+I(raises^2), data)
model20
summary(model20)
#R^2 = 0.4086

vif(lm(rating~I(learning^2)+I(privileges*learning), data))
model21 = lm(rating~I(learning^2)+I(privileges*learning), data)
model21
summary(model21)
#R^2 0.3618

vif(lm(rating~I(learning^2)+I(privileges*raises), data))
model22 = lm(rating~I(learning^2)+I(privileges*raises), data)
model22
summary(model22)
#R^2 = 0.4001

vif(lm(rating~I(learning^2)+I(learning*raises), data)) 
#VIF > 5

vif(lm(rating~I(raises^2)+I(privileges*learning), data))
model23 = lm(rating~I(raises^2)+I(privileges*learning), data)
model23
summary(model23)
#R^2 = 0.3707

vif(lm(rating~I(raises^2)+I(privileges*raises), data))
model24 = lm(rating~I(raises^2)+I(privileges*raises), data)
model24
summary(model24)
#R^2 = 0.3099

vif(lm(rating~I(raises^2)+I(learning*raises), data))
#VIF = 4.813011

vif(lm(rating~I(privileges^2)+learning, data))
model25 = lm(rating~I(privileges^2)+learning, data)
model25
summary(model25)
#R^2 = 0.362

vif(lm(rating~I(privileges^2)+raises, data))
model26 = lm(rating~I(privileges^2)+raises, data)
model26
summary(model26)
#R^2 = 0.3282

vif(lm(rating~I(privileges^2)+privileges, data))
#VIF = 53.88317

vif(lm(rating~I(learning^2)+privileges, data))
model27 = lm(rating~I(learning^2)+privileges, data)
model27
summary(model27)
#R^2 = 0.3719

vif(lm(rating~I(learning^2)+learning, data))
#VIF = 106.0369

vif(lm(rating~I(learning^2)+raises, data))
model28 = lm(rating~I(learning^2)+raises, data)
model28
summary(model28)
#R^2 = 0.4213

vif(lm(rating~I(raises^2)+privileges, data))
model29 = lm(rating~I(raises^2)+privileges, data)
model29
summary(model29)
#R^2 = 0.3155

vif(lm(rating~I(privileges^2)+learning+log(raises), data))
model30 = lm(rating~I(privileges^2)+learning+log(raises), data)
model30
summary(model30)
#R^2 = 0.4049

vif(lm(rating~I(learning^2)+learning+log(raises), data))
#VIF > 114 

vif(lm(rating~I(raises^2)+learning+log(raises), data))
#VIF > 25

vif(lm(rating~I(privileges*learning)+learning+log(raises), data))
model31 = lm(rating~I(privileges*learning)+learning+log(raises), data)
model31
summary(model31)
#R^2 = 0.401

vif(lm(rating~I(raises*learning)+learning+log(raises), data))
#VIF > 14

vif(lm(rating~I(privileges*raises)+learning+log(raises), data))
model32 = lm(rating~I(privileges*raises)+learning+log(raises), data)
model32
summary(model32)
#R^2 = 0.4008

vif(lm(rating~I(privileges^2)+learning+raises, data))
model33 = lm(rating~I(privileges^2)+learning+raises, data)
model33
summary(model33)
#R^2 = 0.3955

#Таким образом лучшей моделью является rating~I(learning^2)+raises , т.к. R^2 = 0.4213

#------------------------------------------------------------------------------------------------------
#Часть 2
#------------------------------------------------------------------------------------------------------

#Найдём доверительные интервалы для коэффициентов лучшей модели из первой части задания при p = 95%
model28 = lm(rating~I(learning^2)+raises, data)
model28
summary(model28)

Intercept_coef = qt(0.95, df=24)*11.223415 # 19.2019395988867
learning_coef = qt(0.95, df=24)*0.001660 # 0.00284006425264965
raises_coef = qt(0.95, df=24)*0.211003 # 0.361001251507129

27.055473-Intercept_coef # 7.853533
27.055473+Intercept_coef # 46.25741
#Intercept_Interval = [7.853533 , 46.25741], 0 не входит

0.003949-learning_coef # 0.001108936
0.003949+learning_coef # 0.006789064
#learning_Interval = [0.001108936, 0.006789064], 0 не входит

0.211003-raises_coef # -0.1499983
0.211003+raises_coef # 0.5720043
#raises_Interval = [-0.1499983, 0.5720043] 0 входит

new.data = data.frame(learning = 75, raises = 70)
predict(model28, new.data, interval = "confidence")
#[68.27085, 83.34731]








