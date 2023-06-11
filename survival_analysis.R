final_data = read.csv('./data/final_data/final_data.csv')

#head(final_data)

## set.seed
set.seed(2023)

## require library
library(survival)
library(randomForestSRC)

## 생존분석 모형 적합을 위한 변수 생성
# time : event time
# status : censoring(0) or not censoring(1)

# X
final_data$time1 = NA
final_data$status1 = NA
# Y
final_data$time2 = NA
final_data$status2 = NA
# Z
final_data$time3 = NA
final_data$status3 = NA

# time1
censoring_idx = as.numeric(rownames(final_data[is.na(final_data$dispatch_waiting_time),]))
no_censoring_idx = as.numeric(rownames(final_data[!is.na(final_data$dispatch_waiting_time),]))

final_data[censoring_idx, ]$time1 = final_data[censoring_idx,]$cancel_time
final_data[censoring_idx, ]$status1 = 0
final_data[no_censoring_idx, ]$time1 = final_data[no_censoring_idx,]$dispatch_waiting_time
final_data[no_censoring_idx, ]$status1 = 1

# time2
censoring_idx = as.numeric(rownames(final_data[is.na(final_data$total_waiting_time),]))
no_censoring_idx = as.numeric(rownames(final_data[!is.na(final_data$total_waiting_time),]))

final_data[censoring_idx, ]$time2 = final_data[censoring_idx, ]$cancel_time - final_data[censoring_idx, ]$dispatch_waiting_time
final_data[censoring_idx, ]$status2 = 0
final_data[no_censoring_idx, ]$time2 = final_data[no_censoring_idx, ]$total_waiting_time - final_data[no_censoring_idx, ]$dispatch_waiting_time
final_data[no_censoring_idx, ]$status2 = 1


# time3
censoring_idx = as.numeric(rownames(final_data[is.na(final_data$total_waiting_time),]))
no_censoring_idx = as.numeric(rownames(final_data[!is.na(final_data$total_waiting_time),]))

final_data[censoring_idx, ]$time3 = final_data[censoring_idx, ]$cancel_time
final_data[censoring_idx, ]$status3 = 0
final_data[no_censoring_idx, ]$time3 = final_data[no_censoring_idx, ]$total_waiting_time
final_data[no_censoring_idx, ]$status3 = 1

colSums(is.na(final_data))

final_data[final_data$time1 == 0,'time1'] = 0.1 # time1이 0인 데이터는 0.1로 대체
final_data[!is.na(final_data$time2)& (final_data$time2 == 0),'time2'] = 0.1 # time2가 0인 데이터는 0.1로 대체

# year 변수
final_data$year = ifelse((final_data$year == 2018) | (final_data$year == 2019), "before_COVID", "after_COVID")

#colnames(final_data)

final_data$using_time = as.factor(final_data$using_time)
final_data$using_day = as.factor(final_data$using_day)
final_data$이용목적 = as.factor(final_data$이용목적)
final_data$year = as.factor(final_data$year)

#str(final_data)

final_data$status1 = as.integer(final_data$status1)
final_data$status2 = as.integer(final_data$status2)
final_data$status3 = as.integer(final_data$status3)


## test data 생성 - dispatch가 null이 아닌 데이터
test_data = final_data[!is.na(final_data$time2),]
test_idx = as.numeric(sample(rownames(test_data), 10000, replace = FALSE))
test_data = final_data[test_idx,]
#print(c(nrow(test_data), ncol(test_data)))

## final_data에서 test_data 삭제
#print(c(nrow(final_data), ncol(final_data)))
final_data = final_data[-test_idx,]
#print(c(nrow(final_data), ncol(final_data)))

## 변수 type 변환
# as.factor : using_time, using_day, 이용목적
# numeric : 출발동_최근접_차고지_거리, 이동거리, 
#           총_종사자_수, 보건업_종사자_수, 상대적_보건업_종사자_수, 
#           구별_장애인_시설_수, 장애인구수, 인구수, 상대적_장애인구수



# sample_data1 (for X and Z)
idx = sample(rownames(final_data), 100000, replace = FALSE)
sample_data1 = final_data[idx,]

# sample_data2 (for Y)
sample_data2 <- final_data[!is.na(final_data$time2), ]
idx <- sample(1:nrow(sample_data2), 100000, replace = FALSE)
sample_data2 <- sample_data2[idx, ]


### X

## 모형 적합

# 1. 모수적 생존 모형 (gaussian, logistic)
# 1-1. gaussian
mod111 = survreg(Surv(time1,status1)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
                            총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
                            구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수, dist = 'gaussian', data = sample_data1)

summary(mod111)
concordance(mod111)$concordance # c-index: 0.6406
concordance(mod111, newdata = test_data)$concordance # test_data c-index: 0.6346

step(mod111)

# 단계적 선택법
mod112 = survreg(Surv(time1,status1)~year + using_time + 
                   using_day + 이용목적 + 출발동_최근접_차고지_거리 + 
                   이동거리 + 총_종사자_수 + 보건업_종사자_수 + 
                   구별_장애인_시설_수 + 상대적_장애인구수, dist = 'gaussian', data = sample_data1)

summary(mod112)
concordance(mod112)$concordance # train data c-index: 0.6407
concordance(mod112, newdata = test_data)$concordance # test_data c-index: 0.6347



# 1-2. logistic

mod121 = survreg(Surv(time1,status1)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
          총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
          구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수, dist = 'logistic', data = sample_data1)

summary(mod121)
concordance(mod121)$concordance # c-index: 0.6410
concordance(mod121, newdata = test_data)$concordance # test_data c-index: 0.6359
step(mod121)


# 단계적 선택법
mod122 = survreg(Surv(time1,status1)~ year + using_time + 
                   using_day + 이용목적 + 출발동_최근접_차고지_거리 + 
                   이동거리 + 총_종사자_수 + 보건업_종사자_수 + 
                   구별_장애인_시설_수 + 상대적_장애인구수, dist = 'logistic', data = sample_data1)

summary(mod122)
concordance(mod122)$concordance # train_data c-index: 0.6410
concordance(mod122, newdata = test_data)$concordance # test_data c-index: 0.6360

# 2. coxph

#2-1. 모형 적합
coxph_fit1 = coxph(Surv(time1, status1)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
                    총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
                    구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수, data = sample_data1)
summary(coxph_fit1)
concordance(coxph_fit1)$concordance # c-index: 0.6398
concordance(coxph_fit1, newdata = test_data)$concordance # test_data c-index: 0.6330

step(coxph_fit1)

# 단계적 선택법
coxph_fit2 = coxph(Surv(time1, status1)~ year + using_day + 이용목적 + 
                     출발동_최근접_차고지_거리 + 이동거리 + 총_종사자_수 + 
                     구별_장애인_시설_수 + 인구수 + 상대적_장애인구수,  data = sample_data1)

summary(coxph_fit2)
concordance(coxph_fit2)$concordance # c-index : 0.6400
concordance(coxph_fit2, newdata = test_data)$concordance # test_data c-index: 0.6331


# 2-2. Cox 비례 위험 모형 가정 검정
# 0.05 이상: 비례위험 모형을 만족한다.
cox.zph(coxph_fit2, global = F) # -> 모든 변수가 비례 위험 가정을 만족하지는 않는다.

# 3. machine learning

# # 3-1. lasso (coxph 가정 위배됨 -> 적용X)
# library(glmnet)
# 
# # tuning
# cvfit = cv.glmnet(model.matrix(coxph_fit), y, family="cox", type.measure="C")
# plot(cvfit)
# lam = cvfit&lambda.min
# 
# # final lasso
# mod31 = glmnet(model.matrix(coxph_fit), y, family = 'cox', lambda=lam)
# predict(mod31, type="coef")
# 
# # lasso모형과 coxph모형의 회귀계수 비교
# cbind(coef(glmnet_fit), coef(coxph_fit))

# 3-2 random forest
rf <- rfsrc(Surv(time1, status1)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
                                 총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
                                 구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수,
                                 data = sample_data1, forest = TRUE, ntree = 500)


print(rf)
get.cindex(rf$yvar[,1], rf$yvar[,2], rf$predicted.oob) # train_data c-index: 0.3716
rf2 = predict(rf, newdata = test_data)
get.cindex(rf2$yvar[,1], rf2$yvar[,2], rf2$predicted) # test_data c-index: 0.3733

### Y 

# 1. 모수적 생존 모형 (gaussian, logistic)
# 1-1. gaussian
mod111 = survreg(Surv(time2,status2)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
                   총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
                   구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수, dist = 'gaussian', data = sample_data2)

summary(mod111)
concordance(mod111)$concordance # c-index: 0.5716
concordance(mod111, newdata = test_data)$concordance # test_data c-index: 0.5700

step(mod111)

# 단계적 선택법 
mod112 = survreg(Surv(time2,status2)~year + using_time + 
                   using_day + 이용목적 + 출발동_최근접_차고지_거리 + 
                   이동거리 + 총_종사자_수 + 보건업_종사자_수 + 
                   상대적_보건업_종사자_수 + 구별_장애인_시설_수 + 
                   장애인구수 + 상대적_장애인구수, dist = 'gaussian', data = sample_data2)

summary(mod112)
concordance(mod112)$concordance # train data c-index: 0.5715
concordance(mod112, newdata = test_data)$concordance # test_data c-index: 0.5698


# 

# 1-2. logistic

mod121 = survreg(Surv(time2,status2)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
                   총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
                   구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수, dist = 'logistic', data = sample_data2)

summary(mod121)
concordance(mod121)$concordance # c-index: 0.5715
concordance(mod121, newdata = test_data)$concordance # test_data c-index: 0.5695
step(mod121)


# 단계적 선택법
mod123 = survreg(Surv(time2,status2)~year + using_time + 
                   using_day + 이용목적 + 출발동_최근접_차고지_거리 + 
                   이동거리 + 총_종사자_수 + 보건업_종사자_수 + 
                   상대적_보건업_종사자_수 + 구별_장애인_시설_수 + 
                   장애인구수 + 상대적_장애인구수
                   , dist = 'logistic', data = sample_data2)

summary(mod123)
concordance(mod123)$concordance # train_data c-index: 0.5715
concordance(mod123, newdata = test_data)$concordance # test_data c-index: 0.5694

# 1-3. exponential
mod131 = survreg(Surv(time2,status2)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
                   총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
                   구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수, dist = 'exponential', data = sample_data2)

summary(mod131)
concordance(mod131)$concordance # c-index: 0.5715
concordance(mod131, newdata = test_data)$concordance # test_data c-index: 0.5700
step(mod131)

# 단계적 선택법
mod132 = survreg(Surv(time2,status2)~year + using_time + 
                   using_day + 이용목적 + 출발동_최근접_차고지_거리 + 
                   이동거리 + 총_종사자_수 + 보건업_종사자_수 + 
                   상대적_보건업_종사자_수 + 구별_장애인_시설_수 + 
                   장애인구수
                 , dist = 'exponential', data = sample_data2)

summary(mod132)
concordance(mod132)$concordance # train_data c-index: 0.5716
concordance(mod132, newdata = test_data)$concordance # test_data c-index: 0.5698

# 1-4. weibull - not converge
mod141 = survreg(Surv(time2,status2)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
                   총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
                   구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수, dist = 'weibull', data = sample_data2)

summary(mod141)
concordance(mod141)$concordance # c-index: 0.5701
concordance(mod141, newdata = test_data)$concordance # test_data c-index: 0.5681
step(mod141)

# 단계적 선택법
mod142 = survreg(Surv(time2,status2)~year + using_time + 
                   using_day + 이용목적 + 출발동_최근접_차고지_거리 + 
                   이동거리 + 총_종사자_수 + 보건업_종사자_수 + 
                   상대적_보건업_종사자_수 + 구별_장애인_시설_수 + 
                   장애인구수 + 인구수 + 상대적_장애인구수
                 , dist = 'weibull', data = sample_data2)

summary(mod142)
concordance(mod142)$concordance # train_data c-index: 0.5701
concordance(mod142, newdata = test_data)$concordance # test_data c-index: 0.5681

# 1-5. lognormal
mod151 = survreg(Surv(time2,status2)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
                   총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
                   구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수, dist = 'lognormal', data = sample_data2)

summary(mod151)
concordance(mod151)$concordance # c-index: 0.5716
concordance(mod151, newdata = test_data)$concordance # test_data c-index: 0.5704
step(mod151)

# 단계적 선택법
mod152 = survreg(Surv(time2,status2)~ year + using_time + 
                   using_day + 이용목적 + 출발동_최근접_차고지_거리 + 
                   이동거리 + 총_종사자_수 + 보건업_종사자_수 + 
                   상대적_보건업_종사자_수 + 구별_장애인_시설_수 + 
                   장애인구수 + 상대적_장애인구수
                 , dist = 'lognormal', data = sample_data2)

summary(mod152)
concordance(mod152)$concordance # train_data c-index: 0.5716
concordance(mod152, newdata = test_data)$concordance # test_data c-index: 0.5703


# 2. coxph

#2-1. 모형 적합
coxph_fit1 = coxph(Surv(time2, status2)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
                     총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
                     구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수, data = sample_data2)
summary(coxph_fit1)
concordance(coxph_fit1)$concordance # c-index: 0.5705
concordance(coxph_fit1, newdata = test_data)$concordance # test_data c-index: 0.5685

step(coxph_fit1)

# 단계적 선택법
coxph_fit2 = coxph(Surv(time2, status2)~ year + using_time + using_day + 
                     이용목적 + 출발동_최근접_차고지_거리 + 이동거리 + 
                     총_종사자_수 + 보건업_종사자_수 + 상대적_보건업_종사자_수 + 
                     구별_장애인_시설_수 + 장애인구수 + 인구수 + 
                     상대적_장애인구수, data = sample_data2)
summary(coxph_fit2)
concordance(coxph_fit2)$concordance # c-index : 0.5705
concordance(coxph_fit2, newdata = test_data)$concordance # test_data c-index: 0.5685


# 2-2. Cox 비례 위험 모형 가정 검정
# 0.05 이상: 비례위험 모형을 만족한다.
cox.zph(coxph_fit2, global = F) # -> 모든 변수가 Cox 비례 위험 가정을 만족하지 않는다. 

# 3. machine learning

# 3-1 Cox 비례 위험 모형 가정 X

# 3-2 random forest
library(randomForestSRC)


rf <- rfsrc(Surv(time2, status2)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
              총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
              구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수,
            data = sample_data2, forest = TRUE, ntree = 500)


print(rf)
get.cindex(rf$yvar[,1], rf$yvar[,2], rf$predicted.oob) # train_data c-index: 0.4283
rf2 = predict(rf, newdata = test_data)
get.cindex(rf2$yvar[,1], rf2$yvar[,2], rf2$predicted) # test_data c-index: 0.4229



## X+Y

## X 최종모델 (test_data 기준으로 c-index가 가장 높은 모수적 생존모형 - logistic 가정)
X_final_mod = survreg(Surv(time1,status1)~ year + using_time + 
                   using_day + 이용목적 + 출발동_최근접_차고지_거리 + 
                   이동거리 + 총_종사자_수 + 보건업_종사자_수 + 
                   구별_장애인_시설_수 + 상대적_장애인구수, dist = 'logistic', data = sample_data1)

summary(X_final_mod)
concordance(X_final_mod)$concordance # train_data c-index: 0.6410
concordance(X_final_mod, newdata = test_data)$concordance # test_data c-index: 0.6360

## Y 최종모델 (test_data 기준으로 c-index가 가장 높은 모수적 생존모형 - gaussian 가정)
Y_final_mod = survreg(Surv(time2,status2)~ year + using_time + 
                   using_day + 이용목적 + 출발동_최근접_차고지_거리 + 
                   이동거리 + 총_종사자_수 + 보건업_종사자_수 + 
                   상대적_보건업_종사자_수 + 구별_장애인_시설_수 + 
                   장애인구수 + 상대적_장애인구수
                 , dist = 'lognormal', data = sample_data2)

summary(Y_final_mod)
concordance(Y_final_mod)$concordance # train_data c-index: 0.5716
concordance(Y_final_mod, newdata = test_data)$concordance # test_data c-index: 0.5703

# X+Y
X_test_data <- test_data[, c('year', 'using_time', 'using_day', '이용목적',
                             '출발동_최근접_차고지_거리', '이동거리', '총_종사자_수', '보건업_종사자_수',
                             '구별_장애인_시설_수', '상대적_장애인구수')]
Y_test_data <- test_data[, c('year', 'using_time', 'using_day', '이용목적',
                             '출발동_최근접_차고지_거리', '이동거리', '총_종사자_수', '보건업_종사자_수',
                             '상대적_보건업_종사자_수', '구별_장애인_시설_수', '장애인구수',
                             '상대적_장애인구수')]

X_pred <- predict(X_final_mod, newdata = X_test_data)
Y_pred <- predict(Y_final_mod, newdata = Y_test_data)
Z_pred_from_XY = X_pred+Y_pred 

Cindex(Surv(test_data$time3, test_data$status3), Z_pred_from_XY) # Z c-index (test_data) : 0.6416

### Z

# 1. 모수적 생존 모형 (gaussian, logistic)
# 1-1. gaussian
mod111 = survreg(Surv(time3,status3)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
                   총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
                   구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수, dist = 'gaussian', data = sample_data1)

summary(mod111)
concordance(mod111)$concordance # c-index: 0.6485
concordance(mod111, newdata = test_data)$concordance # test_data c-index: 0.6421

step(mod111)

# 단계적 선택법 
mod112 = survreg(Surv(time3,status3)~year + using_time + 
                   using_day + 이용목적 + 출발동_최근접_차고지_거리 + 
                   이동거리 + 총_종사자_수 + 보건업_종사자_수 + 
                   구별_장애인_시설_수 + 장애인구수 + 인구수, dist = 'gaussian', data = sample_data1)

summary(mod112)
concordance(mod112)$concordance # train data c-index: 0.6485
concordance(mod112, newdata = test_data)$concordance # test_data c-index: 0.6421


# 

# 1-2. logistic

mod121 = survreg(Surv(time3,status3)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
                   총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
                   구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수, dist = 'logistic', data = sample_data1)

summary(mod121)
concordance(mod121)$concordance # c-index: 0.6492
concordance(mod121, newdata = test_data)$concordance # test_data c-index: 0.6430
step(mod121)


# 단계적 선택법
mod123 = survreg(Surv(time3,status3) ~ year + using_time + 
                   using_day + 이용목적 + 출발동_최근접_차고지_거리 + 
                   이동거리 + 총_종사자_수 + 보건업_종사자_수 + 
                   구별_장애인_시설_수 + 장애인구수 + 인구수, 
                 dist = 'logistic', data = sample_data1)

summary(mod123)
concordance(mod123)$concordance # train_data c-index: 0.6492
concordance(mod123, newdata = test_data)$concordance # test_data c-index: 0.6431

# 2. coxph

#2-1. 모형 적합
coxph_fit1 = coxph(Surv(time3, status3)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
                     총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
                     구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수, data = sample_data1)
summary(coxph_fit1)
concordance(coxph_fit1)$concordance # c-index: 0.6478
concordance(coxph_fit1, newdata = test_data)$concordance # test_data c-index: 0.6409

step(coxph_fit1)

# 단계적 선택법
coxph_fit2 = coxph(Surv(time3, status3)~ year + using_time + using_day + 
                     이용목적 + 출발동_최근접_차고지_거리 + 이동거리 + 
                     총_종사자_수 + 상대적_보건업_종사자_수 + 구별_장애인_시설_수 + 
                     장애인구수 + 인구수, data = sample_data1)
summary(coxph_fit2)
concordance(coxph_fit2)$concordance # c-index : 0.6477
concordance(coxph_fit2, newdata = test_data)$concordance # test_data c-index: 0.6408


# 2-2. Cox 비례 위험 모형 가정 검정
# 0.05 이상: 비례위험 모형을 만족한다.
cox.zph(coxph_fit2, global = F) # -> 거의 모든 변수가 비례위험 가정을 만족하지 않는다. 

# 3. machine learning

# 3-1 Cox 비례 위험 모형 가정 X

# 3-2 random forest


rf <- rfsrc(Surv(time3, status3)~year+using_time+using_day+이용목적+출발동_최근접_차고지_거리+이동거리+
              총_종사자_수+보건업_종사자_수+상대적_보건업_종사자_수+
              구별_장애인_시설_수+장애인구수+인구수+상대적_장애인구수,
            data = sample_data2, forest = TRUE, ntree = 500)


print(rf)
get.cindex(rf$yvar[,1], rf$yvar[,2], rf$predicted.oob) # train_data c-index: 0.3604
rf2 = predict(rf, newdata = test_data)
get.cindex(rf2$yvar[,1], rf2$yvar[,2], rf2$predicted) # test_data c-index: 0.3639

## 최종 모형
Z_final_mod = survreg(Surv(time3,status3) ~ year + using_time + 
                   using_day + 이용목적 + 출발동_최근접_차고지_거리 + 
                   이동거리 + 총_종사자_수 + 보건업_종사자_수 + 
                   구별_장애인_시설_수 + 장애인구수 + 인구수, 
                 dist = 'logistic', data = sample_data1)

summary(Z_final_mod)
concordance(Z_final_mod)$concordance # train_data c-index: 0.6492
concordance(Z_final_mod, newdata = test_data)$concordance # test_data c-index: 0.6431


### multiple event survival analysis



