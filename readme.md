<h1 align="center"> 🚕 서울시 장애인 콜택시 이용 데이터 분석 </h1>
<h4 align="center"> 23-1학기 의생명 통계학 기말프로젝트 </h4>

## Introduction
- 주제: 서울시 장애인 콜택시 이용 데이터 분석을 통한 이용 개선점 도출
- 목적: 장애인 콜택시 대기시간을 생존 시간으로 정의하여 생존 분석에 적용 및 분석하여 장애인 콜택시 운영 개선점 도출하고자 함
- 분석 기간: 2023.05 ~ 2023.06
- 데이터 출처 및 제공
    - 서울시설공단
    - 서울시 열린 데이터 광장
    - 공공데이터포털
- 참여자
    - [정다운](https://github.com/daunJJ)
    - [최윤서](https://github.com/YunSeo00)


## Content
**데이터 전처리**
- [장애인 콜택시 이용 데이터 전처리 및 유의한 변수 생성](https://github.com/daunJJ/CallTaxi/blob/master/full_year_preprocessing.ipynb) 
- [인구, 위치, 교통, 복지 요인 데이터 Join 및 파생 변수 생성](https://github.com/daunJJ/CallTaxi/blob/master/table_join.ipynb)

**생존함수 모형 정의**

<img src="https://github.com/daunJJ/Survival_Analysis/assets/109944763/0e29b4de-0cd4-4a0d-841e-f506aca91c49" width="500"/>

- 대기시간 정의에 따라 총 3가지의 모형 적합
    - Z모형: 호출부터 승차까지 걸린 시간 = 총 대기시간
    - X모형: 호출부터 배차까지 걸린 시간 = 배차 대기시간
    - Y모형: 배차부터 승차까지 걸린 시간 = 승차 대기시간

**생존 모형 적합**
- [모수적 생존모형, Coxph 회귀모형, RandomForest 모형 적합](https://github.com/daunJJ/CallTaxi/blob/master/survival_analysis.R)

## Presentation
[<img src="https://github.com/daunJJ/CallTaxi/assets/109944763/90d018c4-fc4f-4492-967f-661093f65c0f" width="500"/>](https://github.com/daunJJ/CallTaxi/blob/master/%EC%9E%A5%EC%95%A0%EC%9D%B8%20%EC%BD%9C%ED%83%9D%EC%8B%9C%20%EC%9D%B4%EC%9A%A9%20%EB%8D%B0%EC%9D%B4%ED%84%B0%20%EB%B6%84%EC%84%9D.pdf)

