<h1 align="center"> 🚕 서울시 장애인 콜택시 이용 데이터 분석 </h1>
<h4 align="center"> 2023-1학기 의생명 통계학 기말프로젝트 </h4>

## Introduction

- 주제: 서울시 장애인 콜택시 이용 데이터 분석을 통한 이용 개선점 도출
- 분석 기간: 2023.05 ~ 2023.06
- 데이터 출처 및 제공
    - 서울시설공단
    - 서울시 열린 데이터 광장
    - 공공데이터포털
- 참여자
    - [정다운](https://github.com/daunJJ)
    - [최윤서](https://github.com/YunSeo00)

<br>

## 문제 인식 및 목표 설정

- 문제 인식:
    - 장애인 이동권에 대한 사회적 문제
    - ‘장애인콜택시’ 서비스가 실질적으로 장애인에게 이동 편의를 제공하고 있는지에 대한 실상을 파악하고 문제점을 개선하고자 함
- 목표 설정:
    - 장애인 콜택시 대기시간을 생존 시간으로 정의하여 생존 분석에 적용
    - 생존분석을 통해 대기시간에 영향을 미치는 요인을 분석하고 이를 토대로 장애인 콜택시 운영 개선점 도출
- 결과: 저조한 Accuracy
- 문제점: 171개의 class를 구분하기 위해서는 다량의 데이터를 처리할 수 있는 환경 필요

<br>

## 문제 해결 과정

### 데이터 전처리
  - 수집한 데이터를 이용하여 규모를 조정하고, 범주화를 하는 등 파생변수 생성
  - 택시를 미리 예약하는 것이 아닌 ‘바로콜’ 형식의 데이터만 추출
  - 접수 일시, 배차 일시, 탑승 일시, 취소 일시 등의 시간 변수를 접수 일시를 기준으로 소요 시간으로 변경

### 생존함수 모형 정의

  <img src="https://github.com/daunJJ/Survival_Analysis/assets/109944763/0e29b4de-0cd4-4a0d-841e-f506aca91c49" width="700" height= "250"/>
  
- ‘장애인 콜택시 대기시간’을 ‘생존시간’으로 정의
- 콜택시 호출 중 ‘취소’한 경우를 ‘중도절단’으로 정의
- 대기시간 정의에 따라 총 3가지의 모형 적합
    - Z모형: 호출부터 승차까지 걸린 시간 = 총 대기시간
    - X모형: 호출부터 배차까지 걸린 시간 = 배차 대기시간
    - Y모형: 배차부터 승차까지 걸린 시간 = 승차 대기시간

### 생존 모형 적합
  - 각 모형마다 모수적 생존모형, Coxph 회귀모형, RandomForest 모형을 적합
  - C-index를 비교하여 최종 모형 선정
