
setwd("C:\\data")
weatherAUS = read.csv('weatherAUS.csv')  


df = weatherAUS[ ,c(-1,-2, -22, -23)]
str(df)

sqrt(ncol(df)) # 4.242641

df$RainTomorrow <- as.factor(df$RainTomorrow)

model_weather <- randomForest(RainTomorrow ~ ., 
                              data = df, 
                              ntree=400, mtry=4, 
                              importance = T,
                              na.action=na.omit)
model_weather # error rate: 14.42%
names(model_weather)

#  Confusion matrix 
con <- model_weather$confusion
con

acc <- (con[1,1]+con[2,2]) / sum(con)
cat('accuracy =', acc)

#  TOP3 변수
varImpPlot(model_weather)
# Humidity3pm(습도), Sunshine(햇빛), WindGustSpeed(돌풍속도) 


##########################


bank = read.csv('UniversalBank.csv',  stringsAsFactors = F)
str(bank) 

#'data.frame':	5000 obs. of  14 variables:
#$ ID                :고객구분(제외)  int  1 2 3 4 5 6 7 8 9 10 ...
#$ Age               :나이  int  25 45 39 35 35 37 53 50 35 34 ...
#$ Experience        :경력  int  1 19 15 9 8 13 27 24 10 9 ...
#$ Income            :소득  int  49 34 11 100 45 29 72 22 81 180 ...
#$ ZIP.Code          :우편번호(제외)  int  91107 90089 94720 94112 91330 92121 91711 93943 90089 93023 ...
#$ Family            :가족수  int  4 3 1 1 4 4 2 1 3 1 ...
#$ CCAvg             :월 신용카드 사용액  num  1.6 1.5 1 2.7 1 0.4 1.5 0.3 0.6 8.9 ...
#$ Education         :교육수준  int  1 1 1 2 2 2 2 3 2 3 ...
#$ Mortgage          :담보채권  int  0 0 0 0 0 155 0 0 104 0 ...
#$ Personal.Loan     :개인대출(Y변수:수락 or 거절)  int  0 0 0 0 0 0 0 0 0 1 ...
#$ Securities.Account:유가증권계정  int  1 1 0 0 0 0 0 0 0 0 ...
#$ CD.Account        :CD계좌  int  0 0 0 0 0 0 0 0 0 0 ...
#$ Online            :온라인뱅킹  int  0 0 0 0 0 1 1 0 1 0 ...
#$ CreditCard        :신용카드  int  0 0 0 0 1 0 0 1 0 0 ...


dim(bank) # 5000   14
bank = bank[c(-1, -5)] # ID, ZIP.Code 제외 
dim(bank) # 5000   12
sqrt(11)


# Personal.Loan : 대출 수락 or 거절
bank$Personal.Loan <- as.factor(bank$Personal.Loan)


model_bank <- randomForest(Personal.Loan ~ ., 
                              data = bank, 
                              ntree=500, mtry=3, 
                              importance = T,
                              na.action=na.omit)
model_bank # error rate: 14.42%

# Confusion matrix 
con <- model_bank$confusion
con

acc <- (con[1,1]+con[2,2]) / sum(con)
cat('accuracy =', acc) # accuracy = 0.9883787

# TOP3 변수
varImpPlot(model_bank)
# Income > Education > Family

