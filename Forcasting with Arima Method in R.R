
##1. preprocessing Data dan identifikasi model

#Import dan Plot Data (memanggil data asli dan membuat plot grafik)

DataCovid <- ts(datauji$`Kasus Positif Covid Harian`)
ts.plot(DataCovid, col="black", ylab="Jumlah Kasus Covid", xlab="Hari" , main="Plot Peramalan Kasus Positif Covid-19")
DataCovid

#Uji akar unit(augmented dickey fuller) terhadap data asli
library(tseries)
adf.test(DataCovid)

#uji adf menunjukkan bahwa hipotesis nol adanya unit root dalam data (data tidak stasioner) 
#di terima yang dapat dikonfirmasikan dengan menggunakan plot ACF yang meluruh secara 
#lambat menuju nol 

#Transformasi akar terhadap data asli

DataCovid_trans=sqrt(DataCovid)

##Differencing thd data hasil transformasi akar 
DataCovid_diff=diff(DataCovid_trans)

#Plot Grafik Data Dari Hasil Differencing 
ts.plot(DataCovid_diff, col="black", ylab="Jumlah Kasus Covid", xlab="Hari" , main="Plot Peramalan Kasus Positif Covid-19")

#Uji Akar Unit(Augmented Dickey Fuller Test) terhadap Data Hasil Differencing
adf.test(DataCovid_diff)



library(forecast)

#dataAA=auto.arima(DataCovid,trace = TRUE)


#2. Estimasi Parameter Dari Model
#Membuat Plot Grafik Acf Brdsrkn Data hasil differencing 


acf(DataCovid_diff, lag.max = 36)

#membuat plot grafik pacf berdsrkn data hasil differencing 
pacf(DataCovid_diff, lag.max = 36)

library(forecast)

#Kemungkinan lain . 0,1,2
#dugaanku=arima(DataCovid_trans, order=c(0,1,2))
#summary(dugaanku)


#Model 1 ARIMA (0,1,1)
dugaan1=arima(DataCovid_trans, order=c(0,1,1))
summary(dugaan1)

#Model 2 ARIMA (0,1,2)
dugaan2=arima(DataCovid_trans, order=c(0,1,2))
summary(dugaan2)

#Model 3 ARIMA (0,1,3)
dugaan3=arima(DataCovid_trans, order=c(0,1,3))
summary(dugaan3)

#Model 4 ARIMA (1,1,0)
dugaan4=arima(DataCovid_trans, order=c(1,1,0))
summary(dugaan4)

#Model 5 ARIMA (1,1,1)
dugaan5=arima(DataCovid_trans, order=c(1,1,1))
summary(dugaan5)

#Model 6 ARIMA (1,1,2)
dugaan6=arima(DataCovid_trans, order=c(1,1,2))
summary(dugaan6)

#Model 7 ARIMA (1,1,3)
dugaan7=arima(DataCovid_trans, order=c(1,1,3))
summary(dugaan7)

#Model 8 ARIMA (2,1,0)
dugaan8=arima(DataCovid_trans, order=c(2,1,0))
summary(dugaan8)

#Model 9 ARIMA (2,1,1)
dugaan9=arima(DataCovid_trans, order=c(2,1,1))
summary(dugaan9)

#Model 10 ARIMA (2,1,2)
dugaan10=arima(DataCovid_trans, order=c(2,1,2))
summary(dugaan10)

#Model 11 ARIMA (2,1,3)
dugaan11=arima(DataCovid_trans, order=c(2,1,3))
summary(dugaan11)




#3. Diagnostic Checking Model dan Pemilihan Model
library(lmtest)


#residualdataAA=resid(dataAA)
#residualdataAA

residual_1=resid(dugaan1)
residual_1

residual_2=resid(dugaan2)
residual_2

residual_3=resid(dugaan3)
residual_3

residual_4=resid(dugaan4)
residual_4

residual_5=resid(dugaan5)
residual_5


residual_6=resid(dugaan6)
residual_6

residual_7=resid(dugaan7)
residual_7


residual_8=resid(dugaan8)
residual_8

residual_9=resid(dugaan9)
residual_9

residual_10=resid(dugaan10)
residual_10

residual_11=resid(dugaan11)
residual_11

#hasil uji white noise 
#Box.test(residualdataAA,type = "Ljung")

#Model 1 ARIMA (0,1,1)
Box.test(residual_1,type="Ljung")

#Model 2 ARIMA (0,1,2)
Box.test(residual_2,type="Ljung")

#Model 3 ARIMA (0,1,3)
Box.test(residual_3, type="Ljung")

#Model 4 ARIMA (1,1,0)
Box.test(residual_4,type="Ljung")

#Model 5 ARIMA (1,1,1)
Box.test(residual_5,type="Ljung")

#Model 6 ARIMA (1,1,2)
Box.test(residual_6,type="Ljung")

#Model 7 ARIMA (1,1,3)
Box.test(residual_7, type="Ljung")

#Model 8 ARIMA (2,1,0)
Box.test(residual_8,type="Ljung")

#Model 9 ARIMA (2,1,1)
Box.test(residual_9,type="Ljung")

#Model 10 ARIMA (2,1,2)
Box.test(residual_10,type="Ljung")

#Model 11 ARIMA (2,1,3)
Box.test(residual_11,type="Ljung")



#Box.test(residualku,type="Ljung")
#hasil uji normalitas terhdap nilai residual

#ks.test(residualdataAA,"pnorm", mean(residualdataAA),sd(residualdataAA))

#Model 1 ARIMA (0,1,1)
ks.test(residual_1,"pnorm", mean(residual_1),sd(residual_1))

#Model 2 ARIMA (0,1,2)
ks.test(residual_2,"pnorm", mean(residual_2),sd(residual_2))

#Model 3 ARIMA (0,1,3)
ks.test(residual_3,"pnorm", mean(residual_3),sd(residual_3))

#Model 4 ARIMA (1,1,0)
ks.test(residual_4,"pnorm", mean(residual_4),sd(residual_4))

#Model 5 ARIMA (1,1,1)
ks.test(residual_5,"pnorm", mean(residual_5),sd(residual_5))

#Model 6 ARIMA (1,1,2)
ks.test(residual_6,"pnorm", mean(residual_6),sd(residual_6))

#Model 7 ARIMA (1,1,3)
ks.test(residual_7,"pnorm", mean(residual_7),sd(residual_7))

#Model 8 ARIMA (2,1,0)
ks.test(residual_8,"pnorm", mean(residual_8),sd(residual_8))

#Model 9 ARIMA (2,1,1)
ks.test(residual_9,"pnorm", mean(residual_9),sd(residual_9))

#Model 10 ARIMA (2,1,2)
ks.test(residual_10,"pnorm", mean(residual_10),sd(residual_10))

#Model 11 ARIMA (2,1,3)
ks.test(residual_11,"pnorm", mean(residual_11),sd(residual_11))



#Hasil diagnostic checking terhdap nilai residual 

acfStat <- function(DataCovid_trans,lag=36)
{
  out1=acf(DataCovid_trans,lag.max = lag,plot = F)
  acfout=out1$acf
  out2=pacf(DataCovid_trans,lag.max = lag,plot = F)
  pacfout=NULL
  pacfout[1]=1
  pacfout=c(pacfout,out2$acf)
  temp1=NULL
  temp1[1]=NULL
  temp2=NULL
  temp2[1]=NULL
  for (i in 1:lag)
  {
    temp1[i+1]=Box.test(DataCovid_trans,lag=i, type="Ljung")$statistic
    temp2[i+1]=Box.test(DataCovid_trans,lag=i, type="Ljung")$p.value
  }
  result=cbind(ACF=acfout,PACF=pacfout,"Q-Stats"=temp1, "P-Value" = temp2)
}


hasil_1=acfStat(dugaan1$residuals)
hasil_1

hasil_2=acfStat(dugaan2$residuals)
hasil_2

hasil_3=acfStat(dugaan3$residuals)
hasil_3

hasil_4=acfStat(dugaan4$residuals)
hasil_4

hasil_5=acfStat(dugaan5$residuals)
hasil_5

hasil_6=acfStat(dugaan6$residuals)
hasil_6

hasil_7=acfStat(dugaan7$residuals)
hasil_7

hasil_8=acfStat(dugaan8$residuals)
hasil_8

hasil_9=acfStat(dugaan9$residuals)
hasil_9

hasil_10=acfStat(dugaan10$residuals)
hasil_10

hasil_11=acfStat(dugaan11$residuals)
hasil_11

#tsdiag(dataAA)
tsdiag(dugaan1)
tsdiag(dugaan2)
tsdiag(dugaan3)
tsdiag(dugaan4)
tsdiag(dugaan5)
tsdiag(dugaan6)
tsdiag(dugaan7)
tsdiag(dugaan8)
tsdiag(dugaan9)
tsdiag(dugaan10)
tsdiag(dugaan11)


#4. Peramalan 
#peramalan dengan model ARIMA model 9
fcast=forecast(dugaan10,h=12)
fcast

#membuat plot grafik pada 
library(forecast)
library(TSA)
plot.forecast(fcast)


#4. Peramalan 
Prediksi=forecast(dugaan10,h=12)
Prediksi
plot(Prediksi)


Prediksi1=forecast(DataCovid)
plot(Prediksi1)
Prediksi1

x1 = Prediksi1
plot(Prediksi, type = "l", xlim=c(0,210),ylim = c(0,120), xlab = "t", ylab = "xt", main = Plot Grafik Data dan Hasil Peramalan)
plot(Prediksi1, type = "l" , xlim=c(0,210), ylim = c(0,120))

??plot.forecast
plot.ts(Prediksi, xlab="Minggu ke-", ylab="Xt*")

plot(x1,y1,col="black")

??par(new=T)

forecasting <- forecast(DataCovid,model=dugaan10,h=12)
forecasting
