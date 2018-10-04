> library("urca")
> library(readxl)
> library(pwt8)
> data("pwt8.0")
> View(pwt8.0)
> br <- subset(pwt8.0, country=="Brazil", 
               +              select = c("rgdpna","emp","xr"))
> colnames(br) <-  c("PIB","Emprego","Câmbio")
> PIB <- br$PIB[45:62]
> #Separando Variáveis
  > PIB <- br$PIB[45:62]
  > EMPREGO <- br$Emprego[45:62]
  > CAMBIO <- br$Câmbio[45:62]
  > Anos <- seq(from=1994, to=2011, by=1)
  > Anos <- seq(from=1994, to=2011, by=1)
  > #Análise para o emprego
    > plot(EMPREGO, type = "l")
  > emprego <- ts(EMPREGO, start = 1994, frequency = 1)
  > plot(emprego, main="Pessoa Empregadas no Brasil", 
         +      ylab="Qte de Pessoas Empregadas-milhões", 
         +      xlab="Ano")
  > acf(emprego)
  > pacf(emprego)
  > reglinEMP <- lm(EMPREGO ~ Anos)
  > reglinEMP
  
  Call:
    lm(formula = EMPREGO ~ Anos)
  
  Coefficients:
    (Intercept)         Anos  
  -3736.592        1.908  
  
  > summary(reglinEMP)
  
  Call:
    lm(formula = EMPREGO ~ Anos)
  
  Residuals:
    Min      1Q  Median      3Q     Max 
  -3.3808 -1.3820  0.0899  1.0826  4.6436 
  
  Coefficients:
    Estimate Std. Error t value Pr(>|t|)    
  (Intercept) -3.737e+03  1.958e+02  -19.08 1.97e-12 ***
    Anos         1.908e+00  9.778e-02   19.51 1.40e-12 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
  Residual standard error: 2.152 on 16 degrees of freedom
  Multiple R-squared:  0.9597,	Adjusted R-squared:  0.9572 
  F-statistic: 380.7 on 1 and 16 DF,  p-value: 1.399e-12
  
  > plot(emprego)
  > abline(reglinEMP, col="Blue")
  > plot(CAMBIO)
  > plot(emprego)
  > plot(PIB,type="l",col="blue")
  > plot(emprego,type="l",col="red")
  > plot(CAMBIO,type="l",col="green")
  > acf(emprego)
  > pacf(emprego)
  > residuosEMP<-reglinEMP$residuals
  > reglinEMP<-lm(residuosEMP~Anos)
  > plot(residuosEMP,type="l")
  > abline(reglinEMP,col="blue")
  > pdemprego <- diff(emprego)
  > diferenca1 <- (data.frame(emprego[2:18],pdemprego))
  > DIFERENCA<- ts(diferenca1, start=1994, frequency=1)
  > plot(DIFERENCA,plot.type="single",col=c("black","green"))
  > PIB <- br$PIB[45:62]
  > EMPREGO <- br$Emprego[45:62]
  > CAMBIO <- br$ CAMBIO [45:62]
  > Anos <- seq(from=1994, to=2011, by=1)
  > # estimando a série temporal - certo
    > arima123 <- arima(emprego, c(1,2,3))
  > arima120 <- arima(emprego, c(1,2,0))
  > arima121 <- arima(emprego, c(1,2,1))
  > arima122 <- arima(emprego, c(1,2,2))
  > arima122 <- arima(emprego, c(2,2,0))
  > arima122 <- arima(emprego, c(2,2,1))
  > arima122 <- arima(emprego, c(2,2,2))
  > arima122 <- arima(emprego, c(2,2,3))
  > arima122 <- arima(emprego, c(2,2,3))
  > arima122 <- arima(emprego, c(0,2,1))
  > arima122 <- arima(emprego, c(0,2,2))
  > arima122 <- arima(emprego, c(0,2,3))
  > arima122 <- arima(emprego, c(1,2,0))