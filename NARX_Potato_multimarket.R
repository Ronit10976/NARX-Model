data=read.table(file="clipboard",sep="\t", header=TRUE)
data
Delhi=ts(log(data$Delhi),frequency = 12, start = c(2010, 1))
Kolkata=ts(log(data$Kolkata),frequency = 12, start = c(2010, 1))
Mumbai=ts(log(data$Mumbai),frequency = 12, start = c(2010, 1))
Chennai=ts(log(data$Chennai),frequency = 12, start = c(2010, 1))
Banglore=ts(log(data$Bengaluru),frequency = 12, start = c(2010, 1))
Guwahati=ts(log(data$Guwahati),frequency = 12, start = c(2010, 1))

x=ccf(Delhi,Mumbai)
x
# y0=Delhi[1:115] #Original rescaled Training dataset
# z0=Delhi[116:129]# Original rescaled testing dataset

y0=Guwahati[1:115] #Original rescaled Training dataset
z0=Guwahati[116:129]# Original rescaled testing dataset

y1=Mumbai[1:115] #Original rescaled Training dataset
z1=Mumbai[116:129]# Original rescaled testing dataset

y2=Kolkata[1:115] #Original rescaled Training dataset
z2=Kolkata[116:129]# Original rescaled testing dataset

y3=Banglore[1:115] #Original rescaled Training dataset
z3=Banglore[116:129]# Original rescaled testing dataset


net=nnetar(y0)#xreg=ts(y1)
predict <- forecast(net,h=12)#xreg=ts(z1),
a8=data.frame(predict)
b8=a8[,1]

fit3=ts.lstm(ts=Guwahati,  tsLag=4, LSTMUnits=6, DropoutRate = 0, 
             Epochs = 15, CompLoss = "mse", CompMetrics = "mae", 
             ActivationFn = "tanh", SplitRatio = 0.90, ValidationSplit = 0.1)


rmse_NARX=sqrt(mean((z0-b8)^2))
rmse_NARX
mape_NARX=(mean((abs(z0-b8))/z0))*100
mape_NARX
# mad_NARX=(mean(abs(z1-b8)))
# mad_NARX



xt<-read.table(file = "clipboard", sep = "\t", header = TRUE)

a=xt$Actual
f1=xt$ARIMA
f2=xt$ARIMAX.Mum
f3=xt$ARIMAX.Kol
f4=xt$ARIMAX.Ban
f5=xt$TDNN
f6=xt$NARX.Mum
f7=xt$NARX.Kol
f8=xt$NARX.Ban


e1=a-f1
e2=a-f2
e3=a-f3
e4=a-f4
e5=a-f5
e6=a-f6
e7=a-f7
e8=a-f8
dm.test(e8,e7,alternative = "greater",h = 1,power = 2)
#DM.test(f1,f2,y,loss.type="SE",h=1,c=FALSE,H1="more")



