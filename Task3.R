#librerie
library("FactoMineR")
library("factoextra")

#lettura dei dati flows.csv
data<- read.csv('flows/flows1.csv')

data.pca <- prcomp(data, scale=TRUE)
data.pca
#biplot
fviz_pca_var(data.pca, col.var = "black",
             title="Biplot variables - scale = TRUE")+

  theme(plot.title = element_text(hjust = 0.5), 
        aspect.ratio = 4/4,) +
  expand_limits(x=c(-1.12,1.12), y=c(-1.12,1.12))
       # coord_cartesian(xlim = c(-1.1,1.1))        


# 1  
# VARIABILI CORRELATE: Bwd.Packet.Length.Std - Average.Packet.Size
x<-data$Bwd.Packet.Length.Std
y<-data$Average.Packet.Size
plot(x, y, pch=20, xlab = ("Bwd.Packet.Length.Std"), ylab=("Average.Packet.Size"),
     main=("Linear fitting: 95% regression confidence and prediction intervals"))

# 2
# VARIABILI CORRELATE: Total.Length.of.Bwd.Packet - Total.Fwd.Packet
x<-data$Total.Length.of.Bwd.Packet
y<-data$Total.Fwd.Packet
plot(x, y, pch=20, xlab = ("Total.Length.of.Bwd.Packet"), ylab=("Total.Fwd.Packet"),
     main=("Linear fitting: 95% regression confidence and prediction intervals"))

# 3
# VARIABILI CORRELATE: Fwd.IAT.Std - Flow.Duration
x<-data$Fwd.IAT.Std
y<-data$Flow.Duration
plot(x, y, pch=20, xlab = ("Fwd.IAT.Std"), ylab=("Flow.Duration"),
     main=("Linear fitting: 95% regression confidence and prediction intervals"))    

# 4
# VARIABILI CORRELATE: Total.TCP.Flow.Time  - Fwd.IAT.Std
x<-data$Total.TCP.Flow.Time 
y<-data$Fwd.IAT.Std 
plot(x, y, pch=20, xlab = ("Total.TCP.Flow.Time "), ylab=("Fwd.IAT.Std"),
     log="xy",
     main=("Linear fitting: 95% regression confidence and prediction intervals"))

# 5
# VARIABILI CORRELATE: Total.TCP.Flow.Time  - Flow.Duration
x<-data$Total.TCP.Flow.Time 
y<-data$Flow.Duration 
plot(x, y, pch=20, xlab = ("Total.TCP.Flow.Time "), ylab=("Flow.Duration"),
     log="xy",
      main=("Linear fitting: 95% regression confidence and prediction intervals"))

# 6
# VARIABILI ANTICORRELATE: Bwd.Packet.Length.Std - Total.TCP.Flow.Time
x<-data$Bwd.Packet.Length.Std 
y<-data$Total.TCP.Flow.Time 
plot(x, y, pch=20, xlab = ("Bwd.Packet.Length.Std "), ylab=("Total.TCP.Flow.Time"),
     ylim=c(-1e10,3e10),
     main=("Linear fitting: 95% regression confidence and prediction intervals"))

# 7
# VARIABILI ANTICORRELATE: Average.Packet.Size - Total.TCP.Flow.Time
x<-data$Average.Packet.Size 
y<-data$Total.TCP.Flow.Time 
plot(x, y, pch=20, xlab = ("Average.Packet.Size "), ylab=("Total.TCP.Flow.Time"),
     ylim=c(-1e10,3e10),
     main=("Linear fitting: 95% regression confidence and prediction intervals"))

# 8
# VARIABILI ANTICORRELATE: Fwd.IAT.Std - Flow.Bytes.s
x<-data$Fwd.IAT.Std 
y<-data$Flow.Bytes.s 
plot(x, y, pch=20, xlab = ("Fwd.IAT.Std "), ylab=("Flow.Bytes.s"),
     ylim=c(-1e7,1.5e8),
     main=("Linear fitting: 95% regression confidence and prediction intervals"))

# 9
# VARIABILI ANTICORRELATE: Flow.Duration - Flow.Bytes.s
x<-data$Flow.Duration 
y<-data$Flow.Bytes.s 
plot(x, y, pch=20, xlab = ("Flow.Duration "), ylab=("Flow.Bytes.s"),
     ylim=c(-1e7,1.5e8),
     main=("Linear fitting: 95% regression confidence and prediction intervals"))

# 10
# VARIABILI ANTICORRELATE: Total.Fwd.Packet  - Flow.Bytes.s
x<-data$Total.Fwd.Packet  
y<-data$Flow.Bytes.s 
plot(x, y, pch=20, xlab = ("Total.Fwd.Packet  "), ylab=("Flow.Bytes.s"),
     #ylim=c(-1e7,1.5e8),
     log="xy",
     main=("Linear fitting: 95% regression confidence and prediction intervals"))

l_mod<-lm(y~x)

lines(x,predict(l_mod), col="red", lwd=2)

# Coefficiente di determinazione
R2<-summary(l_mod)$r.squared
R2

c_int<- predict(l_mod, level = 0.95, interval="confidence")
p_int<- predict(l_mod, level = 0.95, interval="prediction")


#plot(x, y, pch=20, xlab = ("Bwd.Packet.Length.Std"), ylab=("Average.Packet.Size"))
lines(x,predict(l_mod), col="red", lwd=2)
lines(x, c_int[,2], type="o", lty=2, col="blue")
lines(x, c_int[,3], type="o", lty=2, col="blue")
lines(x, p_int[,2], type="o", lty=2, col="green")
lines(x, p_int[,3], type="o", lty=2, col="green")
legend( x="right", 
        legend=c("Linear fitting", "Confidence intervals", "Prediction intervals"),
        col=c("red","blue", "green"), lwd=1, cex=0.7)
