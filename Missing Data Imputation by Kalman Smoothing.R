rm(list=ls())  # remove variables
graphics.off() # close figures

library(fpp2)   #Untuk loading data austorists
library(readxl)  # Load readxl package
library(stats)

#Imputasi data hilang dengan data contoh
x <- austourists
miss <- sample(1:length(x), 5)
x[miss] <- NA
fit <- StructTS(x, type = "BSM")
sm <- tsSmooth(fit)
estim <- sm[,1]+sm[,2]+sm[,3]   #merekonstruksi hasil smooting dari unsur level, tren dan musiman

#Plot hasil estimasi data hilang
plot(x, ylim=range(austourists))
points(time(x)[miss], estim[miss], col="red", pch=1)
points(time(x)[miss], austourists[miss], col="blue", pch=1)
legend("topleft", pch=1, col=c(2,4), legend=c("Estimate","Actual"))

#Imputasi data hilang pada data riil (Volume Ekspor Luar Negeri HS 2 Digit 15 (Lemak & Minyak Hewan))
rm(list=ls())  # remove variables
graphics.off() # close figures

# Membaca data dari file Excel
IBS <- read_excel("E:/@Yoga Sasmita - ITS/Disertasi/Pengolahan/ARIMAX/ForecastIBS.xlsx")
data <- IBS[1:57, 11:13]  # Assumes columns 11-13 contain relevant data
data <- as.matrix(sapply(data, as.numeric))

# Create time series object
tsdata <- ts(data, start=c(2010, 1), frequency=4)
colnames(tsdata) <- c("IBS", "ELN", "EAW")

# Handling missing data using Kalman Smoothing for the ELN series
x <- tsdata[, "ELN"]
miss <- 27  # Index of the missing value
x[miss] <- NA  # Introduce a missing value

fit <- StructTS(x, type="BSM")
sm <- tsSmooth(fit)

# Hasil estimasi data hilang
estim <- rowSums(sm[, 1:3])
x[miss] <- estim[miss]  
ELNnew <- x

plot(x, ylim=range(tsdata[,"ELN"], na.rm=TRUE), ylab="ELN", xlab="Time")
points(time(x)[miss], estim[miss], col="red", pch=1)
legend("topleft", pch=1, col=c(2,4), legend=c("Estimate"))

# Create new data frame with the filled ELN series
datanew <- data.frame(tsdata[, c("IBS", "EAW")], ELNnew)
tsdatanew <- ts(datanew, start=c(2010, 1), frequency=4)
plot(tsdatanew[,"ELNnew"])
