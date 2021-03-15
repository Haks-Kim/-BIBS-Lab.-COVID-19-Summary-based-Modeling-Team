#rm(list=ls())
library(dplyr)
library(PerformanceAnalytics)

#### Importing Data #### 
# Importing daily confirmed cases data
# Path
path <- "C:/Users/김학용/Desktop/코로나 연구 인턴/주차별 작업사항/최종 정리(8.31)/"
data_path <- paste0(path, "data/")
result_path <- paste0(path, "result/")
plot_path <- paste0(path, "plot/SLR/")

coef_Logi <- read.csv("result/segmented_Logistic_result.csv")[1:7]
colnames(coef_Logi) <- c("Country", 
                         "α1(Logistic)", "β1(Logistic)", "γ1(Logistic)",
                         "α2(Logistic)", "β2(Logistic)", "γ2(Logistic)")
coef_Gomp <- read.csv("result/segmented_Gompertz_result.csv")[1:7]
colnames(coef_Gomp) <- c("Country", 
                         "α1(Gompertz)", "β1(Gompertz)", "γ1(Gompertz)",
                         "α2(Gompertz)", "β2(Gompertz)", "γ2(Gompertz)")
coef_total <- cbind(coef_Logi, coef_Gomp)[,-8]

#### Correlation Analysis ####
# correlation coefficients of Logistic Models
jpeg(filename=paste0("plot/etc/Correlation_Logistic.jpeg"), width = 8, height = 8, units = 'in',res=600)
chart.Correlation(log(coef_Logi[, c(2:7)]), histogram = TRUE, pch=19)
dev.off()

# correlation coefficients of Gompertz Models
jpeg(filename=paste0("plot/etc/Correlation_Gompertz.jpeg"), width = 8, height = 8, units = 'in',res=600)
chart.Correlation(log(coef_Gomp[, c(2:7)]), histogram = TRUE, pch=19)
dev.off()

# correlation coefficients of both models in the same segment.
jpeg(filename=paste0("plot/etc/Correlation_Logistic vs Gompertz(segment1).jpeg"), width = 8, height = 8, units = 'in',res=600)
chart.Correlation(log(coef_total[,c(2:4, 8:10)]), histogram = TRUE, pch=19)
dev.off()
jpeg(filename=paste0("plot/etc/Correlation_Logistic vs Gompertz(segment2).jpeg"), width = 8, height = 8, units = 'in',res=600)
chart.Correlation(log(coef_total[,c(5:7, 11:13)]), histogram = TRUE, pch=19)
dev.off()
