# 清除工作环境
rm(list = ls())

# 加载必要的库
library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(tseries)
library(extRemes)
library(timeDate)
library(timeSeries)
library(fBasics)
library(fGarch)
library(fExtremes)
library(ismev)
library(evd)
library(mgcv)
library(nlme)
library(dplyr)
# 读取Excel文件
data <- read_excel("/Users/shidongping/Desktop/极值分析/【05】Calculation of mean excess BMI-years.xlsx", sheet = 1)
# 对同一个new_new_ID，仅保留year列最小的数据
data <- data %>%
  group_by(new_new_ID) %>%
  slice_min(year) %>%
  ungroup()

# 转换new_age为数值型
data$new_age <- as.numeric(data$new_age)

# 计算new_age的均值和标准差
age_mean <- mean(data$new_age, na.rm = TRUE)
age_sd <- sd(data$new_age, na.rm = TRUE)

# 打印age的均值和标准差
cat("Mean of age:", age_mean, "\n")
cat("Standard deviation of age:", age_sd, "\n")

# 计算gender的男女比例
gender_table <- table(data$gender)
gender_ratio <- prop.table(gender_table)

# 打印男女比例
cat("Gender ratio:\n")
print(gender_ratio)

# 读取Excel文件
data <- read_excel("/Users/shidongping/Desktop/极值分析/【05】Calculation of mean excess BMI-years.xlsx", sheet = 1)
# 对同一个new_new_ID，仅保留year列最大的数据
data <- data %>%
  group_by(new_new_ID) %>%
  slice_max(year) %>%
  ungroup()
# 将 FPG 列强制转换为数值型（如果存在非数值数据会变为 NA）
data$FPG <- as.numeric(data$FPG)
# 创建每年总人数和 FPG >= 7.0 人数的汇总表
summary_table <- data %>%
  group_by(year) %>%
  summarise(
    total_count = n(),  # 每年总人数
    FPG_ge7_count = sum(FPG >= 7.0)  # 每年 FPG >= 7.0 的人数
  )
# 计算 unique new_new_ID 的个数
new_new_ID_count <- data %>% 
  summarise(unique_new_new_ID = n_distinct(new_new_ID))

# 打印结果
print(new_new_ID_count)
# 打印结果以便查看
print(summary_table)
# FPG 和 BMI 分析
FPG <- as.vector(data$FPG)
mean_ref_Cumulative_BMI_years <- as.vector(data$mean_ref_Cumulative_BMI_years)
basicStats(FPG)
hist(FPG)
basicStats(mean_ref_Cumulative_BMI_years)
hist(mean_ref_Cumulative_BMI_years)
plot(mean_ref_Cumulative_BMI_years, FPG, panel.first = grid(10, 10))

# 单变量过程找到 FPG 的阈值
mrlplot(FPG, xlab = "阈值 u (FPG)")
mePlot(FPG, doplot = TRUE)
runTheta(FPG, quantiles = seq(0.75, 0.95, length = 5), title = "PFG的阈值")
#threshold需要调整
u_fpg = 7

# 对 FPG 进行 GPD 拟合
gpdfit_fpg <- gpd.fit(FPG, threshold = u_fpg)
fevd(FPG, threshold = u_fpg, type = "GP")
gpd.diag(gpdfit_fpg)

cat("Scale Parameter (σ):", gpdfit_fpg$mle[1], "Standard Error:", gpdfit_fpg$se[1], "\n")
cat("Shape Parameter (ξ):", gpdfit_fpg$mle[2], "Standard Error:", gpdfit_fpg$se[2], "\n")


# 单变量过程找到mean_ref_Cumulative_BMI_years的阈值
mrlplot(mean_ref_Cumulative_BMI_years, xlab = "阈值 u (BMI)")
mePlot(mean_ref_Cumulative_BMI_years, doplot = TRUE)
runTheta(mean_ref_Cumulative_BMI_years, quantiles = seq(0.75, 0.95, length = 5), title = "BMI的阈值")
u_cu_bmi = 8.48

# 对 BMI 进行 GPD 拟合
gpdfit_cu_bmi <- gpd.fit(mean_ref_Cumulative_BMI_years, threshold = u_cu_bmi)
fevd(mean_ref_Cumulative_BMI_years, threshold = u_cu_bmi, type = "GP")
gpd.diag(gpdfit_cu_bmi)

# 打印出尺度和形状参数及其标准误
cat("Scale Parameter (σ):", gpdfit_cu_bmi$mle[1], "Standard Error:", gpdfit_cu_bmi$se[1], "\n")
cat("Shape Parameter (ξ):", gpdfit_cu_bmi$mle[2], "Standard Error:", gpdfit_cu_bmi$se[2], "\n")

# 双变量分析
# 设定双变量阈值
u_biv <- c(u_cu_bmi, u_fpg)

# 组合 FPG 和 BMI 数据，作为双变量分析的数据
data_biv <- cbind(mean_ref_Cumulative_BMI_years, FPG)
# 使用 fbvpot 进行双变量广义帕累托分布拟合
fit <- fbvpot(data_biv, u_biv, model = "log")
# 打印拟合结果
print(fit)

# 绘制结果图
plot(fit)
fit <- fbvpot(data_biv, u_biv, model = "bilog")
# 打印拟合结果
print(fit)

# 绘制结果图
plot(fit)

fit <- fbvpot(data_biv, u_biv, model = "alog")
# 打印拟合结果
print(fit)

# 绘制结果图
plot(fit)

fit <- fbvpot(data_biv, u_biv, model = "neglog")
# 打印拟合结果
print(fit)

# 绘制结果图
plot(fit)

fit <- fbvpot(data_biv, u_biv, model = "negbilog")
# 打印拟合结果
print(fit)

# 绘制结果图
plot(fit)

fit <- fbvpot(data_biv, u_biv, model = "aneglog")
# 打印拟合结果
print(fit)

# 绘制结果图
plot(fit)
fit <- fbvpot(data_biv, u_biv, model = "ct")
# 打印拟合结果
print(fit)

# 绘制结果图
plot(fit)

fit <- fbvpot(data_biv, u_biv, model = "hr")
# 打印拟合结果
print(fit)

# 绘制结果图
plot(fit)

fit <- fbvpot(data_biv, u_biv, model = "amix")

#fit <- fbvpot(data_biv, u_biv, std.err = FALSE)

# 打印拟合结果
print(fit)

# 绘制结果图
plot(fit)

chiplot(data_biv)

