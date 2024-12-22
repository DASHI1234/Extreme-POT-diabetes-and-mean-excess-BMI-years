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
library(evir)

# 读取Excel数据的多个sheet
data1 <- read_excel("/Users/shidongping/Desktop/极值分析/【002】Cumulative BMI-years calculation of reference and grouping for standard for 1.xlsx", sheet = 1)
data2 <- read_excel("/Users/shidongping/Desktop/极值分析/【002】Cumulative BMI-years calculation of reference and grouping for standard for 2.xlsx", sheet = 1)
data3 <- read_excel("/Users/shidongping/Desktop/极值分析/【002】Cumulative BMI-years calculation of reference and grouping for standard for 2.xlsx", sheet = 2)

# 选择特定的sheet进行操作（例如data1）
data <- data3
# 对同一个new_new_ID，仅保留year列最大的数据
data <- data %>%
  group_by(new_ID) %>%
  slice_max(year) %>%
  ungroup()
new_ID_count <- data %>% 
  summarise(unique_new_ID = n_distinct(new_ID))
# 打印结果
print(new_ID_count)
# 将 FPG 列强制转换为数值型（如果存在非数值数据会变为 NA）
data$FPG <- as.numeric(data$FPG)
# FPG 和 BMI 分析
FPG <- as.vector(data$FPG)
Cumulative_BMI_years <- as.vector(data$mean_Cumulative_BMI_years)
basicStats(FPG)
hist(FPG)
basicStats(Cumulative_BMI_years)
hist(Cumulative_BMI_years)
plot(Cumulative_BMI_years, FPG, panel.first = grid(10, 10))

# 单变量过程找到 FPG 的阈值
mrlplot(FPG, xlab = "Threshold u (FPG)")

# 添加 x = 7 的竖线
abline(v = 7, col = "red", lty = 2)

# 获取图形窗口的尺寸，计算标签的 y 位置
par_values <- par()
plot_height <- par_values$usr[4] - par_values$usr[3]  # 图形的高度
text_y_position <- par_values$usr[3] + 0.9 * plot_height  # 假设标签位于图形的 90% 高度处

# 添加文本标签在竖线的右侧
text(x = 7, y = text_y_position, labels = "u=7", pos = 4, col = "red", xpd = TRUE, adj = 0)

jpeg("Annualized_old_FPG-mean-residual-life-plot.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 500)
# 单变量过程找到 FPG 的阈值
mrlplot(FPG, xlab = "Threshold u (FPG)")

# 添加 x = 7 的竖线
abline(v = 7, col = "red", lty = 2)

# 获取图形窗口的尺寸，计算标签的 y 位置
par_values <- par()
plot_height <- par_values$usr[4] - par_values$usr[3]  # 图形的高度
text_y_position <- par_values$usr[3] + 0.9 * plot_height  # 假设标签位于图形的 90% 高度处

# 添加文本标签在竖线的右侧
text(x = 7, y = text_y_position, labels = "u=7", pos = 4, col = "red", xpd = TRUE, adj = 0)

dev.off()  # 关闭JPEG设备
#mePlot(FPG, doplot = TRUE)
runTheta(FPG, quantiles = seq(0.75, 0.95, length = 5), title = "PFG的阈值")
#threshold需要调整
u_fpg = 7

# 对 FPG 进行 GPD 拟合
gpdfit_fpg <- gpd.fit(FPG, threshold = u_fpg)
fevd(FPG, threshold = u_fpg, type = "GP")
gpd.diag(gpdfit_fpg)
# 保存 QQ 图到jpeg文件
jpeg("Annualized_old_FPG-QQ-plot.jpeg", units = "in", width = 7, height = 6, res = 500)
gpd.diag(gpdfit_fpg)  
dev.off()  # 关闭设备

#cat("Scale Parameter (σ):", gpdfit_fpg$mle[1], "Standard Error:", gpdfit_fpg$se[1], "\n")
#cat("Shape Parameter (ξ):", gpdfit_fpg$mle[2], "Standard Error:", gpdfit_fpg$se[2], "\n")

# 单变量过程找到 mean_ref_Cumulative_BMI_years 的阈值
mrlplot(Cumulative_BMI_years, xlab = "Threshold u (CEBR)")

# 添加 x = 6.78 的竖线
abline(v = 2.18, col = "red", lty = 2)

# 获取图形窗口的尺寸，计算标签的 y 位置
par_values <- par()
plot_height <- par_values$usr[4] - par_values$usr[3]  # 图形的高度
text_y_position <- par_values$usr[3] + 0.9 * plot_height  # 假设标签位于图形的 90% 高度处

# 添加文本标签在竖线的右侧
text(x = 2.18, y = text_y_position, labels = "u=2.18", pos = 4, col = "red", xpd = TRUE, adj = 0)

# 保存图像到jpeg文件
jpeg("Annualized_CEBR-old_BMI-mean-residual-life-plot.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 500)
# 单变量过程找到 mean_ref_Cumulative_BMI_years 的阈值
mrlplot(Cumulative_BMI_years, xlab = "Threshold u (CEBR)")

# 添加 x = 6.78 的竖线
abline(v = 2.18, col = "red", lty = 2)

# 获取图形窗口的尺寸，计算标签的 y 位置
par_values <- par()
plot_height <- par_values$usr[4] - par_values$usr[3]  # 图形的高度
text_y_position <- par_values$usr[3] + 0.9 * plot_height  # 假设标签位于图形的 90% 高度处

# 添加文本标签在竖线的右侧
text(x = 2.18, y = text_y_position, labels = "u=2.18", pos = 4, col = "red", xpd = TRUE, adj = 0)

dev.off()  # 关闭JPEG设备

#mePlot(mean_ref_Cumulative_BMI_years, doplot = TRUE)
runTheta(Cumulative_BMI_years, quantiles = seq(0.70, 0.90, length = 5), title = "BMI的阈值")
u_cu_bmi =  2.18

# 对 BMI 进行 GPD 拟合
gpdfit_cu_bmi <- gpd.fit(Cumulative_BMI_years, threshold = u_cu_bmi)
fevd(Cumulative_BMI_years, threshold = u_cu_bmi, type = "GP")
gpd.diag(gpdfit_cu_bmi)
jpeg("Annualized_old_BMI-QQ-plot.jpeg", units = "in", width = 7, height = 6, res = 500)
gpd.diag(gpdfit_cu_bmi) 
dev.off()  # 关闭设备
# 打印出尺度和形状参数及其标准误
#cat("Scale Parameter (σ):", gpdfit_cu_bmi$mle[1], "Standard Error:", gpdfit_cu_bmi$se[1], "\n")
#cat("Shape Parameter (ξ):", gpdfit_cu_bmi$mle[2], "Standard Error:", gpdfit_cu_bmi$se[2], "\n")

# 双变量分析
# 设定双变量阈值
u_biv <- c(u_cu_bmi, u_fpg)

# 组合 FPG 和 BMI 数据，作为双变量分析的数据
data_biv <- cbind(Cumulative_BMI_years, FPG)
# 使用 fbvpot 进行双变量广义帕累托分布拟合
fit <- fbvpot(data_biv, u_biv, model = "log")
# 打印拟合结果
print(fit)
# 绘制结果图
#plot(fit)
# 计算chi
a =0.97823  
chi <- 2 - 2^a
chi

# 假设fit是之前拟合的模型，这里需要确保fit是正确的模型对象
# 计算参数a的95%置信区间
ci_a <- confint(fit, "dep")

# 打印参数a的95%置信区间
print(ci_a)

# 根据参数a的置信区间计算chi的95%置信区间
# 假设ci_a的下限是a的最小值，上限是a的最大值
chi_lower <- 2 - 2^(ci_a[1])
chi_upper <- 2 - 2^(ci_a[2])

# 打印chi的95%置信区间
cat("95% CI for chi:", chi_lower, "to", chi_upper, "\n")

# 绘制结果图
plot(fit)
