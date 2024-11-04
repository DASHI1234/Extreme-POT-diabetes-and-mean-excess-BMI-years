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
#data <- read_excel("/Users/shidongping/Desktop/极值分析/【05】Calculation of mean excess BMI-years.xlsx", sheet = 1)
# 对同一个new_new_ID，仅保留year列最小的数据
# 读取Excel数据的多个sheet
data1 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 1)
data2 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 2)
data3 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 3)
data4 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 4)
# 选择特定的sheet进行操作（例如data1）
data <- data4
# 对同一个new_new_ID，仅保留year列最大的数据
data <- data %>%
  group_by(new_new_ID) %>%
  slice_max(year) %>%
  ungroup()
new_new_ID_count <- data %>% 
  summarise(unique_new_new_ID = n_distinct(new_new_ID))
# 打印结果
print(new_new_ID_count)
# 将 FPG 列强制转换为数值型（如果存在非数值数据会变为 NA）
data$FPG <- as.numeric(data$FPG)
# 创建每年总人数和 FPG >= 7.0 人数的汇总表
summary_table <- data %>%
  group_by(year) %>%
  summarise(
    total_count = n(),  # 每年总人数
    FPG_ge7_count = sum(FPG >= 7.0)  # 每年 FPG >= 7.0 的人数
  )
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
mrlplot(FPG, xlab = "Threshold u (FPG)")
# 添加 x = 7 的竖线
abline(v = 7, col = "red", lty = 2)
# 这里使用 par() 函数获取图形窗口的尺寸，并计算标签的 y 位置
par_values <- par()
plot_height <- par_values$usr[4] - par_values$usr[3]  # 图形的高度
text_y_position <- par_values$usr[3] + 0.9 * plot_height  # 假设标签位于图形的 90% 高度处
# 添加文本标签
text(x = 7, y = text_y_position, labels = "u=7", pos = 3, col = "red")

jpeg("FPG-mean-residual-life-plot-60+-update.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 500)
# 单变量过程找到 FPG 的阈值
mrlplot(FPG, xlab = "Threshold u (FPG)")
# 添加 x = 7 的竖线
abline(v = 7, col = "red", lty = 2)
# 这里使用 par() 函数获取图形窗口的尺寸，并计算标签的 y 位置
par_values <- par()
plot_height <- par_values$usr[4] - par_values$usr[3]  # 图形的高度
text_y_position <- par_values$usr[3] + 0.9 * plot_height  # 假设标签位于图形的 90% 高度处
# 添加文本标签
text(x = 7, y = text_y_position, labels = "u=7", pos = 3, col = "red")
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
jpeg("FPG-QQ-plot-total.jpeg", units = "in", width = 7, height = 6, res = 500)
gpd.diag(gpdfit_fpg)  
dev.off()  # 关闭设备

#cat("Scale Parameter (σ):", gpdfit_fpg$mle[1], "Standard Error:", gpdfit_fpg$se[1], "\n")
#cat("Shape Parameter (ξ):", gpdfit_fpg$mle[2], "Standard Error:", gpdfit_fpg$se[2], "\n")

# 单变量过程找到mean_ref_Cumulative_BMI_years的阈值
mrlplot(mean_ref_Cumulative_BMI_years, xlab = "Threshold u (Mean Excess BMI-years)")
abline(v = 6.78, col = "red", lty = 2)
# 这里使用 par() 函数获取图形窗口的尺寸，并计算标签的 y 位置
par_values <- par()
plot_height <- par_values$usr[4] - par_values$usr[3]  # 图形的高度
text_y_position <- par_values$usr[3] + 0.9 * plot_height  # 假设标签位于图形的 90% 高度处
# 添加文本标签
text(x = 7.5, y = text_y_position, labels = "u=6.78", pos = 3, col = "red")
# 保存图像到jpeg文件
jpeg("BMI-mean-residual-life-plot-30-44-update.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 500)
mrlplot(mean_ref_Cumulative_BMI_years, xlab = "Threshold u (Mean Excess BMI-years)")
abline(v = 6.78, col = "red", lty = 2)
# 这里使用 par() 函数获取图形窗口的尺寸，并计算标签的 y 位置
par_values <- par()
plot_height <- par_values$usr[4] - par_values$usr[3]  # 图形的高度
text_y_position <- par_values$usr[3] + 0.9 * plot_height  # 假设标签位于图形的 90% 高度处
# 添加文本标签
text(x = 8, y = text_y_position, labels = "u=6.78", pos = 3, col = "red")
dev.off()  # 关闭JPEG设备

#mePlot(mean_ref_Cumulative_BMI_years, doplot = TRUE)
runTheta(mean_ref_Cumulative_BMI_years, quantiles = seq(0.70, 0.90, length = 5), title = "BMI的阈值")
u_cu_bmi = 6.44

# 对 BMI 进行 GPD 拟合
gpdfit_cu_bmi <- gpd.fit(mean_ref_Cumulative_BMI_years, threshold = u_cu_bmi)
fevd(mean_ref_Cumulative_BMI_years, threshold = u_cu_bmi, type = "GP")
gpd.diag(gpdfit_cu_bmi)
jpeg("BMI-QQ0-plot-total.jpeg", units = "in", width = 7, height = 6, res = 500)
gpd.diag(gpdfit_cu_bmi) 
dev.off()  # 关闭设备
# 打印出尺度和形状参数及其标准误
#cat("Scale Parameter (σ):", gpdfit_cu_bmi$mle[1], "Standard Error:", gpdfit_cu_bmi$se[1], "\n")
#cat("Shape Parameter (ξ):", gpdfit_cu_bmi$mle[2], "Standard Error:", gpdfit_cu_bmi$se[2], "\n")

# 双变量分析
# 设定双变量阈值
u_biv <- c(u_cu_bmi, u_fpg)

# 组合 FPG 和 BMI 数据，作为双变量分析的数据
data_biv <- cbind(mean_ref_Cumulative_BMI_years, FPG)
# 使用 fbvpot 进行双变量广义帕累托分布拟合
fit <- fbvpot(data_biv, u_biv, model = "log")
# 打印拟合结果
print(fit)

# 计算chi
a = 0.97439   
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

# 计算 \overline{\chi}
#a = 0.9318   
#chi_bar <- 2 * a - 1
#chi_bar

# 绘制结果图
plot(fit)

# 加载必要的库
library(ggplot2)
library(dplyr)

# 创建数据集
data1 <- data.frame(
  Age_Group = c("18-29", "30-44", "45-59", "60+"),
  x = c(0.222, 0.116, 0.086, 0.035),
  CI_Lower = c(0.132, 0.088, 0.061, 0.004),
  CI_Upper = c(0.309, 0.143, 0.111, 0.066)
)

# 确保 Age_Group 是因子并按照顺序排列
data1 <- data1 %>%
  mutate(Age_Group = factor(Age_Group, levels = c("18-29", "30-44", "45-59", "60+")))

# 绘制图形
ggplot(data1, aes(x = Age_Group, y = x)) +
  geom_point(size = 3, colour = 'skyblue') +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, colour = 'skyblue') +
  labs(title = "", x = "Age Group", y = "x (95%CI)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  annotate("segment", x = 1, xend = 1, y = data1$CI_Lower[1], yend = data1$CI_Upper[1], linetype = "dashed", color = "red") +
  annotate("segment", x = 2, xend = 2, y = data1$CI_Lower[2], yend = data1$CI_Upper[2], linetype = "dashed", color = "red") +
  annotate("segment", x = 3, xend = 3, y = data1$CI_Lower[3], yend = data1$CI_Upper[3], linetype = "dashed", color = "red") +
  annotate("segment", x = 4, xend = 4, y = data1$CI_Lower[4], yend = data1$CI_Upper[4], linetype = "dashed", color = "red") +
  annotate("text", x = 1, y = data1$CI_Lower[1] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[1], data1$CI_Upper[1]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 2, y = data1$CI_Lower[2] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[2], data1$CI_Upper[2]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 3, y = data1$CI_Lower[3] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[3], data1$CI_Upper[3]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 4, y = data1$CI_Lower[4] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[4], data1$CI_Upper[4]), size = 3, hjust = 0.5, colour = "black")

jpeg("x_95CI_age_group.jpeg", units = "in", width = 7, height = 6, res = 500)
ggplot(data1, aes(x = Age_Group, y = x)) +
  geom_point(size = 3, colour = 'skyblue') +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, colour = 'skyblue') +
  labs(title = "", x = "Age Group", y = "x (95%CI)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  annotate("segment", x = 1, xend = 1, y = data1$CI_Lower[1], yend = data1$CI_Upper[1], linetype = "dashed", color = "red") +
  annotate("segment", x = 2, xend = 2, y = data1$CI_Lower[2], yend = data1$CI_Upper[2], linetype = "dashed", color = "red") +
  annotate("segment", x = 3, xend = 3, y = data1$CI_Lower[3], yend = data1$CI_Upper[3], linetype = "dashed", color = "red") +
  annotate("segment", x = 4, xend = 4, y = data1$CI_Lower[4], yend = data1$CI_Upper[4], linetype = "dashed", color = "red") +
  annotate("text", x = 1, y = data1$CI_Lower[1] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[1], data1$CI_Upper[1]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 2, y = data1$CI_Lower[2] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[2], data1$CI_Upper[2]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 3, y = data1$CI_Lower[3] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[3], data1$CI_Upper[3]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 4, y = data1$CI_Lower[4] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[4], data1$CI_Upper[4]), size = 3, hjust = 0.5, colour = "black")
dev.off()  # 关闭设备

