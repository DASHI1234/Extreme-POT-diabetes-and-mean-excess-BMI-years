# 清除工作环境
rm(list = ls())

# 加载必要的库
library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
# 读取数据
data <- read_excel("/Users/shidongping/Desktop/极值分析/【05】Calculation of mean excess BMI-years.xlsx", sheet = 1)
# 读取Excel数据的多个sheet
data1 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 1)
data2 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 2)
data3 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 3)
data4 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 4)
# 选择特定的sheet进行操作（例如data1）
data <- data1
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

# 清除工作环境
rm(list = ls())
# 创建数据框
data <- data.frame(
  age_group = rep(c("18-29", "30-44", "45-59", "60+"), times = 2),
  gender = rep(c("female", "male"), each = 4),
  count = c(2008, 6629, 4715, 1838, 2775, 9264, 6232, 2777)
)

# 创建列联表，将count列转换为频数表
contingency_table <- xtabs(count ~ age_group + gender, data = data)

# 查看列联表
print("列联表：")
print(contingency_table)

# 进行卡方检验
chi_square_test <- chisq.test(contingency_table)

# 检查检验的有效性
if (any(chi_square_test$expected < 5)) {
  warning("部分单元格的预期频数小于5，卡方检验结果可能不可靠。")
}

# 打印卡方检验结果
print("卡方检验结果：")
print(chi_square_test)

# 创建数据框
data <- data.frame(
  age_group = rep(c("18-29", "30-44", "45-59", "60+"), times = 2),
  diabetes_status = rep(c("Diabetes Incidence", "No Diabetes Incidence"), each = 4),
  count = c(18, 286, 470, 395, 4765, 15607, 10477, 4220)
)

# 根据 count 列构建列联表
contingency_table <- xtabs(count ~ age_group + diabetes_status, data = data)

# 查看列联表
print("列联表：")
print(contingency_table)

# 执行卡方检验
chi_square_test <- chisq.test(contingency_table)

# 检查预期频数是否符合检验要求
if (any(chi_square_test$expected < 5)) {
  warning("部分单元格的预期频数小于5，卡方检验结果可能不可靠。")
}

# 打印卡方检验结果
print("卡方检验结果：")
print(chi_square_test)

# 解释卡方检验结果
if (chi_square_test$p.value < 0.05) {
  print("结果：在 95% 置信水平下，年龄组与糖尿病患病状态之间存在显著关联。")
} else {
  print("结果：在 95% 置信水平下，年龄组与糖尿病患病状态之间无显著关联。")
}


# 清除工作环境
rm(list = ls())

# 加载必要的库
library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(tidyr)
# 读取数据
data <- read_excel("/Users/shidongping/Desktop/极值分析/【05】Calculation of mean excess BMI-years.xlsx", sheet = 1)
# 读取Excel数据的多个sheet
data1 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 1)
data2 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 2)
data3 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 3)
data4 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 4)
# 选择特定的sheet进行操作（例如data1）
data <- data4
#计算FPG和mean_ref_Cumulative_BMI_years（n>5000）的分别正态性及来两者的相关性，
# 计算正态性
# 对同一个new_new_ID，仅保留year列最大的数据
data <- data %>%
  group_by(new_new_ID) %>%
  slice_max(year) %>%
  ungroup()
data$FPG <- as.numeric(data$FPG)
data$mean_ref_Cumulative_BMI_years <- as.numeric(data$mean_ref_Cumulative_BMI_years)

# data <- data[data$FPG > 7, ]
# data <- data[data$mean_ref_Cumulative_BMI_years > 0, ]
# 去除 NA 值
fpg_data <- na.omit(data$FPG)
bmi_data <- na.omit(data$mean_ref_Cumulative_BMI_years)
hist(fpg_data)
hist(bmi_data)
# 检查样本大小
n_fpg <- length(fpg_data)
n_bmi <- length(bmi_data)
n_fpg
n_bmi
basicStats(fpg_data)
basicStats(bmi_data)
# 进行 KS 检验
# KS 检验 FPG 的正态性
ks_fpg <- ks.test(fpg_data, "pnorm", mean(fpg_data), sd(fpg_data))
print(ks_fpg)
# KS 检验 mean_ref_Cumulative_BMI_years 的正态性
ks_bmi <- ks.test(bmi_data, "pnorm", mean(bmi_data), sd(bmi_data))
print(ks_bmi)

# Shapiro-Wilk Test
shapiro.test(fpg_data)
shapiro.test(bmi_data)

# 2. Spearman秩相关分析 (适用于非正态数据)
spearman_test_filtered <- cor.test(fpg_data, bmi_data, method = "spearman")
spearman_test_filtered


# 清除工作环境
rm(list = ls())

# 加载必要的库
library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(fBasics)
# 读取数据
data <- read_excel("/Users/shidongping/Desktop/极值分析/【05】Calculation of mean excess BMI-years.xlsx", sheet = 1)
# 读取Excel数据的多个sheet
data1 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 1)
data2 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 2)
data3 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 3)
data4 <- read_excel("/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx", sheet = 4)
# 选择特定的sheet进行操作（例如data1）
data <- data4
data$FPG <- as.numeric(data$FPG)
data$mean_ref_Cumulative_BMI_years <- as.numeric(data$mean_ref_Cumulative_BMI_years)
#3. # 对FPG进行对数变换，使用log()函数
data <- data %>%
  mutate(FPG_log = log(FPG + 1))  # 加1是为了避免对0取对数
data$FPG_log <- as.numeric(data$FPG_log)
hist(data$FPG_log)
basicStats(data$FPG_log)

# 创建ggplot对象
p <- ggplot(data, aes(x = mean_ref_Cumulative_BMI_years, y = FPG_log)) +
  geom_point(color = "blue", alpha = 0.6) + # 添加蓝色透明度为0.6的点
  labs(title = "Scatter Plot of FPG_log vs. Mean Excess BMI-years",
       x = "Mean Excess BMI-years", y = "FPG_log") + # 添加图表标题和轴标签
  theme(plot.title = element_text(hjust = 0.5)) # 使标题居中

# 检查数据是否正常及绘图对象是否生成
if (!exists("p")) {
  stop("未能成功创建ggplot对象，请检查输入数据和代码。")
}

# 保存图像到PNG文件
jpeg("scatter plot-60+.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)
print(p)  # 打印ggplot对象到PNG文件
dev.off()  # 关闭PNG设备

# 通知用户图像保存成功
print("图像已成功保存为T10_044.jpeg")


