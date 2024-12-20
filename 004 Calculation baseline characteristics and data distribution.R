# 清除工作环境
rm(list = ls())

# 加载必要的库
library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
# 读取Excel数据的多个sheet
# 读取Excel数据的多个sheet
data1 <- read_excel("/Users/shidongping/Desktop/极值分析/【002】Cumulative BMI-years calculation of reference and grouping for standard for 1.xlsx", sheet = 1)
data2 <- read_excel("/Users/shidongping/Desktop/极值分析/【002】Cumulative BMI-years calculation of reference and grouping for standard for 2.xlsx", sheet = 1)
data3 <- read_excel("/Users/shidongping/Desktop/极值分析/【002】Cumulative BMI-years calculation of reference and grouping for standard for 2.xlsx", sheet = 2)

# 选择特定的sheet进行操作（例如data1）
data <- data3
# 对同一个new_new_ID，仅保留year列最小的数据
data <- data %>%
  group_by(new_ID) %>%
  slice_min(year) %>%
  ungroup()

new_ID_count <- data %>% 
  summarise(unique_new_ID = n_distinct(new_ID))
# 打印结果
print(new_ID_count)
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

# 计算比例，prop.table() 函数将返回每个类别的比例
gender_ratio <- prop.table(gender_table)

# 将比例转换为百分比
gender_ratio_percent <- gender_ratio * 100

# 创建一个数据框，包含性别、个数和百分比
gender_summary <- data.frame(
  Gender = names(gender_table),
  Count = as.numeric(gender_table),
  Ratio = gender_ratio_percent
)

# 打印结果
print(gender_summary)
# 打印男女比例
cat("Gender ratio:\n")
print(gender_ratio)

# 清除工作环境
rm(list = ls())
# 创建数据框
data <- data.frame(
  age_group = rep(c("Total", "Young", "Old"), times = 2),
  gender = rep(c("female", "male"), each = 3),
  count = c(15712, 9075, 6637, 16530, 9437, 7093)
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

# 加载dplyr包，如果尚未加载
library(dplyr)
data <- data1

# 对同一个new_new_ID，仅保留year列最大的数据
data <- data %>%
  group_by(new_ID) %>%
  slice_max(year) %>%
  ungroup()

# 判断FPG列，如果大于等于7.0则为diabetes，否则为No diabetes
data <- data %>%
  mutate(Diabetes_Status = ifelse(FPG >= 7.0, "Diabetes", "No diabetes"))

# 计算Diabetes和No diabetes的个数
diabetes_counts <- table(data$Diabetes_Status)

# 将计数转换为数据框
diabetes_summary <- data.frame(
  Condition = names(diabetes_counts),
  Count = as.numeric(diabetes_counts)
)

# 打印结果
print(diabetes_summary)

# 创建数据框
data <- data.frame(
  age_group = rep(c("Total", "Young", "Old"), times = 2),
  diabetes_status = rep(c("Diabetes Incidence", "No Diabetes Incidence"), each = 3),
  count = c(3282, 1131, 2151, 28960, 17381, 11579)
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
library(fBasics)

data <- data3
#计算FPG和mean_ref_Cumulative_BMI_years（n>5000）的分别正态性及来两者的相关性，
# 计算正态性
# 对同一个new_new_ID，仅保留year列最大的数据
data <- data %>%
  group_by(new_ID) %>%
  slice_max(year) %>%
  ungroup()
data$FPG <- as.numeric(data$FPG)
data$mean_Cumulative_BMI_years <- as.numeric(data$mean_Cumulative_BMI_years)

# 去除 NA 值
fpg_data <- na.omit(data$FPG)
bmi_data <- na.omit(data$mean_Cumulative_BMI_years)
hist(fpg_data)
hist(bmi_data)

# 检查样本大小
n_fpg <- length(fpg_data)
n_bmi <- length(bmi_data)
n_fpg
n_bmi
basicStats(fpg_data)
basicStats(bmi_data)
#boxplot(fpg_data, main = "Boxplot of fpg_data")


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
