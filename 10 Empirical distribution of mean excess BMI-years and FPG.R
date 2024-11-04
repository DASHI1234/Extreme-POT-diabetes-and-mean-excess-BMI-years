# 清除工作环境
rm(list = ls())

# 加载必要的包
library(copula)
library(readxl)
library(dplyr)
library(MASS)

# 读取数据
data <- read_excel("/Users/shidongping/Desktop/极值分析/【05】Calculation of mean excess BMI-years.xlsx", sheet = 1)
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

# 计算unique的new_new_ID数量
new_new_ID_count <- data %>% 
  summarise(unique_new_new_ID = n_distinct(new_new_ID))

# 筛选 mean_ref_Cumulative_BMI_years 大于0的 new_new_ID
#data <- data[data$mean_ref_Cumulative_BMI_years > 0, ]
# 打印结果
print(new_new_ID_count)

# 将 FPG 列强制转换为数值型
data$FPG <- as.numeric(data$FPG)

# 将 FPG 和 mean_ref_Cumulative_BMI_years 转换为向量
FPG <- as.vector(data$FPG)
mean_ref_Cumulative_BMI_years <- as.vector(data$mean_ref_Cumulative_BMI_years)

# 计算经验分布函数
ecdf_FPG <- ecdf(FPG)
ecdf_mean_ref_Cumulative_BMI_years <- ecdf(mean_ref_Cumulative_BMI_years)

# 生成经验Copula数据
empirical_copula_data <- cbind(ecdf_mean_ref_Cumulative_BMI_years(mean_ref_Cumulative_BMI_years), 
                               ecdf_FPG(FPG))
# 绘制经验Copula散点图
plot(empirical_copula_data, 
     main = "Scatterplot of Empirical Copula",
     xlab = "ECDF of Mean Excess BMI-years", 
     ylab = "ECDF of FPG", 
     pch = 19, col = "grey")

# 添加对角线，用于参考完全独立的变量
abline(0, 1, col = "red", lty = 2)

#请把上述copula散点图的横纵坐标分别分成5份，整个图做成5*5的区域，然后计算每个小区域的密度，并输出密度值
# 分割坐标轴成5份
x_bins <- seq(0, 1, length.out = 6)
y_bins <- seq(0, 1, length.out = 6)

# 初始化一个矩阵来存储密度值
density_matrix <- matrix(0, nrow = 5, ncol = 5)

# 计算每个小区域的密度
for (i in 1:5) {
  for (j in 1:5) {
    in_bin <- empirical_copula_data[, 1] >= x_bins[i] & empirical_copula_data[, 1] < x_bins[i+1] &
      empirical_copula_data[, 2] >= y_bins[j] & empirical_copula_data[, 2] < y_bins[j+1]
    density_matrix[i, j] <- sum(in_bin) / nrow(empirical_copula_data)
  }
}

# 输出密度值
print(density_matrix)

# 绘制经验Copula散点图并添加区域网格
plot(empirical_copula_data, 
     main = "Scatterplot of Empirical Copula",
     xlab = "ECDF of Mean Excess BMI-years", 
     ylab = "ECDF of FPG", 
     pch = 19, col = "grey")

# 添加对角线，用于参考完全独立的变量
#abline(0, 1, col = "red", lty = 2)

# 添加网格线
for (i in 1:5) {
  abline(v = x_bins[i+1], col = "black", lty = 3)
  abline(h = y_bins[i+1], col = "black", lty = 3)
}

# 在每个小区域内添加密度值文本
for (i in 1:5) {
  for (j in 1:5) {
    text(x = (x_bins[i] + x_bins[i+1])/2, 
         y = (y_bins[j] + y_bins[j+1])/2, 
         labels = round(density_matrix[i, j], 5), 
         col = "blue")
  }
}
# 保存图像到 JPEG 文件
jpeg("Empirical Copula- 60+-update density.jpeg", units = "in", width = 14, height = 8, res = 300)

# 分割坐标轴成5份
x_bins <- seq(0, 1, length.out = 6)
y_bins <- seq(0, 1, length.out = 6)

# 初始化一个矩阵来存储密度值
density_matrix <- matrix(0, nrow = 5, ncol = 5)

# 计算每个小区域的密度
for (i in 1:5) {
  for (j in 1:5) {
    in_bin <- empirical_copula_data[, 1] >= x_bins[i] & empirical_copula_data[, 1] < x_bins[i+1] &
      empirical_copula_data[, 2] >= y_bins[j] & empirical_copula_data[, 2] < y_bins[j+1]
    density_matrix[i, j] <- sum(in_bin) / nrow(empirical_copula_data)
  }
}

# 输出密度值
print(density_matrix)

# 绘制经验Copula散点图并添加区域网格
plot(empirical_copula_data, 
     main = "Scatterplot of Empirical Copula",
     xlab = "ECDF of Mean Excess BMI-years", 
     ylab = "ECDF of FPG", 
     pch = 19, col = "grey")

# 添加对角线，用于参考完全独立的变量
#abline(0, 1, col = "red", lty = 2)

# 添加网格线
for (i in 1:5) {
  abline(v = x_bins[i+1], col = "black", lty = 3)
  abline(h = y_bins[i+1], col = "black", lty = 3)
}

# 在每个小区域内添加密度值文本
for (i in 1:5) {
  for (j in 1:5) {
    text(x = (x_bins[i] + x_bins[i+1])/2, 
         y = (y_bins[j] + y_bins[j+1])/2, 
         labels = round(density_matrix[i, j], 5), 
         col = "blue")
  }
}

# 关闭 JPEG 设备
dev.off()

# 通知用户图像保存成功
print("图像已成功保存为 scatterplot-60+.jpeg")



#底下内容备份
# 保存图像到 JPEG 文件
jpeg("Empirical Copula- total-1.jpeg", units = "in", width = 14, height = 8, res = 300)

# 再次绘制图像并保存
plot(empirical_copula_data, 
     main = "Scatterplot of Empirical Copula",
     xlab = "ECDF of Mean Excess BMI-years", 
     ylab = "ECDF of FPG", 
     pch = 19, col = "blue")

# 添加对角线，用于参考完全独立的变量
abline(0, 1, col = "red", lty = 2)

# 关闭 JPEG 设备
dev.off()

# 通知用户图像保存成功
print("图像已成功保存为 scatterplot-60+.jpeg")

