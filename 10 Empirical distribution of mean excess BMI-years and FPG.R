# 清除工作环境
rm(list = ls())

# 加载必要的包
library(copula)
library(readxl)
library(dplyr)
library(MASS)
# 绘制等高线图
library(ggplot2)
library(viridis)
library(plotly)

# 读取数据
#data <- read_excel("/Users/shidongping/Desktop/极值分析/【05】Calculation of mean excess BMI-years.xlsx", sheet = 1)
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
empirical_copula_data <- cbind(
  ecdf_mean_ref_Cumulative_BMI_years(mean_ref_Cumulative_BMI_years),
  ecdf_FPG(FPG)
)

# 分割坐标轴成5份
x_bins <- seq(0, 1, length.out = 6)
y_bins <- seq(0, 1, length.out = 6)

# 初始化一个矩阵来存储密度值
density_matrix <- matrix(0, nrow = 5, ncol = 5)

# 计算每个小区域的密度
for (i in 1:5) {
  for (j in 1:5) {
    in_bin <- empirical_copula_data[, 1] >= x_bins[i] & empirical_copula_data[, 1] < x_bins[i + 1] &
      empirical_copula_data[, 2] >= y_bins[j] & empirical_copula_data[, 2] < y_bins[j + 1]
    density_matrix[i, j] <- sum(in_bin) / nrow(empirical_copula_data)
  }
}

# 输出密度值
print(density_matrix)

# 转换为 proportion matrix
proportion_matrix <- density_matrix

# 输出 proportion_matrix 检查
print(proportion_matrix)
cat("Sum of all proportions:", sum(proportion_matrix), "\n")
# 创建网格数据
x_centers <- x_bins[-length(x_bins)]
y_centers <- y_bins[-length(y_bins)]
grid <- expand.grid(X = x_centers, Y = y_centers)
grid$Proportion <- as.vector(proportion_matrix)

# 绘制三维表面图，其中颜色显示的是区域内一致的比例值

plot_ly() %>%
  add_surface(
    x = ~x_centers,
    y = ~y_centers,
    z = ~proportion_matrix,
    surfacecolor = ~proportion_matrix,
    colors = colorRamp(c("blue", "yellow", "red"))
  ) %>%
  layout(
    title = "3D Surface Plot of Empirical Copula Proportion",
    scene = list(
      xaxis = list(title = "ECDF of Mean Excess BMI-years"),
      yaxis = list(title = "ECDF of FPG"),
      zaxis = list(title = "Proportion")
    )
  )

reticulate::py_install("kaleido")

install.packages("processx") # orca 依赖
install.packages("reticulate") # Python 依赖
install.packages("reticulate")
library(reticulate)
# 保存为 JPEG 文件
library(plotly)
reticulate::py_config()
reticulate::py_install("plotly")
reticulate::py_install("kaleido")

# 创建 3D 图形
p <- plot_ly() %>%
  add_surface(
    x = ~x_centers,
    y = ~y_centers,
    z = ~proportion_matrix,
    surfacecolor = ~proportion_matrix,
    colors = colorRamp(c("blue", "yellow", "red"))
  ) %>%
  layout(
    title = "3D Surface Plot of Empirical Copula Proportion",
    scene = list(
      xaxis = list(title = "ECDF of Mean Excess BMI-years"),
      yaxis = list(title = "ECDF of FPG"),
      zaxis = list(title = "Proportion")
    )
  )

# 保存为 JPEG 文件
p <- plotly::plotly_build(p)
plotly::save_image(p, "Empirical_Copula_60+_3D_Surface_Plot.jpeg", width = 1400, height = 800, scale = 2)

