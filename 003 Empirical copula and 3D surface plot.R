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

# 计算unique的new_new_ID数量
new_ID_count <- data %>% 
  summarise(unique_new_ID = n_distinct(new_ID))
# 打印结果
print(new_ID_count)

# 将 FPG 列强制转换为数值型
data$FPG <- as.numeric(data$FPG)

# 将 FPG 和 mean_ref_Cumulative_BMI_years 转换为向量
FPG <- as.vector(data$FPG)
Cumulative_BMI_years <- as.vector(data$mean_Cumulative_BMI_years)

# 计算经验分布函数
ecdf_FPG <- ecdf(FPG)
ecdf_Cumulative_BMI_years <- ecdf(Cumulative_BMI_years)

# 生成经验Copula数据
empirical_copula_data <- cbind(
  ecdf_Cumulative_BMI_years(Cumulative_BMI_years),
  ecdf_FPG(FPG)
)

# 分割坐标轴成5份
x_bins <- seq(0, 1, length.out = 6)
y_bins <- seq(0, 1, length.out = 6)

# 初始化一个矩阵来存储密度值
density_matrix <- matrix(0, nrow = 5, ncol = 5)
density_matrix_lower <- matrix(0, nrow = 5, ncol = 5)  # 存储密度置信区间下界
density_matrix_upper <- matrix(0, nrow = 5, ncol = 5)  # 存储密度置信区间上界

set.seed(1234)  # 设置随机种子，使得结果可以重现

# 计算每个小区域的密度和置信区间
n_bootstrap <- 1000  # bootstrap重复次数

for (i in 1:5) {
  for (j in 1:5) {
    # 找到当前区域内的数据点
    in_bin <- empirical_copula_data[, 1] >= x_bins[i] & empirical_copula_data[, 1] < x_bins[i+1] &
      empirical_copula_data[, 2] >= y_bins[j] & empirical_copula_data[, 2] < y_bins[j+1]
    
    sample_data <- empirical_copula_data[in_bin, ]
    n_points_in_bin <- nrow(sample_data)  # 当前区域的点数
    
    # 计算该区域的密度
    if (n_points_in_bin > 0) {
      density_matrix[i, j] <- n_points_in_bin / nrow(empirical_copula_data)
      
      # 使用Bootstrap计算密度的95%置信区间
      bootstrap_densities <- numeric(n_bootstrap)
      
      for (b in 1:n_bootstrap) {
        # Bootstrap抽样：这里从整个 `empirical_copula_data` 中抽样，以确保多样性
        bootstrap_sample <- empirical_copula_data[sample(1:nrow(empirical_copula_data), nrow(sample_data), replace = TRUE), ]
        
        # 计算bootstrap样本的密度
        bootstrap_density <- sum(bootstrap_sample[, 1] >= x_bins[i] & bootstrap_sample[, 1] < x_bins[i+1] &
                                   bootstrap_sample[, 2] >= y_bins[j] & bootstrap_sample[, 2] < y_bins[j+1]) / nrow(bootstrap_sample)
        
        bootstrap_densities[b] <- bootstrap_density
      }
      
      # 计算95%的置信区间
      density_matrix_lower[i, j] <- quantile(bootstrap_densities, 0.025)
      density_matrix_upper[i, j] <- quantile(bootstrap_densities, 0.975)
    }
  }
}

# 输出密度值矩阵和置信区间
cat("密度值矩阵：\n")
print(density_matrix)

cat("密度置信区间下界：\n")
print(density_matrix_lower)

cat("密度置信区间上界：\n")
print(density_matrix_upper)


# 加载必要的包
library(ggplot2)
library(dplyr)

# 创建数据集
data1 <- data.frame(
  Age_Group = c("Total Group", "Younger Age Group", "Older Age Group"),
  Copula_Density = c(0.061, 0.067, 0.051),
  CI_Lower = c(0.051, 0.052, 0.034),
  CI_Upper = c(0.072, 0.080, 0.069)
)

# 确保 Age_Group 是因子并按顺序排列
data1 <- data1 %>% 
  mutate(Age_Group = factor(Age_Group, levels = c("Total Group", "Younger Age Group", "Older Age Group")))

# 绘制图形
ggplot(data1, aes(x = Age_Group, y = Copula_Density)) +
  geom_point(size = 3, colour = 'skyblue') +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, colour = 'skyblue') +
  labs(title = "", x = "", y = "Empirical Copula Proportion" )+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA), # 设置内侧边框
    plot.background = element_rect(fill = "white", colour = NA) # 背景色，无边框
  ) +
  # 添加置信区间文字标签
  geom_text(aes(x = Age_Group, y = CI_Lower - 0.02, 
                label = sprintf("CI: [%.3f, %.3f]", CI_Lower, CI_Upper)), 
            size = 3, hjust = 0.5, colour = "black")



jpeg("Annualized_CEBR_Copula_Density PLOT.jpeg", units = "in", width = 7, height = 6, res = 500)
# 绘制图形
ggplot(data1, aes(x = Age_Group, y = Copula_Density)) +
  geom_point(size = 3, colour = 'skyblue') +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, colour = 'skyblue') +
  labs(title = "", x = "Group", y = "Empirical Copula Proportion" )+
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA), # 设置内侧边框
    plot.background = element_rect(fill = "white", colour = NA) # 背景色，无边框
  ) +
  # 添加置信区间文字标签
  geom_text(aes(x = Age_Group, y = CI_Lower - 0.02, 
                label = sprintf("CI: [%.3f, %.3f]", CI_Lower, CI_Upper)), 
            size = 3, hjust = 0.5, colour = "black")

dev.off()  # 关闭设备
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
      xaxis = list(title = "ECDF of CEBR"),
      yaxis = list(title = "ECDF of FPG"),
      zaxis = list(title = "Proportion")
    )
  )

#reticulate::py_install("kaleido")
#install.packages("processx") # orca 依赖
#install.packages("reticulate") # Python 依赖
#install.packages("reticulate")
library(reticulate)
# 保存为 JPEG 文件
library(plotly)
#reticulate::py_config()
#reticulate::py_install("plotly")
#reticulate::py_install("kaleido")

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
      xaxis = list(title = "ECDF of CEBR"),
      yaxis = list(title = "ECDF of FPG"),
      zaxis = list(title = "Proportion")
    )
  )

# 保存为 JPEG 文件
p <- plotly::plotly_build(p)
plotly::save_image(p, "Annualized-CEBR-old_Empirical_Copula_3D_Surface_Plot.jpeg", width = 1400, height = 800, scale = 2)

