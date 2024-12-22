# 清除工作环境
rm(list = ls())

# 加载必要的包
library(ggplot2)
library(dplyr)

# 创建数据集
data1 <- data.frame(
  Age_Group = c("Total Group", "Younger Age Group", "Older Age Group"),
  x = c(0.085, 0.128, 0.044),
  CI_Lower = c(0.072, 0.109, 0.025),
  CI_Upper = c(0.097, 0.147, 0.059)
)

# 确保 Age_Group 是因子并按顺序排列
data1 <- data1 %>%  
  mutate(Age_Group = factor(Age_Group, levels = c("Total Group", "Younger Age Group", "Older Age Group")))

# 绘制图形
ggplot(data1, aes(x = Age_Group, y = x)) +
  # 添加点
  geom_point(size = 3, colour = 'skyblue') +
  # 添加误差条
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, colour = 'skyblue') +
  # 设置图形标题和轴标签
  labs(title = "", x = "Group", y = expression(chi ~ "(95% CI)")) +  # 使用表达式绘制y轴标签
  # 设置主题样式
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # 标题居中
    axis.title.x = element_text(size = 14),             # x轴标题字体大小
    axis.title.y = element_text(size = 14),             # y轴标题字体大小
    axis.text.x = element_text(size = 12),              # x轴刻度字体大小
    axis.text.y = element_text(size = 12),               # y轴刻度字体大小
    panel.border = element_rect(colour = "black", fill = NA), # 设置内侧边框
    plot.background = element_rect(fill = "white", colour = NA) # 背景色，无边框
  ) +
  # 添加红色虚线标注误差范围
  annotate("segment", x = 1, xend = 1, y = data1$CI_Lower[1], yend = data1$CI_Upper[1], linetype = "dashed", color = "red") +
  annotate("segment", x = 2, xend = 2, y = data1$CI_Lower[2], yend = data1$CI_Upper[2], linetype = "dashed", color = "red") +
  annotate("segment", x = 3, xend = 3, y = data1$CI_Lower[3], yend = data1$CI_Upper[3], linetype = "dashed", color = "red") +
  # 在每个点下方标注误差范围的文本
  annotate("text", x = 1, y = data1$CI_Lower[1] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[1], data1$CI_Upper[1]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 2, y = data1$CI_Lower[2] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[2], data1$CI_Upper[2]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 3, y = data1$CI_Lower[3] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[3], data1$CI_Upper[3]), size = 3, hjust = 0.5, colour = "black")


jpeg("Annualized_CEBR-x_95CI_age_group-add broder.jpeg", units = "in", width = 7, height = 6, res = 500)
# 绘制图形
ggplot(data1, aes(x = Age_Group, y = x)) +
  # 添加点
  geom_point(size = 3, colour = 'skyblue') +
  # 添加误差条
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, colour = 'skyblue') +
  # 设置图形标题和轴标签
  labs(title = "", x = "Group", y = expression(chi ~ "(95% CI)")) +  # 使用表达式绘制y轴标签
  # 设置主题样式
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # 标题居中
    axis.title.x = element_text(size = 14),             # x轴标题字体大小
    axis.title.y = element_text(size = 14),             # y轴标题字体大小
    axis.text.x = element_text(size = 12),              # x轴刻度字体大小
    axis.text.y = element_text(size = 12),               # y轴刻度字体大小
    panel.border = element_rect(colour = "black", fill = NA), # 设置内侧边框
    plot.background = element_rect(fill = "white", colour = NA) # 背景色，无边框
  ) +
  # 添加红色虚线标注误差范围
  annotate("segment", x = 1, xend = 1, y = data1$CI_Lower[1], yend = data1$CI_Upper[1], linetype = "dashed", color = "red") +
  annotate("segment", x = 2, xend = 2, y = data1$CI_Lower[2], yend = data1$CI_Upper[2], linetype = "dashed", color = "red") +
  annotate("segment", x = 3, xend = 3, y = data1$CI_Lower[3], yend = data1$CI_Upper[3], linetype = "dashed", color = "red") +
  # 在每个点下方标注误差范围的文本
  annotate("text", x = 1, y = data1$CI_Lower[1] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[1], data1$CI_Upper[1]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 2, y = data1$CI_Lower[2] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[2], data1$CI_Upper[2]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 3, y = data1$CI_Lower[3] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[3], data1$CI_Upper[3]), size = 3, hjust = 0.5, colour = "black")

dev.off()  # 关闭设备

