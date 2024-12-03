# 清除工作环境
rm(list = ls())

# 加载入必要的库
library(ggplot2)
library(dplyr)

# 创建数据集
data1 <- data.frame(
  Age_Group = c("18-29", "30-44", "45-59", "60+"),
  x = c(0.222, 0.116, 0.086, 0.035),
  CI_Lower = c(0.132, 0.088, 0.061, 0.004),
  CI_Upper = c(0.309, 0.143, 0.111, 0.066)
)

# 确保 Age_Group 是因子并按顺序排列
data1 <- data1 %>% mutate(Age_Group = factor(Age_Group, levels = c("18-29", "30-44", "45-59", "60+")))
# 绘制图形
ggplot(data1, aes(x = Age_Group, y = x)) +
  geom_point(size = 3, colour = 'skyblue') +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, colour = 'skyblue') +
  labs(title = "", x = "Age Group", y = expression(chi ~ "(95% CI)")) +
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
  annotate("segment", x = 1, xend = 1, y = data1$CI_Lower[1], yend = data1$CI_Upper[1], linetype = "dashed", color = "red") +
  annotate("segment", x = 2, xend = 2, y = data1$CI_Lower[2], yend = data1$CI_Upper[2], linetype = "dashed", color = "red") +
  annotate("segment", x = 3, xend = 3, y = data1$CI_Lower[3], yend = data1$CI_Upper[3], linetype = "dashed", color = "red") +
  annotate("segment", x = 4, xend = 4, y = data1$CI_Lower[4], yend = data1$CI_Upper[4], linetype = "dashed", color = "red") +
  annotate("text", x = 1, y = data1$CI_Lower[1] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[1], data1$CI_Upper[1]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 2, y = data1$CI_Lower[2] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[2], data1$CI_Upper[2]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 3, y = data1$CI_Lower[3] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[3], data1$CI_Upper[3]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 4, y = data1$CI_Lower[4] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[4], data1$CI_Upper[4]), size = 3, hjust = 0.5, colour = "black")

jpeg("x_95CI_age_group-add broder.jpeg", units = "in", width = 7, height = 6, res = 500)
# 绘制图形
ggplot(data1, aes(x = Age_Group, y = x)) +
  geom_point(size = 3, colour = 'skyblue') +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, colour = 'skyblue') +
  labs(title = "", x = "Age Group", y = expression(chi ~ "(95% CI)")) +
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
  annotate("segment", x = 1, xend = 1, y = data1$CI_Lower[1], yend = data1$CI_Upper[1], linetype = "dashed", color = "red") +
  annotate("segment", x = 2, xend = 2, y = data1$CI_Lower[2], yend = data1$CI_Upper[2], linetype = "dashed", color = "red") +
  annotate("segment", x = 3, xend = 3, y = data1$CI_Lower[3], yend = data1$CI_Upper[3], linetype = "dashed", color = "red") +
  annotate("segment", x = 4, xend = 4, y = data1$CI_Lower[4], yend = data1$CI_Upper[4], linetype = "dashed", color = "red") +
  annotate("text", x = 1, y = data1$CI_Lower[1] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[1], data1$CI_Upper[1]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 2, y = data1$CI_Lower[2] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[2], data1$CI_Upper[2]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 3, y = data1$CI_Lower[3] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[3], data1$CI_Upper[3]), size = 3, hjust = 0.5, colour = "black") +
  annotate("text", x = 4, y = data1$CI_Lower[4] - 0.02, label = sprintf("CI: [%.3f, %.3f]", data1$CI_Lower[4], data1$CI_Upper[4]), size = 3, hjust = 0.5, colour = "black")

dev.off()  # 关闭设备

