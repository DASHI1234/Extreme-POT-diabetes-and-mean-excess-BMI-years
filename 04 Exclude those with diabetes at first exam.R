# 清除工作环境
rm(list = ls())
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)
# 读取数据
data <- read_excel("/Users/shidongping/Desktop/极值分析/【02】Remove duplicate medical exam records.xlsx", sheet = 1)

# 使用n_distinct()函数计算new_new_ID列中唯一值的个数
new_new_ID_count <- data %>% 
  summarise(unique_new_new_ID = n_distinct(new_new_ID))

# 打印结果
print(new_new_ID_count)

# 按照 new_new_ID 分组，选择每个组中 year 最小的年份，并检查对应的 FPG 是否 >= 7.0
filtered_data <- data %>%
  group_by(new_new_ID) %>%
  filter(year == min(year)) %>%
  mutate(exclude = ifelse(FPG >= 7.0, TRUE, FALSE)) %>%
  ungroup()

# 标记需要排除的 new_new_ID
ids_to_exclude <- filtered_data %>%
  filter(exclude) %>%
  pull(new_new_ID)

# 排除标记为排除的 new_new_ID 的所有记录
final_data_1 <- data %>%
  filter(!new_new_ID %in% ids_to_exclude)

# 对于同一个 new_new_ID，按照 year 从小到大排列，找到 FPG 第一次出现 >= 7.0 的年份，并保留该年份及之前的年份数据
final_data_2 <- final_data_1 %>%
  group_by(new_new_ID) %>%
  arrange(year) %>%  # 按 year 排序
  mutate(first_FPG_ge7_year = ifelse(FPG >= 7.0, year, NA)) %>%  # 标记 FPG >= 7.0 的年份
  mutate(first_FPG_ge7_year = min(first_FPG_ge7_year, na.rm = TRUE)) %>%  # 找到第一次出现 FPG >= 7.0 的年份
  mutate(first_FPG_ge7_year = ifelse(is.infinite(first_FPG_ge7_year), NA, first_FPG_ge7_year)) %>%  # 如果没有 FPG >= 7.0，则标记为 NA
  filter(is.na(first_FPG_ge7_year) | year <= first_FPG_ge7_year) %>%  # 保留 FPG >= 7.0 之前及该年份的记录
  ungroup()

# 写入筛选后的数据到 Excel 文件
write_xlsx(final_data_2, "/Users/shidongping/Desktop/极值分析/【04】Exclude those with diabetes at first exam.xlsx")