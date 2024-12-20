# 清除工作环境
rm(list = ls())

# 加载必要的库
library(readxl)
library(tidyverse)
library(writexl)
# 读取Excel文件
data <- read_excel("/Users/shidongping/Desktop/男女疾病负担/过程数据/泼天富贵/【处理版汇总V4.0_操作版本】体检_20240717_SDP.xlsx",sheet = 1)

# 将year列转换为数值类型
data <- data %>%
  mutate(year = as.numeric(year))

# 新增new_ID列，确保对于如上筛选出的birthday列相同以及gender列相同的数据赋上相同的new_ID
data_filtered <- data %>%
  group_by(birthday, gender) %>%
  mutate(new_ID = cur_group_id()) %>%
  ungroup()

# 确定需要计算非缺失值的列，排除指定的列
columns_to_check <- setdiff(names(data_filtered), c("new_ID", "ID", "note", 
                                                    "conclusion_1-conclusion_28", "conclusion_total", 
                                                    "history_diabetes", "history_hypertension", "Cerebral_apoplexy", 
                                                    "Coronary_heart_disease", "gout", "Abnormal_lipid_metabolism(hypertriglyceridemia)"))

# 为每一行计算非缺失值的数量
data_with_counts <- data_filtered %>%
  rowwise() %>%
  mutate(data_count = sum(!is.na(across(all_of(columns_to_check))))) %>%
  ungroup()

# 根据 new_ID, year 进行分组，并筛选出数据量最全的记录
data_clean <- data_with_counts %>%
  group_by(new_ID, year) %>%
  arrange(desc(data_count)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-data_count)

# 使用n_distinct()函数计算new_ID列中唯一值的个数
new_ID_count <- data_clean %>% 
  summarise(unique_new_ID = n_distinct(new_ID))

# 打印结果
print(new_ID_count)

# 筛选出new_ID相同（至少有两年的不同year数据）
data_filtered_1 <- data_clean %>%
  group_by(new_ID) %>%
  filter(n_distinct(year) >= 2)

# 按照 new_ID 分组，选择每个组中 year 最小的年份，并检查对应的 FPG 是否 >= 7.0
filtered_data_2 <- data_filtered_1 %>%
  group_by(new_ID) %>%
  filter(year == min(year)) %>%
  mutate(exclude = ifelse(FPG >= 7.0, TRUE, FALSE)) %>%
  ungroup()

# 标记需要排除的new_ID
ids_to_exclude <- filtered_data_2 %>%
  filter(exclude) %>%
  pull(new_ID)

# 排除标记为排除的 new_ID 的所有记录
final_data_1 <- data_filtered_1 %>%
  filter(!new_ID %in% ids_to_exclude)

# 对于同一个 new_ID，按照 year 从小到大排列，找到 FPG 第一次出现 >= 7.0 的年份，并保留该年份及之前的年份数据
final_data_2 <- final_data_1 %>%
  group_by(new_ID) %>%
  arrange(year) %>%  # 按 year 排序
  mutate(first_FPG_ge7_year = ifelse(FPG >= 7.0, year, NA)) %>%  # 标记 FPG >= 7.0 的年份
  mutate(first_FPG_ge7_year = min(first_FPG_ge7_year, na.rm = TRUE)) %>%  # 找到第一次出现 FPG >= 7.0 的年份
  mutate(first_FPG_ge7_year = ifelse(is.infinite(first_FPG_ge7_year), NA, first_FPG_ge7_year)) %>%  # 如果没有 FPG >= 7.0，则标记为 NA
  filter(is.na(first_FPG_ge7_year) | year <= first_FPG_ge7_year) %>%  # 保留 FPG >= 7.0 之前及该年份的记录
  ungroup()

# 使用n_distinct()函数计算new_ID列中唯一值的个数
new_ID_count <- final_data_2 %>% 
  summarise(unique_new_ID = n_distinct(new_ID))

# 打印结果
print(new_ID_count)

# 将结果写入Excel文件
write_xlsx(final_data_2, "/Users/shidongping/Desktop/极值分析/【001】data processing.xlsx")
