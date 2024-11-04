# 清除工作环境
rm(list = ls())

# 加载必要的库
library(readxl)
library(tidyverse)
library(writexl)

# 读取Excel文件
data <- read_excel("/Users/shidongping/Desktop/极值分析/【01】Choose individuals with at least two times of medical exam records.xlsx", sheet = 1)

# 筛选出 new_ID 列和 ID 列都相同的数据
data_filtered <- data %>%
  group_by(new_ID, ID) %>%
  filter(n() > 1) %>%
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

# 根据 new_ID, ID, year 进行分组，并筛选出数据量最全的记录
data_clean <- data_with_counts %>%
  group_by(new_ID, ID, year) %>%
  arrange(desc(data_count)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-data_count)

# 去掉 ID 编号出现次数仅为1的数据
data_clean <- data_clean %>%
  group_by(ID) %>%
  filter(n() > 1) %>%
  ungroup()

# 生成新的 new_new_ID 列，从 1 开始递增
data_clean <- data_clean %>%
  group_by(ID) %>%
  mutate(new_new_ID = cur_group_id()) %>%
  ungroup()

# 确保 new_new_ID 是从 1 开始连续的
data_clean <- data_clean %>%
  arrange(new_new_ID) %>%
  mutate(new_new_ID = dense_rank(new_new_ID))

# 将 new_new_ID 列放在最前面
data_clean <- data_clean %>%
  select(new_new_ID, everything())

# 查看筛选后的数据
print(data_clean)

# 将结果写入Excel文件
write_xlsx(data_clean, "/Users/shidongping/Desktop/极值分析/【02】Remove duplicate medical exam records.xlsx")
