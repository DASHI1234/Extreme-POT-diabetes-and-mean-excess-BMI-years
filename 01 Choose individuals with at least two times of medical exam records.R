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

# 筛选出birthday列相同以及gender列相同（至少有两年的不同year数据）
data_filtered <- data %>%
  group_by(birthday, gender) %>%
  filter(n_distinct(year) >= 2)

# 新增new_ID列，确保对于如上筛选出的birthday列相同以及gender列相同的数据赋上相同的new_ID
data_filtered <- data_filtered %>%
  group_by(birthday, gender) %>%
  mutate(new_ID = cur_group_id()) %>%
  ungroup()

# 筛选出至少两年new_age列差值和year列差值相同的数据
data_filtered <- data_filtered %>%
  arrange(birthday, gender, year) %>%
  mutate(new_age_diff = new_age - lag(new_age),
         year_diff = year - lag(year)) %>%
  filter(!is.na(new_age_diff) & new_age_diff == year_diff) %>%
  group_by(birthday, gender) %>%
  filter(n() >= 2) %>%
  ungroup()

# 生成按顺序从1逐步增加的连续ID，确保相同的原new_ID仍然相同
data_filtered <- data_filtered %>%
  mutate(new_ID = dense_rank(new_ID))

# 将new_ID列放到最前面
data_filtered <- data_filtered %>%
  select(new_ID, everything())

# 查看筛选后的数据
print(data_filtered)

# 将数据框写入Excel文件
write_xlsx(data_filtered, "/Users/shidongping/Desktop/极值分析/【01】Choose individuals with at least two times of medical exam records.xlsx")

