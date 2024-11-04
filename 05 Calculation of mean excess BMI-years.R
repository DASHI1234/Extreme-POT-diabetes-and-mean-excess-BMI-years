# 清除工作环境
rm(list = ls())
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)

# 读取数据
data <- read_excel("/Users/shidongping/Desktop/极值分析/【04】Exclude those with diabetes at first exam.xlsx", sheet = 1)

# 计算 unique new_new_ID 的个数
new_new_ID_count <- data %>% 
  summarise(unique_new_new_ID = n_distinct(new_new_ID))

# 打印结果
print(new_new_ID_count)

# 数据处理
reference_BMI_ow <- 24  # 设置参考 BMI

data <- data %>%
  group_by(new_new_ID) %>%
  arrange(year) %>%
  mutate(
    new_BMI_diff = new_BMI - reference_BMI_ow,  # 计算 new_BMI_diff
    ref_BMI_year = case_when(
      new_BMI_diff > 0 & lag(new_BMI_diff) > 0 ~ {
        (year - lag(year)) * (new_BMI_diff + lag(new_BMI_diff)) / 
          if_else(new_BMI_diff == lag(new_BMI_diff), 1, 2)
      },
      new_BMI_diff < 0 & lag(new_BMI_diff) < 0 ~ {
        (year - lag(year)) * (new_BMI_diff + lag(new_BMI_diff)) / 
          if_else(new_BMI_diff == lag(new_BMI_diff), 1, 2)
      },
      TRUE ~ (year - lag(year)) / 2 * lag(new_BMI_diff) / 
        (abs(lag(new_BMI_diff)) + abs(new_BMI_diff)) + 
        (year - lag(year)) / 2 * new_BMI_diff / 
        (abs(lag(new_BMI_diff)) + abs(new_BMI_diff))  # 一正一负
    ),
    ref_BMI_year = replace_na(ref_BMI_year, 0),  # 处理无前一年数据的情况
    ref_Cumulative_BMI_years = cumsum(ref_BMI_year)  # 累加计算
  ) %>%
  ungroup()

# 计算 mean_ref_Cumulative_BMI_years
mean_ref_Cumulative_BMI_years <- data %>%
  group_by(new_new_ID) %>%
  summarise(
    mean_ref_Cumulative_BMI_years = last(ref_Cumulative_BMI_years) / (sum(year - first(year)) / n())
  )

data <- data %>%
  group_by(new_new_ID) %>%
  mutate(mean_ref_Cumulative_BMI_years = last(ref_Cumulative_BMI_years) / (sum(year - first(year)) / n())) %>%
  ungroup()  # 确保在后续操作中不再按组处理

# 将处理后的数据写入 Excel 文件
write_xlsx(data, "/Users/shidongping/Desktop/极值分析/【05】Calculation of mean excess BMI-years.xlsx")
