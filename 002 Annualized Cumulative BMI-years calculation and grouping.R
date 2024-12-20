# 清除工作环境
rm(list = ls())
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)

# 读取数据
data <- read_excel("/Users/shidongping/Desktop/极值分析/【001】data processing.xlsx", sheet = 1)

# 数据处理-overweight
reference_BMI_ow <- 24  # 设置参考 BMI

data <- data %>%
  group_by(new_ID) %>%  # 按 new_ID 分组
  arrange(year) %>%  # 按 year 从小到大排序
  mutate(
    # 计算 BMI 差值
    new_BMI_diff = new_BMI - reference_BMI_ow,
    
    # 根据 BMI 差值计算 ref_BMI_year
    BMI_year = case_when(
      # 情况 1：BMI 差值同为正值
      new_BMI_diff > 0 & lag(new_BMI_diff) > 0 ~ (year - lag(year)) * (new_BMI_diff + lag(new_BMI_diff)) / 2,
      
      # 情况 2：BMI 差值同为负值
      new_BMI_diff < 0 & lag(new_BMI_diff) < 0 ~ (year - lag(year)) * (new_BMI_diff + lag(new_BMI_diff)) / 2,
      
      # 情况 3：BMI 差值一正一负
      TRUE ~ (year - lag(year)) * (
        abs(lag(new_BMI_diff)) / (abs(lag(new_BMI_diff)) + abs(new_BMI_diff)) * lag(new_BMI_diff) / 2 +
          abs(new_BMI_diff) / (abs(lag(new_BMI_diff)) + abs(new_BMI_diff)) * new_BMI_diff / 2
      )
    ),
    
    # 将缺失值（NA）替换为 0，避免累加时出错
    BMI_year = replace_na(BMI_year, 0),
    
    # 计算累计 ref_BMI_year
    Cumulative_BMI_years = cumsum(BMI_year)
  ) %>%
  ungroup()

# 计算 mean_Cumulative_BMI_years
data <- data %>%
  group_by(new_ID) %>% 
  arrange(year, .by_group = TRUE) %>%  # 按 year 升序排列，确保 last() 取最大年份的值
  mutate(
    # 计算标准化的 mean_Cumulative_BMI_years
    mean_Cumulative_BMI_years = last(Cumulative_BMI_years) / (last(year) - first(year))
  ) %>% 
  ungroup()  # 解除分组，避免后续代码受到分组影响


# 对每个new_ID按照年份从大到小排列
data_sorted <- data %>%
  group_by(new_ID) %>%
  arrange(desc(year))

# 获取每个new_ID最小年份的new_age数据进行分组
min_year_data <- data_sorted %>%
  group_by(new_ID) %>%
  slice_min(order_by = year, with_ties = FALSE) %>%
  mutate(age_group = case_when(
    new_age < 45 ~ "young",
    new_age >= 45 ~ "old"
  ))

# 将最小年份数据与所有年份数据进行合并
data_grouped <- data_sorted %>%
  left_join(min_year_data %>% select(new_ID, age_group), by = "new_ID")

# 将数据按年龄分组后，分别输出到同一个Excel文件的不同sheet
age_groups <- split(data_grouped, data_grouped$age_group)


# 将处理后的数据写入 Excel 文件
write_xlsx(data, "/Users/shidongping/Desktop/极值分析/【002】Cumulative BMI-years calculation of reference and grouping for standard for 1.xlsx")
# 将处理后的数据写入 Excel 文件
write_xlsx(age_groups, "/Users/shidongping/Desktop/极值分析/【002】Cumulative BMI-years calculation of reference and grouping for standard for 2.xlsx")


