# 清除工作环境
rm(list = ls())

# 加载必要的库
library(dplyr)
library(readxl)
library(writexl)

# 读取数据
data <- read_excel("/Users/shidongping/Desktop/极值分析/【05】Calculation of mean excess BMI-years.xlsx", sheet = 1)

# 对每个new_new_ID按照年份从大到小排列
data_sorted <- data %>%
  group_by(new_new_ID) %>%
  arrange(desc(year))

# 获取每个new_new_ID最小年份的new_age数据进行分组
min_year_data <- data_sorted %>%
  group_by(new_new_ID) %>%
  slice_min(order_by = year, with_ties = FALSE) %>%
  mutate(age_group = case_when(
    new_age >= 18 & new_age < 30 ~ "18-29",
    new_age >= 30 & new_age < 45 ~ "30-44",
    new_age >= 45 & new_age < 60 ~ "45-59",
    new_age >= 60 ~ "60+"
  ))

# 将最小年份数据与所有年份数据进行合并
data_grouped <- data_sorted %>%
  left_join(min_year_data %>% select(new_new_ID, age_group), by = "new_new_ID")

# 将数据按年龄分组后，分别输出到同一个Excel文件的不同sheet
age_groups <- split(data_grouped, data_grouped$age_group)

# 定义输出文件路径
output_file <- "/Users/shidongping/Desktop/极值分析/【07】Divide the entire population into age groups.xlsx"

# 将每个age group的数据写入不同的Sheet
write_xlsx(age_groups, path = output_file)

#请给出不同sheet的new_new_ID数量，且同一个new_new_ID按照year排列，选择最大year，并计算最大yearFPG≥7.0的数量
# 统计不同age group的new_new_ID数量，以及最大year FPG≥7.0的数量
summary_info <- lapply(age_groups, function(group_data) {
  # 获取unique的new_new_ID数量
  unique_ids_count <- n_distinct(group_data$new_new_ID)
  
  # 选择每个new_new_ID的最大年份
  max_year_fpg_count <- group_data %>%
    group_by(new_new_ID) %>%
    filter(year == max(year)) %>%
    summarise(max_year_fpg_7 = max(FPG, na.rm = TRUE) >= 7) %>%
    summarise(count = sum(max_year_fpg_7))
  
  list(
    unique_ids_count = unique_ids_count,
    fpg_count = max_year_fpg_count$count
  )
})

# 将统计结果整理为数据框
summary_df <- data.frame(
  age_group = names(summary_info),
  unique_ids_count = sapply(summary_info, function(x) x$unique_ids_count),
  fpg_count = sapply(summary_info, function(x) x$fpg_count)
)

print(summary_df)
