# 清除工作环境
rm(list = ls())

# 加载必要的库
library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
# 读取Excel文件
data <- read_excel("/Users/shidongping/Desktop/极值分析/【02】Remove duplicate medical exam records.xlsx", sheet = 1)

# 定义一个函数来处理包含多个测量值的单元格
process_cell <- function(cell_value) {
  # 如果单元格包含"/"，说明有多个测量值
  if (grepl("/", cell_value)) {
    # 将测量值分割为单独的数值，并计算均值
    values <- as.numeric(unlist(strsplit(cell_value, "/")))
    return(mean(values))
  } else {
    # 如果只有一个测量值，直接返回该值
    return(as.numeric(cell_value))
  }
}

# 对SBP、DBP和pulse列应用上述函数
data <- data %>%
  mutate(
    SBP = sapply(SBP, process_cell),
    DBP = sapply(DBP, process_cell),
  )

# 将结果写入Excel文件
write_xlsx(data, "/Users/shidongping/Desktop/极值分析/【03-1】Take the mean of the SBP and DBP cells.xlsx")
#上Excel文件处理后仍有缺，是因为最开始数据填写不够规范，需要手动补充（最后没有用到SBP和DBP，可以先不关注）
#上述操作不处理也行，就说数据预处理阶段做了处理，所以保留了。

#做表2：
#同一个new_new_ID，按照year从小到大排列，出现的第一次为1（occurance1），第二次为2，以此类推
#对同一个new_new_ID，至少会有2次occurance，
#“occurance相同”意味着occurance整体出现的次数相同，且每一次occurance对应的year也相同
#输出所有“occurance相同”的情况，并输出对应的次数

# 计算每个new_new_ID在不同年份的出现次数
data <- data %>%
  group_by(new_new_ID) %>%
  arrange(new_new_ID, year) %>%
  mutate(occurrence = row_number())

# 为了比较相同occurance的情况，将每个new_new_ID及其对应的occurance和year转换为字符串
occurrence_patterns <- data %>%
  group_by(new_new_ID) %>%
  summarize(pattern = paste(year, collapse = ","))

# 统计相同occurance模式的出现次数
pattern_counts <- occurrence_patterns %>%
  count(pattern) %>%
  arrange(desc(n))

# 筛选出至少有2次occurrence的模式
pattern_counts_filtered <- pattern_counts %>%
  filter(n >= 2)

# 输出结果到控制台
print(pattern_counts_filtered)

# 将结果写入Excel文件
write_xlsx(pattern_counts_filtered, "/Users/shidongping/Desktop/极值分析/【03-2】Show the distribution of exam counts and years.xlsx")
