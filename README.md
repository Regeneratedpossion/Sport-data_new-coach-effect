# Sport data_new coach effect

library(readxl)

# 读取数据
df <- read_excel("/Users/jiahuifan/Downloads/data_shooting_coach.xlsx")



#---------------检查变量名与数据结构----------------
# 查看前几行
head(df)

# 查看数据结构
# 查看列名
names(df)

# 查看变量类型
str(df)

# 查看维度（多少行多少列）
dim(df)

# 快速摘要
summary(df)


#---------------检查缺失值----------------
# 每列缺失值数量
colSums(is.na(df))

# 哪些行有缺失值
df[!complete.cases(df), ]


#---------------检查异常值----------------

# 加载必要的库
library(dplyr)
library(ggplot2)
library(tidyr)


# --------------------1. 检查每个数值变量的异常值：Z-score-------------------
numeric_columns <- c("GF", "GA", "Gls_Standard", "Sh_Standard", "SoT_Standard",
                     "SoT_percent_Standard", "G_per_Sh_Standard", "G_per_SoT_Standard",
                     "Dist_Standard", "PK_Standard", "PKatt_Standard", "FK_Standard",
                     "xG_Expected", "npxG_Expected", "npxG_per_Sh_Expected", "G_minus_xG_Expected", 
                     "np:G_minus_xG_Expected", "elo_pre", "opp_elo_pre")


# 对每列计算 Z-score 并统计异常值数量
zscore_outliers <- sapply(numeric_columns, function(var) {
  x <- df[[var]]
  x_z <- scale(x)  # 计算 Z-score
  sum(abs(x_z) > 3, na.rm = TRUE)  # 统计异常值个数
})

# 展示异常值数量表格
zscore_outliers <- sort(zscore_outliers, decreasing = TRUE)
zscore_outliers



# ------------3. 检查 `coach` 是否只有 0 和 1------------
table(df$coach)  # 包含了 教练名字 的字符串变量，如果需要建模，那么需要转换成0，1的dummy变量

# --------------4. 检查 `Result` 和 `ForAgainst` 变量的类别----------
table(df$Result)##输赢平场次，可以用于得分变量 Points：3，1，0
table(df$ForAgainst)

# ---------------5. 检查 `elo_pre` 是否在一个合理范围内（如果有其他异常值标准可修改）------------
summary(df$elo_pre)




# ---------------7.构造换帅变量--------------

library(dplyr)
library(lubridate)

# 确保你的 Date 列是日期格式
df <- df %>%
  mutate(Date = as.Date(Date))

# 按球队 + 日期排序，并创建教练变化标记
df <- df %>%
  arrange(Team, Date) %>%
  group_by(Team) %>%
  mutate(
    coach_prev = lag(coach),
    coach_change = if_else(coach != coach_prev, 1, 0),
    coach_change = replace_na(coach_change, 0)
  ) %>%
  ungroup()

# ------------8.异质性计算（基于OLS模型的需要，同时用一个自变量概括多个自变量可以减少列数）-----------------
library(dplyr)
library(zoo)

performance_vars <- c("Sh_Standard", "SoT_Standard", "xG_Expected")

# 定义一个函数：对单支球队计算滚动异质性
compute_rolling_hetero <- function(df_team) {
  df_team <- df_team %>%
    arrange(Date)

  # 提取表现子集
  perf_mat <- df_team %>% select(all_of(performance_vars)) %>% as.matrix()

  # 计算滚动窗口的标准差均值
  hetero_vec <- rollapply(
    data = perf_mat,
    width = 5,
    FUN = function(x) mean(apply(x, 2, sd, na.rm = TRUE)),
    by.column = FALSE,
    fill = NA,
    align = "right"
  )

  df_team$heterogeneity <- hetero_vec
  return(df_team)
}

# 按球队分组处理
df <- df %>%
  group_by(Team) %>%
  group_modify(~ compute_rolling_hetero(.x)) %>%
  ungroup()

#----------------9.是否需要继续概括变量----------------
##
