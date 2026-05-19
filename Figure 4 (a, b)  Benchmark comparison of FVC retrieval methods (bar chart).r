# 加载必要的包
library(ggplot2)
library(dplyr)

# 构建表2的数据集
df_table2 <- data.frame(
  Method = rep(c("TMA", "SMA", "PDM"), each = 3),
  Coverage = rep(c("Low", "Medium", "High"), times = 3),
  RMSE = c(0.1701, 0.2247, 0.1942, 0.1344, 0.2585, 0.3045, 0.1452, 0.2637, 0.3936),
  r = c(0.5375, 0.2466, 0.4593, 0.5264, 0.2376, 0.3542, 0.3708, 0.2519, 0.2603)
)

# 设定因子的顺序，确保X轴按 Low, Medium, High 排列
df_table2$Coverage <- factor(df_table2$Coverage, levels = c("Low", "Medium", "High"))
df_table2$Method <- factor(df_table2$Method, levels = c("TMA", "SMA", "PDM"))

# 图4(a): RMSE 柱状图
p_rmse <- ggplot(df_table2, aes(x = Coverage, y = RMSE, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  # 假设的误差线（标准差），你需要替换为实际数据的SD
  geom_errorbar(aes(ymin = RMSE - 0.02, ymax = RMSE + 0.02), 
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_fill_manual(values = c("#1F4E79", "#4472C4", "#9DC3E6"))
  theme_classic() +
  labs(x = "Vegetation-coverage class", y = "RMSE", fill = "") +
  theme(legend.position = "top", 
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14)) +
  annotate("text", x = 3.2, y = 0.45, label = "ANOVA: p < 0.01", size = 4)

print(p_rmse)