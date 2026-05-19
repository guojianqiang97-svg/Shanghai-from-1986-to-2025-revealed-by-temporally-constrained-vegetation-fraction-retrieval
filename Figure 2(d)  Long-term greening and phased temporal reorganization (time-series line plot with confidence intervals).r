# 构建模拟的时序数据结构
set.seed(123)
years <- 1986:2025
mean_fvc <- seq(0.49, 0.59, length.out = length(years)) + sin(seq(0, 2*pi, length.out = length(years))) * 0.05
ci_upper <- mean_fvc + 0.015
ci_lower <- mean_fvc - 0.015

df_ts <- data.frame(Year = years, Mean_FVC = mean_fvc, Upper = ci_upper, Lower = ci_lower)

# 图2(d): 时序折线图
p_timeseries <- ggplot(df_ts, aes(x = Year, y = Mean_FVC)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "#C6DBEF", alpha = 0.6) + # 95% uncertainty band
  geom_line(color = "#1F4E79", linewidth = 1) +
  geom_point(color = "#1F4E79", size = 1.5) +
  # 添加阶段性断点虚线段 (假设断点在2000和2012年)
  geom_vline(xintercept = c(2000, 2012), linetype = "dashed", color = "#4472C4") +
  annotate("text", x = 2000, y = max(df_ts$Upper), label = "Breakpoint", vjust = -0.5, size = 3) +
  annotate("text", x = 2012, y = max(df_ts$Upper), label = "Breakpoint", vjust = -0.5, size = 3) +
  theme_bw() +
  labs(x = "Year", y = "Mean FVC") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14))

print(p_timeseries)