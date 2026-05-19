# 基于表3数据计算各分区的均值和标准差
df_zone_summary <- data.frame(
  Zone = factor(c("Central urban", "Inner suburb", "Outer suburb"), 
                levels = c("Central urban", "Inner suburb", "Outer suburb")),
  Veg_Mean = c(23.41, 27.59, 25.55),
  Veg_SD = c(5.12, 1.83, 5.86), # 依据数据的SD值
  Veg_Letter = c("b", "a", "ab"),
  Park_Mean = c(44.20, 20.84, 5.70),
  Park_SD = c(9.51, 9.78, 2.87), # 依据数据的SD值
  Park_Letter = c("a", "b", "c")
)

# --- 图6(a): 植被覆盖度代理指标 ---
p6a <- ggplot(df_zone_summary, aes(x = Zone, y = Veg_Mean)) +
  geom_bar(stat = "identity", width = 0.6, fill = c("#1F4E79", "#4472C4", "#9DC3E6"), color = "black") +
  geom_errorbar(aes(ymin = Veg_Mean - Veg_SD, ymax = Veg_Mean + Veg_SD), width = 0.15) +
  geom_text(aes(label = Veg_Letter, y = Veg_Mean + Veg_SD + 1.5), size = 5) +
  theme_bw() +
  labs(y = "Vegetation-coverage proxy (%)", x = "") +
  theme(panel.grid.major.x = element_blank()) +
  annotate("text", x = 3, y = 33, label = "ANOVA: p < 0.10\nKruskal-Wallis: p < 0.10", size = 3.5, hjust = 0.5)

# --- 图6(b): 公园绿地占比 ---
p6b <- ggplot(df_zone_summary, aes(x = Zone, y = Park_Mean)) +
  geom_bar(stat = "identity", width = 0.6, fill = c("#1F4E79", "#4472C4", "#9DC3E6"), color = "black") +
  geom_errorbar(aes(ymin = Park_Mean - Park_SD, ymax = Park_Mean + Park_SD), width = 0.15) +
  geom_text(aes(label = Park_Letter, y = Park_Mean + Park_SD + 2.5), size = 5) +
  theme_bw() +
  labs(y = "Park-green ratio (%)", x = "") +
  theme(panel.grid.major.x = element_blank()) +
  annotate("text", x = 3, y = 58, label = "ANOVA: p < 0.001\nKruskal-Wallis: p < 0.01", size = 3.5, hjust = 0.5)

grid.arrange( )