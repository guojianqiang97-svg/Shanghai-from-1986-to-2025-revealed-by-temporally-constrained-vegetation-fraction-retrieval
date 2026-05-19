# --- 图4(c): 跨覆盖度级别的平均RMSE ---
df_mean_rmse <- data.frame(
  Method = factor(c("TMA", "SMA", "PDM"), levels = c("TMA", "SMA", "PDM")),
  Mean_RMSE = c(0.1963, 0.2325, 0.2675), # 近似值计算自Table 2
  SD = c(0.027, 0.088, 0.125), # 估算的跨类标准差
  Letter = c("b", "ab", "a")
)

p4c <- ggplot(df_mean_rmse, aes(x = Method, y = Mean_RMSE, fill = Method)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = Mean_RMSE - SD, ymax = Mean_RMSE + SD), width = 0.15) +
  geom_text(aes(label = Letter, y = Mean_RMSE + SD + 0.02), size = 5) +
  scale_fill_manual(values = c("#1F4E79", "#4472C4", "#9DC3E6")) +
  theme_classic() +
  labs(y = "Mean RMSE across coverage classes", x = "") +
  theme(legend.position = "none") +
  annotate("text", x = 1, y = 0.38, label = "One-way ANOVA\np < 0.05", size = 4, hjust = 0)

# --- 图4(d): 观测值 vs 估算值 散点图 ---
# 生成模拟验证样本点 (需替换为你提炼的300个验证样本数据)
set.seed(42)
obs_fvc <- runif(150, 0, 1)
df_scatter <- data.frame(
  Observed = rep(obs_fvc, 3),
  Estimated = c(obs_fvc * 0.95 + 0.01 + rnorm(150, 0, 0.05), # TMA
                obs_fvc * 0.75 + 0.07 + rnorm(150, 0, 0.15), # SMA
                obs_fvc * 0.75 + 0.09 + rnorm(150, 0, 0.20)),# PDM
  Method = rep(c("TMA", "SMA", "PDM"), each = 150)
)
df_scatter$Method <- factor(df_scatter$Method, levels = c("TMA", "SMA", "PDM"))

p4d <- ggplot(df_scatter, aes(x = Observed, y = Estimated, color = Method, shape = Method)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + # 1:1线
  scale_color_manual(values = c("#1F4E79", "#8FAADC", "#B4C6E7")) +
  scale_shape_manual(values = c(16, 18, 17)) +
  theme_bw() +
  labs(x = "Observed FVC", y = "Estimated FVC") +
  coord_fixed(ratio = 1) + xlim(-0.05, 1.05) + ylim(-0.05, 1.05) +
  annotate("text", x = 0, y = 0.95, label = "TMA: y = 0.95x + 0.01 ***, R^2 = 0.65", hjust = 0, size = 3.5) +
  annotate("text", x = 0, y = 0.88, label = "SMA: y = 0.75x + 0.07 ***, R^2 = 0.41", hjust = 0, size = 3.5) +
  annotate("text", x = 0, y = 0.81, label = "PDM: y = 0.75x + 0.09 ***, R^2 = 0.40", hjust = 0, size = 3.5) +
  theme(legend.position = "bottom", panel.grid = element_blank())

print(p4d)