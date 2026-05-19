library(ggplot2)
library(dplyr)
library(gridExtra)

# --- 图2(a): 1986与2025年端点比较 ---
df_endpoint <- data.frame(
  Indicator = rep(c("NDVI", "FVC"), each = 2),
  Year = rep(c("1986", "2025"), times = 2),
  Value = c(0.50, 0.61, 0.49, 0.59) # 基于论文原文提取
)

p2a <- ggplot(df_endpoint, aes(x = Indicator, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = Value - 0.015, ymax = Value + 0.015), 
                position = position_dodge(width = 0.7), width = 0.15) +
  scale_fill_manual(values = c("#9DC3E6", "#4472C4")) +
  theme_classic() +
  labs(y = "Municipality-wide mean value", x = "", fill = "") +
  ylim(0, 0.8) +
  annotate("text", x = 1, y = 0.65, label = "**\n22.0%", size = 4) +
  annotate("segment", x = 0.825, xend = 1.175, y = 0.63, yend = 0.63) +
  annotate("text", x = 2, y = 0.63, label = "**\n21.4%", size = 4) +
  annotate("segment", x = 1.825, xend = 2.175, y = 0.61, yend = 0.61)

# --- 图2(b): 各阶段FVC均值及斜率标注 ---
df_stages <- data.frame(
  Stage = factor(c("Whole period", "Stage I", "Stage II", "Stage III"), 
                 levels = c("Whole period", "Stage I", "Stage II", "Stage III")),
  Mean = c(0.524, 0.540, 0.502, 0.532),
  Letter = c("ab", "a", "b", "a"),
  Slope_Label = c("Slope = -0.0004\nns", "Slope = -0.0010\nns", "Slope = -0.0045\n**", "Slope = 0.0053\n**")
)

p2b <- ggplot(df_stages, aes(x = Stage, y = Mean)) +
  geom_point(color = "#1F4E79", size = 4) +
  geom_errorbar(aes(ymin = Mean - 0.015, ymax = Mean + 0.015), width = 0.1, color = "#4472C4") +
  geom_text(aes(label = Letter, y = Mean + 0.025), size = 5) +
  geom_text(aes(label = Slope_Label, y = Mean - 0.025), size = 3) +
  theme_bw() +
  labs(y = "Mean FVC", x = "") +
  ylim(0.46, 0.60) +
  theme(panel.grid = element_blank())

# --- 图2(c): 线性斜率比较 (Sen's Slope) ---
df_slopes <- data.frame(
  Stage = factor(c("Whole period", "Stage I", "Stage II", "Stage III"), 
                 levels = c("Whole period", "Stage I", "Stage II", "Stage III")),
  Slope = c(-0.0004, -0.0010, -0.0045, 0.0053),
  Letter = c("b", "b", "c", "a")
)

p2c <- ggplot(df_slopes, aes(x = Stage, y = Slope)) +
  geom_bar(stat = "identity", width = 0.6, fill = c("#9DC3E6", "#7cb5ec", "#4472C4", "#1F4E79"), color = "black") +
  geom_errorbar(aes(ymin = Slope - 0.0007, ymax = Slope + 0.0007), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(aes(label = Letter, y = ifelse(Slope > 0, Slope + 0.0012, Slope - 0.0012)), size = 5) +
  theme_classic() +
  labs(y = "Linear slope of mean FVC", x = "") +
  annotate("text", x = 1, y = 0.007, label = "ANOVA: F = 61.8, p < 0.001", size = 4, hjust = 0)


grid.arrange(p2a, p2b, p2c, ncol = 2)