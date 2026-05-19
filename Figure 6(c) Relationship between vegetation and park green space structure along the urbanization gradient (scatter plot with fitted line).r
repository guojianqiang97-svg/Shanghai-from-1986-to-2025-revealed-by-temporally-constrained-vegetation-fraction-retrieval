library(ggrepel) # 用于处理文字标签防重叠

# 构建表3的数据集
df_table3 <- data.frame(
  District = c("Huangpu", "Xuhui", "Changning", "Jing'an", "Putuo", "Hongkou", "Yangpu", 
               "Pudong", "Minhang", "Baoshan", "Jiading", "Songjiang", 
               "Jinshan", "Qingpu", "Fengxian", "Chongming"),
  Zone = c(rep("Central urban", 7), rep("Inner suburban", 5), rep("Outer suburban", 4)),
  Veg_Proxy = c(14.54, 26.40, 29.65, 22.51, 27.07, 18.83, 24.88, 
                28.47, 27.87, 30.11, 25.59, 25.90, 
                21.79, 22.73, 22.94, 34.76),
  Park_Ratio = c(62.16, 42.53, 46.39, 39.84, 48.52, 36.21, 33.72, 
                 22.52, 27.93, 31.89, 13.19, 8.65, 
                 6.07, 8.57, 6.63, 1.54)
)

# 图6(c): 散点图与线性拟合
p_scatter <- ggplot(df_table3, aes(x = Park_Ratio, y = Veg_Proxy)) +
  geom_point(color = "#3A729E", size = 3) +
  geom_smooth(method = "lm", color = "#1F4E79", fill = "#C6DBEF", level = 0.95) +
  # 标记几个关键的区名 (参考图注说明)
  geom_text_repel(aes(label = ifelse(District %in% c("Chongming", "Huangpu", "Changning", "Pudong", "Baoshan"), District, "")),
                  box.padding = 0.5, size = 4) +
  theme_bw() +
  labs(x = "Park-green ratio (%)", y = "Vegetation-coverage proxy (%)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14)) +
  annotate("text", x = 60, y = 18, label = "r = -0.335\np = 0.204", size = 4)

print(p_scatter)