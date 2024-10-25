
############################  simple mediation analysis  ############################

#自变量：知识结构CPT_SUM，因变量：SPF（sum），中介变量：神经数据 不同脑区的TF Gpower

rm(list = ls())

# get the current fold path 
getwd()  # "/Users/chaoqun_ye/Documents/Coding/R/ANOVA_YE"
# setwd()


library(readxl)
#step1:打开excel数据
file_path <- '/Users/chaoqun_ye/Documents/File/ye/Doctor Dissertation/yj1/data_demo4/zhongjie_test_data_6_finnal2.xlsx'
data<- read_excel(file_path)

str(data)  #查看数据

###step2 检查和删除缺失值
anyNA <- any(is.na(data)) 
anyNA

# class(data)


# # remove the NA value  
# data_cor <- na.omit(data_cor)
# 
# anyNA2 <- any(is.na(data_cor))  
# print(anyNA2)

data_cor <- data[,-c(1:10)]

str(data_cor)



# 将现有的数据框中的SPF的分维度加起来，作为SPF衡量的指标

# library(dplyr)


# data_cor <- data_cor %>%
#   mutate(SPF_SUM= (SPF_FLU + SPF_ORI_Rater + SPF_ORI_Weighted + SPF_FLX),
#   )


str(data_cor)


#------cor-----

# load the needed libraries
library(corrplot)

# the correlation matrix
# cor(x, y = NULL, use = "everything",
#     method = c("pearson", "kendall", "spearman"))cor <- cor(data_cor2, method="pearson")
cor <- cor(data_cor, method = "pearson")

# use the corrplot() to draw the correlation heatmap 绘制相关性的热图
# ?corrplot
corrplot(cor) # 全矩阵
corrplot(cor, type = "lower",tl.cex = 0.3) # 下三角矩阵
corrplot(cor, type = "upper",tl.cex = 0.3) # 上三角矩阵

corrplot(cor, type = "upper") # 上三角矩阵

# cor_sanjiaojuzhen <- corrplot(cor, type = "upper") # 上三角矩阵
# cor_sanjiaojuzhen

#######################################################################################
# # TIFF格式 图片保存
# 
# # 论文投稿常用的位图格式为TIFF格式，在保存图形时如果需要输出位图，建议保存为TIFF格式。
# # 在使用tiff()函数时，可以调整函数中的参数来调整输出图形，比如设置宽度width、高度height、图片压缩类型compression等。
# 
# # 创建画布
# tiff(filename = "/Users/chaoqun_ye/Desktop/cor_matrix_plot.tif",
#      width = 480, height = 480, units = "px", pointsize = 12,
#      # compression = c("none", "rle", "lzw", "jpeg", "zip", "lzw+p", "zip+p"),
     # bg = "white", res = 300, family = "",
#      # type = c( “cairo”, “Xlib”, “quartz”), 
#      type =  "quartz", 
#      antialias = "default")
# # # 部分参数解释
# # filename # 要输出图形的文件名称
# # width = 480 # 设置输出图形的宽度
# # height = 480 # 设置输出图形的高度
# # units = "px" # 设置宽度和高度的单位，默认px(像素)，其余有英寸in、厘米cm、毫米mm。
# # pointsize = 12 # 图形上文字的大小，默认为12
# # compression  # 设置输出图形的压缩类型
# # # 可选择的压缩类型有"none", "rle", "lzw", "jpeg", "zip", "lzw+p", "zip+p"
# # # tiff格式图形建议选择lzw压缩
# # bg = "white" # 设置图形背景色
# # res = NA  # 设置导出图形的分辨率，默认72ppi。
# # family = ""  # 设置图形上文字的字体
# 
# 
# # Create your plot here
# corrplot(cor, type = "upper") 
# 
# # Close the device
# dev.off()
#######################################################################################


#  计算相关系数及其相关显著性水平p-value
library(Hmisc)
correlation <- rcorr(as.matrix(data_cor))

# 可以用 相关系数名称$r、来提取相关系数, 相关系数名称$P来提取显著性p-value
correlation$r  # 相关矩阵 Extract the correlation coefficients
correlation$P # 显著性水平 Extract p-values

# 获取相关系数矩阵和显著性矩阵
cor_matrix <- correlation$r
p_matrix <- correlation$P
class(cor_matrix)
class(p_matrix)

#  构建一个以"row,column,cor,p"为列名的矩阵提取函数
corMatrix <- function(cor,p){
  ut <- upper.tri(cor)
  data.frame(row = rownames(cor)[row(cor)[ut]],
             column = rownames(cor)[col(cor)[ut]],
             cor = (cor)[ut],
             p = p[ut])
}

correlation <- corMatrix(correlation$r,correlation$P)
head(correlation)

# 存储
# write.table(correlation,file = "CPT_SHG_TF_power_correlation5_test.csv",
#             row.names = F,sep = ",")


# plot the correlation map

# 推荐1
library(PerformanceAnalytics)
chart.Correlation(data_cor, histogram=TRUE, pch=10) ## 信息最全1
# One of "pearson" (default), "kendall", or "spearman", can be abbreviated.
# ??chart.Correlation

# 推荐2
library(GGally)
# ggpairs(iris,aes(color=Species))
ggpairs(data_cor)          ## 信息次全2
cor_plot <- ggpairs(data_cor) 
cor_plot
`# ??ggpairs

ggsave("/Users/chaoqun_ye/Desktop/cor_plot_shuxing.tiff", plot = cor_plot, dpi = 300)

# #  plotly  packages
# library(plotly)
# # ?chart.Boxplot
# chart.Boxplot(data_cor,plot.engine = "plotly")
# # chart.Boxplot(data_cor,plot.engine = "ggplot2")





# 
# 使用散点图可视化您的数据

library(ggpubr)
library(ggplot2)
library(ggExtra)
library(hrbrthemes)


str(data_cor)

# Basic scatter plot.
ggplot(data_cor, aes(x=CPT_SUM, y=SPF_SUM)) + 
  geom_point( color="#69b3a2") +
  theme_ipsum()

# with linear trend
ggplot(data_cor, aes(x=CPT_SUM, y=SPF_SUM)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()

# linear trend + confidence interval
ggplot(data_cor, aes(x=CPT_SUM, y=SPF_SUM)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

# 
ggscatter(data_cor, x = "CPT_SUM", y = "SPF_SUM", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Knowledge Structure (total score of CPT task) ", ylab = "Problem-Finding Ability",
          color = "#69b3a2",                # 设置散点的颜色
          add.params = list(color = "red", fill = "lightgray"))  # 设置拟合线和置信区间颜色)


ggscatter(data_cor, x = "CPT_SUM", y = "frontal_6_10hz", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Knowledge Structure (total score of CPT task) ", ylab = "Frontal Neural Oscillatory Activation")



# classic plot :


p <- ggplot(data_cor, aes(x=CPT_SUM, y=SPF_SUM)) + 
  geom_point() +
  theme(legend.position="none")
p




# with marginal histogram
p1 <- ggMarginal(p, type="histogram")
p1

# marginal density
p2 <- ggMarginal(p, type="density")
p2

# marginal boxplot
p3 <- ggMarginal(p, type="boxplot")
p3

## 
p_test <- ggplot(data_cor, aes(x=CPT_SUM, y=SPF_SUM)) +
  geom_point() +
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  # theme_ipsum()
  theme(legend.position="none")

p_test

# with marginal histogram
p_test1 <- ggMarginal(p_test, type="histogram")
p_test1

# marginal density
p_test2 <- ggMarginal(p_test, type="density")
p_test2



#######################################################################################

# piris <- ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
#   geom_point()
# ggMarginal(piris, groupColour = TRUE, groupFill = TRUE)

str(data)  #查看数据

data$Knowledge_Structure_Group <- as.factor(data$Knowledge_Structure_Group)


p2_test <- ggplot(data, aes(x=CPT_SUM, y=SPF_SUM, colour = Knowledge_Structure_Group)) +
  geom_point() +
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  theme(legend.position = "bottom")   # 将图例移到顶部，使用 "top"
  # theme_ipsum()
  # theme(legend.position="none")

p2_test


p2_test1 <- ggplot(data, aes(x=CPT_SUM, y=SPF_SUM, colour = Knowledge_Structure_Group)) +
  geom_point() +
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  theme(legend.position = "bottom") +  # 将图例移到顶部，使用 "top"
  # theme_ipsum()
  # theme(legend.position="none")

  scale_y_continuous(breaks = seq(0, 1800, by = 200))  # 调整y轴刻度分辨率

p2_test1



# ggMarginal(p2_test, groupColour = TRUE, groupFill = TRUE)


ggMarginal(p2_test, groupColour = TRUE, groupFill = TRUE,
           type = "density",  # 使用密度图
           margins = "both",  # 在顶部和右侧添加密度图
           size = 5,  # 设置边缘密度图的大小
           adjust = 1.5,  # 调整密度曲线的平滑度，数值越大曲线越平滑
           bw = "nrd0",  # 使用默认的带宽估计法（可以尝试 如 sj、bcv 等其他方法，以获取不同的曲线形状法）
           alpha = 0.5,  # 设置透明度，0为完全透明，1为不透明
           colour = "black",  # 边缘密度曲线的颜色
           fill = "lightblue")  # 密度曲线的填充颜色


###############################################################################

p2_test2 <- ggplot(data, aes(x = CPT_SUM, y = SPF_SUM, colour = Knowledge_Structure_Group)) + 
  geom_point() + 
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE) +
  theme_minimal() +  # 使用最小化主题去除背景
  theme(
    legend.position = "bottom",  # 图例位置
    panel.border = element_rect(colour = "black", fill=NA, size=1), # 在散点图外部添加黑色框
    # axis.ticks = element_blank(),  # 去除x、y轴的刻度线
    axis.ticks = element_line(),  # 显示x、y轴的刻度线
    panel.grid = element_blank()  # 去除网格线
  ) +
  # scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1800, by = 200))   # y轴从0开始 调整y轴刻度分辨率
  scale_y_continuous(breaks = seq(0, 1800, by = 200))   # 调整y轴刻度分辨率 
  # scale_x_continuous(limits = c(0, NA))  # x轴从0开始

p2_test2


ggMarginal(p2_test2, groupColour = TRUE, groupFill = TRUE,
           type = "density",  # 使用密度图
           margins = "both",  # 在顶部和右侧添加密度图
           size = 5,  # 设置边缘密度图的大小
           adjust = 1.5,  # 调整密度曲线的平滑度，数值越大曲线越平滑
           bw = "nrd0",  # 使用默认的带宽估计法（可以尝试 如 sj、bcv 等其他方法，以获取不同的曲线形状法）
           alpha = 0.5,  # 设置透明度，0为完全透明，1为不透明
           colour = "black",  # 边缘密度曲线的颜色
           fill = "lightblue")  # 密度曲线的填充颜色




###############################################################################

p2_test3 <- ggplot(data, aes(x = CPT_SUM, y = SPF_SUM, colour = Knowledge_Structure_Group)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = y ~ x, color = "black", fill = "lightgray", se = TRUE) +
  labs(x = " Knowledge Structure Score", y = "Problem-Finding Ability") +    # 设置 x 和 y 轴的名称
  theme_minimal() +     # 使用最小化主题去除背景
  theme(
    legend.position = "bottom",  # 图例位置
    panel.border = element_rect(colour = "black", fill=NA, size=2), # 在散点图外部添加黑色框
    axis.ticks = element_line(),    # 显示x、y轴的刻度线
    panel.grid = element_blank(),   # 去除网格线
    axis.title.x = element_text(size = 14,  family = "Times New Roman", face = "bold"),  # 调整 x 轴标签的字体大小和字体样式
    axis.title.y = element_text(size = 14,  family = "Times New Roman", face = "bold")   # 调整 y 轴标签的字体大小和字体样式
  ) +
  # scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1800, by = 200))   # y轴从0开始 调整y轴刻度分辨率
  scale_y_continuous(breaks = seq(200, 1800, by = 200))   # 调整y轴刻度分辨率
  


p2_test3


ggMarginal(p2_test3, groupColour = TRUE, groupFill = TRUE,
           type = "density",  # 使用密度图
           margins = "both",  # 在顶部和右侧添加密度图
           size = 5,  # 设置边缘密度图的大小
           adjust = 1.5,  # 调整密度曲线的平滑度，数值越大曲线越平滑
           bw = "nrd0",  # 使用默认的带宽估计法（可以尝试 如 sj、bcv 等其他方法，以获取不同的曲线形状法）
           alpha = 0.5,  # 设置透明度，0为完全透明，1为不透明
           colour = "black",  # 边缘密度曲线的颜色
           fill = "lightblue")  # 密度曲线的填充颜色




####################################### x = CPT_SUM, y = SPF_SUM ########################################

p2_test4 <- ggplot(data, aes(x = CPT_SUM, y = SPF_SUM, colour = Knowledge_Structure_Group)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = y ~ x, color = "black", fill = "lightgray", se = TRUE) +
  labs(x = " Knowledge Structure Score", y = "Problem-Finding Ability") +    # 设置 x 和 y 轴的名称
  theme_minimal() +     # 使用最小化主题去除背景
  theme(
    legend.position = "bottom",  # 图例位置
    panel.border = element_rect(colour = "black", fill=NA, size=2), # 在散点图外部添加黑色框
    axis.ticks = element_line(),    # 显示x、y轴的刻度线
    panel.grid = element_blank(),   # 去除网格线
    axis.title.x = element_text(size = 14, # family = "Times New Roman",
                                face = "bold"),  # 调整 x 轴标签的字体大小和字体样式
    axis.title.y = element_text(size = 14, # family = "Times New Roman", 
                                face = "bold")   # 调整 y 轴标签的字体大小和字体样式
  ) +
  

  stat_cor(
    method = "pearson", 
    label.x = 180, label.y = 1300,
    size = 5, # family = "Times New Roman", 
    color = "black", parse = TRUE  # 调整字体大小、颜色和解析格式
  ) + 

  # scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1800, by = 200))   # y轴从0开始 调整y轴刻度分辨率
  scale_y_continuous(breaks = seq(200, 1800, by = 200))   # 调整y轴刻度分辨率



p2_test4


p2_test4_density <- ggMarginal(p2_test4, groupColour = TRUE, groupFill = TRUE,
           type = "density",  # 使用密度图
           margins = "both",  # 在顶部和右侧添加密度图
           size = 5,  # 设置边缘密度图的大小
           adjust = 1.5,  # 调整密度曲线的平滑度，数值越大曲线越平滑
           bw = "nrd0",  # 使用默认的带宽估计法（可以尝试 如 sj、bcv 等其他方法，以获取不同的曲线形状法）
           alpha = 0.5,  # 设置透明度，0为完全透明，1为不透明
           colour = "black",  # 边缘密度曲线的颜色
           fill = "lightblue")  # 密度曲线的填充颜色

p2_test4_density

# ggsave("/Users/chaoqun_ye/Desktop/KS__problem-finding ability.tiff", plot = p2_test4_density, dpi = 300)

ggsave("/Users/chaoqun_ye/Desktop/KS__problem-finding ability.pdf", plot = p2_test4_density, dpi = 300)





####################################### x = CPT_SUM, y = fronto_central_4_10hz ########################################

str(data)

p2_test5 <- ggplot(data, aes(x = CPT_SUM, y = fronto_central_4_10hz, colour = Knowledge_Structure_Group)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = y ~ x, color = "black", fill = "lightgray", se = TRUE) +
  # labs(x = " Knowledge Structure Score", y = "The Neural Oscillatory Activation of Fronto-central ") +    # 设置 x 和 y 轴的名称
  labs(x = " Knowledge Structure Score", y = "Fronto-central (4-10 Hz)") +    # 设置 x 和 y 轴的名称
  theme_minimal() +     # 使用最小化主题去除背景
  theme(
    legend.position = "bottom",  # 图例位置
    panel.border = element_rect(colour = "black", fill=NA, size=2), # 在散点图外部添加黑色框
    axis.ticks = element_line(),    # 显示x、y轴的刻度线
    panel.grid = element_blank(),   # 去除网格线
    axis.title.x = element_text(size = 14,  # family = "Times New Roman", 
                                face = "bold"),  # 调整 x 轴标签的字体大小和字体样式
    axis.title.y = element_text(size = 14,  # family = "Times New Roman", 
                                face = "bold")   # 调整 y 轴标签的字体大小和字体样式
  ) + 
  
  
  stat_cor(
    method = "pearson",
    label.x = 180, label.y = 3.6,
    size = 5, # family = "Times New Roman", 
    color = "black", parse = TRUE  # 调整字体大小、颜色和解析格式
  ) + 
  # 
  # scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1800, by = 200))   # y轴从0开始 调整y轴刻度分辨率
  scale_y_continuous(breaks = seq(-2, 6, by = 0.5))   # 调整y轴刻度分辨率



p2_test5


p2_test5_density <- ggMarginal(p2_test5, groupColour = TRUE, groupFill = TRUE,
           type = "density",  # 使用密度图
           margins = "both",  # 在顶部和右侧添加密度图
           size = 5,  # 设置边缘密度图的大小
           adjust = 1.5,  # 调整密度曲线的平滑度，数值越大曲线越平滑
           bw = "nrd0",  # 使用默认的带宽估计法（可以尝试 如 sj、bcv 等其他方法，以获取不同的曲线形状法）
           alpha = 0.5,  # 设置透明度，0为完全透明，1为不透明
           colour = "black",  # 边缘密度曲线的颜色
           fill = "lightblue")  # 密度曲线的填充颜色

p2_test5_density

# getwd()

# ggsave("/Users/chaoqun_ye/Desktop/KS_Frontal-central.tiff", plot = p2_test5_density, dpi = 300)
ggsave("/Users/chaoqun_ye/Desktop/KS_Frontal-central.pdf", plot = p2_test5_density, dpi = 300)




####################################### x = CPT_SUM, y = parietal_6_14hz ########################################


str(data)

p2_test6 <- ggplot(data, aes(x = CPT_SUM, y = parietal_6_14hz, colour = Knowledge_Structure_Group)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = y ~ x, color = "black", fill = "lightgray", se = TRUE) +
  # labs(x = " Knowledge Structure Score", y = "The Neural Oscillatory Activation of Fronto-central ") +    # 设置 x 和 y 轴的名称
  labs(x = " Knowledge Structure Score", y = "Parietal (6-14 Hz)") +    # 设置 x 和 y 轴的名称
  theme_minimal() +     # 使用最小化主题去除背景
  theme(
    legend.position = "bottom",  # 图例位置
    panel.border = element_rect(colour = "black", fill=NA, size=2), # 在散点图外部添加黑色框
    axis.ticks = element_line(),    # 显示x、y轴的刻度线
    panel.grid = element_blank(),   # 去除网格线
    axis.title.x = element_text(size = 14,  # family = "Times New Roman", 
                                face = "bold"),  # 调整 x 轴标签的字体大小和字体样式
    axis.title.y = element_text(size = 14,  # family = "Times New Roman", 
                                face = "bold")   # 调整 y 轴标签的字体大小和字体样式
  ) + 
  
  
  stat_cor(
    method = "pearson",
    label.x = 180, label.y = 2.8,
    size = 5, # family = "Times New Roman", 
    color = "black", parse = TRUE  # 调整字体大小、颜色和解析格式
  ) + 
  # 
  # scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1800, by = 200))   # y轴从0开始 调整y轴刻度分辨率
  scale_y_continuous(breaks = seq(-5, 5, by = 0.5))   # 调整y轴刻度分辨率



p2_test6


p2_test6_density <- ggMarginal(p2_test6, groupColour = TRUE, groupFill = TRUE,
                               type = "density",  # 使用密度图
                               margins = "both",  # 在顶部和右侧添加密度图
                               size = 5,  # 设置边缘密度图的大小
                               adjust = 1.5,  # 调整密度曲线的平滑度，数值越大曲线越平滑
                               bw = "nrd0",  # 使用默认的带宽估计法（可以尝试 如 sj、bcv 等其他方法，以获取不同的曲线形状法）
                               alpha = 0.5,  # 设置透明度，0为完全透明，1为不透明
                               colour = "black",  # 边缘密度曲线的颜色
                               fill = "lightblue")  # 密度曲线的填充颜色

p2_test6_density

# getwd()

# ggsave("/Users/chaoqun_ye/Desktop/KS_Parietal.tiff", plot = p2_test6_density, dpi = 300)

ggsave("/Users/chaoqun_ye/Desktop/KS_Parietal.pdf", plot = p2_test6_density, dpi = 300)







####################################### x = SPF_SUM, y = fronto_central_4_10hz ########################################

str(data)

p2_test7 <- ggplot(data, aes(x = SPF_SUM, y = fronto_central_4_10hz, colour = Knowledge_Structure_Group)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = y ~ x, color = "black", fill = "lightgray", se = TRUE) +
  # labs(x = " Knowledge Structure Score", y = "The Neural Oscillatory Activation of Fronto-central ") +    # 设置 x 和 y 轴的名称
  labs(x = " Problem-Finding Ability ", y = "Fronto-central (4-10 Hz)") +    # 设置 x 和 y 轴的名称
  theme_minimal() +     # 使用最小化主题去除背景
  theme(
    legend.position = "bottom",  # 图例位置
    panel.border = element_rect(colour = "black", fill=NA, size=2), # 在散点图外部添加黑色框
    axis.ticks = element_line(),    # 显示x、y轴的刻度线
    panel.grid = element_blank(),   # 去除网格线
    axis.title.x = element_text(size = 14,  # family = "Times New Roman", 
                                face = "bold"),  # 调整 x 轴标签的字体大小和字体样式
    axis.title.y = element_text(size = 14,  # family = "Times New Roman", 
                                face = "bold")   # 调整 y 轴标签的字体大小和字体样式
  ) + 
  
  
  stat_cor(
    method = "pearson",
    label.x = 180, label.y = 3.6,
    size = 5, # family = "Times New Roman", 
    color = "black", parse = TRUE  # 调整字体大小、颜色和解析格式
  ) + 
  # 
  # scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1800, by = 200))   # y轴从0开始 调整y轴刻度分辨率
  scale_y_continuous(breaks = seq(-2, 6, by = 0.5)) +  # 调整y轴刻度分辨率
  scale_x_continuous(breaks = seq(200, 1800, by = 200))



p2_test7


p2_test7_density <- ggMarginal(p2_test7, groupColour = TRUE, groupFill = TRUE,
                               type = "density",  # 使用密度图
                               margins = "both",  # 在顶部和右侧添加密度图
                               size = 5,  # 设置边缘密度图的大小
                               adjust = 1.5,  # 调整密度曲线的平滑度，数值越大曲线越平滑
                               bw = "nrd0",  # 使用默认的带宽估计法（可以尝试 如 sj、bcv 等其他方法，以获取不同的曲线形状法）
                               alpha = 0.5,  # 设置透明度，0为完全透明，1为不透明
                               colour = "black",  # 边缘密度曲线的颜色
                               fill = "lightblue")  # 密度曲线的填充颜色

p2_test7_density

# getwd()

# ggsave("/Users/chaoqun_ye/Desktop/SPF_Frontal-central.tiff", plot = p2_test7_density, dpi = 300)

ggsave("/Users/chaoqun_ye/Desktop/SPF_Frontal-central.pdf", plot = p2_test7_density, dpi = 300)




####################################### x = SPF_SUM, y = parietal_6_14hz ########################################


str(data)

p2_test8 <- ggplot(data, aes(x = SPF_SUM, y = parietal_6_14hz, colour = Knowledge_Structure_Group)) + 
  geom_point() + 
  geom_smooth(method = lm, formula = y ~ x, color = "black", fill = "lightgray", se = TRUE) +
  # labs(x = " Knowledge Structure Score", y = "The Neural Oscillatory Activation of Fronto-central ") +    # 设置 x 和 y 轴的名称
  labs(x = " Problem-Finding Ability ", y = "Parietal (6-14 Hz)") +    # 设置 x 和 y 轴的名称
  theme_minimal() +     # 使用最小化主题去除背景
  theme(
    legend.position = "bottom",  # 图例位置
    panel.border = element_rect(colour = "black", fill=NA, size=2), # 在散点图外部添加黑色框
    axis.ticks = element_line(),    # 显示x、y轴的刻度线
    panel.grid = element_blank(),   # 去除网格线
    axis.title.x = element_text(size = 14,  # family = "Times New Roman", 
                                face = "bold"),  # 调整 x 轴标签的字体大小和字体样式
    axis.title.y = element_text(size = 14,  # family = "Times New Roman", 
                                face = "bold")   # 调整 y 轴标签的字体大小和字体样式
  ) + 
  
  
  stat_cor(
    method = "pearson",
    label.x = 180, label.y = 2.8,
    size = 5, # family = "Times New Roman", 
    color = "black", parse = TRUE  # 调整字体大小、颜色和解析格式
  ) + 
  # 
  # scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1800, by = 200))   # y轴从0开始 调整y轴刻度分辨率
  scale_y_continuous(breaks = seq(-5, 5, by = 0.5)) +  # 调整y轴刻度分辨率
  scale_x_continuous(breaks = seq(200, 1800, by = 200))


p2_test8


p2_test8_density <- ggMarginal(p2_test8, groupColour = TRUE, groupFill = TRUE,
                               type = "density",  # 使用密度图
                               margins = "both",  # 在顶部和右侧添加密度图
                               size = 5,  # 设置边缘密度图的大小
                               adjust = 1.5,  # 调整密度曲线的平滑度，数值越大曲线越平滑
                               bw = "nrd0",  # 使用默认的带宽估计法（可以尝试 如 sj、bcv 等其他方法，以获取不同的曲线形状法）
                               alpha = 0.5,  # 设置透明度，0为完全透明，1为不透明
                               colour = "black",  # 边缘密度曲线的颜色
                               fill = "lightblue")  # 密度曲线的填充颜色

p2_test8_density

# getwd()

# ggsave("/Users/chaoqun_ye/Desktop/SPF_Parietal.tiff", plot = p2_test8_density, dpi = 300)

ggsave("/Users/chaoqun_ye/Desktop/SPF_Parietal.pdf", plot = p2_test8_density, dpi = 300)



# ---------------------------------mediation 数据标准化----------------------------------

str(data_cor)

#标准化
#中心化 Xc<- c(scale(X, center=TRUE, scale=FALSE)) #Centering IV; hours of sleep
# 选择需要标准化的变量，因子变量不做标准化
variables_to_standardize <- 
  c("CPT_SUM", "SPF_SUM",
   "frontal_6_10hz", "fronto_central_4_10hz", "left_temporoparietal_6_16hz", 
   "parietal_6_14hz", "right_temporoparietal_7_14hz"
   )


# 对选定的变量进行标准化
data_cor_st <- scale(data_cor[, variables_to_standardize])

# 查看标准化后的数据
head(data_cor_st)
class(data_cor_st) 

# 将标准化后的数据集转换为数据框
data_cor_st <- as.data.frame(data_cor_st)
class(data_cor_st)
# 将未标准化的 Discipline_id（类别变量不需要标准化） 添加到数据框中
#CSSdate_st$Discipline_id <- CSSdate$Discipline_id

# 查看数据框
head(data_cor_st)

#如果有因子变量要转换as.factor
#CSSdate_st$Discipline_id = as.factor(CSSdate_st$Discipline_id)

str(data_cor_st)



# ---------------------------------mediation Bootstrapping 方法 1----------------------------------

## Bootstrapping 方法 
# 调用 PROCESS 函数并传递所有必要参数，包括 data 参数

results1 <- bruceR::PROCESS(
  x = 'CPT_SUM',
  y = 'SPF_SUM',
  meds ='frontal_6_10hz', 
  data = data_cor_st,
  nsim = 10000,
  seed = 2024   #设置随机种子，避免每次结果不同
)



#  Indirect effect(s) of X on Y 显著
results2 <- bruceR::PROCESS(
  x = 'CPT_SUM',
  y = 'SPF_SUM',
  meds ='fronto_central_4_10hz', 
  data = data_cor_st,
  nsim = 10000,
  seed = 2024   #设置随机种子，避免每次结果不同
)




results3 <- bruceR::PROCESS(
  x = 'CPT_SUM',
  y = 'SPF_SUM',
  meds ='left_temporoparietal_6_16hz', 
  data = data_cor_st,
  nsim = 10000,
  seed = 2024   #设置随机种子，避免每次结果不同
)



#  Indirect effect(s) of X on Y 显著
results4 <- bruceR::PROCESS(
  x = 'CPT_SUM',
  y = 'SPF_SUM',
  meds ='parietal_6_14hz', 
  data = data_cor_st,
  nsim = 10000,
  seed = 2024   #设置随机种子，避免每次结果不同
)




results5 <- bruceR::PROCESS(
  x = 'CPT_SUM',
  y = 'SPF_SUM',
  meds ='right_temporoparietal_7_14hz', 
  data = data_cor_st,
  nsim = 10000,
  seed = 2024   #设置随机种子，避免每次结果不同
)








# ---------------------------------mediation Bootstrapping 方法 2----------------------------------

# activate process for R
source("/Users/chaoqun_ye/Documents/Coding/R/Mediation model/processv43/PROCESS v4.3 for R/process.R")

library(broom)    # broom：用于整理结果，使得结果更美观
library(lavaan)   # lavaan：用于计算分析

str(data_cor_st)


process(data = data_cor_st, x = 'CPT_SUM', y = 'SPF_SUM', m = 'frontal_6_10hz', 
        model = 4, effsize = 1, total = 1, stand = 1,  
        modelbt = 1, boot = 10000, seed = 654321)



#  Indirect effect(s) of X on Y 显著
process(data = data_cor_st, x = 'CPT_SUM', y = 'SPF_SUM', m = 'fronto_central_4_10hz',
        model = 4, effsize = 1, total = 1, stand = 1,  #cov = 'Sex_id',
        modelbt = 1, boot = 10000, seed = 654321)



process(data = data_cor_st, x = 'CPT_SUM', y = 'SPF_SUM', m = 'left_temporoparietal_6_16hz',
        model = 4, effsize = 1, total = 1, stand = 1,  #cov = 'Sex_id',
        modelbt = 1, boot = 10000, seed = 654321)



# Direct effect of X on Y       不显著，
# Indirect effect(s) of X on Y. 显著
process(data = data_cor_st, x = 'CPT_SUM', y = 'SPF_SUM', m = 'parietal_6_14hz',
        model = 4, effsize = 1, total = 1, stand = 1,  #cov = 'Sex_id',
        modelbt = 1, boot = 10000, seed = 654321)




process(data = data_cor_st, x = 'CPT_SUM', y = 'SPF_SUM', m = 'right_temporoparietal_7_14hz', 
        model = 4, effsize = 1, total = 1, stand = 1,  
        modelbt = 1, boot = 10000, seed = 654321)











































