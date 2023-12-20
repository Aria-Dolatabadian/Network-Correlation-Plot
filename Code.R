library(metan)
library(writexl)

data<-read.csv("file.csv", header = TRUE)

all<-corr_coef(data)
all
plot(all)
Ex<-all$cor
Ex<-as.data.frame(Ex)
class(Ex)

png("correlation_plot.png", width = 900, height = 700)  # Adjust width and height as needed
plot(all)
dev.off()

Ex <- all$cor
Ex <- as.data.frame(Ex)
class(Ex)

write_xlsx(Ex, "Correlation value of maize.xlsx")

P1<-network_plot(
  all,
  min_cor = NULL,
  show = c("signif", "all"),
  p_val = 0.05,
  legend = c("full", "range"),
  colours = c("red", "white", "darkgreen"),
  legend_width = 1,
  legend_height = 15,
  legend_position = c("right", "left", "top", "bottom"),
  curved = TRUE,
  angle = 90,
  curvature = 0.5,
  expand_x = 0.25,
  expand_y = 0.25
)



P1
ggsave(filename = "Network Correlation Plot.jpg", plot = P1,
       width = 35, height = 30, dpi = 1500, units = "cm")

library(readxl)
CC<-read_excel("Correlation value of maize.xlsx")
View(CC)
library(sjPlot)
head(CC)
tab_corr(CC)
#to show the lower triangle only
tab_corr(CC,
         triangle = "lower", 
         p.numeric = TRUE,
         file = "Maize significant Correlation Neumeric.docx")
require(ggplot2)
library(reshape2)
Ex1<-all$cor
Ex1<-as.data.frame(Ex1)
class(Ex1)
corr<-data.frame(Ex1)
corr_df<-as.data.frame(corr)
corr_df$variable1<-rownames(corr_df)
melted_corr<-melt(corr_df, id.vars = "variable1", variable.name = "variable2", value.name = "value")

P2<-ggplot(melted_corr, aes(x=variable1, y=variable2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "red", mid = "white", high = "green", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  coord_polar() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 9.5),
        axis.text.y = element_text(size = 9))

P2
library(ggplot2)
ggsave(filename = "Circular Correlation Plot.jpg", plot = P2,
       width = 35, height = 30, dpi = 1500, units = "cm")

