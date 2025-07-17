## Cross-species integrative omics identifies conserved metabolic biomarkers and genetic regulatory mechanisms of mortality risk

## The code of the Part 2
## GO mortalty description
setwd("/Users/liupeihao/Desktop")
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
install.packages("ggsignif")
library(ggsignif)
data1<-read.csv("G0Mortality.CSV",header = T) 
head(data1)
plot.format=theme(plot.background=element_blank(),
                  panel.grid=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_rect(color="black",linewidth=1,fill=NA),
                  axis.line=element_blank(),
                  axis.ticks=element_line(color="black",linewidth=0.5),
                  axis.text=element_text(color="black",size=8),
                  axis.title=element_text(color="black",size=11),
                  plot.title=element_text(color="black",size=9),
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.text=element_text(color="black",size=7),
                  legend.title=element_text(color="black",size=7))
data1$Trait <- factor(data1$Sex, levels = unique(data1$Sex))  
p1 <- ggplot(data1, aes(x = Group, y = newMortality, fill = Group)) +
  geom_boxplot(position = position_dodge(1), outlier.size = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "#F3FFD1", color = "#F3FFD1", position = position_dodge(1)) + 
  scale_fill_manual(values = c("#EE7E77","#68A7BE")) +  
  facet_wrap(~Sex) +plot.format+
  labs(x = "Group", y = "Mortality(%)") +
  theme_bw() +
  theme(
        panel.border = element_rect(size = 0.8),
        plot.margin = margin(1, 1, 1, 1, 'cm'),
        panel.grid.major = element_line(linetype = "dotted", color = "gray", size = 0.4), 
        panel.grid.minor = element_line(linetype = "dotted", color = "gray", size = 0.4),
        legend.position = "none") +
  ylim(0, 12.5)+ 
  stat_compare_means(comparisons=list(c("H", "L")),
                     label = "p.signif",
                     method="wilcox.test",size = 4)
p1


#wilcox test
install.packages("tidyverse")
install.packages("nortest")
install.packages("car")
library(tidyverse)
library(nortest)
library(car)

## Dam
data<-read.table("G0Dam.txt",header=T)
head(data)
a <- data %>% filter(Group == "H") %>% select(Mortality)
b <- data %>% filter(Group == "L") %>% select(Mortality)
a <- a$Mortality 
b <- b$Mortality
shapiro.test(a)
shapiro.test(b)
library(nortest)
lillie.test(a)
lillie.test(b)
data$Group <- as.factor(data$Group)
leveneTest(Mortality~Group, data=data)
#Wilcoxon test
wilcox.test(Mortality~Group, data=data, var.equal=T) 
## wilcoxon test：W = 182, p-value = 9.971e-08

## Sire
data<-read.table("G0Sire.txt",header=T)
head(data)
a <- data %>% filter(Group == "H") %>% select(Mortality)
b <- data %>% filter(Group == "L") %>% select(Mortality)
a <- a$Mortality 
b <- b$Mortality
shapiro.test(a)
shapiro.test(b)
library(nortest)
lillie.test(a)
lillie.test(b)
data$Group <- as.factor(data$Group)
leveneTest(Mortality~Group, data=data)
#Wilcoxon检验
wilcox.test(Mortality~Group, data=data, var.equal=T) 
## wilcoxon test：W = 110, p-value = 5.67e-06



## G1 mortalty description
setwd("/Users/liupeihao/Desktop")
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
install.packages("ggsignif")
library(ggsignif)
data1<-read.csv("G1Mortality.CSV",header = T) 
head(data1)
data1$Trait <- factor(data1$Type, levels = c("RowMortality", "AdjustMortality "))
plot.format=theme(plot.background=element_blank(),
                  panel.grid=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_rect(color="black",linewidth=1,fill=NA),
                  axis.line=element_blank(),
                  axis.ticks=element_line(color="black",linewidth=0.5),
                  axis.text=element_text(color="black",size=8),
                  axis.title=element_text(color="black",size=11),
                  plot.title=element_text(color="black",size=9),
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.text=element_text(color="black",size=7),
                  legend.title=element_text(color="black",size=7))
p1 <- ggplot(data1, aes(x = Group, y = newMortality, fill = Group)) +
  geom_boxplot(position = position_dodge(1), outlier.size = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "#F3FFD1", color = "#F3FFD1", position = position_dodge(1)) + 
  scale_fill_manual(values = c("#EE7E77", "#68A7BE")) +  
  facet_wrap(~Type) +plot.format+
  labs(x = "Group", y = "Mortality(%)") +
  theme_bw() +
  theme(
        panel.border = element_rect(size = 0.8),
        plot.margin = margin(1, 1, 1, 1, 'cm'),
        panel.grid.major = element_line(linetype = "dotted", color = "gray", size = 0.4), 
        panel.grid.minor = element_line(linetype = "dotted", color = "gray", size = 0.4),
        legend.position = "none") +
  ylim(0, 150)+ 
  stat_compare_means(comparisons=list(c("H", "L")),
                     label = "p.signif",
                     method="wilcox.test",size = 4)
p1


#wilcox test
install.packages("tidyverse")
install.packages("nortest")
install.packages("car")
library(tidyverse)
library(nortest)
library(car)

# Row Mortality
data<-read.table("RowG1Mortality.txt",header=T)
head(data)
a <- data %>% filter(Group == "H") %>% select(Mortality)
b <- data %>% filter(Group == "L") %>% select(Mortality)
a <- a$Mortality 
b <- b$Mortality
shapiro.test(a)
shapiro.test(b)
library(nortest)
lillie.test(a)
lillie.test(b)
data$Group <- as.factor(data$Group)
leveneTest(Mortality~Group, data=data)
#Wilcoxon检验
wilcox.test(Mortality~Group, data=data, var.equal=F)
## wilcoxon test：W = 108054, p-value = 1.464e-10

# Adjust Mortality
data<-read.table("AdjustG1Mortality.txt",header=T)
head(data)
a <- data %>% filter(Group == "H") %>% select(Mortality)
b <- data %>% filter(Group == "L") %>% select(Mortality)
a <- a$Mortality 
b <- b$Mortality
shapiro.test(a)
shapiro.test(b)
library(nortest)
lillie.test(a)
lillie.test(b)
data$Group <- as.factor(data$Group)
leveneTest(Mortality~Group, data=data)
#Wilcoxon检验
wilcox.test(Mortality~Group, data=data, var.equal=F) 
## wilcoxon test：W = 46690, p-value < 2.2e-16



## G2 mortalty description
setwd("/Users/liupeihao/Desktop")
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
install.packages("ggsignif")
library(ggsignif)
data1<-read.csv("G2Mortality.CSV",header = T) 
head(data1)
plot.format=theme(plot.background=element_blank(),
                  panel.grid=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_rect(color="black",linewidth=1,fill=NA),
                  axis.line=element_blank(),
                  axis.ticks=element_line(color="black",linewidth=0.5),
                  axis.text=element_text(color="black",size=8),
                  axis.title=element_text(color="black",size=11),
                  plot.title=element_text(color="black",size=9),
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.text=element_text(color="black",size=7),
                  legend.title=element_text(color="black",size=7))
p1 <- ggplot(data1, aes(x = Group, y = Mortality, color = Group,  fill = Group)) +
  geom_boxplot(width = 0.6,size = 0.8) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "#F3FFD1", color = "#F3FFD1", position = position_dodge(1)) + 
  scale_fill_manual(values = c( "#68A7BE","#FF9991","#F59EAF","#EE7E77")) + 
  scale_color_manual(values = c( "#68A7BE","#FF9991","#F59EAF","#EE7E77")) + 
  plot.format+
  labs(x = "Group", y = "Mortality(%)") +
  theme_bw() +
  theme(
        panel.border = element_rect(size = 0.8),
        plot.margin = margin(1, 1, 1, 1, 'cm'),
        panel.grid.major = element_line(linetype = "dotted", color = "gray", size = 0.4), 
        panel.grid.minor = element_line(linetype = "dotted", color = "gray", size = 0.4),
        legend.position = "none") +
  ylim(20, 82)+
  stat_compare_means(comparisons = list(c("C", "T1"), c("T1", "T2"), c("T2", "T3")), 
      label = "p.signif",  
      method = "t.test",  
      size = 4, 
      position = position_dodge(0.8),label.y = 73)
p1


## Different analysis of cytokines
setwd("/Users/liupeihao/Desktop")
library(ggplot2)
library(ggthemes)
library(ggpubr)
install.packages("wesanderson")
library(wesanderson)
install.packages("ggforce")
library(ggforce)
install.packages("tidyverse")
library(tidyverse)
install.packages("patchwork")
library(patchwork)
install.packages("ggbeeswarm")
library(ggbeeswarm)
install.packages("ggsci")
library(ggsci)
install.packages("multcompView")
library(multcompView)
install.packages("magrittr")
library(magrittr)
install.packages("ggprism")
library(ggprism)

data <-read.csv("antibody.csv",sep=",") 
head(data)

## T-AOC
p1 <- ggplot(data, aes(x = Group, y = TAOC, fill = Group)) +
  geom_violin(trim = FALSE, size = 0.6, width=0.6) +  
  geom_boxplot(colour = "black", size = 0.6, fill = "white", outlier.size = 0.4,width=0.1) +
  scale_fill_manual(values = c("#EE7E77","#68A7BE")) +
  xlab("Group") +
  ylab("Content(mmol/L)") +
  scale_y_continuous(limits = c(0.5,1.3),
                     breaks = c(0.5,0.7, 0.9,1.1,1.3),
                     labels = c(0.5,0.7, 0.9,1.1,1.3),
                     guide = "prism_offset",
                     expand = c(0.02,0.02))+
  theme_classic() +
  theme(plot.background=element_blank(),  
        panel.grid=element_blank(),  
        panel.background=element_blank(),  
        panel.border=element_rect(color="black",linewidth=1,fill=NA),  
        axis.line=element_blank(),  
        axis.ticks=element_line(color="black",linewidth=0.5),  
        axis.text=element_text(color="black",size=8),  
        axis.title=element_text(color="black",size=11),  
        plot.title=element_text(color="black",size=9),  
        legend.background=element_blank(),  
        legend.key=element_blank(),  
        legend.text=element_text(color="black",size=7),  
        legend.title=element_text(color="black",size=7),  
        legend.position = "none",
        plot.margin = margin(0.5, 0.4, 0.5, 0.5, 'cm'),
        panel.grid.major = element_line(linetype = "dotted", color = "gray", size = 0.4), 
        panel.grid.minor = element_line(linetype = "dotted", color = "gray", size = 0.4))+ 
  stat_compare_means(comparisons=list(c("H", "L")),
                     label = "p.signif",
                     method="wilcox.test",size = 4,label.y = 1.2)
p1

filtered_data <- data %>%
  filter(Group %in% c("H", "L"))
wilcox_result <- wilcox.test(TAOC ~ Group, data = filtered_data)  
wilcox_result
## W = 787.5, p-value = 0.1228

##  IFN
p2 <- ggplot(data, aes(x = Group, y = IFN, fill = Group)) +
  geom_violin(trim = FALSE, size = 0.6, width=0.6) +  
  geom_boxplot(colour = "black", size = 0.6, fill = "white", outlier.size = 0.4,width=0.1) +
  scale_fill_manual(values = c("#EE7E77","#68A7BE")) +
  scale_color_manual(values = c("#5BC5B1","#D84B67")) +
  xlab("Group") +
  ylab("Content(ng/L)") +
  scale_y_continuous(limits = c(20,110),
                     breaks = c(20,50, 80,110),
                     labels = c(20,50, 80,110),
                     guide = "prism_offset",
                     expand = c(0.05,0.05))+
  theme_classic() +
  theme(plot.background=element_blank(),  
        panel.grid=element_blank(),  
        panel.background=element_blank(),  
        panel.border=element_rect(color="black",linewidth=1,fill=NA),  
        axis.line=element_blank(),  
        axis.ticks=element_line(color="black",linewidth=0.5),  
        axis.text=element_text(color="black",size=8),  
        axis.title=element_text(color="black",size=11),  
        plot.title=element_text(color="black",size=9),  
        legend.background=element_blank(),  
        legend.key=element_blank(),  
        legend.text=element_text(color="black",size=7),  
        legend.title=element_text(color="black",size=7),  
        legend.position = "none",
        plot.margin = margin(0.5, 0.4, 0.5, 0, 'cm'),
        panel.grid.major = element_line(linetype = "dotted", color = "gray", size = 0.4), 
        panel.grid.minor = element_line(linetype = "dotted", color = "gray", size = 0.4))+ 
  stat_compare_means(comparisons=list(c("H", "L")),
                     label = "p.signif",
                     method="wilcox.test",size = 4,label.y = 103)
p2

filtered_data <- data %>%
  filter(Group %in% c("H", "L"))
wilcox_result <- wilcox.test(IFN ~ Group, data = filtered_data)  
wilcox_result
# W = 1153, p-value = 4.075e-08

##  IL1
p3 <- ggplot(data, aes(x = Group, y = IL1, fill = Group)) +
  geom_violin(trim = FALSE, size = 0.6, width=0.6) +  
  geom_boxplot(colour = "black", size = 0.6, fill = "white", outlier.size = 0.4,width=0.1) +
  scale_fill_manual(values = c("#EE7E77","#68A7BE")) +
  scale_color_manual(values = c("#5BC5B1","#D84B67")) +
  xlab("Group") +
  ylab("Content(ng/L)") +
  scale_y_continuous(limits = c(10,40),
                     breaks = c(10,20,30,40),
                     labels = c(10,20,30,40),
                     guide = "prism_offset",
                     expand = c(0.04,0.04))+
  theme_classic() +
  theme(plot.background=element_blank(),  
        panel.grid=element_blank(),  
        panel.background=element_blank(),  
        panel.border=element_rect(color="black",linewidth=1,fill=NA),  
        axis.line=element_blank(),  
        axis.ticks=element_line(color="black",linewidth=0.5),  
        axis.text=element_text(color="black",size=8),  
        axis.title=element_text(color="black",size=11),  
        plot.title=element_text(color="black",size=9),  
        legend.background=element_blank(),  
        legend.key=element_blank(),  
        legend.text=element_text(color="black",size=7),  
        legend.title=element_text(color="black",size=7),  
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0, 'cm'),
        panel.grid.major = element_line(linetype = "dotted", color = "gray", size = 0.4), 
        panel.grid.minor = element_line(linetype = "dotted", color = "gray", size = 0.4))+ 
  stat_compare_means(comparisons=list(c("H", "L")),
                     label = "p.signif",
                     method="wilcox.test",size = 4,label.y = 37)
p3

filtered_data <- data %>%
  filter(Group %in% c("H", "L"))
wilcox_result <- wilcox.test(IL1 ~ Group, data = filtered_data)  
wilcox_result
# W = 1120, p-value = 2.876e-07

## The three diagrams are merged together
library(patchwork) 
p <- p1+p2+p3
p


## ROC curve of IL1 and IFN
setwd("/Users/liupeihao/Desktop")
install.packages("pROC")
library(pROC)

aSAH <-read.csv("antibodyROC.csv",sep=",")
head(aSAH)
roc1 <- roc(aSAH$Group,aSAH$IFN, plot=TRUE)
plot(roc1, print.thres="best", print.thres.best.method="youden")
ci.auc(roc1)
par(pty = "s")
roc(aSAH$Group,aSAH$IFN, plot=TRUE)
roc(aSAH$Group,aSAH$IFN, plot=TRUE, legacy.axes=TRUE)
roc(aSAH$Group,aSAH$IFN,plot=TRUE, legacy.axes=TRUE,percent=TRUE,xlab="False Positive Percentage",ylab="True Postive Percentage")
roc(aSAH$Group,aSAH$IFN,plot=TRUE, legacy.axes=TRUE,percent=TRUE,xlab="False Positive Percentage",ylab="True Postive Percentage",col="#EE7E77", lwd=2)
roc1 <- roc(aSAH$Group,aSAH$IL1, plot=TRUE)
plot(roc1, print.thres="best", print.thres.best.method="youden")
ci.auc(roc1)
plot.roc(aSAH$Group,aSAH$IFN,legacy.axes=TRUE,col="#EE7E77", lwd=2,print.auc=TRUE,print.auc.y=0.45,print.auc.x=0.4, lty = 1)
plot.roc(aSAH$Group,aSAH$IL1, legacy.axes=TRUE,col="#68A7BE", lwd=2,print.auc=TRUE, add=TRUE, print.auc.y=0.35,print.auc.x=0.4, lty = 2)
legend("bottomright", legend=c("IFN", "IL1"),col=c("#EE7E77", "#68A7BE"), lwd=2)


## Correlation between cytokines and mortality
setwd("/Users/liupeihao/Desktop")
install.packages("cowplot")
install.packages("ggpubr")
install.packages("gridExtra")
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra) 
library(ggprism)
library(ggExtra)
data<-read.csv("newcorrelation.csv")
head(data)
  
## IL1 with Mortality
p1 <- ggplot(data, aes(x = IL1, y = Mortality)) +  
  geom_point(size = 1.5, color = "#777777") +  
  geom_smooth(method = "lm", color = "#003A75", fill = "#68A7BE", size = 1, se = TRUE) +  
  stat_cor(method = "kendall", label.x = 15, label.y = 33, label.sep = " ", size = 3) +  
  xlab("IL1") + ylab("Mortality(%)") +  
  theme(plot.background=element_blank(),  
        panel.grid=element_blank(),  
        panel.background=element_blank(),  
        panel.border=element_rect(color="black",linewidth=1,fill=NA),  
        axis.line=element_blank(),  
        axis.ticks=element_line(color="black",linewidth=0.5),  
        axis.text=element_text(color="black",size=8),  
        axis.title=element_text(color="black",size=11),  
        plot.title=element_text(color="black",size=9),  
        legend.background=element_blank(),  
        legend.key=element_blank(),  
        legend.text=element_text(color="black",size=7),  
        legend.title=element_text(color="black",size=7),  
        legend.position = "none",  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))+
  scale_x_continuous(limits = c(15, 32)) +  
  scale_y_continuous(limits = c(0, 35))  
p_marginal <- ggMarginal(p1,  
                         type = "histogram",  
                         margins = "x",   
                         xparams = list(fill = "#68A7BE", color = "black")) 
p_marginal  


## IFN with Mortality
p2 <- ggplot(data, aes(x = IFN, y = Mortality)) +
      geom_point(size = 1.5, color = "#777777") +  
      geom_smooth(method = "lm", color = "#9F0000", fill = "#EE7E77", size = 1, se = TRUE) +  
      stat_cor(method = "kendall", label.x = 40, label.y = 33, label.sep = " ", size = 3) +
      xlab("IFN") + ylab("Mortality(%)") +  
      theme(plot.background=element_blank(),  
        panel.grid=element_blank(),  
        panel.background=element_blank(),  
        panel.border=element_rect(color="black",linewidth=1,fill=NA),  
        axis.line=element_blank(),  
        axis.ticks=element_line(color="black",linewidth=0.5),  
        axis.text=element_text(color="black",size=8),  
        axis.title=element_text(color="black",size=11),  
        plot.title=element_text(color="black",size=9),  
        legend.background=element_blank(),  
        legend.key=element_blank(),  
        legend.text=element_text(color="black",size=7),  
        legend.title=element_text(color="black",size=7),  
        legend.position = "none",  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))+
      scale_x_continuous(limits = c(38, 83)) +  
      scale_y_continuous(limits = c(0, 35))
p_marginal <- ggMarginal(p2,  
                         type = "histogram",  
                         margins = "x",   
                         xparams = list(fill = "#EE7E77", color = "black")) 
p_marginal  


## TAOC with mortality
p3 <- ggplot(data, aes(x = TAOC, y = Mortality)) +
      geom_point(size = 1.5, color = "#777777") +  
      geom_smooth(method = "lm", color = "#E2AA60", fill = "#EAD76E", size = 1, se = TRUE) +  # 添加回归线
      stat_cor(method = "kendall", label.x = 0.6, label.y = 33, label.sep = " ", size = 3) +
      xlab("T-AOC") + ylab("Mortality(%)") +  
      theme(plot.background=element_blank(),  
        panel.grid=element_blank(),  
        panel.background=element_blank(),  
        panel.border=element_rect(color="black",linewidth=1,fill=NA),  
        axis.line=element_blank(),  
        axis.ticks=element_line(color="black",linewidth=0.5),  
        axis.text=element_text(color="black",size=8),  
        axis.title=element_text(color="black",size=11),  
        plot.title=element_text(color="black",size=9),  
        legend.background=element_blank(),  
        legend.key=element_blank(),  
        legend.text=element_text(color="black",size=7),  
        legend.title=element_text(color="black",size=7),  
        legend.position = "none",  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))+
      scale_x_continuous(limits = c(0.6, 1.05)) +  
      scale_y_continuous(limits = c(0, 35))
p_marginal <- ggMarginal(p3,  
                         type = "histogram",  
                         margins = "x",   
                         xparams = list(fill = "#EAD76E", color = "black")) 
p_marginal  


## Different analysis of Growth Performance
setwd("/Users/liupeihao/Desktop")
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
install.packages("ggsignif")
library(ggsignif)

## Body Weight
data1<-read.csv("BW.CSV",header = T) 
head(data1)
plot.format=theme(plot.background=element_blank(),
                  panel.grid=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_rect(color="black",linewidth=1,fill=NA),
                  axis.line=element_blank(),
                  axis.ticks=element_line(color="black",linewidth=0.5),
                  axis.text=element_text(color="black",size=8),
                  axis.title=element_text(color="black",size=11),
                  plot.title=element_text(color="black",size=9),
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.text=element_text(color="black",size=7),
                  legend.title=element_text(color="black",size=7))
data1$Days <- factor(data1$Days, levels = unique(data1$Days))  
p1 <- ggplot(data1, aes(x = Group, y = BW, fill = Group)) +
  geom_boxplot(position = position_dodge(1), outlier.size = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "#F3FFD1", color = "#F3FFD1", position = position_dodge(1)) + 
  scale_fill_manual(values = c("#EE7E77","#68A7BE")) +  
  facet_wrap(~Days) +plot.format+
  labs(x = "Group", y = "Body Weight(g)") +
  theme_bw() +
  theme(
        panel.border = element_rect(size = 0.8),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.grid.major = element_line(linetype = "dotted", color = "gray", size = 0.4), 
        panel.grid.minor = element_line(linetype = "dotted", color = "gray", size = 0.4),
        legend.position = "none") +
  ylim(1000,3500)+ 
  stat_compare_means(comparisons=list(c("H", "L")),
                     label = "p.signif",
                     method="wilcox.test",size = 4)
p1

## Weight Gain
data1<-read.csv("GW.CSV",header = T) 
head(data1)
plot.format=theme(plot.background=element_blank(),
                  panel.grid=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_rect(color="black",linewidth=1,fill=NA),
                  axis.line=element_blank(),
                  axis.ticks=element_line(color="black",linewidth=0.5),
                  axis.text=element_text(color="black",size=8),
                  axis.title=element_text(color="black",size=11),
                  plot.title=element_text(color="black",size=9),
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.text=element_text(color="black",size=7),
                  legend.title=element_text(color="black",size=7))
data1$Week <- factor(data1$Week, levels = unique(data1$Week))  
p1 <- ggplot(data1, aes(x = Group, y = WG, fill = Group)) +
  geom_boxplot(position = position_dodge(1), outlier.size = 0.5) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "#F3FFD1", color = "#F3FFD1", position = position_dodge(1)) + 
  scale_fill_manual(values = c("#EE7E77","#68A7BE")) +  
  facet_wrap(~Week) +plot.format+
  labs(x = "Group", y = "Gain Weight(g)") +
  theme_bw() +
  theme(
        panel.border = element_rect(size = 0.8),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.grid.major = element_line(linetype = "dotted", color = "gray", size = 0.4), 
        panel.grid.minor = element_line(linetype = "dotted", color = "gray", size = 0.4),
        legend.position = "none") +
  ylim(100,1100)+ 
  stat_compare_means(comparisons=list(c("H", "L")),
                     label = "p.signif",
                     method="wilcox.test",size = 4)
p1



## Metabolomic-related description
## Super Class 
setwd("/Users/liupeihao/Desktop")
library(ggplot2)  
library(dplyr)  
library(scales)  

data <- read.csv("SuperClass.csv")  
head(data)
data <- data %>% mutate(Percentage = ifelse(Percentage > 100, 100, Percentage) / 100)  
data <- data %>%  mutate(remaining = 1 - Percentage,end = cumsum(Percentage + remaining),start = end - Percentage, start_remaining = end - remaining  )  
  colors <- c(  
  "Benzenoids" = "#CCEBC5",  
  "Lipids and lipid-like molecules" = "#9CD1C7",  
  "Nucleosides, nucleotides, and analogues" = "#F4CFE4",  
  "Organic acids and derivatives" = "#BEBAD8",  
  "Organic oxygen compounds" = "#BCDD78",  
  "Organoheterocyclic compounds" = "#8AB0D0",  
  "Phenylpropanoids and polyketides" = "#FFFFBB",  
  "Other" = "#EB8677",  
  "Unclassified" = "#F3B76F")  
data$Super.Class <- factor(data$Super.Class, levels = unique(data$Super.Class))  
p1<-ggplot(data, aes(y = Super.Class)) +  
  geom_tile(aes(x = 0.5, width = 1, height = 0.8), color="#000000", linewidth=0.4, fill = "#EEEEEE") +  
  geom_rect(aes(xmin = 0, xmax = Percentage, ymin = as.numeric(factor(Super.Class)) - 0.4, ymax = as.numeric(factor(Super.Class)) + 0.4, fill = Super.Class), color = "#000000", linewidth=0.4) +  
  geom_text(aes(x = Percentage/2, label = scales::percent(Percentage)), size = 3, color = "black") + 
  scale_x_continuous(labels = scales::number, limits = c(0, 1)) +  
  scale_fill_manual(values = colors) +  
  labs(x = "Percentage", y = "Super Class", fill = "Super Class") +  
  theme_classic() +  
  theme(axis.title = element_text(size = 11, color = 'black'),  
        axis.text.x = element_text(size = 8,color = 'black'),  
        axis.text.y = element_text(size = 8, color = 'black'),  
        axis.line = element_line(),  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),  
        axis.ticks = element_line(size = 0.5),  
        legend.position = 'none')  
p1

## OPLS-DA
setwd("/Users/liupeihao/Desktop")
BiocManager::install("ropls",update = FALSE)
install.packages("ggforce")
install.packages("ggplot2")
install.packages("ggprism")
library(ropls)
library(ggplot2)
library(ggforce)
library(ggprism) 
otu_raw <- read.table(file="otu.txt",sep="\t",header=T,check.names=FALSE ,row.names=1)
group <- read.table(file="group.txt",sep="\t",header=T,check.names=FALSE ,row.names=1)
otu <- t(otu_raw) 
df1_oplsda <- opls(otu, group$group, predI = 1, orthoI = NA) 
##      R2X(cum) R2Y(cum) Q2(cum) RMSEE pre ort pR2Y  pQ2
##Total    0.365    0.891   0.694 0.134   1   6 0.05 0.05

data <- as.data.frame(df1_oplsda@scoreMN)
o1 <- df1_oplsda@orthoScoreMN[,1]
data$o1 <- o1
data$group = group$group
data$samples = rownames(data)
x_lab <- df1_oplsda@modelDF[1, "R2X"] * 100
write.csv(data, file = "42-to1.csv", row.names = FALSE)

col=c("#EE7E77", "#68A7BE")
p1 <- ggplot(data,aes(x=p1,y=o1,color=group)) +
  theme_bw() +
  geom_point(size=1.5) +
  geom_vline(xintercept = 0,lty="dashed",color="#D1287A",size = 0.8) +
  geom_hline(yintercept = 0,lty="dashed",color="#D1287A",size = 0.8) +
  labs(x=paste0("Tp1 (",x_lab,"%)"), y="To1") + 
  stat_ellipse(data=data,
               geom = "polygon",level = 0.95,
               linetype = 2,linewidth=1,
               aes(fill=group),
               alpha=0.2,
               show.legend = T,size = 0.5) +
  scale_color_manual(values = col) +
  scale_fill_manual(values = c("#FEECE7","#DEEEED")) +
  theme(axis.title.x=element_text(size=13), 
        axis.title.y=element_text(size=13, angle=90), 
        axis.text.y=element_text(size=8),
        axis.text.x=element_text(size=8),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.grid = element_blank(),
        text = element_text(size=11),
        panel.border = element_rect(size = 0.5, color = "black"))
p1

data_VIP <- df1_oplsda@vipVn
data_VIP_select <- data_VIP[data_VIP > 0] data_VIP_select <- cbind(otu_raw[names(data_VIP_select), ], data_VIP_select)
names(data_VIP_select)[13] <- "VIP"
data_VIP_select <- data_VIP_select[order(data_VIP_select$VIP, decreasing = TRUE), ]
data_VIP_select$G = rownames(data_VIP_select)
write.csv(data_VIP_select,"42-newVIP.csv")


## Differential accumulation metabolites
## Volcano plot
setwd("/Users/liupeihao/Desktop")
library(dplyr)
library(ggplot2) 
library(ggprism) 
library(ggrepel) 
library(reshape2)

BRCA_Match_DEG <- read.csv("42DMs.csv")
head(BRCA_Match_DEG)
BRCA_Match_DEG$log2FC <- (BRCA_Match_DEG$log2FoldChange)
BRCA_Match_DEG$logP <- -log10(BRCA_Match_DEG$pvalue)
colnames(BRCA_Match_DEG)[1] <- "DM_name"
head(BRCA_Match_DEG)
BRCA_Match_DEG1 <- BRCA_Match_DEG %>% 
  mutate(DM = case_when(log2FC > 0.5849625 & logP > 1.3 ~ "UP",
                         abs(log2FC) < 0.5849625 | logP < 1.3 ~ "NO",
                         log2FC < -0.5849625 & logP > 1.3 ~ "DOWN"))
head(BRCA_Match_DEG1)

p1 <- ggplot(BRCA_Match_DEG1, aes(x = log2FC, y = logP, colour = DM)) +  
  geom_point(alpha = 0.85, size = 2) +  
  scale_color_manual(values = c("#68A7BE", 'gray',"#EE7E77")) +  
  scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 2.5)) + 
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24,4)) + 
  geom_vline(xintercept = c(-0.5849625, 0.5849625), lty = 2, col = "black", lwd = 0.5) +  
  geom_hline(yintercept = 1.3, lty = 2, col = "black", lwd = 0.5) +   
  labs(x = "log2FC", y = "-log2(p-value)") +  
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),  
        legend.position = "none",  
        legend.title = element_blank(),  
        axis.title.x=element_text(size=11),   
        axis.title.y=element_text(size=11, angle=90),   
        axis.text.y=element_text(size=8),  
        axis.text.x=element_text(size=8),  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),  
        panel.grid = element_blank(),  
        text = element_text(size=11),  
        panel.border = element_rect(size = 1, color = "black"))  
p2 <- p1 + geom_text(aes(x = -3.7, y =23.5, label = "DOWN"), color = "black", hjust = 0, size = 2.5, family = "Arial", fontface = "bold") +
           geom_text(aes(x = 0, y = 23.5, label = "NO"), color = "black", hjust = 0.5, size = 2.5, family = "Arial", fontface = "bold") +
           geom_text(aes(x = 2.8, y =23.5, label = "UP"), color = "black", hjust = 0, size = 2.5, family = "Arial", fontface = "bold")+
           geom_hline(yintercept = 22, lty = 2, col = "grey", lwd = 0.5)

p2


## Differential Accumulation Metabolites Statistical
## Super Class and VIP of DAMs
setwd("/Users/liupeihao/Desktop") 
library(ggplot2)  
library(dplyr)  
library(ggtext)  
mydata<-read.csv("42DMsuperclass.csv",sep=",")  
head(mydata)  
desired_order <- c("Lipids and lipid-like molecules","Organic acids and derivatives","Organoheterocyclic compounds","Organic oxygen compounds",  
                   "Benzenoids","Phenylpropanoids and polyketides","Other","Unclassified","Nucleosides, nucleotides, and analogues")  
desired_order <- desired_order[desired_order %in% mydata$SuperClass]  
mydata$SuperClass <- factor(mydata$SuperClass, levels = desired_order)  
colors <- c("Benzenoids" = "#CCEBC5","Lipids and lipid-like molecules" = "#9CD1C7","Nucleosides, nucleotides, and analogues" = "#F4CFE4",  
            "Organic acids and derivatives" = "#BEBAD8","Organic oxygen compounds" = "#BCDD78", "Organoheterocyclic compounds" = "#8AB0D0", "Phenylpropanoids and polyketides" = "#FFFFBB",  
            "Other" = "#EB8677","Unclassified" = "#F3B76F")  
p1 <- ggplot(data = mydata, aes(SuperClass, Number, fill = Group)) +  
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.7, size = 0.25) +  
  scale_fill_manual(values = c("#68A7BE","#EE7E77")) +  
  coord_flip()+  
  scale_y_continuous(breaks = seq(0, 40, by = 20)) +  
  theme(  
    plot.title = element_text(hjust = 0.5),  
    legend.position = "none",  
    legend.title = element_blank(),  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm'),  
    axis.text.y = element_markdown(hjust = 1, size = 12), 
    axis.ticks.y = element_line(color = "black", linewidth = 0.5)) +  
  scale_x_discrete(labels = function(x) {  
    color_values <- colors[x]  
    paste0("<span style='font-size:18pt; color:", color_values, "; border: 1px solid black;'>&#9679;</span>")})  
p1

mydata<-read.csv("42DMVIP.csv",sep=",")  
head(mydata)  
desired_order <- c("Lipids and lipid-like molecules","Organic acids and derivatives","Organoheterocyclic compounds","Organic oxygen compounds",  
                   "Benzenoids","Phenylpropanoids and polyketides","Other","Unclassified","Nucleosides, nucleotides, and analogues")  
desired_order <- desired_order[desired_order %in% mydata$SuperClass]  
mydata$SuperClass <- factor(mydata$SuperClass, levels = desired_order)  
colors <- c("Benzenoids" = "#CCEBC5","Lipids and lipid-like molecules" = "#9CD1C7","Nucleosides, nucleotides, and analogues" = "#F4CFE4",  
            "Organic acids and derivatives" = "#BEBAD8","Organic oxygen compounds" = "#BCDD78", "Organoheterocyclic compounds" = "#8AB0D0", "Phenylpropanoids and polyketides" = "#FFFFBB",  
            "Other" = "#EB8677","Unclassified" = "#F3B76F")  
newSuperClass_colors <- colors[mydata$SuperClass]  
p2 <- ggplot(data = mydata, aes(x = SuperClass, y = VIP, fill = SuperClass)) + 
  geom_boxplot(outlier.shape = NA, position = position_dodge(0.75), width = 0.55) +  
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), color = "black", size = 0.8, alpha = 0.7) +  
  scale_fill_manual(values = colors) +
  coord_flip() +  
  scale_y_continuous(breaks = seq(0, 5, by = 1)) +  
  theme(  
    axis.text.x = element_text(color = "black",size = 8),  
    axis.text.y = element_blank(), 
    axis.title.x = element_text(color = "black",  size = 11),  
    axis.title.y = element_blank(),  
    legend.position = "none",  
    legend.title = element_blank(),  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = unit(c(0.5, 1.5, 0.5, 0), 'cm'),  
    axis.ticks.y = element_blank())  
p2

p <- p1+p2
p


## Distribution of CV and VIP across nine  super class metabolites
setwd("/Users/liupeihao/Desktop")  
library(ggplot2)  
library(dplyr)  
library(ggtext) 

mydata<-read.csv("CVandVIP.csv",sep=",")  
head(mydata)  
desired_order <- c("Lipids and lipid-like molecules","Organic acids and derivatives","Organoheterocyclic compounds","Organic oxygen compounds",  
                   "Benzenoids","Phenylpropanoids and polyketides","Other","Unclassified","Nucleosides, nucleotides, and analogues")  
desired_order <- desired_order[desired_order %in% mydata$SuperClass]  
mydata$SuperClass <- factor(mydata$SuperClass, levels = desired_order)  
colors <- c("Benzenoids" = "#CCEBC5","Lipids and lipid-like molecules" = "#9CD1C7","Nucleosides, nucleotides, and analogues" = "#F4CFE4",  
            "Organic acids and derivatives" = "#BEBAD8","Organic oxygen compounds" = "#BCDD78", "Organoheterocyclic compounds" = "#8AB0D0", "Phenylpropanoids and polyketides" = "#FFFFBB",  
            "Other" = "#EB8677","Unclassified" = "#F3B76F")  

p1 <- ggplot(data = mydata, aes(x = SuperClass, y = CV, fill = SuperClass)) +  
  geom_violin() +  
  scale_fill_manual(values = colors) + 
  scale_y_continuous(breaks = c(0, 6, 12, 18), limits = c(0, 18))+  
  coord_flip() +  
  theme(  
    legend.position = "none",  
    legend.title = element_blank(),  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm'),  
    axis.text.y = element_markdown(size = 12,margin = margin(t = -2)), 
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),  
    axis.text.x = element_text(color = "black", size = 8)) +  
  geom_hline(yintercept = 1, color = "#D1287A", linetype = "dashed", size = 0.5)+
  labs(x = "Super Class", y = "CV") +  
  scale_x_discrete(labels = function(x) {  
    color_values <- colors[x]  
    paste0("<span style='font-size:21pt; color:", color_values, "; border: 1px solid black;'>&#9679;</span>")}) 
p1

p2 <- ggplot(data = mydata, aes(x = SuperClass, y = VIP, fill = SuperClass)) +  
  geom_violin() +  
  scale_fill_manual(values = colors) + 
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4), limits = c(0, 4))+ 
  geom_hline(yintercept = 1, color = "#D1287A", linetype = "dashed", size = 0.5) +
   coord_flip() +  
  theme(  
    legend.position = "none",  
    legend.title = element_blank(),  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),   
    axis.title = element_text(color = "black", size = 11),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0.5, 0.5, 0, 'cm'),  
    axis.text.x = element_text(color = "black", size = 8)) +  
  labs(x = "Super Class", y = "VIP") 
p2

library(patchwork)
p <- p1+p2
p


## DIfference Analysis of CV across H and L group
setwd("/Users/liupeihao/Desktop")  
library(ggplot2)  
library(dplyr)  
library(ggtext)
library(ggsignif)  

mydata<-read.csv("newCV.csv",sep=",")  
head(mydata)  
wilcox_test <- wilcox.test(CV ~ Group, data = mydata)  
p_value <- wilcox_test$p.value  
p1 <- ggplot(data = mydata, aes(x = Group, y = CV, fill = Group)) +  
  geom_violin() +  
  scale_fill_manual(values = c("#EE7E77", "#68A7BE")) +  
  scale_y_continuous(breaks = c(0, 6, 12, 18), limits = c(0, 18.5))+  
  theme(  
    legend.position = "none",  
    legend.title = element_blank(),  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),  
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),  
    axis.text = element_text(color = "black", size = 8)) +  
  labs(x = "Group", y = "CV") +  
  stat_compare_means(comparisons=my_comparisons,
                     label.y = c(17),
                     mmethod="wilcox.test",label = "p.signif",size = 4)
p1  


## The distribution of coefficients of variation within different ranges
setwd("/Users/liupeihao/Desktop")
install.packages("gcookbook")
install.packages("ggplot2")
install.packages("scales")
library(gcookbook)
library(ggplot2)
library(scales)
data <- read.csv('42-CV.csv',header = T)
head(data)

p1 <- ggplot(data, aes(x = new, y = Number, color = Group, fill = Group)) +  
  geom_col(position = "dodge", color = "black", width = 0.75) +  
  geom_text(aes(label = Number), position = position_dodge(width = 0.8), vjust = -0.5, color = "black", family = "Arial Bold", size = 2) +  
  scale_color_manual(values = c("#EE7E77","#68A7BE")) +  
  scale_fill_manual(values = c("#EE7E77","#68A7BE")) +  
  labs(x = 'CV(%)', y = 'Number') +  
  theme(axis.text.x = element_text(color = 'black', size = 8, angle = 90, hjust = 1, vjust = 0.5), 
        axis.text= element_text(color = 'black', size = 8),  
        axis.title = element_text(color = 'black', size = 11),  
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
        panel.grid.minor = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1),  
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm')) +  
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),  
                     labels = c("0-10", "20-30", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", ">100"),  
                     expand = c(0.02, 0)) +  
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400), limits = c(0, 410))  
p1  


## CV distribution of nine Super Class（CV：0-5）
setwd("/Users/liupeihao/Desktop")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggsignif")
library(ggplot2)
library(ggpubr)
library(ggsignif)
library(ggplot2)
library(dplyr)
library(ggtext)

df<-read.csv("MetaCV1.CSV")  
head(df)   
desired_order <- c("Lipids and lipid-like molecules","Organic acids and derivatives","Organoheterocyclic compounds","Organic oxygen compounds",  
                   "Benzenoids","Phenylpropanoids and polyketides","Other","Unclassified","Nucleosides, nucleotides, and analogues")  
desired_order <- desired_order[desired_order %in% mydata$SuperClass]  
colors <- c("Benzenoids" = "#CCEBC5","Lipids and lipid-like molecules" = "#9CD1C7","Nucleosides, nucleotides, and analogues" = "#F4CFE4",  
            "Organic acids and derivatives" = "#BEBAD8","Organic oxygen compounds" = "#BCDD78", "Organoheterocyclic compounds" = "#8AB0D0", "Phenylpropanoids and polyketides" = "#FFFFBB",  
            "Other" = "#EB8677","Unclassified" = "#F3B76F")  
df$SuperClass <- factor(df$SuperClass, levels = desired_order)  
available_colors <- colors[names(colors) %in% levels(df$SuperClass)]  

results <- df %>%
  group_by(SuperClass) %>%
  summarise(p_value = wilcox.test(CV ~ Group, data = .)$p.value, .groups = 'drop') %>%
  mutate(label = case_when(p_value < 0.001 ~ "***",p_value < 0.01 ~ "**",p_value < 0.05 ~ "*",TRUE ~ "NS"))

label_data <- data.frame(SuperClass = results$SuperClass,y = 5.6, label = results$label)

bracket_data <- data.frame(
  SuperClass = results$SuperClass) %>%
  mutate(x = as.numeric(SuperClass) - 0.35,xend = as.numeric(SuperClass) + 0.35,y = 5.4)

p1 <- ggplot(df, aes(x = SuperClass, y = CV, fill = Group)) +  
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.5) +  
  scale_fill_manual(values = c("#EE7E77", "#68A7BE")) + 
  theme_minimal() +  
  ylim(0, 6) +  
  labs(x = "Super Class", y = "Coefficient of Variation") +  
  theme(  
    axis.title.x = element_text(size = 11),  
    axis.title.y = element_text(size = 11),  
    axis.text = element_text(size = 8, color = "black"),  
    axis.text.x = ggtext::element_markdown(size = 8, color = "black"),
    axis.ticks = element_line(color = "black", size = 0.5),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    panel.background = element_rect(fill = "white"),  
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm')
  ) +  
  scale_x_discrete(labels = function(x) {  
    color_values <- colors[x]  
    paste0("<span style='font-size:22pt; color:", color_values, "; border: 1px solid black;'>&#9679;</span>")
  }) +  
  geom_text(data = label_data, aes(x = SuperClass, y = y, label = label), 
            inherit.aes = FALSE, size = 5, fontface = "bold") +  
  geom_segment(data = bracket_data, aes(x = x, xend = xend, y = y, yend = y), 
               size = 0.5, inherit.aes = FALSE)
p1


## Venn of VIP and CV
setwd("/Users/liupeihao/Desktop")
install.packages("ggVennDiagram")
library(ggVennDiagram)
library(ggplot2)
library(dplyr)

otu_tax<-read.csv("Venn.csv")
head(otu_tax)
df1 <- otu_tax$VIP
df2<-otu_tax$DM
Venn_data<-list( VIP=df1,DM=df2)
ggVennDiagram(Venn_data, set_size = 6, set_color = "#000000") +scale_fill_gradient(low ="#68A7BE", high = "#EE7E77")+  
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))



## ## Visualization of enrichment analysis results
setwd("/Users/liupeihao/Desktop")
library(readxl)
library(ggplot2)

## KEGG enrichment analysis
KEGG<-read_excel('KEGG.xlsx') 
head(KEGG)
KEGG$Pathway <- factor(KEGG$Pathway, levels = unique(KEGG$Pathway))
p1 <- ggplot(KEGG, aes(x = Pathway, y = Metaratio, fill = -log10(Pvalue))) + 
     geom_bar(stat = 'identity', color = 'black', width = 0.65) +
     coord_flip() +    
     scale_fill_gradient(low = "#68A7BE", high ="#EE7E77") +
     scale_x_discrete(expand = c(0, 0.8)) +
     scale_y_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, 0.5)) +
     labs(x = '', y = 'Metabolites Ratio', fill = '-lg(p-value)') +  
     theme(axis.title = element_text(size = 11, colour = 'Black'), 
           axis.text.y = element_text(size = 8, colour = 'Black'), 
           axis.text.x = element_text(size = 10, colour = 'Black'), 
           axis.line = element_line(size = 0.5, colour = 'black'),
           axis.line.y = element_blank(),  
           axis.ticks.y = element_blank(), 
           panel.background = element_rect(fill = "white"), 
           panel.grid.major.y = element_blank(), 
           panel.grid.minor.y = element_blank(), 
           panel.grid.minor.x = element_blank(),
           legend.text = element_text(size = 8, colour = 'Black'),  
           legend.title = element_text(size = 11, colour = 'Black'),
           plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))
p1

## MSEA
MSEA<-read_excel('MSEA-important.xlsx') 
head(MSEA)
MSEA$MetaSet <- factor(MSEA$MetaSet, levels = unique(MSEA$MetaSet))
p2 <- ggplot(MSEA, aes(x = MetaSet, y = Enrichmentratio, fill = logp)) + 
     geom_bar(stat = 'identity', color = 'black', width = 0.65) +
     coord_flip() +    
     scale_fill_gradient(low = "#68A7BE", high ="#EE7E77") +
     scale_x_discrete(expand = c(0, 0.8)) +
     scale_y_continuous(limits = c(0, 12), expand = c(0, 0), breaks = seq(0, 12, 4)) +
     labs(x = '', y = 'Enrichment Ratio', fill = '-lg(p-value)') +  
     theme(axis.title = element_text(size = 11, colour = 'Black'), 
           axis.text.y = element_text(size = 8, colour = 'Black'), 
           axis.text.x = element_text(size = 10, colour = 'Black'), 
           axis.line = element_line(size = 0.5, colour = 'black'),
           axis.line.y = element_blank(),  
           axis.ticks.y = element_blank(), 
           panel.background = element_rect(fill = "white"), 
           panel.grid.major.y = element_blank(), 
           panel.grid.minor.y = element_blank(), 
           panel.grid.minor.x = element_blank(),
           legend.text = element_text(size = 8, colour = 'Black'),  
           legend.title = element_text(size = 11, colour = 'Black'),
           plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))
p2




## The code of the Part 3
/home/cgbl/biosoft/plink1.9/plink --bfile new_phliu_allsample --allow-no-sex --allow-extra-chr --chr-set 40 --pca 3 --out snp.pca3 
awk '{print 1,$3,$4,$5}' snp.pca3.eigenvec > c.txt
/home/cgbl/biosoft/gemma/gemma-0.98.4-linux-static-AMD64 -bfile new_phliu_allsample -gk 1 -o snp.all.matrix
nohup  bash 1.sh  &
## details of document 1.bash
#!/bin/bash  
for i in {1..150}  
do
/home/cgbl/biosoft/gemma/gemma-0.98.4-linux-static-AMD64 -bfile peihao1100 -lmm 1 -k output1-100/snp.all.matrix.cXX.txt -c c.txt -n $i -o t$i 
done

## Merge mGWAS results for each metabolite
# Run metalocimerge.py to generate combined_meta_loci.txt file
# (Located in /home/phliu/DAMsgroupeffect folder)
python metalocimerge.py

## Summarize GWAS results in mGWAS901-1871 folder
# Result files stored in DAMs folder
python newhebing.py       # Process Benzenoids class
python newhebing_1.py     # Process Lipids class
python newhebing_2.py     # Process Nucleosides class
python newhebing_3.py     # Process Organicacids class
python newhebing_4.py     # Process Organicoxygen class
python newhebing_5.py     # Process Organoheterocyclic class
python newhebing_6.py     # Process Other class
python newhebing_7.py     # Process Phenylpropanoids class
python newhebing_8.py     # Process Unclassified class


# Process integrated GWAS results in /home/phliu/mGWAS901-1871/DAMs/ folder
## Unclassified class results
awk -F'[[:space:]]+' '$3 < 1.69E-06' DAMs_Unclassified_result.txt > filtered_Unclassified_result.txt 
awk '{ $3 = $2; print }' filtered_Unclassified_result.txt > updated_Unclassified_result.txt
awk '{ $1 = "chr" $1; print }' updated_Unclassified_result.txt > new_Unclassified_result.txt 

## Other class results
awk -F'[[:space:]]+' '$3 < 1.69E-06' DAMs_Other_result.txt> filtered_Other_result.txt
awk '{ $3 = $2; print }' filtered_Other_result.txt > updated_Other_result.txt
awk '{ $1 = "chr" $1; print }' updated_Other_result.txt > new_Other_result.txt 

## Phenylpropanoids class results
awk -F'[[:space:]]+' '$3 < 1.69E-06' DAMs_Phenylpropanoids_result.txt> filtered_Phenylpropanoids_result.txt
awk '{ $3 = $2; print }' filtered_Phenylpropanoids_result.txt > updated_Phenylpropanoids_result.txt
awk '{ $1 = "chr" $1; print }' updated_Phenylpropanoids_result.txt > new_Phenylpropanoids_result.txt 

## Organoheterocyclic class results
awk -F'[[:space:]]+' '$3 < 1.69E-06' DAMs_Organoheterocyclic_result.txt> filtered_Organoheterocyclic_result.txt
awk '{ $3 = $2; print }' filtered_Organoheterocyclic_result.txt > updated_Organoheterocyclic_result.txt
awk '{ $1 = "chr" $1; print }' updated_Organoheterocyclic_result.txt > new_Organoheterocyclic_result.txt 

## Organicoxygen class results
awk -F'[[:space:]]+' '$3 < 1.69E-06' DAMs_Organicoxygen_result.txt> filtered_Organicoxygen_result.txt
awk '{ $3 = $2; print }' filtered_Organicoxygen_result.txt > updated_Organicoxygen_result.txt
awk '{ $1 = "chr" $1; print }' updated_Organicoxygen_result.txt > new_Organicoxygen_result.txt 

## Organicacids class results
awk -F'[[:space:]]+' '$3 < 1.69E-06' DAMs_Organicacids_result.txt> filtered_Organicacids_result.txt
awk '{ $3 = $2; print }' filtered_Organicacids_result.txt > updated_Organicacids_result.txt
awk '{ $1 = "chr" $1; print }' updated_Organicacids_result.txt > new_Organicacids_result.txt 

## Nucleosides class results
awk -F'[[:space:]]+' '$3 < 1.69E-06' DAMs_Nucleosides_result.txt> filtered_Nucleosides_result.txt
awk '{ $3 = $2; print }' filtered_Nucleosides_result.txt > updated_Nucleosides_result.txt
awk '{ $1 = "chr" $1; print }' updated_Nucleosides_result.txt > new_Nucleosides_result.txt 

## Benzenoids class results
awk -F'[[:space:]]+' '$3 < 1.69E-06' DAMs_Benzenoids_result.txt> filtered_Benzenoids_result.txt
awk '{ $3 = $2; print }' filtered_Benzenoids_result.txt > updated_Benzenoids_result.txt
awk '{ $1 = "chr" $1; print }' updated_Benzenoids_result.txt > new_Benzenoids_result.txt 

## Lipids class results
awk -F'[[:space:]]+' '$3 < 1.69E-06' DAMs_Lipids_result.txt> filtered_Lipids_result.txt
awk '{ $3 = $2; print }' filtered_Lipids_result.txt > updated_Lipids_result.txt
awk '{ $1 = "chr" $1; print }' updated_Lipids_result.txt > new_Lipids_result.txt 


## Circos plot generation
python Circos.py  # Prepare files for Circos visualization
# Note: Set BIN Setting to 'max'

# Format for Circos input files:
# Column 1: Chromosome
# Column 2: Position
# Column 3: Position + 100000
# Column 4: Significance marker (1 for significant sites, 0.1 for non-significant)
# Column 5: "." (dot)
# Use TBtools' Advanced Circos plugin for visualization

# Color scheme for classes:
"Benzenoids" = "#CCEBC5",
"Lipids and lipid-like molecules" = "#9CD1C7",
"Nucleosides, nucleotides, and analogues" = "#F4CFE4",
"Organic acids and derivatives" = "#BEBAD8",
"Organic oxygen compounds" = "#BCDD78",
"Organoheterocyclic compounds" = "#8AB0D0",
"Phenylpropanoids and polyketides" = "#FFFFBB",    "#FFF701"
"Other" = "#EB8677",
"Unclassified" = "#F3B76F"


## Extract loci for each DAM
python DAMslociextract.py  # Generates DAMsloci.txt in /home/phliu/mGWAS901-1871/DAMs/folder



## Network diagram of mQTL（It is related to at least two metabolites） and metabolites
setwd("/Users/liupeihao/Desktop")  

library(igraph)  
library(ggplot2)  
library(readr)  
library(dplyr)  
library(RColorBrewer)  

edges <- read_csv("SNPmeta.csv", col_names = c("from", "to"))
snp_class <- read_csv("SNP.csv", col_names = c("rsid", "Class")) 
meta_class <- read_csv("meta.csv", col_names = c("Meta", "Class")) 
size_data <- read_csv("Size.csv", col_names = c("Meta", "size"))  

size_data <- size_data %>%
  mutate(size = as.numeric(size))  

snp_colors <- c(
  "intergenic region" = "#E04D54",
  "upstream gene variant" = "#FCD9D3",
  "intron variant" = "#6CCCDC",
  "downstream gene variant" = "#F9C129",
  "loss function variant" = "#F3B76F",
  "missense variant" = "#208B45",
  "synonymous variant" = "#FCDF8E")  

meta_colors <- c(  
    "Benzenoids" = "#CCEBC5",
    "Lipids and lipid-like molecules" = "#9CD1C7",
    "Nucleosides, nucleotides, and analogues" = "#F4CFE4",
    "Organic acids and derivatives" = "#BEBAD8",
    "Organic oxygen compounds" = "#BCDD78",
    "Organoheterocyclic compounds" = "#8AB0D0",
    "Phenylpropanoids and polyketides" = "#FFFFBB",
    "Other" = "#EB8677",
    "Unclassified" = "#F3B76F")  

snp_nodes <- snp_class %>%  
  mutate(  
    nodeName = rsid,  
    Class = as.character(Class),  
    color = snp_colors[Class],  
    shape = "circle",
    size = 3 
  ) %>%  
  select(nodeName, Class, color, shape, size)  

meta_nodes <- meta_class %>%  
  mutate(  
    nodeName = Meta,  
    Class = as.character(Class),  
    color = meta_colors[Class],  
    shape = "square"
  ) %>%  
  left_join(size_data, by = "Meta") %>% 
  mutate(size = ifelse(is.na(size), 1, as.numeric(size))) %>%  
  select(nodeName, Class, color, shape, size)  
 
nodes <- bind_rows(snp_nodes, meta_nodes)  

g <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)  

num_nodes <- vcount(g)  
set.seed(42)  
radii <- runif(num_nodes, min = 0, max = 3)  
angles <- runif(num_nodes, min = 0, max = 2 * pi)  
x_coords <- radii * cos(angles)  
y_coords <- radii * sin(angles)  
layout <- cbind(x_coords, y_coords)  

plot(g,  
     vertex.label = NA,  
     vertex.color = V(g)$color, 
     vertex.shape = V(g)$shape, 
     vertex.size = V(g)$size,  
     edge.color = "#CCCED0",  
     edge.width = 0.8,   
     layout = layout)   

legend("right", pch = 21, pt.bg = snp_colors, legend = names(snp_colors), title = "Variant Type", bty = "n", cex = 1, xjust = 0, yjust = 0.5)  
legend("bottomright", fill = meta_colors, legend = names(meta_colors), title = "Metabolite Class", bty = "n", cex = 1, xjust = 0, yjust = 0.5)  





## Comparison of mQTL and molQTL
## File extraction
tar -xzvf "/home/phliu/mQTLmolQTLoverlap/3aQTLoverlap/chicken_gtex_v0.significant_cis_3aqtl.txt.tar.gz" -C "/home/phliu/mQTLmolQTLoverlap/3aQTLoverlap/3aQTL"
tar -xzvf "/home/phliu/mQTLmolQTLoverlap/lnQTLoverlap/chicken_gtex_v0.significant_cis_lncrna.txt.tar.gz" -C "/home/phliu/mQTLmolQTLoverlap/lnQTLoverlap/lnQTL"
tar -xzvf "/home/phliu/mQTLmolQTLoverlap/sQTLoverlap/chicken_gtex_v0.significant_cis_sqtl.txt.tar.gz" -C "/home/phliu/mQTLmolQTLoverlap/sQTLoverlap/sQTL/"
tar -xzvf "/home/phliu/mQTLmolQTLoverlap/exQTLoverlap/chicken_gtex_v0.significant_cis_exqtl.txt.tar.gz" -C "/home/phliu/mQTLmolQTLoverlap/exQTLoverlap/exQTL/"


## Comparative analysis of mQTL and eQTL
# Conversion of mQTL locus encoding: Performed in /home/phliu/mQTLmolQTLoverlap/ folder
python 2_replace.py  # Replace locus IDs with unified system

## eQTL quantity statistics: In /home/phliu/mQTLmolQTLoverlap/eQTLoverlap/ folder
python 1_extract.py  # Merge significant eQTLs from 28 tissues by tissue-locus relationships, generate "eQTL_combined_data.txt"
python 11.eQTLquchongfu.py  # Remove duplicate loci within same tissue, keep unique values, generate "eQTL_combined_data_unique.txt"
awk -F'_' '{print $1 "\t" substr($0, index($0, $2))}' eQTL_combined_data_unique.txt > eQTL_separated_columns.txt
python 10_eQTLnumber.py  # Count eQTL numbers in different tissues, generate "eQTL_feature_counts.txt" (delete useless first row)

## mQTL quantity statistics:
python 4_MarkingSite.py  # Annotate loci indicating which tissues have both QTL and eQTL (Code in /home/phliu/mQTLmolQTLoverlap/, results in /home/phliu/mQTLmolQTLoverlap/eQTLoverlap/)
python 6_tissue_statistics.py  # Count loci that are both QTL and eQTL in different tissues, generate "DAMs_Sites_statistic.txt"

## Enrichment analysis
python Enrichment.py  # Calculate enrichment factors


## Comparative analysis of mQTL and 3aQTL
## 3aQTL quantity statistics: In /home/phliu/mQTLmolQTLoverlap/3aQTLoverlap/ folder
python 1_extract.py  # Merge significant 3aQTLs from 28 tissues, generate "3aQTL_combined_data.txt"
python 11.eQTLquchongfu.py  # Remove duplicates, generate "3aQTL_combined_data_unique.txt"
awk -F'_' '{print $1 "\t" substr($0, index($0, $2))}' 3aQTL_combined_data_unique.txt > 3aQTL_separated_columns.txt
python 10_eQTLnumber.py  # Count 3aQTL numbers per tissue, generate "3aQTL_feature_counts.txt" (delete first row)

## mQTL quantity statistics:
python 4_MarkingSite.py  # Annotate loci with 3aQTL tissue associations (Code in /home/phliu/mQTLmolQTLoverlap/, results in /home/phliu/mQTLmolQTLoverlap/3aQTLoverlap/)
python 6_tissue_statistics.py  # Count loci that are both QTL and 3aQTL, generate "DAMs_Sites_statistic.txt"

## Enrichment analysis
python Enrichment.py  # Calculate enrichment factors


## Comparative analysis of mQTL and exQTL
## exQTL quantity statistics: In /home/phliu/mQTLmolQTLoverlap/exQTLoverlap/ folder
python 1_extract.py  # Merge significant exQTLs from 28 tissues, generate "exQTL_combined_data.txt"
python 11.eQTLquchongfu.py  # Remove duplicates, generate "exQTL_combined_data_unique.txt"
awk -F'_' '{print $1 "\t" substr($0, index($0, $2))}' exQTL_combined_data_unique.txt > exQTL_separated_columns.txt
python 10_eQTLnumber.py  # Count exQTL numbers per tissue, generate "exQTL_feature_counts.txt" (delete first row)

## mQTL quantity statistics:
python 4_MarkingSite.py  # Annotate loci with exQTL tissue associations (Code in /home/phliu/mQTLmolQTLoverlap/, results in /home/phliu/mQTLmolQTLoverlap/exQTLoverlap/)
python 6_tissue_statistics.py  # Count loci that are both QTL and exQTL, generate "DAMs_Sites_statistic.txt"

## Enrichment analysis
python Enrichment.py  # Calculate enrichment factors


## Comparative analysis of mQTL and lnQTL
## lnQTL quantity statistics: In /home/phliu/mQTLmolQTLoverlap/lnQTLoverlap/ folder
python 1_extract.py  # Merge significant lnQTLs from 28 tissues, generate "lnQTL_combined_data.txt"
python 11.eQTLquchongfu.py  # Remove duplicates, generate "lnQTL_combined_data_unique.txt"
awk -F'_' '{print $1 "\t" substr($0, index($0, $2))}' lnQTL_combined_data_unique.txt > lnQTL_separated_columns.txt
python 10_eQTLnumber.py  # Count lnQTL numbers per tissue, generate "lnQTL_feature_counts.txt" (delete first row)

## mQTL quantity statistics:
python 4_MarkingSite.py  # Annotate loci with lnQTL tissue associations (Code in /home/phliu/mQTLmolQTLoverlap/, results in /home/phliu/mQTLmolQTLoverlap/lnQTLoverlap/)
python 6_tissue_statistics.py  # Count loci that are both QTL and lnQTL, generate "DAMs_Sites_statistic.txt"

## Enrichment analysis
python Enrichment.py  # Calculate enrichment factors


## Comparative analysis of mQTL and sQTL
## sQTL quantity statistics: In /home/phliu/mQTLmolQTLoverlap/sQTLoverlap/ folder
python 1_extract.py  # Merge significant sQTLs from 28 tissues, generate "sQTL_combined_data.txt"
python 11.eQTLquchongfu.py  # Remove duplicates, generate "sQTL_combined_data_unique.txt"
awk -F'_' '{print $1 "\t" substr($0, index($0, $2))}' sQTL_combined_data_unique.txt > sQTL_separated_columns.txt
python 10_eQTLnumber.py  # Count sQTL numbers per tissue, generate "sQTL_feature_counts.txt" (delete first row)

## mQTL quantity statistics:
python 4_MarkingSite.py  # Annotate loci with sQTL tissue associations (Code in /home/phliu/mQTLmolQTLoverlap/, results in /home/phliu/mQTLmolQTLoverlap/sQTLoverlap/)
python 6_tissue_statistics.py  # Count loci that are both QTL and sQTL, generate "DAMs_Sites_statistic.txt"

## Enrichment analysis
python Enrichment.py  # Calculate enrichment factors


## Visualization of results
setwd("/Users/liupeihao/Desktop")  
library("ggplot2")  
data <- read.csv("coloc.csv", header = TRUE)  
head(data)  
library(ggpubr)
new_order <- c("Brain", "Cerebellum", "Hypothalamus", "Retina", "Pituitary", "Thymus", "Adipose", "Leukocytes", "Macrophage", "Bursa", "Spleen", "Lung", "Trachea", "Smallintestine", "Jejunum", "Duodenum", "Ileum", "Cecum", "Liver", "Kidney", "Ovary", "Testis", "Oviduct", "Heart", "Skin", "Embryo", "Muscle", "Blood")  

p1 <- ggplot(data, aes(x = Tissue, y = MolQTL)) +  
  geom_point(aes(size = log10(`Enriched_Count`), fill = log10(`Enrichment_Fold`)),  
             shape = 21, color = "black", stroke = 0.6) +  
  scale_size(range = c(0, 6),   
             name = expression(atop("log"[10], "Loci Number"))) +  
  scale_fill_gradientn(colors = c( "#68A7BE", 'gray',"#EE7E77"),  
                       name = expression(atop("log"[10], "Enrichment Fold"))) +
  theme(axis.text.x = element_text(color = 'black', size = 11, angle = 270, hjust = 0, vjust = 0.5),  
        axis.text.y = element_text(color = 'black', size = 11),  
        axis.title.x = element_text(color = 'black', size = 15),  
        axis.title.y = element_text(color = 'black', size = 15),  
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
        panel.grid.minor = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),  
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),  
        legend.position = "right",
        legend.title = element_text(hjust = 0), 
        legend.text = element_text(hjust = 0)) + 
  scale_x_discrete(limits = new_order) +   
  labs(x = '', y = '') +  
  guides(fill = guide_colorbar(title = expression(atop("log"[10], "(Enrichment Fold)"))),  
         size = guide_legend(title = expression(atop("log"[10], "(Loci Number)")))) 
p1


## Differential expression of genes corresponding to pleiotropy mQTLs
## Extraction of loci
awk -F'\t' '$2 == "Liver" {print $1}' DAMs_Sites_new_sQTL.txt > Liver_sQTL.txt
awk -F'\t' '$2 == "Liver" {print $1}' DAMs_Sites_new_lnQTL.txt > Liver_lnQTL.txt
awk -F'\t' '$2 == "Liver" {print $1}' DAMs_Sites_new_exQTL.txt > Liver_exQTL.txt
awk -F'\t' '$2 == "Liver" {print $1}' DAMs_Sites_new_3aQTL.txt > Liver_3aQTL.txt
awk -F'\t' '$2 == "Liver" {print $1}' DAMs_Sites_new_eQTL.txt > Liver_eQTL.txt

## Convert the site into a physical location format
Run the tiquweidian.pl code to match sequentially

## Preparation of Annotation Files
setwd("/Users/liupeihao/Desktop")
BiocManager::install("ChIPpeakAnno",update = FALSE)
library(ChIPpeakAnno)
options(scipen=100)
ann=read.table("Gallus_6.gff.txt",header=T,sep="\t") 
ann=with(ann, GRanges(as.character(Chr),IRanges(as.numeric(Gene_start), as.numeric(Gene_end)),id = as.character(Gene_ID)))
head(ann)
peak=read.table("Liver_eQTL_annotation.txt",header=T,sep="\t")
region=100 
peak$BIN_START=peak$BIN_START-region
peak$BIN_END=peak$BIN_END+region
peak <- peak[complete.cases(peak),]
peak <- with(peak, GRanges(as.character(CHR), IRanges(as.numeric(BIN_START) , as.numeric(BIN_END)), id = as.character(peak_ID)))
o = findOverlaps(ann,peak)
results=cbind(as.data.frame(ann[queryHits(o)]),as.data.frame(peak[subjectHits(o)]))
results2=cbind(results[,12],results[,7],results[,8:9],results[,6])
names(results2)= c("peak_ID","CHR","BIN_START","BIN_END","QTL_ID;Gene_name")
results2$BIN_START=results2$BIN_START+region
results2$BIN_END=results2$BIN_END-region
write.table(results2,"eQTL_annotation.txt",sep="\t",quote=F,row.names=F)

## Extract gene expression data
Run the expressiontiqu.exe code to extract gene expression data from the gene expression matrix


## DEseq2 calculation of differentially expressed genes
setwd("/Users/liupeihao/Desktop")

BiocManager::install("DESeq2")
library(DESeq2)

install.packages("dplyr")
library(dplyr)

countData <- read.csv("sQTL_geneexpression.csv", header=TRUE, row.names=1)
head(countData)
sampleData <- read.csv("expressionGroup.csv", header=TRUE, row.names=1)
head(sampleData)
countData1 <- countData[rowSums(countData) != 0, ]
sampleData$group <- as.factor(sampleData$group)
dds <- DESeqDataSetFromMatrix(countData = countData1, colData = sampleData, design = ~ group)
dds <- DESeq(dds)
res <- results(dds)
res_df <- as.data.frame(res)
res_df <- res_df %>%
  mutate(group = case_when(
    log2FoldChange >= 0.263 & padj <= 0.05 ~ "UP",
    log2FoldChange <= -0.263 & padj <= 0.05 ~ "Down",
    TRUE ~ "not_change"))
table(res_df$group)
write.csv(res_df, file = "sQTL_diffexpre.csv", quote = FALSE)


countData <- read.csv("lnQTL_geneexpression.csv", header=TRUE, row.names=1)
head(countData)
sampleData <- read.csv("expressionGroup.csv", header=TRUE, row.names=1)
head(sampleData)
countData1 <- countData[rowSums(countData) != 0, ]
sampleData$group <- as.factor(sampleData$group)
dds <- DESeqDataSetFromMatrix(countData = countData1, colData = sampleData, design = ~ group)
dds <- DESeq(dds)
res <- results(dds)
res_df <- as.data.frame(res)
res_df <- res_df %>%
  mutate(group = case_when(
    log2FoldChange >= 0.263 & padj <= 0.05 ~ "UP",
    log2FoldChange <= -0.263 & padj <= 0.05 ~ "Down",
    TRUE ~ "not_change"))
table(res_df$group)
write.csv(res_df, file = "lnQTL_diffexpre.csv", quote = FALSE)



countData <- read.csv("eQTL_geneexpression.csv", header=TRUE, row.names=1)
head(countData)
sampleData <- read.csv("expressionGroup.csv", header=TRUE, row.names=1)
head(sampleData)
countData1 <- countData[rowSums(countData) != 0, ]
sampleData$group <- as.factor(sampleData$group)
dds <- DESeqDataSetFromMatrix(countData = countData1, colData = sampleData, design = ~ group)
dds <- DESeq(dds)
res <- results(dds)
res_df <- as.data.frame(res)
res_df <- res_df %>%
  mutate(group = case_when(
    log2FoldChange >= 0.263 & padj <= 0.05 ~ "UP",
    log2FoldChange <= -0.263 & padj <= 0.05 ~ "Down",
    TRUE ~ "not_change"))
table(res_df$group)
write.csv(res_df, file = "eQTL_diffexpre.csv", quote = FALSE)



countData <- read.csv("exQTL_geneexpression.csv", header=TRUE, row.names=1)
head(countData)
sampleData <- read.csv("expressionGroup.csv", header=TRUE, row.names=1)
head(sampleData)
countData1 <- countData[rowSums(countData) != 0, ]
sampleData$group <- as.factor(sampleData$group)
dds <- DESeqDataSetFromMatrix(countData = countData1, colData = sampleData, design = ~ group)
dds <- DESeq(dds)
res <- results(dds)
res_df <- as.data.frame(res)
res_df <- res_df %>%
  mutate(group = case_when(
    log2FoldChange >= 0.263 & padj <= 0.05 ~ "UP",
    log2FoldChange <= -0.263 & padj <= 0.05 ~ "Down",
    TRUE ~ "not_change"))
table(res_df$group)
write.csv(res_df, file = "exQTL_diffexpre.csv", quote = FALSE)


countData <- read.csv("3aQTL_geneexpression.csv", header=TRUE, row.names=1)
head(countData)
sampleData <- read.csv("expressionGroup.csv", header=TRUE, row.names=1)
head(sampleData)
countData1 <- countData[rowSums(countData) != 0, ]
sampleData$group <- as.factor(sampleData$group)
dds <- DESeqDataSetFromMatrix(countData = countData1, colData = sampleData, design = ~ group)
dds <- DESeq(dds)
res <- results(dds)
res_df <- as.data.frame(res)
res_df <- res_df %>%
  mutate(group = case_when(
    log2FoldChange >= 0.263 & padj <= 0.05 ~ "UP",
    log2FoldChange <= -0.263 & padj <= 0.05 ~ "Down",
    TRUE ~ "not_change"))
table(res_df$group)
write.csv(res_df, file = "3aQTL_diffexpre.csv", quote = FALSE)


##Analysis of eGene expression corresponding to eQTL
Run the eQTLGene. py code to extract the genes corresponding to eQTL and generate the eQTLGene. Diver. txt file
Run the sQTLGene. py code to extract the genes corresponding to sQTL and generate the sQTLGene_Liver.exe file
Run the 3aQTLGene. py code to extract the genes corresponding to 3aQTL and generate the 3aQTLGene. Diver. txt file
Run the exQTLGene. py code to extract the genes corresponding to exQTL and generate the exQTLGene_Liver.exe file

Run the expressiontiqu.exe code sequentially to extract gene expression data from the gene expression matrix

##Perform differential analysis using DEseq2
countData <- read.csv("eQTLGene_geneexpression.csv", header=TRUE, row.names=1)
head(countData)
sampleData <- read.csv("expressionGroup.csv", header=TRUE, row.names=1)
head(sampleData)
countData1 <- countData[rowSums(countData) != 0, ]
sampleData$group <- as.factor(sampleData$group)
dds <- DESeqDataSetFromMatrix(countData = countData1, colData = sampleData, design = ~ group)
dds <- DESeq(dds)
res <- results(dds)
res_df <- as.data.frame(res)
res_df <- res_df %>%
  mutate(group = case_when(
    log2FoldChange >= 0.263 & padj <= 0.05 ~ "UP",
    log2FoldChange <= -0.263 & padj <= 0.05 ~ "Down",
    TRUE ~ "not_change"))
table(res_df$group)
write.csv(res_df, file = "eQTL_diffexpre.csv", quote = FALSE)


countData <- read.csv("exQTLGene_geneexpression.csv", header=TRUE, row.names=1)
head(countData)
sampleData <- read.csv("expressionGroup.csv", header=TRUE, row.names=1)
head(sampleData)
countData1 <- countData[rowSums(countData) != 0, ]
sampleData$group <- as.factor(sampleData$group)
dds <- DESeqDataSetFromMatrix(countData = countData1, colData = sampleData, design = ~ group)
dds <- DESeq(dds)
res <- results(dds)
res_df <- as.data.frame(res)
res_df <- res_df %>%
  mutate(group = case_when(
    log2FoldChange >= 0.263 & padj <= 0.05 ~ "UP",
    log2FoldChange <= -0.263 & padj <= 0.05 ~ "Down",
    TRUE ~ "not_change"))
table(res_df$group)
write.csv(res_df, file = "exQTL_diffexpre.csv", quote = FALSE)


countData <- read.csv("3aQTLGene_geneexpression.csv", header=TRUE, row.names=1)
head(countData)
sampleData <- read.csv("expressionGroup.csv", header=TRUE, row.names=1)
head(sampleData)
countData1 <- countData[rowSums(countData) != 0, ]
sampleData$group <- as.factor(sampleData$group)
dds <- DESeqDataSetFromMatrix(countData = countData1, colData = sampleData, design = ~ group)
dds <- DESeq(dds)
res <- results(dds)
res_df <- as.data.frame(res)
res_df <- res_df %>%
  mutate(group = case_when(
    log2FoldChange >= 0.263 & padj <= 0.05 ~ "UP",
    log2FoldChange <= -0.263 & padj <= 0.05 ~ "Down",
    TRUE ~ "not_change"))
table(res_df$group)
write.csv(res_df, file = "3aQTL_diffexpre.csv", quote = FALSE)



countData <- read.csv("sQTLGene_geneexpression.csv", header=TRUE, row.names=1)
head(countData)
sampleData <- read.csv("expressionGroup.csv", header=TRUE, row.names=1)
head(sampleData)
countData1 <- countData[rowSums(countData) != 0, ]
sampleData$group <- as.factor(sampleData$group)
dds <- DESeqDataSetFromMatrix(countData = countData1, colData = sampleData, design = ~ group)
dds <- DESeq(dds)
res <- results(dds)
res_df <- as.data.frame(res)
res_df <- res_df %>%
  mutate(group = case_when(
    log2FoldChange >= 0.263 & padj <= 0.05 ~ "UP",
    log2FoldChange <= -0.263 & padj <= 0.05 ~ "Down",
    TRUE ~ "not_change"))
table(res_df$group)
write.csv(res_df, file = "sQTL_diffexpre.csv", quote = FALSE)



## ## Calculation and visualization of Lambda values
## Python calculates Lambda values
import pandas as pd
from scipy.stats import norm
import os
def calculate_lambda(file_path):
    try:
        data = pd.read_csv(file_path, sep='\t')
        p_value = data['p_wald']
        z = norm.ppf(p_value / 2)
        lambda_val = round(median_nan(z**2) / 0.454, 3)
        return lambda_val
    except Exception as e:
        print(f"Error processing {file_path}: {e}")
        return None
def median_nan(lst):
    return pd.Series(lst).dropna().median()
with open('newname.csv', 'r') as file:
    filenames = file.read().splitlines()
output_filename = 'all_lambda_results.txt'
with open(output_filename, 'w') as outfile:
    for filename in filenames:
        full_path = os.path.abspath(filename) 
        print(f"Processing file: {full_path}")
        lambda_value = calculate_lambda(full_path)
        if lambda_value is not None:
            outfile.write(f"{os.path.basename(filename)}: {lambda_value}\n")
            print(f"Processed {filename} and saved lambda to {output_filename}")
        else:
            print(f"Failed to process {filename}")
print(f"All lambda results saved to {output_filename}")


## Visualization of results
setwd("/Users/liupeihao/Desktop")
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra) 

## gene different expressed
data<-read.csv("geneDEG.csv")
head(data)

p1 <- ggplot(data, aes(x = padj, y = FoldChange, color = Group, fill = Type)) +  
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.8, fill = "#FEECE7", alpha = 0.6) +  
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1.2, ymax = Inf, fill = "#DEEEED", alpha = 0.6) + 
  geom_point(shape = 21, size = 2.5, color = "black") +   
  scale_fill_manual(values = c("#68A7BE", "#EE7E77","#F3E5B4", "#B9DAC5")) +  
  xlab("p-adjust") + ylab("Fold Change") + 
  theme(  
    axis.title = element_text(size = 11, colour = 'black'),  
    axis.text = element_text(size = 8, colour = 'black'),   
    axis.ticks = element_line(color = "black"),  
    plot.title = element_text(hjust = 0.5),  
    plot.margin = unit(c(1, 1, 1, 1), 'cm'),
    panel.background = element_blank(),   
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right") +   
  scale_x_continuous(limits = c(0, 0.05)) +  
  scale_y_continuous(limits = c(0, 2)) +  
  geom_hline(yintercept = 1.2, color = "grey", linetype = "dashed", size = 0.5) +  
  geom_hline(yintercept = 0.8, color = "grey", linetype = "dashed", size = 0.5) +     
  guides(fill = guide_legend(title = "Type"), color = guide_legend(title = NULL))
p1


## gene different expressed
data<-read.csv("molgeneDEG.csv")
head(data)

p1 <- ggplot(data, aes(x = padj, y = FoldChange, color = Group, fill = Type)) +  
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.8, fill = "#FEECE7", alpha = 0.6) +  
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 1.2, ymax = Inf, fill = "#DEEEED", alpha = 0.6) + 
  geom_point(shape = 21, size = 2.5, color = "black") +   
  scale_fill_manual(values = c("#68A7BE", "#EE7E77","#F3E5B4", "#B9DAC5")) +  
  xlab("p-adjust") + ylab("Fold Change") + 
  theme(  
    axis.title = element_text(size = 11, colour = 'black'),  
    axis.text = element_text(size = 8, colour = 'black'),   
    axis.ticks = element_line(color = "black"),  
    plot.title = element_text(hjust = 0.5),  
    plot.margin = unit(c(1, 1, 1, 1), 'cm'),
    panel.background = element_blank(),   
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right") +   
  scale_x_continuous(limits = c(0, 0.05)) +  
  scale_y_continuous(limits = c(0, 5), 
                     breaks = seq(0, 5, by = 1), 
                     labels = scales::number_format(accuracy = 0.1)) +
  geom_hline(yintercept = 1.2, color = "grey", linetype = "dashed", size = 0.5) +  
  geom_hline(yintercept = 0.8, color = "grey", linetype = "dashed", size = 0.5) +     
  guides(fill = guide_legend(title = "Type"), color = guide_legend(title = NULL))
p1



## heritability estimated by GCTA
## Lambda and Heritability Result visualization
setwd("/Users/liupeihao/Desktop")
install.packages("ggplot2")
library(ggplot2)
library(ggtext)
library(dplyr)

phe <- read.csv("h2lambda.csv")
phe$newSuperClass <- as.factor(phe$newSuperClass)

desired_order <- c("Lipids and lipid-like molecules","Organic acids and derivatives","Organoheterocyclic compounds","Organic oxygen compounds",  
  "Benzenoids","Phenylpropanoids and polyketides","Others","Unclassified","Nucleosides, nucleotides, and analogues")
desired_order <- desired_order[desired_order %in% levels(phe$newSuperClass)]
phe$newSuperClass <- factor(phe$newSuperClass, levels = desired_order)
class_colors <- c("Benzenoids" = "#CCEBC5","Lipids and lipid-like molecules" = "#9CD1C7","Nucleosides, nucleotides, and analogues" = "#F4CFE4","Organic acids and derivatives" = "#BEBAD8",
  "Organic oxygen compounds" = "#BCDD78","Organoheterocyclic compounds" = "#8AB0D0","Phenylpropanoids and polyketides" = "#FFFFBB","Unclassified" = "#F3B76F","Others" = "#EB8677")


p1 <- ggplot(phe, aes(x = newSuperClass, y = lambda, fill = newSuperClass)) +  
  geom_boxplot(width = 0.6, color = "black", size = 0.4, outlier.size = 0.5) +  
  scale_fill_manual(values = class_colors) + 
  labs(x = "Super Class", y = "Lambda") +   
  theme_bw() +  
  theme(  
    legend.position = "none",  
    legend.title = element_blank(),  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(1, 1, 1, 1, 'cm'),  
    axis.text.y = element_markdown(color = "black", size = 8, margin = margin(t = -2)), 
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),
    axis.text.x = element_markdown(color = "black", size = 8) 
  ) +   
  scale_y_continuous(limits = c(0.8, 1.08), expand = c(0.01, 0.01), breaks = seq(0.8, 1.08, 0.07)) +  
  geom_hline(yintercept = 0.95, color = "#D1287A", linetype = "dashed", size = 0.5) +  
  geom_hline(yintercept = 1.05, color = "#D1287A", linetype = "dashed", size = 0.5) +  
  scale_x_discrete(labels = function(x) {  
    color_values <- class_colors[x]  
    paste0("<span style='font-size:24pt; color:", color_values, "; border: 1px solid black;'>&#9679;</span>")
  }) 
p1
 

p1 <- ggplot(phe, aes(x = newSuperClass, y = heritablity, fill = newSuperClass)) +  
  geom_boxplot(width = 0.6, color = "black", size = 0.4, outlier.size = 0.5) +  
  scale_fill_manual(values = class_colors) + 
  labs(x = "Super Class", y = "Heritability") +   
  theme_bw() +  
  theme(  
    legend.position = "none",  
    legend.title = element_blank(),  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(1, 1, 1, 1, 'cm'),  
    axis.text.y = element_markdown(color = "black", size = 8, margin = margin(t = -2)), 
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),
    axis.text.x = element_markdown(color = "black", size = 8)  
  ) +   
  scale_y_continuous(limits = c(0, 1), expand = c(0.02, 0.02), breaks = seq(0, 1, 0.2)) + 
    geom_hline(yintercept = 0.2, color = "#D1287A", linetype = "dashed", size = 0.5) +   
  scale_x_discrete(labels = function(x) {  
    color_values <- class_colors[x]  
    paste0("<span style='font-size:24pt; color:", color_values, "; border: 1px solid black;'>&#9679;</span>")
  }) 
p1
 


## Functional analysis of QTLs in Animal QTLdb
## Run traitsummary.py to count loci per trait category
## Visualization in R
setwd("/Users/liupeihao/Desktop")
library(ggplot2)
library(ggforce)
library(grid) 
library(cowplot)
library(tidyverse) 

# Read trait category data
data <- read.table("TraitCategory_Counts.txt", header = TRUE, sep = "\t")
head(data)
data$TraitCategory <- factor(data$TraitCategory, levels = unique(data$TraitCategory)) 

# Create bar plot
p1 <- ggplot(data, aes(x = TraitCategory, y = Count/1000)) +   
     geom_bar(stat = "identity", width = 0.8, fill = "#FEDFB2", colour = "#000000") +   
     labs(x = "", y = "mQTL Count (Thousands)")  +  
     theme(  
        axis.title = element_text(size = 11, colour = 'black'),  
        axis.text.x = element_text(size = 8, colour = 'black'),  
        axis.text.y = element_text(size = 9, colour = 'black'),   
        axis.line = element_line(size = 0.5, colour = 'black'),  
        axis.ticks = element_line(color = "black"),  
        plot.title = element_text(hjust = 0.5),  
        plot.margin = margin(0.5, 0.5, 0.5, 0, "cm"),  
        panel.background = element_blank(),   
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +
     coord_flip()+scale_y_continuous(limits = c(0, 6), expand = c(0.02, 0.02), breaks = seq(0, 6, 2)) 
p1

## Pie chart for QTL proportions
phe <- read.csv("mQTLQTL.csv")
head(phe)
p2 <- ggplot(phe, aes(x = "", y = n, fill = Color)) + 
  geom_col(color = "black", linewidth = 0.5, show.legend = FALSE) +  
  geom_text(aes(label = paste0(prop, "%")), position = position_stack(vjust = 0.5), size = 6, family = "Arial") + 
  scale_fill_manual(values = c("#BABABA", "#FEDFB2")) + 
  coord_polar("y", start = 0) + 
  theme_void() +
  theme(text = element_text(size = 6),legend.position = "none")
p2

p3 <- ggdraw() +draw_plot(p1) + 
  draw_plot(p2, x = 0.6, y = 0.28, width = 0.45, height = 0.45)
p3



## Group Effect Testing
## Model construction and ANOVA in /home/phliu/DAMsgroupeffect folder

# Step 1: Extract significant loci
bash extract.sh  # Extract significant loci

# Step 2: Extract genotype data for significant loci
bash extractloci.sh  # Extract genotypes from binary files

# Step 3: LD-based filtering to remove highly linked sites
# Criteria: --indep-pairwise 50 10 0.1
bash LDshaixuan.sh  # Perform LD pruning

# Step 4: Re-extract independent loci after filtering
bash newextract.sh  # Extract genotypes for independent significant loci

# Step 5: Convert binary files to ped and map format
bash zhuanhuan.sh  # Convert to ped/map format

# Step 6: Remove first 6 columns from ped files
bash quqian6lie.sh  # Keep only genotype information

# The following steps in "/home/phliu/DAMsgroupeffect/modified_ped_files"
# Step 7: Merge genotypes across loci
perl merge.pl  # Combine genotypes for all loci

# Step 8: Transform genotypes to numeric coding
perl transform.pl  # Convert to numeric format
# Add column names QTL1, QTL2, ... based on number of loci

# Step 9: Add sample IDs to genotype files
python tianjiaID.py  # Add ID column to genotype data


## Phenotype Data Processing
# Split consolidated phenotype file into individual metabolite tables
python PHEchaifen.py  # Generate individual CSV files per metabolite

# Set R path
export PATH=$PATH:/usr/lib64/R/bin 

## Model Fitting and ANOVA
# Step 10: Fit models and perform ANOVA
Rscript lmnew.r  # Processes each metabolite, outputs ANOVA p-values and F-statistics

# Step 11: Compare models between H and L groups
Rscript lmcompare.r  # Builds separate models using random 80 samples from H/L groups
# Results in meta..._results_HL.txt files, summarized in HL_summary_results.tsv

# Step 12: Summarize QTL parameters across models
python HLQTLsummary.py  # Consolidates QTL parameters from both models
# Generates meta..._qtl_summary_HL.txt files

# Step 13: Filter significant QTLs
perl QTL_extract.pl  # Extract significant loci from both models
# Outputs filtered_meta..._qtl_summary_HL.txt files

# Step 14: Count significant QTLs
perl QTLtongji.pl  # Count significant QTLs in each group
# Generates HL_significant_counts_summary.txt


## Result visualization
setwd("/Users/liupeihao/Desktop")
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra) 

## Comparison of P-values
data<-read.csv("Modelcomparison.csv")
head(data)

data$NO_Group <- as.numeric(data$NO_Group)
data$Group <- as.numeric(data$Group)

p1 <- ggplot(data, aes(x = NO_Group, y = Group, color = group, fill = group)) +
      geom_point(shape = 21, size = 1.8) + 
      scale_color_manual(values = c("#EE7E77","#68A7BE")) + 
      scale_fill_manual(values = c("#EE7E77","#68A7BE")) +  
      xlab(expression("Model 1: -log"[10]*"(p-value)")) +   
      ylab(expression("Model 2: -log"[10]*"(p-value)")) +  
      theme(
        axis.title = element_text(size = 11, colour = 'black'),
        axis.text = element_text(size = 8, colour = 'black'), 
        axis.line = element_line(size = 0.5, colour = 'black'),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) + 
      scale_x_continuous(limits = c(0, 80)) +  
      scale_y_continuous(limits = c(0, 80)) +
      labs(color = "ANOVA:\np-value", fill = "ANOVA:\np-value") +  
      geom_abline(slope = 1, intercept = 0, color = "#D1287A", linetype = "dashed", size = 0.8) 
p1


## Comparison of F-statistic
setwd("/Users/liupeihao/Desktop")
library("tidyverse")
library("gghalves")
library("ggsci")
library("cowplot")
library("ggpubr")

data <- read.csv('HL_Fstatistic.csv',header = T)
head(data)
data$Group <- factor(data$Group, levels = c("Model 1", "Model 2"))
p1 <- ggplot(data, aes(x = Group, y = Fstatistic, fill = Group)) +
      geom_half_violin(data = filter(data,Group =='Model 1'),side ='l',nudge =0.2,  color ='black', trim = TRUE)+
      geom_half_violin(data = filter(data,Group =='Model 2'),side ='r',nudge =0.2,  color ='black', trim = TRUE)+
      geom_line(aes(group = Metabolite),lwd = 0.5, color = "#CCB78D")+
      geom_point(aes(group = Group),size = 2, position = position_dodge(0.3),shape = 21)+
      geom_boxplot(aes(group = Group), width = 0.3, size = 0.7,alpha =0.4,color = 'black',outlier.shape = NA)+
      scale_fill_manual(values = c("#68A7BE", "#EE7E77")) + 
      labs(x = '', y = 'F-statistic') +
      theme_classic(base_family = "Arial") +
      theme(
            axis.text.x = element_text(color = 'black', size = 10),
            axis.text.y = element_text(color = 'black', size = 8),
            axis.title = element_text(color = 'black', size = 11),
            axis.line = element_line(color = "black", size = 0.6),
            axis.ticks = element_line(color = "black", size = 0.6, lineend = "round"),
            plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
            legend.position = "none") +scale_y_continuous(limits = c(0, 18), breaks = seq(0, 18, by = 3))
p1


## P-value correlation analysis
data<-read.csv("HLcorreltaion.csv")
head(data)
p1 <- ggplot(data, aes(x = anovalgP, y = dmlgP)) +
      geom_point(aes(fill = VIP), shape = 21, size = 2) + 
      geom_smooth(method = "lm", color = "#174F6F", fill = "#DEEEED", size = 1, se = TRUE) +  
      stat_cor(method = "pearson", label.x = 0, label.y = 10, label.sep = "\n") +
      scale_fill_gradient(low = "#FEECE7", high = "#EE7E77") + 
      xlab(expression("ANOVA:-log"[10]*"(p-value)")) + ylab(expression("DAMs:-log"[10]*"(p-value)")) +
      theme(
        axis.title = element_text( size = 10, colour = 'black'),
        axis.text = element_text(size = 8, colour = 'black'), 
        axis.line = element_line(size = 0.5, colour = 'black'),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) + 
      scale_x_continuous(limits = c(0, 45)) +  
      scale_y_continuous(limits = c(0, 20))
p1



## The feature description of mQTL
## MAF Statistics for Different Locus Sets
# In /home/phliu/mQTL/significantsuggestive directory
python remove.py  # Remove DAMs mQTLs from all QTLs to prepare noDAMsmQTL.txt
python maftiqu.py  # Extract MAF for different locus categories, generating noDAMsmQTL_maf.txt and DAMsmQTL_maf.txt
python mafhebing.py  # Merge MAF files into DAMsmerged_maf.txt


## chCADD Score Statistics for Different Locus Sets
# In /home/phliu/chCADD-scores/meta directory
python CADDweidiantiqu.py  # Extract chromosome, position, and allele info from bim files (Chrom Position Ref Alt)

# Annotate SNPs with chCADD scores
python3 chCADD_annotate_SNP.py -i DAMslociPhastConsvcf.tsv -c 1,2 -p /home/phliu/chCADD-scores/chCADD-scores/chCADD.tsv.gz >> chCADD_DAMsmQTL.tsv
python3 chCADD_annotate_SNP.py -i noDAMsmQTLPhastConsvcf.tsv -c 1,2 -p /home/phliu/chCADD-scores/chCADD-scores/chCADD.tsv.gz >> chCADD_noDAMsmQTL.tsv

# Process chCADD results
awk 'NR==1 {print; next} {print $1 "_" $2, $5}' chCADD_DAMsmQTL.tsv > newchCADD_DAMsmQTL.tsv
awk 'NR==1 {print; next} {print $1 "_" $2, $5}' chCADD_noDAMsmQTL.tsv > newchCADD_noDAMsmQTL.tsv

python newchCADDhebing.py  # Generate newchCADD.txt file


## Conservation Score Statistics for Different Locus Sets
# In /home/phliu/snpEff/db/phastCons directory
python phastconstiqu.py  # Generate DAMsloci_phastCons.txt and noDAMsloci_phastCons.txt
python phastconshebing.py  # Generate DAMsphastCons_combined.txt file


## TSS-Distance Statistics for Different Loci
# Extract lncRNA information from bed file
awk 'BEGIN { FS=OFS="\t" } $7 == "lncRNA" { print $5, $1, $2, $3, $4 }' 6a_lncRNA_biotype.bed > 6a_lncRNA.bed

# Calculate distance to TSS
python TSSdistance.py  # Generate distance_to_tss.txt file

# Extract TSS distance information
awk 'NR>1 {print $1"_"$2, $6}' distance_to_tss.txt > id_distance.txt

# Extract TSS distance for different locus categories
python tssdistancetiqu.py  # Generate DAMsmQTL_distance.txt and noDAMsmQTL_distance.txt

# Merge TSS distance files
python newtssdistancehebing.py  # Generate tssdistanc_combined_DAMs.txt file


## Result visualization
setwd("/Users/liupeihao/Desktop")
library(ggplot2)
library(ggpubr)

data1 <- read.table('DAMsmerged_maf.txt',header = T)
head(data1)
colors <- c("#EE7E77","#68A7BE")
data1$Type <- factor(data1$type, levels = c("mQTL", "nomQTL"))  

p1 <- ggplot(data1, aes(x = type, y = maf, fill = type)) +
  geom_boxplot(width = 0.5, size = 0.4, color="black", outlier.size = 0.5) +  
  scale_fill_manual(values = colors) + 
  labs(x = '', y = 'MAF') +
  theme_classic() +
  ylim(0,0.6)+
  theme(
    axis.title.y = element_text(size = 11, color = 'black'),
    axis.text.x = element_text(size = 11,color = 'black'),
    axis.text.y = element_text(size = 8,  color = 'black'),
    axis.ticks.length = unit(0.095, "cm"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    panel.background = element_rect(fill = "white"),
    axis.line = element_blank())+
    geom_signif(
    comparisons = list(c("mQTL", "nomQTL")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = c(0.52),
    size = 0.5, color = "black", textsize = 4,  family = "Arial Black")+  
    scale_x_discrete(labels = c("mQTL" = "mQTL  ", "nomQTL" = "  non-mQTL"))   
p1


data2 <- read.table('DAMsphastCons_combined.txt',header = T)
head(data2)
colors <- c("#EE7E77","#68A7BE")
data2$type <- factor(data2$type, levels = c("mQTL", "nomQTL"))  

p2 <- ggplot(data2, aes(x = type, y = phastCons, fill = type)) +
  geom_boxplot(width = 0.5, size = 0.4, color="black", outlier.size = 0.2) +  
  scale_fill_manual(values = colors) + 
  labs(x = '', y = 'phastCons') +
  theme_classic() +
  coord_cartesian(ylim = c(0, 1.2)) +  
  scale_y_continuous(breaks = c(0, 0.4, 0.8, 1.2)) + 
  theme(
    axis.title.y = element_text(size = 11, color = 'black'),
    axis.text.x = element_text(size = 11,color = 'black'),
    axis.text.y = element_text(size = 8,  color = 'black'),
    axis.ticks.length = unit(0.095, "cm"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    panel.background = element_rect(fill = "white"),
    axis.line = element_blank())+
    geom_signif(
    comparisons = list(c("mQTL", "nomQTL")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = c(1.01),
    size = 0.5, color = "black", textsize = 4,  family = "Arial Black")+  
    scale_x_discrete(labels = c("mQTL" = "mQTL  ", "nomQTL" = "  non-mQTL"))
p2


data3 <- read.table('tssdistanc_combined_DAMs.txt',header = T)
head(data3)
colors <- c("#EE7E77","#68A7BE")
data3$type <- factor(data3$type, levels = c("mQTL", "nomQTL"))  

p3 <- ggplot(data3, aes(x = type, y = tssdistance/1000, fill = type)) +
  geom_boxplot(width = 0.5, size = 0.4, color="black", outlier.size = 0.2) +  
  scale_fill_manual(values = colors) + 
  labs(x = '', y = 'Distance to TSS(kb)') +
  theme_classic() +
  ylim(0,850)+
  theme(
    axis.title.y = element_text(size = 11, color = 'black'),
    axis.text.x = element_text(size = 11,color = 'black'),
    axis.text.y = element_text(size = 8,  color = 'black'),
    axis.ticks.length = unit(0.095, "cm"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    panel.background = element_rect(fill = "white"),
    axis.line = element_blank())+
    geom_signif(
    comparisons = list(c("mQTL", "nomQTL")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = c(750),
    size = 0.5, color = "black", textsize = 4,  family = "Arial Black")+  
    scale_x_discrete(labels = c("mQTL" = "mQTL  ", "nomQTL" = "  non-mQTL"))
p3



## Chromatin State Enrichment Analysis
# Step 1: Count loci in each chromatin state for mQTLs
python locistatesummary_xunhuan.py

# Step 2: Calculate enrichment factors, p-values, and adjusted p-values
python Simplyenrichmenanalysis.py

# Step 3: Consolidate enrichment results across metabolites and tissues
python ResultsMerge.py

## Result visualization
library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("patchwork")  
library(patchwork) 

plot.format=theme(plot.background=element_blank(),
                  panel.grid=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_rect(color="black",linewidth=1,fill=NA),
                  axis.line=element_blank(),
                  axis.ticks=element_line(color="black",linewidth=0.5),
                  axis.text.x=element_text(color="black",size=10),
                  axis.text.y=element_text(color="black",size=9),
                  axis.title=element_text(color="black",size=13),
                  plot.title=element_text(color="black",size=7),
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.text=element_text(color="black",size=7),
                  legend.title=element_text(color="black",size=7))

data <- read.csv('chrsate.csv',header = T)
head(data)
data_long <- melt(data, id.vars = "State", variable.name = "Tissue", value.name = "Value")
head(data_long)
data_long$State <- factor(data_long$State, levels = c("TssA", "TssAHet", "TxFlnK", "TxFlnkWk", "TxFlnkHet", "EnhA", "EnhAMe", "EnhAWk", "EnhAHet", "EnhPois", "ATAC_Is", "TssBiv", "Repr", "ReprWk", "Qui"))
p1 <- ggplot(data_long, aes(x = State, y = Value, color = State)) +
  geom_boxplot(aes(fill = State), width = 0.7, size = 0.5, color = "black") +
  geom_jitter(shape=21, size = 1.5, width = 0.1, aes(fill = State), color = "black") + 
  ## tat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") + 
  scale_color_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  scale_fill_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  labs(y = 'Enrichment Factor', x = 'Chromatin State') +  
  coord_flip() + 
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0.5, 0.6, 0.5, 0.5, 'cm'),
        axis.title.y = element_text(size = 11, color = 'black'), 
        axis.text.y = element_text(size = 8, color = 'black'), 
        axis.ticks.y = element_line(color = 'black'),             
        axis.title.x = element_text(size = 11, color = 'black'),  
        axis.text.x = element_text(size = 8, color = 'black'),  
        axis.ticks.x = element_line(color = 'black')) +           
  geom_hline(yintercept = 1, color = "#D1287A", linetype = "dashed", size = 0.5) +  
  scale_y_continuous(limits = c(-0.5, 3.6),
                     breaks = c(0, 1, 2, 3),
                     labels = c(0, 1, 2, 3),
                     expand = c(0, 0))
p1


p1 <- ggplot(data_long, aes(x = State, y = Value, color = State)) +
  geom_boxplot(aes(fill = State), width = 0.7, size = 0.5, color = "black") +
  geom_jitter(shape=21, size = 1.5, width = 0.1, aes(fill = State), color = "black") + 
  ## tat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") + 
  scale_color_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  scale_fill_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  labs(y = 'Enrichment Factor', x = 'Chromatin State') +  
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0.5, 0.6, 0.5, 0.5, 'cm'),
        axis.title.y = element_text(size = 11, color = 'black'), 
        axis.text.y = element_text(size = 8, color = 'black'), 
        axis.ticks.y = element_line(color = 'black'),             
        axis.title.x = element_text(size = 11, color = 'black'),  
        axis.text.x = element_text(size = 8, color = 'black'),  
        axis.ticks.x = element_line(color = 'black')) +           
  geom_hline(yintercept = 1, color = "#D1287A", linetype = "dashed", size = 0.5) +  
  scale_y_continuous(limits = c(-0.5, 3.6),
                     breaks = c(0, 1, 2, 3),
                     labels = c(0, 1, 2, 3),
                     expand = c(0, 0))
p1

p1 <- ggplot(data_long, aes(x = State, y = Value, color = State)) +
  geom_boxplot(aes(fill = State), width = 0.6, size = 0.5, color = "black") +
  geom_jitter(shape = 21, size = 1.5, width = 0.1, aes(fill = State), color = "black") + 
  scale_color_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  scale_fill_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  labs(y = 'Enrichment Factor', x = '') +  
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0.5, 0.6, 0.5, 0.5, 'cm'),
        axis.title.y = element_text(size = 11, color = 'black'), 
        axis.text.y = element_text(size = 8, color = 'black'), 
        axis.ticks.y = element_line(color = 'black'),             
        axis.title.x = element_text(size = 11, color = 'black'),  
        axis.text.x = element_text(size = 8, color = 'black', angle = 90, hjust = 1, vjust = 0.5),  # 旋转90度并调整对齐
        axis.ticks.x = element_line(color = 'black')) +           
  geom_hline(yintercept = 1, color = "#D1287A", linetype = "dashed", size = 0.5) +  
  scale_y_continuous(limits = c(-0.5, 3.6),
                     breaks = c(0, 1, 2, 3),
                     labels = c(0, 1, 2, 3),
                     expand = c(0, 0))


## SNPEff Annotation of Loci
# Step 1: Extract annotation information for mQTLs
python tiqu.py

## Statistical Analysis and Visualization of All mQTL Annotations
# Step 2: Extract mQTL information
python mQTLextract.py  # Generate mQTL_information.txt

# Step 3: Count occurrences of different variant types
python newpositionstatistic.py  # Generate mQTLvariant_counts.txt

## Result visualization
setwd("/Users/liupeihao/Desktop")
library(ggplot2)
library(ggforce)
library(grid) 

data <- read.table("alllociposition.txt", header = TRUE, sep = "\t")
head(data)
data$Region <- factor(data$Region, levels = unique(data$Region)) 
p1 <- ggplot(data, aes(x = Region, y = Ratio)) +   
     geom_bar(stat = "identity",  width = 0.8, fill = "#68A7BE", colour = "#000000") +   
     labs(x = "", y = "Proportion(%) of SNP")  +  
     theme(  
        axis.title = element_text(size = 11, colour = 'black'),  
        axis.text.x = element_text(size = 8, colour = 'black'),  
        axis.text.y = element_text(size = 9, colour = 'black'),   
        axis.line = element_line(size = 0.5, colour = 'black'),  
        axis.ticks = element_line(color = "black"),  
        plot.title = element_text(hjust = 0.5),  
        plot.margin = margin(1, 0, 0.5, 0.5, "cm"),  
        panel.background = element_blank(),   
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +
     coord_flip()
p1


data1 <- read.table("mQTLlociposition.txt", header = TRUE, sep = "\t")
head(data1)
data1$Region <- factor(data1$Region, levels = unique(data1$Region)) 
p2 <- ggplot(data1, aes(x = Region, y = Ratio)) +   
     geom_bar(stat = "identity",  width = 0.8, fill = "#EE7E77", colour = "#000000") +   
     labs(x = "", y = "")  +  
     theme(  
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),   
        axis.text.x = element_text(size = 8, colour = 'black'),   
        axis.line = element_line(size = 0.5, colour = 'black'),  
        axis.ticks.x = element_line(color = "black"),  
        plot.title = element_text(hjust = 0.5),  
        plot.margin = margin(1, 0.5, 0.5, 0, "cm"),  
        panel.background = element_blank(),   
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +  
     coord_flip()
p2

p=p1+p2
p



## Correlation between heritability and the number of QTLS
setwd("/Users/liupeihao/Desktop")
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra) 

data<-read.csv("h2QTLcount.csv")
head(data)

install.packages("coin")
library(coin)
spearman_test <- cor.test(data$locinumber, data$heritablity, method = "spearman")
spearman_test
# rho：0.2223965 ，p-value=0.006231

p1 <- ggplot(data, aes(x = heritablity, y = locinumber)) +
  geom_point(shape = 21, size = 2, fill="#FEDFB2",color="black") +
  geom_smooth(method = "lm",  linetype = "solid", color = "#D1287A",fill="#FF99DA") +  
  xlab("Heritability") + ylab("Number of mQTLs") + 
  theme(
    legend.position = "none",  
    axis.title = element_text(size = 11, colour = 'black'),
    axis.text = element_text(size = 8, colour = 'black'), 
    axis.line = element_line(size = 0.5, colour = 'black'),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(0, 1)) +  
  scale_y_continuous(limits = c(0, 25000))+
  guides(color = guide_legend(ncol = 1),  
         fill = guide_legend(ncol = 1)) 
p1



## chCADD Score Statistics for Different Locus Sets
# In /home/phliu/chCADD-scores/meta directory

# Step 1: Extract chromosome, position and allele information from bim files
python CADDweidiantiqu.py  # Output format: Chrom Position Ref Alt

# Step 2: Annotate SNPs with chCADD scores
python3 chCADD_annotate_SNP.py -i DAMslociPhastConsvcf.tsv -c 1,2 -p /home/phliu/chCADD-scores/chCADD-scores/chCADD.tsv.gz >> chCADD_DAMsmQTL.tsv
python3 chCADD_annotate_SNP.py -i noDAMsmQTLPhastConsvcf.tsv -c 1,2 -p /home/phliu/chCADD-scores/chCADD-scores/chCADD.tsv.gz >> chCADD_noDAMsmQTL.tsv

# Step 3: Process chCADD results
awk 'NR==1 {print; next} {print $1 "_" $2, $5}' chCADD_DAMsmQTL.tsv > newchCADD_DAMsmQTL.tsv
awk 'NR==1 {print; next} {print $1 "_" $2, $5}' chCADD_noDAMsmQTL.tsv > newchCADD_noDAMsmQTL.tsv

# Step 4: Merge chCADD results
python newchCADDhebing.py  # Generates newchCADD.txt file

## Result visualization
setwd("/Users/liupeihao/Desktop")
install.packages("ggplot2")
library(ggplot2)
phe<-read.csv("chCADD.csv")
head(phe)
phe$class <- as.factor(phe$class)
str(phe$class)
str(phe$chCADD)
class_colors <- c("Benzenoids" = "#CCEBC5","Lipids and lipid-like molecules" = "#9CD1C7","Nucleosides, nucleotides, and analogues" = "#F4CFE4","Organic acids and derivatives" = "#BEBAD8",
                  "Organic oxygen compounds" = "#BCDD78","Organoheterocyclic compounds" = "#8AB0D0","Phenylpropanoids and polyketides" = "#FFFFBB","Unclassified" = "#F3B76F","Other" = "#EB8677")
desired_order <- c("Lipids and lipid-like molecules","Organic acids and derivatives","Organoheterocyclic compounds", "Organic oxygen compounds",  
                   "Benzenoids","Phenylpropanoids and polyketides","Other","Unclassified", "Nucleosides, nucleotides, and analogues")  
phe$class <- factor(phe$class, levels = desired_order)  
p1 <- ggplot(phe, aes(x = class, y =chCADD, fill = class)) + 
  geom_boxplot(width = 0.6,color = "black", size = 0.4,outlier.size=0.001) +  
  scale_fill_manual(values = class_colors) + 
  theme(  
    legend.position = "none",  
    legend.title = element_blank(),  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(1, 1, 1, 1, 'cm'),  
    axis.text.x = element_markdown(size = 12,margin = margin(t = -2)), 
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),  
    axis.text.y = element_text(color = "black", size = 8)) + 
  scale_y_continuous(limits = c(0, 40))+   
  xlab("Super Calss") + ylab("chCADD") +  
  scale_x_discrete(labels = function(x) {  
    color_values <- class_colors[x]  
    paste0("<span style='font-size:24pt; color:", color_values, "; border: 2px solid black;'>&#9679;</span>")})  
p1



## Heritability calculation
## Comparison of heritability between groups H and L
## Comparison of heritability calculation with different sample sizes in Group L
## Enable GCTA to perform the calculation
python GCTA_L_h2.py   Run the GCTA_L_h2.py code to calculate the heritability of all individual-differential metabolites in group L
python GCTA_L80_h2.py   Run the GCTA_L80_h2.py code to calculate the heritability of 80 differentially expressed metabolites in group L
python GCTA_L180_h2.py   Run the GCTA_L180_h2.py code to calculate the heritability of 180 differentially expressed metabolites in group L
python GCTA_L280_h2.py   Run the GCTA_L280_h2.py code to calculate the heritability of 280 differentially expressed metabolites in group L

python GCTA_H_h2.py   Run the GCTA_H_h2.py code to calculate the heritability of the individual differential metabolites in group H


## The heritability explained by QTLS of different sets
## Construct the kinship matrix of all loci
/home/cgbl/biosoft/gcta/gcta_1.93.2beta/gcta64 --bfile /home/phliu/DAMsgroupeffect/Heritability/allloci/peihao --autosome-num 35 --autosome  --make-grm-alg 1  --thread-num 30 --out allloci
## Covariate file
cov.txt
## Heritability calculation
运行allGCTA_heritability.sh代码，依次计算建议位点的遗传力
## Resultes Summary
python Heritability_summary-VG.py

## Build the kinship matrix of mQTL
Run the GCTAgrm.sh code to build the kinship matrix in sequence
## Preparation of phenotypic documents
Run the Pheprepare.py code and prepare the phenotype files in sequence to generate the metaxxxphe.txt series files
## Covariate file
cov.txt
## Calculation of heritability
Run the GCTA_heritability.sh code to calculate the heritability of mQTL in sequence
/home/cgbl/biosoft/gcta/gcta_1.93.2beta/gcta64 --grm newmeta1031locigrm --pheno meta1031phe.txt --mpheno 1 --qcovar cov.txt --reml --thread-num 100 --out meta1031（不收敛）
## Resultes Summary
python Heritability_summary.py
python Heritability_summary-VG.py。将这个流程中的中文改为英文


##  Result visualization
setwd("/Users/liupeihao/Desktop")
library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("ggpubr")
library(ggpubr)
install.packages("ggsignif")
library(ggsignif)

plot.format=theme(plot.background=element_blank(),
                  panel.grid=element_blank(),
                  panel.background=element_blank(),
                  panel.border = element_blank(),  
                  axis.line = element_line(color = "black", size = 0.5),
                  axis.ticks=element_line(color="black",linewidth=0.5),
                  axis.text=element_text(color="black",size=8),
                  axis.title=element_text(color="black",size=11),
                  plot.title=element_text(color="black",size=7),
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.text=element_text(color="black",size=7),
                  legend.title=element_text(color="black",size=7))

## Comparison of heritability in L group gradient population
data <- read.csv('Lheritability.csv',header = T)
head(data)
data$Group <- factor(data$Group, levels = c("80", "180", "280", "All"))
p1 <- ggplot(data, aes(x = Group, y = heritability, color = Group)) +
  geom_violin(aes(fill = Group), width = 0.7, size = 0.5, color = "black") +  
  geom_boxplot(width = 0.15, outlier.shape = NA, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, aes(fill = Group),color = "black") + 
  scale_color_manual(values = c("#c8dfe7", "#7ab1c6", "#568db6", "#418198")) + 
  scale_fill_manual(values = c("#c8dfe7", "#7ab1c6", "#568db6", "#418198")) + 
  labs(x = 'Sample Sizes', y = 'Heritability')  +
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+
        scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, by = 0.25))+
  stat_compare_means(comparisons = list(c("80", "180"), c("180", "280"), c("280", "All")), 
      label = "p.signif",  
      method = "t.test",  
      size = 4, 
      position = position_dodge(0.8),label.y = 1.02)
p1


## Comparison of heritability in L and H group
data <- read.csv('LHheritability.csv',header = T)
head(data)
data$Group <- factor(data$Group, levels = c("L", "H"))
p1 <- ggplot(data, aes(x = Group, y = heritability, color = Group)) +
  geom_violin(aes(fill = Group), width = 0.7, size = 0.5, color = "black") +  
  geom_boxplot(width = 0.15, outlier.shape = NA, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, aes(fill = Group),color = "black") + 
  scale_color_manual(values = c("#68A7BE","#EE7E77")) + 
  scale_fill_manual(values = c("#68A7BE","#EE7E77")) + 
  labs(x = 'Group', y = 'Heritability')  +
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+
        scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, by = 0.25))+
  stat_compare_means(comparisons = list(c("L", "H")), 
      label = "p.signif",  
      method = "t.test",  
      size = 4, 
      position = position_dodge(0.8),label.y = 1.02)
p1


## Comparison of heritability estimated by QTL/mQTL
data <- read.csv('HeritabilityQTLmQTL.csv',header = T)
head(data)
data$Group <- factor(data$Group, levels = c("QTL", "mQTL"))
p1 <- ggplot(data, aes(x = Group, y = heritability, color = Group)) +
  geom_violin(aes(fill = Group), width = 0.7, size = 0.5, color = "black") +  
  geom_boxplot(width = 0.15, outlier.shape = NA, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, aes(fill = Group),color = "black") + 
  scale_color_manual(values = c("#68A7BE","#EE7E77")) + 
  scale_fill_manual(values = c("#68A7BE","#EE7E77")) + 
  labs(x = 'Group', y = 'Heritability')  +
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+
        scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, by = 0.25))+
  stat_compare_means(comparisons = list(c("QTL", "mQTL")), 
      label = "p.signif",  
      method = "t.test",  
      size = 4, 
      position = position_dodge(0.8),label.y = 1.02)
p1


## Comparison of Vg estimated by QTL/mQTL
data <- read.csv('VgQTLmQTL.csv',header = T)
head(data)
data$Group <- factor(data$Group, levels = c("QTL", "mQTL"))
p1 <- ggplot(data, aes(x = Group, y = Vg, color = Group)) +
  geom_violin(aes(fill = Group), width = 0.7, size = 0.5, color = "black") +  
  geom_boxplot(width = 0.15, outlier.shape = NA, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, aes(fill = Group),color = "black") + 
  scale_color_manual(values = c("#68A7BE","#EE7E77")) + 
  scale_fill_manual(values = c("#68A7BE","#EE7E77")) + 
  labs(x = 'Group', y = 'V(G)')  +
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))+
        scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 2))+
  stat_compare_means(comparisons = list(c("QTL", "mQTL")), 
      label = "p.signif",  
      method = "t.test",  
      size = 4, 
      position = position_dodge(0.8),label.y = 11)
p1



## Statistics of MAF
/home/cgbl/biosoft/plink1.9/plink --allow-extra-chr --chr-set 35 --bfile /home/phliu/17metaGWAS/L/L --freq --out /home/phliu/17metaGWAS/MAF/L_freq
/home/cgbl/biosoft/plink1.9/plink --allow-extra-chr --chr-set 35 --bfile /home/phliu/17metaGWAS/H/H --freq --out /home/phliu/17metaGWAS/MAF/H_freq

## Extract the MAFs with significant loci in different groups to generate Lmafmeta.... txt, Hmafmeta.... txt
export PATH=$PATH:/usr/lib64/R/bin 
Rscript MAFtiqu.r

## Summarize the results. Compile the results of groups L and H into four tables
python MAFsummary.py
生成Lmaf_summary.txt、Hmaf_summary.txt

##  Result visualization
setwd("/Users/liupeihao/Desktop")
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra) 
library(RColorBrewer)
install.packages("coin")
library(coin)

data<-read.csv("mafcorrelation.csv")
head(data)
spearman_test_result <- spearman_test(Lmaf ~ Hmaf, data = data, distribution = approximate(nresample= 9999))
spearman_test_result
#p-value < 1e-04
spearman_corr <- cor(data$Lmaf, data$Hmaf, method = "pearson")
print(spearman_corr)
#0.7880003

class_colors <- c("Benzenoids" = "#CCEBC5","Lipids and lipid-like molecules" = "#9CD1C7","Nucleosides, nucleotides, and analogues" = "#F4CFE4","Organic acids and derivatives" = "#BEBAD8",
                  "Organic oxygen compounds" = "#BCDD78","Organoheterocyclic compounds" = "#8AB0D0","Phenylpropanoids and polyketides" = "#FFFFBB","Unclassified" = "#F3B76F","Other" = "#EB8677")

p1 <- ggplot(data, aes(x = Lmaf, y = Hmaf, color = Superclass, fill = Superclass )) +
  geom_point(shape = 21, size = 2) +
  scale_color_manual(values = class_colors) + 
  scale_fill_manual(values = class_colors) +  
  xlab("MAF of L Group") + ylab("MAF of H Group") + 
  theme(
    axis.title = element_text(size = 12, colour = 'black'),  
    axis.text = element_text(size = 9, colour = 'black'),   
    axis.line = element_line(size = 0.5, colour = 'black'),  
    axis.ticks = element_line(color = "black"),  
    plot.title = element_text(hjust = 0.5, size = 12), 
    plot.margin = margin(1, 1, 1, 1, "cm"),  
    panel.background = element_blank(),   
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()) +   
  scale_x_continuous(limits = c(0, 0.5)) +  
  scale_y_continuous(limits = c(0, 0.5)) +  
  guides(color = guide_legend(ncol = 1),  
         fill = guide_legend(ncol = 1)) +  
  labs(color = "Metabolites", fill = "Metabolites") +  
  geom_abline(slope = 1, intercept = 0, color = "#060B6F", linetype = "dashed", size = 0.5) +  
  ggtitle(expression("Pearson's r = 0.79, p-value< 1e-04")) 
p1  



## Annotation and functional enrichment analysis of at least two metabolite-related sites
setwd("/Users/liupeihao/Desktop")
BiocManager::install("ChIPpeakAnno",update = FALSE)
library(ChIPpeakAnno)
options(scipen=100)
ann=read.table("Gallus_6.gff.txt",header=T,sep="\t") 
ann=with(ann, GRanges(as.character(Chr),IRanges(as.numeric(Gene_start), as.numeric(Gene_end)),id = as.character(Gene_ID)))
head(ann)
peak=read.table("DAMspartloci annotation.txt",header=T,sep="\t")
region=100 
peak$BIN_START=peak$BIN_START-region
peak$BIN_END=peak$BIN_END+region
peak <- peak[complete.cases(peak),]
peak <- with(peak, GRanges(as.character(CHR), IRanges(as.numeric(BIN_START) , as.numeric(BIN_END)), id = as.character(peak_ID)))
o = findOverlaps(ann,peak)
results=cbind(as.data.frame(ann[queryHits(o)]),as.data.frame(peak[subjectHits(o)]))
results2=cbind(results[,12],results[,7],results[,8:9],results[,6])
names(results2)= c("peak_ID","CHR","BIN_START","BIN_END","QTL_ID;Gene_name")
results2$BIN_START=results2$BIN_START+region
results2$BIN_END=results2$BIN_END-region
write.table(results2,"DAMs_partsite_annotation.txt",sep="\t",quote=F,row.names=F)

## Functional enrichment of candidate genes
setwd("/Users/liupeihao/Desktop")

install.packages('aPEAR')
library(aPEAR)
install.packages('BiocManager') 
BiocManager::install('clusterProfiler')
library(clusterProfiler)
BiocManager::install('org.Gg.eg.db')
library(org.Gg.eg.db)
library(DOSE)
library(ggplot2)
install.packages('cols4all')
library(cols4all)

gene_ids <- read.csv("geneID.csv", header = TRUE, stringsAsFactors = FALSE)

gene_ids <- as.character(gene_ids$EntrezGeneID)
gene_ids <- gene_ids[!is.na(gene_ids) & nzchar(gene_ids)]

# Conduct GO enrichment analysis
GO_all_diff <- enrichGO(gene = gene_ids, OrgDb = org.Gg.eg.db, keyType = 'ENTREZID', ont = "ALL",pAdjustMethod = "BH", pvalueCutoff  = 1, qvalueCutoff = 1, readable = TRUE)
head(GO_all_diff)
output_file <- "GO_results.csv"
write.csv(GO_all_diff, file = output_file, row.names = TRUE)

# Conduct KEGG enrichment analysis
kegg_enrich <- enrichKEGG(gene = gene_ids, organism = 'gga', keyType = 'ncbi-geneid', pAdjustMethod = "BH", pvalueCutoff  = 1, qvalueCutoff  = 1)
head(kegg_enrich)
output_file <- "KEGG_results.csv"
write.csv(kegg_enrich, file = output_file, row.names = TRUE)


##  Result visualization
setwd("/Users/liupeihao/Desktop")
library(tidyverse)
library(ggplot2)
library(dplyr)

## Gene
data <- read.csv('KEGG.csv',header = T)
head(data)
p1 <- ggplot(data) +  
      geom_col(aes(x = Count, y = reorder(Description, `logp`)), color = 'black', width = 0.6, fill = '#68A7BE') +  
      labs(x = 'Gene Count', y = NULL) +  
      theme_test(base_size = 11) +  
      theme(axis.text.y = element_text(angle = 0, hjust = 1),  
            axis.text = element_text(color = 'black'),  
            plot.margin = margin(0.5, 0.5, 1.5, 0.5, 'cm'),  
            panel.border = element_rect(size = 1.2)) +  
      scale_x_continuous(expand = c(0, 0.5), limits = c(0, 10),  
                         sec.axis = sec_axis(~./5, name = '-log10(p-value)', breaks = seq(0, 2, 0.5))) +  
      geom_line(aes(x = `logp` * 5, y = reorder(Description, `logp`), group = 1), linetype = 3, cex = 1) +  
      geom_point(aes(x = `logp` * 5, y = reorder(Description, `logp`)), color = '#E05E2F', size = 3.5) + 
      geom_text(aes(x = Count, y = reorder(Description, `logp`), label = Count), vjust = 0.5, hjust = -0.5, size = 2.5, fontface = 'bold')
p1

## eGene
data <- read.csv('GO.csv',header = T)
head(data)
p1 <- ggplot(data) +  
      geom_col(aes(x = Count, y = reorder(Description, `logp`)), color = 'black', width = 0.6, fill = '#EE7E77') +  
      labs(x = 'Gene Count', y = NULL) +  
      theme_test(base_size = 11) +  
      theme(axis.text.y = element_text(angle = 0, hjust = 1),  
            axis.text = element_text(color = 'black'),  
            plot.margin = margin(0.5, 0.5, 1.5, 0.5, 'cm'),  
            panel.border = element_rect(size = 1.2)) +  
      scale_x_continuous(expand = c(0, 0.5), limits = c(0, 18),  
                         sec.axis = sec_axis(~./3, name = '-log10(p-value)', breaks = seq(0, 6, 2))) +  
      geom_line(aes(x = `logp` * 3, y = reorder(Description, `logp`), group = 1), linetype = 3, cex = 1) +  
      geom_point(aes(x = `logp` * 3, y = reorder(Description, `logp`)), color = '#535794', size = 3.5) + 
      geom_text(aes(x = Count, y = reorder(Description, `logp`), label = Count), vjust = 0.5, hjust = -0.5, size = 2.5, fontface = 'bold')
p1



## Analysis of variance components (grouping, body weight, Top_SNP)
# Preparation of Genotype Files
# # in the/home/phliu DAMsgroupeffect/Toploci folder below
1. Run the locipvalueextract.py code to extract the p-value of each significant point from the GWAS result file
2. Run the Toplociextract.sh code to extract the most significant sites of each metabolite
3. Run the extractloci.sh code to extract the information of the Top site from the binary file
4. Run the zhuanhuan.sh code to convert the binary file into ped and map files
5. Run quqian6lie. Sh code, will be six columns before ped out, retain only site information 
6. Run the merge.pl code to merge the two genotypes at each locus
7. Run the transform.pl code: perl transform.pl, add column names, QTL1
8. Run the tianjiaID.py code and add the ID column to the converted genotype data

# Preparation of BW, Group, and TopQTL data
Run feature_merge.py to merge the Group and BW with the QTL data

# Calculation of Variance Components (on mack)
Rscript Variance_HLC.r

# Merging of Results
Python Variance_summary.py, generate hLC_summary_varianc.csv. Translate the Chinese in this analysis process into English

## Result visualization
setwd("/Users/liupeihao/Desktop")
library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("ggforce")
library(ggforce) 

plot.format=theme(plot.background=element_blank(),
                  panel.grid=element_blank(),
                  panel.background=element_blank(),
                  panel.border = element_blank(),  
                  axis.line = element_line(color = "black", size = 0.5),
                  axis.ticks=element_line(color="black",linewidth=0.5),
                  axis.text.y=element_text(color="black",size=8),
                  axis.text.x=element_text(color="black",size=9),
                  axis.title=element_text(color="black",size=11),
                  plot.title=element_text(color="black",size=7),
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.text=element_text(color="black",size=7),
                  legend.title=element_text(color="black",size=7),
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))

data <- read.csv('HLCvariance.csv',header = T)
head(data)
data$Type <- factor(data$Type, levels = c("Group", "QTL", "BW", "Residuals"))
p1 <- ggplot(data, aes(x = Type, y =  Varianceexplained, color = Type)) +
  geom_violin(aes(fill = Type), width = 1, size = 0.5, color = "black") +  
  geom_boxplot(width = 0.15, outlier.shape = NA, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, aes(fill = Type),color = "black") + 
  scale_color_manual(values = c("#DEEEED", "#68A7BE", "#FEECE7", "#EE7E77")) + 
  scale_fill_manual(values = c("#DEEEED", "#68A7BE", "#FEECE7", "#EE7E77")) + 
  labs(x = '', y = 'Variance Explained(%)')  +
  theme( legend.position = "none")+  
  plot.format +
  facet_zoom(ylim = c(0, 25), zoom.size = 1) +
  stat_compare_means(comparisons = list(c("Group", "QTL"), c("Group", "BW"), c("QTL", "BW")), 
      label = "p.signif",  
      method = "t.test",  
      size = 4, 
      position = position_dodge(0.8),label.y = c(80,90,80))
p1



## Differences analysis of intensity of L-cysteine with different genotype
awk '$12 < 1.6941E-06' meta1374.txt > locimeta1374.txt 
/home/cgbl/biosoft/plink1.9/plink --allow-extra-chr --chr-set 35 --bfile /home/phliu/mGWAS1-100/peihao1100 --extract locimeta1374.txt --make-bed --out meta1374loci
/home/cgbl/biosoft/plink1.9/plink  --chr-set 35 --allow-extra-chr  --bfile  meta1374loci --recode --out  meta1374loci_1
perl hebing.pl
python intensityaverage.py
python intensityaverage1.py

python SNPextract.py

## resuult visualization（18_10290729 rs317441580）
setwd("/Users/liupeihao/Desktop")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggsignif")
library(ggplot2)
library(ggpubr)
library(ggsignif)

df<-read.csv("lcysteineloci.CSV")
head(df)

plot.format=theme(plot.background=element_blank(),
                  panel.grid=element_blank(),
                  panel.background=element_blank(),
                  panel.border = element_blank(),  
                  axis.line = element_line(color = "black", size = 0.5),
                  axis.ticks=element_line(color="black",linewidth=0.5),
                  axis.text.y=element_text(color="black",size=8),
                  axis.text.x=element_text(color="black",size=8),
                  axis.title=element_text(color="black",size=11),
                  plot.title=element_text(color="black",size=9),
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.text=element_text(color="black",size=7),
                  legend.title=element_text(color="black",size=9),
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))

p <- ggplot(df, aes(X18_10290729, y = lcysteine, fill = Group)) +  
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.7), alpha = 0.7,outlier.size = 0.5) +  
  scale_fill_manual(values = c("#EE7E77", "#68A7BE")) +  
  theme_minimal() +   
  ylim(8, 11) +  
  labs(x = "Genotype", y = "Intensity") +
  plot.format +
  stat_summary(fun = mean, aes(group = Group),geom = "line", position = position_dodge(width = 0.7), color = c("#9B0E27", "#9B0E27","#9B0E27","#001560","#001560","#001560"), size = 1) + 
  stat_summary(fun = mean, aes(group = Group), geom = "point", position = position_dodge(width = 0.7), color = c("#9B0E27", "#9B0E27","#9B0E27","#001560","#001560","#001560"), size = 2)   
p  




## The code of the Part 4
## Biomarker identified by LASSO regression 
setwd("/Users/liupeihao/Desktop")
install.packages("glmnet")
library(glmnet)

mydata <- read.csv("RF_42.csv")
head(mydata)
y <- as.matrix(mydata[, 1])  
x <- as.matrix(mydata[, 2:1872]) 
set.seed(12345)
lasso_model <- glmnet(x, y, family = "binomial", alpha = 1) 
print(lasso_model) 
plot(lasso_model,xvar = "lambda",label = F) 
     
cv_model <- cv.glmnet(x, y, family = "binomial",alpha = 1,nfolds = 10)
plot(cv_model)
lambda_min <- cv_model$lambda.min
lambda_min  ## 0.002010968
lambda_1se <- cv_model$lambda.1se
lambda_1se  ##  0.008504873
coef_cv <- coef(lasso_model, s = lambda_min)
coef_cv 
exp(coef_cv)
coef_cv <- as.matrix(coef_cv) 
coef_cv <- data.frame(coef_cv) 
coef_cv$OR <- exp(coef_cv$s1)
nonzero_vars <- rownames(coef_cv[coef_cv$OR != 1, ]) 
nonzero_vars
nonzero_vars <- nonzero_vars [2:93] 
filtered_OR <- coef_cv[nonzero_vars, c("OR")]
write.csv(filtered_OR, "42_filtered_OR_values.csv", row.names = TRUE)
lasso_data <- mydata[,nonzero_vars]
write.csv(lasso_data,"42_lasso_data.csv")

## results visualization
library(ggplot2)  
library(readxl)  
library(gridExtra)  
library(dplyr)    
data <- read_excel('42Features_top20.xlsx')  
head(data)  
data$newMeta <- factor(data$newMeta, levels = unique(data$newMeta))   
 
p1 <- ggplot(data, aes(x = newMeta, y = log10OR1000, fill = log10OR1000)) +   
     coord_flip() +  
     geom_bar(stat = 'identity', color = 'black', width = 0.65) +    
     scale_fill_gradient2(low = "#68A7BE", mid = "white", high = "#F08080", midpoint = 0) +  
     scale_x_discrete(expand = c(0, 0.8)) +  
     labs(x = '', y = '1K×log10(Odds Ratio)') +  
     scale_y_continuous(limits = c(-0.6, 1.8), expand = c(0, 0), breaks = seq(-0.6, 1.8, 0.6)) +  
     theme(axis.title = element_text(size = 11, colour = 'black'),   
           axis.text.x = element_text(size = 8, colour = 'black'),  
           axis.text.y = element_text(size = 10, colour = 'black'),    
           axis.line = element_line(size = 0.5, colour = 'black'),  
           axis.line.y = element_blank(),   
           axis.ticks.y = element_blank(),  
           panel.background = element_rect(fill = "white"),   
           axis.ticks.length = unit(0.15, "cm"),   
           panel.grid.major.y = element_blank(),   
           panel.grid.minor.y = element_blank(),   
           panel.grid.minor.x = element_blank(),  
           legend.position = "none",  
           plot.margin = margin(1, 0.1, 1, 1, 'cm')) +  
     geom_hline(yintercept = 0, color = "#000000", linetype = "solid", size = 0.5)  
p1

p2 <- ggplot(data, aes(x = 0, y = newMeta, fill = superclass)) +  
     geom_point(shape=21,size = 4,color="black") + 
     scale_fill_manual(values = c(
    "Benzenoids" = "#CCEBC5",
    "Lipids and lipid-like molecules" = "#9CD1C7",
    "Nucleosides, nucleotides, and analogues" = "#F4CFE4",
    "Organic acids and derivatives" = "#BEBAD8",
    "Organic oxygen compounds" = "#BCDD78",
    "Organoheterocyclic compounds" = "#8AB0D0",
    "Phenylpropanoids and polyketides" = "#FFFFBB",
    "Other" = "#EB8677",
    "Unclassified" = "#F3B76F"))  +
     theme( axis.title = element_blank(), 
         axis.text = element_blank(), 
         axis.line = element_blank(), 
         axis.ticks = element_blank(), 
         panel.background = element_rect(fill = "white"),   
         panel.grid.major = element_blank(),   
         panel.grid.minor = element_blank(),   
         plot.margin = margin(1, 1, 1, 0, 'cm'),  
         legend.position = "none" ) 
p2

p<-p1+p2
p



## Features validation
setwd("/Users/liupeihao/Desktop")
install.packages("randomForest")
library(randomForest)

## G1 generation
data <- read.csv("LASSO.csv")  
head(data)  
data$Type <- factor(data$Type)  
set.seed(123)  
train_index <- sample(1:nrow(data), 0.7 * nrow(data))  
xf <- data[train_index, ]  
valid_data <- data[-train_index, ]  
rf_model <- randomForest(Type ~ ., data = xf, ntree = 500, importance = TRUE)   

predictions <- predict(rf_model, newdata = valid_data, type = "prob")  
roc_obj <- roc(valid_data$Type, predictions[, levels(data$Type)[2]]) 
auc_value <- auc(roc_obj)  
ci_value <- ci.auc(roc_obj)  
p1<-plot(roc_obj, print.auc = TRUE, print.auc.x = 0.4,print.auc.y = 0.5, 
         auc.polygon = TRUE, auc.polygon.col = "#F1F5FB", max.auc.polygon = FALSE, 
         grid = c(0.5, 0.2), grid.col = c("black", "black"), print.thres = TRUE, print.thres.cex = 0.9,  
         smooth = FALSE, main = "", col = "#277985",  legacy.axes = TRUE)   
text(0.6, 0.1, paste("95% CI: [", round(ci_value[1], 3), ", ", round(ci_value[2], 3), "]", sep=""),  col = "darkgray", cex = 0.8)

## G2 generation
data<-read.csv("LASSO.csv")
head(data)
data$Type<-factor(data$Type)
rf_model <- randomForest(Type ~ ., data = data, ntree = 500, importance = TRUE)   
tidudata <- read.csv("tidu-part.csv")  
head(tidudata)  
tidudata$Type <- factor(tidudata$Type)  
predictions <- predict(rf_model, newdata = tidudata, type = "prob")   
roc_obj <- roc(tidudata$Type, predictions[, levels(data$Type)[2]]) 
auc_value <- auc(roc_obj)  
ci_value <- ci.auc(roc_obj) 
p1<-plot(roc_obj, print.auc = TRUE, print.auc.x = 0.4, print.auc.y = 0.5, 
         auc.polygon = TRUE, auc.polygon.col = "#FFFAF7", max.auc.polygon = FALSE, 
         grid = c(0.5, 0.2), grid.col = c("black", "black"), print.thres = TRUE, print.thres.cex = 0.9, 
         smooth = FALSE,main = "",col = "#BB598E", legacy.axes = TRUE)  
text(0.6, 0.1, paste("95% CI: [", round(ci_value[1], 3), ", ", round(ci_value[2], 3), "]", sep=""),  col = "darkgray", cex = 0.8)

## LASSO Fearures: Visualization of classification effect based on PCA dimension reduction and model prediction probability
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
plt.rcParams['font.family'] = 'Arial'
plt.rcParams['axes.unicode_minus'] = False
df = pd.read_excel('/Users/liupeihao/Desktop/lasso.xlsx')
X = df.drop(['y'], axis=1)
y = df['y']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, 
                                                    random_state=42, stratify=df['y'])
df
from hyperopt import fmin, tpe, hp
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score

parameter_space_rf = {
    'n_estimators': hp.choice('n_estimators', [50, 100, 200, 300]),   
    'max_depth': hp.choice('max_depth', [5, 10, 20, None]),          
    'min_samples_split': hp.uniform('min_samples_split', 0.01, 0.5),  
    'min_samples_leaf': hp.uniform('min_samples_leaf', 0.01, 0.5)     
}

def objective(params):
    model = RandomForestClassifier(
        n_estimators=params['n_estimators'],
        max_depth=params['max_depth'],
        min_samples_split=params['min_samples_split'],
        min_samples_leaf=params['min_samples_leaf'],
        random_state=42)
    
    model.fit(X_train, y_train)
    
    y_pred = model.predict(X_test)
    
    accuracy = accuracy_score(y_test, y_pred)
    
    return -accuracy

best_params = fmin(
    fn=objective,                  
    space=parameter_space_rf,      
    algo=tpe.suggest,              
    max_evals=100                   
)


print("Best hyperparameters:", best_params)
## Best hyperparameters: {'max_depth': 3, 'min_samples_leaf': 0.010636825905619464, 'min_samples_split': 0.013859957758632808, 'n_estimators': 3}

best_model_class = RandomForestClassifier(
    n_estimators=[50, 100, 200, 300][best_params['n_estimators']],
    max_depth=[5, 10, 20, None][best_params['max_depth']],
    min_samples_split=best_params['min_samples_split'],
    min_samples_leaf=best_params['min_samples_leaf'],
    random_state=42
)

best_model_class.fit(X_train, y_train)


probabilities = best_model_class.predict_proba(X_test)
probability_df = pd.DataFrame(probabilities, columns=[f'Prob_Class_{i}' for i in range(probabilities.shape[1])])
probability_df.index = X_test.index
probability_df.reset_index(drop=True, inplace=True)
probability_df.head()
from sklearn.decomposition import PCA
df_selected = X_test
pca = PCA(n_components=1)
pca_result = pca.fit_transform(df_selected)
explained_variance = pca.explained_variance_ratio_
pca_df = pd.DataFrame(pca_result, columns=[f"PC1 ({explained_variance[0]*100:.2f}%)"])
pca_df.head()
combined_df = pd.concat([probability_df, pca_df, y_test.reset_index(drop=True)], axis=1)
combined_df.rename(columns={y_test.name: 'True'}, inplace=True)
combined_df.head()
combined_df.to_excel('/Users/liupeihao/Desktop/test.xlsx', index=False)

colors = combined_df['True'].map({0: '#68A7BE', 1: '#EE7E77'})
plt.figure(figsize=(4, 3),dpi=1200)
plt.scatter(combined_df['Prob_Class_1'], combined_df['PC1 (98.58%)'], 
            c=colors, edgecolor='black', s=50) 
plt.axvline(x=0.5, color='gray', linestyle='--')
plt.xlabel('Predicted value for H')
plt.ylabel('PC1 (98.58%)')
plt.title('30% of the Discovery Cohort Dataset')

for true_value, color, label in [(0, '#68A7BE', 'L'), (1, '#EE7E77', 'H')]: 
    plt.scatter([], [], color=color, edgecolor='black', s=100, label=label)
plt.legend(title="Class", loc="center left", bbox_to_anchor=(1, 0.5))
plt.tight_layout(rect=[0.5, 0.5, 1.5, 0.5])
plt.savefig('/Users/liupeihao/Desktop/testset.pdf', format='pdf', bbox_inches='tight')


probabilities = best_model_class.predict_proba(X_train)
probability_df = pd.DataFrame(probabilities, columns=[f'Prob_Class_{i}' for i in range(probabilities.shape[1])])
probability_df.index = X_train.index
probability_df.reset_index(drop=True, inplace=True)
df_selected = X_train
pca = PCA(n_components=1)
pca_result = pca.fit_transform(df_selected)
explained_variance = pca.explained_variance_ratio_
pca_df = pd.DataFrame(pca_result, columns=[f"PC1 ({explained_variance[0]*100:.2f}%)"])
combined_df = pd.concat([probability_df, pca_df, y_train.reset_index(drop=True)], axis=1)
combined_df.rename(columns={y_train.name: 'True'}, inplace=True)
combined_df.head()
combined_df.to_excel('/Users/liupeihao/Desktop/training.xlsx', index=False)  
colors = combined_df['True'].map({0: '#68A7BE', 1: '#EE7E77'})
plt.figure(figsize=(4, 3),dpi=1200)
plt.scatter(combined_df['Prob_Class_1'], combined_df['PC1 (98.80%)'], 
            c=colors, edgecolor='black', s=50)  
plt.axvline(x=0.5, color='gray', linestyle='--')
plt.xlabel('Predicted value for H')
plt.ylabel('PC1 (98.80%)')
plt.title('70% of the Discovery Cohort Dataset')
for true_value, color, label in [(0, '#68A7BE', 'L'), (1, '#EE7E77', 'H')]: 
    plt.scatter([], [], color=color, edgecolor='black', s=100, label=label)
plt.legend(title="Class", loc="center left", bbox_to_anchor=(1, 0.5))
plt.tight_layout(rect=[0.5, 0.5, 1.5, 0.5])
plt.savefig('/Users/liupeihao/Desktop/trainingset.pdf', format='pdf', bbox_inches='tight')
plt.show()



## WGCNA
setwd("/Users/liupeihao/Desktop")

if (!requireNamespace("BiocManager", quietly = TRUE))  
    install.packages("BiocManager")   
BiocManager::install("impute")  
BiocManager::install("preprocessCore")  
install.packages("WGCNA")
library(WGCNA)

devtools::install_github("Hy4m/linkET", force = TRUE)
install.packages("tidyverse")
library(linkET)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

options(stringsAsFactors = FALSE)

femData = read.csv("42day_Meta.csv") 
dim(femData) 
names(femData) 
datExpr0 = as.data.frame(t(femData[, -1])) 
names(datExpr0) = femData$ID 
rownames(datExpr0) = names(femData)[-1] 
gsg = goodSamplesGenes(datExpr0, verbose = 3) 
gsg$allOK 

if (!gsg$allOK)
{if (sum(!gsg$goodGenes)>0)
 printFlush(paste("Removing genes:", paste(names(datExpr0)[!gsg$goodGenes], collapse = ", ")));
 if (sum(!gsg$goodSamples)>0)
 printFlush(paste("Removing samples:", paste(rownames(datExpr0)[!gsg$goodSamples], collapse = ", ")));
 datExpr0 = datExpr0[gsg$goodSamples, gsg$goodGenes]} 
gsg = goodSamplesGenes(datExpr0, verbose = 3) 
gsg$allOK 

## Individual clustering
sampleTree = hclust(dist(datExpr0), method = "average") 
sizeGrWindow(12,9)
par(cex = 0.8)
par(mar = c(2,6,2,2))
plot(sampleTree, main = "Sample clustering to detect outliers",  sub="", xlab="", cex.lab = 1.5,
     cex.axis = 1.5, cex.main = 2)

clust = cutreeStatic(sampleTree, cutHeight =8000000, minSize = 10)
table(clust)
keepSamples = (clust==1) 
datExpr = datExpr0[keepSamples, ]  
nGenes = ncol(datExpr) 
nSamples = nrow(datExpr) 

## Phenotypic clustering
traitData = read.csv("42day_Antibody.csv")
dim(traitData)
names(traitData)
femaleSamples = rownames(datExpr) 
traitRows = match(femaleSamples, traitData$ID)  
datTraits = traitData[traitRows, -1]
rownames(datTraits) = traitData[traitRows, 1]
collectGarbage()
datTraits$TAOC <- as.numeric(datTraits$TAOC)
datTraits$IL1 <- as.numeric(datTraits$IL1)
datTraits$IFN <- as.numeric(datTraits$IFN)
sampleTree2 = hclust(dist(datExpr), method = "average")
traitValues <- as.matrix(datTraits) 
traitColors <- numbers2colors(traitValues, signed = FALSE)
plotDendroAndColors(sampleTree2,
                    traitColors, groupLabels = names(datTraits),
                    main = "Sample dendrogram and trait heatmap")

## The soft threshold is determined
powers = c(c(1:10), seq(from = 12, to=20, by=2))
sft = pickSoftThreshold(datExpr, powerVector = powers, verbose = 5)
sizeGrWindow(9, 5)
par(mfrow = c(1, 2)) 
cex1 = 0.9  
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     xlab="Soft Threshold (power)", ylab="Scale Free Topology Model Fit,signed R^2", type="n",
     main = "Scale independence")
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     labels=powers, cex=cex1, col="red")
abline(h=0.9, col="red")
plot(sft$fitIndices[,1], sft$fitIndices[,5],
     xlab="Soft Threshold (power)", ylab="Mean Connectivity", type="n",
     main = "Mean connectivity")
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1, col="red")

## Metabolite clustering
softPower = 5  
adjacency = adjacency(datExpr, power = softPower) 
TOM = TOMsimilarity(adjacency)
dissTOM = 1 - TOM
memory.limit(50000)
geneTree = hclust(as.dist(dissTOM), method = "average")
sizeGrWindow(12, 9)
par(mar = c(2,6,2,2)) 
plot(geneTree, xlab="", sub="", main = "Metabolite clustering on TOM-based dissimilarity",
     labels = FALSE, hang = 0.04)
minModuleSize = 100  
dynamicMods = cutreeDynamic(dendro = geneTree,
                            distM = dissTOM,   
                            deepSplit = 2,     
                            pamRespectsDendro = FALSE,  
                            minClusterSize = minModuleSize)  
table(dynamicMods) 
dynamicColors = labels2colors(dynamicMods) 
table(dynamicColors) 
sizeGrWindow(8, 6)  
plotDendroAndColors(geneTree, dynamicColors,
                    "Dynamic Tree Cut", 
                    dendroLabels = FALSE, 
                    hang = 0.03,  
                    addGuide = TRUE,  
                    guideHang = 0.05,  
                    main = "Metabolite dendrogram and module colors")  

## Modular clustering
MEList = moduleEigengenes(datExpr, colors = dynamicColors)  
MEs = MEList$eigengenes 
MEDiss = 1 - cor(MEs) 
METree = hclust(as.dist(MEDiss), method = "average")  
sizeGrWindow(7, 6)
plot(METree, main = "Clustering of module eigenmetabolites",
     xlab = "", sub = "")

## The metabolite characteristics were combined into clusters
MEDissThres = 0.2  
abline(h = MEDissThres, col = "red") 
merge = mergeCloseModules(datExpr, dynamicColors, cutHeight = MEDissThres, verbose = 3)
mergedColors = merge$colors 
mergedMEs = merge$newMEs  
sizeGrWindow(12, 9)  
plotDendroAndColors(geneTree, cbind(dynamicColors, mergedColors),
                    c("Dynamic Tree Cut", "Merged dynamic"),
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)

## Analysis of correlation between metabolite modules and phenotypes
moduleColors = mergedColors  
colorOrder = c("grey",  standardColors(50))  
moduleLabels = match(moduleColors, colorOrder) - 1  
MEs = mergedMEs  
nGenes = ncol(datExpr)  
nSamples = nrow(datExpr) 
MEs0 = moduleEigengenes(datExpr, moduleColors)$eigengenes  
MEs = orderMEs(MEs0)  
moduleTraitCor = cor(MEs, datTraits, use = "p")  
moduleTraitPvalue = corPvalueStudent(moduleTraitCor, nSamples)  
sizeGrWindow(10, 6)  
textMatrix = paste(signif(moduleTraitCor, 2), "\n(",  
                   signif(moduleTraitPvalue, 1), ")", sep = "")
dim(textMatrix) = dim(moduleTraitCor)  
par(mar = c(6, 8.5, 3, 3)) 
labeledHeatmap(Matrix = moduleTraitCor,  
               xLabels = names(datTraits), 
               yLabels = names(MEs), 
               ySymbols = names(MEs), 
               colorLabels = FALSE,  
               colors = blueWhiteRed(50),  
               textMatrix = textMatrix, 
               setStdMargins = FALSE, 
               cex.text = 0.8, 
               zlim = c(-1, 1),  
               main = paste("Module-trait relationships"))


moduleNames <- c("Brown", "Blue", "Turquoise", "Grey") 
traitNames <- c("TAOC", "IL1", "IFN")  
moduleTraitDF <- expand.grid(Module = moduleNames, Trait = traitNames)  
moduleTraitDF$Correlation <- c(moduleTraitCor)
moduleTraitDF$Pvalue <- c(moduleTraitPvalue)
write.csv(moduleTraitDF, file = "module_trait_relationship.csv", row.names = FALSE)

colnames(MEs)<-str_remove(colnames(MEs),"ME")
mantel <- mantel_test(datTraits,MEs,spec_select = list(TAOC = 1, IL1 = 2, IFN = 3))%>% 
          mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
           pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf), labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))
color_palette <- colorRampPalette(c("#68A7BE","white","#EE7E77"))(100)
p1 <- qcorrplot(correlate(MEs, method = "pearson"), diag = FALSE, type = "upper") +
  geom_square() +  
  geom_mark(only_mark = TRUE, size = 5) + 
  geom_couple(aes(colour = pd, size = rd), data = mantel,curvature = nice_curvature(0.15),
                  nudge_x = 0.2,label.colour = "black",label.fontface = 2,label.size = 4,drop = TRUE) + 
  scale_fill_gradientn(colours = color_palette)+
  scale_size_manual(values = c(1, 2, 3)) +
  scale_colour_manual(values = c("#4575b4", "#8EB068", "#ED8759")) +
  guides(size = guide_legend(title = "Mantel's r", override.aes = list(colour = "grey35"), order = 2),
         colour = guide_legend(title = "Mantel's p", override.aes = list(size = 3), order = 1),
         fill = guide_colorbar(title = "pearson's r", order = 3)) +
  theme(legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_text(color = "black", size = 10, face = "bold"),
        plot.margin = unit(c(1, 1, 1, 1), units = "cm"))
ggsave("moduleandtrait.pdf", plot = p1, width = 20 / 2.54, height = 12 / 2.54, units = "in") 

## IFN with brown model
IFN = as.data.frame(datTraits$IFN) 
names(IFN) = "IFN" 
modNames = substring(names(MEs), 1)  
geneModuleMembership = as.data.frame(cor(datExpr, MEs, use = "p")) 
MMPvalue = as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership), nSamples))  
names(geneModuleMembership) = paste("MM", modNames, sep="") 
names(MMPvalue) = paste("p.MM", modNames, sep="") 
geneTraitSignificance = as.data.frame(cor(datExpr, IFN, use = "p")) 
GSPvalue = as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nSamples)) 
names(geneTraitSignificance) = paste("GS.", names(IFN), sep="")  
names(GSPvalue) = paste("p.GS.", names(IFN), sep="") 
module = "brown"  
column = match(module, modNames)  
moduLegenes = moduleColors == module  
sizeGrWindow(7, 7) 
par(mfrow = c(1, 1))  

verboseScatterplot(abs(geneModuleMembership[moduLegenes, column]),  
                   abs(geneTraitSignificance[moduLegenes, 1]), 
                   xlab = paste("Module Membership in", module, "module"),  
                   ylab = "Metabolites significance for IFN",  
                   main = paste("Module membership vs. Metabolite significance\n"),  
                   cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = module)  

## The relationship table between the module and the metabolite is derived
names(datExpr)
annot = read.csv(file = "annotation.csv")
dim(annot)
names(annot)
probes = names(datExpr)
probes2annot = match(probes, annot$Genes.table.ID)
sum(is.na(probes2annot))
geneInfo0 = data.frame(Genes.table.ID = probes,
                       Gene.name = annot$Gene.name[probes2annot],
                       moduleColor = moduleColors,
                       geneTraitSignificance,
                       GSPvalue)
modOrder = order(-abs(cor(MEs, IFN, use = "p")))
for (mod in 1:ncol(geneModuleMembership))
{
  oldNames = names(geneInfo0)
  geneInfo0 = data.frame(geneInfo0, geneModuleMembership[, modOrder[mod]],
                         MMPvalue[, modOrder[mod]]);
  names(geneInfo0) = c(oldNames, paste("MM.", modNames[modOrder[mod]], sep=""),
                       paste("p.MM.", modNames[modOrder[mod]], sep=""))}

geneOrder = order(geneInfo0$moduleColor)
geneInfo = geneInfo0[geneOrder, ]
write.csv(geneInfo, file = "IFN.csv")

## Interactions between metabolites and metabolites
net = blockwiseModules(datExpr,
                       power = sft$powerEstimate,
                       maxBlockSize = 6000,
                       TOMType = "unsigned",
                       minModuleSize = 30,
                       reassignThreshold = 0,
                       mergeCutHeight = 0.25,
                       numericLabels = TRUE,
                       pamRespectsDendro = FALSE,
                       saveTOMs = TRUE,
                       saveTOMFileBase = "AS-green-FPKM-TOM",
                       verbose = 3)
nGenes = ncol(datExpr)
nSamples = nrow(datExpr)
geneTree = net$dendrograms[[1]]
dissTOM = 1 - TOMsimilarityFromExpr(datExpr, power = 6)
plotTOM = dissTOM^7
diag(plotTOM) = NA
nSelect = 400
set.seed(10)
select = sample(nGenes, size = nSelect)
selectTOM = dissTOM[select, select]
selectTree = hclust(as.dist(selectTOM), method = "average")
selectColors = moduleColors[select]
sizeGrWindow(9, 9)
plotDiss = selectTOM^7
diag(plotDiss) = NA
TOMplot(plotDiss, selectTree, selectColors, main = "Network heatmap plot, selected metabolite")

## Derive metabolites within modules associated with the target phenotype
cytoDir = "CytoscapeInput"
dir.create(cytoDir)
for (mod in 1:nrow(table(moduleColors))) {
    modules = names(table(moduleColors))[mod]
    probes = colnames(datExpr0)
    inModule = (moduleColors == modules) 
    modProbes = probes[inModule]
    modGenes = modProbes
    modTOM = TOM[inModule, inModule]
    dimnames(modTOM) = list(modProbes, modProbes)
    edges_File = paste("CytoscapeInput-edges-", modules , ".txt", sep="")
    nodes_File = paste("CytoscapeInput-nodes-", modules, ".txt", sep="")
    outEdge = file.path(cytoDir, edges_File)
    outNode = file.path(cytoDir, nodes_File)
    cyt = exportNetworkToCytoscape(modTOM,
                                   edgeFile = outEdge,
                                   nodeFile = outNode,
                                   weighted = TRUE,
                                   threshold = 0.02,
                                   nodeNames = modProbes,
                                   altNodeNames = modGenes,
                                   nodeAttr = moduleColors[inModule])}


## Correlation between metabolites of each module and IFN
setwd("/Users/liupeihao/Desktop")
install.packages("ggbeeswarm")
install.packages("ggplot2")
library(ggplot2)
library(ggbeeswarm)
install.packages("Hmisc")
library(Hmisc)
install.packages("ggpubr")
library(ggpubr)

mydata <-read.csv("IFNmodule.csv",sep=",") 
head(mydata)

p1 <- ggplot(mydata, aes(x = Module, y = Correlationcoefficient, fill = Module)) +
    coord_flip() + 
    geom_quasirandom(size = 2, shape = 21, stroke = 0.7, color = "black", aes(fill = Module)) +
    scale_fill_manual(values = c("#1198BE", "#BF855F", "#B9B9B9", "#B8DBB0")) +
    labs(x = NULL, y = 'Correlation Coefficient') +
    theme(
        axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black',  size = 11),
        axis.title = element_text(color = 'black',size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(1, 1, 1, 1, 'cm'),
        legend.position = 'none') +
    geom_vline(xintercept = c(1.5, 2.5,3.5), color = "gray", linetype = "dashed", size = 0.8)+
    geom_hline(yintercept = 0, color = "#D1287A", linetype = "dashed", size = 0.8)
p1

## Brown module metabolite coexpression network visualization
setwd("/Users/liupeihao/Desktop")
library(igraph)  
library(ggplot2)  
library(readr)  
library(dplyr)  

edges <- read_csv("Brownedge.csv", col_names = c("from", "to"))  
nodes <- read_csv("Brownnode.csv", col_names = c("nodeName", "Class"))  
class_colors <- c(  
  "Benzenoids" = "#CCEBC5",  
  "Lipids and lipid-like molecules" = "#9CD1C7",  
  "Nucleosides, nucleotides, and analogues" = "#F4CFE4",  
  "Organic acids and derivatives" = "#BEBAD8",  
  "Organic oxygen compounds" = "#BCDD78",  
  "Organoheterocyclic compounds" = "#8AB0D0",  
  "Phenylpropanoids and polyketides" = "#FFFFBB",  
  "Other" = "#EB8677",  
  "Unclassified" = "#F3B76F") 
g <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)  
num_nodes <- vcount(g)  
set.seed(42)  
radii <- runif(num_nodes, min = 0, max = 1)
angles <- runif(num_nodes, min = 0, max = 2 * pi)
x_coords <- radii * cos(angles)  
y_coords <- radii * sin(angles)  
layout <- cbind(x_coords, y_coords)  
plot(g, vertex.label = NA, vertex.color = V(g)$color, vertex.size = 7,edge.color = "#948370", edge.width = 1,layout = layout)
## Add legend
legend("right", fill = class_colors, legend = names(class_colors),bty = "n", cex = 0.8,xjust = 0, yjust = 0.5)


## Correlation between metabolites of brown module and IFN
setwd("/Users/liupeihao/Desktop")
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra) 

data<-read.csv("IFNBrownmodule.csv")
head(data)
p1 <-  ggplot(data, aes(x = Correlationcoefficient, y = - log10(p.GS.IFN), color = Group, fill = Group)) +  
      geom_point(shape = 21, size = 2, color = "black", stroke = 0.35) +   
      scale_fill_manual(values = c("#68A7BE", 'gray', "#EE7E77")) +  
      xlab("Correlation Coefficient") +   
      ylab(expression(-log[10](p-value))) +
      theme(  
        axis.title = element_text(size = 11, colour = 'black'),  
        axis.text = element_text(size = 8, colour = 'black'),   
        axis.ticks = element_line(color = "black"),  
        plot.title = element_text(hjust = 0.5),  
        plot.margin = margin(1, 1, 1, 1, "cm"),  
        panel.background = element_blank(),   
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1),  
        legend.position = c(0.35, 0.75)) +    
      scale_x_continuous(limits = c(-0.6, 0.6)) +  
      scale_y_continuous(limits = c(0, 6)) +  
      geom_hline(yintercept = 1.3, color = "#D1287A", linetype = "dashed", size = 0.5)+   
      guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL))
p1


## One of the most significantly related metabolites is described in the Brown module
setwd("/Users/liupeihao/Desktop")
install.packages("stringr")  
library(stringr) 
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra) 

data<-read.csv("IFNmetabolite.csv")
head(data)

p1 <- ggplot(data, aes(x = IFN-γ, y = 2,6,6-Trimethyl-2-cyclohexene-1,4-dione)) +
  geom_point(shape = 21, size = 2,fill="#948370",color="black") +
  geom_smooth(method = "lm", color = "#533628", fill = "#F0D097", size = 1, se = TRUE) +
  labs(x = "Content of IFN-γ", y = str_wrap("Intensity of 2,6,6-Trimethyl -2-cyclohexene-1,4-dione", width = 30),
  title = expression(paste("Correlation coefficient = 0.57 ", italic("P"), " = 3.39E-06"))) + 
  theme(
    axis.title = element_text(size = 11, colour = 'black'),
    axis.text = element_text(size = 8, colour = 'black'), 
    axis.line = element_line(size = 0.5, colour = 'black'),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(size = 10, hjust = 1),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()) + 
  guides(color = guide_legend(ncol = 1),  
         fill = guide_legend(ncol = 1)) 
p1



## WGCNA and LASSO VENN: Biomarker
setwd("/Users/liupeihao/Desktop")
install.packages("ggVennDiagram")
library(ggVennDiagram)
library(ggplot2)
library(dplyr)
otu_tax<-read.csv("Venn.csv")
head(otu_tax)
df1 <- otu_tax$LASSO
df2<-otu_tax$Brown
Venn_data<-list(LASSO=df1,Brown=df2)
ggVennDiagram(Venn_data, set_size = 6, set_color = "grey40") +scale_fill_gradient(low ="#DEEEED", high = "#EE7E77")+ theme(legend.position = "left")



## Biomerker: LASSO Fearures: Visualization of classification effect based on PCA dimension reduction and model prediction probability
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
plt.rcParams['font.family'] = 'Arial'
plt.rcParams['axes.unicode_minus'] = False
df = pd.read_excel('/Users/liupeihao/Desktop/lassowgcna.xlsx')
X = df.drop(['y'], axis=1)
y = df['y']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, 
                                                    random_state=42, stratify=df['y'])
df
from hyperopt import fmin, tpe, hp
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score
parameter_space_rf = {
    'n_estimators': hp.choice('n_estimators', [50, 100, 200, 300]),   
    'max_depth': hp.choice('max_depth', [5, 10, 20, None]),          
    'min_samples_split': hp.uniform('min_samples_split', 0.01, 0.5),  
    'min_samples_leaf': hp.uniform('min_samples_leaf', 0.01, 0.5)     
}
def objective(params):
    model = RandomForestClassifier(
        n_estimators=params['n_estimators'],
        max_depth=params['max_depth'],
        min_samples_split=params['min_samples_split'],
        min_samples_leaf=params['min_samples_leaf'],
        random_state=42)
    
    model.fit(X_train, y_train)
    
    y_pred = model.predict(X_test)
    
    accuracy = accuracy_score(y_test, y_pred)
    
    return -accuracy

best_params = fmin(
    fn=objective,                  
    space=parameter_space_rf,      
    algo=tpe.suggest,              
    max_evals=100                   
)

print("Best hyperparameters:", best_params)
# Best hyperparameters: {'max_depth': 0, 'min_samples_leaf': 0.010727219329565565, 'min_samples_split': 0.056570096646257194, 'n_estimators': 1}

best_model_class = RandomForestClassifier(
    n_estimators=[50, 100, 200, 300][best_params['n_estimators']],
    max_depth=[5, 10, 20, None][best_params['max_depth']],
    min_samples_split=best_params['min_samples_split'],
    min_samples_leaf=best_params['min_samples_leaf'],
    random_state=42
)


best_model_class.fit(X_train, y_train)
probabilities = best_model_class.predict_proba(X_test)
probability_df = pd.DataFrame(probabilities, columns=[f'Prob_Class_{i}' for i in range(probabilities.shape[1])])
probability_df.index = X_test.index
probability_df.reset_index(drop=True, inplace=True)
probability_df.head()
from sklearn.decomposition import PCA
df_selected = X_test
pca = PCA(n_components=1)
pca_result = pca.fit_transform(df_selected)
explained_variance = pca.explained_variance_ratio_
pca_df = pd.DataFrame(pca_result, columns=[f"PC1 ({explained_variance[0]*100:.2f}%)"])
pca_df.head()
combined_df = pd.concat([probability_df, pca_df, y_test.reset_index(drop=True)], axis=1)
combined_df.rename(columns={y_test.name: 'True'}, inplace=True)
combined_df.head()
combined_df.to_excel('/Users/liupeihao/Desktop/test12.xlsx', index=False)  
colors = combined_df['True'].map({0: '#68A7BE', 1: '#EE7E77'})
plt.figure(figsize=(4, 3),dpi=1200)
plt.scatter(combined_df['Prob_Class_1'], combined_df['PC1 (99.99%)'], 
            c=colors, edgecolor='black', s=50)  
plt.axvline(x=0.5, color='gray', linestyle='--')
plt.xlabel('Predicted value for H')
plt.ylabel('PC1 (99.99%)')
plt.title('30% of the Discovery Cohort Dataset')
for true_value, color, label in [(0, '#68A7BE', 'L'), (1, '#EE7E77', 'H')]: 
    plt.scatter([], [], color=color, edgecolor='black', s=100, label=label)
plt.legend(title="Class", loc="center left", bbox_to_anchor=(1, 0.5))
plt.tight_layout(rect=[0.5, 0.5, 1.5, 0.5])
plt.savefig('/Users/liupeihao/Desktop/testset12.pdf', format='pdf', bbox_inches='tight')


probabilities = best_model_class.predict_proba(X_train)
probability_df = pd.DataFrame(probabilities, columns=[f'Prob_Class_{i}' for i in range(probabilities.shape[1])])
probability_df.index = X_train.index
probability_df.reset_index(drop=True, inplace=True)
df_selected = X_train
pca = PCA(n_components=1)
pca_result = pca.fit_transform(df_selected)
explained_variance = pca.explained_variance_ratio_
pca_df = pd.DataFrame(pca_result, columns=[f"PC1 ({explained_variance[0]*100:.2f}%)"])
combined_df = pd.concat([probability_df, pca_df, y_train.reset_index(drop=True)], axis=1)
combined_df.rename(columns={y_train.name: 'True'}, inplace=True)
combined_df.head()
combined_df.to_excel('/Users/liupeihao/Desktop/training12.xlsx', index=False)  
colors = combined_df['True'].map({0: '#68A7BE', 1: '#EE7E77'})
plt.figure(figsize=(4, 3),dpi=1200)
plt.scatter(combined_df['Prob_Class_1'], combined_df['PC1 (99.99%)'], 
            c=colors, edgecolor='black', s=50)
plt.axvline(x=0.5, color='gray', linestyle='--')
plt.xlabel('Predicted value for H')
plt.ylabel('PC1 (99.99%)')
plt.title('70% of the Discovery Cohort Dataset')
for true_value, color, label in [(0, '#68A7BE', 'L'), (1, '#EE7E77', 'H')]: 
    plt.scatter([], [], color=color, edgecolor='black', s=100, label=label)
plt.legend(title="Class", loc="center left", bbox_to_anchor=(1, 0.5))
plt.tight_layout(rect=[0.5, 0.5, 1.5, 0.5])
plt.savefig('/Users/liupeihao/Desktop/trainingset12.pdf', format='pdf', bbox_inches='tight')



## Biomarker：Features important rank
setwd("/Users/liupeihao/Desktop")
install.packages("randomForest")
library(randomForest)
data<-read.csv("LASSOBrown.csv")
head(data)
data$Type<-factor(data$Type)
rf_model <- randomForest(Type ~ ., data = data, ntree = 500, importance = TRUE)   
importance_scores <- importance(rf_model)
feature_importance <- data.frame(Metabolite = rownames(importance_scores), Importance = importance_scores[, "MeanDecreaseGini"])
ordered_features <- feature_importance[order(-feature_importance$Importance), ]
importance_gini <- importance(rf_model, type = 2)
importance_accuracy <- importance(rf_model, type = 1)
combined_importance <- data.frame(
  Metabolite = rownames(importance_scores),
  MeanDecreaseGini = importance_scores[, "MeanDecreaseGini"],
  GiniImportance = importance_gini[, "MeanDecreaseGini"],
  AccuracyImportance = importance_accuracy[, "MeanDecreaseAccuracy"])
ordered_combined <- combined_importance[order(-combined_importance$MeanDecreaseGini), ]
write.csv(ordered_combined, file = "LASSOBrown_features.csv", row.names = FALSE)

## results visualization
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(stringr) 
data<-read_excel('LASSOBrown.xlsx') 
head(data)
data$GiniImportance <- as.numeric(as.character(data$GiniImportance))   
data$newMetabolite <- str_wrap(as.character(data$newMetabolite), width = 15)  
data$newMetabolite <- factor(data$newMetabolite, levels = unique(data$newMetabolite))  

p1 <- ggplot(data, aes(x = newMetabolite, y = GiniImportance, fill = GiniImportance)) +   
     geom_bar(stat = 'identity', color = 'black', width = 0.65) +    
     scale_fill_gradient(low = "#FEECE7", high = "#EE7E77") +  
     scale_x_discrete(expand = c(0, 0.8)) +  
     labs(x = '', y = 'Gini Importance') +  
     scale_y_continuous(limits = c(0, 20), expand = c(0, 0), breaks = seq(0, 20, 5)) +  
     theme(axis.title = element_text(size = 13, colour = 'black'),   
           axis.text.y = element_text(size = 10, colour = 'black'),   
           axis.line = element_line(size = 0.5, colour = 'black'),  
           axis.text.x = element_text(size = 10, colour = 'black', angle = 90, hjust = 1),   
           axis.line.x = element_blank(),   
           axis.ticks.length = unit(0.15, "cm"),   
           panel.background = element_rect(fill = "white"),   
           panel.grid.major.y = element_blank(),   
           panel.grid.minor.y = element_blank(),   
           panel.grid.minor.x = element_blank(),  
           legend.position = "none",  
           plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'))  
p1 



## Correlation between biomarkers
setwd("/Users/liupeihao/Desktop")
install.packages("corrplot")
library(corrplot)
library(ggplot2)
dt <- read.csv("42day_correlation.csv", sep = ",")   
head(dt)  
cor <- cor(dt, method = "pearson")  
res <- cor.mtest(dt, conf.level = .95)  
p <- res$p  
mycol <- colorRampPalette(c('#61AACF', '#98CADD', "#EAEFF6", '#F9EFEF', '#E9C6C6', "#DA9599"), alpha = 0.6)   
labels <- colnames(dt)  
label_colors <- rep("black", length(labels))  
label_colors[labels %in% c("HMDB0061700", "HMDB0011549", "Butyric.acid",   
                            "Hexyl.glucoside", "LMFA01140039",   
                            "LMFA01060107", "dinor.12.oxo.PDA",   
                            "Allylestrenol")] <- "#9CD1C7"  
label_colors[labels %in% c("X3.Oxohexanoic.acid", "L.Cysteine",   
                            "Pyrraline", "LMFA01050242")] <- "#BEBAD8"  
label_colors[labels %in% c("X5.Iodouracil", "HMDB0030994",   
                            "HMDB0255307", "HMDB0253651")] <- "#8AB0D0"   
p1 <- corrplot(cor,   
                method = "square",   
                type = "lower",   
                order = "original",   
                p.mat = p,   
                sig.level = 0.05,   
                insig = "blank",   
                cl.pos = "r",   
                col = mycol(6),   
                tl.cex = 0.8,   
                tl.col = label_colors, 
                outline = '#989898', 
                mar = c(2, 0, 0, 0),   
                tl.pos = "tp")  
p1

variables <- colnames(dt)  
rect_data <- data.frame(  
  Variable = variables,  
  Color = c(rep("#9CD1C7", 8), rep("#BEBAD8", 4), rep("#8AB0D0", 4)),  
  x = seq_along(variables))   
p2 <- ggplot(rect_data, aes(xmin = x - 0.4, xmax = x + 0.4, ymin = 0, ymax = 0.8, fill = Color)) +  
  geom_rect(color = "black") + 
  scale_fill_identity() +  
  xlab(NULL) +   
  ylab(NULL) +   
  theme_void() +    
  theme(legend.position = "none") +  
  coord_fixed(ratio = 1) +  
  theme(plot.margin = margin(0, 0, 0, 0, "cm")) +  
  annotate("text", x = 4, y = -0.2, label = "Lipids and lipid-like molecules", size = 4, color = "black") + 
  annotate("text", x = 12, y = -0.2, label = "Organic acids and derivatives", size = 4, color = "black") + 
  annotate("text", x = 16, y = -0.2, label = "Organoheterocyclic compounds", size = 4, color = "black") 
p2


## Correlation between Hexyl glucoside and L-cysteine、Butyric acid
setwd("/Users/liupeihao/Desktop")
install.packages("cowplot")
install.packages("ggpubr")
install.packages("gridExtra")
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra)

data<-read.csv("new42day_correlation_3meta.csv")
head(data)
  
p1 <- ggplot(data, aes(x = Hexylglucoside, y = Butyricacid)) +
      geom_point(size = 1.5, color = "#777777") +  
      geom_smooth(method = "lm", color = "#D4553D", fill = "#FFE48A", size = 1, se = TRUE) +
      stat_cor(method = "pearson", label.x = 400, label.y = 700, label.sep = "\n") +
      xlab("Intensity of Hexyl glucoside") + ylab("Intensity of Butyric acid") + 
      theme(
         plot.background=element_blank(),  
        panel.grid=element_blank(),  
        panel.background=element_blank(),  
        panel.border=element_rect(color="black",linewidth=1,fill=NA),  
        axis.line=element_blank(),  
        axis.ticks=element_line(color="black",linewidth=0.5),  
        axis.text=element_text(color="black",size=8),  
        axis.title=element_text(color="black",size=11),  
        plot.title=element_text(color="black",size=9),  
        legend.background=element_blank(),  
        legend.key=element_blank(),    
        legend.position = "none",  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))+
      scale_x_continuous(limits = c(380, 1500)) +  
      scale_y_continuous(limits = c(200, 800))
p1 <- ggMarginal(p1, type = "histogram", margins = "both",  
                 xparams = list(fill = "#EE7E77", color = "black"),  
                 yparams = list(fill = "#68A7BE", color = "black")) 
p1

p2 <- ggplot(data, aes(x = Hexylglucoside, y = LCysteine)) +
      geom_point(size = 1.5, color = "#777777") +  
      geom_smooth(method = "lm", color = "#D4553D", fill = "#FFE48A", size = 1, se = TRUE) +
      stat_cor(method = "pearson", label.x = 400, label.y = 28000, label.sep = "\n") +
      xlab("Intensity of Hexyl glucoside") + ylab("Intensity of L-Cysteine") +  
      theme(
         plot.background=element_blank(),  
        panel.grid=element_blank(),  
        panel.background=element_blank(),  
        panel.border=element_rect(color="black",linewidth=1,fill=NA),  
        axis.line=element_blank(),  
        axis.ticks=element_line(color="black",linewidth=0.5),  
        axis.text=element_text(color="black",size=8),  
        axis.title=element_text(color="black",size=11),  
        plot.title=element_text(color="black",size=9),  
        legend.background=element_blank(),  
        legend.key=element_blank(),    
        legend.position = "none",  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))+
      scale_x_continuous(limits = c(380, 1500)) +  
      scale_y_continuous(limits = c(5000, 30000))
p2 <- ggMarginal(p2, type = "histogram", margins = "both",  
                 xparams = list(fill = "#EE7E77", color = "black"),  
                 yparams = list(fill = "#68A7BE", color = "black")) 
p2



## relationship between biomarker and mortality
library(readr)  
library(dplyr) 
library(tidyr)  

# Read data (with samples participating in WGCNA analysis)
data <- read_csv("42day_correlation_mortality.csv")   
results <- data.frame(Metabolite = character(), Kendall_Correlation = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)  
for (i in 1:16) {  
  metabolite <- colnames(data)[i]  
  test_result <- cor.test(data[[i]], data[[17]], method = "kendall")  
  results <- rbind(results, data.frame(Metabolite = metabolite,   
                                        Kendall_Correlation = test_result$estimate,   
                                        P_Value = test_result$p.value))}  
write_csv(results, "kendall_correlation_results.csv")  

## results visualization
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra) 
data<-read.csv("kendall_correlation_results.csv")
head(data)
p1 <- ggplot(data, aes(x = Kendall_Correlation, y = logp, color = Group, fill = Group)) +  
      geom_point(shape = 21, size = 2, color = "black") +   
      scale_fill_manual(values = c("#68A7BE", 'gray', "#EE7E77")) +  
      xlab("Correlation Coefficient") +   
      ylab(expression(-log[10](p-value))) +  
      theme(  
        axis.title = element_text(size = 11, colour = 'black'),  
        axis.text = element_text(size = 8, colour = 'black'),   
        axis.ticks = element_line(color = "black"),  
        plot.title = element_text(hjust = 0.5),  
         plot.margin = unit(c(1, 1, 1, 1), 'cm'),
        panel.background = element_blank(),   
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.position = c(0.35, 0.75)) +   
      scale_x_continuous(limits = c(-0.5, 0.5)) +  
      scale_y_continuous(limits = c(0, 8)) +  
      geom_hline(yintercept = 1.3, color = "#D1287A", linetype = "dashed", size = 0.5) +  
      geom_text(data = subset(data,   Metabolite %in% c("Butyrate", "L-Cysteine")),  
                aes(label =Metabolite),   
                vjust = -1,  
                size = 3,    
                color = "black")+   
      guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL))
p1  



## biomarker features description
setwd("/Users/liupeihao/Desktop")
library(ggpubr)
library(ggplot2)  
library(dplyr) 
library(ggtext) 

data <- read.csv('16metafeature_fenmian.csv', header = TRUE)  
head(data)

highlight_data <- data.frame(  
  Class = c("Lipids and lipid-like molecules", "Organic acids and derivatives", "Organoheterocyclic compounds",  
            "Lipids and lipid-like molecules", "Organic acids and derivatives", "Organoheterocyclic compounds",  
            "Lipids and lipid-like molecules", "Organic acids and derivatives", "Organoheterocyclic compounds"),  
  Count = c(1.05, 1.02, 1.05, 0.87, 0.84, 0.82, 0.56, 0.54, 0.64),  
  Type = c("Fold Change", "Fold Change", "Fold Change",   
           "Variable Important in Project", "Variable Important in Project", "Variable Important in Project",  
           "Coefficient of Variation", "Coefficient of Variation", "Coefficient of Variation"))  

mean_data <- data %>%group_by(Class, Type) %>%summarise(Mean = mean(Count, na.rm = TRUE))

p1 <- ggplot(data, aes(x = Class, y = Count)) +  
  geom_boxplot(width = 0.45, size = 0.5, color = "black", fill = "#68A7BE") + 
  geom_point(data = highlight_data, aes(x = Class, y = Count), color = "black", size = 3, shape = 23,  fill = "#EE7E77")+
  geom_point(data = mean_data, aes(x = Class, y = Mean),color = "black", fill = "#FEDFB2", size = 3.5, shape = 21, position = position_dodge(width = 0.75)) +  
  labs(x = 'Super Class', y = 'Value') +  
  theme_classic() +  
  theme(  
    axis.title = element_text(size = 13, color = 'black'),  
    axis.text.y = element_markdown(color = 'black', size = 10, hjust = 1), 
    axis.text.x = element_markdown(size = 8, color = 'black'),  
    axis.ticks = element_line(color = "black"),  
    plot.title = element_text(hjust = 0.5),  
    plot.margin = unit(c(1, 1, 1, 1), 'cm'),    
    panel.background = element_blank(),   
    panel.grid.major = element_line(linetype = "dotted", color = "gray", size = 0.4), 
    panel.grid.minor = element_line(linetype = "dotted", color = "gray", size = 0.4),
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    strip.background = element_rect(fill = "lightgray", color = "black"),  
    panel.spacing = unit(0.1, "cm")  
  ) +  
  coord_flip() +  
  facet_wrap(~ Type) 
p1



## Calculation of metabolite AUC value
setwd("/Users/liupeihao/Desktop")
install.packages("pROC")
library(pROC)
data <- read.csv("LASSOBrown.csv", header = TRUE)
head(data)
data$Type <- ifelse(data$Type == "H", 1, 0)
metabolite_columns <- names(data)[-1]
AUC_results <- data.frame(Metabolite = character(), AUC = numeric(), stringsAsFactors = FALSE)
for (metabolite in metabolite_columns) {
  roc_obj <- roc(data$Type, data[[metabolite]], quiet = TRUE)
  auc_value <- auc(roc_obj)
  AUC_results <- rbind(AUC_results, data.frame(Metabolite = metabolite, AUC = auc_value))
}
print(AUC_results)
write.csv(AUC_results, "LASSOBrown_AUC_results.csv", row.names = FALSE)

data <- read.csv("Brown.csv", header = TRUE)
head(data)
data$Type <- ifelse(data$Type == "H", 1, 0)
metabolite_columns <- names(data)[-1]
AUC_results <- data.frame(Metabolite = character(), AUC = numeric(), stringsAsFactors = FALSE)
for (metabolite in metabolite_columns) {
  roc_obj <- roc(data$Type, data[[metabolite]], quiet = TRUE)
  auc_value <- auc(roc_obj)
  AUC_results <- rbind(AUC_results, data.frame(Metabolite = metabolite, AUC = auc_value))
}
print(AUC_results)
write.csv(AUC_results, "Brown_AUC_results.csv", row.names = FALSE)

data <- read.csv("LASSO.csv", header = TRUE)
head(data)
data$Type <- ifelse(data$Type == "H", 1, 0)
metabolite_columns <- names(data)[-1]
AUC_results <- data.frame(Metabolite = character(), AUC = numeric(), stringsAsFactors = FALSE)
for (metabolite in metabolite_columns) {
  roc_obj <- roc(data$Type, data[[metabolite]], quiet = TRUE)
  auc_value <- auc(roc_obj)
  AUC_results <- rbind(AUC_results, data.frame(Metabolite = metabolite, AUC = auc_value))
}
print(AUC_results)
write.csv(AUC_results, "LASSO_AUC_results.csv", row.names = FALSE)


## results visualization
library(ggplot2)  
library(dplyr)  

data <- read.csv('MetaboliteAUC.csv',header = T)
head(data)

head(data)
mean_data <- data %>%  
  group_by(Group) %>%  
  summarise(mean_ROC = mean(ROC, na.rm = TRUE))  

p1 <- ggplot(data, aes(x = Group, y = ROC, fill = Group)) +
  geom_boxplot(width = 0.5,color = "black", size = 0.5) +  
  scale_fill_manual(values = c("#FEDFB2", "#68A7BE","#EE7E77")) + 
  labs(x = '', y = 'AUC') +
  ylim(0.4, 1) +
  coord_flip() +  
  theme_classic() +
  theme(
    panel.background = element_blank(),   
    panel.grid.major = element_line(linetype = "dotted", color = "gray", size = 0.4), 
    panel.grid.minor = element_line(linetype = "dotted", color = "gray", size = 0.4),
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    strip.background = element_rect(fill = "lightgray", color = "black"),
    axis.title = element_text(size = 11,  color = 'black'),
    axis.text.x = element_text(size = 8, color = 'black'),
    axis.text.y = element_text(size = 8, color = 'black'),
    axis.ticks = element_line(size = 0.5),
    axis.line = element_line(size = 0.5, color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    plot.margin = margin(1, 1, 1, 1, 'cm'),
    legend.position = "none")+
  geom_hline(yintercept = 0.5, color = "#D1287A", linetype = "dashed", size = 0.5) +
  geom_line(data = mean_data, aes(x = Group, y = mean_ROC),   
            group = 1, color = "#9A009D", size = 1) +
  geom_point(data = mean_data, aes(x = Group, y = mean_ROC),   
             color = "#9A009D", size = 1.5)
p1




## The code of the Part 5
# Cross-Species Validation of Metabolite Biomarkers

# Model Selection in G1 Generation
setwd("/Users/liupeihao/Desktop")

install.packages(c("caret", "pROC", "dplyr", "ggplot2","klaR"))
library(caret)
library(pROC)
library(dplyr)
library(ggplot2)
library(klaR)

data <- read.csv("16metaG1.csv", stringsAsFactors = TRUE)
data$Group <- as.factor(data$Group)

set.seed(123) 
train_index <- createDataPartition(data$Group, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
results <- data.frame(Model = character(),
                      Accuracy = numeric(),
                      Precision = numeric(),
                      Recall = numeric(),
                      F1Score = numeric(),
                      AUC = numeric(),
                      stringsAsFactors = FALSE)
trControl <- trainControl(method = "cv", number = 10)  

models <- list(
  Logistic_Regression = train(Group ~ ., data = train_data, method = "glm", family = "binomial", trControl = trControl),
  Decision_Tree = train(Group ~ ., data = train_data, method = "rpart", trControl = trControl),
  Discriminant_Analysis = train(Group ~ ., data = train_data, method = "lda", trControl = trControl),
  KNN = train(Group ~ ., data = train_data, method = "knn", tuneLength = 5, trControl = trControl),
  Naive_Bayes = train(Group ~ ., data = train_data, method = "nb", trControl = trControl),
  Bagged_Trees = train(Group ~ ., data = train_data, method = "treebag", trControl = trControl)
)

roc_data <- data.frame()

for (model_name in names(models)) {
  model <- models[[model_name]]

  preds <- predict(model, test_data)
  probs <- predict(model, test_data, type = "prob")[, 2]  率
  cm <- confusionMatrix(preds, test_data$Group)
  accuracy <- cm$overall['Accuracy']
  precision <- posPredValue(preds, test_data$Group, positive = "H")
  recall <- sensitivity(preds, test_data$Group, positive = "H")
  f1_score <- (2 * precision * recall) / (precision + recall)
  roc_curve <- roc(test_data$Group, probs, levels = rev(levels(test_data$Group)))
  auc_value <- auc(roc_curve)
  roc_points <- data.frame(FPR = 1 - roc_curve$specificities, TPR = roc_curve$sensitivities, Model = model_name)
  roc_data <- rbind(roc_data, roc_points)  
  results <- rbind(results, data.frame(Model = model_name,
                                        Accuracy = accuracy,
                                        Precision = precision,
                                        Recall = recall,
                                        F1Score = f1_score,
                                        AUC = auc_value))
}
print(results)

write.csv(results, "model_performance_metrics.csv", row.names = FALSE)

# The ROC curves of all models were plotted using ggplot2
roc_data$Model <- factor(roc_data$Model, levels = names(models))
model_colors <- c("#AA2B46", "#E58760", "#F6DEA4", "#C9DEE5", "#6CB3DA", "#3B5DA3")
p1 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 0.8) +
  geom_abline(linetype = "dashed", color = "#53595E") +  
  labs(x = "False Positive Rate", y = "True Positive Rate", color = "Model") + 
  theme(  
    plot.title = element_text(hjust = 0.5),  
    legend.position = "right",  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11), 
    axis.text = element_text(color = "black", size=8),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm')
  ) + 
  scale_color_manual(values = model_colors) + 
  coord_fixed(ratio = 1)
p1

## Bar chart visualization
library(ggplot2)
library(ggforce)
library(grid) 
library(cowplot) 

data <- read.csv("model_performance_metrics.csv", stringsAsFactors = TRUE)
head(data)

data$Model <- factor(data$Model, levels = unique(data$Model)) 

p1 <- ggplot(data, aes(x = Model, y = Precision)) +   
     geom_bar(stat = "identity",  width = 0.8, fill = "#DEEEED", colour = "#000000") +   
     labs(x = "", y = "Precision")  +  
     theme(  
        axis.title = element_text(size = 11, colour = 'black'),  
        axis.text.x = element_text(size = 8, colour = 'black'),  
        axis.text.y = element_text(size = 9, colour = 'black'),   
        axis.line = element_line(size = 0.5, colour = 'black'),  
        axis.ticks = element_line(color = "black"),  
        plot.title = element_text(hjust = 0.5),  
        plot.margin = margin(0.5, 0.5, 0.5, 0, "cm"),  
        panel.background = element_blank(),   
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +
     coord_flip()
p1


p2 <- ggplot(data, aes(x = Model, y = -Recall)) +   
     geom_bar(stat = "identity", width = 0.8, fill = "#FEECE7", colour = "#000000") +   
     labs(x = "", y = "Recall")  +  
     scale_y_continuous(  
         labels = function(x) abs(x)) +
     theme(  
        axis.title = element_text(size = 11, colour = 'black'),  
        axis.text.x = element_text(size = 8, colour = 'black'),  
        axis.text.y = element_blank(),
        axis.line = element_line(size = 0.5, colour = 'black'),  
        axis.ticks = element_line(color = "black"),  
        plot.title = element_text(hjust = 0.5),  
        plot.margin = margin(0.5, 0, 0.5, 0.5, "cm"),  
        panel.background = element_blank(),   
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()
     ) +
     coord_flip() +
     scale_x_discrete(position = "top") 
p2 

p3 <- plot_grid(p2, p1, nrow = 1,align = "h",rel_widths = c(0.59, 1))
p3


## G1 and G2, verify each other
data <- read.csv("16metaG1.csv", stringsAsFactors = TRUE)
data$Group <- as.factor(data$Group)
set.seed(123)
train_index <- createDataPartition(data$Group, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data1 <- data[-train_index, ]

data2 <- read.csv("16metaG2.csv", stringsAsFactors = TRUE)
data2$Group <- as.factor(data2$Group)  
data2$Group <- factor(data2$Group, levels = levels(train_data$Group))

trControl <- trainControl(method = "cv", number = 10) 
discriminant_model <- train(Group ~ ., data = train_data, method = "lda", trControl = trControl)

roc_data <- data.frame()

preds_test1 <- predict(discriminant_model, test_data1)
probs_test1 <- predict(discriminant_model, test_data1, type = "prob")[, "H"]
roc_curve1 <- roc(test_data1$Group, probs_test1, levels = c("L", "H"), direction = "<")
auc_value1 <- auc(roc_curve1)
## Area under the curve: 0.8582
cm_test1 <- confusionMatrix(preds_test1, test_data1$Group)
accuracy_value1 <- cm_test1$overall['Accuracy']
## 0.8717949
roc_points1 <- data.frame(FPR = 1 - roc_curve1$specificities, 
                          TPR = roc_curve1$sensitivities, 
                          Model = "G1 Test")
roc_data <- rbind(roc_data, roc_points1)


preds_test2 <- predict(discriminant_model, data2)
probs_test2 <- predict(discriminant_model, data2, type = "prob")[, "H"]
roc_curve2 <- roc(data2$Group, probs_test2, levels = c("L", "H"), direction = "<")
auc_value2 <- auc(roc_curve2)
## Area under the curve: 0.6133
cm_test2 <- confusionMatrix(preds_test2, data2$Group)
accuracy_value2 <- cm_test2$overall['Accuracy']
## 0.64
roc_points2 <- data.frame(FPR = 1 - roc_curve2$specificities, 
                          TPR = roc_curve2$sensitivities, 
                          Model = "G2 Cohort")
roc_data <- rbind(roc_data, roc_points2)

labels_data <- data.frame(
  Model = factor(c("G1 Test", "G2 Cohort")),
  Label = c(
    sprintf("%.2f(%.2f)", auc_value1, accuracy_value1),
    sprintf("%.2f(%.2f)", auc_value2, accuracy_value2)
  ),
  x = c(0.7, 0.7),
  y = c(0.3, 0.15))


model_colors <- c("#EE7E77", "#68A7BE")
line_types <- c(1, 1)

p1 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model, linetype = Model)) +
  geom_line(size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +  
  geom_text(data = labels_data, 
            aes(x = x, y = y, label = Label, color = Model),
            size = 4, fontface = "bold", show.legend = FALSE) +
  labs(x = "False Positive Rate", y = "True Positive Rate", 
       color = "Cohort", linetype = "Cohort") +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "right",  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11), 
    axis.text = element_text(color = "black", size = 8),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm')) + 
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = line_types) + 
  coord_fixed(ratio = 1)
p1



## The three metabolites G1 and G2 verify each other
data <- read.csv("16metaG1_part.csv", stringsAsFactors = TRUE)
data$Group <- as.factor(data$Group)

set.seed(123)
train_index <- createDataPartition(data$Group, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data1 <- data[-train_index, ]

data2 <- read.csv("16metaG2_part.csv", stringsAsFactors = TRUE)
data2$Group <- as.factor(data2$Group)  
data2$Group <- factor(data2$Group, levels = levels(train_data$Group))

trControl <- trainControl(method = "cv", number = 10) 
discriminant_model <- train(Group ~ ., data = train_data, method = "lda", trControl = trControl)

roc_data <- data.frame()

preds_test1 <- predict(discriminant_model, test_data1)
probs_test1 <- predict(discriminant_model, test_data1, type = "prob")[, "H"]
roc_curve1 <- roc(test_data1$Group, probs_test1, levels = c("L", "H"), direction = "<")
auc_value1 <- auc(roc_curve1)
## Area under the curve: 0.77
cm_test1 <- confusionMatrix(preds_test1, test_data1$Group)
accuracy_value1 <- cm_test1$overall['Accuracy']
## 0.81
roc_points1 <- data.frame(FPR = 1 - roc_curve1$specificities, 
                          TPR = roc_curve1$sensitivities, 
                          Model = "G1 Test")
roc_data <- rbind(roc_data, roc_points1)

preds_test2 <- predict(discriminant_model, data2)
probs_test2 <- predict(discriminant_model, data2, type = "prob")[, "H"]
roc_curve2 <- roc(data2$Group, probs_test2, levels = c("L", "H"), direction = "<")
auc_value2 <- auc(roc_curve2)
## Area under the curve: 0.83
cm_test2 <- confusionMatrix(preds_test2, data2$Group)
accuracy_value2 <- cm_test2$overall['Accuracy']
## 0.66
roc_points2 <- data.frame(FPR = 1 - roc_curve2$specificities, 
                          TPR = roc_curve2$sensitivities, 
                          Model = "G2 Cohort")
roc_data <- rbind(roc_data, roc_points2)

labels_data <- data.frame(
  Model = factor(c("G1 Test", "G2 Cohort")),
  Label = c(
    sprintf("%.2f(%.2f)", auc_value1, accuracy_value1),
    sprintf("%.2f(%.2f)", auc_value2, accuracy_value2)
  ),
  x = c(0.7, 0.7),
  y = c(0.3, 0.15))

model_colors <- c("#EE7E77", "#68A7BE")
line_types <- c(1, 1)

p1 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model, linetype = Model)) +
  geom_line(size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +  
  geom_text(data = labels_data, 
            aes(x = x, y = y, label = Label, color = Model),
            size = 4, fontface = "bold", show.legend = FALSE) +
  labs(x = "False Positive Rate", y = "True Positive Rate", 
       color = "Cohort", linetype = "Cohort") +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "right",  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11), 
    axis.text = element_text(color = "black", size = 8),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm')) + 
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = line_types) + 
  coord_fixed(ratio = 1)
p1


## Three metabolites were mutually verified in two populations
data <- read.csv("16metaVerify1.csv", stringsAsFactors = TRUE)
data$Group <- as.factor(data$Group)

set.seed(123)
train_index <- createDataPartition(data$Group, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data1 <- data[-train_index, ]

data2 <- read.csv("16metaVerify2.csv", stringsAsFactors = TRUE)
data2$Group <- as.factor(data2$Group)  
data2$Group <- factor(data2$Group, levels = levels(train_data$Group))

trControl <- trainControl(method = "cv", number = 10) 
discriminant_model <- train(Group ~ ., data = train_data, method = "lda", trControl = trControl)

roc_data <- data.frame()

preds_test1 <- predict(discriminant_model, test_data1)
probs_test1 <- predict(discriminant_model, test_data1, type = "prob")[, "H"]
roc_curve1 <- roc(test_data1$Group, probs_test1, levels = c("L", "H"), direction = "<")
auc_value1 <- auc(roc_curve1)
## Area under the curve: 0.7438
cm_test1 <- confusionMatrix(preds_test1, test_data1$Group)
accuracy_value1 <- cm_test1$overall['Accuracy']
## 0.6923077 
roc_points1 <- data.frame(FPR = 1 - roc_curve1$specificities, 
                          TPR = roc_curve1$sensitivities, 
                          Model = "Verify Cohort1 Test")
roc_data <- rbind(roc_data, roc_points1)

preds_test2 <- predict(discriminant_model, data2)
probs_test2 <- predict(discriminant_model, data2, type = "prob")[, "H"]
roc_curve2 <- roc(data2$Group, probs_test2, levels = c("L", "H"), direction = "<")
auc_value2 <- auc(roc_curve2)
## Area under the curve: 0.5696
cm_test2 <- confusionMatrix(preds_test2, data2$Group)
accuracy_value2 <- cm_test2$overall['Accuracy']
## 0.5106383 
roc_points2 <- data.frame(FPR = 1 - roc_curve2$specificities, 
                          TPR = roc_curve2$sensitivities, 
                          Model = "Verify Cohort2")
roc_data <- rbind(roc_data, roc_points2)

labels_data <- data.frame(
  Model = factor(c("Verify Cohort1 Test", "Verify Cohort2")),
  Label = c(
    sprintf("%.2f(%.2f)", auc_value1, accuracy_value1),
    sprintf("%.2f(%.2f)", auc_value2, accuracy_value2)
  ),
  x = c(0.7, 0.7),
  y = c(0.3, 0.15))

model_colors <- c("#EE7E77", "#68A7BE")
line_types <- c(1, 1)

p1 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model, linetype = Model)) +
  geom_line(size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +  
  geom_text(data = labels_data, 
            aes(x = x, y = y, label = Label, color = Model),
            size = 4, fontface = "bold", show.legend = FALSE) +
  labs(x = "False Positive Rate", y = "True Positive Rate", 
       color = "Cohort", linetype = "Cohort") +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "right",  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11), 
    axis.text = element_text(color = "black", size = 8),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm')) + 
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = line_types) + 
  coord_fixed(ratio = 1)
p1


## 16 signature metabolites + cytokine-related metabolites
data <- read.csv("16metaG1cytokines.csv", stringsAsFactors = TRUE)
data$Group <- as.factor(data$Group)

set.seed(123)
train_index <- createDataPartition(data$Group, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data1 <- data[-train_index, ]

data2 <- read.csv("16metaG2cytokines.csv", stringsAsFactors = TRUE)
data2$Group <- as.factor(data2$Group)  
data2$Group <- factor(data2$Group, levels = levels(train_data$Group))

trControl <- trainControl(method = "cv", number = 10) 
discriminant_model <- train(Group ~ ., data = train_data, method = "lda", trControl = trControl)

roc_data <- data.frame()

preds_test1 <- predict(discriminant_model, test_data1)
probs_test1 <- predict(discriminant_model, test_data1, type = "prob")[, "H"]
roc_curve1 <- roc(test_data1$Group, probs_test1, levels = c("L", "H"), direction = "<")
auc_value1 <- auc(roc_curve1)
## Area under the curve: 0.8794
cm_test1 <- confusionMatrix(preds_test1, test_data1$Group)
accuracy_value1 <- cm_test1$overall['Accuracy']
## 0.8589744 
roc_points1 <- data.frame(FPR = 1 - roc_curve1$specificities, 
                          TPR = roc_curve1$sensitivities, 
                          Model = "G1 Test")
roc_data <- rbind(roc_data, roc_points1)

preds_test2 <- predict(discriminant_model, data2)
probs_test2 <- predict(discriminant_model, data2, type = "prob")[, "H"]
roc_curve2 <- roc(data2$Group, probs_test2, levels = c("L", "H"), direction = "<")
auc_value2 <- auc(roc_curve2)
## Area under the curve: 0.651
cm_test2 <- confusionMatrix(preds_test2, data2$Group)
accuracy_value2 <- cm_test2$overall['Accuracy']
## 0.75
roc_points2 <- data.frame(FPR = 1 - roc_curve2$specificities, 
                          TPR = roc_curve2$sensitivities, 
                          Model = "G2 Cohort")
roc_data <- rbind(roc_data, roc_points2)

labels_data <- data.frame(
  Model = factor(c("G1 Test", "G2 Cohort")),
  Label = c(
    sprintf("%.2f(%.2f)", auc_value1, accuracy_value1),
    sprintf("%.2f(%.2f)", auc_value2, accuracy_value2)
  ),
  x = c(0.7, 0.7),
  y = c(0.3, 0.15))

model_colors <- c("#EE7E77", "#68A7BE")
line_types <- c(1, 1)

p1 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model, linetype = Model)) +
  geom_line(size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +  
  geom_text(data = labels_data, 
            aes(x = x, y = y, label = Label, color = Model),
            size = 4, fontface = "bold", show.legend = FALSE) +
  labs(x = "False Positive Rate", y = "True Positive Rate", 
       color = "Cohort", linetype = "Cohort") +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "right",  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11), 
    axis.text = element_text(color = "black", size = 8),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm')) + 
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = line_types) + 
  coord_fixed(ratio = 1)
p1


## Three metabolites and cytokine-related metabolites were mutually verified in G1 and G2
data <- read.csv("16metaG1cytokines_part.csv", stringsAsFactors = TRUE)
data$Group <- as.factor(data$Group)

set.seed(123)
train_index <- createDataPartition(data$Group, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data1 <- data[-train_index, ]

data2 <- read.csv("16metaG2cytokines_part.csv", stringsAsFactors = TRUE)
data2$Group <- as.factor(data2$Group)  
data2$Group <- factor(data2$Group, levels = levels(train_data$Group))

trControl <- trainControl(method = "cv", number = 10) 
discriminant_model <- train(Group ~ ., data = train_data, method = "lda", trControl = trControl)

roc_data <- data.frame()

preds_test1 <- predict(discriminant_model, test_data1)
probs_test1 <- predict(discriminant_model, test_data1, type = "prob")[, "H"]
roc_curve1 <- roc(test_data1$Group, probs_test1, levels = c("L", "H"), direction = "<")
auc_value1 <- auc(roc_curve1)
## Area under the curve: 0.7862
cm_test1 <- confusionMatrix(preds_test1, test_data1$Group)
accuracy_value1 <- cm_test1$overall['Accuracy']
## 0.7948718  
roc_points1 <- data.frame(FPR = 1 - roc_curve1$specificities, 
                          TPR = roc_curve1$sensitivities, 
                          Model = "G1 Test")
roc_data <- rbind(roc_data, roc_points1)

preds_test2 <- predict(discriminant_model, data2)
probs_test2 <- predict(discriminant_model, data2, type = "prob")[, "H"]
roc_curve2 <- roc(data2$Group, probs_test2, levels = c("L", "H"), direction = "<")
auc_value2 <- auc(roc_curve2)
## Area under the curve: 0.8008
cm_test2 <- confusionMatrix(preds_test2, data2$Group)
accuracy_value2 <- cm_test2$overall['Accuracy']
##  0.625
roc_points2 <- data.frame(FPR = 1 - roc_curve2$specificities, 
                          TPR = roc_curve2$sensitivities, 
                          Model = "G2 Cohort")
roc_data <- rbind(roc_data, roc_points2)

labels_data <- data.frame(
  Model = factor(c("G1 Test", "G2 Cohort")),
  Label = c(
    sprintf("%.2f(%.2f)", auc_value1, accuracy_value1),
    sprintf("%.2f(%.2f)", auc_value2, accuracy_value2)
  ),
  x = c(0.7, 0.7),
  y = c(0.3, 0.15))

model_colors <- c("#EE7E77", "#68A7BE")
line_types <- c(1, 1)

p1 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model, linetype = Model)) +
  geom_line(size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +  
  geom_text(data = labels_data, 
            aes(x = x, y = y, label = Label, color = Model),
            size = 4, fontface = "bold", show.legend = FALSE) +
  labs(x = "False Positive Rate", y = "True Positive Rate", 
       color = "Cohort", linetype = "Cohort") +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "right",  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11), 
    axis.text = element_text(color = "black", size = 8),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm')) + 
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = line_types) + 
  coord_fixed(ratio = 1)
p1


## Three metabolites and cytokine-related metabolites were cross-verified in two populations
data <- read.csv("16metaVerify1cytokines.csv", stringsAsFactors = TRUE)
data$Group <- as.factor(data$Group)

set.seed(123)
train_index <- createDataPartition(data$Group, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data1 <- data[-train_index, ]

data2 <- read.csv("16metaVerify2cytokines.csv", stringsAsFactors = TRUE)
data2$Group <- as.factor(data2$Group)  
data2$Group <- factor(data2$Group, levels = levels(train_data$Group))

trControl <- trainControl(method = "cv", number = 10) 
discriminant_model <- train(Group ~ ., data = train_data, method = "lda", trControl = trControl)

roc_data <- data.frame()

preds_test1 <- predict(discriminant_model, test_data1)
probs_test1 <- predict(discriminant_model, test_data1, type = "prob")[, "H"]
roc_curve1 <- roc(test_data1$Group, probs_test1, levels = c("L", "H"), direction = "<")
auc_value1 <- auc(roc_curve1)
## Area under the curve: 0.675
cm_test1 <- confusionMatrix(preds_test1, test_data1$Group)
accuracy_value1 <- cm_test1$overall['Accuracy']
## 0.7826087 
roc_points1 <- data.frame(FPR = 1 - roc_curve1$specificities, 
                          TPR = roc_curve1$sensitivities, 
                          Model = "Verify Cohort1 Test")
roc_data <- rbind(roc_data, roc_points1)

# 在test_data2上评估
preds_test2 <- predict(discriminant_model, data2)
probs_test2 <- predict(discriminant_model, data2, type = "prob")[, "H"]
roc_curve2 <- roc(data2$Group, probs_test2, levels = c("L", "H"), direction = "<")
auc_value2 <- auc(roc_curve2)
## Area under the curve: 0.5789
cm_test2 <- confusionMatrix(preds_test2, data2$Group)
accuracy_value2 <- cm_test2$overall['Accuracy']
## 0.5405405 
roc_points2 <- data.frame(FPR = 1 - roc_curve2$specificities, 
                          TPR = roc_curve2$sensitivities, 
                          Model = "Verify Cohort2")
roc_data <- rbind(roc_data, roc_points2)

labels_data <- data.frame(
  Model = factor(c("Verify Cohort1 Test", "Verify Cohort2")),
  Label = c(
    sprintf("%.2f(%.2f)", auc_value1, accuracy_value1),
    sprintf("%.2f(%.2f)", auc_value2, accuracy_value2)
  ),
  x = c(0.7, 0.7),
  y = c(0.3, 0.15))

model_colors <- c("#EE7E77", "#68A7BE")
line_types <- c(1, 1)

p1 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model, linetype = Model)) +
  geom_line(size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +  
  geom_text(data = labels_data, 
            aes(x = x, y = y, label = Label, color = Model),
            size = 4, fontface = "bold", show.legend = FALSE) +
  labs(x = "False Positive Rate", y = "True Positive Rate", 
       color = "Cohort", linetype = "Cohort") +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "right",  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11), 
    axis.text = element_text(color = "black", size = 8),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm')) + 
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = line_types) + 
  coord_fixed(ratio = 1)
p1


## The metabolites related to human survival reported in the literature were cross-verified in two populations
data <- read.csv("newmetaVerify1.csv", stringsAsFactors = TRUE)
data$Group <- as.factor(data$Group)
for (i in 2:7) {data[, i] <- as.numeric(data[, i])}

set.seed(123)
train_index <- createDataPartition(data$Group, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data1 <- data[-train_index, ]

data2 <- read.csv("newmetaVerify2.csv", stringsAsFactors = TRUE)
data2$Group <- as.factor(data2$Group)  
data2$Group <- factor(data2$Group, levels = levels(train_data$Group))
for (i in 2:7) {data[, i] <- as.numeric(data[, i])}

trControl <- trainControl(method = "cv", number = 10) 
discriminant_model <- train(Group ~ ., data = train_data, method = "lda", trControl = trControl)

roc_data <- data.frame()

preds_test1 <- predict(discriminant_model, test_data1)
probs_test1 <- predict(discriminant_model, test_data1, type = "prob")[, "H"]
roc_curve1 <- roc(test_data1$Group, probs_test1, levels = c("L", "H"), direction = "<")
auc_value1 <- auc(roc_curve1)
## Area under the curve: 0.9758
cm_test1 <- confusionMatrix(preds_test1, test_data1$Group)
accuracy_value1 <- cm_test1$overall['Accuracy']
## 0.9411765  
roc_points1 <- data.frame(FPR = 1 - roc_curve1$specificities, 
                          TPR = roc_curve1$sensitivities, 
                          Model = "Verify Cohort1 Test")
roc_data <- rbind(roc_data, roc_points1)

preds_test2 <- predict(discriminant_model, data2)
probs_test2 <- predict(discriminant_model, data2, type = "prob")[, "H"]
roc_curve2 <- roc(data2$Group, probs_test2, levels = c("L", "H"), direction = "<")
auc_value2 <- auc(roc_curve2)
## Area under the curve: 0.8929
cm_test2 <- confusionMatrix(preds_test2, data2$Group)
accuracy_value2 <- cm_test2$overall['Accuracy']
## 0.7647059 
roc_points2 <- data.frame(FPR = 1 - roc_curve2$specificities, 
                          TPR = roc_curve2$sensitivities, 
                          Model = "Verify Cohort2")
roc_data <- rbind(roc_data, roc_points2)

labels_data <- data.frame(
  Model = factor(c("Verify Cohort1 Test", "Verify Cohort2")),
  Label = c(
    sprintf("%.2f(%.2f)", auc_value1, accuracy_value1),
    sprintf("%.2f(%.2f)", auc_value2, accuracy_value2)
  ),
  x = c(0.7, 0.7),
  y = c(0.3, 0.15))

model_colors <- c("#EE7E77", "#68A7BE")
line_types <- c(1, 1)

p1 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model, linetype = Model)) +
  geom_line(size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +  
  geom_text(data = labels_data, 
            aes(x = x, y = y, label = Label, color = Model),
            size = 4, fontface = "bold", show.legend = FALSE) +
  labs(x = "False Positive Rate", y = "True Positive Rate", 
       color = "Cohort", linetype = "Cohort") +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "right",  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11), 
    axis.text = element_text(color = "black", size = 8),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm')) + 
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = line_types) + 
  coord_fixed(ratio = 1)
p1


## Metabolites related to human survival are mutually verified in G1 and G2
data <- read.csv("G1newmeta.csv", stringsAsFactors = TRUE)
data$Group <- as.factor(data$Group)

set.seed(123)
train_index <- createDataPartition(data$Group, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data1 <- data[-train_index, ]

data2 <- read.csv("G2newmeta.csv", stringsAsFactors = TRUE)
data2$Group <- as.factor(data2$Group)  
data2$Group <- factor(data2$Group, levels = levels(train_data$Group))

trControl <- trainControl(method = "cv", number = 10) 
discriminant_model <- train(Group ~ ., data = train_data, method = "lda", trControl = trControl)

roc_data <- data.frame()

preds_test1 <- predict(discriminant_model, test_data1)
probs_test1 <- predict(discriminant_model, test_data1, type = "prob")[, "H"]
roc_curve1 <- roc(test_data1$Group, probs_test1, levels = c("L", "H"), direction = "<")
auc_value1 <- auc(roc_curve1)
## Area under the curve: 0.6169
cm_test1 <- confusionMatrix(preds_test1, test_data1$Group)
accuracy_value1 <- cm_test1$overall['Accuracy']
## 0.7948718  
roc_points1 <- data.frame(FPR = 1 - roc_curve1$specificities, 
                          TPR = roc_curve1$sensitivities, 
                          Model = "G1 Test")
roc_data <- rbind(roc_data, roc_points1)

preds_test2 <- predict(discriminant_model, data2)
probs_test2 <- predict(discriminant_model, data2, type = "prob")[, "H"]
roc_curve2 <- roc(data2$Group, probs_test2, levels = c("L", "H"), direction = "<")
auc_value2 <- auc(roc_curve2)
## Area under the curve: 0.7787
cm_test2 <- confusionMatrix(preds_test2, data2$Group)
accuracy_value2 <- cm_test2$overall['Accuracy']
##  0.6607143 
roc_points2 <- data.frame(FPR = 1 - roc_curve2$specificities, 
                          TPR = roc_curve2$sensitivities, 
                          Model = "G2 Cohort")
roc_data <- rbind(roc_data, roc_points2)

labels_data <- data.frame(
  Model = factor(c("G1 Test", "G2 Cohort")),
  Label = c(
    sprintf("%.2f(%.2f)", auc_value1, accuracy_value1),
    sprintf("%.2f(%.2f)", auc_value2, accuracy_value2)
  ),
  x = c(0.7, 0.7),
  y = c(0.3, 0.15))

model_colors <- c("#EE7E77", "#68A7BE")
line_types <- c(1, 1)


p1 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model, linetype = Model)) +
  geom_line(size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +  
  geom_text(data = labels_data, 
            aes(x = x, y = y, label = Label, color = Model),
            size = 4, fontface = "bold", show.legend = FALSE) +
  labs(x = "False Positive Rate", y = "True Positive Rate", 
       color = "Cohort", linetype = "Cohort") +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "right",  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11), 
    axis.text = element_text(color = "black", size = 8),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm')) + 
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = line_types) + 
  coord_fixed(ratio = 1)
p1




## Validation of metabolites related to acute human diseases in the population
data <- read.csv("NewmetaVerify1.csv", stringsAsFactors = TRUE)
data$Group <- as.factor(data$Group)
for (i in 2:11) {data[, i] <- as.numeric(data[, i])}

set.seed(123)
train_index <- createDataPartition(data$Group, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data1 <- data[-train_index, ]

data2 <- read.csv("NewmetaVerify2.csv", stringsAsFactors = TRUE)
data2$Group <- as.factor(data2$Group)  
data2$Group <- factor(data2$Group, levels = levels(train_data$Group))
for (i in 2:11) {data[, i] <- as.numeric(data[, i])}

trControl <- trainControl(method = "cv", number = 10) 
discriminant_model <- train(Group ~ ., data = train_data, method = "lda", trControl = trControl)

roc_data <- data.frame()

preds_test1 <- predict(discriminant_model, test_data1)
probs_test1 <- predict(discriminant_model, test_data1, type = "prob")[, "H"]
roc_curve1 <- roc(test_data1$Group, probs_test1, levels = c("L", "H"), direction = "<")
auc_value1 <- auc(roc_curve1)
## Area under the curve: 0.8673
cm_test1 <- confusionMatrix(preds_test1, test_data1$Group)
accuracy_value1 <- cm_test1$overall['Accuracy']
## 0.8095238   
roc_points1 <- data.frame(FPR = 1 - roc_curve1$specificities, 
                          TPR = roc_curve1$sensitivities, 
                          Model = "Verify Cohort1 Test")
roc_data <- rbind(roc_data, roc_points1)

preds_test2 <- predict(discriminant_model, data2)
probs_test2 <- predict(discriminant_model, data2, type = "prob")[, "H"]
roc_curve2 <- roc(data2$Group, probs_test2, levels = c("L", "H"), direction = "<")
auc_value2 <- auc(roc_curve2)
## Area under the curve: 0.8444
cm_test2 <- confusionMatrix(preds_test2, data2$Group)
accuracy_value2 <- cm_test2$overall['Accuracy']
## 0.7916667
roc_points2 <- data.frame(FPR = 1 - roc_curve2$specificities, 
                          TPR = roc_curve2$sensitivities, 
                          Model = "Verify Cohort2")
roc_data <- rbind(roc_data, roc_points2)

labels_data <- data.frame(
  Model = factor(c("Verify Cohort1 Test", "Verify Cohort2")),
  Label = c(
    sprintf("%.2f(%.2f)", auc_value1, accuracy_value1),
    sprintf("%.2f(%.2f)", auc_value2, accuracy_value2)
  ),
  x = c(0.7, 0.7),
  y = c(0.3, 0.15))

model_colors <- c("#EE7E77", "#68A7BE")
line_types <- c(1, 1)

p1 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model, linetype = Model)) +
  geom_line(size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +  
  geom_text(data = labels_data, 
            aes(x = x, y = y, label = Label, color = Model),
            size = 4, fontface = "bold", show.legend = FALSE) +
  labs(x = "False Positive Rate", y = "True Positive Rate", 
       color = "Cohort", linetype = "Cohort") +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "right",  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11), 
    axis.text = element_text(color = "black", size = 8),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm')) + 
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = line_types) + 
  coord_fixed(ratio = 1)
p1


## Metabolites related to human acute diseases are mutually verified in G1 and G2
data <- read.csv("G1Newmeta.csv", stringsAsFactors = TRUE)
data$Group <- as.factor(data$Group)

set.seed(123)
train_index <- createDataPartition(data$Group, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data1 <- data[-train_index, ]

data2 <- read.csv("G2Newmeta.csv", stringsAsFactors = TRUE)
data2$Group <- as.factor(data2$Group)  
data2$Group <- factor(data2$Group, levels = levels(train_data$Group))

trControl <- trainControl(method = "cv", number = 10) 
discriminant_model <- train(Group ~ ., data = train_data, method = "lda", trControl = trControl)

roc_data <- data.frame()

preds_test1 <- predict(discriminant_model, test_data1)
probs_test1 <- predict(discriminant_model, test_data1, type = "prob")[, "H"]
roc_curve1 <- roc(test_data1$Group, probs_test1, levels = c("L", "H"), direction = "<")
auc_value1 <- auc(roc_curve1)
## Area under the curve: 0.6942
cm_test1 <- confusionMatrix(preds_test1, test_data1$Group)
accuracy_value1 <- cm_test1$overall['Accuracy']
## 0.8076923  
roc_points1 <- data.frame(FPR = 1 - roc_curve1$specificities, 
                          TPR = roc_curve1$sensitivities, 
                          Model = "G1 Test")
roc_data <- rbind(roc_data, roc_points1)

preds_test2 <- predict(discriminant_model, data2)
probs_test2 <- predict(discriminant_model, data2, type = "prob")[, "H"]
roc_curve2 <- roc(data2$Group, probs_test2, levels = c("L", "H"), direction = "<")
auc_value2 <- auc(roc_curve2)
## Area under the curve: 0.6838
cm_test2 <- confusionMatrix(preds_test2, data2$Group)
accuracy_value2 <- cm_test2$overall['Accuracy']
##  0.7142857  
roc_points2 <- data.frame(FPR = 1 - roc_curve2$specificities, 
                          TPR = roc_curve2$sensitivities, 
                          Model = "G2 Cohort")
roc_data <- rbind(roc_data, roc_points2)

labels_data <- data.frame(
  Model = factor(c("G1 Test", "G2 Cohort")),
  Label = c(
    sprintf("%.2f(%.2f)", auc_value1, accuracy_value1),
    sprintf("%.2f(%.2f)", auc_value2, accuracy_value2)
  ),
  x = c(0.7, 0.7),
  y = c(0.3, 0.15))

model_colors <- c("#EE7E77", "#68A7BE")
line_types <- c(1, 1)

p1 <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model, linetype = Model)) +
  geom_line(size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +  
  geom_text(data = labels_data, 
            aes(x = x, y = y, label = Label, color = Model),
            size = 4, fontface = "bold", show.legend = FALSE) +
  labs(x = "False Positive Rate", y = "True Positive Rate", 
       color = "Cohort", linetype = "Cohort") +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "right",  
    panel.background = element_blank(),  
    panel.grid = element_blank(),  
    axis.title = element_text(color = "black", size = 11), 
    axis.text = element_text(color = "black", size = 8),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm')) + 
  scale_color_manual(values = model_colors) +
  scale_linetype_manual(values = line_types) + 
  coord_fixed(ratio = 1)
p1



## Calculate the AUC value of each combination in a loop
## Conduct analysis on the server
## Run Rscript G12_Biomarker.r. In the G1 queue, 80% of the training set data and 20% of the test set data are included. G2 is also used as the test set data (randomly select 3000 combinations, including all combinations).
## Run Rscript V12_Biomarker.r. In the Verify1 queue, 80% of the training set data and 20% of the test set data are included. Verify2 is also used as the test set data (randomly select 3000 combinations, including all combinations).

## Visualization of results
setwd("/Users/liupeihao/Desktop")

library(dplyr)
library(ggplot2)
library(plotrix) 

data <- read.csv("MortalityAUC.csv")
head(data)

summary_data <- data %>%
  group_by(Num_Metabolites, Group) %>%
  summarise(mean_AUC = mean(AUC),se_AUC = std.error(AUC),.groups = 'drop')

group_colors <- c("G1 Test" = "#EE7E77", "G2 Cohort" = "#68A7BE",
                  "Verify Cohort1 Test" = "#EDAFC6", "Verify Cohort2" = "#6FBFDA")

point_shapes <- c("G1 Test" = 16,"G2 Cohort" = 16,      
                  "Verify Cohort1 Test" = 18,"Verify Cohort2" = 18)

p1 <- ggplot(summary_data, aes(x = Num_Metabolites, color = Group)) +
  geom_line(aes(y = mean_AUC), size = 0.8) +
  geom_point(aes(y = mean_AUC, shape = Group, size = ifelse(Group %in% c("G1 Test", "G2 Cohort"), "G", "V")),show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_AUC - 1 * se_AUC, 
                    ymax = mean_AUC + 1 * se_AUC),
                    width = 0.4, size = 0.8) +
  scale_color_manual(values = group_colors) +
  scale_shape_manual(values = point_shapes) +
  scale_size_manual(values = c("G" = 1.8, "V" = 2.5),guide = "none") +
  labs(x = "Number of Metabolites", y = "AUC") +
  scale_x_continuous(limits = c(0, 32),breaks = seq(0, 32, by = 4),expand = expansion(add = c(0, 0.5))) +
  scale_y_continuous(limits = c(0.4, 1),breaks = seq(0.4, 1, by = 0.2),expand = expansion(add = c(0, 0.01))) +
  theme_classic() +
  theme(
    axis.title = element_text(color = "black", size = 11), 
    axis.text = element_text(color = "black", size = 8),  
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
    axis.ticks = element_line(size = 0.6),
    axis.ticks.length = unit(0.12, "cm"),
    legend.position = "bottom",
    legend.title = element_blank())
p1


## Statistics of AUC values of metabolites in different numbers of models
## Run the "extractAUC.py" code, count the AUC values in the model corresponding to each metabolite in the G1 and G2 queues, generate "extracted_aucs.txt", and then organize them in excel to generate the "extracted_aucs.csv" file
setwd("/Users/liupeihao/Desktop")
library(ggplot2)  

data <- read.csv("G12_extracted_aucs.csv")
head(data)

metabolite_order <- c(
  "HMDB0061700", "HMDB0011549", "Butyrate", "Hexyl glucoside",
  "LMFA01140039", "LMFA01060107", "dinor-12-oxo PDA", "Allylestrenol",
  "3-Oxohexanoic acid", "L-Cysteine", "Pyrraline", "LMFA01050242",
  "5-Iodouracil", "HMDB0030994", "HMDB0255307", "HMDB0253651",
  "Citrulline", "L-Tyrosine", "N-acetyltryptophan", "Indole-3-propionic acid",
  "Inosine", "Hypoxanthine", "Allantoin", "Pseudouridine",
  "L-Histidine", "L-Acetylcarnitine", "L-carnitine",
  "Uridine", "Serotonin", "Glycerol 3-phosphate", "L-Glutamic acid")

data$Metabolite <- factor(data$Metabolite, levels = metabolite_order)

p1 <- ggplot(data = data, aes(x = factor(Metabolite  ), y = AUC, fill = Group)) +
  geom_boxplot(outlier.size = 0.3) +
  scale_fill_manual(name = "Cohort", 
                    values = c("G1" = "#EE7E77", "G2" = "#68A7BE"),
                    labels = c("G1" = "G1 Test", "G2" = "G2 Cohort")) +
  labs(x = "", y = "AUC") +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, by = 0.2)) +
  theme_classic() +
  theme(axis.title = element_text(color = "black", size = 11),
        axis.text = element_text(color = "black", size = 8),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        axis.ticks = element_line(size = 0.6),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  
        axis.ticks.length = unit(0.12, "cm"))+
  geom_vline(xintercept = 16.5, linetype = "32", color = "gray40", size = 0.5)+
  geom_vline(xintercept = 20.5, linetype = "32", color = "gray40", size = 0.5)+
  geom_vline(xintercept = 24.5, linetype = "32", color = "gray40", size = 0.5)
p1


## Run the "extractAUC.py" code, count the AUC values in the model corresponding to each metabolite in the G1 and G2 queues, generate "extracted_aucs.txt", and then organize them in excel to generate the "extracted_aucs.csv" file
data <- read.csv("V12_extracted_aucs.csv")
head(data)

metabolite_order <- c(
  "Butyrate", "Cysteine", "Pyrraline", 
  "Citrulline", "N-acetyltryptophan", 
  "Inosine", "Hypoxanthine", "Allantoin", "Pseudouridine", 
  "Histidine", "Acetylcarnitine", "Carnitine", "Uridine", "Serotonin", "Glycerol 3-phosphate")
  
data$Metabolite <- factor(data$Metabolite, levels = metabolite_order)

p1 <- ggplot(data = data, aes(x = factor(Metabolite  ), y = AUC, fill = Group)) +
  geom_boxplot(outlier.size = 0.3) +
  scale_fill_manual(name = "Cohort", 
                    values = c("G1" = "#EE7E77", "G2" = "#68A7BE"),
                    labels = c("G1" = "Verify Cohort1 Test", "G2" = "Verify Cohort2")) +
  labs(x = "", y = "AUC") +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(0.2, 1, by = 0.2)) +
  theme_classic() +
  theme(axis.title = element_text(color = "black", size = 11),
        axis.text = element_text(color = "black", size = 8),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        axis.ticks = element_line(size = 0.6),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  
        axis.ticks.length = unit(0.12, "cm"))+
  geom_vline(xintercept = 3.5, linetype = "32", color = "gray40", size = 0.5)+
  geom_vline(xintercept = 5.5, linetype = "32", color = "gray40", size = 0.5)+
  geom_vline(xintercept = 9.5, linetype = "32", color = "gray40", size = 0.5)
p1


## The distribution of life-related metabolites
## Metabolites source: Plasma metabolomic profiles associated with mortality and longevity in a prospective analysis of 13,512 individuals
setwd("/Users/liupeihao/Desktop")
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
library(ggpubr)

data1 <-read.csv("knownmeta50.csv",sep=",") 
head(data1)
p1 <- ggplot(data1, aes(x = Group, y = newmeta50, fill = Group)) +  
          scale_fill_manual(values = c("#EE7E77","#68A7BE")) +    
          xlab("") + ylab("Intensity") +
          stat_boxplot(geom="errorbar",position = position_dodge(width = 0.1),width=0.1) + 
          geom_boxplot(notch = TRUE,position = position_dodge(width = 0.02),width=0.6, linewidth = 0.6, outlier.size = 0.6) +                      
          stat_summary(fun = mean, geom = "point", size = 2,color = "#D5E50A") +
          theme_classic() +
          scale_y_continuous(limits = c(3,15), expand = c(0.05, 0.05), breaks = seq(3, 15, 3)) +
          theme(
      panel.grid.major = element_line(color = "gray", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 0.8),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      axis.text.x = element_text(color = "black", size = 10),  
      axis.text.y = element_text(color = "black", size = 8),  
      axis.title.x = element_text(family = "Arial", size = 11),  
      axis.title.y = element_text( family = "Arial", size = 11), 
      plot.margin = unit(c(1, 0, 1, 1), 'cm'),
      legend.position = "none")+
    geom_signif(
    comparisons = list(c("H", "L")),
    map_signif_level = TRUE, 
    test = wilcox.test, 
    y_position = c(13.80),
    size = 0.8, color = "black", textsize = 4,  family = "Arial Black",fontface = "bold")
p1

data1 <-read.csv("knownmeta152.csv",sep=",") 
head(data1)
p2 <- ggplot(data1, aes(x = Group, y = newmeta152, fill = Group)) +  
          scale_fill_manual(values = c("#EE7E77","#68A7BE")) +    
          xlab("") + ylab("") +
          stat_boxplot(geom="errorbar",position = position_dodge(width = 0.1),width=0.1) + 
          geom_boxplot(notch = TRUE,position = position_dodge(width = 0.02),width=0.6, linewidth = 0.6, outlier.size = 0.6) +                  
          stat_summary(fun = mean, geom = "point", size = 2,color = "#D5E50A") +
          theme_classic() +
          scale_y_continuous(limits = c(16,22), expand = c(0.05, 0.05), breaks = seq(16, 22, 2)) +
          theme(
      panel.grid.major = element_line(color = "gray", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 0.8),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      axis.text.x = element_text(color = "black", size = 10),  
      axis.text.y = element_text(color = "black", size = 8),  
      axis.title.x = element_text(family = "Arial", size = 11),  
      axis.title.y = element_text( family = "Arial", size = 11), 
      plot.margin = unit(c(1, 0, 1, 0), 'cm'),
      legend.position = "none")+
    geom_signif(
    comparisons = list(c("H", "L")),
    map_signif_level = TRUE, 
    test = wilcox.test, 
    y_position = c(21.20),
    size = 0.8, color = "black", textsize = 4,  family = "Arial Black",fontface = "bold")
p2

data1 <-read.csv("knownmeta1691.csv",sep=",") 
head(data1)
p3 <- ggplot(data1, aes(x = Group, y = newmeta1691, fill = Group)) +  
          scale_fill_manual(values = c("#EE7E77","#68A7BE")) +    
          xlab("") + ylab("") +
          stat_boxplot(geom="errorbar",position = position_dodge(width = 0.1),width=0.1) + 
          geom_boxplot(notch = TRUE,position = position_dodge(width = 0.02),width=0.6, linewidth = 0.6, outlier.size = 0.6) +                
          stat_summary(fun = mean, geom = "point", size = 2,color = "#D5E50A") +
          theme_classic() +
          scale_y_continuous(limits = c(9,15), expand = c(0.05, 0.05), breaks = seq(9, 15, 2)) +
          theme(
      panel.grid.major = element_line(color = "gray", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 0.8),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      axis.text.x = element_text(color = "black", size = 10),  
      axis.text.y = element_text(color = "black", size = 8),  
      axis.title.x = element_text(family = "Arial", size = 11),  
      axis.title.y = element_text( family = "Arial", size = 11), 
      plot.margin = unit(c(1, 0, 1, 0), 'cm'),
      legend.position = "none")+
    geom_signif(
    comparisons = list(c("H", "L")),
    map_signif_level = TRUE, 
    test = wilcox.test, 
    y_position = c(14.20),
    size = 0.8, color = "black", textsize = 4,  family = "Arial Black",fontface = "bold")
p3

data1 <-read.csv("knownmeta1763.csv",sep=",") 
head(data1)
p4 <- ggplot(data1, aes(x = Group, y = newmeta1763, fill = Group)) +  
          scale_fill_manual(values = c("#EE7E77","#68A7BE")) +    
          xlab("") + ylab("") +
          stat_boxplot(geom="errorbar",position = position_dodge(width = 0.1),width=0.1) + 
          geom_boxplot(notch = TRUE,position = position_dodge(width = 0.02),width=0.6, linewidth = 0.6, outlier.size = 0.6) +                     
          stat_summary(fun = mean, geom = "point", size = 2,color = "#D5E50A") +
          theme_classic() +
          scale_y_continuous(limits = c(14,17.2), expand = c(0.05, 0.05), breaks = seq(14, 17, 1)) +
          theme(
      panel.grid.major = element_line(color = "gray", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 0.8),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      axis.text.x = element_text(color = "black", size = 10),  
      axis.text.y = element_text(color = "black", size = 8),  
      axis.title.x = element_text(family = "Arial", size = 11),  
      axis.title.y = element_text( family = "Arial", size = 11), 
      plot.margin = unit(c(1, 1, 1, 0), 'cm'),
      legend.position = "none")+
    geom_signif(
    comparisons = list(c("H", "L")),
    map_signif_level = TRUE, 
    test = wilcox.test, 
    y_position = c(16.80),
    size = 0.8, color = "black", textsize = 4,  family = "Arial Black",fontface = "bold")
p4

library(patchwork) 
p <- p1+p2+p3+p4+ plot_layout(nrow = 1)
p



## Venn Diagram of Genes Associated with Human Metabolic Syndrome (MetS)
Data source: Multivariate genomic analysis of 5 million people elucidates the genetic architecture of shared components of the metabolic syndrome.
setwd("/Users/liupeihao/Desktop")
install.packages("ggVennDiagram")
library(ggVennDiagram)
library(ggplot2)
library(dplyr)
otu_tax<-read.csv("Venn.csv")
head(otu_tax)
df1 <- otu_tax$MetS
df2<-otu_tax$Biomarker
Venn_data<-list(MetS=df1,Biomarker=df2)
ggVennDiagram(Venn_data, set_size = 6, set_color = "grey40") +scale_fill_gradient(low ="#DEEEED", high = "#EE7E77")+  
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) 



## Correlation Coefficient of Biomarker related genes with mortality
setwd("/Users/liupeihao/Desktop")
library(readr)  
library(dplyr) 
library(tidyr)  
data <- read.csv("livergene17meta_correlation.csv")   
results <- data.frame(Metabolite = character(), Kendall_Correlation = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)  
for (i in 3:176) {  
  metabolite <- colnames(data)[i]  
  test_result <- cor.test(data[[i]], data[[2]], method = "kendall")  
  results <- rbind(results, data.frame(Metabolite = metabolite,   
                                        Kendall_Correlation = test_result$estimate,   
                                        P_Value = test_result$p.value))}  
write.csv(results, "kendall_correlation_results.csv")  

## Results Visualization
data<-read.csv("kendall_correlation_results.csv")
head(data)
p1 <- ggplot(data, aes(x = Kendall_Correlation, y = logp, color = Group, fill = Group)) +
      geom_point(shape = 21, size = 2,color="black") + 
      scale_fill_manual(values = c('#68A7BE', 'gray',"#EE7E77")) +  
      xlab("Kendall's r") + ylab(expression(-log[10](p-value))) +  +  
      theme(
        axis.title = element_text( size = 11, colour = 'black'),
        axis.text = element_text( size = 8 ,colour = 'black'), 
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1))+ 
      scale_x_continuous(limits = c(-0.6, 0.6)) +  
      scale_y_continuous(limits = c(0, 4))+
      geom_hline(yintercept = 1.3, color = "#D1287A", linetype = "dashed", size = 0.5) 
p1


## Visualization of the MR results of related metabolites.
## Data source: Genetic mapping of serum metabolome to chronic diseases among Han Chinese
setwd("/Users/liupeihao/Desktop")
library(tidyverse)
library(patchwork)  
install.packages("ggprism")
library(ggprism) 
library(GGally)
library(grid)
library(ggplot2)

data_pro<-read.csv("cysteine.csv")
head(data_pro)
p1 <- data_pro %>%   
  ggplot(aes(log2pvalue, TraitMethod, fill =Trait )) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.4, color="black", show.legend = FALSE) +  
  geom_stripped_rows() +  
  scale_x_continuous(expand = c(0, 0),  
                     limits = c(0, 6),  
                     breaks = c(0, 2, 4, 6),  
                     labels = c(0, 2, 4, 6),  
                     guide = "prism_offset") +   
  labs(x = "-log2(p-value)", y = "Trait/Method") +  
  theme_prism(base_line_size = 0.5) +  
  theme(  
        panel.background = element_blank(),  
        plot.background = element_blank(),  
        axis.text.y = element_text(color = "black", size = 9, face = "plain"),  
        axis.text.x = element_text(color = "black", size = 9, face = "plain", angle = 90, hjust = 1),  
        axis.title.x = element_text(color = "black", size = 11, face = "plain"),  
        axis.title.y = element_text(color = "black", size = 11, face = "plain"),  
        plot.margin = unit(c(0.1, 0.5, 0.5, 0.5), "cm"),
        axis.ticks.length = unit(0.1, "cm")) +  
  scale_fill_manual(values = c("#68A7BE", "#68A7BE"))+
  coord_flip() 
p2 <- data_pro %>% 
  ggplot(aes(y=TraitMethod))+
  geom_errorbar(aes(xmin = Beatse1 , xmax =Betase2),
                position = position_dodge(0.62), width = 0.3,size=0.5)+
  geom_point(aes(x=Beta),shape = 21,size=3.2,fill="#EE7E77")+
  scale_x_continuous(limits = c(-0.18,0.06),
                     breaks = c(-0.18,-0.12,-0.06,0, 0.06),
                     labels = c(-0.18,-0.12,-0.06,0, 0.06),
                     guide = "prism_offset",
                     expand = c(0,0))+
  geom_stripped_rows()+
  labs(y=NULL)+
  coord_cartesian(clip="off")+
  theme_prism(base_line_size = 0.5) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.line.x=element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text.y=element_text(color="black",size=9,face = "plain"),
        axis.title.y=element_text(color="black",size=11,face = "plain"),
        plot.margin = unit(c(0.5,0.5,0.1,0.5),"cm"),
        axis.ticks.length = unit(0.1, "cm"))+
  coord_flip() 
p<-p2/p1
p

data_pro<-read.csv("S-Allyl-L-cysteine.csv")
head(data_pro)
p1 <- data_pro %>%   
  ggplot(aes(log2pvalue, TraitMethod, fill =Trait )) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.4, color="black", show.legend = FALSE) +  
  geom_stripped_rows() +  
  scale_x_continuous(expand = c(0, 0),  
                     limits = c(0, 8),  
                     breaks = c(0, 2, 4, 6, 8),  
                     labels = c(0, 2, 4, 6, 8),  
                     guide = "prism_offset") +   
  labs(x = "-log2(p-value)", y = "Trait/Method") +  
  theme_prism(base_line_size = 0.5) +  
  theme(  
        panel.background = element_blank(),  
        plot.background = element_blank(),  
        axis.text.y = element_text(color = "black", size = 9, face = "plain"),  
        axis.text.x = element_text(color = "black", size = 9, face = "plain", angle = 90, hjust = 1),  
        axis.title.x = element_text(color = "black", size = 11, face = "plain"),  
        axis.title.y = element_text(color = "black", size = 11, face = "plain"),  
        plot.margin = unit(c(0.1, 0.5, 0.5, 0.5), "cm"),
        axis.ticks.length = unit(0.1, "cm")) +  
  scale_fill_manual(values = c("#68A7BE", "#68A7BE"))+
  coord_flip()
p2 <- data_pro %>% 
  ggplot(aes(y=TraitMethod))+
  geom_errorbar(aes(xmin = Beatse1 , xmax =Betase2),
                position = position_dodge(0.62), width = 0.3,size=0.5)+
  geom_point(aes(x=Beta),shape = 21,size=3.2,fill="#EE7E77")+
  scale_x_continuous(limits = c(-0.1,0.05),
                     breaks = c(-0.1,-0.05,0, 0.05),
                     labels = c(-0.1,-0.05,0, 0.05),
                     guide = "prism_offset",
                     expand = c(0,0))+
  geom_stripped_rows()+
  labs(y=NULL)+
  coord_cartesian(clip="off")+
  theme_prism(base_line_size = 0.5) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.line.x=element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text.y=element_text(color="black",size=9,face = "plain"),
        axis.title.y=element_text(color="black",size=11,face = "plain"),
        plot.margin = unit(c(0.5,0.5,0.1,0.5),"cm"),
        axis.ticks.length = unit(0.1, "cm"))+
  coord_flip() 
p<-p2/p1
p

data_pro<-read.csv("S-Glutathionyl-L-cysteine.csv")
head(data_pro)
p1 <- data_pro %>%   
  ggplot(aes(log2pvalue, TraitMethod, fill =Trait )) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.4, color="black", show.legend = FALSE) +  
  geom_stripped_rows() +  
  scale_x_continuous(expand = c(0, 0),  
                     limits = c(0, 15),  
                     breaks = c(0, 5, 10, 15),  
                     labels = c(0, 5, 10, 15),  
                     guide = "prism_offset") +   
  labs(x = "-log2(p-value)", y = "Trait/Method") +  
  theme_prism(base_line_size = 0.5) +  
  theme(  
        panel.background = element_blank(),  
        plot.background = element_blank(),  
        axis.text.y = element_text(color = "black", size = 9, face = "plain"),  
        axis.text.x = element_text(color = "black", size = 9, face = "plain", angle = 90, hjust = 1),  
        axis.title.x = element_text(color = "black", size = 11, face = "plain"),  
        axis.title.y = element_text(color = "black", size = 11, face = "plain"),  
        plot.margin = unit(c(0.1, 0.5, 0.5, 0.5), "cm"),
        axis.ticks.length = unit(0.1, "cm")) +  
  scale_fill_manual(values = c("#68A7BE", "#68A7BE", "#68A7BE"))+
  coord_flip()
p2 <- data_pro %>% 
  ggplot(aes(y=TraitMethod))+
  geom_errorbar(aes(xmin = Beatse1 , xmax =Betase2),
                position = position_dodge(0.62), width = 0.3,size=0.5)+
  geom_point(aes(x=Beta),shape = 21,size=3.2,fill="#EE7E77")+
  scale_x_continuous(limits = c(-0.05,0.05),
                     breaks = c(-0.05,0, 0.05),
                     labels = c(-0.05,0, 0.05),
                     guide = "prism_offset",
                     expand = c(0,0))+
  geom_stripped_rows()+
  labs(y=NULL)+
  coord_cartesian(clip="off")+
  theme_prism(base_line_size = 0.5) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.line.x=element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text.y=element_text(color="black",size=9,face = "plain"),
        axis.title.y=element_text(color="black",size=11,face = "plain"),
        plot.margin = unit(c(0.5,0.5,0.1,0.5),"cm"),
        axis.ticks.length = unit(0.1, "cm"))+
  coord_flip() 
p<-p2/p1
p

data_pro<-read.csv("LPC182.csv")
head(data_pro)
p1 <- data_pro %>%   
  ggplot(aes(log2pvalue, TraitMethod, fill =Trait )) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.4, color="black", show.legend = FALSE) +  
  geom_stripped_rows() +  
  scale_x_continuous(expand = c(0, 0),  
                     limits = c(0, 12),  
                     breaks = c(0, 4, 8, 12),  
                     labels = c(0, 4, 8, 12),  
                     guide = "prism_offset") +   
  labs(x = "-log2(p-value)", y = "Trait/Method") +  
  theme_prism(base_line_size = 0.5) +  
  theme(  
        panel.background = element_blank(),  
        plot.background = element_blank(),  
        axis.text.y = element_text(color = "black", size = 9, face = "plain"),  
        axis.text.x = element_text(color = "black", size = 9, face = "plain", angle = 90, hjust = 1),  
        axis.title.x = element_text(color = "black", size = 11, face = "plain"),  
        axis.title.y = element_text(color = "black", size = 11, face = "plain"),  
        plot.margin = unit(c(0.1, 0.5, 0.5, 0.5), "cm"),
        axis.ticks.length = unit(0.1, "cm")) +  
  scale_fill_manual(values = c("#68A7BE", "#68A7BE", "#68A7BE"))+
  coord_flip()
p2 <- data_pro %>% 
  ggplot(aes(y=TraitMethod))+
  geom_errorbar(aes(xmin = Beatse1 , xmax =Betase2),
                position = position_dodge(0.62), width = 0.3,size=0.5)+
  geom_point(aes(x=Beta),shape = 21,size=3.2,fill="#EE7E77")+
  scale_x_continuous(limits = c(0.01,0.03),
                     breaks = c(0.01,0.02, 0.03),
                     labels = c(0.01,0.02, 0.03),
                     guide = "prism_offset",
                     expand = c(0,0))+
  geom_stripped_rows()+
  labs(y=NULL)+
  coord_cartesian(clip="off")+
  theme_prism(base_line_size = 0.5) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.line.x=element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text.y=element_text(color="black",size=9,face = "plain"),
        axis.title.y=element_text(color="black",size=11,face = "plain"),
        plot.margin = unit(c(0.5,0.5,0.1,0.5),"cm"),
        axis.ticks.length = unit(0.1, "cm"))+
  coord_flip() 
p<-p2/p1
p

data_pro<-read.csv("LPC183.csv")
head(data_pro)
p1 <- data_pro %>%   
  ggplot(aes(log2pvalue, TraitMethod, fill =Trait )) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.4, color="black", show.legend = FALSE) +  
  geom_stripped_rows() +  
  scale_x_continuous(expand = c(0, 0),  
                     limits = c(0, 12),  
                     breaks = c(0, 4, 8, 12),  
                     labels = c(0, 4, 8, 12),  
                     guide = "prism_offset") +   
  labs(x = "-log2(p-value)", y = "Trait/Method") +  
  theme_prism(base_line_size = 0.5) +  
  theme(  
        panel.background = element_blank(),  
        plot.background = element_blank(),  
        axis.text.y = element_text(color = "black", size = 9, face = "plain"),  
        axis.text.x = element_text(color = "black", size = 9, face = "plain", angle = 90, hjust = 1),  
        axis.title.x = element_text(color = "black", size = 11, face = "plain"),  
        axis.title.y = element_text(color = "black", size = 11, face = "plain"),  
        plot.margin = unit(c(0.1, 0.5, 0.5, 0.5), "cm"),
        axis.ticks.length = unit(0.1, "cm")) +  
  scale_fill_manual(values = c("#68A7BE", "#68A7BE", "#68A7BE"))+
  coord_flip()
p2 <- data_pro %>% 
  ggplot(aes(y=TraitMethod))+
  geom_errorbar(aes(xmin = Beatse1 , xmax =Betase2),
                position = position_dodge(0.62), width = 0.3,size=0.5)+
  geom_point(aes(x=Beta),shape = 21,size=3.2,fill="#EE7E77")+
  scale_x_continuous(limits = c(-0.1,0),
                     breaks = c(-0.1,-0.08, -0.06,-0.04,-0.02,0),
                     labels = c(-0.1,-0.08, -0.06,-0.04,-0.02,0),
                     guide = "prism_offset",
                     expand = c(0,0))+
  geom_stripped_rows()+
  labs(y=NULL)+
  coord_cartesian(clip="off")+
  theme_prism(base_line_size = 0.5) +
 theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.line.x=element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text.y=element_text(color="black",size=9,face = "plain"),
        axis.title.y=element_text(color="black",size=11,face = "plain"),
        plot.margin = unit(c(0.5,0.5,0.1,0.5),"cm"),
        axis.ticks.length = unit(0.1, "cm"))+
  coord_flip() 
p<-p2/p1
p



## Enrichment Analysis in 114 Human Diseases
## Background data source: PTWAS: Investigating tissue-relevant causal molecular mechanisms of complex traits using probabilistic TWAS analysis.
## Enrichment analysis by Python
import pandas as pd  
from scipy.stats import hypergeom  
human_data = pd.read_csv("Human114complextrait.txt", sep="\t")  
benzenoids_data = pd.read_csv("17metagene.txt", sep="\t", header=None, names=["Gene_name"])  
trait_counts = human_data.groupby('Trait')['Gene_name'].nunique().reset_index()  
trait_counts.columns = ['Trait', 'Total_Genes']    
benzenoids_counts = human_data[human_data['Gene_name'].isin(benzenoids_data['Gene_name'])]  
benzenoids_counts = benzenoids_counts.groupby('Trait')['Gene_name'].nunique().reset_index()  
benzenoids_counts.columns = ['Trait', 'Benzenoids_Genes']  
enrichment_results = pd.merge(trait_counts, benzenoids_counts, on='Trait', how='left').fillna(0)  
enrichment_results['Enrichment_Factor'] = enrichment_results['Benzenoids_Genes'] / enrichment_results['Total_Genes']  
enrichment_results['p_value'] = hypergeom.sf(enrichment_results['Benzenoids_Genes'] - 1,  
                                               enrichment_results['Total_Genes'].sum(),  
                                               enrichment_results['Benzenoids_Genes'].sum(),  
                                               enrichment_results['Total_Genes'])  
enrichment_results['p_adj'] = enrichment_results['p_value'] * len(enrichment_results)  
enrichment_results['p_adj'] = enrichment_results['p_adj'].clip(upper=1)   
enrichment_results['Gene_Ratio'] = enrichment_results['Benzenoids_Genes'] / enrichment_results['Total_Genes']  
enrichment_results.to_csv("17meta114traits.txt", sep="\t", index=False)    
print(enrichment_results)

## Results Visualization
library(ggplot2)
library(ggforce)
library(grid) 
library(cowplot) 

data <- read.csv("17meta114traits.csv", stringsAsFactors = TRUE)
head(data)

data$Trait<- factor(data$Trait, levels = unique(data$Trait)) 

p1 <- ggplot(data, aes(x = Trait, y = lo2p)) +   
     geom_bar(stat = "identity",  width = 0.8, fill = "#68A7BE", colour = "#000000") +    
     labs(x = expression(),y = expression(-log[2](p-value))) +
     theme(  
        axis.title = element_text(size = 11, colour = 'black'),  
        axis.text.x = element_text(size = 8, colour = 'black',vjust = 0),  
        axis.text.y = element_text(size = 9, colour = 'black'),   
        axis.line = element_line(size = 0.5, colour = 'black'),  
        axis.ticks = element_line(color = "black"),  
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.5, 0.5, 0.5, 0, "cm"),  
        panel.background = element_blank(),   
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +
     coord_flip()
p1


p2 <- ggplot(data, aes(x = Trait, y = -lo2gr)) +   
     geom_bar(stat = "identity", width = 0.8, fill = "#EE7E77", colour = "#000000") +   
     labs(x = expression(),y = expression(-log[2](Genecount))) +
     scale_y_continuous(  
         labels = function(x) abs(x)) +
     theme(  
        axis.title = element_text(size = 11, colour = 'black'),  
        axis.text.x = element_text(size = 8, colour = 'black'),  
        axis.text.y = element_blank(),
        axis.line = element_line(size = 0.5, colour = 'black'),  
        axis.ticks = element_line(color = "black"), 
        plot.margin = margin(0.5, 0.5, 0.5, 0, "cm"), 
        plot.title = element_text(hjust = 0.5),  
        panel.background = element_blank(),   
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()
     ) +
     coord_flip() +
     scale_x_discrete(position = "top") 
p2 

p3 <- plot_grid(p2, p1, nrow = 1,align = "h",rel_widths = c(0.59, 1))+
     theme(plot.margin = margin(0, 1, 0, 1, "cm"))
p3



## Biomarker related gene enrichment
setwd("/Users/liupeihao/Desktop")
install.packages('aPEAR')
library(aPEAR)
install.packages('BiocManager') 
BiocManager::install('clusterProfiler')
library(clusterProfiler)
BiocManager::install('org.Gg.eg.db')
library(org.Gg.eg.db)
library(DOSE)
library(ggplot2)
install.packages('cols4all')
library(cols4all)

gene_ids <- read.csv("17metageneID.csv", header = TRUE, stringsAsFactors = FALSE)
head(gene_ids)

# Conduct GO enrichment analysis
GO_all_diff <- enrichGO(gene = gene_ids, OrgDb = org.Gg.eg.db, keyType = 'ENTREZID', ont = "ALL",pAdjustMethod = "BH", pvalueCutoff  = 1, qvalueCutoff = 1, readable = TRUE)
head(GO_all_diff)
output_file <- "GO_results.csv"
write.csv(GO_all_diff, file = output_file, row.names = TRUE)

# Conduct KEGG enrichment analysis
kegg_enrich <- enrichKEGG(gene = gene_ids, organism = 'gga', keyType = 'ncbi-geneid', pAdjustMethod = "BH", pvalueCutoff  = 1, qvalueCutoff  = 1)
head(kegg_enrich)
output_file <- "KEGG_results.csv"
write.csv(kegg_enrich, file = output_file, row.names = TRUE)


##  Result visualization
setwd("/Users/liupeihao/Desktop")
library(tidyverse)
library(ggplot2)
library(dplyr)

## Gene
data <- read.csv('KEGG.csv',header = T)
head(data)
p1 <- ggplot(data) +  
      geom_col(aes(x = Count, y = reorder(Description, `logp`)), color = 'black', width = 0.6, fill = '#68A7BE') +  
      labs(x = 'Gene Count', y = NULL) +  
      theme_test(base_size = 11) +  
      theme(axis.text.y = element_text(angle = 0, hjust = 1),  
            axis.text = element_text(color = 'black'),  
            plot.margin = margin(0.5, 0.5, 1.5, 0.5, 'cm'),  
            panel.border = element_rect(size = 1.2)) +  
      scale_x_continuous(expand = c(0, 0.5), limits = c(0, 6),  
                         sec.axis = sec_axis(~./2, name = '-log10(p-value)', breaks = seq(0, 3, 01))) +  
      geom_line(aes(x = `logp` * 2, y = reorder(Description, `logp`), group = 1), linetype = 3, cex = 1) +  
      geom_point(aes(x = `logp` * 2, y = reorder(Description, `logp`)), color = '#E05E2F', size = 3.5) + 
      geom_text(aes(x = Count, y = reorder(Description, `logp`), label = Count), vjust = 0.5, hjust = -0.5, size = 2.5, fontface = 'bold')
p1

## eGene
data <- read.csv('GO.csv',header = T)
head(data)
p1 <- ggplot(data) +  
      geom_col(aes(x = Count, y = reorder(Description, `logp`)), color = 'black', width = 0.6, fill = '#EE7E77') +  
      labs(x = 'Gene Count', y = NULL) +  
      theme_test(base_size = 11) +  
      theme(axis.text.y = element_text(angle = 0, hjust = 1),  
            axis.text = element_text(color = 'black'),  
            plot.margin = margin(0.5, 0.5, 1.5, 0.5, 'cm'),  
            panel.border = element_rect(size = 1.2)) +  
      scale_x_continuous(expand = c(0, 0.5), limits = c(0, 12),  
                         sec.axis = sec_axis(~./4, name = '-log10(p-value)', breaks = seq(0, 3, 1))) +  
      geom_line(aes(x = `logp` * 4, y = reorder(Description, `logp`), group = 1), linetype = 3, cex = 1) +  
      geom_point(aes(x = `logp` * 4, y = reorder(Description, `logp`)), color = '#535794', size = 3.5) + 
      geom_text(aes(x = Count, y = reorder(Description, `logp`), label = Count), vjust = 0.5, hjust = -0.5, size = 2.5, fontface = 'bold')
p1




## The code of the Part 6

## Butyrate description
## G1：Comparison of H and L groups
setwd("/Users/liupeihao/Desktop")
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
library(ggpubr)
data1 <-read.csv("meta1384_42.csv",sep=",") 
head(data1)
p1 <- ggplot(data1, aes(x = Group, y = Butyricacid, fill = Group)) +  
          scale_fill_manual(values = c("#EE7E77","#68A7BE")) +    
          xlab("") + ylab("Intensity") +
          stat_boxplot(geom="errorbar",position = position_dodge(width = 0.2),width=0.1) + 
          geom_boxplot(notch = TRUE,position = position_dodge(width = 0.2),width=0.6,size=0.9,outlier.size = 1.5) +                      
          stat_summary(fun = function(x) mean(x, na.rm = TRUE), geom = "point", size = 2, color = "#D5E50A") +
          theme_classic() +
            ylim(7.5,12)+
          theme(
      panel.grid.major = element_line(color = "gray", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 1),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      axis.text.x = element_text(color = "black", size = 10),  
      axis.text.y = element_text(color = "black", size = 8),  
      axis.title.x = element_text(size = 11),  
      axis.title.y = element_text(size = 11), 
      plot.margin = margin(1, 1, 1, 1, 'cm'),
      legend.position = "none")+
    geom_signif(
    comparisons = list(c("H", "L")),
    map_signif_level = TRUE, 
    test = wilcox.test, 
    y_position = c(11.60),
    size = 0.8, color = "black", textsize = 5,  family = "Arial Black",fontface = "bold")
p1

## G2：Comparison of T（H） and C（L） groups
setwd("/Users/liupeihao/Desktop")
library(ggplot2)
library(cowplot) 
install.packages("ggpubr")
library(ggpubr)
install.packages("ggsignif")
library(ggsignif)

data$newGroup <- as.factor(data$newGroup)
p1 <- ggplot(data, aes(x = newGroup, y = butyricacid)) +  
  geom_boxplot(aes(color = newGroup), width = 0.55, size = 0.8) +  
  geom_jitter(aes(color = newGroup), width = 0.01) +  
  scale_color_manual(values = c("#68A7BE","#EE7E77")) +   
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "#631EA2") +  
  labs(x = 'Group', y = 'Intensity') +
  ylim(0, 11) +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) + 
  theme_classic() +
  theme(
      panel.grid.major = element_line(color = "gray", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 1),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      axis.text.x = element_text(color = "black", size = 11),  
      axis.text.y = element_text(color = "black", size = 8),  
      axis.title.x = element_text(size = 11),  
      axis.title.y = element_text(size = 11), 
      plot.margin = margin(1, 1, 1, 1, 'cm'),
      legend.position = "none")+    
  stat_compare_means(data = data,   
                     comparisons = list(c("C", "T")),  
                     method = "t.test",   
                     aes(group = newGroup),    
                     label = "p.signif",   
                     size = 4,     
                     label.y = 11)  
p1

## L-cysteine description
## G1：Comparison of H and L groups
setwd("/Users/liupeihao/Desktop")
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
library(ggpubr)
data1 <-read.csv("meta1374_42.csv",sep=",") 
head(data1)
p1 <- ggplot(data1, aes(x = Group, y = Lcysteine, fill = Group)) +  
          scale_fill_manual(values = c("#EE7E77","#68A7BE")) +    
          xlab("") + ylab("Intensity") +
          stat_boxplot(geom="errorbar",position = position_dodge(width = 0.2),width=0.1) + 
          geom_boxplot(notch = TRUE,position = position_dodge(width = 0.2),width=0.6,size=0.9,outlier.size = 1.5) +                      
          stat_summary(fun = mean, geom = "point", size = 2,color = "#D5E50A") +
          theme_classic() +
            ylim(8,12)+
          theme(
      panel.grid.major = element_line(color = "gray", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 1),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      axis.text.x = element_text(color = "black", size = 10),  
      axis.text.y = element_text(color = "black", size = 8),  
      axis.title.x = element_text(size = 11),  
      axis.title.y = element_text(size = 11), 
      plot.margin = margin(1, 1, 1, 1, 'cm'),
      legend.position = "none")+
    geom_signif(
    comparisons = list(c("H", "L")),
    map_signif_level = TRUE, 
    test = wilcox.test, 
    y_position = c(11.20),
    size = 0.8, color = "black", textsize = 5,  family = "Arial Black",fontface = "bold")
p1

## G2：Comparison of T（H） and C（L） groups
setwd("/Users/liupeihao/Desktop")
library(ggplot2)
library(cowplot) 
install.packages("ggpubr")
library(ggpubr)
install.packages("ggsignif")
library(ggsignif)

data <- read.csv('meta1374.csv',header = T)
head(data)
data$newGroup <- as.factor(data$newGroup)
p1 <- ggplot(data, aes(x = newGroup, y = newmeta1374)) +  
  geom_boxplot(aes(color = newGroup), width = 0.55, size = 0.8) +  
  geom_jitter(aes(color = newGroup), width = 0.01) +  
  scale_color_manual(values = c("#68A7BE","#EE7E77")) +   
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "#631EA2") +  
  labs(x = 'Group', y = 'Intensity') +
  ylim(12, 15) +
  theme_classic() +
  theme(
      panel.grid.major = element_line(color = "gray", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black", linewidth = 1),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      axis.text.x = element_text(color = "black", size = 11),  
      axis.text.y = element_text(color = "black", size = 8),  
      axis.title.x = element_text(size = 11),  
      axis.title.y = element_text(size = 11), 
      plot.margin = margin(1, 1, 1, 1, 'cm'),
      legend.position = "none")+    
  stat_compare_means(data = data,   
                     comparisons = list(c("C", "T")),  
                     method = "t.test",   
                     aes(group = newGroup),    
                     label = "p.signif",   
                     size = 4,     
                     label.y = 14.5)  
p1


## colco package was used for colocalization analysis
setwd("/Users/liupeihao/Desktop")
if(!require("remotes"))
   install.packages("remotes")
library(remotes)
install_github("chr1swallace/coloc@main", build_vignettes=TRUE)
install.packages("coloc")
library(coloc)
install.packages("dplyr")
library(dplyr)
devtools::install_github("boxiangliu/locuscomparer")
library(locuscomparer)
install.packages("readr")
library(readr)

## Cerebellum
gwas <- read.table("L-cysteineQTL_end.txt", header = TRUE)
head(gwas)
nrow(gwas)
eqtl <- read.table("CerebellumeQTL_end.txt", header = TRUE)
head(eqtl)
nrow(eqtl)
dataset1 <- list(snp = gwas$SNP,pvalues = gwas$pval_nominal,beta = gwas$beta,varbeta = gwas$varbeta,type = "quant",N = 649, sdY=0.5) 
dataset2 <- list(snp = eqtl$SNP,pvalues = eqtl$pval_nominal,type = "quant",N = 52,MAF = eqtl$af)
result <- coloc.abf(dataset1 = dataset1,dataset2 = dataset2)
need_result <- result$results %>% dplyr::arrange(desc(SNP.PP.H4))
write.csv(need_result, "Cerebellum_result.csv")
write.csv(result$summary,'Cerebellum_result_summary.csv')

## Jejunum
gwas <- read.table("Meta1374QTL_end.txt", header = TRUE)
head(gwas)
nrow(gwas)
eqtl <- read.table("JejunumeQTL_end.txt", header = TRUE)
head(eqtl)
nrow(eqtl)
dataset1 <- list(snp = gwas$SNP,pvalues = gwas$pval_nominal,beta = gwas$beta,varbeta = gwas$varbeta,type = "quant",N = 649, sdY=0.5)
dataset2 <- list(snp = eqtl$SNP,pvalues = eqtl$pval_nominal,type = "quant",N = 65, MAF = eqtl$af)
result <- coloc.abf(dataset1 = dataset1,dataset2 = dataset2)
need_result <- result$results %>% dplyr::arrange(desc(SNP.PP.H4))
write.csv(need_result, "Jejunum_result.csv")
write.csv(result$summary,'Jejunum_result_summary.csv')


## Finemapping
## Finemapping by FINEMAP
/home/cgbl/biosoft/plink1.9/plink --chr-set 35 --allow-extra-chr --bfile /home/phliu/mGWAS1-100/peihao1100 --extract site.txt --make-bed --out site
/home/cgbl/biosoft/plink1.9/plink --chr-set 35 --allow-extra-chr --bfile site --r square --out meta1374_1
awk '{gsub(/\t/, " "); print}' meta1374_1.ld > meta1374.ld
awk '{print $1,$1"_"$3,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12}' meta1374.txt > meta1374_1.txt
/home/cgbl/phliu/WGRGWAS/Finemap/finemap_v1.4_x86_64/finemap_v1.4_x86_64 --sss --in-files master --dataset 1



## Finmapping by susie
/home/cgbl/biosoft/plink1.9/plink --chr-set 35 --allow-extra-chr --bfile /home/phliu/mGWAS1-100/peihao1100 --extract newmeta1384site_susie_a.txt --make-bed --out site
/home/cgbl/biosoft/plink1.9/plink -chr-set 35 --allow-extra-chr --bfile site --recode A --out site_new
cat site_new.raw |awk '{$2="";$3="";$4="";$5="";$6="";print$0}' > recodefile2.raw
sed -s 's/     //g' recodefile2.raw > recodefile3.raw
cat recodefile3.raw | sed "1d" | awk '{$1="";print$0}' | sed 's/^[ ]*//g' > recodefile4.raw

export PATH=$PATH:/usr/lib64/R/bin
R
setwd("/home/phliu/meta1384susie/a")
a = read.table("recodefile4.raw")
b=scale(a, center=TRUE,  scale=TRUE)
write.table(b,file="matrix.txt")

a = read.table("phe.txt")
b=scale(a, center=TRUE, scale=TRUE)
write.table(b,file="Y.txt")

a = read.table("pos1.txt")
b = as.matrix(a)
write.table(b,file="pos.txt")

a = read.table("chrome.txt")
b = as.matrix(a)
write.table(b,file="chrom.txt")

a = read.table("allele1.txt")
b = as.matrix(a)
write.table(b,file="allele.txt")

a = read.table("true_coef1.txt")
b = as.matrix(a)
write.table(b,file="true_coef.txt")

Y=as.matrix(read.table("Y.txt"))
mat=as.matrix(read.table("matrix.txt"))
allele=as.matrix(read.table("allele.txt"))
chrom=as.matrix(read.table("chrom.txt"))
pos=as.matrix(read.table("pos.txt"))
true_coef=as.matrix(read.table("true_coef.txt"))
CC=list(Y,chrom,pos,mat,allele,true_coef)
names(CC)=c("Y","chrom","pos","mat","allele","true_coef")
save(CC,file = "aCC.RData")
class(CC) 

setwd("/Users/liupeihao/Desktop")
install.packages("susieR")
library(susieR)
set.seed(1)
Y=as.matrix(read.table("Y.txt"))
mat=as.matrix(read.table("matrix.txt"))
true_coef=as.matrix(read.table("true_coef.txt"))
data = load("aCC.RData")
data2 = eval(parse(text = data))
attach(data2)
dim(Y)
b <- true_coef[,2]  
par(font.axis = 2, font.lab = 2)
plot(b, pch = 16, ylab = 'effect size')
which(b != 0)
sumstats <- univariate_regression(mat, Y[,1])
z_scores <- sumstats$betahat / sumstats$sebetahat
susie_plot(z_scores, y = "z", b=b)

install.packages("Rfast")
library(Rfast)
complete_cases <- complete.cases(mat, Y[, 1])
mat_complete <- mat[complete_cases, ]
Y_complete <- Y[complete_cases, 1]
fitted <- susie(mat_complete, Y_complete, L = 4, verbose = TRUE)
print(fitted$sets)
susie_plot(fitted, y="PIP", b=b)
i  <- fitted$sets$cs[[1]]    
z3 <- cbind (i, z_scores[i], fitted$pip[i])
colnames(z3) <- c('position', 'z-score', 'PIP')
z3[order(z3[,2], decreasing = TRUE),]
write.table(z3, file = "susieR_finemapping.csv", sep = "\t", row.names = FALSE, col.names = TRUE)
z3 <- cbind (fitted$snps, z_scores, fitted$pip)
colnames(z3) <- c('z-score', 'PIP')
z3[order(z3[,2], decreasing = TRUE),]
write.table(z3, file = "susieR_finemapping_all_a.csv", sep = "\t", row.names = FALSE, col.names = TRUE)


## results visualization
## LD Calculation
/home/cgbl/biosoft/plink1.9/plink --allow-extra-chr --chr-set 35 --bfile peihao1100 --extract Butyrateloci.txt --make-bed --out CHR3
/home/cgbl/biosoft/plink1.9/plink --chr-set 35 --allow-extra-chr --bfile CHR3 --r square --out CHR3_1


/home/cgbl/biosoft/plink1.9/plink --allow-extra-chr --chr-set 35 --bfile peihao1100 --extract Lcysteine.txt --make-bed --out CHR2
/home/cgbl/biosoft/plink1.9/plink --chr-set 35 --allow-extra-chr --bfile CHR2 --r square --out CHR2_1

## ggplot visualization
setwd("/Users/liupeihao/Desktop")
library(ggplot2) 

## rs318007359：3_106476774
data<-read.csv("Butyrate.csv",header=T)
head(data)

data <- data %>%mutate(pos_mb = pos / 1000000, log10_pvalue = -log10(pvalue))

p1 <- ggplot(data, aes(x = pos_mb, y = log10_pvalue)) +  
  geom_point(aes(color = ld), shape = 16, size = 2, alpha = 1) + 
  scale_color_gradientn(name = expression(LD ~ (r^2)), colours = c("#68A7BE", "#EE7E77"),values = scales::rescale(c(min(data$ld), 0, max(data$ld))),limits = c(0, 1),
                        guide = guide_colorbar(barwidth = unit(0.5, "cm"), barheight = unit(1.8, "cm"), direction = "vertical", title.position = "top", title.hjust = 0.5, frame.colour = "black", frame.linewidth = 0.5)) +
  scale_x_continuous(name = "Chromosome 3 Position (MB)",expand = expansion(mult = c(0.02, 0.02)),breaks = scales::pretty_breaks(n = 6)) +   
  scale_y_continuous(name = expression(-log[10](p-value)),expand = expansion(mult = c(0.05, 0.02)),breaks = scales::pretty_breaks(n = 3)) +   
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_text(color = 'black', size = 9),
    axis.title = element_text(color = 'black', size = 11),
    axis.line = element_line(color = 'black', linewidth = 0.5),
    axis.ticks = element_line(color = 'black'),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0.5, 2.5, 0.5, 0.5, 'cm'),  
    legend.position = c(1.05, 0.5),  
    legend.justification = c(0, 0.5),  
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)) +
  annotate("text",x = 106.476774, y = max(data$log10_pvalue) * 0.95,
           label = "rs318007359 (PIP=0.93)",vjust = -0.5,color = "#D1287A", fontface = "bold",size = 3)
p1


## rs313565219：2_20088535
## rs732044877(PP.H4=1)：2_20134466
## rs316055999(PP.H4=0.99): 2_20652753
data<-read.csv("Lcysteine.csv",header=T)
head(data)

data <- data %>%mutate(pos_mb = pos / 1000000, log10_pvalue = -log10(pvalue))

p1 <- ggplot(data, aes(x = pos_mb, y = log10_pvalue)) +  
  geom_point(aes(color = ld), shape = 16, size = 2, alpha = 1) + 
  scale_color_gradientn(name = expression(LD ~ (r^2)), colours = c("#68A7BE", "#EE7E77"),values = scales::rescale(c(min(data$ld), 0, max(data$ld))),limits = c(0, 1),
                        guide = guide_colorbar(barwidth = unit(0.5, "cm"), barheight = unit(1.8, "cm"), direction = "vertical", title.position = "top", title.hjust = 0.5, frame.colour = "black", frame.linewidth = 0.5)) +
  scale_x_continuous(name = "Chromosome 2 Position (MB)",expand = expansion(mult = c(0.01, 0.02)),breaks = scales::pretty_breaks(n = 6)) +   
  scale_y_continuous(name = expression(-log[10](p-value)),expand = expansion(mult = c(0.1, 0.02)),breaks = scales::pretty_breaks(n = 6),limits = c(5.5, 20.5)) +   
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_text(color = 'black', size = 9),
    axis.title = element_text(color = 'black', size = 11),
    axis.line = element_line(color = 'black', linewidth = 0.5),
    axis.ticks = element_line(color = 'black'),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0.5, 2.5, 0.5, 0.5, 'cm'),  
    legend.position = c(1.05, 0.5),  
    legend.justification = c(0, 0.5),  
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)) +
    annotate("text",x = 20.134466, y = max(data$log10_pvalue) * 0.95,label = "rs732044877 (PP.H4=1)",hjust = -0.1,vjust = -2,color = "#D1287A", fontface = "bold",size = 3)
  ## annotate("text",x = 20.088535, y = max(data$log10_pvalue) * 0.95,label = "rs313565219",vjust = -0.5,color = "#D1287A", fontface = "bold",size = 3)
  ## +annotate("text",x = 20.652753, y = max(data$log10_pvalue) * 0.95,label = "rs316055999 (PP.H4=0.99)",vjust = -0.5,color = "#D1287A", fontface = "bold",size = 3)
p1



## Visualization of dual-luciferase reporter genes
setwd("/Users/liupeihao/Desktop")
library(ggpubr)
library(cowplot)
library(agricolae)
library(ggplot2)
library(stringr)

## MSRA
data <- read.csv("MSRA.csv", header = T)
head(data)

data$Group  <- as.factor(data$Group)
nom <- bartlett.test(data$relative~data$Group,data = data)
nom
oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
out <-LSD.test(oneway,"data$Group",p.adj = "none")
out
mar <- out$groups
rownamemar <- row.names(mar)
newmar <- data.frame(rownamemar, mar$`data$relative`, mar$groups)
sort <- newmar[order(newmar$rownamemar),]
rowname <- row.names(out$means)
mean <- out$means[,1]
sd <- out$means[,2]
marker <- sort$mar.groups
plotdata <- data.frame(rowname, mean, sd, marker)
plotdata

plotdata$rowname <- factor(plotdata$rowname, levels = c("4.18/TK", "MP/TK","MP+MSA/TK", "MP+MST/TK"))

p1 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(aes(fill =rowname),stat = "identity", position = "dodge", color = "Black",  width = 0.6, size = 0.5,alpha = 0.8) +  
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.5) +
    geom_jitter(data = data, aes(x = Group, y = relative, fill = Group),shape = 21, size = 2.5, position = position_jitter(width = 0.25, height = 0), show.legend = FALSE) +    
    geom_text(aes(x = rowname, y = mean + sd + 0.7, label = marker), size = 5, position = position_dodge(1)) +  
    scale_fill_manual(values = c( "#DEEEED","#68A7BE","#FEECE7","#EE7E77"),name = NULL) + 
    scale_y_continuous(expand = c(0, 0)) + 
    expand_limits(y = 8.2) +
    labs(x = "", y = str_wrap("Relative Luciferase Activity")) + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11, color = 'black'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.line = element_line(color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        plot.margin = margin(0.5, 0.5, 0, 0.5, 'cm'),
        legend.position = "bottom",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())+
    guides(fill = guide_legend(nrow = 2))
p1


## PTER
data <- read.csv("PTER.csv", header = T)
head(data)

data$Group  <- as.factor(data$Group)
nom <- bartlett.test(data$relative~data$Group,data = data)
nom
oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
out <-LSD.test(oneway,"data$Group",p.adj = "none")
out
mar <- out$groups
rownamemar <- row.names(mar)
newmar <- data.frame(rownamemar, mar$`data$relative`, mar$groups)
sort <- newmar[order(newmar$rownamemar),]
rowname <- row.names(out$means)
mean <- out$means[,1]
sd <- out$means[,2]
marker <- sort$mar.groups
plotdata <- data.frame(rowname, mean, sd, marker)
plotdata

plotdata$rowname <- factor(plotdata$rowname, levels = c("4.18/TK", "PP/TK","PP+PSA/TK", "PP+PSG/TK"))

p1 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(aes(fill =rowname),stat = "identity", position = "dodge", color = "Black",  width = 0.6, size = 0.5,alpha = 0.8) +  
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.5) +
    geom_jitter(data = data, aes(x = Group, y = relative, fill = Group),shape = 21, size = 2.5, position = position_jitter(width = 0.25, height = 0), show.legend = FALSE) +    
    geom_text(aes(x = rowname, y = mean + sd + 0.7, label = marker), size = 5, position = position_dodge(1)) +  
    scale_fill_manual(values = c( "#DEEEED","#68A7BE","#FEECE7","#EE7E77"),name = NULL) + 
    scale_y_continuous(expand = c(0, 0)) + 
    expand_limits(y = 8.2) +
    labs(x = "", y = str_wrap("Relative Luciferase Activity")) + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11, color = 'black'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.line = element_line(color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        plot.margin = margin(0.5, 0.5, 0, 0.5, 'cm'),
        legend.position = "bottom",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())+
    guides(fill = guide_legend(nrow = 2))
p1


## Gene Expression and Metabolites Intensity
/home/cgbl/biosoft/plink1.9/plink --allow-extra-chr --chr-set 35 --bfile peihao1100 --extract newmeta1384site_chr3.txt --make-bed --out CHR3
/home/cgbl/biosoft/plink1.9/plink  --chr-set 35 --allow-extra-chr  --bfile CHR3 --recode --out CHR3_1

## results visualization
setwd("/Users/liupeihao/Desktop")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggsignif")
library(ggplot2)
library(ggpubr)
library(ggsignif)
install.packages("patchwork")  
library(patchwork)

## MSRA and Butyrate
df<-read.csv("MSRAexpression.csv")
head(df)
p1 <- ggplot(df, aes(x = Group, y = FPKM)) +
  geom_boxplot(fill="#EE7E77", width = 0.3,size = 0.5, outlier.size = 0.5,position=position_nudge(x=-0.3)) + 
  theme_classic() + 
  scale_y_continuous(limits = c(7, 21), breaks = c(7, 14, 21))+
   labs(x = "rs318007359: genotype", 
       y = expression(paste("FPKM of ", italic("MSRA")))) +
  theme(
    panel.border=element_rect(color="black",linewidth=1,fill=NA),
    axis.title.x = element_text(size = 11,color = 'black'),  
    axis.title.y = element_text(size = 11,color = "#EE7E77"),  
    axis.text.x = element_text(size = 10, color = 'black'),  
    axis.text.y = element_text(size = 8,  color = "#EE7E77"),  
    axis.ticks.length = unit(0.1, "cm"),  
    axis.ticks = element_line(),  
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "#EE7E77"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),  
    legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0))+
  geom_signif(
    comparisons = list(c("A/A", "A/T"), c("A/T", "T/T"), c("A/A", "T/T")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = c(19, 18.2, 20),
    size = 0.8, color = "#EE7E77", textsize = 4) 
p1

df1<-read.csv("3_106476774.csv")
head(df1)
p2 <- ggplot(df1, aes(x = Group, y =  Intensity)) +
  geom_boxplot(fill="#68A7BE", width = 0.3,size = 0.5, outlier.size = 0.5, position=position_nudge(x=0.3)) + 
  theme_classic() + 
  scale_y_continuous(limits = c(8, 12), breaks = c(8, 10, 12), position = "right", )+
   labs(x = "rs318007359: genotype", 
       y = "Intensity of Butyrate") +
    theme(
    panel.border = element_rect(color = "black", linewidth = 1, fill = NA),
    axis.title.x = element_text(size = 11, color = 'black'),  
    axis.title.y.right = element_text(size = 11, color = "#68A7BE",margin = margin(l = 10)),  
    axis.text.x = element_text(size = 10, color = 'black'),  
    axis.text.y.right = element_text(size = 8,color = "#68A7BE",margin = margin(l = 5)),  
    axis.ticks.length = unit(0.1, "cm"),  
    axis.ticks.y.right = element_line(color = "#68A7BE"),  
    axis.line.x = element_line(color = "black"),
    axis.line.y.right = element_line(color = "#68A7BE"), 
    axis.line.y.left = element_blank(), 
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),  
    legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0)) +
  geom_signif(
    comparisons = list(c("A/A", "A/T"), c("A/T", "T/T"), c("A/A", "T/T")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = c(7, 6.7, 6),
    size = 0.8, color = "#68A7BE", textsize = 4) 
p2


## PTER and Lcysteine
df<-read.csv("PTERexpression.csv")
head(df)
p1 <- ggplot(df, aes(x = Group, y = FPKM)) +
  geom_boxplot(fill="#EE7E77", width = 0.3,size = 0.5, outlier.size = 0.5,position=position_nudge(x=-0.3)) + 
  theme_classic() + 
  scale_y_continuous(limits = c(4, 12), breaks = c(4, 8, 12))+
   labs(x = "rs732044877: genotype", 
       y = expression(paste("FPKM of ", italic("PTER")))) +
  theme(
    panel.border=element_rect(color="black",linewidth=1,fill=NA),
    axis.title.x = element_text(size = 11,color = 'black'),  
    axis.title.y = element_text(size = 11,color = "#EE7E77"),  
    axis.text.x = element_text(size = 10, color = 'black'),  
    axis.text.y = element_text(size = 8,  color = "#EE7E77"),  
    axis.ticks.length = unit(0.1, "cm"),  
    axis.ticks = element_line(),  
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "#EE7E77"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),  
    legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0))+
  geom_signif(
    comparisons = list(c("A/G", "G/G")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = c(11.3),
    size = 0.8, color = "#EE7E77", textsize = 4) 
p1

df1<-read.csv("2_20134466.csv")
head(df1)
p2 <- ggplot(df1, aes(x = Group, y =  Intensity)) +
  geom_boxplot(fill="#68A7BE", width = 0.3,size = 0.5, outlier.size = 0.5, position=position_nudge(x=0.3)) + 
  theme_classic() + 
  scale_y_continuous(limits = c(6, 12), breaks = c(6, 9, 12), position = "right", )+
   labs(x = "rs318007359: genotype", 
       y = "Intensity of L-cysteine") +
    theme(
    panel.border = element_rect(color = "black", linewidth = 1, fill = NA),
    axis.title.x = element_text(size = 11, color = 'black'),  
    axis.title.y.right = element_text(size = 11, color = "#68A7BE",margin = margin(l = 10)),  
    axis.text.x = element_text(size = 10, color = 'black'),  
    axis.text.y.right = element_text(size = 8,color = "#68A7BE",margin = margin(l = 5)),  
    axis.ticks.length = unit(0.1, "cm"),  
    axis.ticks.y.right = element_line(color = "#68A7BE"),  
    axis.line.x = element_line(color = "black"),
    axis.line.y.right = element_line(color = "#68A7BE"), 
    axis.line.y.left = element_blank(), 
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),  
    legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0)) +
  geom_signif(
    comparisons = list(c("A/A", "A/G"), c("A/G", "G/G"), c("A/A", "G/G")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = c(7, 6.7, 6.2),
    size = 0.8, color = "#68A7BE", textsize = 4) 
p2



## Intestinal microbiome correlation analysis
## Butyric acid of different group
setwd("/Users/liupeihao/Desktop")
install.packages("ggpubr")
install.packages("ggsignif")
install.packages("ggthemes")  
library(ggthemes)
library(ggplot2)
library(ggpubr)
library(ggsignif)

mydata <- read.csv("ButyricAcid.txt", sep="\t")
head(mydata)
mydata$Group <- factor(mydata$Group,levels=c("LB","HB"))
p1 <- ggplot(mydata, aes(Group, ButyricAcid, fill = Group)) +
  geom_boxplot(color = "black", width = 0.5) + 
  scale_fill_manual(values = c("#68A7BE","#EE7E77")) +
  labs(x = "Group", y = "Concentration") +
  theme(
    axis.text.x = element_text(color = 'black',  size = 8),
    axis.text.y = element_text(color = 'black',size = 8),
    axis.title = element_text(color = 'black',size = 11),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA,size = 0.7),
    panel.background = element_rect(fill = "white"),  
    plot.margin = margin(1, 1, 1, 1, 'cm'),
    legend.position = "none") + 
  ylim(c(0, 1)) +
  geom_hline(yintercept = 0.2, color = "#D1287A", linetype = "dashed", size = 0.5) +
  stat_compare_means(aes(group = Group), method = "wilcox.test", label = "p.signif", label.y = 0.98, label.x = 1.5) + 
  geom_signif(annotations = "", y_position = 0.985, xmin = 1, xmax = 2, tip_length = 0.03, size = 0.6)
p1

## Microbial abundance calculation
setwd("/Users/liupeihao/Desktop")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("ggprism")
install.packages("plyr")
library (reshape2)
library(ggplot2)
library(ggprism)
library(plyr)

## Calculation of abundances at the generic level
genus=read.table('genusGUT-1.txt',row.names=1,header=T,comment.char='',sep='\t')
head(genus)
genus$rowsum <- apply(genus[, -1], 1, sum)
genus <- genus[order(genus$rowsum, decreasing = TRUE), ]
genus <- genus[, -ncol(genus)]
genus$HB <- apply(genus[,1:50], 1, mean) 
genus$LB <- apply(genus[,51:100], 1, mean)
genus <- genus[, c("HB", "LB")]
dim(genus)
head(genus)
genus1<- genus[1:10,]/apply(genus,2,sum)
genus2 <- 1 - apply(genus1, 2, sum)
df3 <- rbind(genus1,genus2)
rownames(df3)[11] <- "Others"
df3$Genus <- factor(rownames(df3), levels = rev(rownames(df3)))
write.table(df3,"GenusAbundance.txt",sep = "\t") 
df_genus <- melt(df3,id = 'Genus')
names(df_genus)[2] <- 'group'  
p1 <- ggplot(df_genus, aes(group, 100 * value), position = "stack") +
  geom_bar(aes(fill = Genus), stat = "identity", color = "black", size = 0.4,
           position = "stack", width = 0.6, data = df_genus) +
  scale_fill_manual(values = c("#28437d","#4675b0","#6892c5","#a6badf","#e7e4f8","#f4dced","#f1ccde","#eab7ca","#d2798c","#b34557","#91d8d2")) +
  theme(axis.text.x = element_text(color = 'black',size = 10),
        axis.text.y = element_text(color = 'black',size = 10),
        axis.title = element_text(color = 'black', size = 13),
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(1, 1, 1, 1, 'cm')) +
  xlab("Group") + ylab("Relative Abundance(%)")+ 
  scale_x_discrete(name = "Groups", breaks = unique(df_genus$group), labels = unique(df_genus$group)) +
  scale_y_continuous(name = "Relative Abundance(%)", breaks = seq(0, 100, by = 20), limits = c(0, 102))  
p1

## Calculation of abundances at the phylum level
phylum=read.table('phylumGUT-1.txt',row.names=1,header=T,comment.char='',sep='\t')
head(phylum)
phylum$rowsum <- apply(phylum[, -1], 1, sum)
phylum <- phylum[order(phylum$rowsum, decreasing = TRUE), ]
phylum <- phylum[, -ncol(phylum)]
phylum$HB <- apply(phylum[,1:50], 1, mean) 
phylum$LB <- apply(phylum[,51:100], 1, mean)
phylum <- phylum[, c("HB", "LB")]
phylum1<- phylum[1:11,]/apply(phylum,2,sum)
df1<-phylum1
df1$phylum <- factor(rownames(df1), levels = rev(rownames(df1)))
write.table(df1,"PhylumAbundance.txt",sep = "\t") 
df_phylum <- melt(df1,id = 'phylum')
names(df_phylum)[2] <- 'group'  
df_phylum$group_numeric <- ifelse(as.character(df_phylum$group) == "HB", 0,  
                                   ifelse(as.character(df_phylum$group) == "LB", 1,   
                                          NA)) 
unique(df_phylum$group_numeric) 
p2 <- ggplot(df_phylum, aes(x = factor(group_numeric), y = 100 * value), position = "stack") +  
  geom_bar(aes(fill = phylum), stat = "identity", color = "black", size = 0.4,  
           position = "stack", width = 0.6) +  
  scale_fill_manual(values = c("#28437d","#4675b0","#6892c5","#a6badf","#e7e4f8","#f4dced","#f1ccde","#eab7ca","#d2798c","#b34557","#91d8d2")) +  
  theme(axis.text.x = element_text(color = 'black', size = 10),  
        axis.text.y = element_text(color = 'black', size = 10),  
        axis.title = element_text(color = 'black', size = 13),  
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
        panel.grid.minor = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1),  
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(1, 2, 1, 1, 'cm')) +  
  xlab("Group") + ylab("Relative Abundance (%)") +   
  scale_x_discrete(name = "Groups", limits = as.character(0:4), breaks = as.character(0:1),labels = c("HB", "LB")) +  
  scale_y_continuous(name = "Relative Abundance (%)", breaks = seq(0, 100, by = 20), limits = c(0, 102)) +  
  geom_magnify(from = list(xmin = 0.5, xmax = 2.5, ymin = 94, ymax = 100),  
               to = list(xmin = 3, xmax = 5, ymin = 5, ymax = 96),  
               shape = "rect")  
p2


## Alphe and Beta diversity of computation
setwd("/Users/liupeihao/Desktop")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("agricolae")
install.packages("vegan")
library(ggplot2)
library(ggpubr)
library(agricolae)
library(vegan)

## Alpha diversity
otu=read.table('genusGUT-1.txt',row.names=1,header=T,comment.char='',sep='\t')
otu=t(otu)
shannon=diversity(otu,"shannon")  
simpson=diversity(otu,"simpson")  
richness <- specnumber(otu) 
alpha=data.frame(shannon,simpson,richness)
write.table(alpha,'alpha-summary.tsv',sep = '\t',quote=F)

## Beta diversity
data <- read.table("genusGUT-1.txt", header = TRUE, sep = "\t", row.names = 1)
data <- data / apply(data, 2, sum)
data <- t(data)
group <- read.delim('Group.txt', sep = '\t', stringsAsFactors = FALSE)
bray <- vegdist(data, method = 'bray')
bray <- as.matrix(bray)
write.table(bray,"bray-crutis.txt",sep = "\t") 
pcoa <- cmdscale(bray, k = 3, eig = T)
pcoa_data <- data.frame({pcoa$point})
pcoa_data$Sample_ID <- rownames(pcoa_data)
names(pcoa_data)[1:3] <- paste0("PCoA", 1:3)
eig = pcoa$eig
total_eig <- sum(eig)
eig_percent <- round(eig / total_eig * 100, 1)
print(eig_percent)
##26.0 14.7 10.5

poi = pcoa$points
poi = as.data.frame(poi)
pcoa_result <- cbind(pcoa_data,group ) 
head(pcoa_result)
write.table(pcoa_result,"pcoa_result.txt",sep = "\t") 
dune.div <- adonis2(data ~ group, data = group, permutations = 999, method="bray")
dune.div
##           Df SumOfSqs   R2      F Pr(>F)  
## Model     1   0.1339 0.02 1.9996  0.042 *
## Residual 98   6.5647 0.98                
## Total    99   6.6987 1.00   

## Results visualization
## Alpha diversity
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggsignif")
library(ggplot2)
library(ggpubr)
library(ggsignif)

df<-read.csv("alpha.CSV")
head(df)
p1 <- ggplot(df, aes(x = Group, y = shannon, fill = Group)) +
   geom_boxplot(width = 0.5,position = position_dodge(width = 0.7),alpha = 0.35) + 
  scale_fill_manual(values = c("#EE7E77", "#68A7BE")) +
  geom_jitter(aes(fill = Group), width = 0.2, size = 1.5, alpha = 1,shape = 21,color="#000000", stroke = 0.5) +  
  scale_color_manual(values = c("#EE7E77", "#68A7BE")) + 
  theme_minimal() +  
  ylim(2,3.05)+
  labs(x = "", y = "Shannon") +
  theme(
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text = element_text(size = 8,color="black"),
    axis.ticks = element_line(size = 0.5),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(1, 0.1, 1, 1, 'cm'),
    legend.position = "none") 
p2 <- ggplot(df, aes(x = Group, y = simpson, fill = Group)) +
  geom_boxplot(width = 0.5,position = position_dodge(width = 0.7),alpha = 0.35) + 
  scale_fill_manual(values = c("#EE7E77", "#68A7BE")) +
  geom_jitter(aes(fill = Group), width = 0.2, size = 1.5, alpha = 1,shape = 21,color="#000000", stroke = 0.5) +  
  scale_color_manual(values = c("#EE7E77", "#68A7BE")) + 
  theme_minimal() +  
  ylim(0.74,0.96)+
  labs(x = "", y = "Simpson") +
  theme(
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text = element_text(size = 8,color="black"),
    axis.ticks = element_line(size = 0.5),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(1, 0.1, 1, 0.1, 'cm'),
    legend.position = "none") 
p3 <- ggplot(df, aes(x = Group, y = richness, fill = Group)) +
  geom_boxplot(width = 0.5,position = position_dodge(width = 0.7),alpha = 0.35) + 
  scale_fill_manual(values = c("#EE7E77", "#68A7BE")) +
  geom_jitter(aes(fill = Group), width = 0.2, size = 1.5, alpha = 1,shape = 21,color="#000000", stroke = 0.5) +  
  scale_color_manual(values = c("#EE7E77", "#68A7BE")) +  
  theme_minimal() +  
  ylim(60,85)+
  labs(x = "", y = "Richness") +
  theme(
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text = element_text(size = 8,color="black"),
    axis.ticks = element_line(size = 0.5),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(1, 1, 1, 0.1, 'cm'),
    legend.position = "none")

p<-p1+p2+p3
p

## Beta diversity
data <- read.table("pcoa_result.txt", header = TRUE, sep = "\t", row.names = 1)
head(data)
p <- ggplot(data, aes(x = PCoA1, y = PCoA2, fill = group)) +
  geom_point(aes(shape = group), size = 2, stroke = 0.5, color = "#000000") + 
  labs(x = "PCoA1(26%)", y = "PCoA2(14.7%)") +
  scale_fill_manual(values = c("#EE7E77", "#68A7BE")) +
  scale_shape_manual(values = c(21, 24)) + 
  ylim(-0.25, 0.3) +
  xlim(-0.4, 0.3) +
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y = element_text(size = 8, color="black"),
        axis.text.x = element_text(size = 8, color="black"),
        axis.ticks = element_line(size = 0.5),
        legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 8),  
        panel.grid.minor = element_blank(),  
        plot.margin = margin(1, 2, 1, 1, 'cm'),  
        panel.border = element_rect(color = "black", fill = NA, size = 1),  
        panel.background = element_rect(color = 'black', fill = 'transparent'),  
        panel.grid.major = element_line(color = "gray", linetype = "dotted")) +  
  stat_ellipse(aes(color = group), geom = "path", level = 0.9,   
               linetype = 2, size = 0.8, show.legend = TRUE) +  
  scale_color_manual(values = c("#EE7E77", "#68A7BE")) +  
  ggtitle("PERMANOVA: R2=0.02, P-value=0.042")
p


## LEfse analysis
setwd("/Users/liupeihao/Desktop")
install.packages("tidyverse")
install.packages("microeco")
install.packages("magrittr")
install.packages("tidytree")
install.packages("ggsignif")
library(ggsignif)
library(tidyverse)
library(microeco)
library(magrittr)
library(tidytree)

install.packages("remotes")
remotes::install_github("YuLab-SMU/ggtree")
library(ggtree)

otu<-read.csv("Otu.csv", row.names = 1)
group<-read.csv("Group.csv", row.names = 1)
tax<-read.csv("taxonomy.csv", row.names = 1)
dataset <- microtable$new(sample_table = group,otu_table = otu,tax_table = tax)
dataset
lefse <- trans_diff$new(dataset = dataset, method = "lefse", group = "Group", alpha = 0.05, lefse_subgroup = NULL)
head(lefse$res_diff)
write.csv(lefse$res_diff, file = "lefse_analysis_results.csv", row.names = FALSE)

lefse$plot_diff_bar(use_number = 1:10, width = 0.4, group_order = c("HB", "LB")) 
lefse$plot_diff_abund(use_number = 1:10,group_order = c("HB", "LB"),add_sig = T)  
otu_table <- dataset$otu_table
sample_table <- dataset$sample_table
head(otu_table)
head(sample_table)
hb_samples <- rownames(sample_table[sample_table$Group == "HB", ])
lb_samples <- rownames(sample_table[sample_table$Group == "LB", ])
hb_abundance <- rowMeans(otu_table[, colnames(otu_table) %in% hb_samples], na.rm = TRUE)
lb_abundance <- rowMeans(otu_table[, colnames(otu_table) %in% lb_samples], na.rm = TRUE)
result <- data.frame(OTU_ID = rownames(otu_table),HB_Avg_Abundance = hb_abundance,LB_Avg_Abundance = lb_abundance)
write.csv(result, file = "abundance_comparison.csv", row.names = FALSE)

## results visualization
library(ggplot2)
data<-read.csv("LEfseresults.csv")
head(data)
data$Taxa <- factor(data$Taxa, levels = unique(data$Taxa))

p1 <- ggplot(data, aes(y = Taxa, x = LDA, fill = Group)) + 
     geom_bar(stat = 'identity', color = 'black', width = 0.5) +    
     scale_fill_manual(values = c("#EE7E77", "#68A7BE")) +
     scale_y_discrete(expand = c(0.15, 0.15)) +
     labs(x = 'LDA Score', y = '') +  
     scale_x_continuous(limits = c(0, 4), expand = c(0.02, 0.02), breaks = seq(0, 4, 1)) +
     theme(axis.title = element_text(size = 8, colour = 'black'), 
           axis.text.x = element_text(size = 6, colour = 'black'), 
           axis.text.y = element_text(size = 8, colour = 'black'), 
           axis.line = element_line( colour = 'black'),
           panel.background = element_rect(fill = "white"), 
           panel.grid.major.y = element_blank(), 
           panel.grid.minor.y = element_blank(), 
           panel.grid.minor.x = element_blank(),
           legend.text = element_text(face = "bold", size = 8, colour = 'black'),  
           legend.title = element_blank(),  
           legend.key.size = unit(0.5, "cm"),
           legend.position = "top",  
           plot.margin = margin(0, 1, 1, 1, 'cm'))
p1


## ## Stamp difference analysis at Genus level
setwd("/Users/liupeihao/Desktop")
install.packages("ggtern")
install.packages("reshape2")
install.packages("tidyverse")
library(ggplot2)
library(ggtern)
library(reshape2)
library(tidyverse)

data <- read.table("genusGUT.txt", header = TRUE, row.names = 1)
group <- read.delim('Group.txt', sep = '\t', stringsAsFactors = FALSE)
data  =  data %>% filter(apply(data,1,mean) > 1)
data = t(data)
data1 <- cbind(data, group)
data1$group = as.factor(data1$group)
data_subset <- data1 %>%
  filter(group %in% c("HB", "LB")) %>%
  select(group, all_of(names(data1)[!names(data1) %in% "Group"])) %>%
  select(group, everything()) 
dfg = data_subset %>% select_if(is.numeric) %>%map_df(~ broom::tidy(t.test(. ~ group,data = data_subset)), .id = 'var')
dfg$p.value = p.adjust(dfg$p.value,"none")
dfg = dfg %>% filter(p.value < 0.05)

abun.bar = data_subset[,c(dfg$var,"group")] %>% 
  gather(variable,value,-group) %>% 
  group_by(variable,group) %>% 
  summarise(Mean = mean(value), .groups = 'keep')
diff.mean <- dfg[,c("var","estimate","conf.low","conf.high","p.value")]
diff.mean$group <- c(ifelse(diff.mean$estimate >0,levels(data1$group)[1],levels(data1$group)[2]))
diff.mean <- diff.mean[order(diff.mean$estimate,decreasing = TRUE),]

## results visualization
abun.bar$variable  =  factor(abun.bar$variable,levels = rev(diff.mean$var))
col <- c("#EE7E77", "#68A7BE")
p1 = ggplot(abun.bar,aes(variable,Mean,fill = group)) +
  scale_fill_manual(values=col)+
  scale_x_discrete(limits = levels(diff.mean$var)) +
  coord_flip() +
  xlab("") +
  ylab("Mean proportion (%)") +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(),
        axis.ticks.length = unit(0.2,"lines"), 
        axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(colour='black', size=8),
        axis.text=element_text(colour='black',size=6),
        legend.title=element_blank(),
        legend.text=element_text(size=8,colour = "black",
                                 margin = margin(r = 20)),
        legend.position = c(-1,1),
        legend.direction = "horizontal",
        legend.key.width = unit(0.8,"cm"),
        legend.key.height = unit(0.5,"cm"))
for (i in 0:2) 
p1 <- p1 + annotate('rect', xmin = i+0.5, xmax = i+1.5, ymin = -Inf, ymax = Inf, 
                      fill = ifelse(i %% 2 == 0, '#DEEEED', '#FEECE7'))
p1 = p1+geom_bar(stat = "identity",position = "dodge",
                 width = 0.7,colour = "black")
p1

diff.mean$var = factor(diff.mean$var,levels = levels(abun.bar$variable))
diff.mean$p.value = as.numeric(diff.mean$p.value)
diff.mean$p.value = round(diff.mean$p.value,3)
diff.mean$p.value = as.character(diff.mean$p.value)
p2 = ggplot(diff.mean,aes(var,estimate,fill = group)) +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(),
        axis.ticks.length = unit(0.4,"lines"), 
        axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(colour='black', size=8),
        axis.text=element_text(colour='black',size=6),
        axis.text.y = element_blank(),
        legend.position = "none",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 11,colour = "black",hjust = 0.5)) +
  scale_x_discrete(limits = levels(diff.mean$var)) +
  coord_flip() +
  xlab("") +
  ylab("Difference in mean proportions (%)") +
  labs(title="95% confidence intervals") 
p2
for (i in 0:2) 
p2 <- p2 + annotate('rect', xmin = i+0.5, xmax = i+1.5, ymin = -Inf, ymax = Inf, 
                      fill = ifelse(i %% 2 == 0, '#DEEEED', '#FEECE7'))

p2 <- p2 +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(0.8), width = 0.3, linewidth = 0.8) +
  geom_point(shape = 21,size = 5) +
  scale_fill_manual(values=col) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'black')
p2 
   
p3 <- ggplot(diff.mean,aes(var,estimate,fill = group)) +
  geom_text(aes(y = 0,x = var),label = diff.mean$p.value,
            hjust = 0,fontface = "bold",inherit.aes = FALSE,size =4) +
  geom_text(aes(x = nrow(diff.mean)/2 +0.5,y = 0.85),label = "P-value (corrected)",
            srt = 90,size = 5) +
  coord_flip() +
  ylim(c(0,1)) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())
p3

library(patchwork)
p <- p1 + p2 + p3 + plot_layout(widths = c(4,6,2))
p


## Analysis of microbial relative content difference
setwd("/Users/liupeihao/Desktop")
library(ggplot2)

data <- read.csv('twomicrobiome.csv',header = T)
head(data)
colors <- c("#EE7E77",  "#68A7BE")

p1 <- ggplot(data, aes(x = Group, y = Blautia, color = Group)) +
  geom_boxplot(width = 0.6, size = 0.8) +  
  geom_jitter(width = 0.01) +
  scale_color_manual(values = colors) +  
  labs(x = '', y = 'Relative Abundance') +
  theme_classic() +
  ylim(0,6.5)+
  theme(
    axis.title = element_text(size = 11,color = 'black'),
    axis.text.x = element_text(size = 8, color = 'black'),
    axis.text.y = element_text(size = 8, color = 'black'),
    axis.ticks = element_line(size = 0.5),
    axis.line = element_line(size = 0.5, color = "black"),
    axis.ticks.length = unit(0.1, "cm"),
    legend.position = "none",
    plot.margin = margin(1, 0, 1, 1, 'cm'),
  ) +
  stat_compare_means(aes(group = Group), method = "wilcox.test", label = "p.signif",label.y = 6.01,label.x =1.35)
p2 <- p1 + geom_signif(annotations = "",y_position = 6,xmin = 1,xmax = 2,tip_length = 0.03,size = 0.5,color = "black")
p2

p3 <- ggplot(data, aes(x = Group, y = Christensenellaceae_R.7_group, color = Group)) +
  geom_boxplot(width = 0.6, size = 0.8) +  
  geom_jitter(width = 0.01) +
  scale_color_manual(values = colors) +  
  labs(x = 'Group', y = '') +
  ylim(0,6.5)+
  theme_classic() +
  theme(
    axis.title = element_text(size = 11,color = 'black'),
    axis.text.x = element_text(size = 8, color = 'black'),
    axis.text.y = element_text(size = 8, color = 'black'),
    axis.ticks = element_line(size = 0.5),
    axis.line = element_line(size = 0.5, color = "black"),
    axis.ticks.length = unit(0.1, "cm"),
    legend.position = "none",
    plot.margin = margin(1, 0, 1, 0, 'cm'),
  ) +
  stat_compare_means(aes(group = Group), method = "wilcox.test", label = "p.signif",label.y = 6.01,label.x =1.35)
p4 <- p3 + geom_signif(annotations = "",y_position = 6,xmin = 1,xmax = 2,tip_length = 0.03,size = 0.5,color = "black")
p4

p5 <- ggplot(data, aes(x = Group, y =  B.C, color = Group)) +
  geom_boxplot(width = 0.6, size = 0.8) +  
  geom_jitter(width = 0.01) +
  scale_color_manual(values = colors) +  
  labs(x = '', y = '') +
  ylim(0,15)+
  theme_classic() +
  theme(
    axis.title = element_text(size = 11,color = 'black'),
    axis.text.x = element_text(size = 8, color = 'black'),
    axis.text.y = element_text(size = 8, color = 'black'),
    axis.ticks = element_line(size = 0.5),
    axis.line = element_line(size = 0.5, color = "black"),
    axis.ticks.length = unit(0.1, "cm"),
    legend.position = "right",
    plot.margin = margin(1, 1, 1, 0, 'cm'), 
    legend.text = element_text(size = 8, colour = 'black'),  
    legend.title = element_text(size = 11, colour = 'black'),  
  ) +
  stat_compare_means(aes(group = Group), method = "wilcox.test", label = "p.signif",label.y = 13.9,label.x =1.35)
p6 <- p5 + geom_signif(annotations = "",y_position = 13.8,xmin = 1,xmax = 2,tip_length = 0.03,size = 0.5,color = "black")
p6

p<-p2+p4+p6
p


## Relationship between microorganisms and butyric acid content
setwd("/Users/liupeihao/Desktop")
install.packages("cowplot")
install.packages("ggpubr")
install.packages("gridExtra")
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra) 

data<-read.csv("correlation.csv")
head(data)

p1 <- ggplot(data, aes(x = butyrate, y = Christensenellaceae_R.7_group)) +
      geom_point(size = 1.5, color = "#777777") +  
      geom_smooth(method = "lm", color = "#EE7E77", fill = "#FFD0DC", size = 1, se = TRUE) +
      stat_cor(method = "kendall", label.x = 0.5, label.y = 4.5, label.sep = "\n") +
      labs(x = "Butyric acid", y = str_wrap("RChristensenellaceae_R7_group", width = 10)) + 
      theme(
        plot.background=element_blank(),  
        panel.grid=element_blank(),  
        panel.background=element_blank(),  
        panel.border=element_rect(color="black",linewidth=1,fill=NA),  
        axis.line=element_blank(),  
        axis.ticks=element_line(color="black",linewidth=0.5),  
        axis.text=element_text(color="black",size=8),  
        axis.title=element_text(color="black",size=11),  
        plot.title=element_text(color="black",size=9),  
        legend.background=element_blank(),  
        legend.key=element_blank(),    
        legend.position = "none",  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))+
      scale_x_continuous(limits = c(0, 1)) +  
      scale_y_continuous(limits = c(0, 5))
p1 <- ggMarginal(p1, type = "histogram", margins = "y", yparams = list(fill = "#EE7E77", color = "black"))  
p1

p2 <- ggplot(data, aes(x = butyrate, y = Blautia)) +
      geom_point(size = 1.5, color = "#777777") +  
      geom_smooth(method = "lm", color = "#68A7BE", fill = "#DEEEED", size = 1, se = TRUE) +  
      stat_cor(method = "kendall", label.x = 0.5, label.y = 4.5, label.sep = "\n") +
      xlab("Butyric acid") + ylab("Blautia") + 
      theme(
        plot.background=element_blank(),  
        panel.grid=element_blank(),  
        panel.background=element_blank(),  
        panel.border=element_rect(color="black",linewidth=1,fill=NA),  
        axis.line=element_blank(),  
        axis.ticks=element_line(color="black",linewidth=0.5),  
        axis.text=element_text(color="black",size=8),  
        axis.title=element_text(color="black",size=11),  
        plot.title=element_text(color="black",size=9),  
        legend.background=element_blank(),  
        legend.key=element_blank(),    
        legend.position = "none",  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))+
      scale_x_continuous(limits = c(0, 1)) +  
      scale_y_continuous(limits = c(0, 6))
p2 <- ggMarginal(p2, type = "histogram", margins = "y", yparams = list(fill = "#68A7BE", color = "black"))  
p2

p3 <- ggplot(data, aes(x = butyrate, y = BC)) +
      geom_point(size = 1.5, color = "#777777") +  
      geom_smooth(method = "lm", color = "#CF6E0C", fill = "#FFCC99", size = 1, se = TRUE) + 
      stat_cor(method = "kendall", label.x = 0.5, label.y = 4.5, label.sep = "\n") +
      xlab("Butyric acid") + ylab("B/C") + 
      theme(
        plot.background=element_blank(),  
        panel.grid=element_blank(),  
        panel.background=element_blank(),  
        panel.border=element_rect(color="black",linewidth=1,fill=NA),  
        axis.line=element_blank(),  
        axis.ticks=element_line(color="black",linewidth=0.5),  
        axis.text=element_text(color="black",size=8),  
        axis.title=element_text(color="black",size=11),  
        plot.title=element_text(color="black",size=9),  
        legend.background=element_blank(),  
        legend.key=element_blank(),    
        legend.position = "none",  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))+
      scale_x_continuous(limits = c(0, 1)) +  
      scale_y_continuous(limits = c(0, 13), breaks = seq(0, 12, 3))
p3 <- ggMarginal(p3, type = "histogram", margins = "y", yparams = list(fill = "#CF6E0C", color = "black"))  
p3


## ROC curve of microorganisms
setwd("/Users/liupeihao/Desktop")
install.packages("pROC")
library(pROC)
aSAH <-read.csv("correlation.csv",sep=",")
head(aSAH)

plot.roc(aSAH$Group,aSAH$Christensenellaceae_R.7_group,legacy.axes=TRUE,col="#EE7E77", lwd=2,print.auc=TRUE,print.auc.y=0.4,print.auc.x=0.4, lty = 1)
plot.roc(aSAH$Group,aSAH$Blautia, legacy.axes=TRUE,col="#68A7BE", lwd=2, print.auc=TRUE, add=TRUE, print.auc.y=0.35,print.auc.x=0.4, lty = 1)
plot.roc(aSAH$Group,aSAH$BC, legacy.axes=TRUE,col="#CF6E0C", lwd=2, print.auc=TRUE, add=TRUE, print.auc.y=0.3,print.auc.x=0.4, lty = 1)
legend("bottomright", legend=c("Christensenellaceae_R.7_group", "Blautia","B/C"),col=c("#EE7E77", "#68A7BE","#CF6E0C"), lwd=2)



## Visualization of cell experiment results
### CCK8 results
setwd("/Users/liupeihao/Desktop")

library(ggpubr)
library(cowplot)
library(agricolae)
library(ggplot2)

data <- read.csv("CCK8.csv", header = T)
head(data)
data$Group  <- as.factor(data$Group)
nom <- bartlett.test(data$relative~data$Group,data = data)
nom
# p-value < 2.2e-16

oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
# 6.416e-10 

out <-LSD.test(oneway,"data$Group",p.adj = "none")
out
mar <- out$groups
rownamemar <- row.names(mar)
newmar <- data.frame(rownamemar, mar$`data$relative`, mar$groups)
sort <- newmar[order(newmar$rownamemar),]
rowname <- row.names(out$means)
mean <- out$means[,1]
sd <- out$means[,2]

marker <- sort$mar.groups
plotdata <- data.frame(rowname, mean, sd, marker)
plotdata

plotdata$rowname <- factor(plotdata$rowname, levels = c("Group1", "Group2", "Group3", "Group4", "Group5", "Group6"))

p1 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(stat = "identity", position = "dodge", color = "Black", aes(fill =rowname), width = 0.6, size = 0.6,alpha = 0.8) + 
    geom_jitter(data = data, aes(x = Group, y = relative, fill = Group),shape = 21, size = 2, position = position_jitter(width = 0.25, height = 0), show.legend = FALSE) +     
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) + 
     scale_fill_manual(values = c( "#FEDFB2","#DEEEED","#68A7BE","#FBD9D9","#EE7E77"),name = NULL) +  
    geom_text(aes(x = rowname, y = mean + sd + 8, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0)) + 
    expand_limits(y = 110) +
    labs(x = "", y = "Cell Viability(%)") + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11,  color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.line = element_line(size = 0.6, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
p1


## elisa results
setwd("/Users/liupeihao/Desktop")

library(ggpubr)
library(cowplot)
library(agricolae)
library(ggplot2)

## CAT(CAT(μmoL/min/mg prot))
data <- read.csv("CAT.csv", header = T)
head(data)

data$Group  <- as.factor(data$Group)
nom <- bartlett.test(data$relative~data$Group,data = data)
nom
# p-value = 0.01187

oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
# 4.169e-10 ***

out <-LSD.test(oneway,"data$Group",p.adj = "none")
out
mar <- out$groups
rownamemar <- row.names(mar)
newmar <- data.frame(rownamemar, mar$`data$relative`, mar$groups)
sort <- newmar[order(newmar$rownamemar),]
rowname <- row.names(out$means)
mean <- out$means[,1]
sd <- out$means[,2]
marker <- sort$mar.groups

plotdata <- data.frame(rowname, mean, sd, marker)
plotdata
plotdata$rowname <- factor(plotdata$rowname, levels = c("1", "2", "3", "4", "5", "6"))


p1 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(stat = "identity", position = "dodge", color = "Black", aes(fill =rowname), width = 0.6, size = 0.6,alpha = 0.8) + 
    geom_jitter(data = data, aes(x = Group, y = relative, fill = Group),shape = 21, size = 2, position = position_jitter(width = 0.25, height = 0), show.legend = FALSE) +     
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) + 
    scale_fill_manual(values = c( "#FEDFB2","#DEEEED","#68A7BE","#FBD9D9","#EE7E77"),name = NULL) +  
    geom_text(aes(x = rowname, y = mean + sd + 20, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0), limits = c(0, 410)) + 
    labs(x = "", y = "CAT(μmoL/min/mg prot)") + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11,  color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.line = element_line(size = 0.6, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
p1


## GPx(GPx(nmol/min/mg prot))
data <- read.csv("GPx.csv", header = T)
head(data)

data$Group  <- as.factor(data$Group)
nom <- bartlett.test(data$relative~data$Group,data = data)
nom
# p-value = 0.2211

oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
# 2.2e-16 ***

out <-LSD.test(oneway,"data$Group",p.adj = "none")
out
mar <- out$groups
rownamemar <- row.names(mar)
newmar <- data.frame(rownamemar, mar$`data$relative`, mar$groups)
sort <- newmar[order(newmar$rownamemar),]
rowname <- row.names(out$means)
mean <- out$means[,1]
sd <- out$means[,2]
marker <- sort$mar.groups

plotdata <- data.frame(rowname, mean, sd, marker)
plotdata
plotdata$rowname <- factor(plotdata$rowname, levels = c("1", "2", "3", "4", "5", "6"))
p1 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(stat = "identity", position = "dodge", color = "Black", aes(fill =rowname), width = 0.6, size = 0.6,alpha = 0.8) + 
    geom_jitter(data = data, aes(x = Group, y = relative, fill = Group),shape = 21, size = 2, position = position_jitter(width = 0.25, height = 0), show.legend = FALSE) +     
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) + 
    scale_fill_manual(values = c( "#FEDFB2","#DEEEED","#68A7BE","#FBD9D9","#EE7E77"),name = NULL) + 
    geom_text(aes(x = rowname, y = mean + sd + 30, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0), limits = c(0, 610)) + 
    labs(x = "", y = "GPx(nmol/min/mg prot)") + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11,  color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.line = element_line(size = 0.6, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
p1


## MDA(MDA(nmol/mL))
data <- read.csv("MDA.csv", header = T)
head(data)

data$Group  <- as.factor(data$Group)
nom <- bartlett.test(data$relative~data$Group,data = data)
nom
# p-value = 0.669

oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
# 4.933e-15 ***

out <-LSD.test(oneway,"data$Group",p.adj = "none")
out
mar <- out$groups
rownamemar <- row.names(mar)
newmar <- data.frame(rownamemar, mar$`data$relative`, mar$groups)
sort <- newmar[order(newmar$rownamemar),]
rowname <- row.names(out$means)
mean <- out$means[,1]
sd <- out$means[,2]
marker <- sort$mar.groups
plotdata <- data.frame(rowname, mean, sd, marker)
plotdata

plotdata$rowname <- factor(plotdata$rowname, levels = c("1", "2", "3", "4", "5", "6"))

p1 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(stat = "identity", position = "dodge", color = "Black", aes(fill =rowname), width = 0.6, size = 0.6,alpha = 0.8) + 
    geom_jitter(data = data, aes(x = Group, y = relative, fill = Group),shape = 21, size = 2, position = position_jitter(width = 0.25, height = 0), show.legend = FALSE) +     
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) + 
    scale_fill_manual(values = c( "#FEDFB2","#DEEEED","#68A7BE","#FBD9D9","#EE7E77"),name = NULL) + 
    geom_text(aes(x = rowname, y = mean + sd + 0.3, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0), limits = c(0, 4.1)) + 
    labs(x = "", y = "MDA(nmol/mL)") + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11,  color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.line = element_line(size = 0.6, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
p1


## SOD(SOD(U/mg prot))
data <- read.csv("SOD.csv", header = T)
head(data)

data$Group  <- as.factor(data$Group)
nom <- bartlett.test(data$relative~data$Group,data = data)
nom
# p-value = 0.004403

oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
# 3.544e-06 ***

out <-LSD.test(oneway,"data$Group",p.adj = "none")
out
mar <- out$groups
rownamemar <- row.names(mar)
newmar <- data.frame(rownamemar, mar$`data$relative`, mar$groups)
sort <- newmar[order(newmar$rownamemar),]
rowname <- row.names(out$means)
mean <- out$means[,1]
sd <- out$means[,2]
marker <- sort$mar.groups
plotdata <- data.frame(rowname, mean, sd, marker)
plotdata

plotdata$rowname <- factor(plotdata$rowname, levels = c("1", "2", "3", "4", "5", "6"))
p1 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(stat = "identity", position = "dodge", color = "Black", aes(fill =rowname), width = 0.6, size = 0.6,alpha = 0.8) + 
    geom_jitter(data = data, aes(x = Group, y = relative, fill = Group),shape = 21, size = 2, position = position_jitter(width = 0.25, height = 0), show.legend = FALSE) +     
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) + 
    scale_fill_manual(values = c( "#FEDFB2","#DEEEED","#68A7BE","#FBD9D9","#EE7E77"),name = NULL) +  
    geom_text(aes(x = rowname, y = mean + sd + 5, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0), limits = c(0, 82)) + 
    labs(x = "", y = "SOD(U/mg prot)") + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11,  color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.line = element_line(size = 0.6, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
p1


##  western blot results
setwd("/Users/liupeihao/Desktop")

library(ggpubr)
library(cowplot)
library(agricolae)
library(ggplot2)

## NRF2
data <- read.csv("NRF2.csv", header = T)
head(data)
data$Group  <- as.factor(data$Group)

nom <- bartlett.test(data$relative~data$Group,data = data)
nom
# p-value < 2.2e-16

oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
# 3.063e-07

out <-LSD.test(oneway,"data$Group",p.adj = "none")
out
mar <- out$groups
rownamemar <- row.names(mar)
newmar <- data.frame(rownamemar, mar$`data$relative`, mar$groups)
sort <- newmar[order(newmar$rownamemar),]
rowname <- row.names(out$means)
mean <- out$means[,1]
sd <- out$means[,2]
marker <- sort$mar.groups
plotdata <- data.frame(rowname, mean, sd, marker)
plotdata

plotdata$rowname <- factor(plotdata$rowname, levels = c("Group1", "Group2", "Group3", "Group4", "Group5", "Group6"))
p1 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(stat = "identity", position = "dodge", color = "Black", aes(fill =rowname), width = 0.6, size = 0.6,alpha = 0.8) + 
    geom_jitter(data = data, aes(x = Group, y = relative, fill = Group),shape = 21, size = 2, position = position_jitter(width = 0.25, height = 0), show.legend = FALSE) +     
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) + 
    scale_fill_manual(values = c( "#FEDFB2","#DEEEED","#68A7BE","#FBD9D9","#EE7E77"),name = NULL) +  
    geom_text(aes(x = rowname, y = mean + sd + 0.15, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0)) + 
    expand_limits(y = 2.1) +
    labs(x = "", y = "Relative Protein Activity") + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11,  color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.line = element_line(size = 0.6, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
p1


## NQO1
data <- read.csv("NQO1.csv", header = T)
head(data)
data$Group  <- as.factor(data$Group)

nom <- bartlett.test(data$relative~data$Group,data = data)
nom
# p-value < 2.2e-16

oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
# 0.009599

out <-LSD.test(oneway,"data$Group",p.adj = "none")
out
mar <- out$groups
rownamemar <- row.names(mar)
newmar <- data.frame(rownamemar, mar$`data$relative`, mar$groups)
sort <- newmar[order(newmar$rownamemar),]
rowname <- row.names(out$means)
mean <- out$means[,1]
sd <- out$means[,2]
marker <- sort$mar.groups
plotdata <- data.frame(rowname, mean, sd, marker)
plotdata


plotdata$rowname <- factor(plotdata$rowname, levels = c("Group1", "Group2", "Group3", "Group4", "Group5", "Group6"))
p2 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(stat = "identity", position = "dodge", color = "Black", aes(fill =rowname), width = 0.6, size = 0.6,alpha = 0.8) + 
    geom_jitter(data = data, aes(x = Group, y = relative, fill = Group),shape = 21, size = 2, position = position_jitter(width = 0.25, height = 0), show.legend = FALSE) +     
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) + 
    scale_fill_manual(values = c( "#FEDFB2","#DEEEED","#68A7BE","#FBD9D9","#EE7E77"),name = NULL) +  
    geom_text(aes(x = rowname, y = mean + sd + 0.15, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0)) + 
    expand_limits(y = 3.1) +
    labs(x = "", y = "Relative Protein Activity") + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11,  color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.line = element_line(size = 0.6, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
p2


## HO-1
data <- read.csv("HO-1.csv", header = T)
head(data)
data$Group  <- as.factor(data$Group)

nom <- bartlett.test(data$relative~data$Group,data = data)
nom
# p-value < 2.2e-16

oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
# 1.45e-05

out <-LSD.test(oneway,"data$Group",p.adj = "none")
out
mar <- out$groups
rownamemar <- row.names(mar)
newmar <- data.frame(rownamemar, mar$`data$relative`, mar$groups)
sort <- newmar[order(newmar$rownamemar),]
rowname <- row.names(out$means)
mean <- out$means[,1]
sd <- out$means[,2]
marker <- sort$mar.groups
plotdata <- data.frame(rowname, mean, sd, marker)
plotdata

plotdata$rowname <- factor(plotdata$rowname, levels = c("Group1", "Group2", "Group3", "Group4", "Group5", "Group6"))
p3 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(stat = "identity", position = "dodge", color = "Black", aes(fill =rowname), width = 0.6, size = 0.6,alpha = 0.8) + 
    geom_jitter(data = data, aes(x = Group, y = relative, fill = Group),shape = 21, size = 2, position = position_jitter(width = 0.25, height = 0), show.legend = FALSE) +     
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) + 
    scale_fill_manual(values = c( "#FEDFB2","#DEEEED","#68A7BE","#FBD9D9","#EE7E77"),name = NULL) +  
    geom_text(aes(x = rowname, y = mean + sd + 0.15, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0)) + 
    expand_limits(y = 3.1) +
    labs(x = "", y = "Relative Protein Activity") + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11,  color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.line = element_line(size = 0.6, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
p1
