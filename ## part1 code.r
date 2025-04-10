## part1 code

## GO mortalty description
setwd("/Users/liupeihao/Desktop")
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
install.packages("ggsignif")
library(ggsignif)
data1<-read.csv("G10Mortality.CSV",header = T) 
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
data<-read.table("G10Dam.txt",header=T)
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
data<-read.table("G10Sire.txt",header=T)
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
data1<-read.csv("G11Mortality.CSV",header = T) 
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
data<-read.table("RowG11Mortality.txt",header=T)
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
data<-read.table("AdjustG11Mortality.txt",header=T)
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
data1<-read.csv("G12Mortality.CSV",header = T) 
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



## Analysis of association between cytokines and mortality
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



## Growth performance description
## Body weigt
setwd("/Users/liupeihao/Desktop")
install.packages("ggbeeswarm")
install.packages("ggplot2")
library(ggplot2)
library(ggbeeswarm)
install.packages("Hmisc")
library(Hmisc)
install.packages("ggpubr")
library(ggpubr)
library(patchwork) 
mydata <-read.csv("newBW.csv",sep=",") 
head(mydata)

p1 <- ggplot(mydata, aes(x = Group, y = BW28day/1000, color = Group)) +
  scale_color_manual(values = c("#EE7E77", "#68A7BE")) +
  geom_beeswarm(aes(shape = Group), cex = 1.2, size = 1.2, priority = "descending") +
  theme_classic() +
  labs(x = NULL, y = 'Body Weight (Kg)') +
  theme(axis.title = element_text(size = 11, color = 'black'),
        axis.text.x = element_text(size = 8,color = 'black'),
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.line = element_line(),
        plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm'),
        axis.ticks = element_line(size = 0.5),
        legend.position = 'none') +
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median, geom = 'crossbar', width = 0.3, size = 0.5, color = 'black') +
  stat_summary(fun.data = function(x) median_hilow(x, 0.5), geom = 'errorbar', width = 0.15, size = 0.8, color = 'black') +
  coord_cartesian(ylim = c(1.04, 3.55))
p1
my_comparisons <- list(c("H", "L"))
p2 <- p1 + stat_compare_means(comparisons=my_comparisons,
                     label.y = c(3.25),
                     method="wilcox.test",label = "p.signif",size = 4)
p2




p3 <- ggplot(mydata, aes(x = Group, y = BW35day/1000, color = Group)) +  
  scale_color_manual(values = c("#EE7E77", "#68A7BE")) +  
  geom_beeswarm(aes(shape = Group), cex = 1.2, size = 1.2, priority = "descending") +  
  theme_classic() +  
  labs(x = NULL, y = '') +  
  theme(axis.title.x = element_text(size = 11, color = 'black'),  
        axis.text.x = element_text(size = 8,color = 'black'),  
        axis.line = element_line(),  
        axis.ticks = element_line(size = 0.5),  
        legend.position = 'none',  
        plot.margin = margin(0.5, 0, 0.5, -0.4, 'cm'),
        axis.line.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        axis.text.y = element_blank()) +  
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median, geom = 'crossbar', width = 0.3, size = 0.5, color = 'black') +  
  stat_summary(fun.data = function(x) median_hilow(x, 0.5), geom = 'errorbar', width = 0.15, size = 0.8, color = 'black') +  
  coord_cartesian(ylim = c(1.04, 3.55))  
p3
my_comparisons <- list(c("H", "L"))
p4 <- p3 + stat_compare_means(comparisons=my_comparisons,
                     label.y = c(3.25),
                     method="wilcox.test",label = "p.signif",size = 4)
p4

p5 <- ggplot(mydata, aes(x = Group, y = BW42day/1000, color = Group)) +
  scale_color_manual(values = c("#EE7E77", "#68A7BE")) +
  geom_beeswarm(aes(shape = Group), cex = 1.2, size = 1.2, priority = "descending") +
  theme_classic() +
  labs(x = NULL, y = '') +
  theme(axis.title.x = element_text(size = 11, color = 'black'),  
        axis.text.x = element_text(size = 8,color = 'black'),
        axis.line = element_line(),  
        axis.ticks = element_line(size = 0.5),  
        legend.position = 'right',  
        axis.line.y = element_blank(),
        plot.margin = margin(0.5, 0.3, 0.5, -0.4, 'cm'),     
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        legend.title = element_text(size = 8)) +
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median, geom = 'crossbar', width = 0.3, size = 0.5, color = 'black') +
  stat_summary(fun.data = function(x) median_hilow(x, 0.5), geom = 'errorbar', width = 0.15, size = 0.8, color = 'black') +
  coord_cartesian(ylim = c(1.04, 3.55))
p5
my_comparisons <- list(c("H", "L"))
p6 <- p5 + stat_compare_means(comparisons=my_comparisons,
                     label.y = c(3.25),
                     method="wilcox.test",label = "p.signif",size = 4)
p6

p <- p2+p4+p6 
p


## Weight Gain
## Body weigt
mydata <-read.csv("newBW.csv",sep=",") 
head(mydata)

p1 <- ggplot(mydata, aes(x = Group, y = WG1, color = Group)) +
  scale_color_manual(values = c("#EE7E77", "#68A7BE")) +
  geom_beeswarm(aes(shape = Group), cex = 1.2, size = 1.2, priority = "descending") +
  theme_classic() +
  labs(x = NULL, y = 'Gain Weight (g)') +
  theme(axis.title = element_text(size = 11, color = 'black'),
        axis.text.x = element_text(size = 8,color = 'black'),
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.line = element_line(),
        plot.margin = margin(0.5, 0, 0.5, 0.5, 'cm'),
        axis.ticks = element_line(size = 0.5),
        legend.position = 'none') +
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median, geom = 'crossbar', width = 0.3, size = 0.5, color = 'black') +
  stat_summary(fun.data = function(x) median_hilow(x, 0.5), geom = 'errorbar', width = 0.15, size = 0.8, color = 'black') +
  coord_cartesian(ylim = c(100, 1080))+  
  scale_y_continuous(breaks = c(100, 350, 600, 850, 1100)) 
p1
my_comparisons <- list(c("H", "L"))
p2 <- p1 + stat_compare_means(comparisons=my_comparisons,
                     label.y = c(980),
                     method="wilcox.test",label = "p.signif",size = 4)
p2

p3 <- ggplot(mydata, aes(x = Group, y = WG2, color = Group)) +
  scale_color_manual(values = c("#EE7E77", "#68A7BE")) +
  geom_beeswarm(aes(shape = Group), cex = 1.2, size = 1.2, priority = "descending") +
  theme_classic() +
  labs(x = NULL, y = '') +
  theme(axis.title.x = element_text(size = 11, color = 'black'),  
        axis.text.x = element_text(size = 8,color = 'black'),
        axis.line = element_line(),  
        axis.ticks = element_line(size = 0.5),  
        legend.position = 'right',  
        axis.line.y = element_blank(),
        plot.margin = margin(0.5, 0.3, 0.5, -0.4, 'cm'),     
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        legend.title = element_text(size = 8)) +
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median, geom = 'crossbar', width = 0.3, size = 0.5, color = 'black') +
  stat_summary(fun.data = function(x) median_hilow(x, 0.5), geom = 'errorbar', width = 0.15, size = 0.8, color = 'black') +
  coord_cartesian(ylim = c(100, 1080))
p3
my_comparisons <- list(c("H", "L"))
p4 <- p3 + stat_compare_means(comparisons=my_comparisons,
                     label.y = c(980),
                     mmethod="wilcox.test",label = "p.signif",size = 4)
p4

p <- p2+p4 
p



## Population Structure Analysis
## PCA
## Computational PCA
/home/cgbl/biosoft/plink1.9/plink --chr-set 39 --allow-extra-chr --bfile new_phliu_allsample --pca 50 --threads 50 --out newsnp.pca3 --make-founders

## R language for visualization
setwd("/Users/liupeihao/Desktop")
install.packages("scatterplot3d")

library(scatterplot3d)
pdf(file = "PCA.pdf")
data <- read.table("new2PCA.txt", header = TRUE, sep = "\t")
par(mar = c(3.1, 3.1, 3.1, 6.1), xpd = TRUE)
attach(data)
mycolour <- c(rep("#73B79B",80), rep("#42B6C5",312))
scatterplot3d(PC1, PC2, PC3, color = mycolour, pch = 20, main = "F&M PCA", cex.symbols = 1.2, font.lab = 2, font.axis = 2, xpd = TRUE, mar = c(4.1, 3.1, 3.1, 6.1))
legend("right", legend = unique(data$Group), fill = c("#73B79B","#42B6C5"), title = "Group")
dev.off()

pdf(file = "newPCA.pdf")
data <- read.table("new2PCA.txt", header = TRUE, sep = "\t")
par(mar = c(3.1, 3.1, 3.1, 6.1), xpd = TRUE)
attach(data)
mycolour <- c(rep("#73B79B",80), rep("#42B6C5",312))
scatterplot3d(PC1, PC2, PC3, color = mycolour, pch = 20, main = "H&L PCA", cex.symbols = 1.2, font.lab = 2, font.axis = 2, xpd = TRUE, mar = c(4.1, 3.1, 3.1, 6.1))
legend("right", legend = unique(data$Group), fill = c("#73B79B","#42B6C5"), title = "Group")
dev.off()

## LD decay
## PopLDdecay
git clone https://github.com/hewm2008/PopLDdecay.git 
cd PopLDdecay; chmod 755 configure; ./configure;
make;
mv PopLDdecay  bin/;    #     [rm *.o]
/home/phliu/LD/PopLDdecay/bin/PopLDdecay

## HL Group                                                                                                                                                                                            
/home/phliu/LD/PopLDdecay/bin/PopLDdecay -InVCF allsample.vcf -OutStat H.stat.gz -SubPop H.txt
/home/phliu/LD/PopLDdecay/bin/PopLDdecay -InVCF allsample.vcf -OutStat L.stat.gz -SubPop L.txt
/home/phliu/LD/PopLDdecay/bin/PopLDdecay -InVCF allsample.vcf -OutStat C.stat.gz -SubPop C.txt
ls *stat.gz
cat multi.list
perl /home/phliu/LD/PopLDdecay/bin/Plot_MultiPop.pl -inList newmulti.list --output re2 -bin1 10 -bin2 500

## FM Group
/home/phliu/LD/PopLDdecay/bin/PopLDdecay -InVCF allsample.vcf -OutStat M.stat.gz -SubPop F.txt
/home/phliu/LD/PopLDdecay/bin/PopLDdecay -InVCF allsample.vcf -OutStat F.stat.gz -SubPop M.txt
ls *stat.gz
cat multi.list
perl /home/phliu/LD/PopLDdecay/bin/Plot_MultiPop.pl -inList newmultiold.list --output oldre2 -bin1 10 -bin2 500






## Metabolism Description
setwd("/Users/liupeihao/Desktop")
library(ggplot2)  
library(dplyr)  
library(scales)  

## Super Class 
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

## Other
data <- read.csv("Other.csv")  
head(data)  
data <- data %>%  mutate(remaining = 87 - Number,end = cumsum(Number + remaining),start = end - Number,start_remaining = end - remaining)  
data$Super.Class <- factor(data$Super.Class, levels = unique(data$Super.Class))   
p1 <- ggplot(data, aes(y = Super.Class)) +  
  geom_tile(aes(x = 43.5, width = 87, height = 0.8), color="#000000", linewidth=0.4, fill = "#EEEEEE") +  
  geom_rect(aes(xmin = 0, xmax = Number, ymin = as.numeric(factor(Super.Class)) - 0.4, ymax = as.numeric(factor(Super.Class)) + 0.4), fill = "#EB8677", color = "#000000", linewidth=0.4) +  
  geom_text(aes(x = Number/2, label = Number), size = 3, color = "black") +  
  scale_x_continuous(limits = c(0, 87)) +  
  labs(x = "Number", y = "Other Class") +  
  theme_classic() +  
  theme(axis.title = element_text(size = 11, color = 'black'),  
        axis.text.x = element_text(size = 8,color = 'black'),  
        axis.text.y = element_text(size = 8, color = 'black'),  
        axis.line = element_line(),  
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),  
        axis.ticks = element_line(size = 0.5),  
        legend.position = 'none')  
p1  


## #OPLS-DA（Orthogonal Partial Least Squares Discriminant Analysis）
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


## Differential Accumlation of Metabolites
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
## Super Class and VIPof DAMs
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
  labs(x = "Super Class", y = "CV") +  
  scale_x_discrete(labels = function(x) {  
    color_values <- colors[x]  
    paste0("<span style='font-size:21pt; color:", color_values, "; border: 1px solid black;'>&#9679;</span>")}) 
p1

p2 <- ggplot(data = mydata, aes(x = SuperClass, y = VIP, fill = SuperClass)) +  
  geom_violin() +  
  scale_fill_manual(values = colors) + 
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4), limits = c(0, 4))+  
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

p <- p1+p2
p


## Comparison of CV between H and L groups
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


## CV distribution of nine Super Class
setwd("/Users/liupeihao/Desktop")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggsignif")
library(ggplot2)
library(ggpubr)
library(ggsignif)

df<-read.csv("MetaCV.CSV")  
head(df)   
mydata<-read.csv("CVandVIP.csv",sep=",")  
desired_order <- c("Lipids and lipid-like molecules","Organic acids and derivatives","Organoheterocyclic compounds","Organic oxygen compounds",  
                   "Benzenoids","Phenylpropanoids and polyketides","Other","Unclassified","Nucleosides, nucleotides, and analogues")  
desired_order <- desired_order[desired_order %in% mydata$SuperClass]  
colors <- c("Benzenoids" = "#CCEBC5","Lipids and lipid-like molecules" = "#9CD1C7","Nucleosides, nucleotides, and analogues" = "#F4CFE4",  
            "Organic acids and derivatives" = "#BEBAD8","Organic oxygen compounds" = "#BCDD78", "Organoheterocyclic compounds" = "#8AB0D0", "Phenylpropanoids and polyketides" = "#FFFFBB",  
            "Other" = "#EB8677","Unclassified" = "#F3B76F")  
df$SuperClass <- factor(df$SuperClass, levels = desired_order)  
available_colors <- colors[names(colors) %in% levels(df$SuperClass)]  

p1 <- ggplot(df, aes(x = SuperClass, y = CV, fill = Group)) +  
  geom_boxplot(width = 0.5,position = position_dodge(width = 0.7), outlier.size = 0.5) +  
  scale_fill_manual(values = c("#EE7E77", "#68A7BE")) + 
  theme_minimal() +  
  ylim(0,2)+  
  labs(x = "Super Class", y = "CV") +  
  theme(  
    axis.title.x = element_text(size = 11),  
    axis.title.y = element_text(size = 11),  
    axis.text = element_text(size = 8, color="black"),  
    axis.text.x = ggtext::element_markdown(size = 8, color="black"),
    axis.ticks = element_line(color = "black", size = 0.5),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    panel.background = element_rect(fill = "white"),  
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm')) +  
  scale_x_discrete(labels = function(x) {  
    color_values <- colors[x]  
   paste0("<span style='font-size:22pt; color:", color_values, "; border: 1px solid black;'>&#9679;</span>")})  
p1  


## pie chart of VIP and CV
setwd("/Users/liupeihao/Desktop")
install.packages("tidyverse")
library(tidyverse)

phe<-read.csv("VIP.csv")
head(phe)
p1<-ggplot(phe, aes(x = "", y = n, fill = Color)) + 
      geom_col(color = "black") + 
      geom_bar(stat = "identity", width = 1) +
      geom_text(aes(label = paste0(prop, "%")), position = position_stack(vjust = 0.5), size = 5, 
            family = "Arial") + 
      scale_fill_manual(values = c( "#EE7E77","#68A7BE")) + 
      coord_polar("y", start = 0)  + 
      theme_void() +
      theme(text = element_text(size = 16),legend.position = "bottom")
p1

phe<-read.csv("CV.csv")
head(phe)
p2<-ggplot(phe, aes(x = "", y = n, fill = Color)) + 
      geom_col(color = "black") + 
      geom_bar(stat = "identity", width = 1) +
      geom_text(aes(label = paste0(prop, "%")), position = position_stack(vjust = 0.5), size = 5, 
            family = "Arial") + 
      scale_fill_manual(values = c( "#EE7E77","#68A7BE")) + 
      coord_polar("y", start = 0)  + 
      theme_void() +
      theme(text = element_text(size = 16),legend.position = "bottom")
p2


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




## Visualization of enrichment analysis results
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