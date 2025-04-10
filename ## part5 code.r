## part6 code

## phenotype description
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

data <- read.csv('meta1384.csv',header = T)
head(data)
data$Group <- as.factor(data$Group)
p1 <- ggplot(data, aes(x = Group, y = butyricacid)) +  
  geom_boxplot(aes(color = Group), width = 0.55, size = 0.8) +  
  geom_jitter(aes(color = Group), width = 0.01) +  
  scale_color_manual(values = c("#68A7BE", "#FFCBB5", "#FF9991", "#EE7E77")) +   
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "#631EA2") +  
  labs(x = 'Group', y = 'Intensity') +  
  ylim(0, 15) +  
  theme_classic() +  
  theme(  
    axis.title = element_text(size = 12, color = "black"),  
    axis.text.x = element_text(size = 9, color = "black"),  
    axis.text.y = element_text(size = 9, color = "black"),  
    axis.ticks = element_line(),  
    axis.line = element_line(color = "black"),  
    axis.ticks.length = unit(0.1, "cm"),  
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),  
    legend.position = "none") +    
  stat_compare_means(data = data, comparisons = list(c("C", "T1"), c("C", "T2"), c("C", "T3")),
                     method = "t.test", 
                     aes(group = Group),   
                     label = "p.signif", size = 4,     
                     label.y = c(11, 12.5, 14))   
p1  


data$newGroup <- as.factor(data$newGroup)
p2 <- ggplot(data, aes(x = newGroup, y = butyricacid)) +  
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
p2


## Manhattan map and QQ plot
awk '{print $1 "\t" $3 "\t" $12}' meta1384.txt > huatumeta1384.txt 

setwd("/Users/liupeihao/Desktop")
install.packages("CMplot")
library(CMplot)
data=read.table("meta1384huatu_hanghao.txt",header = T)
head(data)
p1 <- CMplot(data,plot.type = "m",
             col=c("#009bb8", "#003e91"),ylim = c(0, 10),
             threshold = c(0.05/590285,1/590285),
             threshold.col=c('grey','black'),
             threshold.lty = c(1,2),threshold.lwd = c(1,1), amplify = T,
             signal.cex = c(1,1), signal.pch = c(20,20),
             file = "pdf", dpi = 600, file.output = TRUE,verbose = TRUE, width = 9, height = 3)
p1
p2<-CMplot(data, plot.type = "q",box = FALSE,col="#009bb8",
           conf.int.col = "#003e91",threshold.col = "red",threshold.lty = 2,
           width = 5, height = 5,
           file = "pdf", dpi = 300, file.output = TRUE,verbose = TRUE)
p2

## Manhattan map by using ggplot2
library(tidyverse) 
install.packages("ggprism")
library(ggprism) 
library(ggtext)
library(ggrepel)
gwas <- read_tsv("huatumeta1384.txt", col_names = TRUE)
gwag_result <- gwas %>% group_by(chr) %>% 
  summarise(chr_len=max(ps)) %>% 
  mutate(tot=cumsum(chr_len)-chr_len) %>%
  select(-chr_len) %>%
  left_join(gwas,.,by=c("chr"="chr")) %>%
  arrange(chr,ps) %>%
  mutate(Chromosome=ps+tot)
axis <- gwag_result %>%
  group_by(chr) %>%
  summarize(center=(max(Chromosome) + min(Chromosome))/2)
desired_chromosomes <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 16, 20, 24, 28)  
p1 <- ggplot(gwag_result, aes(x = Chromosome, y = -log(p_wald, base = 10))) +  
  geom_point(aes(color = as.factor(chr)), size = 1.5) +  
  geom_hline(yintercept =  7.07,color="grey",linetype=2)+
  geom_hline(yintercept =  5.77,color="black",linetype=2)+
  scale_color_manual(values = rep(c("#009bb8", "#003e91"), 28)) +  
  scale_x_continuous(  
    breaks = axis$center, 
    labels = ifelse(axis$chr %in% desired_chromosomes, axis$chr, ""), 
    expand = c(0.01, 0.01)  
  ) +  
  scale_y_continuous(limits = c(0, 10), expand = c(0.01, 0)) +  
  labs(y = "-log<sub>10</sub>(p-value)") +  
  coord_cartesian(clip = "off") +  
  theme_prism(base_fontface = "plain", base_line_size = 0.5) +  
  theme(  
    legend.position = "none",  
    axis.text = element_text(color = "black", size = 12),  
    axis.title.y = element_markdown(size = 14, color = "black"),  
    axis.title.x = element_text(size = 16, color = "black"),
    axis.ticks.length = unit(0.1, "cm"))  
ggsave("Butyricacid.pdf", plot = p1, width = 10, height = 3)  


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

## result visualization
setwd("/Users/liupeihao/Desktop")
library(ggplot2)
library(grid) 
library(scales)

## GWAS and FINEMAP
data <- read.csv("Chr3GWAS.csv", header = TRUE)  
head(data)  
data$fill_color <- ifelse(data$Pos == 106476774, '#D1287A',   
                          ifelse(-log10(data$p) < 5.771061747, '#A5B9C6', '#1932AD'))  
p1 <- ggplot(data, aes(x = Pos / 1000, y = -log10(p), fill = fill_color)) +  
  geom_point(shape = 21, size = 2.5, color = "#121415", stroke = 0) +  
  scale_fill_identity() +   
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(106000, 110000)) +  
  scale_y_continuous(expand = c(0, 0), limits = c(-4, 10),   
                     breaks = seq(0, 10, by = 5),   
                     labels = number_format(accuracy = 0.1)) +     
  coord_cartesian(xlim = c(106000, 110000), ylim = c(-4.5, 10.5)) +   
  theme(axis.text.x = element_text(color = 'black', size = 8),  
        axis.text.y = element_text(color = 'black', size = 8),  
        axis.title.x = element_text(color = 'black', size = 10),  
        axis.title.y = element_text(color = 'black', size = 10, hjust = 1),  
        axis.line = element_line(color = 'black'),   
        axis.ticks = element_line(color = 'black'),   
        axis.ticks.length = unit(0.1, "cm"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.border = element_blank(),   
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(1, 1, 1, 1, 'cm')) +  
  labs(x = 'Chr.3(Kbp)', y = '-lg(p-value)')  
p1  
data <- read.csv("Chr3Finemap.csv", header = TRUE)  
head(data)     
data$Group <- ifelse(data$Pos == 106476774, "Group1", "Group2")  
p2 <- ggplot(data, aes(x = Pos / 1000, y = p,,fill = Group)) +  
  geom_point(shape = 21,size = 2.5, color = "#121415", stroke = 0,show.legend = FALSE) + 
  scale_fill_manual(values = c('#D1287A', '#1932AD')) +  
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(106000, 110000), position = "top") +  
  scale_y_reverse(expand = c(0, 0), limits = c(1, 0), breaks = c(0.5, 1)) +  
  coord_cartesian(xlim = c(106000, 110000), ylim = c(1, 0)) +   
  theme(axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(),  
        axis.title.x = element_blank(),   
        axis.text.y = element_text(color = '#0B278E', size = 8),  
        axis.title.y = element_text(color = '#0B278E', size = 10, hjust = 0),  
        axis.ticks.y = element_line(color = 'black'),  
        axis.line.x = element_line(color = '#898580', linetype = "dashed"),  
        axis.line.y = element_blank(),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.border = element_blank(),   
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(0, 0, 0, 0, 'cm')) +  
  labs(y = 'PIP') +  
  geom_text(data = subset(data, Pos == 106476774),aes(label = "rs318007359"),color = '#D1287A',vjust = 0,hjust = -0.1,size = 3)  
p2 
p<-p1 + annotation_custom(grob = ggplotGrob(p2), xmin = 105550, xmax = 110040,  ymin = -4, ymax = 0.32)
p

## FINEMAP in GWAS
p1 <- ggplot(data, aes(x = Pos / 1000, y = -log10(p), fill = fill_color)) +  
  geom_point(shape = 21, size = 2.5, color = "#121415", stroke = 0) +  
  scale_fill_identity() +   
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(106000, 110000)) +  
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10),   
                     breaks = seq(0, 10, by = 2),   
                     labels = number_format(accuracy = 0.1)) +     
  coord_cartesian(xlim = c(106000, 110000), ylim = c(-0.5, 10.5)) +   
  theme(axis.text.x = element_text(color = 'black', size = 8),  
        axis.text.y = element_text(color = 'black', size = 8),  
        axis.title.x = element_text(color = 'black', size = 11),  
        axis.title.y = element_text(color = 'black', size = 11, hjust = 0.5),  
        axis.line = element_line(color = 'black'),   
        axis.ticks = element_line(color = 'black'),   
        axis.ticks.length = unit(0.1, "cm"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.border = element_blank(),   
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(1, 1, 1, 1, 'cm')) + 
  xlab("Chr.3(Kbp)") +   
  ylab(expression(-log[10](p-value))) +  
  geom_text(data = subset(data, Pos == 106476774),aes(label = "PIP=0.93"),color = '#D1287A',vjust = -1, hjust = 1,size = 3) +
  geom_hline(yintercept = 5.77, color = "black", linetype = 2)
p1  



## Dual luciferase
setwd("/Users/liupeihao/Desktop")
library(ggpubr)
library(cowplot)
library(agricolae)
library(ggplot2)
library(stringr)

data <- read.csv("MSRA.csv", header = T)
head(data)
data$Group  <- as.factor(data$Group)
nom <- bartlett.test(data$relative~data$Group,data = data)
nom
# result:p-value = 0.0001313
oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
# result: 2.2e-16 ***
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

plotdata$rowname <- factor(plotdata$rowname, levels = c("4.18/TK", "MP/TK", "MSA/TK", "MST/TK", "MP+MSA/TK", "MP+MST/TK"))
p1 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(stat = "identity", position = "dodge", color = "Black", fill = "#4496CC", width = 0.6, size = 0.5) +  
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.5) +  
    geom_text(aes(x = rowname, y = mean + sd + 0.7, label = marker), size = 5, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0)) + 
    expand_limits(y = 8.2) +
    labs(x = "", y = str_wrap("Relative Luciferase Activity", width = 20)) + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11, color = 'black'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.ticks = element_line(),
        axis.line = element_line(color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        plot.margin = margin(1, 0.5, 3, 2, 'cm'),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
p1


## MSRA gene expression with different genotype
setwd("/Users/liupeihao/Desktop")
library(ggplot2)
library(ggpubr)

data <- read.csv('newMSRA-1.csv',header = T)
head(data)
colors <- c("#EE7E77", "#FEDFB2","#68A7BE")
p1 <- ggplot(data, aes(x = Group, y = FPKM)) +  
  geom_boxplot(width = 0.5, size = 0.5, outlier.shape = NA) +  
  geom_jitter(aes(fill = Tissue), size=2, width = 0.01,alpha=0.9,shape=21,color="black") + 
  scale_fill_manual(values = colors) + 
  labs(x = '', y = 'FPKM of MSRA') +  
  theme_classic() +  
  ylim(8, 21) +  
  theme(  
    axis.title = element_text(size = 10,color = 'black'),  
    axis.text.x = element_text(size = 9, color = 'black'),  
    axis.text.y = element_text(size = 8, color = 'black'),  
    axis.ticks.length = unit(0.1, "cm"),  
    axis.ticks = element_line(size = 0.5),  
    axis.line = element_line(size = 0.5, color = "black"),
    plot.margin = margin(1, 1, 1, 1, 'cm'), 
    legend.text = element_text(size = 8, colour = 'black'),  
    legend.title =element_text(size = 10, colour = 'black'),  
    legend.position = "right") +  
  geom_signif(  
    comparisons = list(c("A/A", "A/T"), c("A/T", "T/T"), c("A/A", "T/T")),  
    map_signif_level = TRUE,   
    test = wilcox.test,   
    y_position = c(18.5, 19, 20),  
    size = 0.5, color = "black", textsize = 3, family = "Arial Black")
p1


## different genotype count
setwd("/Users/liupeihao/Desktop")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("ggprism")
install.packages("plyr")
library (reshape2)
library(ggplot2)
library(ggprism)
library(plyr)

phylum1=read.table('3_106476774.csv',header=T,comment.char='',sep=',')
head(phylum1)

phylum_colors <- c("A/A" = "#EE7E77","A/T" = "#FEDFB2","T/T" = "#68A7BE")

phylum_order <- c("A/A", "A/T", "T/T")
phylum_order <- rev(phylum_order)
phylum1$Genotype <- factor(phylum1$Genotype, levels = phylum_order)
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
                  legend.text=element_text(color="black",size=10),
                  legend.title=element_text(color="black",size=10))


p1 <- ggplot(phylum1, aes(x = group, y = 100 * value, fill = Genotype)) +
  geom_bar(stat = "identity", color = "white", size = 0.4, position = "stack", width = 0.6) +
  scale_fill_manual(values = phylum_colors) + 
  plot.format+
  theme(plot.margin = margin(1, 1, 1, 1, 'cm')) +
  xlab("Group") + 
  ylab("Percentage(%)") + 
  scale_x_discrete(breaks = unique(phylum1$group), labels = unique(phylum1$group)) +
  scale_y_continuous(name = "Percentage(%)", breaks = seq(0, 100, by = 20), limits = c(-0.1, 102))
p1


## Butyric acid intensity of idfferent genotype
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

df<-read.csv("Butyricacid.csv")
head(df)

p1 <- ggplot(df, aes(x = X3_106476774, y = Butyricacid,fill =X3_106476774)) +
  stat_boxplot(geom="errorbar",position = position_dodge(width = 0.2),width=0.1) + 
  geom_boxplot(position = position_dodge(width = 0.2),width=0.55,size=0.5,outlier.size = 1) + 
  scale_fill_manual(values = c("#EE7E77","#FEDFB2", "#68A7BE")) + 
  theme_classic() + 
  ylim(8,12.5)+
  labs(x = "", y = "Butyric acid") +
  theme(
      axis.title = element_text(size = 11,color = 'black'),  
    axis.text.x = element_text(size = 10, color = 'black'),  
    axis.text.y = element_text(size = 8, color = 'black'),  
    axis.ticks.length = unit(0.1, "cm"),  
    axis.ticks = element_line(),  
    axis.line = element_line(color = "black"),
    plot.margin = margin(1, 1, 1, 1, 'cm'),  
    legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0))+
geom_signif(
    comparisons = list(c("A/A", "A/T"), c("A/T", "T/T"), c("A/A", "T/T")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = c(11.5, 11.7, 12),
    size = 1, color = "black", textsize = 4,  family = "Arial Black",fontface = "bold")
p1


## correlation between butyricacid and MSRA
setwd("/Users/liupeihao/Desktop")
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra) 

data<-read.csv("newMSRA.csv")
head(data)

spearman_test <- cor.test(data$meta1384, data$FPKM, method = "pearson")
spearman_test
# cor：0.3049353  ，p-value = 0.02957

colors <- c("#EE7E77", "#FEDFB2","#68A7BE")
p1 <- ggplot(data, aes(x = meta1384, y = FPKM, fill = Tissue)) +
  geom_point(shape = 21, size = 2) +
  scale_fill_manual(values = colors) + 
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, fill="#E0DDD5",color = "#898580") + 
  xlab("Intensity of Butyric acid") + ylab("FPKM of MSRA") + 
  theme(
    axis.title = element_text(size = 10, colour = 'black'),
    axis.text = element_text(size = 8, colour = 'black'), 
    axis.line = element_line(size = 0.5, colour = 'black'),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(8.5, 11.5)) +  
  scale_y_continuous(limits = c(8.5, 18.5))+
  guides(color = guide_legend(ncol = 1),  
         fill = guide_legend(ncol = 1)) +
  labs(color = "black", fill = "Tissue")
p1


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