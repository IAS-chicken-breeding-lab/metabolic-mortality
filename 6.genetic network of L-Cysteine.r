## part6 code

## phenotype description
## G1：Comparison of H and L groups
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
library(ggplot2)
library(cowplot) 
install.packages("ggpubr")
library(ggpubr)
install.packages("ggsignif")
library(ggsignif)

data <- read.csv('meta1374.csv',header = T)
head(data)
data$newGroup <- as.factor(data$newGroup)
p1 <- ggplot(data, aes(x = Group, y = newmeta1374)) +
  geom_boxplot(aes(color = Group), width = 0.55, size = 0.8) +  
  geom_jitter(aes(color = Group), width = 0.01) +  
  scale_color_manual(values = c( "#68A7BE","#FFCBB5","#FF9991","#EE7E77")) + 
  stat_summary(fun = mean, geom = "point",shape = 18,  size = 3, color = "#631EA2") +
  labs(x = 'Group', y = 'Intensity') +
  ylim(12.5, 14.5) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 9,color = "black"),
    axis.text.x = element_text(size = 6,color = "black"),
    axis.text.y = element_text(size = 6,color = "black"),
    axis.ticks = element_line(),
    axis.line = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
    legend.position = "none")+    
  stat_compare_means(data = data, comparisons = list(c("C", "T1"), c("T1", "T2"), c("T2", "T3")),
                     method = "t.test", 
                     aes(group = Group),   
                     label = "p.signif", size = 4,     
                     label.y = c(14.5, 15, 15.5))   
p1  
 

data$newGroup <- as.factor(data$newGroup)
p2 <- ggplot(data, aes(x = newGroup, y = newmeta1374)) +  
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
p2

## Manhattan map and QQ plot
awk '{print $1 "\t" $3 "\t" $12}' meta1374.txt > huatumeta1374.txt 

install.packages("CMplot")
library(CMplot)
data=read.table("meta1374huatu_hanghao.txt",header = T)
head(data)
p1 <- CMplot(data,plot.type = "m",
             col=c("#DA9599", "#E9C6C6"),ylim = c(0, 20),
             threshold = c(0.05/590285,1/590285),
             threshold.col=c('grey','black'),
             threshold.lty = c(1,2),threshold.lwd = c(1,1), amplify = T,
             signal.cex = c(1,1), signal.pch = c(20,20),
             file = "pdf", dpi = 600, file.output = TRUE,verbose = TRUE, width = 9, height = 3)
p1
p2<-CMplot(data, plot.type = "q",box = FALSE,col="#F06060",
           conf.int.col = "#FFC085",threshold.col = "red",threshold.lty = 2,
           width = 5, height = 5,
           file = "pdf", dpi = 300, file.output = TRUE,verbose = TRUE)
p2

## Manhattan map by using ggplot2
library(tidyverse) 
install.packages("ggprism")
library(ggprism) 
library(ggtext)
library(ggrepel)
gwas <- read_tsv("huatumeta1374.txt", col_names = TRUE)
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
  scale_color_manual(values = rep(c("#DA9599", "#E9C6C6"), 28)) +  
  scale_x_continuous(  
    breaks = axis$center, 
    labels = ifelse(axis$chr %in% desired_chromosomes, axis$chr, ""), 
    expand = c(0.01, 0.01)  
  ) +  
  scale_y_continuous(limits = c(0, 20), expand = c(0.015, 0)) +  
  labs(y = "-log<sub>10</sub>(p-value)") +  
  coord_cartesian(clip = "off") +  
  theme_prism(base_fontface = "plain", base_line_size = 0.5) +  
  theme(  
    legend.position = "none",  
    axis.text = element_text(color = "black", size = 12),  
    axis.title.y = element_markdown(size = 14, color = "black"),  
    axis.title.x = element_text(size = 16, color = "black"),
    axis.ticks.length = unit(0.1, "cm"))  
ggsave("L-cysteine.pdf", plot = p1, width = 10, height = 3)


## colco package was used for colocalization analysis
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

## Results visualization
library(ggplot2)
data<-read.csv("Jejunum.csv",header=T)
head(data)
p1 <- ggplot(data, aes(x = logpeQTL, y = logpQTL)) +
  geom_point(aes(fill = SNPPPH4), shape = 23, size = 3, color = "#121415", stroke = 0.5) +  
  scale_fill_gradient(low = "#326db6", high = "#9b3f5c", limits = c(0, 1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(6.75, 11)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 20)) + 
  coord_cartesian(xlim = c(6.75, 11.2), ylim = c(-1, 21)) + 
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black',size = 9),
        axis.title.x = element_text(color = 'black', size = 12),
        axis.title.y = element_text(color = 'black', size = 12),
        axis.line = element_line(color = 'black'), 
        axis.ticks = element_line(color = 'black'), 
        axis.ticks.length = unit(0.1, "cm"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(0.5, 0.2, 0.5, 0.5, 'cm'),
        legend.position = "none",
        plot.title = element_text(size = 11, hjust = 0)) +
  labs(x = 'eQTL -lg(p-value)', y = 'GWAS -lg(p-value)')+
  ggtitle("Jejunum") 
p1

data<-read.csv("Cerebellum.csv",header=T)
head(data)
p2 <- ggplot(data, aes(x = logpeQTL, y = logpQTL)) +  
  geom_point(aes(fill = SNPPPH4), shape = 24, size = 2.5, color = "#121415", stroke = 0.5) +  
  scale_fill_gradient(low = "#326db6", high = "#9b3f5c", limits = c(0, 1)) +  
  scale_x_continuous(expand = c(0, 0), limits = c(4, 16)) +   
  scale_y_continuous(expand = c(0, 0), limits = c(-0.5, 8)) +   
  coord_cartesian(xlim = c(3.5, 16.5), ylim = c(-0.5, 8.2)) +   
  theme(axis.text.x = element_text(color = 'black', size = 9),  
        axis.text.y = element_text(color = 'black', size = 9),  
        axis.title.x = element_text(color = 'black', size = 12),  
        axis.title.y = element_blank(),  
        axis.line = element_line(color = 'black'),   
        axis.ticks = element_line(color = 'black'),   
        axis.ticks.length = unit(0.1, "cm"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.border = element_blank(),   
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(0.5, 0.5, 0.5, 0.2, 'cm'),  
        plot.title = element_text(size = 11, hjust = 0),  
        legend.key.size = unit(0.5, "cm"), 
        legend.text = element_text(size = 7),  
        legend.title = element_text(size = 9)) + 
  labs(x = 'eQTL -lg(p-value)', y = 'GWAS -lg(p-value)') +  
  ggtitle("Cerebellum")   

p<-p1+p2
p


## Finemapping
## Finemapping by FINEMAP
/home/cgbl/biosoft/plink1.9/plink --chr-set 35 --allow-extra-chr --bfile /home/phliu/mGWAS1-100/peihao1100 --extract site.txt --make-bed --out site
/home/cgbl/biosoft/plink1.9/plink --chr-set 35 --allow-extra-chr --bfile site --r square --out meta1374_1
awk '{gsub(/\t/, " "); print}' meta1374_1.ld > meta1374.ld
awk '{print $1,$1"_"$3,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12}' meta1374.txt > meta1374_1.txt
/home/cgbl/phliu/WGRGWAS/Finemap/finemap_v1.4_x86_64/finemap_v1.4_x86_64 --sss --in-files master --dataset 1

## Results visualization
library(ggplot2)
data<-read.csv("geneFinemap.csv",header=T)
head(data)
p1 <- ggplot(data, aes(x = X1000bp , y = Pip)) +
  geom_point(aes(fill = logp), shape = 21, size = 3, color = "#121415", stroke = 0.5) +  
  scale_fill_gradient(low = "#326db6", high = "#9b3f5c", name = "-lg(p-value)") +
  scale_x_continuous(expand = c(0, 0), limits = c(19450, 21370), breaks = seq(19500, 21320, by = 455)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-0.02, 0.22)) + 
  coord_cartesian(xlim = c(19450, 21370), ylim = c(-0.02, 0.22)) + 
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        axis.title.x = element_text(color = 'black',  size = 11),
        axis.title.y = element_text(color = 'black',size = 11),
        axis.line = element_line(color = 'black'), 
        axis.ticks = element_line(color = 'black'), 
        axis.ticks.length = unit(0.1, "cm"),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        plot.title = element_text(size = 10, hjust = 0),  
        legend.key.size = unit(0.5, "cm"), 
        legend.text = element_text(size = 7),  
        legend.title = element_text(size = 9)) +
  labs(x = 'Chr.2(Kbp)', y = 'Fine-mapping(PIP)')+  
  ggtitle("Post-Pr(causal SNPs is 2) = 0.68")   
p1


## gene and loci position
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("trackViewer")
library(trackViewer)
SNP <- c(20088535, 20130661, 20134466, 20652753)
seq1 <- GRanges("chr2", IRanges(SNP, width=1, names=c("rs313565219","chr2:20130661","rs732044877","rs316055999")))
seq1
features <- GRanges("chr2", IRanges(c(19999990, 20127432, 20644330),
                                    width=c(97500, 19835, 19738),
                                    names=c("RSU1", "PTER", "CROT")))
features
features$fill <- c('#B3643C', '#008281', '#F4D448')
features$height <- rep(0.08, 3)
features
seq1$color <- c('#0066CB', '#0066CB', '#8D2898','#8D2898')
seq1$border <-rep("#090909", length(SNP))
seq1$alpha <- rep(0.95, length(SNP))
xaxis <- c(19990000,20160000,20330000,20500000,20670000)
lolliplot(seq1, features,cex = 1,ylab = F,xaxis = xaxis,xaxis.gp = gpar(fontsize=12,lex=1.5))


## Chromatin status
library(ggplot2)  
library(dplyr)  
data <- read.csv('newPTER.csv', header = TRUE)   
head(data)   
data_rect <- data %>%  
  mutate(  start = as.numeric(start),end = as.numeric(end),width = as.numeric(width)) %>%  
  mutate(xmin = start,  xmax = start + width,  ymin = 0,  ymax = 0.001)  
tissue_order <- c("Cerebellum", "Cortex", "Hypothalamus", "Duodenum", "Jejunum", "Ileum", "Cecum", "Colon")
data_rect$tissue <- factor(data_rect$tissue, levels = tissue_order)
p1 <- ggplot(data_rect) +  
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill)) +  
    scale_fill_identity(name = "Chromatin State", breaks = c(
        "#BB322E" ,"#E0709D","#387E4D" ,"#4CA969","#8CC6A8","#FAE84D","#DAA743","#F8D584",
        "#FAE6A3", "#FBECC5","#86A2C4","#D57146","#726E6E","#B0ACAC","#E6E5E5"), 
        labels = c(
        "#BB322E" = "TssA",
        "#E0709D" = "TssAHet",
        "#387E4D" = "TxFlnK",
        "#4CA969" = "TxFlnkWk",
        "#8CC6A8" = "TxFlnkHet",
        "#FAE84D" = "EnhA",
        "#DAA743" = "EnhAMe",
        "#F8D584" = "EnhAWk",
        "#FAE6A3" = "EnhAHet",
        "#FBECC5" = "EnhPois",
        "#86A2C4" = "ATAC_Is",
        "#D57146" = "TssBiv",
        "#726E6E" = "Repr",
        "#B0ACAC" = "ReprWk",
        "#E6E5E5" = "Qui"
    ), guide = "legend") +  
    theme_void() +   
    coord_cartesian(ylim = c(0, 0.001001)) +
    scale_x_continuous(limits = c(20127432, 20147266)) +  
    theme(  
        plot.margin = margin(1, 1, 1, 1, 'cm'),  
        axis.text.x = element_text(size = 10, color = "black"),  
        axis.ticks.x = element_line(size = 0.5, color = "black"),  
        axis.ticks.length.x = unit(0.05, "cm"),
        axis.title.x = element_text(size = 12, color = "black"),  
        axis.line.x = element_line(color = "black", size = 0.5),  
        strip.text.y = element_text(size = 12, hjust = 1),
        legend.position = "right",  
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10)) +  
    labs(x = "Chr2(PTER)") +  
    facet_wrap(~ tissue, scales = "free_y", ncol = 1, strip.position = "left") +
    geom_vline(xintercept = c(20130661, 20134466), color = "#D1287A", linetype = "dashed", size = 0.7)
p1


## Dual luciferase
library(ggpubr)
library(cowplot)
library(agricolae)
library(ggplot2)
library(stringr)

data <- read.csv("PTER.csv", header = T)
head(data)
data$Group  <- as.factor(data$Group)
nom <- bartlett.test(data$relative~data$Group,data = data)
nom
# result:p-value = 4.975e-06
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

plotdata$rowname <- factor(plotdata$rowname, levels = c("4.18/TK", "PP/TK", "PSG/TK", "PSA/TK", "PP+PSG/TK", "PP+PSA/TK"))
p1 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(stat = "identity", position = "dodge", color = "Black", fill = "#FBD9D9", width = 0.6, size = 0.5) +  
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


## PTER gene expression with different genotype
data <- read.csv('newPTER-HS.csv',header = T)
head(data)
colors <- c("#EE7E77", "#68A7BE")
p1<- ggplot(data, aes(x = Group, y = FPKM)) +  
  geom_boxplot(width = 0.5,  outlier.shape = NA) +  
  geom_jitter(aes(fill = Tissue), size=2, width = 0.01,alpha=0.9,shape=21,color="black") + 
  scale_fill_manual(values = colors) + 
  labs(x = '', y = 'FPKM of PTER') +  
  theme_classic() +  
  ylim(4, 12) +  
  theme(  
    axis.title = element_text(size = 11,color = 'black'),  
    axis.text.x = element_text(size = 8, color = 'black'),  
    axis.text.y = element_text(size = 8, color = 'black'),  
    axis.ticks.length = unit(0.15, "cm"),  
    axis.ticks = element_line(),  
    axis.line = element_line(color = "black"),
    plot.margin = margin(1, 1, 1, 1, 'cm'),  
    legend.position = "right") 
p1


data <- read.csv('newPTER-L.csv',header = T)
head(data)
colors <- c( "#FEDFB2")
p3 <- ggplot(data, aes(x = Group, y = FPKM)) +  
  geom_boxplot(width = 0.5, outlier.shape = NA) +  
  geom_jitter(aes(fill = Tissue), size=2, width = 0.01,alpha=0.9,shape=21,color="black") + 
  scale_fill_manual(values = colors) + 
  labs(x = '', y = 'FPKM of PTER') +  
  theme_classic() +  
  ylim(10, 40) +  
  theme(  
    axis.title = element_text(size = 11,color = 'black'),  
    axis.text.x = element_text(size = 8, color = 'black'),  
    axis.text.y = element_text(size = 8, color = 'black'),  
    axis.ticks.length = unit(0.15, "cm"),  
    axis.ticks = element_line(),  
    axis.line = element_line(color = "black"),
    plot.margin = margin(1, 1, 1, 1, 'cm'),  
    legend.position = "none") +
    geom_signif(
    comparisons = list(c("A/G", "G/G")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = 35,
    size = 0.5, color = "black", textsize = 3,  family = "Arial Black")
p3 


## Intensity of L-cysteine with different genotype
/home/cgbl/biosoft/plink1.9/plink --allow-extra-chr --chr-set 35 --bfile peihao1100 --extract meta1374site.txt --make-bed --out CHR2
/home/cgbl/biosoft/plink1.9/plink  --chr-set 35 --allow-extra-chr  --bfile CHR2 --recode --out CHR2_1

## Results visualization
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggsignif")
library(ggplot2)
library(ggpubr)
library(ggsignif)

install.packages("patchwork") 
library(patchwork) 

df<-read.csv("lcysteine.csv")
head(df)

p1 <- ggplot(df, aes(x = X2_20088535, y =  Lcysteine,fill = X2_20088535)) +
  stat_boxplot(geom="errorbar",position = position_dodge(width = 0.2),width=0.1) + 
  geom_boxplot(position = position_dodge(width = 0.2),width=0.55,size=0.5,outlier.size = 1) + 
  scale_fill_manual(values = c("#EE7E77","#FEDFB2", "#68A7BE")) + 
  theme_classic() + 
  ylim(8,12)+
  labs(x = "", y = "L-Cysteine",title = "rs313565219") +
  theme(
      axis.title = element_text(size = 11,color = 'black'),  
    axis.text.x = element_text(size = 10, color = 'black'),  
    axis.text.y = element_text(size = 8, color = 'black'),  
    axis.ticks.length = unit(0.1, "cm"),  
    axis.ticks = element_line(),  
    axis.line = element_line(color = "black"),
    plot.margin = margin(1, 0, 1, 1, 'cm'),  
    legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0))+
geom_signif(
    comparisons = list(c("C/C", "T/C"), c("T/C", "T/T"), c("C/C", "T/T")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = c(11.35, 11.15, 11.72),
    size = 0.8, color = "black", textsize = 4,  family = "Arial Black",fontface = "bold")
p1
p2 <- ggplot(df, aes(x = X2_20130661, y =  Lcysteine,fill = X2_20130661)) +
  stat_boxplot(geom="errorbar",position = position_dodge(width = 0.2),width=0.1) + 
  geom_boxplot(position = position_dodge(width = 0.2),width=0.55,size=0.5,outlier.size = 1) + 
  scale_fill_manual(values = c("#EE7E77","#FEDFB2", "#68A7BE")) + 
  theme_classic() +  
  ylim(8,12)+
  labs(x = "", y = "",title = "chr2:20130661") +
  theme(
      axis.title = element_text(size = 11,color = 'black'),  
    axis.text.x = element_text(size = 10, color = 'black'),  
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    axis.line.y = element_blank(),  
    axis.ticks.length = unit(0.1, "cm"),  
    axis.ticks = element_line(),  
    axis.line = element_line(color = "black"),
    plot.margin = margin(1, 0, 1, 0, 'cm'),  
    legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0))+
geom_signif(
    comparisons = list(c("C/C", "T/C"), c("T/C", "T/T"), c("C/C", "T/T")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = c(11.18, 11.38, 11.72),
    size = 0.8, color = "black", textsize = 4,  family = "Arial Black",fontface = "bold")
p2
p3 <- ggplot(df, aes(x = X2_20134466, y = Lcysteine,fill = X2_20134466)) +
  stat_boxplot(geom="errorbar",position = position_dodge(width = 0.2),width=0.1) + 
  geom_boxplot(position = position_dodge(width = 0.2),width=0.55,size=0.5,outlier.size = 1) + 
  scale_fill_manual(values = c("#EE7E77","#FEDFB2", "#68A7BE")) + 
  theme_classic() +  
  ylim(8,12)+
   labs(x = "", y = "",title = "rs72044877") +
  theme(
    axis.title = element_text(size = 11,color = 'black'),  
    axis.text.x = element_text(size = 10, color = 'black'),  
    axis.text.y = element_text(size = 8, color = 'black'),  
    axis.ticks.length = unit(0.1, "cm"),  
    axis.ticks = element_line(),  
    axis.line = element_line(color = "black"),
    plot.margin = margin(1, 0, 1, 0, 'cm'),   
    legend.position = "none")+
geom_signif(
    comparisons = list(c("A/A", "A/G"), c("A/G", "G/G"), c("A/A", "G/G")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = c(11.22, 11.46, 11.82),
    size = 0.8, color = "black", textsize = 4,  family = "Arial Black",fontface = "bold")
p3
p4 <- ggplot(df, aes(x = X2_20652753, y = Lcysteine,fill = X2_20652753)) +
  stat_boxplot(geom="errorbar",position = position_dodge(width = 0.2),width=0.1) + 
  geom_boxplot(position = position_dodge(width = 0.2),width=0.55,size=0.5,outlier.size = 1) + 
  scale_fill_manual(values = c("#EE7E77","#FEDFB2", "#68A7BE")) + 
  theme_classic() + 
  ylim(8,12)+
  labs(x = "", y = "",title = "rs316055999") +
  theme(
   axis.title = element_text(size = 11,color = 'black'),  
    axis.text.x = element_text(size = 10, color = 'black'),  
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    axis.line.y = element_blank(),  
    axis.ticks.length = unit(0.1, "cm"),  
    axis.ticks = element_line(),  
    axis.line = element_line(color = "black"),
    plot.margin = margin(1, 1, 1, 0, 'cm'),  
    legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0))+
geom_signif(
    comparisons = list(c("A/A", "G/A"), c("G/A", "G/G"), c("A/A", "G/G")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = c(11.17, 11.37, 11.62),
    size = 0.8, color = "black", textsize = 4,  family = "Arial Black",fontface = "bold")
p4

p <- p1 + p2 + p3 + p4 + plot_layout(ncol = 4)
p


## different genotype count
install.packages("reshape2")
install.packages("ggplot2")
install.packages("ggprism")
install.packages("plyr")
library (reshape2)
library(ggplot2)
library(ggprism)
library(plyr)

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

phylum1=read.table('2_20134466.csv',header=T,comment.char='',sep=',')
head(phylum1)
phylum_colors <- c("A/A" = "#EE7E77", "A/G" =  "#FEDFB2","G/G" = "#68A7BE")
phylum_order <- c("G/G", "A/G","A/A")
phylum_order <- rev(phylum_order)
phylum1$Genotype <- factor(phylum1$Genotype, levels = phylum_order)
p1 <- ggplot(phylum1, aes(x = Group, y = 100 * value, fill = Genotype)) +
  geom_bar(stat = "identity", color = "white", size = 0.4, position = "stack", width = 0.6) +
  scale_fill_manual(values = phylum_colors) + 
  plot.format+
  theme(plot.margin = margin(1, 1, 1, 1, 'cm')) +
  xlab("Group") + 
  ylab("Percentage(%)") + 
  scale_x_discrete(breaks = unique(phylum1$Group), labels = unique(phylum1$Group)) +
  scale_y_continuous(name = "Percentage(%)", breaks = seq(0, 100, by = 20), limits = c(-0.1, 102))

p1


## candidate loci LD block
/home/cgbl/biosoft/plink1.9/plink --chr-set 35 --allow-extra-chr --bfile /home/phliu/mGWAS1-100/peihao1100 --extract Candidate4SNP.txt --make-bed --out Candidate4SNP
/home/cgbl/biosoft/plink1.9/plink --chr-set 35 --allow-extra-chr --bfile Candidate4SNP --r --out Candidate4SNP_1 
/home/cgbl/biosoft/plink1.9/plink --chr-set 35 --allow-extra-chr --bfile Candidate4SNP --r2 square --out Candidate4SNP_1_2 

## result visualization
install.packages("ggplot2")
install.packages("dplyr") 
library("ggplot2")
library("dplyr")
data <- read.table('Locild.csv', header = TRUE, comment.char = '', sep = ',')  
p <- ggplot(data = data, aes(x = x, y = y)) +  
  geom_tile(aes(fill = value)) +  
  coord_equal(clip = "off") +  
  geom_text(data = data.frame(x = 1:4, y = 0:3, label = data %>% pull(x) %>% unique()),  
            aes(x = x, y = y, label = label), angle = 90) +  
  theme_void() +  
  scale_fill_gradient2(low = scales::muted("#326db6"),   
                       mid = "white",  
                       high = scales::muted("#9b3f5c"),  
                       midpoint = 0) +  
  theme(legend.position = "bottom") +  
  annotate(geom = "text", x = 0.5, y = 0.2, label = "LD r square", size = 5, color = "black")
p


## Visualization of cysteine test results
library(ggpubr)
library(cowplot)
library(agricolae)
library(ggplot2)
data <- read.csv("CCK8.csv", header = T)
head(data)
data$Group  <- as.factor(data$Group)
nom <- bartlett.test(data$relative~data$Group,data = data)
nom
## results：p-value < 2.2e-16
oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
## results：6.416e-10 
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
plotdata$rowname <- factor(plotdata$rowname, levels = c("Group1", "Group2", "Group3", "Group4", "Group5"))
p1 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(stat = "identity", position = "dodge", color = "Black",   
             width = 0.6, size = 0.6) +  
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd),   
                  position = position_dodge(0.2), width = 0.3, size = 0.6) +  
    geom_text(aes(x = rowname, y = mean + sd + 8, label = marker),   
              size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0)) +   
    expand_limits(y = 110) +  
    labs(x = "", y = "Cell Viability(%)") +   
    theme_bw() +  
    theme(  
        axis.title = element_text(size = 11, color = 'black'),  
        axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(),   
        axis.text.y = element_text(size = 8, color = 'black'),  
        axis.ticks = element_line(size = 0.6),  
        axis.line = element_line(size = 0.6, color = "black"),  
        axis.ticks.length = unit(0.1, "cm"),  
        plot.margin = margin(1, 1, 1, 1, 'cm'),  
        legend.position = "right",  
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 8), 
        panel.border = element_blank(),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +  
    scale_fill_manual(values = c("#FEDFB2", "#DEEEED", "#68A7BE", "#FBD9D9", "#EE7E77"),   
                      labels = c("Control", "H2O2", "H2O2+ML385", "H2O2+IR-61", "H2O2+L-cys"),   
                      name = "")
p1 


## western blot
## NRF2
data <- read.csv("NRF2.csv", header = T)
head(data)
data$Group  <- as.factor(data$Group)
nom <- bartlett.test(data$relative~data$Group,data = data)
nom
## results：p-value < 2.2e-16
oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
## results：3.063e-07
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
    geom_bar(stat = "identity", position = "dodge", color = "Black", fill =  c("#FEDFB2","#DEEEED","#68A7BE","#FBD9D9","#EE7E77"), width = 0.6, size = 0.6) +  
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) +  
    geom_text(aes(x = rowname, y = mean + sd + 0.15, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0)) + 
    expand_limits(y = 2.1) +
    labs(x = "", y = str_wrap("Relative Protein Activity", width = 20)) + 
    theme_bw() +
    theme(
       axis.title = element_text(size = 11, color = 'black'),  
        axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(),   
        axis.text.y = element_text(size = 8, color = 'black'),  
        axis.ticks = element_line(size = 0.6),  
        axis.line = element_line(size = 0.6, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        plot.margin = margin(1, 0.5, 2, 1, 'cm'),
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
## results：p-value < 2.2e-16
oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
## results：0.009599
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
    geom_bar(stat = "identity", position = "dodge", color = "Black", fill = c("#FEDFB2","#DEEEED","#68A7BE","#FBD9D9","#EE7E77"), width = 0.6, size = 0.6) +  
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) +  
    geom_text(aes(x = rowname, y = mean + sd + 0.15, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0)) + 
    expand_limits(y = 3.1) +
    labs(x = "", y = "") + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11, color = 'black'),  
        axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(),   
        axis.text.y = element_text(size = 8, color = 'black'),  
        axis.ticks = element_line(size = 0.6),
        axis.line = element_line(size = 0.6, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        plot.margin = margin(1, 0.5, 2, 0.5, 'cm'),
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
## results：p-value < 2.2e-16
oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
## results：1.45e-05
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
    geom_bar(stat = "identity", position = "dodge", color = "Black",width = 0.6, size = 0.6) +  
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) +  
    geom_text(aes(x = rowname, y = mean + sd + 0.15, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0)) + 
    expand_limits(y = 3.1) +
    labs(x = "", y = "") + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11, color = 'black'),  
        axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(),   
        axis.text.y = element_text(size = 8, color = 'black'),  
        axis.ticks = element_line(size = 0.6),  
        axis.line = element_line(size = 0.6, color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        plot.margin = margin(1, 0.5, 2, 0.5, 'cm'),
        legend.position = "right",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 8))+  
    scale_fill_manual(values = c("#FEDFB2", "#DEEEED", "#68A7BE", "#FBD9D9", "#EE7E77"),   
                      labels = c("Control", "H2O2", "H2O2+ML385", "H2O2+IR-61", "H2O2+L-cys"),   
                      name = "")
p3

p <- p1 + p2 + p3 + plot_layout(ncol = 3)
p


## cytokines
## CAT(CAT(μmoL/min/mg prot))
data <- read.csv("CAT.csv", header = T)
head(data)
data$Group  <- as.factor(data$Group)
nom <- bartlett.test(data$relative~data$Group,data = data)
nom
## results：p-value = 0.01187
oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
## results：4.169e-10 ***
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
    geom_bar(stat = "identity", position = "dodge", color = "Black", fill = c("#FEDFB2","#DEEEED","#68A7BE","#FBD9D9","#EE7E77"), width = 0.6, size = 0.6) +  
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) +  
    geom_text(aes(x = rowname, y = mean + sd + 20, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0), limits = c(0, 410)) + 
    labs(x = "", y = "CAT(μmoL/min/mg prot)") + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11,  color = 'black'),
        axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(),   
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.ticks.y = element_line(size = 0.6),
        axis.line = element_line(size = 0.6, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        plot.margin = margin(1, 0.5, 1, 1, 'cm'),
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
## results：p-value = 0.2211
oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
## results：2.2e-16 ***
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
p2<- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(stat = "identity", position = "dodge", color = "Black", fill = c("#FEDFB2","#DEEEED","#68A7BE","#FBD9D9","#EE7E77"), width = 0.6, size = 0.6) +  
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) +  
    geom_text(aes(x = rowname, y = mean + sd + 30, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0), limits = c(0, 610)) + 
    labs(x = "", y = "GPx(nmol/min/mg prot)") + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11,  color = 'black'),
        axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(),   
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.ticks.y = element_line(size = 0.6),
        axis.line = element_line(size = 0.6, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        plot.margin = margin(1, 0.5, 1, 0.5, 'cm'),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
p2

## SOD(SOD(U/mg prot))
data <- read.csv("SOD.csv", header = T)
head(data)
data$Group  <- as.factor(data$Group)
nom <- bartlett.test(data$relative~data$Group,data = data)
nom
## results：p-value = 0.004403
oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
## results：3.544e-06 ***
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
p3 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(stat = "identity", position = "dodge", color = "Black", fill = c("#FEDFB2","#DEEEED","#68A7BE","#FBD9D9","#EE7E77"), width = 0.6, size = 0.6) +  
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) +  
    geom_text(aes(x = rowname, y = mean + sd + 5, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0), limits = c(0, 82)) + 
    labs(x = "", y = "SOD(U/mg prot)") + 
    theme_bw() +
    theme(
        axis.title = element_text(size = 11,  color = 'black'),
        axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(),   
        axis.text.y = element_text(size = 8, color = 'black'),
        axis.ticks.y = element_line(size = 0.6),
        axis.line = element_line(size = 0.6, color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        plot.margin = margin(1, 0.5, 1, 0.5, 'cm'),
        legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
p3

## MDA(MDA(nmol/mL))
data <- read.csv("MDA.csv", header = T)
head(data)
data$Group  <- as.factor(data$Group)
nom <- bartlett.test(data$relative~data$Group,data = data)
nom
## results：p-value = 0.669
oneway<-aov(data$relative~data$Group,data = data)
anova(oneway)
## results：4.933e-15 ***
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
p4 <- ggplot(plotdata, aes(x = rowname, y = mean, fill = rowname)) +  
    geom_bar(stat = "identity", position = "dodge", color = "Black", width = 0.6, size = 0.6) +  
    geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd), position = position_dodge(0.2), width = 0.3, size = 0.6) +  
    geom_text(aes(x = rowname, y = mean + sd + 0.3, label = marker), size = 4, position = position_dodge(1)) +  
    scale_y_continuous(expand = c(0, 0), limits = c(0, 4.1)) + 
    labs(x = "", y = "MDA(nmol/mL)") + 
    theme_bw() +
    theme(  
        axis.title = element_text(size = 11, color = 'black'),  
        axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(),   
        axis.text.y = element_text(size = 8, color = 'black'),  
        axis.ticks = element_line(size = 0.6),  
        axis.line = element_line(size = 0.6, color = "black"),  
        axis.ticks.length = unit(0.1, "cm"),  
        plot.margin = margin(1, 0.5, 1, 0.5, 'cm'),  
        legend.position = "right",  
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 8), 
        panel.border = element_blank(),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()  
    ) +  
    scale_fill_manual(values = c("#FEDFB2", "#DEEEED", "#68A7BE", "#FBD9D9", "#EE7E77"),   
                      labels = c("Control", "H2O2", "H2O2+ML385", "H2O2+IR-61", "H2O2+L-cys"),   
                      name = "")
p4

p <- p1 + p2 + p3 + p4 + plot_layout(ncol = 4)
p
