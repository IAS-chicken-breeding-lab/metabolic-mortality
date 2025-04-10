## part2 code
## WGS Summary
## MAF distribution
library(ggplot2)
data <- read.table("MAF.txt", header = TRUE, sep = "\t")

p1<- ggplot(data, aes(x = af)) + 
     geom_histogram(aes(y = after_stat(density)), binwidth = 0.02, fill = "#68a7be", colour = "white") + 
     geom_density(alpha = 0.5, fill = "#FEF0F0", size = 0.5) + 
     geom_vline(xintercept = median(data$af, na.rm = TRUE), color = "#D1287A", linetype = "dashed", linewidth = 0.7) +
     annotate("text", x = median(data$af, na.rm = TRUE) + 0.11, y=3.5, 
             label = paste("Median:", round(median(data$af, na.rm = TRUE), 2)), 
             color = "#D1287A", vjust = -1, size = 3, family = "Arial") +
     labs(x = "MAF", y = "Density") +
     ylim(0, 4) +
     xlim(0, 0.5) +
     theme(
    axis.title = element_text(size = 11, colour = 'black'),
    axis.text = element_text(size = 8, colour = 'black'), 
    axis.line = element_line(size = 0.5, colour = 'black'),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank())
p1


## Statistics of the number of loci within the LD block
cd /home/phliu/mGWAS1-100/
/home/cgbl/biosoft/plink1.9/plink --bfile peihao1100 --chr-set 29 --blocks no-pheno-req --out peihaoallsample

import pandas as pd
file_path = 'peihaoallsample.blocks.det'
data = pd.read_csv(file_path, delim_whitespace=True)
data['LD_Length'] = data['BP2'] - data['BP1']
data['SNP_Count'] = data['SNPS'].apply(lambda x: len(x.split('|')))
data['SNP_Count'] = data['NSNPS']
print(data[['CHR', 'BP1', 'BP2', 'LD_Length', 'SNP_Count']])
data[['CHR', 'BP1', 'BP2', 'LD_Length', 'SNP_Count']].to_csv('peihao_LD_block_stats1.csv', index=False)
data[['LD_Length', 'SNP_Count']].to_csv('peihao_LD_block_stats2.csv', index=False)

## resultes visualization
library(gridExtra)
library(cowplot)
library(ggplot2)
library(grid)

LD_stat <- read.csv("peihao_LD_block_stats2.csv")
snp_counts <- LD_stat$SNP_Count
log2_snp_counts <- log2(snp_counts)
snp_counts_median <-  median(LD_stat$SNP_Count)
ld_block_sizes <- LD_stat$LD_Length
log10_block_sizes <- log10(ld_block_sizes)
ld_block_sizes_median <-  median(LD_stat$LD_Length)
p1 <- ggplot(data.frame(log2_snp_counts), aes(x = log2_snp_counts)) +
  geom_histogram(binwidth = 0.5, fill = "#68A7BE", color = "#68A7BE") +
  geom_vline(aes(xintercept = median(log2_snp_counts)), linetype = "dashed", color = "#D1287A") +
  annotate("text", x = median(log2_snp_counts) + 0.15, y = 100*1000, label = paste("Median =", round(snp_counts_median, 2)), 
           color="#D1287A", vjust=1.5, hjust=0, size=4) +
  scale_y_continuous(labels = function(x) x / 1000) +
  labs(x = "log2(SNPs per LD block)", y = "LD blocks (x1000)") +
  theme(
    axis.title = element_text(size = 10, colour = 'black'),
    axis.text = element_text(size = 8, colour = 'black'), 
    axis.line = element_line(size = 0.5, colour = 'black'),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank())
p1 

p2 <- ggplot(data.frame(log10_block_sizes), aes(x = log10_block_sizes)) +
  geom_histogram(binwidth = 0.25, fill = "#68A7BE", color = "#68A7BE") +
  geom_vline(aes(xintercept = median(log10_block_sizes)), linetype = "dashed", color = "#D1287A") +
  annotate("text", x = median(log10_block_sizes) + 0.15, y = 100*1000, label = paste("Median =", round(ld_block_sizes_median, 2)," bp"), 
           color="#D1287A", vjust=1.5, hjust=0, size=4) +
  scale_y_continuous(labels = function(x) x / 1000) +
  labs(x = "LD block size (log10, bp)", y = "LD blocks (x1000)")+
  theme(
    axis.title = element_text(size = 10, colour = 'black'),
    axis.text = element_text(size = 8, colour = 'black'), 
    axis.line = element_line(size = 0.5, colour = 'black'),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank())
p2

final_plot <- ggdraw(p1) +
  draw_plot(p2, x = 0.43, y = 0.3, width = 0.55, height = 0.65)  
print(final_plot)


## Chromosome length and SNP count statistics
library(ggplot2)
data<-read.csv("lengthandloci.csv")
head(data)
library(RColorBrewer)
colors <- colorRampPalette(brewer.pal(12, "Set3"))(28)
data$chromosome <- factor(data$chromosome)
p1 <- ggplot(data, aes(x = length, y = SNPnumber, color = chromosome, fill =  chromosome)) +
  geom_point(shape = 21, size = 2, color="black") +
  scale_color_manual(values = colors) + 
  scale_fill_manual(values = colors) +  
  xlab("Chromosome Length(million)") + ylab("SNP Number(million)") + 
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
  scale_x_continuous(limits = c(0, 200)) +  
  scale_y_continuous(limits = c(0, 2))+
  guides(color = guide_legend(ncol = 4),  
         fill = guide_legend(ncol = 4)) +
  labs(color = "black", fill = "chromosome")
p1

## mGWAS
/home/cgbl/biosoft/plink1.9/plink --bfile new_phliu_allsample --allow-no-sex --allow-extra-chr --chr-set 40 --pca 3 --out snp.pca3 
awk '{print 1,$3,$4,$5}' snp.pca3.eigenvec > c.txt
/home/cgbl/biosoft/gemma/gemma-0.98.4-linux-static-AMD64 -bfile new_phliu_allsample -gk 1 -o snp.all.matrix
nohup  bash 1.sh  &
## details of document 1.bash
#!/bin/bash  
for i in {1..1871}  
do
/home/cgbl/biosoft/gemma/gemma-0.98.4-linux-static-AMD64 -bfile peihao1100 -lmm 1 -k output1-100/snp.all.matrix.cXX.txt -c c.txt -n $i -o t$i 
done

## results visualization: Manhattan map
Run the "hebing.py" code to sort through the results of each class of metabolite association analysis, one by one
## details of hebing.py:
import pandas as pd
with open('Superclass.txt', 'r') as f:
    file_list = [line.strip() for line in f]
combined_df = pd.DataFrame()
for file in file_list:
    df = pd.read_csv(file, sep='\t')
    df = df[['chr', 'ps', 'p_wald']]
    if combined_df.empty:
        combined_df = df
    else:
        combined_df = pd.merge(combined_df, df, on=['chr', 'ps'], how='outer', suffixes=('', '_new'))
combined_df['p_wald'] = combined_df.filter(like='p_wald').min(axis=1)
final_df = combined_df[['chr', 'ps', 'p_wald']]
final_df.to_csv('/home/phliu/mGWAS901-1871/Summary of various types/Supercalss_result.txt', sep='\t', index=False)

awk '{ if(NR>1 && $3 != "") { if(min=="") min=$3; if($3<min) min=$3 }} END { print min }' Benzenoids_result.txt          9.352295e-35
awk '{ if(NR>1 && $3 != "") { if(min=="") min=$3; if($3<min) min=$3 }} END { print min }' Lipids_result.txt              6.333538e-46
awk '{ if(NR>1 && $3 != "") { if(min=="") min=$3; if($3<min) min=$3 }} END { print min }' Nucleosides_result.txt         1.727477e-42
awk '{ if(NR>1 && $3 != "") { if(min=="") min=$3; if($3<min) min=$3 }} END { print min }' Organicacids_result.txt        2.014039e-132
awk '{ if(NR>1 && $3 != "") { if(min=="") min=$3; if($3<min) min=$3 }} END { print min }' Organicoxygen_result.txt       8.8333299e-70
awk '{ if(NR>1 && $3 != "") { if(min=="") min=$3; if($3<min) min=$3 }} END { print min }' Organoheterocyclic_result.txt  2.1175209e-115
awk '{ if(NR>1 && $3 != "") { if(min=="") min=$3; if($3<min) min=$3 }} END { print min }' Phenylpropanoids_result.txt    1.584391e-32
awk '{ if(NR>1 && $3 != "") { if(min=="") min=$3; if($3<min) min=$3 }} END { print min }' Other_result.txt               5.515667e-26
awk '{ if(NR>1 && $3 != "") { if(min=="") min=$3; if($3<min) min=$3 }} END { print min }' Unclassified_result.txt        1.22596e-33


## Draw Manhattan Plot by using ggplot
library(tidyverse) 
install.packages("ggprism")
library(ggprism) 
library(ggtext)
library(ggrepel)

## Lipids and lipid-like molecules
gwas <- read_tsv("Lipids_result.txt")
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
## Display all chromosome numbers
p1<-ggplot(gwag_result,aes(x=Chromosome,y=-log(p_wald, base = 100))) +
           geom_point(aes(color=as.factor(chr)),size=1) +
           geom_hline(yintercept = 5.77,color="black",linetype=2)+
  scale_color_manual(values=rep(c("#CCEBC5","#BABDBA"),28)) +
  scale_x_continuous(label= axis$chr,breaks= axis$center,guide="prism_offset",expand = c(0.01,0.01))+
  scale_y_continuous(limits = c(0, 24),expand = c(0,0),guide="prism_offset")+
  labs(y="-log<sub>100</sub>(p-value)")+
  coord_cartesian(clip="off")+
  theme_prism(base_fontface = "plain",base_line_size = 0.5) + 
  theme(legend.position="none",
        axis.text=element_text(color="black",size=12),
        axis.title.y = element_markdown(size=15,color="black"),
        axis.title.x = element_text(size=15,color="black"))
ggsave("Lipids.pdf", plot = p1, width = 15, height = 3) 
## Only the specified labels are displayed.
desired_chromosomes <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 16, 18, 20, 24, 28)  
p1 <- ggplot(gwag_result, aes(x = Chromosome, y = -log(p_wald, base = 100))) +  
  geom_point(aes(color = as.factor(chr)), size = 1.5) +  
  scale_color_manual(values = rep(c("#9CD1C7", "#BABDBA"), 28)) +  
  scale_x_continuous(  
    breaks = axis$center, 
    labels = ifelse(axis$chr %in% desired_chromosomes, axis$chr, ""), 
    expand = c(0.01, 0.01)  
  ) +  
  scale_y_continuous(limits = c(0, 25), expand = c(0, 0)) +  
  labs(y = "-log<sub>100</sub>(p-value)") +  
  coord_cartesian(clip = "off") +  
  theme_prism(base_fontface = "plain", base_line_size = 0.5) +  
  theme(  
    legend.position = "none",  
    axis.text = element_text(color = "black", size = 12),  
    axis.title.y = element_markdown(size = 14, color = "black"),  
    axis.title.x = element_text(size = 16, color = "black"),
    axis.ticks.length = unit(0.1, "cm"))  
ggsave("Lipids.pdf", plot = p1, width = 13, height = 2.5)  

## Organic acids and derivatives
gwas <- read_tsv("Organicacids_result.txt")
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
p2 <- ggplot(gwag_result, aes(x = Chromosome, y = -log(p_wald, base = 100))) +  
  geom_point(aes(color = as.factor(chr)), size = 1) +  
  scale_color_manual(values = rep(c("#BEBAD8", "#BABDBA"), 28)) +  
  scale_x_continuous(breaks = axis$center, guide = "prism_offset", expand = c(0.01, 0.01)) +  
  scale_y_continuous(limits = c(0, 66), expand = c(0, 0), guide = "prism_offset") +  
  labs(y = "-log<sub>100</sub>(p-value)", x = NULL) + 
  coord_cartesian(clip = "off") +  
  theme_prism(base_fontface = "plain", base_line_size = 0.5) +   
  theme(  
    legend.position = "none",  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.line.x = element_blank(), 
    axis.ticks.length = unit(0.1, "cm"),   
    axis.text.y = element_text(color = "black", size = 12),  
    axis.title.y = element_markdown(size = 14, color = "black") )  
ggsave("Organicacids.pdf", plot = p2, width = 13, height = 2) 

## Organoheterocyclic compounds
gwas <- read_tsv("Organoheterocyclic_result.txt")
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
p3 <- ggplot(gwag_result, aes(x = Chromosome, y = -log(p_wald, base = 100))) +  
  geom_point(aes(color = as.factor(chr)), size = 1) +  
  scale_color_manual(values = rep(c("#8AB0D0", "#BABDBA"), 28)) +  
  scale_x_continuous(breaks = axis$center, guide = "prism_offset", expand = c(0.01, 0.01)) +  
  scale_y_continuous(limits = c(0, 58), expand = c(0, 0), guide = "prism_offset") +  
  labs(y = "-log<sub>100</sub>(p-value)", x = NULL) + 
  coord_cartesian(clip = "off") +  
  theme_prism(base_fontface = "plain", base_line_size = 0.5) +   
  theme(  
    legend.position = "none",  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.line.x = element_blank(), 
    axis.ticks.length = unit(0.1, "cm"),   
    axis.text.y = element_text(color = "black", size = 12),  
    axis.title.y = element_markdown(size = 14, color = "black") )  
ggsave("Organoheterocyclic.pdf", plot = p3, width = 13, height = 2) 

## Organic oxygen compounds
gwas <- read_tsv("Organicoxygen_result.txt")
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
p4 <- ggplot(gwag_result, aes(x = Chromosome, y = -log(p_wald, base = 100))) +  
  geom_point(aes(color = as.factor(chr)), size = 1) +  
  scale_color_manual(values = rep(c("#BCDD78", "#BABDBA"), 28)) +  
  scale_x_continuous(breaks = axis$center, guide = "prism_offset", expand = c(0.01, 0.01)) +  
  scale_y_continuous(limits = c(0, 35), expand = c(0, 0), guide = "prism_offset") +  
  labs(y = "-log<sub>100</sub>(p-value)", x = NULL) + 
  coord_cartesian(clip = "off") +  
  theme_prism(base_fontface = "plain", base_line_size = 0.5) +   
  theme(  
    legend.position = "none",  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.line.x = element_blank(), 
    axis.ticks.length = unit(0.1, "cm"),   
    axis.text.y = element_text(color = "black", size = 12),  
    axis.title.y = element_markdown(size = 14, color = "black") )  
ggsave("Organicoxygen.pdf", plot = p4, width = 13, height = 2)

## Benzenoids
gwas <- read_tsv("Benzenoids_result.txt")
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
p5 <- ggplot(gwag_result, aes(x = Chromosome, y = -log(p_wald, base = 100))) +  
  geom_point(aes(color = as.factor(chr)), size = 1) +  
  scale_color_manual(values = rep(c("#CCEBC5", "#BABDBA"), 28)) +  
  scale_x_continuous(breaks = axis$center, guide = "prism_offset", expand = c(0.01, 0.01)) +  
  scale_y_continuous(limits = c(0, 18), expand = c(0, 0), guide = "prism_offset") +  
  labs(y = "-log<sub>100</sub>(p-value)", x = NULL) + 
  coord_cartesian(clip = "off") +  
  theme_prism(base_fontface = "plain", base_line_size = 0.5) +   
  theme(  
    legend.position = "none",  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.line.x = element_blank(), 
    axis.ticks.length = unit(0.1, "cm"),   
    axis.text.y = element_text(color = "black", size = 12),  
    axis.title.y = element_markdown(size = 14, color = "black") )  
ggsave("Benzenoids.pdf", plot = p5, width = 13, height = 2)

## Phenylpropanoids and polyketides
gwas <- read_tsv("Phenylpropanoids_result.txt")
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
p6 <- ggplot(gwag_result, aes(x = Chromosome, y = -log(p_wald, base = 100))) +  
  geom_point(aes(color = as.factor(chr)), size = 1) +  
  scale_color_manual(values = rep(c("#FFF701", "#BABDBA"), 28)) +  
  scale_x_continuous(breaks = axis$center, guide = "prism_offset", expand = c(0.01, 0.01)) +  
  scale_y_continuous(limits = c(0, 16), expand = c(0, 0), guide = "prism_offset") +  
  labs(y = "-log<sub>100</sub>(p-value)", x = NULL) + 
  coord_cartesian(clip = "off") +  
  theme_prism(base_fontface = "plain", base_line_size = 0.5) +   
  theme(  
    legend.position = "none",  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.line.x = element_blank(), 
    axis.ticks.length = unit(0.1, "cm"),   
    axis.text.y = element_text(color = "black", size = 12),  
    axis.title.y = element_markdown(size = 14, color = "black") )  
ggsave("Phenylpropanoids1.pdf", plot = p6, width = 13, height = 2)



## Other
gwas <- read_tsv("Other_result.txt")
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
p7 <- ggplot(gwag_result, aes(x = Chromosome, y = -log(p_wald, base = 100))) +  
  geom_point(aes(color = as.factor(chr)), size = 1) +  
  scale_color_manual(values = rep(c("#EB8677", "#BABDBA"), 28)) +  
  scale_x_continuous(breaks = axis$center, guide = "prism_offset", expand = c(0.01, 0.01)) +  
  scale_y_continuous(limits = c(0, 15), expand = c(0, 0), guide = "prism_offset") +  
  labs(y = "-log<sub>100</sub>(p-value)", x = NULL) + 
  coord_cartesian(clip = "off") +  
  theme_prism(base_fontface = "plain", base_line_size = 0.5) +   
  theme(  
    legend.position = "none",  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.line.x = element_blank(), 
    axis.ticks.length = unit(0.1, "cm"),   
    axis.text.y = element_text(color = "black", size = 12),  
    axis.title.y = element_markdown(size = 14, color = "black") )  
ggsave("Other.pdf", plot = p7, width = 13, height = 2)

## Unclassified
gwas <- read_tsv("Unclassified_result.txt")
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
p8 <- ggplot(gwag_result, aes(x = Chromosome, y = -log(p_wald, base = 100))) +  
  geom_point(aes(color = as.factor(chr)), size = 1) + 
  scale_color_manual(values = rep(c("#F3B76F", "#BABDBA"), 28)) +  
  scale_x_continuous(breaks = axis$center, guide = "prism_offset", expand = c(0.01, 0.01)) +  
  scale_y_continuous(limits = c(0, 18), expand = c(0, 0), guide = "prism_offset") +  
  labs(y = "-log<sub>100</sub>(p-value)", x = NULL) + 
  coord_cartesian(clip = "off") +  
  theme_prism(base_fontface = "plain", base_line_size = 0.5) +   
  theme(  
    legend.position = "none",  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.line.x = element_blank(), 
    axis.ticks.length = unit(0.1, "cm"),   
    axis.text.y = element_text(color = "black", size = 12),  
    axis.title.y = element_markdown(size = 14, color = "black") )  
ggsave("Unclassified.pdf", plot = p8, width = 13, height = 2)

## Nucleosides, nucleotides, and analogues
gwas <- read_tsv("Nucleosides_result.txt")
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
p9 <- ggplot(gwag_result, aes(x = Chromosome, y = -log(p_wald, base = 100))) +  
  geom_point(aes(color = as.factor(chr)), size = 1) +  
  scale_color_manual(values = rep(c("#F4CFE4", "#BABDBA"), 28)) +  
  scale_x_continuous(breaks = axis$center, guide = "prism_offset", expand = c(0.01, 0.01)) +  
  scale_y_continuous(limits = c(0, 24), expand = c(0, 0), guide = "prism_offset") +  
  labs(y = "-log<sub>100</sub>(p-value)", x = NULL) + 
  coord_cartesian(clip = "off") +  
  theme_prism(base_fontface = "plain", base_line_size = 0.5) +   
  theme(  
    legend.position = "none",  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.line.x = element_blank(), 
    axis.ticks.length = unit(0.1, "cm"),   
    axis.text.y = element_text(color = "black", size = 12),  
    axis.title.y = element_markdown(size = 14, color = "black") )  
ggsave("Nucleosides.pdf", plot = p9, width = 13, height = 2)

## Result merge output
library(patchwork)   
combined_plot <- p9 / p8 / p7 / p6 / p5 / p4 / p3 / p2 / p1  
ggsave("allSuperClass.pdf", plot = combined_plot, width = 13, height = 18)  


## The correlation between the number of metabolite mQTLs and heritability
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra) 
data<-read.csv("h2QTLcount.csv")
head(data)

install.packages("coin")
library(coin)
spearman_test <- cor.test(data$h2, data$Number, method = "spearman")
spearman_test
# rho：0.2290317，p-value < 2.2e-16

p1 <- ggplot(data, aes(x = h2, y = Number, color = Class, fill = Class)) +
  geom_point(shape = 21, size = 2, color="black") +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", color = "#D1287A", aes(group = 1)) + 
  scale_fill_manual(values = c(
    "Benzenoids" = "#CCEBC5",
    "Lipids and lipid-like molecules" = "#9CD1C7",
    "Nucleosides, nucleotides, and analogues" = "#F4CFE4",
    "Organic acids and derivatives" = "#BEBAD8",
    "Organic oxygen compounds" = "#BCDD78",
    "Organoheterocyclic compounds" = "#8AB0D0",
    "Phenylpropanoids and polyketides" = "#FFFFBB",
    "Other" = "#EB8677",
    "Unclassified" = "#F3B76F"
  )) + 
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


## snpeff was used for site annotation
awk '{print $1, $1 "_" $4, $3, $4, $5, $6}' phliu_allsample.bim > phliu_allsample_modified.bim
/home/cgbl/biosoft/plink1.9/plink --chr-set 35 --allow-extra-chr --bfile phliu_allsample_modified --recode vcf-iid -out phliu_allsample_vcf
java -jar /home/cgbl/biosoft/snpeff/snpEff/snpEff.jar -c /home/cgbl/biosoft/snpeff/snpEff/snpEff.config GRCg6a.99 /home/cgbl/phliu/mGWAS-WGS/phliu_allsample_vcf.vcf > /home/cgbl/phliu/mGWAS-WGS/phliu_allsample_SNPeff.vcf
tail -n +40 /home/cgbl/phliu/mGWAS-WGS/phliu_allsample_SNPeff.vcf | awk 'BEGIN{FS="[_=;]"; OFS="\t"} {print $1,$2,$1"_"$2,$3,$4,$5,$6,$7}' > new_phliu_allsample_SNPeff.vcf
sed -i 's/||||||.*$//' new_phliu_allsample_SNPeff.vcf
sed -i 's/ /\t/g' new_phliu_allsample_SNPeff.vcf

## allQTL and QTL position information visualization
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


data1 <- read.table("mQTLposition.txt", header = TRUE, sep = "\t")
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


## Calculation and visualization of Lambda values
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

## heritability estimated by GCTA

## Lambda and Heritability Result visualization
install.packages("ggplot2")
library(ggplot2)

phe<-read.csv("lambda.csv")
head(phe)
phe$newSuperClass <- as.factor(phe$newSuperClass)
str(phe$newSuperClass)
str(phe$Lamba)
desired_order <- c("Lipids and lipid-like molecules","Organic acids and derivatives","Organoheterocyclic compounds","Organic oxygen compounds",  
                   "Benzenoids","Phenylpropanoids and polyketides","Other","Unclassified","Nucleosides, nucleotides, and analogues")  
desired_order <- desired_order[desired_order %in% mydata$SuperClass]  
mydata$SuperClass <- factor(mydata$SuperClass, levels = desired_order) 
class_colors <- c("Benzenoids" = "#CCEBC5","Lipids and lipid-like molecules" = "#9CD1C7","Nucleosides, nucleotides, and analogues" = "#F4CFE4",
                  "Organic acids and derivatives" = "#BEBAD8","Organic oxygen compounds" = "#BCDD78","Organoheterocyclic compounds" = "#8AB0D0",
                  "Phenylpropanoids and polyketides" = "#FFFFBB", "Unclassified" = "#F3B76F","Other" = "#EB8677")
p1 <- ggplot(phe, aes(x = newSuperClass, y = Lamba, fill = newSuperClass)) +  
  geom_boxplot(width = 0.6, color = "black", size = 0.4, outlier.size = 0.5) +  
  scale_fill_manual(values = class_colors) + 
  labs(x = "Super Class", y = "Lambda") +   
  theme_bw() +  
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
    plot.margin = margin(1, 1, 1, 1, 'cm'),  
    axis.text.y = element_markdown(size = 12,margin = margin(t = -2)), 
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),  
    axis.text.x = element_text(color = "black", size = 8)) +   
  scale_y_continuous(limits = c(0.8, 1.08), expand = c(0.01, 0.01), breaks = seq(0.8, 1.08, 0.07)) +  
  geom_hline(yintercept = 0.95, color = "#D1287A", linetype = "dashed", size = 0.5) +  
  geom_hline(yintercept = 1.05, color = "#D1287A", linetype = "dashed", size = 0.5)+  
  scale_x_discrete(labels = function(x) {  
    color_values <- colors[x]  
    paste0("<span style='font-size:24pt; color:", color_values, "; border: 1px solid black;'>&#9679;</span>")}) 
p1
 

her<-read.csv("heritability.csv")
head(her)
her$newSuperClass <- as.factor(her$newSuperClass)
desired_order <- c("Lipids and lipid-like molecules","Organic acids and derivatives","Organoheterocyclic compounds","Organic oxygen compounds",  
                   "Benzenoids","Phenylpropanoids and polyketides","Other","Unclassified","Nucleosides, nucleotides, and analogues")  
desired_order <- desired_order[desired_order %in% her$newSuperClass]  
her$newSuperClass <- factor(her$newSuperClass, levels = desired_order) 
class_colors <- c("Benzenoids" = "#CCEBC5","Lipids and lipid-like molecules" = "#9CD1C7","Nucleosides, nucleotides, and analogues" = "#F4CFE4",
                  "Organic acids and derivatives" = "#BEBAD8","Organic oxygen compounds" = "#BCDD78","Organoheterocyclic compounds" = "#8AB0D0",
                  "Phenylpropanoids and polyketides" = "#FFFFBB", "Unclassified" = "#F3B76F","Other" = "#EB8677")
p2 <- ggplot(her, aes(x = newSuperClass, y = h2, fill = newSuperClass)) +  
  geom_boxplot(width = 0.6, color = "black", size = 0.4, outlier.size = 0.5) +  
  scale_fill_manual(values = class_colors) + 
  labs(x = "Super Class", y = "Heritability") +   
  theme_bw() +  
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
    plot.margin = margin(1, 1, 1, 1, 'cm'),  
    axis.text.y = element_markdown(size = 12,margin = margin(t = -2)), 
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),  
    axis.text.x = element_text(color = "black", size = 8)) +   
  scale_y_continuous(limits = c(0, 1), expand = c(0.01, 0.01), breaks = seq(0, 1, 0.2)) +  
  geom_hline(yintercept = 0.2, color = "#D1287A", linetype = "dashed", size = 0.5) +  
  scale_x_discrete(labels = function(x) {  
    color_values <- colors[x]  
    paste0("<span style='font-size:24pt; color:", color_values, "; border: 1px solid black;'>&#9679;</span>")}) 
p2


## Statistics of heritability
library(ggplot2)  
library(reshape2)  
library(ggtext)  

mydata <- read.csv("h2countratio.csv")  
head(mydata)  
mydata <- melt(mydata, id.vars = 'Group')  
head(mydata)   
desired_order <- c("Lipids and lipid-like molecules","Organic acids and derivatives","Organoheterocyclic compounds", "Organic oxygen compounds",  
                   "Benzenoids","Phenylpropanoids and polyketides","Other","Unclassified", "Nucleosides, nucleotides, and analogues")  
mydata$Group <- factor(mydata$Group, levels = desired_order)  
mydata$variable <- factor(mydata$variable, levels = c("X6","X5", "X4", "X3", "X2", "X1"))  
group_colors <- c("Benzenoids" = "#CCEBC5", "Lipids and lipid-like molecules" = "#9CD1C7", "Nucleosides, nucleotides, and analogues" = "#F4CFE4", "Organic acids and derivatives" = "#BEBAD8",  
                  "Organic oxygen compounds" = "#BCDD78", "Organoheterocyclic compounds" = "#8AB0D0", "Phenylpropanoids and polyketides" = "#FFFFBB", "Other" = "#EB8677", "Unclassified" = "#F3B76F")  
fill_colors <- c( "X1" = "#68A7BE","X2" = "#DEEEED","X3" = "#EDA46D", "X4" = "#FEDFB2", "X5" = "#EE7E77","X6" = "#FEECE7") 
legend_labels <- c("X1" = "0-0.1","X2" = "0.1-0.2","X3" = "0.2-0.3", "X4" = "0.3-0.4","X5" = "0.4-0.5","X6" = ">0.5")  
p1 <- ggplot(mydata, aes(Group, value, fill = variable)) +  
  geom_bar(stat = "identity", color = "black", size = 0.4, position = "stack", width = 0.6) +  
  scale_fill_manual(values = fill_colors, labels = legend_labels) + 
  labs(fill = "Heritability")+
  theme(  
    axis.text.x = element_markdown(color = 'black', size = 7, angle = 45, hjust = 1, margin = margin(t = -3)), 
    axis.title = element_text(color = 'black', size = 11),  
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    panel.background = element_rect(fill = "white"),  
    plot.margin = margin(1, 1, 1, 1, 'cm')  
  ) +  
  xlab("Super Calss") + ylab("Percentage(%)") +  
  scale_x_discrete(labels = function(x) {  
    color_values <- group_colors[x]  
    paste0("<span style='font-size:24pt; color:", color_values, "; border: 2px solid black;'>&#9679;</span>")})  
p1  


## chCADD of mQTL
## chCADD accumulated by python
cat chCADD.tar.gz.aa chCADD.tar.gz.ab chCADD.tar.gz.ac chCADD.tar.gz.ad chCADD.tar.gz.ae chCADD.tar.gz.af chCADD.tar.gz.ag >> chCADD.tar.gz 
tar -xzf chCADD.tar.gz
cd /home/phliu/chCADD-scores/chCADD-scores
chmod +x Miniconda3-latest-Linux-x86_64.sh
./Miniconda3-latest-Linux-x86_64.sh
source ~/.bashrc
conda --version
conda create --name pysam-env python=3.8
conda activate pysam-env
pip install pysam
python -c "import pysam; print(pysam.__version__)"
python3 chCADD_annotate_SNP.py -i phastConsvcf.tsv -c 1,2 -p chCADD.tsv.gz >> chCADD-annotated-sites.tsv

## chCADD visualization
install.packages("ggplot2")
library(ggplot2)
phe<-read.csv("chCADD.tsv",header = TRUE, sep = "\t", stringsAsFactors = FALSE)
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
    plot.margin = margin(1, 1, 1, 1, 'cm'),  
    axis.text.y = element_markdown(size = 12,margin = margin(t = -2)), 
    axis.ticks.y = element_line(color = "black", linewidth = 0.5),  
    axis.text.x = element_text(color = "black", size = 8)) +    
  xlab("Super Calss") + ylab("chCADD") +  
  scale_x_discrete(labels = function(x) {  
    color_values <- class_colors[x]  
    paste0("<span style='font-size:24pt; color:", color_values, "; border: 2px solid black;'>&#9679;</span>")})  
p1


## PVE of mQTL
## PVE accumulated by r
colnames <- c("chr", "rs", "ps", "n_miss", "allele1", "allele0", "af", "beta", "se", "logl_H1", "l_remle", "p_wald")
a4 <- read.table("filtered_meta1.txt", header = FALSE, sep = "\t", col.names = colnames)
head(a4)
a4$pve <- (2 * (a4$beta^2 * a4$af * (1 - a4$af))) / (2 * a4$beta * a4$af * (1 - a4$af) + a4$se^2 * 2 * 2257 * a4$af * (1 - a4$af))
head(a4)
write.table(a4, file = "meta1pve.txt", sep = "\t", row.names = FALSE, quote = FALSE)

## PVE visualization
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggsignif")
library(ggplot2)
library(ggpubr)
library(ggsignif)

df<-read.csv("pve_summary.csv")
head(df)
class_colors <- c("Benzenoids" = "#CCEBC5","Lipids and lipid-like molecules" = "#9CD1C7","Nucleosides, nucleotides, and analogues" = "#F4CFE4","Organic acids and derivatives" = "#BEBAD8",
                  "Organic oxygen compounds" = "#BCDD78","Organoheterocyclic compounds" = "#8AB0D0","Phenylpropanoids and polyketides" = "#FFFFBB","Unclassified" = "#F3B76F","Other" = "#EB8677")
desired_order <- c("Lipids and lipid-like molecules","Organic acids and derivatives","Organoheterocyclic compounds", "Organic oxygen compounds",  
                   "Benzenoids","Phenylpropanoids and polyketides","Other","Unclassified", "Nucleosides, nucleotides, and analogues")  
df$class <- factor(df$Superclass, levels = desired_order)  
p1 <- ggplot(df, aes(x = Superclass, y = PVE, fill =  Class, color =  Class)) +
  geom_boxplot(width = 0.6, position = position_dodge(width = 0.7),outlier.size=1) +   
  scale_color_manual(values = c("#EE7E77", "#68A7BE")) +
  scale_fill_manual(values = c("#EE7E77", "#68A7BE")) +
  theme_minimal() +  
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(x = "Super Class", y = "PVE") +
  theme(axis.text.x = element_markdown(color = 'black', size = 7, angle = 45, hjust = 1, margin = margin(t = -3)), 
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),  
        panel.background = element_rect(fill = "white"), 
        legend.position = "right",
        axis.title=element_text(color="black",size=11),
        plot.margin = margin(1, 1, 1, 1, 'cm'),
        axis.line = element_line(color = "black", size = 0.5), 
        axis.ticks = element_line(color = "black", size = 0.5)) +  
  scale_x_discrete(labels = function(x) {  
    color_values <- class_colors[x]  
    paste0("<span style='font-size:24pt; color:", color_values, "; border: 2px solid black;'>&#9679;</span>")})  
p1


## Statistics of the number of mQTL and Genes
## loci annotation
BiocManager::install("ChIPpeakAnno",update = FALSE)
library(ChIPpeakAnno)
options(scipen=200)
ann=read.table("Gallus_6.gff.txt",header=T,sep="\t") 
ann=with(ann, GRanges(as.character(Chr),IRanges(as.numeric(Gene_start), as.numeric(Gene_end)),id = as.character(Gene_ID)))
head(ann)
peak=read.table("site_processed.txt",header=T,sep="\t")
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
write.table(results2,"site_annotation.txt",sep="\t",quote=F,row.names=F)

## Visualized the number of mQTL and Genes
install.packages("gcookbook")
install.packages("ggplot2")
install.packages("scales")
library(gcookbook)
library(ggplot2)
library(scales)
data <- read.csv('LociGeneNumber.csv',header = T)
head(data)
str(data$Superclass)
class_colors <- c("Benzenoids" = "#CCEBC5","Lipids and lipid-like molecules" = "#9CD1C7","Nucleosides, nucleotides, and analogues" = "#F4CFE4","Organic acids and derivatives" = "#BEBAD8",
                  "Organic oxygen compounds" = "#BCDD78","Organoheterocyclic compounds" = "#8AB0D0","Phenylpropanoids and polyketides" = "#FFFFBB","Unclassified" = "#F3B76F","Other" = "#EB8677")
desired_order <- c("Lipids and lipid-like molecules","Organic acids and derivatives","Organoheterocyclic compounds", "Organic oxygen compounds",  
                   "Benzenoids","Phenylpropanoids and polyketides","Other","Unclassified", "Nucleosides, nucleotides, and analogues")  
data$SuperClass <- factor(data$Superclass, levels = desired_order) 
data$Group <- factor(data$Group,levels = c("Loci", "Gene"))
p1 <- ggplot(data, aes(x = SuperClass, y = Number/1000, color = Group, fill = Group)) +  
  geom_col(position = "dodge", color = "black", width = 0.75) +  
  geom_text(aes(label = Number), position = position_dodge(width = 0.8), vjust = -0.5, color = "black", family = "Arial", size = 2.5) +  
  scale_color_manual(values = c("#68A7BE","#EE7E77")) +  
  scale_fill_manual(values = c("#68A7BE","#EE7E77")) +  
  labs(x = 'Super Class', y = 'Number(K)') +  
  theme_minimal() +  
  theme(  
    legend.position = "right",  
    axis.title = element_text(color="black",size=11),  
    axis.text.x = element_markdown(color = 'black', size = 7, angle = 45, hjust = 1, margin = margin(t = -3)), 
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
    panel.grid.minor = element_blank(),  
    plot.margin = margin(1, 1, 1, 1, 'cm'),  
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(color = "black", size = 0.5), 
    axis.ticks = element_line(color = "black", size = 0.5)) +  
  scale_x_discrete(expand = c(0.06, 0.06),  
                   labels = function(x) {  
                     color_values <- class_colors[x]  
                     paste0("<span style='font-size:24pt; color:", color_values, "; border: 2px solid black;'>&#9679;</span>")}) +  
  scale_y_continuous(limits = c(0, 120),breaks = seq(0, 120, by = 30),labels = function(x) {format(x, big.mark = ",", scientific = FALSE)})
p1


## Comparison of different types of metabolites
library(ggplot2)
library(ggpubr)

data <- read.csv('handCV.csv',header = T)
head(data)
colors <- c("#EE7E77","#68A7BE")
data$Type <- factor(data$Type, levels = c("mQTL", "Non"))  

## Heritability
p1 <- ggplot(data, aes(x =newType, y = h2, fill = newType)) +
  geom_boxplot(width = 0.5, size = 0.4, color="black", outlier.size = 0.5) +  
  scale_fill_manual(values = colors) + 
  labs(x = '', y = 'Heritability') +
  theme_classic() +
  ylim(0,1.05)+
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
    comparisons = list(c("mQTL", "Non")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = c(0.9),
    size = 0.5, color = "black", textsize = 5,  family = "Arial Black")
p1

## Coefficient of variation
p2 <- ggplot(data, aes(x =newType, y = CV, fill = newType)) +
  geom_boxplot(width = 0.5, size = 0.4, color="black", outlier.size = 0.5) +  
  scale_fill_manual(values = colors) + 
  labs(x = '', y = 'CV') +
  theme_classic() +
  ylim(0,20)+
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
    comparisons = list(c("mQTL", "Non")),
    map_signif_level = TRUE, 
    test = t.test, 
    y_position = c(17.5),
    size = 0.5, color = "black", textsize = 5,  family = "Arial Black")
p2


## Comparison of different types of QTL
## MAF visualization
library(ggplot2)
library(ggpubr)

data1 <- read.table('merged_maf.txt',header = T)
head(data1)
colors <- c("#EE7E77","#68A7BE")
data1$Type <- factor(data1$type, levels = c("mQTL", "nomQTL"))  

p1 <- ggplot(data1, aes(x = Type, y = maf, fill = Type)) +
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
    size = 0.5, color = "black", textsize = 5,  family = "Arial Black")+  
    scale_x_discrete(labels = c("mQTL" = "mQTL  ", "nomQTL" = "  nomQTL"))   
p1


## phastCons
## Analysis of conservation scores for all sites on chromosomes 1-28, processed sequentially.  
## Build index files for the reference genome file using the bash script suoyin.sh  
## Contents of suoyin.sh:  
#!/bin/bash  
input_dir="/home/phliu/snpEff/db/phastCons"  

for i in {1..28}; do  
    input_file="${input_dir}/Gallus_gallus.GRCg6a.dna.chromosome.${i}.fa"  
    output_file="${input_dir}/genome${i}.fai"  # Set the output file name  

    if [ -f "$input_file" ]; then  
        /home/phliu/snpEff/db/phastCons/samtools/bin/samtools faidx "$input_file"  
        mv "${input_file}.fai" "$output_file"  # Rename the index file  
        echo "Indexed: $input_file to $output_file"  
    else  
        echo "File not found: $input_file"  
    fi  
done  
## When running, sequentially change the name to genome.fai  
## Preparation of the genome vcf files  
## Split the phliu_allsample_vcf file into 28 files, one for each of the first 28 chromosomes  
## Run the bash script chaifen.sh to split the merged vcf file into 28 files named phliu_chr1.vcf to phliu_chr28.vcf  
## Contents of chaifen.sh:  
#!/bin/bash  
input_vcf="/home/phliu/snpEff/db/phastCons/phliu_allsample_vcf.vcf"  
mkdir -p split_vcf  
for chr in {1..28}; do  
    echo "Processing chromosome ${chr}..."  
    /usr/local/bin/vcftools  --vcf ${input_vcf} --chr ${chr} --recode --stdout > split_vcf/phliu_chr${chr}.vcf  
done  
echo "Splitting complete! Output files are saved in the split_vcf directory."  
## After generating the vcf files, run the new_process_vcf.pl script to process the vcf files, generating Conservation_phliu_chr1.vcf to Conservation_phliu_chr28.vcf  
# Perform conservation analysis  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr1.vcf > Chr1phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr2.vcf > Chr2phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr3.vcf > Chr3phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr4.vcf > Chr4phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr5.vcf > Chr5phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr6.vcf > Chr6phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr7.vcf > Chr7phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr8.vcf > Chr8phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr9.vcf > Chr9phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr10.vcf > Chr10phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr11.vcf > Chr11phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr12.vcf > Chr12phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr13.vcf > Chr13phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr14.vcf > Chr14phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr15.vcf > Chr15phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr16.vcf > Chr16phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr17.vcf > Chr17phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr18.vcf > Chr18phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr19.vcf > Chr19phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr20.vcf > Chr20phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr21.vcf > Chr21phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr22.vcf > Chr22phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr23.vcf > Chr23phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr24.vcf > Chr24phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr25.vcf > Chr25phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr26.vcf > Chr26phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr27.vcf > Chr27phastCons.vcf  
java -jar SnpSift.jar phastCons /home/phliu/snpEff/db/phastCons Conservation_phliu_chr28.vcf > Chr28phastCons.vcf  
## File Merging  
## Run the bash script baoshouxingzhengli.sh to delete the first 34 useless lines from the merged Chr1phastCons.vcf-Chr28phastCons.vcf  
## Contents of baoshouxingzhengli.sh:  
input_dir="/home/phliu/snpEff/db/phastCons"  
for i in {1..28}; do  
    input_file="${input_dir}/Chr${i}phastCons.vcf"  

    if [ -f "$input_file" ]; then  
        sed '1,34d' "$input_file" > "${input_file}.tmp"  
        mv "${input_file}.tmp" "$input_file"  

        echo "Deleted first 34 lines from: $input_file"  
    else  
        echo "File not found: $input_file"  
    fi  
done  
## Run the bash script baoshouxinghebing.sh to merge the modified Chr1phastCons.vcf-Chr28phastCons.vcf  
## Contents of baoshouxinghebing.sh:  
input_dir="/home/phliu/snpEff/db/phastCons"  
output_file="${input_dir}/merged_phastCons.vcf"  # Output file name after merging  
> "$output_file"  
header_written=false  
for i in {1..28}; do  
    input_file="${input_dir}/Chr${i}phastCons.vcf"  

    if [ -f "$input_file" ]; then  
        if [ "$header_written" = false ]; then  
            cat "$input_file" >> "$output_file"  
            header_written=true  
        else  
            sed '1d' "$input_file" >> "$output_file"  
        fi  
        echo "Merged: $input_file"  
    else  
        echo "File not found: $input_file"  
    fi  
done  
echo "All files merged into: $output_file"  
## Extract the site positions and conservation scores  
awk 'BEGIN{FS=OFS="\t"} {print $3, $8}' merged_phastCons.vcf > new_phastCons.txt  
## Replace missing scores with NA  
awk 'BEGIN{FS=OFS="\t"} !/^#/ {if($2==".") $2="NA"; print}' new_phastCons.txt > new_phastCons_updated.txt  

## Result visualization
data2 <- read.table('phastCons.txt',header = T)
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
    size = 0.5, color = "black", textsize = 5,  family = "Arial Black")+  
    scale_x_discrete(labels = c("mQTL" = "mQTL  ", "nomQTL" = "  nomQTL"))
p2


## distance to TSS
## Calculate distance to TSS using python
bed_file = '6a_lncRNA_biotype.bed' 
bim_file = 'phliu_allsample_modified.bim'  
output_file = 'distance_to_tss.txt' 
genes = []
with open(bed_file, 'r') as bed:
    for line in bed:
        fields = line.strip().split()
        chromosome = fields[0]
        start = int(fields[1])  
        end = int(fields[2]) 
        strand = fields[3]    
        gene_id = fields[4]   
        gene_name = fields[5] 
        gene_biotype = fields[6]  
        tss = start if strand == '+' else end
        genes.append((chromosome, tss, gene_id, gene_name, gene_biotype))
with open(bim_file, 'r') as bim, open(output_file, 'w') as outfile:
    outfile.write("Chrom\tPosition\tNearest_Gene_ID\tNearest_Gene_Name\tGene_Biotype\tDistance_to_TSS\n")
    for line in bim:
        fields = line.strip().split()
        chromosome = fields[0] 
        position = int(fields[3]) 
        min_distance = float('inf')
        nearest_gene = None
        for gene in genes:
            if gene[0] == chromosome:  
                distance = abs(position - gene[1])  
                if distance < min_distance:
                    min_distance = distance
                    nearest_gene = gene
        if nearest_gene:
            outfile.write(f"{chromosome}\t{position}\t{nearest_gene[2]}\t{nearest_gene[3]}\t{nearest_gene[4]}\t{min_distance}\n")
        else:
            outfile.write(f"{chromosome}\t{position}\tNA\tNA\tNA\tNA\n")
print(f"Calculation results have been saved to the {output_file} file.")  

## Result visualization
data3 <- read.table('tssdistance_combined_new.txt',header = T)
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
    size = 0.5, color = "black", textsize = 5,  family = "Arial Black")+  
    scale_x_discrete(labels = c("mQTL" = "mQTL  ", "nomQTL" = "  nomQTL"))
p3


## Chr3 visualization
## Manhattan Plot
install.packages("CMplot",type = "binary")
library(CMplot) 
mydf <- read.table("chr3_Manhattan.txt", header = TRUE)   
marker1 <- suppressWarnings(read.table("partSNP.txt", header = FALSE))  
marker1 <- marker1$V1  
gene_info <- suppressWarnings(read.table("partSNP.txt", header = FALSE, stringsAsFactors = FALSE))  
colnames(gene_info) <- c("SNP", "Gene_name")  
color_all <- "#1932AD"  
pch_values <- 18  
cex_values <- 1.5   
highlight_snps <- gene_info$SNP  
highlight_texts <- gene_info$Gene_name   
p1 <- CMplot(mydf, type = "p", plot.type = "m",  
             highlight = highlight_snps,   
             highlight.type = "h",  
             col = "#E9C6C6",  
             amplify = TRUE, bin.size = 1e6, 
             ylim = c(0, 35),  
             cex = 0.8,  
             highlight.col = color_all, highlight.cex = cex_values, highlight.pch = pch_values,  
             highlight.text = highlight_texts,  
             highlight.text.cex = 1,           
             highlight.text.font = 4,          
             file = "pdf", dpi = 300, file.output = TRUE, verbose = TRUE, width = 15, height = 3)

## Visualization of chromatin state
library(ggplot2)  
library(dplyr)   
data <- read.csv('chr3overlap.csv', header = TRUE)   
head(data)  
data_rect <- data %>%  
  mutate(  start = as.numeric(start),end = as.numeric(end),width = as.numeric(width)) %>%  
  mutate(xmin = start,  xmax = start + width,  ymin = 0,  ymax = 0.001)  
tissue_order <- c("Trachea", "Spleen", "Liver")
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
    scale_x_continuous(limits = c(105200000,107820000)) +  
    theme(  
        plot.margin = margin(1, 1, 1, 1, 'cm'),  
        axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(),  
        axis.title.x = element_blank(),  
        axis.ticks.length.x = unit(0.05, "cm"),  
        axis.line.x = element_blank(), 
        strip.text.y = element_text(size = 12, hjust = 1),  
        legend.position = "right",  
        legend.title = element_text(size = 12),   
        legend.text = element_text(size = 10)  
    ) +   
    labs(x = "Chr3") +  
    facet_wrap(~ tissue, scales = "free_y", ncol = 1, strip.position = "left")
p1

## chCADD and phastCons Visualization
install.packages("gcookbook")
install.packages("scales")
library(gcookbook)
library(ggplot2)
library(scales)
data <- read.table('chr3_allloci_phastCons.txt',header = T)
head(data)
p1 <- ggplot(data, aes(x = RANK, y = phastCons)) +   
  geom_col(position = "identity", fill = "#FEDFB2", width = 10) +  
  theme(axis.text.x = element_text(color = 'black',size = 8),
        axis.text.y = element_text(color = 'black', size = 8),
        axis.title.y = element_text(color = 'black',size = 11),
        axis.line = element_line(color = 'black'), 
        axis.title.x = element_blank(), 
        axis.ticks = element_line(color = 'black'), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(1, 1, 1, 1, 'cm')) +  
  coord_cartesian(ylim = c(0, 1)) +   
  scale_x_discrete(expand = c(0.01, 0.01)) +  
  scale_y_continuous(breaks = c(0, 1)) 
p1

data <- read.table('average_cadd_scores.txt',header = T)
head(data)
p1 <- ggplot(data, aes(x = Rank, y = Cadd)) +   
  geom_col(position = "identity", fill = "#68A7BE", width = 10) +  
  theme(axis.text.x = element_text(color = 'black',size = 8),
        axis.text.y = element_text(color = 'black', size = 8),
        axis.title.y = element_text(color = 'black',size = 11),
        axis.line = element_line(color = 'black'), 
        axis.title.x = element_blank(), 
        axis.ticks = element_line(color = 'black'), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(1, 1, 1, 1, 'cm')) +  
  coord_cartesian(ylim = c(0, 23)) +   
  scale_x_discrete(expand = c(0.01, 0.01)) +  
  scale_y_continuous(breaks = c(0, 23)) 
p1

## Functional Enrichment Analysis
## Enrichment analysis by ClusterProfiler Package
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

gene_ids <- read.csv("chr3geneID.csv", header = TRUE, stringsAsFactors = FALSE)
head(gene_ids)
gene_ids <- as.character(gene_ids$EntrezGeneID)
gene_ids <- gene_ids[!is.na(gene_ids) & nzchar(gene_ids)]

# GO term enrichment analysis
GO_all_diff <- enrichGO(gene = gene_ids, OrgDb = org.Gg.eg.db, keyType = 'ENTREZID', ont = "ALL",pAdjustMethod = "BH", pvalueCutoff  = 1,qvalueCutoff = 1, readable = TRUE)
head(GO_all_diff)
output_file <- "GO_results.csv"
write.csv(GO_all_diff, file = output_file, row.names = TRUE)

# KEGG enrichment analysis
kegg_enrich <- enrichKEGG(gene = gene_ids, organism = 'gga', keyType = 'ncbi-geneid', pAdjustMethod = "BH", pvalueCutoff  = 1, qvalueCutoff  = 1)
head(kegg_enrich)
output_file <- "KEGG_results.csv"
write.csv(kegg_enrich, file = output_file, row.names = TRUE)

## Results Visualization
library(tidyverse)
library(ggh4x)
library(ggfun)
library(ggnewscale)
library(grid)

data <- read.csv("chr3enrichment.csv", header=T)  
head(data)  
data$Description <- factor(data$Description, levels = unique(data$Description))  
p1 <- data %>%
  ggplot() +
  geom_point(data = data %>% dplyr::filter(ONTOLOGY == "KEGG"),  
             aes(x = Count, y = interaction(Description, ONTOLOGY), fill = pvalue, size = Count), shape = 21) +  
  scale_fill_gradient(low = "#DEEEED", high = "#68A7BE", name = "KEGG p-value") +  
  ggnewscale::new_scale_fill() + 
  geom_point(data = data %>% dplyr::filter(ONTOLOGY == "CC"),  
             aes(x = Count, y = interaction(Description, ONTOLOGY), fill = pvalue, size = Count), shape = 21) +  
  scale_fill_gradient(low = "#FEECE7", high = "#EE7E77", name = "CC p-value") +  
  ggnewscale::new_scale_fill() + 
  geom_point(data = data %>% dplyr::filter(ONTOLOGY == "BP"),  
             aes(x = Count, y = interaction(Description, ONTOLOGY), fill = pvalue, size = Count), shape = 21) +  
  scale_fill_gradient(low = "#8c96c6", high = "#8c6bb1", name = "BP p-value") +  
  ggnewscale::new_scale_fill() + 
  geom_point(data = data %>% dplyr::filter(ONTOLOGY == "MF"),  
             aes(x = Count, y = interaction(Description, ONTOLOGY), fill = pvalue, size = Count), shape = 21) +  
  scale_fill_gradient(low = "#F5E6C2", high = "#CC9704", name = "MF p-value") +  
  ggnewscale::new_scale_fill() +  
  guides(y = "axis_nested",  
         y.sec = guide_axis_manual(  
           breaks = 1:nrow(data),  
           labels = data$Description  
         )) +  
  ggtitle(label = "GO and KEGG annotation") +  
  labs(x = "Count", y = "Description") +  
  scale_size(range = c(2, 6),  
             guide = guide_legend(override.aes = list(fill = "#000000"))) +  
  theme_bw() +  
  theme(  
    ggh4x.axis.nestline.y = element_line(size = 3, color = c("#68A7BE", "#EE7E77","#8c6bb1", "#CC9704")),  
    ggh4x.axis.nesttext.y = element_text(colour = c("#68A7BE", "#EE7E77","#8c6bb1", "#CC9704")),  
    legend.background = element_roundrect(color = "#969696"),  
    panel.border = element_rect(size = 0.5),  
    plot.margin = margin(t = 1, r = 3, b = 1, l = 1, unit = "cm"),  
    axis.text = element_text(color = "#000000", size = 9),  
    axis.text.y = element_text(color = c(rep("#68A7BE", 5), rep("#EE7E77", 5),rep("#8c6bb1", 5),rep("#CC9704", 5))), 
    axis.text.y.left = element_blank(),  
    axis.ticks.length.y.left = unit(10, "pt"),  
    axis.ticks.y.left = element_line(color = NA),  
    axis.title = element_text(color = "#000000", size = 13),  
    plot.title = element_text(color = "#000000", size = 16, hjust = 0.5)  
  ) +  
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 3))

p1



## Chromatin state enrichment analysis
## Enrichment analysis using Python
import scipy.stats as stats
from statsmodels.stats.multitest import multipletests
import os
organisms = ["Cortex", "Hypothalamus", "Cerebellum", "Testis", "Muscle", "Heart", "Adipose", "Kidney", "Spleen", "BMarrow", "Lung", "Trachea", "Thymus", "Liver", "Provent", "Gizzard", "Duodenum", "Jejunum", "Ileum", "Colon", "Cecum", "ShellGland", "Bursa"]
for organism in organisms:
    loci_count_file = f"/home/abx/PeihaoLiu/Organicacids/{organism}_16_segments_state_loci_count.txt"
    background_file = f"/home/abx/PeihaoLiu/Background/{organism}_background.txt"
    state_loci_count = {}
    with open(loci_count_file, "r") as file:
        next(file) 
        for line in file:
            state, loci_count = line.strip().split("\t")
            state_loci_count[state] = int(loci_count)
    state_background = {}
    with open(background_file, "r") as file:
        for line in file:
            state, background_count = line.strip().split("\t")
            state_background[state] = int(background_count)
    total_loci_count = sum(state_loci_count.values())
    total_background_count = sum(state_background.values())
    enrichment_factors = {}
    p_values = {}
    for state in state_loci_count:
        loci_count = state_loci_count[state]
        background_count = state_background[state]
        enrichment_factor = (loci_count / total_loci_count) / (background_count / total_background_count)
        enrichment_factors[state] = enrichment_factor
        p_value = stats.hypergeom.sf(loci_count - 1, total_background_count, background_count, total_loci_count)
        p_values[state] = p_value
    adjusted_p_values = multipletests(list(p_values.values()), method='fdr_bh')[1]
    output_file = f"/home/abx/PeihaoLiu/Organicacids/Organicacidsresults/{organism}_Enrichment_Results.txt"
    with open(output_file, "w") as outfile:
        outfile.write("State\tEnrichment_Factor\tP_Value\tAdjusted_P_Value\n")
        for i, state in enumerate(state_loci_count):
            outfile.write(f"{state}\t{enrichment_factors[state]}\t{p_values[state]}\t{adjusted_p_values[i]}\n")
    print(f"Results have been saved to {output_file}\n")
print("All files are processed")

## Enrichment analysis results Visualization
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

## Benzenoids
data <- read.csv('Benzenoids_merged_enrichment_factors.csv',header = T)
head(data)
data_long <- melt(data, id.vars = "State", variable.name = "Tissue", value.name = "Value")
head(data_long)
data_long$State <- factor(data_long$State, levels = c("TssA", "TssAHet", "TxFlnK", "TxFlnkWk", "TxFlnkHet", "EnhA", "EnhAMe", "EnhAWk", "EnhAHet", "EnhPois", "ATAC_Is", "TssBiv", "Repr", "ReprWk", "Qui"))
p1 <- ggplot(data_long, aes(x = State, y = Value, color = State)) +
  geom_violin(aes(fill = State), width = 1, size = 0.5, color = "black", draw_quantiles = c(0.25, 0.5, 0.75)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 1.5, fill = "white") +
  scale_color_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  scale_fill_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  labs(y = '', x = 'Chromatin State') +
  coord_flip() +  
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 1, 'cm'))+
  geom_hline(yintercept = 1, color = "#D1287A", linetype = "dotted", size = 0.5) +  
  scale_y_continuous(limits = c(-0.5,3.6),
                     breaks = c(0,1,2,3),
                     labels = c(0,1,2,3),
                     guide = "prism_offset",
                     expand = c(0,0))
p1

## Lipids
data <- read.csv('Lipids_merged_enrichment_factors.csv',header = T)
head(data)
data_long <- melt(data, id.vars = "State", variable.name = "Tissue", value.name = "Value")
head(data_long)
data_long$State <- factor(data_long$State, levels = c("TssA", "TssAHet", "TxFlnK", "TxFlnkWk", "TxFlnkHet", "EnhA", "EnhAMe", "EnhAWk", "EnhAHet", "EnhPois", "ATAC_Is", "TssBiv", "Repr", "ReprWk", "Qui"))
p2 <- ggplot(data_long, aes(x = State, y = Value, color = State)) +
  geom_violin(aes(fill = State), width = 1, size = 0.5, color = "black", draw_quantiles = c(0.25, 0.5, 0.75)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 1.5, fill = "white") +
  scale_color_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  scale_fill_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  labs(y = '', x = 'Chromatin State') +
  coord_flip() + 
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, 'cm'),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),     
        axis.ticks.y = element_blank() )+
  geom_hline(yintercept = 1, color = "#D1287A", linetype = "dotted", size = 0.5)+  
  scale_y_continuous(limits = c(-0.5,3.6),
                     breaks = c(0,1,2,3),
                     labels = c(0,1,2,3),
                     guide = "prism_offset",
                     expand = c(0,0))
p2

## Nucleosides
data <- read.csv('Nucleosides_merged_enrichment_factors.csv',header = T)
head(data)
data_long <- melt(data, id.vars = "State", variable.name = "Tissue", value.name = "Value")
head(data_long)
data_long$State <- factor(data_long$State, levels = c("TssA", "TssAHet", "TxFlnK", "TxFlnkWk", "TxFlnkHet", "EnhA", "EnhAMe", "EnhAWk", "EnhAHet", "EnhPois", "ATAC_Is", "TssBiv", "Repr", "ReprWk", "Qui"))
p3 <- ggplot(data_long, aes(x = State, y = Value, color = State)) +
  geom_violin(aes(fill = State), width = 1, size = 0.5, color = "black", draw_quantiles = c(0.25, 0.5, 0.75)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 1.5, fill = "white") +
  scale_color_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  scale_fill_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  labs(y = '', x = 'Chromatin State') +
  coord_flip() + 
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, 'cm'),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),     
        axis.ticks.y = element_blank() )+
  geom_hline(yintercept = 1, color = "#D1287A", linetype = "dotted", size = 0.5)+  
  scale_y_continuous(limits = c(-0.5,3.6),
                     breaks = c(0,1,2,3),
                     labels = c(0,1,2,3),
                     guide = "prism_offset",
                     expand = c(0,0))
p3

## Organicacids
data <- read.csv('Organicacids_merged_enrichment_factors.csv',header = T)
head(data)
data_long <- melt(data, id.vars = "State", variable.name = "Tissue", value.name = "Value")
head(data_long)
data_long$State <- factor(data_long$State, levels = c("TssA", "TssAHet", "TxFlnK", "TxFlnkWk", "TxFlnkHet", "EnhA", "EnhAMe", "EnhAWk", "EnhAHet", "EnhPois", "ATAC_Is", "TssBiv", "Repr", "ReprWk", "Qui"))
p4 <- ggplot(data_long, aes(x = State, y = Value, color = State)) +
  geom_violin(aes(fill = State), width = 1, size = 0.5, color = "black", draw_quantiles = c(0.25, 0.5, 0.75)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 1.5, fill = "white") +
  scale_color_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  scale_fill_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  labs(y = '', x = 'Chromatin State') +
  coord_flip() + 
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, 'cm'),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),     
        axis.ticks.y = element_blank())+
  geom_hline(yintercept = 1, color = "#D1287A", linetype = "dotted", size = 0.5)+  
  scale_y_continuous(limits = c(-0.5,3.6),
                     breaks = c(0,1,2,3),
                     labels = c(0,1,2,3),
                     guide = "prism_offset",
                     expand = c(0,0))
p4


## Organicoxygen
data <- read.csv('Organicoxygen_merged_enrichment_factors.csv',header = T)
head(data)
data_long <- melt(data, id.vars = "State", variable.name = "Tissue", value.name = "Value")
head(data_long)
data_long$State <- factor(data_long$State, levels = c("TssA", "TssAHet", "TxFlnK", "TxFlnkWk", "TxFlnkHet", "EnhA", "EnhAMe", "EnhAWk", "EnhAHet", "EnhPois", "ATAC_Is", "TssBiv", "Repr", "ReprWk", "Qui"))
p5 <- ggplot(data_long, aes(x = State, y = Value, color = State)) +
  geom_violin(aes(fill = State), width = 1, size = 0.5, color = "black", draw_quantiles = c(0.25, 0.5, 0.75)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 1.5, fill = "white") +
  scale_color_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  scale_fill_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  labs(y = '', x = 'Chromatin State') +
  coord_flip() + 
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, 'cm'),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),     
        axis.ticks.y = element_blank())+
  geom_hline(yintercept = 1, color = "#D1287A", linetype = "dotted", size = 0.5)+  
  scale_y_continuous(limits = c(-0.5,3.6),
                     breaks = c(0,1,2,3),
                     labels = c(0,1,2,3),
                     guide = "prism_offset",
                     expand = c(0,0))
p5


## Organoheterocyclic
data <- read.csv('Organoheterocyclic_merged_enrichment_factors.csv',header = T)
head(data)
data_long <- melt(data, id.vars = "State", variable.name = "Tissue", value.name = "Value")
head(data_long)
data_long$State <- factor(data_long$State, levels = c("TssA", "TssAHet", "TxFlnK", "TxFlnkWk", "TxFlnkHet", "EnhA", "EnhAMe", "EnhAWk", "EnhAHet", "EnhPois", "ATAC_Is", "TssBiv", "Repr", "ReprWk", "Qui"))
p6 <- ggplot(data_long, aes(x = State, y = Value, color = State)) +
  geom_violin(aes(fill = State), width = 1, size = 0.5, color = "black", draw_quantiles = c(0.25, 0.5, 0.75)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 1.5, fill = "white") +
  scale_color_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  scale_fill_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  labs(y = '', x = 'Chromatin State') +
  coord_flip() + 
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, 'cm'),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),     
        axis.ticks.y = element_blank())+
  geom_hline(yintercept = 1, color = "#D1287A", linetype = "dotted", size = 0.5)+  
  scale_y_continuous(limits = c(-0.5,3.6),
                     breaks = c(0,1,2,3),
                     labels = c(0,1,2,3),
                     guide = "prism_offset",
                     expand = c(0,0))
p6


## Other
data <- read.csv('Other_merged_enrichment_factors.csv',header = T)
head(data)
data_long <- melt(data, id.vars = "State", variable.name = "Tissue", value.name = "Value")
head(data_long)
data_long$State <- factor(data_long$State, levels = c("TssA", "TssAHet", "TxFlnK", "TxFlnkWk", "TxFlnkHet", "EnhA", "EnhAMe", "EnhAWk", "EnhAHet", "EnhPois", "ATAC_Is", "TssBiv", "Repr", "ReprWk", "Qui"))
p7 <- ggplot(data_long, aes(x = State, y = Value, color = State)) +
  geom_violin(aes(fill = State), width = 1, size = 0.5, color = "black", draw_quantiles = c(0.25, 0.5, 0.75)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 1.5, fill = "white") +
  scale_color_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  scale_fill_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  labs(y = '', x = 'Chromatin State') +
  coord_flip() + 
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, 'cm'),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),     
        axis.ticks.y = element_blank())+
  geom_hline(yintercept = 1, color = "#D1287A", linetype = "dotted", size = 0.5)+  
  scale_y_continuous(limits = c(-0.5,3.6),
                     breaks = c(0,1,2,3),
                     labels = c(0,1,2,3),
                     guide = "prism_offset",
                     expand = c(0,0))
p7

## Phenylpropanoids
data <- read.csv('Phenylpropanoids_merged_enrichment_factors.csv',header = T)
head(data)
data_long <- melt(data, id.vars = "State", variable.name = "Tissue", value.name = "Value")
head(data_long)
data_long$State <- factor(data_long$State, levels = c("TssA", "TssAHet", "TxFlnK", "TxFlnkWk", "TxFlnkHet", "EnhA", "EnhAMe", "EnhAWk", "EnhAHet", "EnhPois", "ATAC_Is", "TssBiv", "Repr", "ReprWk", "Qui"))
p8 <- ggplot(data_long, aes(x = State, y = Value, color = State)) +
  geom_violin(aes(fill = State), width = 1, size = 0.5, color = "black", draw_quantiles = c(0.25, 0.5, 0.75)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 1.5, fill = "white") +
  scale_color_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  scale_fill_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  labs(y = '', x = 'Chromatin State') +
  coord_flip() + 
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, 'cm'),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),     
        axis.ticks.y = element_blank())+
  geom_hline(yintercept = 1, color = "#D1287A", linetype = "dotted", size = 0.5)+  
  scale_y_continuous(limits = c(-0.5,3.6),
                     breaks = c(0,1,2,3),
                     labels = c(0,1,2,3),
                     guide = "prism_offset",
                     expand = c(0,0))
p8


## Unclassified
data <- read.csv('Unclassified_merged_enrichment_factors.csv',header = T)
head(data)
data_long <- melt(data, id.vars = "State", variable.name = "Tissue", value.name = "Value")
head(data_long)
data_long$State <- factor(data_long$State, levels = c("TssA", "TssAHet", "TxFlnK", "TxFlnkWk", "TxFlnkHet", "EnhA", "EnhAMe", "EnhAWk", "EnhAHet", "EnhPois", "ATAC_Is", "TssBiv", "Repr", "ReprWk", "Qui"))
p9 <- ggplot(data_long, aes(x = State, y = Value, color = State)) +
  geom_violin(aes(fill = State), width = 1, size = 0.5, color = "black", draw_quantiles = c(0.25, 0.5, 0.75)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 1.5, fill = "white") +
  scale_color_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  scale_fill_manual(values = c("#BB322E", "#E0709D", "#387E4D", "#4CA969", "#8CC6A8", "#FAE84D", "#DAA743", "#F8D584", "#FAE6A3", "#FBECC5", "#86A2C4", "#D57146", "#726E6E", "#B0ACAC", "#E6E5E5")) +
  labs(y = '', x = 'Chromatin State') +
  coord_flip() + 
  plot.format +
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, 'cm'),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),     
        axis.ticks.y = element_blank())+
  geom_hline(yintercept = 1, color = "#D1287A", linetype = "dotted", size = 0.5)+  
  scale_y_continuous(limits = c(-0.5,3.6),
                     breaks = c(0,1,2,3),
                     labels = c(0,1,2,3),
                     guide = "prism_offset",
                     expand = c(0,0))
p9
 
p <- (p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9) + plot_layout(ncol = 9)
p