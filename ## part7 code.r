## part7 code

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
          geom_boxplot(notch = TRUE,position = position_dodge(width = 0.02),width=0.6, linewidth = 0.6,, outlier.size = 1) +                      
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
      plot.margin = unit(c(1, 1, 1, 1), 'cm'),
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
p1 <- ggplot(data1, aes(x = Group, y = newmeta152, fill = Group)) +  
          scale_fill_manual(values = c("#EE7E77","#68A7BE")) +    
          xlab("") + ylab("Intensity") +
          stat_boxplot(geom="errorbar",position = position_dodge(width = 0.1),width=0.1) + 
          geom_boxplot(notch = TRUE,position = position_dodge(width = 0.02),width=0.6, linewidth = 0.6,, outlier.size = 1) +                  
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
      plot.margin = unit(c(1, 1, 1, 1), 'cm'),
      legend.position = "none")+
    geom_signif(
    comparisons = list(c("H", "L")),
    map_signif_level = TRUE, 
    test = wilcox.test, 
    y_position = c(21.20),
    size = 0.8, color = "black", textsize = 4,  family = "Arial Black",fontface = "bold")
p1

data1 <-read.csv("knownmeta1691.csv",sep=",") 
head(data1)
p1 <- ggplot(data1, aes(x = Group, y = newmeta1691, fill = Group)) +  
          scale_fill_manual(values = c("#EE7E77","#68A7BE")) +    
          xlab("") + ylab("Intensity") +
          stat_boxplot(geom="errorbar",position = position_dodge(width = 0.1),width=0.1) + 
          geom_boxplot(notch = TRUE,position = position_dodge(width = 0.02),width=0.6, linewidth = 0.6,, outlier.size = 1) +                
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
      plot.margin = unit(c(1, 1, 1, 1), 'cm'),
      legend.position = "none")+
    geom_signif(
    comparisons = list(c("H", "L")),
    map_signif_level = TRUE, 
    test = wilcox.test, 
    y_position = c(14.20),
    size = 0.8, color = "black", textsize = 4,  family = "Arial Black",fontface = "bold")
p1

data1 <-read.csv("knownmeta1763.csv",sep=",") 
head(data1)
p1 <- ggplot(data1, aes(x = Group, y = newmeta1763, fill = Group)) +  
          scale_fill_manual(values = c("#EE7E77","#68A7BE")) +    
          xlab("") + ylab("Intensity") +
          stat_boxplot(geom="errorbar",position = position_dodge(width = 0.1),width=0.1) + 
          geom_boxplot(notch = TRUE,position = position_dodge(width = 0.02),width=0.6, linewidth = 0.6,, outlier.size = 1) +                     
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
      plot.margin = unit(c(1, 1, 1, 1), 'cm'),
      legend.position = "none")+
    geom_signif(
    comparisons = list(c("H", "L")),
    map_signif_level = TRUE, 
    test = wilcox.test, 
    y_position = c(16.80),
    size = 0.8, color = "black", textsize = 4,  family = "Arial Black",fontface = "bold")
p1



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



## Biomarker related gene GO term enrichment
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
gene_ids <- as.character(gene_ids$EntrezGeneID)
gene_ids <- gene_ids[!is.na(gene_ids) & nzchar(gene_ids)]
GO_all_diff <- enrichGO(gene = gene_ids, OrgDb = org.Gg.eg.db, keyType = 'ENTREZID', ont = "ALL",pAdjustMethod = "BH", pvalueCutoff  = 1,qvalueCutoff = 1, readable = TRUE)
head(GO_all_diff)
output_file <- "GO_results.csv"
write.csv(GO_all_diff, file = output_file, row.names = TRUE)

## Results Visualization
data<-read.csv("GO_results_significant.csv",header=T)
head(data)
p1 <- ggplot(data, aes(x = logrichfactor, y = logpvalue)) +  
  geom_point(aes(fill = Count, size = Count), shape = 21, color = "#121415", stroke = 0.5) +  
  scale_fill_gradient(low = "#68A7BE", high = "#EE7E77") +  
  scale_size(range = c(2, 5), name = "Count") +  
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(2, 6)) +   
  scale_y_continuous(expand = c(0.02, 0.02), limits = c(4, 9)) +   
  coord_cartesian(xlim = c(2, 6), ylim = c(4, 9)) +   
  theme(axis.text.x = element_text(color = 'black', size = 8),  
        axis.text.y = element_text(color = 'black', size = 8),  
        axis.title.x = element_text(color = 'black', size = 11),  
        axis.title.y = element_text(color = 'black', size = 11),  
        axis.line = element_line(color = 'black'),   
        axis.ticks = element_line(color = 'black'),   
        axis.ticks.length = unit(0.15, "cm"),   
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.border = element_blank(),   
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(1.5, 1.5, 1.5, 1.5, 'cm')) +  
    labs(x = expression(-log[2](Rich~Factor)), y = expression(-log[2](p-value))) 
p1 



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
setwd("/Users/liupeihao/Desktop")
library(ggplot2)
data<-read.table("17meta114traits.txt",header=T)
head(data)
p1 <- ggplot(data, aes(x = log2EF , y = lo2p)) +  
  geom_point(aes(fill = Benzenoids_Genes, size = Benzenoids_Genes), shape = 21, color = "#121415", stroke = 0.5) +  
  scale_fill_gradient(low = "#68A7BE", high = "#EE7E77") +  
  scale_size(range = c(2, 5), name = "Count") +  
  scale_x_continuous(expand = c(0.02, 0.02), limits = c(4, 10)) +   
  scale_y_continuous(expand = c(0, 0.2), limits = c(0, 8)) +   
  coord_cartesian(xlim = c(4, 10), ylim = c(0, 8)) +   
  theme(axis.text.x = element_text(color = 'black', size = 8),  
        axis.text.y = element_text(color = 'black', size = 8),  
        axis.title.x = element_text(color = 'black', size = 11),  
        axis.title.y = element_text(color = 'black', size = 11),  
        axis.line = element_line(color = 'black'),   
        axis.ticks = element_line(color = 'black'),   
        axis.ticks.length = unit(0.15, "cm"),   
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.border = element_blank(),   
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(1.5, 1.5, 1.5, 1.5, 'cm')) +  
  labs(x = expression(-log[2](Enrichment~Factor)), y = expression(-log[2](p-value))) +  
  geom_text(data = subset(data, Trait %in% c("MAGNETIC_HDL.C")),   
            aes(label = Trait),   
            vjust = 0,   
            hjust = 0,    
            nudge_x = 0.3,   
            color = "Black",   
            size = 3)  +  
  geom_hline(yintercept = 4.321928095, linetype = "dashed", color = "#D1287A")  
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