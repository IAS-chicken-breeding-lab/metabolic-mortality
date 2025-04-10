## part3 code

## Overlap of mQTL and cis-eQTL
# Run 1_extract.py to merge significant eQTLs from 28 tissues based on the tissue and locus correspondence, generating "eQTL_combined_data.txt"  
# Run 11.eQTLquchongfu.py to remove duplicate loci within the same tissue, keeping unique values, generating "eQTL_combined_data_unique.txt"  
# Use awk to split the above file into two columns:   
awk -F'_' '{print $1 "\t" substr($0, index($0, $2))}' eQTL_combined_data_unique.txt > eQTL_separated_columns.txt  
# Run 10_eQTLnumber.py to count the number of eQTLs in different tissues, generating "eQTL_feature_counts.txt"  

# Download the genotype VCF file from ChickneGTEx: Chicken_GTEx.phased.dedup.IDformatted.vcf.gz, and extract chr, ps, and id information for subsequent id conversion  
## View the first 50 lines of the content  
gzip -dc Chicken_GTEx.phased.dedup.IDformatted.vcf.gz | head -n 50  
## Extract chr, ps, and id information  
zcat Chicken_GTEx.phased.dedup.IDformatted.vcf.gz | awk 'NR>=50 {print $1, $2, $3}' > extracted_columns.vcf  
## Merge chr and ps  
awk '{print $1 "_" $2, $3}' extracted_columns.vcf > All_variations_id.vcf  

# Replace mGWAS result ids  
Run 2_replace.py to replace significant loci with the corresponding ids from GTEx for subsequent overlap preparation  
# Count the number of significant loci for each type of metabolite  
Run 3_count_lines.pl to generate line_counts.txt  

# Annotate QTLs, marking which tissue they are eQTLs for  
Run 4_MarkingSite.py to annotate loci, generating "_new" class files  
# Process the annotated files to extract common loci and remove duplicates  
Run 5_Sites_unique.py and save the results in the "processed_sites" folder, manually delete the column name "site" for the next step of statistics  
# Count the number of eQTL/QTLs for each type of metabolite  
Run 3_count_lines.pl to generate "line_counts.txt", combining with the previous "line_counts.txt" files to prepare "SuperClass.csv" for visualizing the number of loci for each type of metabolite  

# Count the number of eQTLs corresponding to QTLs for each type of metabolite in different tissues  
Run 6_tissue_statistics.py and save the results in the "statistics" folder  
# Merge the number of loci corresponding to different types of metabolites in different tissues  
Run 7_ClassTissuemerged.py to generate "merged_statistics.txt", manually replace missing values with 0, and delete the "organization" row  
Run 8_convert_statistics.py to modify the format of "merged_statistics.txt" and save the results in "converted_statistics.csv"  

# Count the number of eQTL/QTLs in different tissues  
Run 9_eQTLQTL_merged.py to count the number of QTL/eQTLs in different tissues and save the results in the "eQTLQTL_tissue" folder  
Run 12_eQTLquchongfu.py to remove duplicates from the result files in the eQTLQTL_tissue folder  
Run 3_count_lines.pl to generate "line_counts.txt", combining with the previous "eQTL_feature_counts.txt" files to prepare "Tissue.csv" for visualizing the number of loci for each type of metabolite.  

# Results visualization
## Overlap result bubble matrix plot  
library("ggplot2")  
data <- read.csv("converted_statistics.csv", header = TRUE)  
head(data)  
new_order <- c("Brain", "Cerebellum", "Hypothalamus", "Retina", "Pituitary", "Thymus", "Adipose", "Leukocytes", "Macrophage", "Bursa", "Spleen", "Lung", "Trachea", "Smallintestine", "Jejunum", "Duodenum", "Ileum", "Cecum", "Liver", "Kidney", "Ovary", "Testis", "Oviduct", "Heart", "Skin", "Embryo", "Muscle", "Blood")  
mycol <- colorRampPalette(c('#61AACF', '#98CADD',  '#E9C6C6', '#DA9599'))  
p1 <- ggplot(data, aes(x = Tissue, y = SuperClass)) +  
  geom_point(aes(size = `log2SNPNumber`, fill = `log2SNPNumber`),   
             shape = 21, color = "#565759", stroke = 0.5) +  
  scale_size(range = c(0, 7.5),   
             name = expression(paste("log"[2], " SNP Number"))) +  
  scale_fill_gradientn(colors = mycol(4),   
                       name = expression(paste("log"[2], " SNP Number"))) + 
  theme(axis.text.x = element_text(color = 'black', size = 12, angle = 270, hjust = 0, vjust = 0.5),  
        axis.text.y = element_text(color = 'black', size = 12),  
        axis.title.x = element_text(color = 'black', size = 15),  
        axis.title.y = element_text(color = 'black', size = 15),  
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
        panel.grid.minor = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),  
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(0.5, 0.5, 1.5, 2.5, 'cm'),  
        legend.position = "left") + 
  scale_x_discrete(limits = new_order) +   
  labs(x = '', y = 'Super Class') +  
  guides(fill = guide_legend(title = expression(paste("log"[2], " SNP Number"))),   
         size = guide_legend(title = expression(paste("log"[2], " SNP Number"))))  
p1   

## Display only eQTL/QTL  
mydata <- read.csv("SuperClass-1.csv")   
head(mydata)  

p2 <- ggplot(mydata, aes(logValue, variable), position = "stack") +  
  geom_bar(aes(fill = SuperClass), stat = "identity", color = "black", size = 0.6,  
           position = "stack", width = 0.6) +  
  scale_fill_manual(values = c("#FEECE7")) +  
  theme(axis.text.x = element_text(color = 'black', face = 'bold', size = 12),  
        axis.text.y = element_text(color = 'black', face = 'bold', size = 12),  
        axis.title = element_text(color = 'black', face = 'bold', size = 14),  
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
        panel.grid.minor = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1.5),  
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(1, 1, 1, 1, 'cm')) +  
  xlab("log2(Number)") + ylab("Super Class")  
p2  

p2 <- ggplot(mydata, aes(logValue, variable), position = "stack") +  
  geom_bar(aes(fill = SuperClass), stat = "identity", color = "black", size = 0.6,  
           position = "stack", width = 0.6) +  
  scale_fill_manual(values = c("#FEECE7")) +  
  scale_x_continuous(limits = c(0, 15)) +   
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(color = 'black', face = 'bold', size = 12),  
        axis.title = element_text(color = 'black', face = 'bold', size = 14),  
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
        panel.grid.minor = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1.5),  
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(1, 1, 1, 1, 'cm')) +  
  xlab("log2(Number)") + ylab("")  
p2  

# Enrichment fold eQTL/QTL in different metabolite categories as a single-column bubble heatmap  
data <- read.csv('inmQTL.csv')  
head(data)  
data$x <- c(" ")   
p3 <- ggplot(data, aes(x = x, y = Superclass)) +  
  geom_point(aes(size = enrichment_fold, fill = enrichment_fold),   
             color = "black",  
             shape = 22,   
             stroke = 0.8) +  
  scale_fill_gradient(low = "#FEECE7", high = "#EE7E77") +  
  scale_size(range = c(2, 8)) +  
  theme(axis.text.y = element_text(color = 'black', face = 'bold', size = 12),  
        axis.text.x = element_text(color = 'black', face = 'bold', size = 12),  
        axis.title = element_text(color = 'black', face = 'bold', size = 14),  
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
        panel.grid.minor = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1.5),  
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(1, 1, 1, 1, 'cm')) +  
  xlab("Ratio(%)") + ylab("Super Class")  
p3  

p3 <- ggplot(data, aes(x = x, y = Superclass)) +  
  geom_point(aes(size = enrichment_fold, fill = enrichment_fold),     
             color = "black",  
             shape = 22,   
             stroke = 0.8) +  
  scale_fill_gradient(low = "#FEECE7", high = "#EE7E77") +  
  scale_size(range = c(2, 8)) +  
  theme(axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        axis.text.x = element_text(color = 'black', face = 'bold', size = 12),  
        axis.title = element_text(color = 'black', face = 'bold', size = 14),  
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
        panel.grid.minor = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1.5),  
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(1, 1, 1, 1, 'cm')) +  
  xlab("") + ylab("") +
  guides(fill = guide_legend(title = "Enrichment Fold"),   
         size = guide_legend(title = "Enrichment Fold"))   
p3 

## Display only QTL/eQTL  
mydata <-read.csv("eQTL_feature_counts-1.csv") 
head(mydata)

new_order <- c("Brain", "Cerebellum", "Hypothalamus", "Retina", "Pituitary", "Thymus", "Adipose", "Leukocytes", "Macrophage", "Bursa", "Spleen", "Lung", "Trachea", "Smallintestine", "Jejunum", "Duodenum", "Ileum", "Cecum", "Liver", "Kidney", "Ovary", "Testis", "Oviduct", "Heart", "Skin", "Embryo", "Muscle", "Blood")

p4 <- ggplot(mydata, aes(variable, logValue), position="stack") +
  geom_bar(aes(fill = SuperClass), stat = "identity", color="black", size=0.6,
           position = "stack", width = 0.6) +
  scale_fill_manual(values=c( "#DEEEED")) +
  theme(axis.text.x = element_text(color = 'black', face = 'bold', size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(color = 'black', face = 'bold', size = 12),
        axis.title = element_text(color = 'black', face = 'bold', size = 14),
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(1, 1, 1, 1, 'cm')) +
        scale_x_discrete(limits = new_order) + 
  xlab("Organization") + ylab("log2(Number)")
p4

p4 <- ggplot(mydata, aes(variable, logValue), position="stack") +
  geom_bar(aes(fill = SuperClass), stat = "identity", color="black", size=0.6,
           position = "stack", width = 0.6) +
  scale_fill_manual(values=c("#DEEEED")) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y = element_text(color = 'black', face = 'bold', size = 12),
        axis.title = element_text(color = 'black', face = 'bold', size = 14),
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(1, 1, 1, 1, 'cm')) +
  scale_x_discrete(limits = new_order) + 
  xlab("") + ylab("log2(Number)")
p4

## Enrichment fold QTL/eQTL in different metabolite categories as a single-column bubble heatmap  
data <- read.csv('enrichment_results.csv')
head(data)

new_order <- c("Brain", "Cerebellum", "Hypothalamus", "Retina", "Pituitary", "Thymus", "Adipose", "Leukocytes", "Macrophage", "Bursa", "Spleen", "Lung", "Trachea", "Smallintestine", "Jejunum", "Duodenum", "Ileum", "Cecum", "Liver", "Kidney", "Ovary", "Testis", "Oviduct", "Heart", "Skin", "Embryo", "Muscle", "Blood")
data$y <- c(" ") 

p3 <- ggplot(data, aes(x = Tissue, y = y)) +  
  geom_point(aes(size = Enrichment_Fold, fill = Enrichment_Fold),   
             color = "black",  
             shape = 22,   
             stroke = 0.8) +  
  scale_fill_gradient(low = "#DEEEED", high = "#68A7BE") +  
  scale_size(range = c(2, 8)) +  
  scale_x_discrete(limits = new_order) +   
  theme(axis.text.x = element_blank(),   
        axis.ticks.x = element_blank(),   
        axis.text.y = element_text(color = 'black', face = 'bold', size = 12),  
        axis.title = element_text(color = 'black', face = 'bold', size = 14),  
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),  
        panel.grid.minor = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1.5),  
        panel.background = element_rect(fill = "white"),  
        plot.margin = margin(1, 1, 1, 1, 'cm')) +  
  xlab("") + ylab("") +  
  guides(fill = guide_legend(title = "Enrichment Fold"),   
         size = guide_legend(title = "Enrichment Fold"))  
p3

## Annotate loci using R  
BiocManager::install("ChIPpeakAnno", update = FALSE)  
library(ChIPpeakAnno)  
options(scipen=200)   
ann = read.table("Gallus_6.gff.txt", header = TRUE, sep = "\t")  
ann = with(ann, GRanges(as.character(Chr), IRanges(as.numeric(Gene_start), as.numeric(Gene_end)), id = as.character(Gene_ID)))  
head(ann)   
peak = read.table("Spleenloci.txt", header = TRUE, sep = "\t")  
region = 100 
peak$BIN_START = peak$BIN_START - region  
peak$BIN_END = peak$BIN_END + region  
peak <- peak[complete.cases(peak),]  
peak <- with(peak, GRanges(as.character(CHR), IRanges(as.numeric(BIN_START), as.numeric(BIN_END)), id = as.character(peak_ID)))  
o = findOverlaps(ann, peak)  
results = cbind(as.data.frame(ann[queryHits(o)]), as.data.frame(peak[subjectHits(o)]))  
results2 = cbind(results[, 12], results[, 7], results[, 8:9], results[, 6])
names(results2) = c("peak_ID", "CHR", "BIN_START", "BIN_END", "QTL_ID;Gene_name")
results2$BIN_START = results2$BIN_START + region  
results2$BIN_END = results2$BIN_END - region    
write.table(results2, "Spleen_site_annotation.txt", sep = "\t", quote = FALSE, row.names = FALSE)  

## Gene and eGene Functional enrichment
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

## Gene functional enrichmental analysis
gene_ids <- read.csv("mQTL_entrezgeneIDs.csv", header = TRUE, stringsAsFactors = FALSE)
gene_ids <- as.character(gene_ids$EntrezGeneID)
gene_ids <- gene_ids[!is.na(gene_ids) & nzchar(gene_ids)]
GO_all_diff <- enrichGO(gene = gene_ids, OrgDb = org.Gg.eg.db, keyType = 'ENTREZID', ont = "ALL",pAdjustMethod = "BH", pvalueCutoff  = 0.05,qvalueCutoff = 0.05, readable = TRUE)
head(GO_all_diff)
output_file <- "Gene_GO_results.csv"
write.csv(GO_all_diff, file = output_file, row.names = TRUE)
kegg_enrich <- enrichKEGG(gene = gene_ids, organism = 'gga', keyType = 'ncbi-geneid', pAdjustMethod = "BH", pvalueCutoff  = 1, qvalueCutoff  = 1)
head(kegg_enrich)
output_file <- "Gene_KEGG_results.csv"
write.csv(kegg_enrich, file = output_file, row.names = TRUE)

## eGene functional enrichmental analysis
gene_ids <- read.csv("eGene_entrezgeneIDs.csv", header = TRUE, stringsAsFactors = FALSE)
gene_ids <- as.character(gene_ids$EntrezGeneID)
gene_ids <- gene_ids[!is.na(gene_ids) & nzchar(gene_ids)]
GO_all_diff <- enrichGO(gene = gene_ids, OrgDb = org.Gg.eg.db, keyType = 'ENTREZID', ont = "ALL",pAdjustMethod = "BH", pvalueCutoff  = 0.05,qvalueCutoff = 0.05, readable = TRUE)
head(GO_all_diff)
output_file <- "eGene_GO_results.csv"
write.csv(GO_all_diff, file = output_file, row.names = TRUE)
kegg_enrich <- enrichKEGG(gene = gene_ids, organism = 'gga', keyType = 'ncbi-geneid', pAdjustMethod = "BH", pvalueCutoff  = 1, qvalueCutoff  = 1)
head(kegg_enrich)
output_file <- "eGene_KEGG_results.csv"
write.csv(kegg_enrich, file = output_file, row.names = TRUE)


## GO results visualization
library(enrichplot)
install.packages(c("kableExtra", "colorblindcheck"))
library(kableExtra)
library(colorblindcheck)

## Gene
result <- read.csv("Gene_GO_results.csv", header = TRUE, stringsAsFactors = FALSE)
set.seed(1)
p1 <- enrichmentNetwork(result, colorBy = 'newpvalue', colorType = 'nes', nodeSize = 'Count', fontSize = 4, drawEllipses = TRUE, pCutoff = 0.01, verbose = TRUE)
p2 <- p1 + scale_color_gradient(low = '#ebcce2', high =  '#832440',name = "-lg(p-value)")
p2

#eGene
result <- read.csv("eGene_GO_results.csv", header = TRUE, stringsAsFactors = FALSE)
set.seed(1)
p1 <- enrichmentNetwork(result, colorBy = 'newpvalue', colorType = 'nes', nodeSize = 'Count', fontSize = 4, drawEllipses = TRUE, pCutoff = 0.01, verbose = TRUE)
p2 <- p1 + scale_color_gradient(low = '#a8dde1', high =  '#313772',name = "-lg(p-value)")


## KEGG results Visualization
library(tidyverse)
library(ggplot2)
library(dplyr)

## Gene
data <- read.csv('GeneTop15KEGG.csv',header = T)
head(data)
p1 <- ggplot(data) +  
      geom_col(aes(x = Count, y = reorder(KEGG, `logp`)), color = 'black', width = 0.6, fill = '#68A7BE') +  
      labs(x = 'Gene Count', y = NULL) +  
      theme_test(base_size = 11) +  
      theme(axis.text.y = element_text(angle = 0, hjust = 1),  
            axis.text = element_text(color = 'black'),  
            plot.margin = margin(0.5, 0.5, 1.5, 0.5, 'cm'),  
            panel.border = element_rect(size = 1.2)) +  
      scale_x_continuous(expand = c(0, 1), limits = c(0, 108),  
                         sec.axis = sec_axis(~./9, name = '-log2(p-value)', breaks = seq(0, 12, 3))) +  
      geom_line(aes(x = `logp` * 9, y = reorder(KEGG, `logp`), group = 1), linetype = 3, cex = 1) +  
      geom_point(aes(x = `logp` * 9, y = reorder(KEGG, `logp`)), color = '#E05E2F', size = 3.5) + 
      geom_text(aes(x = Count, y = reorder(KEGG, `logp`), label = Count), vjust = 0.5, hjust = -0.5, size = 2.5, fontface = 'bold')
p1

## eGene
data <- read.csv('eGeneTop15KEGG.csv',header = T)
head(data)
p1 <- ggplot(data) +  
      geom_col(aes(x = Count, y = reorder(KEGG, `logp`)), color = 'black', width = 0.6, fill = '#EE7E77') +  
      labs(x = 'Gene Count', y = NULL) +  
      theme_test(base_size = 11) +  
      theme(axis.text.y = element_text(angle = 0, hjust = 1),  
            axis.text = element_text(color = 'black'),  
            plot.margin = margin(0.5, 0.5, 1.5, 0.5, 'cm'),  
            panel.border = element_rect(size = 1.2)) +  
      scale_x_continuous(expand = c(0, 0.5), limits = c(0, 60),  
                         sec.axis = sec_axis(~./4, name = '-log2(p-value)', breaks = seq(0, 12, 3))) +  
      geom_line(aes(x = `logp` * 4, y = reorder(KEGG, `logp`), group = 1), linetype = 3, cex = 1) +  
      geom_point(aes(x = `logp` * 4, y = reorder(KEGG, `logp`)), color = '#535794', size = 3.5) + 
      geom_text(aes(x = Count, y = reorder(KEGG, `logp`), label = Count), vjust = 0.5, hjust = -0.5, size = 2.5, fontface = 'bold')
p1



## Differential expression of QTL/eQTL genes in the heart, liver and spleen
## Different analysis by DEseq2
BiocManager::install("DESeq2")
library(DESeq2)
countData <- read.csv("SpleenGene_geneexpression.csv", header=TRUE, row.names=1)
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
write.csv(res_df, file = "Spleen_diffexpre.csv", quote = FALSE)

## Rusults Visualization
library(dplyr)
library(ggplot2) 
library(ggprism) 
library(ggrepel) 
library(reshape2)

## Gene Volcano plot 
BRCA_Match_DEG <- read.csv("Heart_diffexpre.csv")
BRCA_Match_DEG$logP <- -log(BRCA_Match_DEG$padj, base = 10)
head(BRCA_Match_DEG)
BRCA_Match_DEG$label <- ifelse(BRCA_Match_DEG$logP > 1.3 & abs(BRCA_Match_DEG$log2FoldChange) > 0.263034406, BRCA_Match_DEG$gene, "") 
p1 <- ggplot(BRCA_Match_DEG, aes(x = log2FoldChange, y = logP, fill = group)) +  
  geom_point(aes(color = group), shape = 21, alpha = 1, size = 2.5, stroke = 0.5, show.legend = FALSE) +  
  geom_hline(yintercept = 9.5, lty = 2, col = "grey", lwd = 0.5) +  
  geom_hline(yintercept = 1.3, lty = 2, col = "black", lwd = 0.5) +  
  geom_vline(xintercept = c(-0.263034406, 0.263034406), lty = 2, col = "black", lwd = 0.5) +  
  annotate("text", x = -3.8, y = 10, label = "DOWN", color = "black", hjust = 0, size = 3, family = "Arial") +  
  annotate("text", x = 2.6, y = 10, label = "UP", color = "black", hjust = 0, size = 3, family = "Arial") +  
  scale_fill_manual(values = c("UP" = "#EE7E77", "Down" = "#68A7BE", "not_change" = "gray")) +  
  scale_color_manual(values = c("UP" = "#9F0000", "Down" = "#003A75", "not_change" = "gray")) +  
  xlim(c(-5, 5)) + ylim(c(0, 10)) +  
  labs(x = "log2(FC,L/H)", y = "-lg(p-adj)") +   
  theme_prism(border = TRUE) +  
  theme(  
    plot.background = element_blank(),  
    panel.grid = element_blank(),  
    panel.background = element_blank(),  
    panel.border = element_rect(color = "black", linewidth = 1, fill = NA),  
    axis.line = element_blank(),  
    axis.ticks = element_line(color = "black", size = 0.5),  
    axis.ticks.length = unit(0.05, "cm"),  
    axis.text = element_text(color = "black", size = 8, family = "Arial", face = "plain"),  
    axis.title = element_text(color = "black", size = 11, family = "Arial", face = "plain"),  
    plot.title = element_text(color = "black", size = 7, family = "Arial", face = "plain"),
    plot.margin = margin(0.5, 0.2, 0.5, 0.5, 'cm'))  
p1  

BRCA_Match_DEG <- read.csv("Liver_diffexpre.csv")
BRCA_Match_DEG$logP <- -log(BRCA_Match_DEG$padj, base = 10)
head(BRCA_Match_DEG)
BRCA_Match_DEG$label <- ifelse(BRCA_Match_DEG$logP > 1.3 & abs(BRCA_Match_DEG$log2FoldChange) > 0.263034406, BRCA_Match_DEG$gene, "")
p2<- ggplot(BRCA_Match_DEG, aes(x = log2FoldChange, y = logP, fill = group)) +  
  geom_point(aes(color = group), shape = 21, alpha = 1, size = 2.5, stroke = 0.5, show.legend = FALSE) +  
  geom_hline(yintercept = 9.5, lty = 2, col = "grey", lwd = 0.5) +  
  geom_hline(yintercept = 1.3, lty = 2, col = "black", lwd = 0.5) +  
  geom_vline(xintercept = c(-0.263034406, 0.263034406), lty = 2, col = "black", lwd = 0.5) +  
  annotate("text", x = -3.8, y = 10, label = "DOWN", color = "black", hjust = 0, size = 3, family = "Arial") +  
  annotate("text", x = 2.6, y = 10, label = "UP", color = "black", hjust = 0, size = 3, family = "Arial") +  
  scale_fill_manual(values = c("UP" = "#EE7E77", "Down" = "#68A7BE", "not_change" = "gray")) +  
  scale_color_manual(values = c("UP" = "#9F0000", "Down" = "#003A75", "not_change" = "gray")) +  
  xlim(c(-5, 5)) + ylim(c(0, 10)) +  
  labs(x = "log2(FC,L/H)", y = "-lg(p-adj)") +   
  theme_prism(border = TRUE) +  
  theme(  
    plot.background = element_blank(),  
    panel.grid = element_blank(),  
    panel.background = element_blank(),  
    panel.border = element_rect(color = "black", linewidth = 1, fill = NA),  
    axis.line = element_blank(),  
    axis.ticks = element_line(color = "black", size = 0.5),  
    axis.ticks.length = unit(0.05, "cm"),  
    axis.text = element_text(color = "black", size = 8, family = "Arial", face = "plain"),  
    axis.title.x = element_text(color = "black", size = 11, family = "Arial", face = "plain"), 
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    plot.title = element_blank(),
    plot.margin = margin(0.5, 0.2, 0.5, 0, 'cm'))  
p2  

BRCA_Match_DEG <- read.csv("Spleen_diffexpre.csv")
BRCA_Match_DEG$logP <- -log(BRCA_Match_DEG$padj, base = 10)
head(BRCA_Match_DEG)
BRCA_Match_DEG$label <- ifelse(BRCA_Match_DEG$logP > 1.3 & abs(BRCA_Match_DEG$log2FoldChange) > 0.263034406, BRCA_Match_DEG$gene, "")
p3 <- ggplot(BRCA_Match_DEG, aes(x = log2FoldChange, y = logP, fill = group)) +  
  geom_point(aes(color = group), shape = 21, alpha = 1, size = 2.5, stroke = 0.5, show.legend = TRUE) +  
  geom_hline(yintercept = 9.5, lty = 2, col = "grey", lwd = 0.5) +  
  geom_hline(yintercept = 1.3, lty = 2, col = "black", lwd = 0.5) +  
  geom_vline(xintercept = c(-0.263034406, 0.263034406), lty = 2, col = "black", lwd = 0.5) +  
  annotate("text", x = -3.8, y = 10, label = "DOWN", color = "black", hjust = 0, size = 3, family = "Arial") +  
  annotate("text", x = 2.6, y = 10, label = "UP", color = "black", hjust = 0, size = 3, family = "Arial") +  
  scale_fill_manual(values = c("UP" = "#EE7E77", "Down" = "#68A7BE", "not_change" = "gray"),   
                    labels = c("UP" = "Up-regulated", "Down" = "Down-regulated", "not_change" = "not_change")) +  
  scale_color_manual(values = c("UP" = "#9F0000", "Down" = "#003A75", "not_change" = "gray"),   
                     labels = c("UP" = "Up-regulated", "Down" = "Down-regulated", "not_change" = "not_change")) +  
  xlim(c(-5, 5)) + ylim(c(0, 10)) +  
  labs(x = "log2(FC,L/H)", y = "-lg(p-adj)") + 
  theme_prism(border = TRUE) +  
  theme(  
    plot.background = element_blank(),  
    panel.grid = element_blank(),  
    panel.background = element_blank(),  
    panel.border = element_rect(color = "black", linewidth = 1, fill = NA),  
    axis.line = element_blank(),  
    axis.ticks = element_line(color = "black", size = 0.5),  
    axis.ticks.length = unit(0.05, "cm"),  
    axis.text = element_text(color = "black", size = 8, family = "Arial", face = "plain"),  
    axis.title.x = element_text(color = "black", size = 11, family = "Arial", face = "plain"),   
    axis.title.y = element_blank(),   
    axis.text.y = element_blank(),   
    axis.ticks.y = element_blank(),   
    plot.title = element_blank(),  
    legend.position = "right",
    plot.margin = margin(0.5, 0.5, 0.5, 0, 'cm'))  
p3

p<-p1+p2+p3
p

## eGene Volcano plot 
BRCA_Match_DEG <- read.csv("Heart_diffexpre.csv")
BRCA_Match_DEG$logP <- -log(BRCA_Match_DEG$padj, base = 10)
head(BRCA_Match_DEG)
BRCA_Match_DEG$label <- ifelse(BRCA_Match_DEG$logP > 1.3 & abs(BRCA_Match_DEG$log2FoldChange) > 0.263034406, BRCA_Match_DEG$gene, "") 
p1 <- ggplot(BRCA_Match_DEG, aes(x = log2FoldChange, y = logP, fill = group)) +  
  geom_point(aes(color = group), shape = 21, alpha = 1, size = 2.5, stroke = 0.5, show.legend = FALSE) +  
  geom_hline(yintercept = 9.5, lty = 2, col = "grey", lwd = 0.5) +  
  geom_hline(yintercept = 1.3, lty = 2, col = "black", lwd = 0.5) +  
  geom_vline(xintercept = c(-0.263034406, 0.263034406), lty = 2, col = "black", lwd = 0.5) +  
  annotate("text", x = -3.8, y = 10, label = "DOWN", color = "black", hjust = 0, size = 3, family = "Arial") +  
  annotate("text", x = 2.6, y = 10, label = "UP", color = "black", hjust = 0, size = 3, family = "Arial") +  
  scale_fill_manual(values = c("UP" = "#EE7E77", "Down" = "#68A7BE", "not_change" = "gray")) +  
  scale_color_manual(values = c("UP" = "#9F0000", "Down" = "#003A75", "not_change" = "gray")) +  
  xlim(c(-5, 5)) + ylim(c(0, 10)) +  
  labs(x = "log2(FC,L/H)", y = "-lg(p-adj)") +   
  theme_prism(border = TRUE) +  
  theme(  
    plot.background = element_blank(),  
    panel.grid = element_blank(),  
    panel.background = element_blank(),  
    panel.border = element_rect(color = "black", linewidth = 1, fill = NA),  
    axis.line = element_blank(),  
    axis.ticks = element_line(color = "black", size = 0.5),  
    axis.ticks.length = unit(0.05, "cm"),  
    axis.text = element_text(color = "black", size = 8, family = "Arial", face = "plain"),  
    axis.title = element_text(color = "black", size = 11, family = "Arial", face = "plain"),  
    plot.title = element_text(color = "black", size = 7, family = "Arial", face = "plain"),
    plot.margin = margin(0.5, 0.2, 0.5, 0.5, 'cm'))  
p1  

BRCA_Match_DEG <- read.csv("Liver_diffexpre.csv")
BRCA_Match_DEG$logP <- -log(BRCA_Match_DEG$padj, base = 10)
head(BRCA_Match_DEG)
BRCA_Match_DEG$label <- ifelse(BRCA_Match_DEG$logP > 1.3 & abs(BRCA_Match_DEG$log2FoldChange) > 0.263034406, BRCA_Match_DEG$gene, "")
p2<- ggplot(BRCA_Match_DEG, aes(x = log2FoldChange, y = logP, fill = group)) +  
  geom_point(aes(color = group), shape = 21, alpha = 1, size = 2.5, stroke = 0.5, show.legend = FALSE) +  
  geom_hline(yintercept = 9.5, lty = 2, col = "grey", lwd = 0.5) +  
  geom_hline(yintercept = 1.3, lty = 2, col = "black", lwd = 0.5) +  
  geom_vline(xintercept = c(-0.263034406, 0.263034406), lty = 2, col = "black", lwd = 0.5) +  
  annotate("text", x = -3.8, y = 10, label = "DOWN", color = "black", hjust = 0, size = 3, family = "Arial") +  
  annotate("text", x = 2.6, y = 10, label = "UP", color = "black", hjust = 0, size = 3, family = "Arial") +  
  scale_fill_manual(values = c("UP" = "#EE7E77", "Down" = "#68A7BE", "not_change" = "gray")) +  
  scale_color_manual(values = c("UP" = "#9F0000", "Down" = "#003A75", "not_change" = "gray")) +  
  xlim(c(-5, 5)) + ylim(c(0, 10)) +  
  labs(x = "log2(FC,L/H)", y = "-lg(p-adj)") +   
  theme_prism(border = TRUE) +  
  theme(  
    plot.background = element_blank(),  
    panel.grid = element_blank(),  
    panel.background = element_blank(),  
    panel.border = element_rect(color = "black", linewidth = 1, fill = NA),  
    axis.line = element_blank(),  
    axis.ticks = element_line(color = "black", size = 0.5),  
    axis.ticks.length = unit(0.05, "cm"),  
    axis.text = element_text(color = "black", size = 8, family = "Arial", face = "plain"),  
    axis.title.x = element_text(color = "black", size = 11, family = "Arial", face = "plain"), 
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    plot.title = element_blank(),
    plot.margin = margin(0.5, 0.2, 0.5, 0, 'cm'))  

p2  

BRCA_Match_DEG <- read.csv("Spleen_diffexpre.csv")
BRCA_Match_DEG$logP <- -log(BRCA_Match_DEG$padj, base = 10)
head(BRCA_Match_DEG)
BRCA_Match_DEG$label <- ifelse(BRCA_Match_DEG$logP > 1.3 & abs(BRCA_Match_DEG$log2FoldChange) > 0.263034406, BRCA_Match_DEG$gene, "")
p3 <- ggplot(BRCA_Match_DEG, aes(x = log2FoldChange, y = logP, fill = group)) +  
  geom_point(aes(color = group), shape = 21, alpha = 1, size = 2.5, stroke = 0.5, show.legend = TRUE) +  
  geom_hline(yintercept = 9.5, lty = 2, col = "grey", lwd = 0.5) +  
  geom_hline(yintercept = 1.3, lty = 2, col = "black", lwd = 0.5) +  
  geom_vline(xintercept = c(-0.263034406, 0.263034406), lty = 2, col = "black", lwd = 0.5) +  
  annotate("text", x = -3.8, y = 10, label = "DOWN", color = "black", hjust = 0, size = 3, family = "Arial") +  
  annotate("text", x = 2.6, y = 10, label = "UP", color = "black", hjust = 0, size = 3, family = "Arial") +  
  scale_fill_manual(values = c("UP" = "#EE7E77", "Down" = "#68A7BE", "not_change" = "gray"),   
                    labels = c("UP" = "Up-regulated", "Down" = "Down-regulated", "not_change" = "not_change")) +  
  scale_color_manual(values = c("UP" = "#9F0000", "Down" = "#003A75", "not_change" = "gray"),   
                     labels = c("UP" = "Up-regulated", "Down" = "Down-regulated", "not_change" = "not_change")) +  
  xlim(c(-5, 5)) + ylim(c(0, 10)) +  
  labs(x = "log2(FC,L/H)", y = "-lg(p-adj)") + 
  theme_prism(border = TRUE) +  
  theme(  
    plot.background = element_blank(),  
    panel.grid = element_blank(),  
    panel.background = element_blank(),  
    panel.border = element_rect(color = "black", linewidth = 1, fill = NA),  
    axis.line = element_blank(),  
    axis.ticks = element_line(color = "black", size = 0.5),  
    axis.ticks.length = unit(0.05, "cm"),  
    axis.text = element_text(color = "black", size = 8, family = "Arial", face = "plain"),  
    axis.title.x = element_text(color = "black", size = 11, family = "Arial", face = "plain"),   
    axis.title.y = element_blank(),   
    axis.text.y = element_blank(),   
    axis.ticks.y = element_blank(),   
    plot.title = element_blank(),  
    legend.position = "right",
    plot.margin = margin(0.5, 0.5, 0.5, 0, 'cm'))  
p3

p<-p1+p2+p3
p

## Visualization of Differential Gene Counts  
library(reshape2)  
library(ggplot2)  
library(ggprism)  
library(plyr)  

type1 = read.table('gene.txt', header = TRUE)  
head(type1)  
type_colors <- c("UP-regulated" = "#EE7E77", "Down-regulated" = "#68A7BE")  
type_order <- c("Down-regulated", "UP-regulated")   
type_order <- rev(type_order)  
type1$type <- factor(type1$type, levels = type_order)  
group_order <- c("Heart", "Spleen", "Liver")  
type1$group <- factor(type1$group, levels = group_order)    
p2 <- ggplot(type1, aes(x = group, y = value, fill = type)) +  
  geom_bar(stat = "identity", color = "black", size = 0.4, position = "stack", width = 0.6) +  
  scale_fill_manual(values = type_colors) + 
  labs(fill = "Type")+
  theme( 
    legend.position = "top", 
    axis.text.x = element_text(color = 'black', size = 8),  
    axis.text.y = element_text(color = 'black', size = 8),  
    axis.title = element_text(color = 'black', size = 11),  
    panel.background = element_rect(fill = "white"),  
    axis.line = element_line(color = "black", size = 0.5),  
    axis.ticks = element_line(color = "black", size = 0.5, lineend = "round"),  
    plot.margin = margin(1, 1, 1, 1, 'cm')) +  
  ylab("DEGs Ratio(‰)") +   
  xlab("Tissue") +  
  scale_y_continuous(name = "DEGs Ratio(‰)", breaks = seq(0, 1.5, by = 0.5), limits = c(-0.01, 1.51))  
p2  

type1 = read.table('egene.txt', header = TRUE)  
head(type1)  
type_colors <- c("UP-regulated" = "#EE7E77", "Down-regulated" = "#68A7BE") 
type_order <- c("Down-regulated", "UP-regulated")  
type_order <- rev(type_order)  
type1$type <- factor(type1$type, levels = type_order)  
group_order <- c("Heart", "Spleen", "Liver")  
type1$group <- factor(type1$group, levels = group_order)  
p2 <- ggplot(type1, aes(x = group, y = value, fill = type)) +  
  geom_bar(stat = "identity", color = "black", size = 0.4, position = "stack", width = 0.6) +  
  scale_fill_manual(values = type_colors) + 
  labs(fill = "Type")+
  theme( 
    legend.position = "top", 
    axis.text.x = element_text(color = 'black', size = 8),  
    axis.text.y = element_text(color = 'black', size = 8),  
    axis.title = element_text(color = 'black', size = 11),  
    panel.background = element_rect(fill = "white"),  
    axis.line = element_line(color = "black", size = 0.5),  
    axis.ticks = element_line(color = "black", size = 0.5, lineend = "round"),  
    plot.margin = margin(1, 1, 1, 1, 'cm')) +   
  ylab("DEGs Ratio(‰)") +   
  xlab("Tissue") +  
  scale_y_continuous(name = "DEGs Ratio(‰)", breaks = seq(0, 2.5, by = 0.5), limits = c(-0.01, 2.51))  
p2  