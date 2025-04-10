## part4 code
## Biomarker identified by LASSO regression 
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

## Correlation between metabolites of brown module and IFN
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

## Brown module metabolite coexpression network visualization
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


## One of the most significantly related metabolites is described in the Brown module
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
data<-read_excel('LASSOBrownTOP20.xlsx') 
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
           axis.text.x = element_text(size = 10, colour = 'black', angle = 45, hjust = 1),   
           axis.line.x = element_blank(),   
           axis.ticks.length = unit(0.15, "cm"),   
           panel.background = element_rect(fill = "white"),   
           panel.grid.major.y = element_blank(),   
           panel.grid.minor.y = element_blank(),   
           panel.grid.minor.x = element_blank(),  
           legend.position = "none",  
           plot.margin = unit(c(1, 1, 1, 1), 'cm'))  
p1 


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
      geom_text(data = subset(data,   Metabolite %in% c("Butyric acid", "L-Cysteine")),  
                aes(label =Metabolite),   
                vjust = -1,  
                size = 3,    
                color = "black")+   
      guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL))
p1  


## Calculation of metabolite AUC value
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
mean_data <- data %>%  
  group_by(Group) %>%  
  summarise(mean_ROC = mean(ROC, na.rm = TRUE))  
data <- read.csv('MetaboliteAUC.csv',header = T)
mean_data <- data %>%  
  group_by(Group) %>%  
  summarise(mean_ROC = mean(ROC, na.rm = TRUE))  

p1 <- ggplot(data, aes(x = Group, y = ROC, fill = Group)) +
  geom_boxplot(width = 0.5,color = "black", size = 0.5) +  
  scale_fill_manual(values = c("#FEDFB2", "#68A7BE","#EE7E77")) + 
  labs(x = '', y = 'AUC') +
  ylim(0, 1) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 11,  color = 'black'),
    axis.text.x = element_text(size = 8, color = 'black', angle = 45, hjust = 1),
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


## biomarker features description
library(ggpubr)
library(ggplot2)  
library(dplyr)  
data <- read.csv('16metafeature_fenmian.csv', header = TRUE)  
highlight_data <- data.frame(  
  Class = c("Lipids and lipid-like molecules", "Organic acids and derivatives", "Organoheterocyclic compounds",  
            "Lipids and lipid-like molecules", "Organic acids and derivatives", "Organoheterocyclic compounds",  
            "Lipids and lipid-like molecules", "Organic acids and derivatives", "Organoheterocyclic compounds"),  
  Count = c(1.05, 1.02, 1.05, 0.87, 0.84, 0.82, 0.56, 0.54, 0.64),  
  Type = c("Fold Change", "Fold Change", "Fold Change",   
           "Variable Important in Project", "Variable Important in Project", "Variable Important in Project",  
           "Coefficient of Variation", "Coefficient of Variation", "Coefficient of Variation"))  

class_colors <- c("Lipids and lipid-like molecules" = "#9CD1C7",   
                  "Organic acids and derivatives" = "#BEBAD8",   
                  "Organoheterocyclic compounds" = "#8AB0D0")  
desired_order <- c("Lipids and lipid-like molecules","Organic acids and derivatives","Organoheterocyclic compounds")  
data$Class<- factor(data$Class, levels = desired_order)  

p1 <- ggplot(data, aes(x = Class, y = Count)) +  
  geom_boxplot(width = 0.4, size = 0.6, color = "#87B7D7") +   
  labs(x = '', y = 'Value') +  
  theme_classic() +  
  theme(  
    axis.title = element_text(size = 11, color = 'black'),  
    axis.text.y = element_markdown(color = 'black', size = 7, hjust = 1), 
    axis.text.x = element_markdown(size = 10, color = 'black'),  
    axis.ticks = element_line(color = "black"),  
    plot.title = element_text(hjust = 0.5),  
    plot.margin = unit(c(1, 1, 1, 1), 'cm'),    
    panel.background = element_blank(),   
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    strip.background = element_rect(fill = "lightgray", color = "black"),  
    panel.spacing = unit(0.1, "cm")  
  ) +  
  scale_x_discrete(labels = function(x) {  
    color_values <- class_colors[x]  
    paste0("<span style='font-size:27pt; color:", color_values, "; border: 2px solid black;'>&#9679;</span>")})+  
  coord_flip() +  
  facet_wrap(~ Type) +  
  geom_point(data = highlight_data, aes(x = Class, y = Count), color = "#D24542", size = 2, shape = 21, fill = "#D24542") 
p1 


## Correlation between biomarkers
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


## Calculation and visualization of MAF
/home/cgbl/biosoft/plink1.9/plink --allow-extra-chr --chr-set 35 --bfile /home/phliu/17metaGWAS/L/L --freq --out /home/phliu/17metaGWAS/MAF/L_freq
/home/cgbl/biosoft/plink1.9/plink --allow-extra-chr --chr-set 35 --bfile /home/phliu/17metaGWAS/H/H --freq --out /home/phliu/17metaGWAS/MAF/H_freq
/home/cgbl/biosoft/plink1.9/plink --allow-extra-chr --chr-set 35 --bfile /home/phliu/17metaGWAS/F/F --freq --out /home/phliu/17metaGWAS/MAF/F_freq
/home/cgbl/biosoft/plink1.9/plink --allow-extra-chr --chr-set 35 --bfile /home/phliu/17metaGWAS/M/M --freq --out /home/phliu/17metaGWAS/MAF/M_freq
export PATH=$PATH:/usr/lib64/R/bin 
Rscript MAFtiqu.r
python MAFsummary.py
generate Fmaf_summary.txt、Mmaf_summary.txt、Lmaf_summary.txt、Hmaf_summary.txt

## results visualization
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra) 
library(RColorBrewer)
install.packages("coin")
library(coin)

colors <- colorRampPalette(brewer.pal(12, "Set3"))(16)
data<-read.csv("newFMmaf.csv")
head(data)
spearman_test_result <- spearman_test(FMAF ~ MMAF, data = data, distribution = approximate(nresample= 9999))
spearman_test_result
#p-value < 1e-04
spearman_corr <- cor(data$FMAF, data$MMAF, method = "pearson")
print(spearman_corr)
#0.8087298
spearman_test <- cor.test(data$FMAF, data$MMAF, method = "pearson")
# cor 0.8087298  p-value < 2.2e-16
p1 <- ggplot(data, aes(x = FMAF, y = MMAF, color = newMetabolite, fill = newMetabolite)) +  
  geom_point(shape = 21, size = 2) +  
  scale_color_manual(values = colors) +   
  scale_fill_manual(values = colors) +  
  xlab("MAF of L Group") +   
  ylab("MAF of H Group") +   
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
  guides(color = guide_legend(ncol = 2),  
         fill = guide_legend(ncol = 2)) +  
  labs(color = "Metabolites", fill = "Metabolites") +  
  geom_abline(slope = 1, intercept = 0, color = "#060B6F", linetype = "dashed", size = 0.5) +  
  ggtitle(expression("Pearson's r = 0.81, " * italic(P) * "< 2.2e-16")) 
p1  


data<-read.csv("newHLmaf.csv")
head(data)
spearman_test_result <- spearman_test(LMAF ~ HMAF, data = data, distribution = approximate(nresample= 9999))
spearman_test_result
#p-value < 1e-04
spearman_corr <- cor(data$LMAF, data$HMAF, method = "pearson")
print(spearman_corr)
#0.8253212
spearman_test <- cor.test(data$LMAF, data$HMAF, method = "pearson")
# cor 0.8253212 p-value < 2.2e-16
p1 <- ggplot(data, aes(x = LMAF, y = HMAF, color = newMetabolite, fill = newMetabolite)) +
  geom_point(shape = 21, size = 2) +
  scale_color_manual(values = colors) + 
  scale_fill_manual(values = colors) +  
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
  guides(color = guide_legend(ncol = 2),  
         fill = guide_legend(ncol = 2)) +  
  labs(color = "Metabolites", fill = "Metabolites") +  
  geom_abline(slope = 1, intercept = 0, color = "#060B6F", linetype = "dashed", size = 0.5) +  
  ggtitle(expression("Pearson's r = 0.83, " * italic(P) * "< 2.2e-16")) 
p1  


## estimate heritability
#GCTA

## Comparison of heritability in L group gradient population
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

data <- read.csv('L.csv',header = T)
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


## results viaualization
data <- read.csv('HLFM.csv',header = T)
head(data)
library(introdataviz) 
library(rstatix)
library(ggpubr)
library(ggnewscale)
data$Generation <- factor(data$Generation, levels = c("Row Group", "Adjust Group"))
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
                  legend.title=element_text(color="black",size=7),
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))

p1 <- ggplot(data, aes(x = Generation, y = Heritability, fill = newGroup)) +
  geom_split_violin(trim = TRUE, width = 0.8, color = "black", linewidth = 0.4, adjust = 1) + 
  geom_boxplot(aes(group = interaction(Generation, newGroup)),  
               width = 0.15, color = "black", fill ='white', outlier.shape = NA, 
               position = position_dodge(width = 0.2)) + 
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, aes(fill = newGroup),color = "black", position = position_dodge(width = 0.2)) +  
  scale_fill_manual(values = c("#EE7E77","#68A7BE","#EE7E77","#68A7BE")) +  
  labs(x = '', y = "Heritability") +  
  plot.format +
  theme(legend.position = "right") +
  geom_hline(yintercept = 0.5, color = "#b53289", linetype = "dashed", size = 0.5)+
  ylim(0, 1.05)+
  stat_compare_means(aes(group=Group),method = "wilcox.test",label="p.signif",label.y = c(0.97,0.97))
p1


## Heritability of different QTL sets
/home/cgbl/biosoft/gcta/gcta_1.93.2beta/gcta64 --bfile /home/phliu/17metaheritability/peihao --autosome-num 35 --autosome  --make-grm-alg 1  --thread-num 30 --out allloci
cov.txt
#run allGCTA_heritability.sh，calculate h2 of target loci
python Heritability_summary.py
python allHeritability_summary-VG.py

/home/cgbl/biosoft/gcta/gcta_1.93.2beta/gcta64 --bfile meta1595loci --autosome-num 35 --autosome  --make-grm-alg 1  --thread-num 30 --out meta1595grm
awk 'NR > 1 {print $1, $5}' meta_meta832.txt > meta832phe.txt
cov.txt
/home/cgbl/biosoft/gcta/gcta_1.93.2beta/gcta64 --grm meta1595grm --pheno meta1595phe.txt --mpheno 1 --qcovar cov.txt --reml --thread-num 10 --out meta1595
python Heritability_summary.py
python Heritability_summary-VG.py

1.bash newextract.sh
2.significantextractloci.sh
newGCTAgrm.sh
newGCTA_heritability.sh
python newHeritability_summary.py
python newHeritability_summary-VG.py

## results visualization
library(ggplot2)
library(introdataviz) 
library(rstatix)
library(ggpubr)
library(ggnewscale)
plot.format=theme(plot.background=element_blank(),
                  panel.grid=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_rect(color="black",linewidth=1,fill=NA),
                  axis.line=element_blank(),
                  axis.ticks=element_line(color="black",linewidth=0.5),
                  axis.text=element_text(color="black",size=8),
                  axis.title=element_text(color="black",size=11),
                  plot.title=element_text(color="black",size=7),
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.text=element_text(color="black",size=7),
                  legend.title=element_text(color="black",size=7))
data <- read.csv('HeritabilityPorportion.csv',header = T)
head(data)

## all metabolites
data$Group <- factor(data$Type, levels = c("Significant QTL", "Suggestive QTL", "All QTL"))
p1 <- ggplot(data, aes(x = Metabolites, y = h, fill = Group)) +
      geom_point(size = 2.5, aes(color = Group), position = position_dodge(width = 0.4)) +  
      geom_errorbar(aes(ymin = h - Var, ymax = h + Var, color = Group), width = 0, cex = 0.7, position = position_dodge(width = 0.4))+
      scale_color_manual(values = c("#EE7E77", "#68A7BE", "#FEDFB2")) + 
      labs(x = '', y = 'Heritability') +
      theme_classic(base_family = "Arial") +
      theme(plot.margin = margin(1, 1, 0.5, 1, 'cm'),
         axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "right") +
         scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, by = 0.2)) +
         plot.format+
         geom_hline(yintercept = 1, color = "#b53289", linetype = "dashed", size = 0.5)+
         geom_hline(yintercept = 0, color = "#b53289", linetype = "dashed", size = 0.5)
p1

## compare
plot.format1=theme(plot.background=element_blank(),
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
                  legend.title=element_text(color="black",size=7),
                   plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))
data$Type <- factor(data$Type, levels = c("Significant QTL", "Suggestive QTL", "All QTL"))
p1 <- ggplot(data, aes(x = Type, y = h, color = Type)) +
  geom_violin(aes(fill = Type), width = 0.7, size = 0.5, color = "black") +  
  geom_boxplot(width = 0.15, outlier.shape = NA, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, aes(fill = Type),color = "black") + 
  scale_color_manual(values = c("#EE7E77", "#68A7BE", "#FEDFB2")) + 
  scale_fill_manual(values = c("#EE7E77", "#68A7BE", "#FEDFB2")) + 
  labs(x = '', y = 'Heritability')  +
  ylim(0,1.05)+
  plot.format1 +
  theme(legend.position = "none")
p1

## V(G)
data <- read.csv('VG.csv',header = T)
head(data)
data$Type <- factor(data$Type, levels = c("Significant QTL", "Suggestive QTL", "All QTL"))
p1 <- ggplot(data, aes(x = Type, y = VG, color = Type)) +
  geom_violin(aes(fill = Type), width = 0.7, size = 0.5, color = "black") +  
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, aes(fill = Type),color = "black") + 
  scale_color_manual(values = c("#EE7E77", "#68A7BE", "#FEDFB2")) + 
  scale_fill_manual(values = c("#EE7E77", "#68A7BE", "#FEDFB2")) + 
  labs(x = '', y = ' V(G)' )  +
  plot.format1 +
  theme(legend.position = "none")
p1


## Variance component partitioning
Rscript Variance_FM.r
Rscript Variance_HLC.r
Python Variance_summary.py，generate HLC_summary_variance.csv and FM_summary_variance.csv 

Variance_FM.r：
library(lme4)
library(reshape2)
file_prefixes <- c(
  "meta1595", "meta466", "meta1374", "meta1533", 
  "meta1238", "meta1164", "meta1032", "meta1793", "meta1306",
  "meta1476", "meta1384", "meta832", "meta385", "meta1441",
  "meta1731", "meta1801"
)
for (prefix in file_prefixes) {
  metabo_intensities <- read.csv(paste0(prefix, ".csv"))
  cat(paste0(prefix, ".csv"), "\n")
  sample_info <- read.csv(paste0(prefix, "_features.csv"))
  cat(paste0(prefix, "_features.csv"), "\n")
  sample_info$ID <- as.character(sample_info$ID)
  metabo_intensities$ID <- as.character(metabo_intensities$ID)
  sample_info <- merge(sample_info, metabo_intensities, by="ID")
  sample_info$Group <- as.factor(sample_info$Group)
  sample_info$QTL1 <- as.factor(sample_info$QTL1)
  model <- lm(Intensity ~ Group + QTL1 + BW, data = sample_info)
  anova_table <- anova(model)
  print(anova_table) 
  total_ss <- sum(anova_table$"Sum Sq")
  cat("(Total Sum of Squares):", total_ss, "\n")  
  variance_components <- data.frame(
    Effect = c("Group", "QTL1", "BW", "Residuals"),
    Variance_Contribution = numeric(4) 
  )
  for (effect in c("Group", "QTL1", "BW")) {
    if (effect %in% rownames(anova_table)) {
      sum_sq <- anova_table[effect, "Sum Sq"]
      variance_components[variance_components$Effect == effect, "Variance_Contribution"] <- round(sum_sq / total_ss * 100, 4)
      cat(effect, sum_sq, variance_components$Variance_Contribution[variance_components$Effect == effect], "\n")
    } else {
      variance_components[variance_components$Effect == effect, "Variance_Contribution"] <- 0
    }
  }
  variance_components[variance_components$Effect == "Residuals", "Variance_Contribution"] <- round(anova_table["Residuals", "Sum Sq"] / total_ss * 100, 4)
  cat("Residuals sum of squares of variance:", anova_table["Residuals", "Sum Sq"], "contribution,", variance_components$Variance_Contribution[variance_components$Effect == "Residuals"], "\n")
  print(variance_components)
  output_file <- paste0(prefix, "_FM_variance.csv")
  write.csv(variance_components, output_file, row.names = FALSE)
  cat("Results have been saved to:", output_file, "\n")
}

## Results Visualization
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
                  axis.text=element_text(color="black",size=8),
                  axis.title=element_text(color="black",size=11),
                  plot.title=element_text(color="black",size=7),
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.text=element_text(color="black",size=7),
                  legend.title=element_text(color="black",size=7),
                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'))

data <- read.csv('FM_variance.csv',header = T)
head(data)
data$Type <- factor(data$Type, levels = c("Group", "Top QTL", "Body Weight", "Residuals"))
p1 <- ggplot(data, aes(x = Type, y =  Varianceexplained, color = Type)) +
  geom_violin(aes(fill = Type), width = 0.7, size = 0.5, color = "black") +  
  geom_boxplot(width = 0.15, outlier.shape = NA, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, aes(fill = Type),color = "black") + 
  scale_color_manual(values = c("#DEEEED", "#68A7BE", "#FEECE7", "#EE7E77")) + 
  scale_fill_manual(values = c("#DEEEED", "#68A7BE", "#FEECE7", "#EE7E77")) + 
  labs(x = '', y = 'Variance Explained(%)')  +
  plot.format +
  theme( axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")+  
  facet_zoom(ylim = c(0, 20), zoom.size = 1) 
p1

data <- read.csv('HLC_variance.csv',header = T)
head(data)
data$Type <- factor(data$Type, levels = c("Group", "Top QTL", "Body Weight", "Residuals"))
p2 <- ggplot(data, aes(x = Type, y =  Varianceexplained, color = Type)) +
  geom_violin(aes(fill = Type), width = 0.7, size = 0.5, color = "black") +  
  geom_boxplot(width = 0.15, outlier.shape = NA, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, aes(fill = Type),color = "black") + 
  scale_color_manual(values = c("#DEEEED", "#68A7BE", "#FEECE7", "#EE7E77")) + 
  scale_fill_manual(values = c("#DEEEED", "#68A7BE", "#FEECE7", "#EE7E77")) + 
  labs(x = '', y = 'Variance Explained(%)')  +
  plot.format +
  theme( axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")+  
  facet_zoom(ylim = c(0, 20), zoom.size = 1) 
p2

## Faceted histogram of variance explanation rate for each metabolite
plot.format=theme(plot.background=element_blank(),
                  panel.grid=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_rect(color="black",linewidth=1,fill=NA),
                  axis.line=element_blank(),
                  axis.ticks=element_line(color="black",linewidth=0.5),
                  axis.text=element_text(color="black",size=8),
                  axis.title=element_text(color="black",size=11),
                  plot.title=element_text(color="black",size=7),
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.text=element_text(color="black",size=7),
                  legend.title=element_text(color="black",size=7))
library(gcookbook)
library(ggplot2)
library(scales)

data <- read.csv('FM_variance_bardata.csv',header = T)
head(data)
data$Type <- factor(data$Type, levels = c("Group", "Top QTL", "Body Weight", "Residuals"))
p1 <- ggplot(data, aes(x = Type , y = Number,color = Type)) + 
  geom_col(position = "dodge", aes(fill = Type), color="black",size=0.25,width = 0.75) + 
  scale_color_manual(values = c("#DEEEED", "#68A7BE", "#FEECE7", "#EE7E77")) +  
  scale_fill_manual(values = c("#DEEEED", "#68A7BE", "#FEECE7", "#EE7E77")) +  
  labs(x = '', y = 'Variance Explained(%)') + 
  facet_wrap(~Metabolites) +
  plot.format+  
  theme( axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none")+
  coord_cartesian(ylim = c(0, 100))
p1

data <- read.csv('HLC_variance_bardata.csv',header = T)
head(data)
data$Type <- factor(data$Type, levels = c("Group", "Top QTL", "Body Weight", "Residuals"))
p1 <- ggplot(data, aes(x = Type , y = Number,color = Type)) + 
  geom_col(position = "dodge", aes(fill = Type), color="black",size=0.25,width = 0.75) + 
  scale_color_manual(values = c("#DEEEED", "#68A7BE", "#FEECE7", "#EE7E77")) +  
  scale_fill_manual(values = c("#DEEEED", "#68A7BE", "#FEECE7", "#EE7E77")) +  
  labs(x = '', y = 'Variance Explained(%)') + 
  facet_wrap(~Metabolites) +
  plot.format+  
  theme( axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none")+
  coord_cartesian(ylim = c(0, 100))
p1

## Test of Group effect
## Genotype Data Processing
awk -F'[[:space:]]+' '$12 < 1.69E-06 {print $1, $2}' meta1031.txt > meta1031loci.txt  
/home/cgbl/biosoft/plink1.9/plink --allow-extra-chr --chr-set 35 --bfile /home/phliu/17metaGWAS/peihao1100 --extract meta1031loci.txt --make-bed --out meta1031loci  
/home/cgbl/biosoft/plink1.9/plink --chr-set 35 --allow-extra-chr --bfile meta1031loci --recode --out meta1031loci_1  

## Analysis in R
library(base)  
library(stats)  
library(utils)  
# Set the random seed for reproducibility  
set.seed(123)  
# Read data  
meta1031 <- read.csv("meta1031.csv", header = TRUE, stringsAsFactors = FALSE)  
BW <- read.csv("BW.csv", header = TRUE, stringsAsFactors = FALSE)  
Group <- read.csv("Group.csv", header = TRUE, stringsAsFactors = FALSE)  
QTL <- read.table("meta1031numeric.txt", header = TRUE, stringsAsFactors = FALSE)  
# Check data  
head(meta1031)  
head(BW)  
head(Group)  
head(QTL)  
library(broom) # Used to extract p-values from model summaries  
# Merge data frames  
data <- merge(meta1031, BW, by = "ID")  
data <- merge(data, Group, by = "ID")  
data <- merge(data, QTL, by = "ID")  
head(data)  
# Dynamically get QTL column names  
QTL_columns <- grep("^QTL", names(data), value = TRUE)  
# Build the first model Intensity ~ Body + QTL  
# Construct the model formula  
formula <- as.formula(paste("Intensity ~ BW +", paste(QTL_columns, collapse = " + ")))  
# Build the linear model  
modelA <- lm(formula, data = data)  
summary(modelA)  
# Build the second model: Intensity ~ Body + QTL + Group  
# Construct the model formula  
formulb <- as.formula(paste("Intensity ~ BW + Group + ", paste(QTL_columns, collapse = " + ")))  
# Build the linear model  
modelB <- lm(formulb, data = data)  
summary(modelB)  
# Use ANOVA to compare the two models  
anova_result <- anova(modelA, modelB, test="F")  
print(anova_result)  
# Save results  
sink("meta1031_results.txt")  
cat("ANOVA Result:\n")  
print(anova_result)  
cat("\n\n")  
cat("Model A Summary:\n")  
print(summary(modelA))  
cat("\n\n")  
cat("Model B Summary:\n")  
print(summary(modelB))  
sink() 

## Results visualization
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra) 

## Comparison of P-values
data<-read.csv("FM.csv")
head(data)
p1 <- ggplot(data, aes(x = NO_Group, y = Group, color = group, fill = group)) +
      geom_point(shape = 21, size = 2) + 
      scale_color_manual(values = c("#EE7E77","#68A7BE")) + 
      scale_fill_manual(values = c("#EE7E77","#68A7BE")) +  
      xlab(expression("Model 1: -log"[10]*"(p-value)")) +   
      ylab(expression("Model 2: -log"[10]*"(p-value)")) +  
      theme(
        axis.title = element_text( size = 11, colour = 'black'),
        axis.text = element_text(size = 8, colour = 'black'), 
        axis.line = element_line(size = 0.5, colour = 'black'),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) + 
      scale_x_continuous(limits = c(0, 51)) +  
      scale_y_continuous(limits = c(0, 51))+
      labs(color = "ANOVA", fill = "ANOVA")+
      geom_abline(slope = 1, intercept = 0, color = "#999D26", linetype = "dotted", size = 0.5) 
p1

data<-read.csv("HL.csv")
head(data) 
p1 <- ggplot(data, aes(x = NO_Group, y = Group, color = group, fill = group)) +
      geom_point(shape = 21, size = 2) + 
      scale_color_manual(values = c("#EE7E77","#68A7BE")) + 
      scale_fill_manual(values = c("#EE7E77","#68A7BE")) +  
      xlab(expression("Model 1: -log"[10]*"(p-value)")) +   
      ylab(expression("Model 2: -log"[10]*"(p-value)")) +  
      theme(
        axis.title = element_text( size = 11, colour = 'black'),
        axis.text = element_text(size = 8, colour = 'black'), 
        axis.line = element_line(size = 0.5, colour = 'black'),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) + 
      scale_x_continuous(limits = c(0, 51)) +  
      scale_y_continuous(limits = c(0, 51))+
      labs(color = "ANOVA", fill = "ANOVA")+
      geom_abline(slope = 1, intercept = 0, color = "#999D26", linetype = "dotted", size = 0.5) 
p1

## Correlation analysis between Anova analysis and difference test P-value
install.packages("cowplot")
install.packages("ggpubr")
install.packages("gridExtra")
library(dplyr)  
library(ggplot2)
library(cowplot)  
library(ggpubr)  
library(gridExtra) 

data<-read.csv("FMcorreltaion.csv")
head(data)
p1 <- ggplot(data, aes(x = anovalgP, y = dmlgP)) +
      geom_point(aes(fill = VIP), shape = 21, size = 2) + 
      geom_smooth(method = "lm", color = "#860C1E", fill = "#F3DAE9", size = 1, se = TRUE) +  
      stat_cor(method = "pearson", label.x = 0, label.y = 10, label.sep = "\n") +
      scale_fill_gradient(low = "#A3C7E3", high = "#467DA7") + 
      xlab(expression("ANOVA:-log"[10]*"(p-value)")) + ylab(expression("DAMs:--log"[10]*"(p-value)")) +
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
      scale_x_continuous(limits = c(0, 20)) +  
      scale_y_continuous(limits = c(0, 20))
p1


data<-read.csv("HLcorreltaion.csv")
head(data)
p1 <- ggplot(data, aes(x = anovalgP, y = dmlgP)) +
      geom_point(aes(fill = VIP), shape = 21, size = 2) + 
      geom_smooth(method = "lm", color = "#860C1E", fill = "#F3DAE9", size = 1, se = TRUE) +  
      stat_cor(method = "pearson", label.x = 0, label.y = 10, label.sep = "\n") +
      scale_fill_gradient(low = "#A3C7E3", high = "#467DA7") + 
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
      scale_x_continuous(limits = c(0, 16)) +  
      scale_y_continuous(limits = c(0, 20))
p1


## Paired box plot of F statistics
library("tidyverse")
library("gghalves")
library("ggsci")
library("cowplot")
library("ggpubr")

data <- read.csv('HLC_Fstatistic.csv',header = T)
head(data)
data$Group <- factor(data$Group, levels = c("NO_Group", "Group"))
p1 <- ggplot(data, aes(x = Group, y = Fstatistic, fill = Group)) +
      geom_half_violin(data = filter(data,Group =='NO_Group'),side ='l',nudge =0.2,  color ='black', trim = TRUE)+
      geom_half_violin(data = filter(data,Group =='Group'),side ='r',nudge =0.2,  color ='black', trim = TRUE)+
      geom_line(aes(group = Metabolite),lwd = 0.5, color = "#CCB78D")+
      geom_point(aes(group = Group),size = 2, position = position_dodge(0.3),shape = 21)+
      geom_boxplot(aes(group = Group), width = 0.3, size = 0.7,alpha =0.4,color = 'black',outlier.shape = NA)+
      scale_fill_manual(values = c("#68A7BE", "#EE7E77")) + 
      labs(x = '', y = 'F-statistic') +
      theme_classic(base_family = "Arial") +
      scale_x_discrete(labels = c("NO_Group" = "Model1", "Group" = "Model2")) +
      theme(
            axis.text.x = element_text(color = 'black', size = 10),
            axis.text.y = element_text(color = 'black', size = 8),
            axis.title = element_text(color = 'black', size = 11),
            axis.line = element_line(color = "black", size = 0.6),
            axis.ticks = element_line(color = "black", size = 0.6, lineend = "round"),
            plot.margin = margin(1, 1, 0.5, 1, 'cm'),
            legend.position = "none") 
p1


data <- read.csv('FM_Fstatistic.csv',header = T)
head(data)
data$Group <- factor(data$Group, levels = c("NO_Group", "Group"))
p1 <- ggplot(data, aes(x = Group, y = Fstatistic, fill = Group)) +
      geom_half_violin(data = filter(data,Group =='NO_Group'),side ='l',nudge =0.2,  color ='black', trim = TRUE)+
      geom_half_violin(data = filter(data,Group =='Group'),side ='r',nudge =0.2,  color ='black', trim = TRUE)+
      geom_line(aes(group = Metabolite),lwd = 0.5, color = "#CCB78D")+
      geom_point(aes(group = Group),size = 2, position = position_dodge(0.3),shape = 21)+
      geom_boxplot(aes(group = Group), width = 0.3, size = 0.7,alpha =0.4,color = 'black',outlier.shape = NA)+
      scale_fill_manual(values = c("#68A7BE", "#EE7E77")) + 
      labs(x = '', y = 'F-statistic') +
      theme_classic(base_family = "Arial") +
      scale_x_discrete(labels = c("NO_Group" = "Model1", "Group" = "Model2")) +
      theme(
            axis.text.x = element_text(color = 'black', size = 10),
            axis.text.y = element_text(color = 'black', size = 8),
            axis.title = element_text(color = 'black', size = 11),
            axis.line = element_line(color = "black", size = 0.6),
            axis.ticks = element_line(color = "black", size = 0.6, lineend = "round"),
            plot.margin = margin(1, 1, 0.5, 1, 'cm'),
            legend.position = "none") 
p1

## Differences analysis of intensity of Hexyl glucoside（meta1595） with different genotype
awk '$12 < 1.6941E-06' meta1374.txt > locimeta1374.txt 
/home/cgbl/biosoft/plink1.9/plink --allow-extra-chr --chr-set 35 --bfile /home/phliu/mGWAS1-100/peihao1100 --extract locimeta1374.txt --make-bed --out meta1374loci
/home/cgbl/biosoft/plink1.9/plink  --chr-set 35 --allow-extra-chr  --bfile  meta1374loci --recode --out  meta1374loci_1
perl hebing.pl
python intensityaverage.py
python intensityaverage1.py

python SNPextract.py

## resuult visualization
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggsignif")
library(ggplot2)
library(ggpubr)
library(ggsignif)

df<-read.csv("meta1374loci.CSV")
head(df)
p <- ggplot(df, aes(X18_10290729, y = meta1374, fill = Group)) +  
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.7), alpha = 0.7,outlier.size = 0.5) +  
  scale_fill_manual(values = c("#EE7E77", "#68A7BE")) +  
  theme_minimal() +   
  ylim(8, 11) +  
  labs(x = "", y = "Intensity") +  
  theme(  
    axis.text.x = element_text(color = 'black', size = 10),  
    axis.text.y = element_text(color = 'black', size = 8),  
    axis.title = element_text(color = 'black', size = 11),  
    axis.line = element_blank(),  
    axis.ticks = element_line(color = "black", size = 0.6, lineend = "round"),  
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),  
    legend.position = "right",  
    legend.key.size = unit(0.5, "cm"), 
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    legend.title = element_text(size = 9),  
    legend.text = element_text(size = 8), 
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank()) +  
  stat_summary(fun = mean, aes(group = Group),geom = "line", position = position_dodge(width = 0.7), color = c("#9B0E27", "#9B0E27","#9B0E27","#001560","#001560","#001560"), size = 1) + 
  stat_summary(fun = mean, aes(group = Group), geom = "point", position = position_dodge(width = 0.7), color = c("#9B0E27", "#9B0E27","#9B0E27","#001560","#001560","#001560"), size = 2)   
p  

## Faceted histogram of the number of significant QTLS in the model
install.packages("gcookbook")
install.packages("ggplot2")
install.packages("scales")
library(gcookbook)
library(ggplot2)
library(scales)

plot.format=theme(plot.background=element_blank(),
                  panel.grid=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_rect(color="black",linewidth=1,fill=NA),
                  axis.line=element_blank(),
                  axis.ticks=element_line(color="black",linewidth=0.5),
                  axis.text=element_text(color="black",size=8),
                  axis.title=element_text(color="black",size=11),
                  plot.title=element_text(color="black",size=7),
                  legend.background=element_blank(),
                  legend.key=element_blank(),
                  legend.text=element_text(color="black",size=7),
                  legend.title=element_text(color="black",size=7))

data <- read.csv('HL_significant_counts_summary.csv',header = T)
head(data)
data$Type <- factor(data$Group, levels = c("H Group", "L Group", "H and L Group"))
p1 <- ggplot(data, aes(x = Type , y = Number,color = Type)) + 
  geom_col(position = "dodge", aes(fill = Type), color="black",size=0.25,width = 0.55) + 
  scale_color_manual(values = c("#68A7BE", "#EE7E77", "#FEDFB2")) +  
  scale_fill_manual(values = c("#68A7BE", "#EE7E77", "#FEDFB2")) +  
  labs(x = '', y = 'Significant QTL Numbers') + 
  facet_wrap(~Name) +
  plot.format+  
  theme( axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none")+
  coord_cartesian(ylim = c(0, 5))
p1

data <- read.csv('FM_significant_counts_summary.csv',header = T)
head(data)
data$Type <- factor(data$Group, levels = c("H Group", "L Group", "H and L Group"))
p1 <- ggplot(data, aes(x = Type , y = Number,color = Type)) + 
  geom_col(position = "dodge", aes(fill = Type), color="black",size=0.25,width = 0.55) + 
  scale_color_manual(values = c("#68A7BE", "#EE7E77", "#FEDFB2")) +  
  scale_fill_manual(values = c("#68A7BE", "#EE7E77", "#FEDFB2")) +  
  labs(x = '', y = 'Significant QTL Numbers') + 
  facet_wrap(~Name) +
  plot.format+  
  theme( axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none")+
  coord_cartesian(ylim = c(0, 20))
p1