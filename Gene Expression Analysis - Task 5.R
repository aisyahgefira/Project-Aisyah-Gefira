### BASIC CLUSTER ANALYSIS IN R ####
#Link: https://rstudio-pubs-static.s3.amazonaws.com/3773_0afaead59a02436889abc68753e6c20a.html


#Sample Data
source("http://bioconductor.org/biocLite.R")
biocLite("golubEsets")

#load require package and datasets
require(golubEsets)
data(Golub_Merge)
golub <- data.frame(Golub_Merge)[1:7129]

#calculate variance and rearrange columns by variance decreasingly
golub.rearrange <- golub[, order(apply(golub, 2, var), decreasing = T)]
golub <- golub.rearrange[, 1:150]


#Partitioning

#K-Means Clustering
#K-Means Cluster Analysis
fit <- kmeans(x = golub, 8)
fit$cluster  # get cluster assignment
fit$centers  # get cluster center
# get cluster means
aggregate(golub, by = list(fit$cluster), FUN = mean)
#partitioning Around Medoids
require(cluster)
fit <- pam(x = golub, k = 8)
fit$clustering  # get cluster assignment
fit$medoids  # get coordinates of each medoid
#summary method
summary(fit)

#Hierarchical Agglomerative
#Ward Hierarchical Clustering
d <- dist(golub, method = "euclidean")  # distance matrix
fit <- hclust(d, method = "ward")
plot(fit)
groups <- cutree(fit, k = 8)  # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k = 8, border = "red")
#visualization of similarity
require(corrplot)
corrplot(cor(golub.rearrange[, 1:20]))



### ANALYZE C.GLABRATA TIME COURSE EXPRESSION ###
#Link:https://rpubs.com/emptyhb/645504


library(tidyverse)
#Load Data
tab <- read_csv("../data/Ex009_experiment_set_up_20171019.csv")
dat <- read_tsv("../data/Ex009_normalized_log2_read_counts.zip")


#Prepare Data
#choose the samples to use and convert the time points to numeric variable
time2num <- c("pre" = 0, "20m" = 20, "30m" = 30, "45m" = 45, "60m" = 60,
              "90m" = 90, "120m" = 120, "150m" = 150, "180m" = 180, 
              "240m" = 240)
samples.use <- tab %>% 
  filter(Genotype == "wt", Timepoint != "del80") %>% 
  mutate(Time = time2num[Timepoint]) %>% 
  select(Sample, Time)

#extract the list of sample names
wt.list <- samples.use$Sample

#subset the data
dat1 <- dat %>% select(gene, all_of(wt.list))

#there are technical duplicates for each time point. for our purpose
#we just need one value per time point. It is sensible to compute the 
#mean value for each gene at each time point. to do this we first convert
#the data table into a long format, which allows for aggregation functions
dat1.long <- dat1 %>% 
  pivot_longer(starts_with("S"), names_to = "sample") %>% 
  left_join(samples.use, by = c("sample" = "Sample")) %>% 
  select(gene, time = Time, exn = value)

#calculate mean value for each timepoint within each gene
dat1.aggr <- dat1.long %>% 
  group_by(gene, time) %>% 
  summarize(avg.exn = mean(exn), sd.exn = sd(exn), .groups = "drop_last") %>%  
  #subtract the value of the first timepoint from the 
  #rest to form relative expression level (baseline)
  mutate(rel.exn = avg.exn - first(avg.exn))

#convert the long format back to the wide format for extracting matrix
dat1.wide <- dat1.aggr %>% 
  pivot_wider(id_cols = gene, names_from = time, values_from = avg.exn)

dat2.wide <- dat1.aggr %>% 
  pivot_wider(id_cols = gene, names_from = time, values_from = rel.exn)

#convert the result into a matrix for downstream analysis
m.dat1 <- as.matrix(dat1.wide[,-1])
rownames(m.dat1) <- dat1.wide$gene

m.dat2 <- as.matrix(dat2.wide[,-1])
rownames(m.dat2) <- dat2.wide$gene


#Calculate Distance (Dissimilarity) Between Genes
#plot examples
exp.list <- paste0("CAGL0",c("B02475g", "F02145g", "K10868g", "J04202g"))
exp.label <- paste(exp.list, c("PHO84","PHM2","CTA1","HSP12"), sep = " ")
names(exp.label) <- exp.list
dat1.aggr %>% 
  filter(gene %in% exp.list) %>% 
  ggplot(aes(x = time, y = rel.exn)) + geom_line() + geom_point(shape = 0) +
  geom_errorbar(aes(ymin = rel.exn - 1.96*sd.exn, 
                    ymax = rel.exn + 1.96*sd.exn)) +
  facet_wrap(~gene, labeller = labeller(gene = exp.label)) + 
  xlab("time (min)") + ylab("log2 normalized mRNA counts - time_0") +
  labs(caption = "points are average of at least two biological replicates\nerror bars are 95% confidence intervals") +
  theme(plot.caption = element_text(hjust = 0))

#plot examples
exp.list <- paste0("CAGL0",c("K12694g", "K05005g", "D06138g", "D05170g"))
exp.label <- paste(exp.list, c("ACT1","ALG9","HEM2","PHO4"), sep = " ")
names(exp.label) <- exp.list
dat1.aggr %>% 
  filter(gene %in% exp.list) %>% 
  ggplot(aes(x = time, y = rel.exn)) + geom_line() + geom_point(shape = 0) +
  geom_errorbar(aes(ymin = rel.exn - 1.96*sd.exn, 
                    ymax = rel.exn + 1.96*sd.exn)) +
  facet_wrap(~gene, labeller = labeller(gene = exp.label)) + ylim(-5,5) +
  xlab("time (min)") + ylab("log2 normalized mRNA counts - time_0") +
  labs(caption = "points are average of at least two biological replicates\nerror bars are 95% confidence intervals") +
  theme(plot.caption = element_text(hjust = 0))


#A Toy Example
toy <- matrix(c(2,4,4,4,6,6,3,2,20,40,40,40,45,45,35,20), 
              byrow = T, nrow = 4, 
              dimnames = list(gene = paste("gene", 1:4), 
                              time = c(10,20,30,40)))
toy

plot(x = 1, type = "n", xlim = c(10,50), ylim = c(0, 50))
t = c(10,20,30,40)
for(i in 1:4){
  points(t, toy[i,], pch = i)
  lines(t, toy[i,])
}
legend("topright", legend = rownames(toy), pch = 1:4)

#scaling to the rescue
toy.scaled <- scale(t(toy)) #note that most r functions operate on columns
#since we want to operate on the time series, we transpose the matrix so
#that each gene is in a column
matplot(t, toy.scaled, type = "b", pch = 1:4, lty = 1, col = 1)
legend("topright", colnames(toy.scaled), pch = 1:4)

#dist() function
dist(t(toy.scaled)) # dist() computes the distance between rows
#compare
dist(toy)

#Pearson's Correlation Coefficient as Another Way to Measure Similarity
cor(t(toy))
cor(toy)

#Clustering Genes
#use euclidean distance
#we first use the Euclidean distance on the scaled matrix
#note that dist() operates on the rows
t(toy.scaled)
toy.dist1 <- dist(t(toy.scaled), method = "euclidean")
toy.dist1
toy.clust1 <- hclust(toy.dist1)
plot(toy.clust1)

#use correlation matrix
# remember that cor() operates on the columns
t(toy)
toy.dist2 <- 1-cor(t(toy))
toy.dist2
toy.clust2 <- hclust(as.dist(toy.dist2))
plot(toy.clust2)

#Dissecting the heatmap() function
#using the image() function
image(x = 1:4, y = c(10,20,30,40), z = toy, xlab = "gene", ylab = "time (min)", xaxt = "n", yaxt = "n")
axis(side = 1, at = 1:4, labels = rownames(toy))
axis(side = 2, at = c(10,20,30,40), labels = colnames(toy))
heatmap(toy, Rowv = NA, Colv = NA, scale = "none")
#adding the clustering based reordering
#with row clustering
heatmap(toy, Rowv = NULL, Colv = NA, scale = "none")
#adding scaling doesn't change the reordering
#heatmap with row scaling and clustering
heatmap(toy, Rowv = NULL, Colv = NA, scale = "row")

#Implement the reordering step outside the heatmap() function
#below is how reordering is done inside the heatmap function
#recall that we have set toy.dist1 = dist(toy)
toy.dist1
#we can then cluster the genes using this distance matrix, which we stored as toy.clust1
plot(toy.clust1)
#we can then use this order in the heatmap
heatmap(toy, Rowv = as.dendrogram(toy.clust1), Colv = NA, scale = "row")


#Apply to the Data
#check distribution of changes
dat1.aggr %>% 
  group_by(gene) %>% 
  summarize(max.exn = max(abs(rel.exn))) %>% 
  ggplot(aes(max.exn)) + stat_ecdf(geom = "step") +
  scale_x_continuous(breaks = -1:15) +
  xlab("log2 maximum fold change in expression relative to time_0") +
  ylab("cumulative # of genes") +
  ggtitle("Cumulative distribution of maximum fold change in expression")

#subset the data
morethan4fold <- apply(m.dat2, 1, function(x) any(abs(x) >= 2))
m.dat3 <- m.dat2[morethan4fold,]

#we will use the pheatmap function from the pheatmap package, which is 
#similar but with better defaults than the base R's heatmap()
suppressPackageStartupMessages(library(pheatmap))
#nmf.options(grid.patch=TRUE)
colour <- colorRampPalette( c("blue", "black", "yellow"), space="rgb")(64) # for plotting
pheatmap(m.dat3, color = colour, breaks = seq(-8,8,length.out = 65), cluster_rows = F, cluster_cols = F, scale = "none", labels_row = "")

library(corrplot) # plot correlation matrix in heatmap format
dat3.cor <- cor(t(m.dat3), method = "pearson")
dat3.hc <- hclust(as.dist(1-dat3.cor), method = "complete")
od <- dat3.hc$order
colour <- colorRampPalette( c("blue", "black", "yellow"), space="rgb")(64)
corrplot(dat3.cor, col = colour, method = "color", order = "hclust", hclust.method = "ward.D", addrect = 4, tl.pos = "n", title = "Pairwise Pearson's Correlation Coefficient for 1519 genes with >4 fold change")

pheatmap(m.dat3, color = colour, breaks = seq(-8,8,length.out = 65), cluster_rows = dat3.hc, cluster_cols = F, scale = "none", labels_row = "")

dat1.cor <- cor(m.dat1, method = "pearson")
dat1.hc <- hclust(as.dist(1-dat1.cor), method = "complete")
od1 <- dat1.hc$order
colour <- colorRampPalette( c("blue", "black", "yellow"), space="rgb")(64)
pheatmap(dat1.cor, col = colour, cluster_rows = F, cluster_cols = F, scale = "none")



### EXPLORING GENE EXPRESSION PATTERNS USING CLUSTERING METHODS ###
#Link:https://tavareshugo.github.io/data-carpentry-rnaseq/04b_rnaseq_clustering.html



#Setup
##### setup ####

# load packages
library(tidyverse)

# read the data
trans_cts <- read_csv("./data/counts_transformed.csv")
sample_info <- read_csv("./data/sample_info.csv")
test_result <- read_csv("./data/test_result.csv")


##### get counts for candidate genes ####

# set of candidate genes for clustering
candidate_genes <- test_result %>% 
  filter(padj < 0.01) %>%    # filter table
  pull(gene) %>%             # extract the gene column as a vector
  unique()                   # retain only unique values

# Summarise counts 
trans_cts_mean <- trans_cts %>% 
  # convert to long format
  pivot_longer(cols = wt_0_r1:mut_180_r3, names_to = "sample", values_to = "cts")  %>% 
  # join with sample info table
  full_join(sample_info, by = ("sample")) %>% 
  # filter to retain only genes of interest
  filter(gene %in% candidate_genes) %>% 
  # for each gene
  group_by(gene) %>% 
  # scale the cts column
  mutate(cts_scaled = (cts - mean(cts))/sd(cts)) %>% 
  # for each gene, strain and minute
  group_by(gene, strain, minute) %>%
  # calculate the mean (scaled) cts
  summarise(mean_cts_scaled = mean(cts_scaled),
            nrep = n()) %>% 
  ungroup()


#Clustering Basic

#Gene Partitioning Using Hierarchical Clustering
#Clustering Distance Between Samples Using dist()
#Create a matrix
hclust_matrix <- trans_cts %>% 
  select(-gene) %>% 
  as.matrix()
#assign rownames
rownames(hclust_matrix) <- trans_cts$gene
hclust_matrix <- hclust_matrix[candidate_genes, ]
hclust_matrix <- hclust_matrix %>% 
  #transpose the matrix so genes are as columns
  t() %>% 
  #apply scalling to each column of the matrix (genes)
  scale() %>% 
  #transpose back so genes are as rows again
  t()
gene_dist <- dist(hclust_matrix)
#Perform Hierarchical Clusteing Using hclust()
gene_hclust <- hclust(gene_dist, method = "complete")
# The default `plot()` function can be used to produce a simple dendrogram
plot(gene_hclust, labels = FALSE)
abline(h = 10, col = "brown", lwd = 2) # add horizontal line to illustrate cutting dendrogram
cutree(gene_hclust, k = 5)
gene_cluster <- cutree(gene_hclust, k = 5) %>% 
  # turn the named vector into a tibble
  enframe() %>% 
  # rename some of the columns
  rename(gene = name, cluster = value)
head(gene_cluster)
#Visualise Gene Expression Trends per Cluster
trans_cts_cluster <- trans_cts_mean %>% 
  inner_join(gene_cluster, by = "gene")
head(trans_cts_cluster)
trans_cts_cluster %>% 
  ggplot(aes(minute, mean_cts_scaled)) +
  geom_line(aes(group = gene)) +
  facet_grid(rows = vars(strain), cols = vars(cluster))
trans_cts_cluster %>% 
  ggplot(aes(minute, mean_cts_scaled)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  geom_line(stat = "summary", fun = "median", colour = "brown", size = 1.5, 
            aes(group = 1)) +
  facet_grid(rows = vars(strain), cols = vars(cluster))

#Clustering Using Heatmap
library(ComplexHeatmap)
Heatmap(hclust_matrix, show_row_names = FALSE)



### PENALIZED REGRESSION PART 1 ###
#Link: https://slideplayer.com/slide/12916896/
#GANGERTI