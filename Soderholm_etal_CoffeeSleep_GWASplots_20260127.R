# Code for plotting of GWAS results
# R-version used: 4.0.3

#########################################
# Load R packages
#########################################

library(ggplot2) # 3.4.4
library(dplyr) # 1.1.4
library(tidyverse) # 2.0.0
library(qqman) # 0.1.9
library(karyoploteR) # 1.16.0
library(Gviz) # 1.34.1
library(topr) # 2.0.0
library(biomaRt) # 2.46.3
library(webr) # 0.1.5
library(reshape2) # 1.4.4
library(geomtextpath) # 0.1.1
library(rtracklayer) # 1.50.0
library(data.table) # 1.14.10
library(cowplot) # 1.1.2


#########################################
# GWAS coffee run 4
#########################################

gwas_res_coffee4 <- read.table("GWAS_results/Coffee_Run4/Association_coffee_res_Run4.q5question2.glm.linear", head=TRUE, stringsAsFactors = FALSE, sep = '\t', comment.char = "")

gwas_res_coffee4_sign <- gwas_res_coffee4[gwas_res_coffee4$P < 5e-08,]

write.table(gwas_res_coffee4_sign, file = "GWAS_results/Coffee_Run4/CoffeeRun4_sign_SNPs.txt", row.names = FALSE, quote = FALSE, sep = '\t')

gwas_res_coffee4_sign_adj <- gwas_res_coffee4_sign[,c(3,1,2,7,8,9,12,13,15)]
colnames(gwas_res_coffee4_sign_adj) <- c("SNP name","Chr.","Position","Effect allele","Other allele","MAF","BETA","SE","P-value")

write.table(gwas_res_coffee4_sign_adj, file = "GWAS_results/Coffee_Run4/CoffeeRun4_sign_SNPs_adjTable.txt", row.names = FALSE, quote = FALSE, sep = '\t')

# make bed file with color-coding based on BETA-value

gwas_res_coffee4_sign_bed <- gwas_res_coffee4_sign[,c(1,2,2,3,12)]
gwas_res_coffee4_sign_bed$RGB[gwas_res_coffee4_sign_bed$BETA < 0] = 704254
gwas_res_coffee4_sign_bed$RGB[gwas_res_coffee4_sign_bed$BETA > 0] = 16711680

gwas_res_coffee4_sign_bed$C5 <- 0
gwas_res_coffee4_sign_bed$C6 <- "."
gwas_res_coffee4_sign_bed$C7 <- 1200
gwas_res_coffee4_sign_bed$C8 <- 350

gwas_res_coffee4_sign_bed <- gwas_res_coffee4_sign_bed[,c(1:4,7:10,6)]
write.table(gwas_res_coffee4_sign_bed, file = "GWAS_results/Coffee_Run4/CoffeeRun4_sign_SNPs_2.bed", row.names = FALSE, col.names = FALSE, sep = '\t', quote = FALSE)

gwas_res_coffee4_sign1 <- gwas_res_coffee4[gwas_res_coffee4$P < 5e-08,]
gwas_res_coffee4_sign2 <- gwas_res_coffee4[gwas_res_coffee4$P < 5e-07,]
gwas_res_coffee4_sign3 <- gwas_res_coffee4[gwas_res_coffee4$P < 5e-06,]
gwas_res_coffee4_sign4 <- gwas_res_coffee4[gwas_res_coffee4$P < 5e-05,]
gwas_res_coffee4_sign5 <- gwas_res_coffee4[gwas_res_coffee4$P < 5e-04,]

# number of SNPs on each chromosome

table(gwas_res_coffee4$X.CHROM)
table(gwas_res_coffee4_sign$X.CHROM)

# plot beta values (effect sizes)

Coffee_betaplot <- gwas_res_coffee4_sign[order(gwas_res_coffee4_sign$BETA, decreasing = FALSE),]
Coffee_betaplot$ID <- factor(Coffee_betaplot$ID, levels = Coffee_betaplot$ID)
Coffee_betaplot$BetaDirection[Coffee_betaplot$BETA > 0] <- "UP"
Coffee_betaplot$BetaDirection[Coffee_betaplot$BETA < 0] <- "DOWN"
Coffee_betaplot$X.CHROM <- factor(Coffee_betaplot$X.CHROM, levels = c("7","15","22"))

ggplot(Coffee_betaplot, aes(x = reorder(ID, BETA), y = BETA, fill = BetaDirection)) +
  geom_segment(aes(x = ID, xend = ID, y = 0, yend = BETA)) +
  geom_point(size = 4, pch = 21) +
  #xlim(c(-0.2, 0.2)) +
  geom_hline(yintercept = 0) +
  facet_grid(. ~ X.CHROM, space = "free", scales = "free") +
  #coord_flip() +
  theme_classic() +
  scale_fill_manual(values = c("gray70","indianred3")) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, color = "black"), axis.text.y = element_text(size = 10, color = "black"), legend.position = "none", axis.title.x = element_blank())



ggplot(Coffee_betaplot, aes(x = BETA, y = ID)) +
  geom_point(size = 3) +
  xlim(c(-0.2, 0.2)) +
  geom_vline(xintercept = 0) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

names(gwas_res_coffee4)[names(gwas_res_coffee4) == 'X.CHROM'] <- 'CHROM'

png("GWAS_results/Coffee_Run4/Coffee_GWAS_manhattan_Run4.png", height = 1000, width = 2000, res = 300)
topr::manhattan(gwas_res_coffee4, annotate = 5e-08, build = 37)
dev.off()

# Region plots

png("GWAS_results/Coffee_Run4/Coffee_GWAS_regionplot_Run4_chr7.png", height = 400, width = 700)
topr::regionplot(gwas_res_coffee4, legend_position = "bottom", region = "chr7:17000000-17750000", unit_gene = 0.5, scale = 1.25, build = 37, show_genes = TRUE, gene_color = "darkred", protein_coding_only = TRUE, show_overview = FALSE, unit_main = 4, rsids = c("rs4410790","rs6968554","rs10275488"))
dev.off()

png("GWAS_results/Coffee_Run4/Coffee_GWAS_regionplot_Run4_chr15.png", height = 400, width = 700)
topr::regionplot(gwas_res_coffee4, legend_position = "bottom", region = "chr15:74750000-75500000", unit_main = 5, unit_gene = 4, gene_color = "darkred", protein_coding_only = TRUE, scale = 1.25, build = 37, show_overview = FALSE, rsids = c("rs2472297","rs2470893","rs35107470"), show_genes = TRUE)
dev.off()

png("GWAS_results/Coffee_Run4/Coffee_GWAS_regionplot_Run4_chr22.png", height = 400, width = 700)
topr::regionplot(gwas_res_coffee4, legend_position = "bottom", region = "chr22:24250000-25000000", unit_main = 6, unit_gene = 4, gene_color = "darkred", protein_coding_only = TRUE, scale = 1.25, build = 37, show_overview = FALSE, rsids = c("rs6004089"), show_genes = TRUE)
dev.off()


topr::regionplot(gwas_res_coffee4, annotate = 5e-08, legend_position = "bottom", region = "chr7:17000000-17750000", unit_gene = 4, scale = 1.25, build = 37, show_genes = TRUE, gene_color = "darkred", protein_coding_only = TRUE)

topr::regionplot(gwas_res_coffee4, annotate = 5e-08, legend_position = "bottom", region = "chr22:24000000-25250000", unit_gene = 4, gene_color = "darkred", protein_coding_only = TRUE, scale = 1.25, build = 37, show_genes = TRUE)

gtf <- rtracklayer::import("/media/simso02/4TB/GenomeReferences/Ensembl_GTF_files/Homo_sapiens.GRCh37.87.gtf")
gtf_df <- as.data.frame(gtf)
gtf_df2 <- gtf_df[gtf_df$type == "gene",]
gtf_df2 <- gtf_df2[c("seqnames","start","end","gene_name","gene_biotype")]
gtf_df2$seqnames <- paste0("chr", gtf_df2$seqnames)
colnames(gtf_df2) <- c("chrom","gene_start","gene_end","gene_symbol","biotype")

hg19genome <- read.table("/home/simso02/igv/genomes/hg19.genome_FILES/refGene.txt", header = FALSE, sep = '\t', stringsAsFactors = FALSE)
hg19genome <- hg19genome[c("V3","V5","V6","V13")]




#########################################
# GWAS coffee run 5
# including smoke status as covariate
#########################################

gwas_res_coffee5 <- read.table("GWAS_results/Coffee_Run5/Association_coffee_res_Run5.q5question2.glm.linear", head=TRUE, stringsAsFactors = FALSE, sep = '\t', comment.char = "")

gwas_res_coffee5_sign <- gwas_res_coffee5[gwas_res_coffee5$P < 5e-08,]

names(gwas_res_coffee5)[names(gwas_res_coffee5) == 'X.CHROM'] <- 'CHROM'

png("GWAS_results/Coffee_Run5/Coffee_GWAS_manhattan_Run5.png", height = 400, width = 800)
topr::manhattan(gwas_res_coffee5, annotate = 5e-08, build = 37)
dev.off()

png("GWAS_results/Coffee_Run5/Coffee_GWAS_regionplot_Run5_chr7.png", height = 700, width = 800)
topr::regionplot(gwas_res_coffee5, annotate = 5e-08, legend_position = "bottom", region = "chr7:17000000-17750000", unit_gene = 4, scale = 1.25, build = 37, show_genes = TRUE, gene_color = "darkred", protein_coding_only = TRUE)
dev.off()

png("GWAS_results/Coffee_Run5/Coffee_GWAS_regionplot_Run5_chr15.png", height = 700, width = 800)
topr::regionplot(gwas_res_coffee5, annotate = 5e-08, legend_position = "bottom", region = "chr15:73000000-77000000", unit_main = 5, unit_gene = 4, gene_color = "darkred", protein_coding_only = TRUE, scale = 1.25, build = 37)
dev.off()

png("GWAS_results/Coffee_Run5/Coffee_GWAS_regionplot_Run5_chr22.png", height = 700, width = 800)
topr::regionplot(gwas_res_coffee5, annotate = 5e-08, legend_position = "bottom", region = "chr22:24000000-25250000", unit_main = 6, gene_color = "darkred", protein_coding_only = TRUE, scale = 1.25, build = 37)
dev.off()

png("GWAS_results/Coffee_Run5/Coffee_GWAS_regionplot_Run5_chrX.png", height = 700, width = 800)
topr::regionplot(gwas_res_coffee5, annotate = 5e-08, legend_position = "bottom", region = "chrX:30309028-32509028", unit_main = 6, gene_color = "darkred", protein_coding_only = TRUE, scale = 1.25, build = 37)
dev.off()

#########################################
# GWAS sleep-related traits
#########################################


GWAS_sleep <- list(
  "cqsh001" = read.table("GWAS_results/Sleep/cqsh001/Association_cqsh001.cqsh001.glm.linear", head=TRUE, stringsAsFactors = FALSE, sep = '\t', comment.char = ""),
  "cqsh002" = read.table("GWAS_results/Sleep/cqsh002/Association_cqsh002.cqsh002.glm.linear", head=TRUE, stringsAsFactors = FALSE, sep = '\t', comment.char = ""),
  "cqsh003" = read.table("GWAS_results/Sleep/cqsh003/Association_cqsh003.cqsh003.glm.linear", head=TRUE, stringsAsFactors = FALSE, sep = '\t', comment.char = ""),
  "cqsh004" = read.table("GWAS_results/Sleep/cqsh004/Association_cqsh004.cqsh004.glm.linear", head=TRUE, stringsAsFactors = FALSE, sep = '\t', comment.char = ""),
  "cqsh005" = read.table("GWAS_results/Sleep/cqsh005/Association_cqsh005.cqsh005.glm.linear", head=TRUE, stringsAsFactors = FALSE, sep = '\t', comment.char = ""),
  "cqsh014" = read.table("GWAS_results/Sleep/cqsh014/Association_cqsh014.cqsh014.glm.linear", head=TRUE, stringsAsFactors = FALSE, sep = '\t', comment.char = ""),
  "cqsh015" = read.table("GWAS_results/Sleep/cqsh015/Association_cqsh015.cqsh015.glm.linear", head=TRUE, stringsAsFactors = FALSE, sep = '\t', comment.char = "")
)

GWAS_sleep_sign <- list(
  "cqsh001" = GWAS_sleep$cqsh001[GWAS_sleep$cqsh001$P < 5e-08,],
  "cqsh002" = GWAS_sleep$cqsh002[GWAS_sleep$cqsh002$P < 5e-08,],
  "cqsh003" = GWAS_sleep$cqsh003[GWAS_sleep$cqsh003$P < 5e-08,],
  "cqsh004" = GWAS_sleep$cqsh004[GWAS_sleep$cqsh004$P < 5e-08,],
  "cqsh005" = GWAS_sleep$cqsh005[GWAS_sleep$cqsh005$P < 5e-08,],
  "cqsh014" = GWAS_sleep$cqsh014[GWAS_sleep$cqsh014$P < 5e-08,],
  "cqsh015" = GWAS_sleep$cqsh015[GWAS_sleep$cqsh015$P < 5e-08,]
)



for(n in 1:length(names(GWAS_sleep))) {
  
  Name <- names(GWAS_sleep[n])
  print(n)
  print(Name)
  
  names(GWAS_sleep[[n]])[names(GWAS_sleep[[n]]) == 'X.CHROM'] <- 'CHROM'
  
  plt <- topr::manhattan(GWAS_sleep[[n]], annotate = 5e-08, build = 37, ymax = -log10(1e-08))
  
  png(paste0("GWAS_results/Sleep/", Name, "/", Name, "_manhattan.png"), height = 400, width = 800)
  plot(plt)
  dev.off()
  
  print(paste0(n, "/", length(names(GWAS_sleep)), " finished"))
  
}


names(GWAS_sleep$cqsh001)[names(GWAS_sleep$cqsh001) == 'X.CHROM'] <- 'CHROM'

png("GWAS_results/Sleep/cqsh001/cqsh001_manhattan.png", height = 400, width = 800)
topr::manhattan(GWAS_sleep$cqsh001, annotate = 5e-08, build = 37, ymax = -log10(1e-08))
dev.off()


