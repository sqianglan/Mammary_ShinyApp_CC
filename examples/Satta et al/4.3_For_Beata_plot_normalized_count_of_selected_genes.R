### plot the normalzide count of selected genes. 

rm(list=ls())
#library(dplyr)
library(tidyverse)
library(DESeq2)
source('scripts/tools.R')
library(ggpubr)

# output <- 'results/Final_rounds/'
output <- "results/For_Beata/"
dir.create(output)
sampleTable <- read.csv('processed_files/sampleTable.csv', stringsAsFactors = T)



##get normalized count number

dds <- readRDS(file="processed_files/Deseq_dds_txi_countFilted_1.rds")

dds <- estimateSizeFactors(dds)

s <- counts(dds, normalized=TRUE)


gene_attribute_data <- readRDS(file = "processed_files/gene_annotation.rds") 


# sampleTable <- read.csv('processed_files/sampleTable.csv', stringsAsFactors = T)



normalized_count <- #df_var %>% 
  s %>% 
  as.data.frame() %>% 
  #dplyr::select(selected_sampeTable_with_skin$sampleName) %>% 
  .[unique(gene_attribute_data$ensembl_gene_id),] %>% 
rownames_to_column('ensembl_gene_id') %>% 
  pivot_longer(.,2:ncol(.), names_to = 'sample', values_to = 'normalized_count') %>% 
  #separate(col=sample, into = c('group', 'sample'), sep='_Sample_') %>% 
  merge(., sampleTable[, c('sampleName', 'group')], by.x = 'sample', by.y = 'sampleName' ) %>% 
  mutate(group =factor(group, levels= c('E13.5_WT', 
                                         'E13.5_Stab_bcat', 
                                        'E16.5_WT', 
                                        'E16.5_Stab_bcat'
                                        ))) %>% 
  dplyr::filter(group %in% c('E13.5_WT', 'E16.5_WT')) %>% 
  merge(gene_attribute_data[, 1:2], by = 'ensembl_gene_id')

###for human reading 


normalized_count_human <- normalized_count %>% 
  arrange(group) %>% 
  unite('temp', c(group, sample), sep ='_sample_', remove = T) %>% 
  pivot_wider(names_from = temp, values_from = normalized_count, names_prefix = 'Normalized_count_', values_fn=mean)

#write.csv(normalized_count_human, paste0('results/',file_count('results')+1,'_normalized_count.csv', sep = '\t'))

#####
# goi <- read.table('/Users/qianglan/Projects/Common_DB/wnt_targets_Renee.txt', sep= '\t',header = F) %>% dplyr::filter(V1 !='')
#   
#   
# p <- ggplot(data= normalized_count %>% dplyr::filter(external_gene_name %in% goi$V1), aes(x=group, y = normalized_count, col=group))+
#   theme_bw()+
#   theme(axis.text.x = element_blank())+
#   geom_jitter(width = 0.25, size = 1, shape =1)+
#   #scale_y_continuous(limits=c(0,NA))+
#   #coord_cartesian(ylim=c(0, NA))+
#   expand_limits(y=0)+
#   ggtitle('Wnt target Gene from Renee paper')+
#   facet_wrap(~external_gene_name, scales = 'free_y', ncol = 4)
# 
# p
# 
# ggsave(paste0('results/',file_count('results')+1,'_Wnt_targets_Renee.pdf'), width = 9, height = 9)
# 
# fgfs <- normalized_count$external_gene_name %>% .[str_which(string = ., pattern = 'Fgf')] %>% unique()
# 
# wnts <- c('Dkk1', 'Dkk2', 'Dkk4', 'Notum', 'Wif1', 'Sostdc1', 'Ctnnb1' )
# wnts_ligand <- normalized_count$external_gene_name %>% .[str_which(string = ., pattern = 'Wnt|Tcf|Lef')]%>% unique()
# lineages <- c('Krt14', 'Krt8', 'Krt5', 'Krt18', 'Trp63', 'Gata3', 'Shh', 'Lhx2', 'Lor', 'Flg')
# notch <- normalized_count$external_gene_name %>% .[str_which(string = ., pattern = 'Dll|Notch')]%>% unique()
# others <- c( 'Sox2', 'Sox9', 'Sox10', 'Sox11' )
# #goi <- c('Epcam', 'Sox2', 'Sox9', 'Sox10', 'Pthlh', 'Pthlhr', 'Pdgfra', 'Myh9', 'Myh10', 'Myh14', 'Krt14', 'Krt8', 'Krt6a', 'Krt5', 'Krt17', 'Notch1', 'Trp63', 'Tnc')
# 
# goi_df <- data.frame(external_gene_name = c(fgfs, wnts, wnts_ligand, lineages, notch, others)) %>% 
#   mutate(Cluster = case_when(external_gene_name %in% c(wnts, wnts_ligand) ~ 'Wnt_regulator',
#                              external_gene_name %in% lineages ~ 'Cell_lineages_related',
#                              external_gene_name %in% notch ~ 'Notch_signaling',
#                              external_gene_name %in% others ~ 'Gene of interest',
#                              external_gene_name %in% fgfs ~ 'FGFs'
#                              ))
# 
# selected_count <- normalized_count %>% 
#   #dplyr::filter(external_gene_name %in% c('Pthlh', 'Pth1r')) %>% 
#   #dplyr::filter(grepl(x=external_gene_name, pattern = 'tcf|lef', ignore.case = T)) %>% 
#   dplyr::filter(external_gene_name %in% goi_df$external_gene_name) %>%
#   left_join(goi_df, by='external_gene_name')
# 
# #selected_count %>% dplyr::filter(external_gene_name %in% lineages)
# dotplot_per_cluster(df=selected_count,output_folder = output, file_name = 'Gene_of_interest_', width = 9, height = 12)


#goi <- c('Esr1', 'Esr2', 'Foxa1', 'Acta2', 'Smy11', 'Proc', 'Krt19', 'Muc1', 'Icam1', 'Foxc1', 'Notch1', 'Cxcr4')

# goi <- read.table('/Users/qianglan/Projects/Common_DB/Jyoti_project/Henry_mammary_lineage_marker.txt', header = T) %>% 
#   .[[1]] %>% 
#   c(., 'Stat5', 'Foxa1', 'Gata3', 'Edar', 'Muc1') %>% unique()
  
# goi <- c('Rdx', 'Vil1', 'Ezr', 'Ctnna1','Ctnna2','Seva', 'Rhoa', 'Gsn')
#   #c('Fgf4', 'Shh', 'Sox2', 'Ndnf', 'Syk', 'Mki67')
#####

goi <- c('Myh9', 'Myh10', "Myh11")

### Old version 
p <- ggplot(data= normalized_count %>% 
              dplyr::filter(external_gene_name %in% goi) %>% 
              dplyr::mutate(external_gene_name= factor(external_gene_name, levels = goi)),
            aes(x=group, y = normalized_count, col=group))+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  geom_jitter(width = 0.25, size = 1, shape =1)+
  #scale_y_continuous(limits=c(0,NA))+
  #coord_cartesian(ylim=c(0, NA))+
  expand_limits(y=0)+
  ggtitle('Henry_mammary_lineage_marker')+
  facet_wrap(~external_gene_name, scales = 'fixed', ncol = 4)

p

ggsave(paste0(output,file_count(output)+1,'_Beata_interest_gene.pdf'), width = 7, height = 4)


## update version 

selected_count <- normalized_count %>% 
  #dplyr::filter(external_gene_name %in% c('Pthlh', 'Pth1r')) %>% 
  #dplyr::filter(grepl(x=external_gene_name, pattern = 'tcf|lef', ignore.case = T)) %>% 
  dplyr::filter(group %in% c('E13.5_WT', 'E16.5_WT')) %>% 
  dplyr::filter(external_gene_name %in% goi) %>%
  mutate(external_gene_name = factor(external_gene_name, levels = c('Myh9', 'Myh10', "Myh11")),
         group = droplevels(group),
         group = factor(group, levels =c('E13.5_WT', 'E16.5_WT') ))
  
  # mutate(Cluster = 'IFN_pathways_core_enrichment',
  #        group= droplevels(group),
  #        group = ifelse(group == 'WT', 'D2OR', 'D2A1'),
  #        group = factor(group, levels = (c('D2OR', 'D2A1'))))


df_mean <- selected_count %>% group_by(group,external_gene_name) %>% 
  summarise(mean=mean(normalized_count), std=sd(normalized_count)) 

df_plotting_with_std <- merge(selected_count, df_mean, by = c('external_gene_name', 'group')) %>% 
  mutate(external_gene_name = factor(external_gene_name, levels = c('Myh9', 'Myh10', "Myh11")),
         group = factor(group, levels =c('E13.5_WT', 'E16.5_WT') ))
  

my_comparisons <- list(c('E13.5_WT','E16.5_WT'))

## Load DEseq comparision 
df_wt <- read.csv("results/1_2_rounds/33_group_E16.5_WT_vs_E13.5_WT_LFC_shrinkaged.csv")

significance_data <- merge(df_mean,df_wt[, c('external_gene_name', 'padj')], by = 'external_gene_name') %>% 
  group_by(external_gene_name, group) %>% 
  summarise(y = max(mean) + max(std), padj=mean(padj)) %>% ## padj was same between different groups in fact. mean just unify them
  group_by(external_gene_name) %>% 
  summarise(y = max(y), label = mean(padj)) %>% 
  mutate(.,x = (seq(1:nrow(.)))) 



## "#E64B35FF" "#4DBBD5FF" "#00A087FF" "#3C5488FF" "#F39B7FFF" "#8491B4FF" "#91D1C2FF"

library(ggbeeswarm)
p1 <- ggplot(df_plotting_with_std , aes(x = external_gene_name,y=normalized_count,color= group))+
  theme_classic()+
  theme(#axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    strip.background = element_blank(),
    strip.text.x = element_text(
      face = "bold"
    ))+
  #geom_bar(stat = "summary", fun = "mean", width =0.85, fill=NA, show.legend = F)+
  geom_bar(stat = "summary", fun = "mean", position=position_dodge(width=0.8), width =0.7, aes(fill=group), linewidth=0.4, show.legend = T)+
  #geom_bar(position=position_dodge(width=0.6), stat="identity", width =0.5, fill='NA', aes(fill=groups)) +
  # geom_point( position =position_jitterdodge(dodge.width = 0.8), size=0.8, show.legend = F, shape = 21, fill='white')+
  #geom_beeswarm(size=0.8, show.legend = F, shape = 21, fill='white')+
  geom_quasirandom(dodge.width=0.8, show.legend =F , size = 1)+
  
  scale_fill_manual(values=c('E13.5_WT' = '#91D1C2FF', 'E16.5_WT'='#F39B7FFF'), labels = c("E13.5", "E16.5"), name = "Tissues")+
  scale_color_manual(values=c(E13.5_WT = '#008E51','E16.5_WT'='#E64B35FF'),labels = c("E13.5", "E16.5"), name = "Tissues")+
  xlab('')+
  ylab("Normalized Counts")+
  #ylim(-20,75000)+
  # scale_y_cut(
  #   breaks = c(450),
  #   scales = c(0.3),  # 500-2000 range gets 2x density
  #   space = 0.01 # Padding adjustment
  # ) +
  
  # scale_y_break(
  #   c(450, 2000), 
  #   scales = 0.5,  # The upper segment will be 2x compressed (1/0.5 = 2)
  #   space = 0.1#,   # Space between the broken segments
  #   #ticklabels = c(500, seq(750, 2000, by = 250))
  #   ) +  # Custom ticks for upper range
  #facet_wrap(~Gene.Symbol, scales='free_y', nrow=1)+
  #ggtitle('4T1 (Ross et al.)')+
  #stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test")+
  geom_errorbar(aes(ymin=mean-std, ymax=mean+std, color = group),width=0.3, position = position_dodge(width = 0.8), linewidth = 0.3, show.legend = F)#+
# stat_compare_means(comparisons = my_comparisons, 
#                   aes(label =  ..p.signif..), method = "t.test", paired=F, tip.length = 0,vjust = 0.5)
p1
t1 <- p1+ 
  ylim(-20,75000)+
  geom_segment(
  data = significance_data,
  aes(x = x - 0.2,
      xend = x + 0.2),
  y = c(72000, 28000, 2200) , 
  yend = c(72000, 28000, 2200),
  inherit.aes = FALSE
) +
  ggtitle("DEseq statistics")+
  geom_text(
    data = significance_data,
    aes(x = x,
        label = paste0("p = ", round(label,2))),
    y = c(74500, 30500, 4800),
    inherit.aes = FALSE,
    size = 3, show.legend = F
  ) 

t1


p2 <-  ggplot(df_plotting_with_std , aes(x = external_gene_name,y=normalized_count,color= group))+
  theme_classic()+
  theme(#axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    strip.background = element_blank(),
    strip.text.x = element_text(
      face = "bold"
    ))+
  #geom_bar(stat = "summary", fun = "mean", width =0.85, fill=NA, show.legend = F)+
  geom_bar(stat = "summary", fun = "mean", position=position_dodge(width=0.8), width =0.7, aes(fill=group), linewidth=0.4, show.legend = T)+
  #geom_bar(position=position_dodge(width=0.6), stat="identity", width =0.5, fill='NA', aes(fill=groups)) +
  # geom_point( position =position_jitterdodge(dodge.width = 0.8), size=0.8, show.legend = F, shape = 21, fill='white')+
  #geom_beeswarm(size=0.8, show.legend = F, shape = 21, fill='white')+
  geom_quasirandom(dodge.width=0.8, show.legend =F , size = 1)+
  
  scale_fill_manual(values=c('E16.5_WT'='#F39B7FFF','E13.5_WT' = '#91D1C2FF'), labels = c("E16.5", "E13.5"), name = "Tissues")+
  scale_color_manual(values=c('E16.5_WT'='#E64B35FF','E13.5_WT' = '#008E51'),labels = c("E16.5", "E13.5"), name = "Tissues")+
  xlab('')+
  ylab("Normalized Counts")+
  #ylim(-20,2100)+
  # scale_y_cut(
  #   breaks = c(450),
  #   scales = c(0.3),  # 500-2000 range gets 2x density
  #   space = 0.01 # Padding adjustment
  # ) +
  
  # scale_y_break(
  #   c(450, 2000), 
  #   scales = 0.5,  # The upper segment will be 2x compressed (1/0.5 = 2)
  #   space = 0.1#,   # Space between the broken segments
  #   #ticklabels = c(500, seq(750, 2000, by = 250))
  #   ) +  # Custom ticks for upper range
  #facet_wrap(~Gene.Symbol, scales='free_y', nrow=1)+
  #ggtitle('4T1 (Ross et al.)')+
  
  geom_errorbar(aes(ymin=mean-std, ymax=mean+std, color = group),width=0.3, position = position_dodge(width = 0.8), linewidth = 0.3, show.legend = F)
# stat_compare_means(comparisons = my_comparisons,
#                   aes(label =  ..p.signif..), method = "t.test", paired=F, tip.length = 0,vjust = 0.5)
p2

t2 <- p2+
  stat_compare_means(method = "t.test",aes(label =  ..p.signif..), label.y = c(72000, 28000, 2800), show.legend = F,size = 6)+
  ggtitle("t-test")+
  ylim(-20,75000)+
  geom_segment(
  data = significance_data,
  aes(x = x - 0.2,
      xend = x + 0.2),
      y = c(72000, 28000, 2200) , 
      yend = c(72000, 28000, 2200),
  inherit.aes = FALSE
) 

t2

library(patchwork)

## add comparision 
t3 <- p1 + facet_wrap(~group, scales = 'free_y' ) +
  ggtitle("kruskal.test")+
  stat_compare_means(method = "kruskal.test",
                     aes(group = external_gene_name),
                     label = "p.signif", show.legend = F,
                     
                     comparisons = list(c('Myh9', 'Myh10'),
                                         c("Myh10", "Myh11"),
                                         c("Myh9", "Myh11"))
                     #label.y = c(72000, 28000, 2800), show.legend = F,size = 6
                     )
  
t3



t4 <- p1 + facet_wrap(~group, scales = 'free_y' ) +
  ggtitle("pair-wise t test")+
  stat_compare_means(method = "t.test",
                     aes(group = external_gene_name),
                     label = "p.signif", show.legend = F,
                     
                     comparisons = list(c('Myh9', 'Myh10'),
                                        c("Myh10", "Myh11"),
                                        c("Myh9", "Myh11"))
                     #label.y = c(72000, 28000, 2800), show.legend = F,size = 6
  )

t4

layout <- "
AAAAABBBBBC
DDDDDEEEEE#
"


t1 + t2 + guide_area()+ t3 + t4 + plot_layout(guides = "collect", design = layout) + plot_annotation(title = 'Mammary Epithelium')
ggsave(paste0(output, file_count(output)+1, '_Epitheliaum_Myhs.eps'), width =10, height = 8)

write.table(df_plotting_with_std, paste0(output, file_count(output)+1, "_plotting_data_for_Myhs.csv"), sep= '\t')


