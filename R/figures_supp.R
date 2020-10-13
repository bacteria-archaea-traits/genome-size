##################################################################################
# Create supplementary figures
#
# "Genome size, oxygen use and ecological strategy across bacteria and archaea"  
# Nielsen DA , Fierer N , Geoghegan JL, Gillings MR, Gumerov V , Madin JS, 
# Moore L , Paulsen IT, Reddy TBK , Tetu SG, Westoby M*
#
# *Corresponding Author
#
##################################################################################


########
# Fig S1 [genome size and trait]
########

# Fig S1a - sporulation

# Prepare data

#Restrict to species with information
sub <- df[!is.na(df$sporulation) & !is.na(df$genome_size) & !is.na(df$oxyagg) & df$oxyagg != "both",]
#Get count of observations for each box
n <- sub %>% group_by(superkingdom, sporulation, oxyagg) %>%
  summarise(n = n())

sub$sporulation <- as.factor(sub$sporulation)

# Create plot

p1a <- ggplot(sub, aes(x = oxyagg, y = genome_size/1000000)) +
  stat_boxplot(geom ='errorbar', width = 0.3, aes(col = sporulation), position=position_dodge(0.8),  show.legend = FALSE) +
  geom_point(aes(col = sporulation), position = position_jitterdodge(dodge.width = 0.8,jitter.width = 0.3), alpha= 0.4, size = 1, stroke = 0.4, shape = 1, show.legend = FALSE) +
  geom_boxplot(aes(fill = sporulation), color = "black", position=position_dodge(0.8), outlier.shape = NA, width = 0.4) +
  basic_layout +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.21, 0.95), 
    legend.direction = "vertical",
    legend.key.size = unit(0.7,"line"),
    legend.spacing.x = unit(1, 'mm')
  ) +
  
  scale_y_log10()+
  #scale_x_discrete(expand = expand_scale(add = -2.1)) + 
  annotation_logticks(sides="l", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  
  scale_fill_manual(labels = c("don't sporulate", "sporulate"), values = presence_absence) +
  scale_colour_manual(values = c("#000000","#000000")) +
  geom_text(data = n, aes(label = n, y = 0.4, colour = sporulation), position=position_dodge(width = 0.8), size = 2.5, family = font_family, show.legend = FALSE) +
  facet_grid(. ~ superkingdom) + 
  labs(x = labelx_metabolism, y = labely_genome_size, title = "") + 
  guides(fill = guide_legend(title="")) 
p1a

# Fig S1b - motility

# Prepare data

#Restrict to species with information
sub <- df[!is.na(df$motagg) & !is.na(df$genome_size) & !is.na(df$oxyagg) & df$oxyagg != "both",]
#Get count of observations for each box
n <- sub %>% group_by(superkingdom, motagg, oxyagg) %>%
  summarise(n = n())

# Create plot

p1b <- ggplot(sub, aes(x = oxyagg, y = genome_size/1000000)) +
  stat_boxplot(geom ='errorbar', width = 0.3, aes(col = motagg), position=position_dodge(0.8),  show.legend = FALSE) +
  geom_point(aes(col = motagg), position = position_jitterdodge(dodge.width = 0.8,jitter.width = 0.3), alpha= 0.4, size = 1, stroke = 0.4, shape = 1, show.legend = FALSE) +
  geom_boxplot(aes(fill =  motagg), color = "black", position=position_dodge(0.8), outlier.shape = NA, width = 0.4) +
  basic_layout +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    #axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.background = element_blank(),
    legend.position = c(0.15, 0.95), 
    legend.direction = "vertical",
    legend.key.size = unit(0.7,"line"),
    legend.spacing.x = unit(1, 'mm')
  ) +
  
  scale_y_log10()+
  annotation_logticks(sides="l", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  
  scale_fill_manual(labels = c("not motile", "motile"), values = presence_absence) +
  scale_colour_manual(values = c("#000000","#000000")) +
  geom_text(data = n, aes(label = n, y = 0.4, colour =motagg), position=position_dodge(width = 0.8), size = 2.5, family = font_family, show.legend = FALSE) +
  facet_grid(. ~ superkingdom) + 
  labs(x = labelx_metabolism, y = labely_genome_size, title = "") +
  guides(fill = guide_legend(title="")) 
p1b


# Fig S1c - gram stain

# Prepare data

#Restrict to species with information
sub <- df[!is.na(df$gram_stain) & !is.na(df$genome_size) & !is.na(df$oxyagg) & df$oxyagg != "both",]
#Get count of observations for each box
n <- sub %>% group_by(superkingdom, gram_stain, oxyagg) %>%
  summarise(n = n())

# Create plot

p1c <- ggplot(sub, aes(x = oxyagg, y = genome_size/1000000)) +
  stat_boxplot(geom ='errorbar', width = 0.3, aes(col = gram_stain), position=position_dodge(0.8),  show.legend = FALSE) +
  geom_point(aes(col = gram_stain), position = position_jitterdodge(dodge.width = 0.8,jitter.width = 0.3), alpha= 0.4, size = 1, stroke = 0.4, shape = 1, show.legend = FALSE) +
  geom_boxplot(aes(fill =  gram_stain), color = "black", position=position_dodge(0.8), outlier.shape = NA, width = 0.4) +
  basic_layout +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.15, 0.95), 
    legend.direction = "vertical",
    legend.key.size = unit(0.7,"line"),
    legend.spacing.x = unit(1, 'mm')
  ) +
  
  scale_y_log10()+
  annotation_logticks(sides="l", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +

  scale_fill_manual(labels = c("gram -", "gram +"), values = presence_absence) +
  scale_colour_manual(values = c("#000000","#000000")) +
  geom_text(data = n, aes(label = n, y = 0.4, colour = gram_stain), position=position_dodge(width = 0.8), size = 2.5, family = font_family, show.legend = FALSE) +
  facet_grid(. ~ superkingdom) + 
  labs(x = labelx_metabolism, y = labely_genome_size, title = "") +
  guides(fill = guide_legend(title="")) 
p1c


# Fig S1d - shape

# Prepare data

#Restrict to species with information
sub <- df[!is.na(df$shapeagg) & !is.na(df$genome_size) & !is.na(df$oxyagg) & df$oxyagg != "both",]
#Get count of observations for each box
n <- sub %>% group_by(superkingdom, shapeagg, oxyagg) %>%
  summarise(n = n())

# Create plot

p1d <- ggplot(sub, aes(x = oxyagg, y = genome_size/1000000)) +
  stat_boxplot(geom ='errorbar', width = 0.3, aes(col = shapeagg), position=position_dodge(0.8),  show.legend = FALSE) +
  geom_point(aes(col = shapeagg), position = position_jitterdodge(dodge.width = 0.8,jitter.width = 0.3), alpha= 0.4, size = 1, stroke = 0.4, shape = 1, show.legend = FALSE) +
  geom_boxplot(aes(fill =  shapeagg), color = "black", position=position_dodge(0.8), outlier.shape = NA, width = 0.4) +
  basic_layout +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.15, 0.95), 
    legend.direction = "vertical",
    legend.key.size = unit(0.7,"line"),
    legend.spacing.x = unit(1, 'mm')
  ) +
  
  scale_y_log10()+
  annotation_logticks(sides="l", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  
  scale_fill_manual(values = presence_absence) +
  scale_colour_manual(values = c("#000000","#000000")) +
  geom_text(data = n, aes(label = n, y = 0.4, colour = shapeagg), position=position_dodge(width = 0.8), size = 2.5, family = font_family, show.legend = FALSE) +
  facet_grid(. ~ superkingdom) + 
  labs(x = labelx_metabolism, y = labely_genome_size, title = "") +
  guides(fill = guide_legend(title="")) 
p1d


#Save as combined figures
figS1 <- ggarrange(p1a,p1b,p1c,p1d, ncol = 2, nrow = 2,  heights = c(1, 1, 1, 1), labels="AUTO")
figS1

ggsave(filename = "FigS1.png", plot = figS1, device = "png", path = figures_file_path, units = "cm", width = 18, height = 16, dpi = 600, limitsize = TRUE) 
#dev.off()



########
# Fig S2 [genome size vs continuous traits]
#######

# NUMERICAL VARIABLES

# Fig S2a - cell_diameter

# Prepare data

#Restrict to species with information
sub <- df[!is.na(df$d1_mid) & !is.na(df$genome_size) & !is.na(df$metabolism),]

# Create plot

p2a <- ggplot(sub, aes(x = d1_mid, y = genome_size/1000000)) +
  geom_point(aes(colour = metabolism), alpha = 1, size = 0.8, shape = 16, show.legend = FALSE) +
  #stat_ellipse(aes(col = metabolism), level = 0.95) +
  basic_layout +
  theme(
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    # axis.title.y = element_blank(),
    # axis.text.y = element_blank(),
  ) +
  scale_y_log10() +
  scale_x_log10() +
  annotation_logticks(sides="bl", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_color_manual(values = oxygen_all) +
  #facet_grid(. ~ oxyagg) + 
  labs(x = "Cell radial diameter (??m)", y = labely_genome_size, title = "") +
  guides(colour = guide_legend(title=""))
p2a

# Fig S2b - doubling time

# Prepare data

#Restrict to species with information
sub <- df[!is.na(df$doubling_h) & !is.na(df$genome_size) & !is.na(df$metabolism),]

# Create plot

p2b <- ggplot(sub, aes(x = doubling_h, y = genome_size/1000000)) +
  geom_point(aes(col = metabolism), alpha = 1, size = 0.8, shape = 16, show.legend = FALSE) +
  #stat_ellipse(aes(col = metabolism), level = 0.95) +
  basic_layout +
  theme(
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  scale_y_log10() +
  scale_x_log10() +
  annotation_logticks(sides="bl", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_colour_manual(values = oxygen_all) +
  #facet_grid(. ~ superkingdom) + 
  labs(x = "Doubling time (hr)", y = labely_genome_size, title = "") +
  guides(colour = guide_legend(title=""))
p2b


# Fig S2c - rRNA operons

# Prepare data

#Restrict to species with information
sub <- df[!is.na(df$rRNA16S_genes) & !is.na(df$genome_size) & !is.na(df$metabolism),]

# Create plot


p2c <- ggplot(sub, aes(x = rRNA16S_genes, y = genome_size/1000000)) +
  geom_point(aes(col = metabolism), alpha = 1, size = 0.8, shape = 16) +
  #stat_ellipse(aes(col = metabolism), level = 0.95) +
  basic_layout +
  theme(
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right", 
    legend.direction = "vertical",
    legend.text = element_text(size = 8),
    legend.spacing.y = unit(0, 'mm'),
    legend.key.height = unit(1, 'line'),
    legend.key.width = unit(2, 'mm'),
    legend.key.size = unit(2,"line"),
    legend.box.spacing = unit(2, "mm"),
    legend.margin = margin(t = 0, unit='cm'),
    legend.justification = "top"
  ) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(1,5,10,15)) +
  annotation_logticks(sides="l", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_colour_manual(values = oxygen_all) +
  #facet_grid(. ~ superkingdom) + 
  labs(x = "rRNA 16S operon copy number", y = labely_genome_size, title = "") +
  guides(colour = guide_legend(title="", ncol = 1))
p2c


figS2 <- ggarrange(p2a,p2b,p2c, ncol = 3, widths = c(1.1, 0.9, 1.45), labels="AUTO")
figS2

ggsave(filename = "FigS2.png", plot = figS2, device = "png", path = figures_file_path, units = "cm", width = 18, height = 7, dpi = 600, limitsize = TRUE) 
#dev.off()



########
# Fig S3 [genome size and metabolism within phylogenetic groups]
########

df2 <- df %>% filter(!is.na(genome_size) & !is.na(oxyagg) & oxyagg != "both")


# Fig S3a - Major clades within phylum Proteobacteria

# Prepare data

#Restrict to group
sub <- df2 %>% filter(phylum == "Proteobacteria") %>% 
  group_by(class) %>%
  summarise(n = n()) %>% 
  filter(n >= 5) %>%
  inner_join(df2, by = "class") %>%
  mutate(group = class)
  
sub$group[!(sub$group %in% c("Gammaproteobacteria","Alphaproteobacteria"))] <- "Other classes" 

#Get count of observations for each box
n <- sub %>% group_by(group, oxyagg) %>%
  summarise(n = n())

#Set 'other' as last factor level
sub$group <- factor(sub$group, levels = c(unique(sub$group[!(sub$group == "Other orders")]),"Other classes"))

# Create plot

p3a <- ggplot(sub, aes(x = group, y = genome_size/1000000)) +
  stat_boxplot(geom ='errorbar', width = 0.3, aes(col = oxyagg), position=position_dodge(0.8), show.legend = FALSE) +
  geom_point(aes(col = oxyagg), position = position_jitterdodge(dodge.width = 0.8,jitter.width = 0.3), alpha= 0.4, size = 1, stroke = 0.4, shape = 1, show.legend = FALSE) +
  geom_boxplot(aes(fill = oxyagg), color = "black", position=position_dodge(0.8), outlier.shape = NA, width = 0.4, show.legend = FALSE) +
  basic_layout +
  theme(
    axis.title.x = element_blank(),
    #axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.background = element_blank(),
    legend.position = c(0.21, 0.95), 
    legend.direction = "vertical",
    legend.key.size = unit(0.7,"line"),
    legend.spacing.x = unit(1, 'mm')
  ) +
  scale_y_log10(limits = c(0.4,15))+
  annotation_logticks(sides="l", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_colour_manual(values = c("#000000","#000000")) +
  scale_fill_manual(values = oxygen) +
  geom_text(data = n, aes(label = n, y = 0.4, colour = oxyagg), position=position_dodge(width = 0.8), size = 2.5, family = font_family, show.legend = FALSE) +
  labs(x = labelx_metabolism, y = labely_genome_size, title = "") 
p3a


# Fig S3b - Major classes within phylum Firmicutes

# Prepare data

#Restrict to group
sub <- df2 %>% filter(phylum == "Firmicutes") %>% 
  group_by(class) %>%
  summarise(n = n()) %>% 
  filter(n >= 5) %>%
  inner_join(df2, by = "class") %>%
  mutate(group = class)

#Get count of observations for each box
n <- sub %>% group_by(group, oxyagg) %>%
  summarise(n = n())

# Create plot

p3b <- ggplot(sub, aes(x = group, y = genome_size/1000000)) +
  stat_boxplot(geom ='errorbar', width = 0.3, aes(col = oxyagg), position=position_dodge(0.8), show.legend = FALSE) +
  geom_point(aes(col = oxyagg), position = position_jitterdodge(dodge.width = 0.8,jitter.width = 0.3), alpha= 0.4, size = 1, stroke = 0.4, shape = 1, show.legend = FALSE) +
  geom_boxplot(aes(fill = oxyagg), color = "black", position=position_dodge(0.8), outlier.shape = NA, width = 0.4, show.legend = FALSE) +
  basic_layout +
  theme(
    axis.title.x = element_blank(),
    #axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.21, 0.95), 
    legend.direction = "vertical",
    legend.key.size = unit(0.7,"line"),
    legend.spacing.x = unit(1, 'mm')
  ) +
  scale_y_log10(limits = c(0.4,15))+
  annotation_logticks(sides="l", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_colour_manual(values = c("#000000","#000000")) +
  scale_fill_manual(values = oxygen) +
  geom_text(data = n, aes(label = n, y = 0.4, colour = oxyagg), position=position_dodge(width = 0.8), size = 2.5, family = font_family, show.legend = FALSE) +
  labs(x = labelx_metabolism, y = labely_genome_size, title = "") 
p3b


# Fig S3c - Major orders in class Bacteroidia

# Prepare data

#Restrict to group
sub <- df2 %>% filter(class == "Bacteroidia") %>% 
  group_by(order) %>%
  summarise(n = n()) %>% 
  filter(n >= 5) %>%
  inner_join(df2, by = "order") %>%
  mutate(group = order)

sub$group[!(sub$group %in% c("Bacteroidales","Chitinophagales","Cytophagales","Flavobacteriales","Sphingobacteriales"))] <- "Other orders" 

#Get count of observations for each box
n <- sub %>% group_by(group, oxyagg) %>%
  summarise(n = n())

#Set 'other' as last factor level
sub$group <- factor(sub$group, levels = c(unique(sub$group[!(sub$group == "Other orders")]),"Other orders"))

# Create plot

p3c <- ggplot(sub, aes(x = group, y = genome_size/1000000)) +
  stat_boxplot(geom ='errorbar', width = 0.3, aes(col = oxyagg), position=position_dodge(0.8), show.legend = FALSE) +
  geom_point(aes(col = oxyagg), position = position_jitterdodge(dodge.width = 0.8,jitter.width = 0.3), alpha= 0.4, size = 1, stroke = 0.4, shape = 1, show.legend = FALSE) +
  geom_boxplot(aes(fill = oxyagg), color = "black", position=position_dodge(0.8), outlier.shape = NA, width = 0.4, show.legend = FALSE) +
  basic_layout +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.background = element_blank(),
    legend.position = c(0.21, 0.95), 
    legend.direction = "vertical",
    legend.key.size = unit(0.7,"line"),
    legend.spacing.x = unit(1, 'mm')
  ) +
  scale_y_log10(limits = c(0.4,15))+
  annotation_logticks(sides="l", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_colour_manual(values = c("#000000","#000000")) +
  scale_fill_manual(values = oxygen) +
  geom_text(data = n, aes(label = n, y = 0.4, colour = oxyagg), position=position_dodge(width = 0.8), size = 2.5, family = font_family, show.legend = FALSE) +
  labs(x = labelx_metabolism, y = labely_genome_size, title = "") 
p3c


# Fig S3d - Major orders in class Actinobacteriales

# Prepare data

class_count <- df2 %>% filter(phylum == "Actinobacteriota") %>% 
  group_by(class) %>%
  summarise(n = n())

#Restrict to group
sub <- df2 %>% filter(class == "Actinobacteria") %>% 
  group_by(order) %>%
  summarise(n = n()) %>% 
  filter(n >= 5) %>%
  inner_join(df2, by = "order") %>%
  mutate(group = order)

sub$group[!(sub$group %in% c("Actinomycetales","Mycobacteriales","Streptomycetales","Streptosporangiales","Propionibacteriales"))] <- "Other orders" 

#Get count of observations for each box
n <- sub %>% group_by(group, oxyagg) %>%
  summarise(n = n())

#Set 'other' as last factor level
sub$group <- factor(sub$group, levels = c(unique(sub$group[!(sub$group == "Other orders")]),"Other orders"))

# Create plot

p3d <- ggplot(sub, aes(x = group, y = genome_size/1000000)) +
  stat_boxplot(geom ='errorbar', width = 0.3, aes(col = oxyagg), position=position_dodge(0.8), show.legend = FALSE) +
  geom_point(aes(col = oxyagg), position = position_jitterdodge(dodge.width = 0.8,jitter.width = 0.3), alpha= 0.4, size = 1, stroke = 0.4, shape = 1, show.legend = FALSE) +
  geom_boxplot(aes(fill = oxyagg), color = "black", position=position_dodge(0.8), outlier.shape = NA, width = 0.4, show.legend = FALSE) +
  basic_layout +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.21, 0.95), 
    legend.direction = "vertical",
    legend.key.size = unit(0.7,"line"),
    legend.spacing.x = unit(1, 'mm')
  ) +
  scale_y_log10(limits = c(0.4,15))+
  annotation_logticks(sides="l", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_colour_manual(values = c("#000000","#000000")) +
  scale_fill_manual(values = oxygen) +
  geom_text(data = n, aes(label = n, y = 0.4, colour = oxyagg), position=position_dodge(width = 0.8), size = 2.5, family = font_family, show.legend = FALSE) +
  labs(x = labelx_metabolism, y = labely_genome_size, title = "") 
p3d


#Save as combined figures
figS3 <- ggarrange(p3a,p3b,p3c,p3d, ncol = 2, nrow = 2,  heights = c(1, 1, 1, 1), labels="AUTO")
figS3


ggsave(filename = "FigS3.png", plot = figS3, device = "png", path = figures_file_path, units = "cm", width = 18, height = 16, dpi = 600, limitsize = TRUE) 
#dev.off()


########
# Fig S4 [genome size Vs COGs] CELLULAR PROCESSES AND SIGNALING
########

#Only include oxyagg 
m2 <-filter(cog, !is.na(oxyagg) & oxyagg %in% c("aerobic","anaerobic"))
m2$oxyagg <- factor(m2$oxyagg)

create_y_title <- function(cog_names,COG) {
  y <- as.character((cog_names$short_name[cog_names$cog %in% COG]))
  if(length(y)>1) {
    title <- paste(y,collapse=" + ")
  } else {
    title <- y
  }
  return(title)
}


#A - Signal transduction (T)

COGS <- c("T")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p4a <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    #strip.background = element_blank(), 
    #strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p4a


#B - Post transcriptional, protein turnover etc. (O)

COGS <- c("O")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p4b <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    #strip.background = element_blank(), 
    #strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p4b


#C - Cell trafficking secreetion and cesible trans (U)

COGS <- c("U")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p4c <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p4c


#D - Motility (N)

COGS <- c("N")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

#Exclude species with 0 genes in cog to avoid infinite values in centroid
m2 <- m2 %>% filter(m2$N > 0)

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p4d <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p4d


#E - Cell wal, membrane / envelope biogen. (M)

COGS <- c("M")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p4e <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank()
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p4e


#F - Defense mechanism (V)

COGS <- c("V")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p4f <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank()
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p4f


# Combine figure into one
figS4 <- ggarrange(p4a,p4b,p4c,p4d,p4e,p4f, ncol = 2, nrow = 3, labels="AUTO")
figS4

ggsave(filename = "FigS4.png", plot = figS4, device = "png", path = figures_file_path, units = "cm", width = 18, height = 18, dpi = 600, limitsize = TRUE) 
#dev.off()



# Fig S5 [genome size Vs COGs] INFORMATION STORAGE AND PROCESSING
# Runs on same as above (m2)


#A - Translation, ribsomal structure and biogenesis (J)

COGS <- c("J")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p5a <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    #strip.background = element_blank(), 
    #strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10(limits = c(55,300)) + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p5a


#B - Replication, recombination and repair (L)

COGS <- c("L")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p5b <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p5b


#C - Transcription (K)

COGS <- c("K")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p5c <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank()
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p5c


# Combine figure into one
figS5 <- ggarrange(p5a,p5b,p5c, ncol = 1, nrow = 3, labels="AUTO")
figS5

ggsave(filename = "FigS5.png", plot = figS5, device = "png", path = figures_file_path, units = "cm", width = 9, height = 18, dpi = 600, limitsize = TRUE) 
#dev.off()



# Fig S6 [genome size Vs COGs] METABOLISM
# Runs on same as above (m2)


#A - Amino acid transport and metabolism (E)

COGS <- c("E")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p6a <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    #strip.background = element_blank(), 
    #strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p6a


#B - Carbohydrate transport and metabolism (G)

COGS <- c("G")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p6b <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    #strip.background = element_blank(), 
    #strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p6b


#C - Lipid transport and metabolism (I)

COGS <- c("I")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p6c <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p6c


#D - Nucleotide transport and metabolism (F)

COGS <- c("F")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p6d <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 1, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p6d


#E - Coenzyme transport and metabolism (H)

COGS <- c("H")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p6e <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 1, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p6e


#F - Inorganic ion transport and metabolism (P)

COGS <- c("P")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p6f <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p6f



#G - Secondary metabolite biosynthesis, transport and metabolism (P)

COGS <- c("Q")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

#Exclude species with 0 genes in cog to avoid infinite values in centroid
m2 <- m2 %>% filter(m2$Q > 0)

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p6g <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank()
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p6g


#H - Energy production / convertion (C)

COGS <- c("C")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

#Exclude species with 0 genes in cog to avoid infinite values in centroid
m2 <- m2 %>% filter(m2$C > 0)

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p6h <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank()
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p6h


# Combine figure into one
figS6 <- ggarrange(p6a,p6b,p6c,p6d,p6e,p6f,p6g,p6h, ncol = 2, nrow = 4, labels="AUTO")
figS6

ggsave(filename = "FigS6.png", plot = figS6, device = "png", path = figures_file_path, units = "cm", width = 18, height = 24, dpi = 600, limitsize = TRUE) 
#dev.off()



# Fig S7 [genome size Vs COGs] POORLY CHARACTERIZED
# Runs on same as above (m2)


#A - General function (R)

COGS <- c("R")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p7a <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    #strip.background = element_blank(), 
    #strip.text = element_blank(),
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p7a


#B - Unknown function (S)

COGS <- c("S")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- create_y_title(cog_names,COGS)

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p7b <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    #strip.background = element_blank(), 
    #strip.text = element_blank(),
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p7b


# Combine figure into one
figS7 <- ggarrange(p7a,p7b, ncol = 2, nrow = 1, labels="AUTO")
figS7

ggsave(filename = "FigS7.png", plot = figS7, device = "png", path = figures_file_path, units = "cm", width = 18, height = 6, dpi = 600, limitsize = TRUE) 
#dev.off()


# Fig S8 [genome size Vs COGs AND MIST] MIXED
# Runs on same as above (m2) + MIST


#A - General function and unknown (R + S)

COGS <- c("R","S")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- "General + Unknown function"

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p8a <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    #strip.background = element_blank(), 
    #strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p8a


#B - Translation, ribosomal structure and biogenesis, transcription, replication, recombination and repair (J+K+L)

COGS <- c("J","K","L")

m2$COGgroup <- NA
if(length(COGS)>1) {
  m2$COGgroup <- as.numeric(rowSums(m2[,COGS]))
} else {
  m2$COGgroup <- m2[,COGS]
}

y_title <- "Transcr. Transl. Repl. Repair"

centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logCOG=mean(log10(COGgroup), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_COG=(10^mean_logCOG))
centroids 

p8b <- ggplot(m2, aes(x = genome_size/1000000, y = COGgroup, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    #strip.background = element_blank(), 
    #strip.text = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p8b


#C - Mist one-component signalling systems in relation to genome size

y_title <- "One-component signal. modes"

ms2 <- filter(ms, !is.na(oxyagg) & oxyagg %in% c("aerobic","anaerobic") & !is.na(genome_size))
ms2$oxyagg <- factor(ms2$oxyagg)

centroids <- ms2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logsignal=mean(log10(ocp))) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_signal=(10^mean_logsignal))
centroids 

p8c <- ggplot(ms2, aes(x = genome_size/1000000, y = ocp, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_signal, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank()
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p8c


#D - Mist two-component signalling systems in relation to genome size

y_title <- "Two-component signal. modes"

ms2 <- filter(ms, !is.na(oxyagg) & oxyagg %in% c("aerobic","anaerobic") & !is.na(genome_size))
ms2$oxyagg <- factor(ms2$oxyagg)

#Exclude cases where tcp_total = 0 (62 cases)
ms2 <- ms2 %>% filter(tcp_total > 0)

centroids <- ms2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logsignal=mean(log10(tcp_total))) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_signal=(10^mean_logsignal))
centroids 

p8d <- ggplot(ms2, aes(x = genome_size/1000000, y = tcp_total, col = oxyagg)) +
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_signal, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank()
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() + 
  scale_x_log10() +
  scale_color_manual(values = oxygen) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = y_title, title = "")
p8d


# Combine figure into one
figS8 <- ggarrange(p8a,p8b,p8c,p8d, ncol = 2, nrow = 2, heights = c(1,1,1.2,1.2), labels="AUTO")
figS8

ggsave(filename = "FigS8.png", plot = figS8, device = "png", path = figures_file_path, units = "cm", width = 18, height = 12, dpi = 600, limitsize = TRUE) 
#dev.off()



########
# Fig S9 [Frequency distribution of withing species coefficient of variation for genome size]
########


gscv <- df %>% filter(genome_size.count > 10)
gscv$genome_size.CV <- 100*gscv$genome_size.stdev/gscv$genome_size

p9 <- ggplot(gscv, aes(x=genome_size.CV)) +
  geom_histogram(binwidth=1, colour="black", fill="white") +
  basic_layout +
  scale_x_continuous(breaks=seq(0,60,5)) +
  xlab("Genome size coefficient of variation") +
  ylab("Number of species")
p9

ggsave(filename = "FigS9.png", plot = p9, device = "png", path = figures_file_path, units = "cm",
       width = fig_two_panel_width, height = fig_one_panel_height, dpi = 600, limitsize = TRUE)
#dev.off()

#GSsupp4cum.png is a cumulative freq dist of coefficients of variation (as %) for genome size, using only species with 10 or more records. Need to subset and calculate the CV first, as in 
# pcum1 <- ggplot(gscv, aes(genome_size.CV)) + stat_ecdf(geom = "step") +
#   scale_x_continuous(breaks=seq(0,60,5)) +
#   scale_y_continuous(breaks=seq(0,1,0.2)) +
#   xlab("genome size coefficient of variation (%)") +
#   ylab("cumulative fraction of species")
# ggsave(filename = "figs/GSsupp4cum.png", plot=pcum1,
#        width = 12, height = 8, units = "cm", dpi = 300, 
#        bg = "white")
# #dev.off()

rm(n,centroids)
rm(list=ls(pattern = "^[fig]*[1-9]+"))
rm(list=ls(pattern = "^[p]*[1-9]+"))
