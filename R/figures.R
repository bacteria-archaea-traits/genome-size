##################################################################################
# Create main figures
#
# "Genome size, oxygen use and ecological strategy across bacteria and archaea"  
# Nielsen DA , Fierer N , Geoghegan JL, Gillings MR, Gumerov V , Madin JS, 
# Moore L , Paulsen IT, Reddy TBK , Tetu SG, Westoby M*
#
# *Corresponding Author
#
##################################################################################


#######
# Fig 1 [a) Distribution of genome sizes]
#######

# Prepare data

#Restrict to species with metabolism information
sub <- df %>% filter(!is.na(metabolism) & !is.na(genome_size))

#Get count of observations for each box
n <- sub %>% group_by(superkingdom,metabolism) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  add_row(superkingdom = "Archaea", metabolism = "microaerophilic", n = 0)

#Add a 0 for archaea microaerophilic (this seems to cause an odd split in the plot.. find out why)
# add <- n[n$metabolism == "microaerophilic",]
# add$superkingdom <- " Archaea"
# add$n <- 0
# n <- n %>% bind_rows(add)

# Create plot

#Generate axis label
#labely <- expression(paste("Genome size (x10"^" 6",")"))
labely <- "Genome size (Mbp)"

#Check outliers in boxplots
# data <- sub %>% filter(metabolism == "obligate aerobic" & superkingdom == "Bacteria")
# vals <- boxplot(data$genome_size)
# length(vals$out)
# min <- vals$stats[1]
# max <- vals$stats[5]
# get <- data[data$genome_size < min | data$genome_size > max,]


# Fig 1a

# boxplot with all 6 metabolic types                       
p1a <- ggplot(sub, aes(x = metabolism, y = genome_size/1000000)) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_jitter(data=sub, width = 0.3, alpha= 0.3, size=1, stroke = 0.4, shape = 1, show.legend = FALSE) +
  geom_boxplot(aes(fill = oxyagg), colour = "black", outlier.shape = NA, width = 0.3, show.legend = FALSE) +
  basic_layout +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    ) +
  scale_y_log10() +
  annotation_logticks(sides="l", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_fill_manual(values = oxygen) +
  geom_text(data = n, aes(label = n, y = 0.4), size = 2.5, family = font_family) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Oxygen use", y = labely, title = "")
p1a


# Fig 1b

labelx <- expression("Growth temperature "( degree*C))

sub <- df %>% filter(!is.na(oxyagg) & !is.na(genome_size) & oxyagg %in% c("aerobic","anaerobic") & !is.na(growth_tmp))
#sub$oxyagg <- factor(sub$oxyagg, levels = c("anaerobic","aerobic"))

#the following fits for intercepts and slopes are from the model with interactions removed
ll1 <- data.frame(superkingdom = c("Archaea", "Bacteria"), Z1 = c(0.747,  0.8005), Z2=c(-0.00495, -0.00495))
ll2 <- data.frame(superkingdom = c("Archaea", "Bacteria"), Z1 = c(0.620, 0.6735), Z2=c(-0.00495, -0.00495))

p1b <- ggplot(sub, aes(x = growth_tmp, y = genome_size/1000000, col = oxyagg)) +

  geom_point(data = sub[sub$oxyagg == "aerobic",], shape = 20, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_point(data = sub[sub$oxyagg == "anaerobic",], shape = 20, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  
  geom_abline(data = ll1, mapping=aes(intercept = Z1, slope = Z2)) +
  geom_abline(data = ll2, mapping=aes(intercept = Z1, slope = Z2), linetype = "dashed") +
  
  basic_layout +
  #theme(plot.margin=unit(c(1,1,-0.5,1),"cm")) + #c(bottom,left,top,right)
  
  annotation_logticks(sides="l", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10() +
  
  scale_color_manual(values = oxygen) +
  
  facet_grid(. ~ superkingdom) + 
  labs(x = labelx, y = labely, title = "")
p1b



#Save as combined figures
fig1 <- ggarrange(p1a, p1b, ncol = 1, nrow = 2,  heights = c(1.4, 1), labels="AUTO")
fig1

ggsave(filename = "Fig1.png", plot = fig1, device = "png", path = figures_file_path, units = "cm", width = fig_two_panel_width, height = 18, dpi = 600, limitsize = FALSE) 
# dev.off()

rm(p1a,p1b)

#######
# Fig 2 [Relationships of genome size to temperature and oxyagg]
#######


# Fig 2a

sub <- df %>% filter(!is.na(oxyagg) & !is.na(genome_size) & oxyagg %in% c("aerobic","anaerobic"))

#Get best represented phyla - each with more than 5 representatives in each oxyagg
#Use this list to mark organisms for inclusion in the origianl data frame
phyl <- sub %>% group_by(phylum,oxyagg) %>% 
  summarise(n = n()) %>%
  filter(n > 5 & !grepl("_",phylum)) %>%
  summarise(n = n()) %>%
  filter(n == 2) %>% 
  inner_join(sub, by = "phylum") %>%
  group_by(phylum) %>%
  summarise(n = n()) %>%
  filter(n > 40)

sub <- sub %>% group_by(superkingdom) %>% 
  mutate(group = ifelse(phylum %in% phyl$phylum, phylum, ifelse(superkingdom == "Bacteria","Other Bacteria","Other Archaea")))

#Get count of observations for each box
n <- sub %>% group_by(group,oxyagg) %>%
  summarise(n = n())

#Set factor levels
sub$group <- factor(sub$group, levels = c(sort(unique(sub$group[!grepl("Other",sub$group)])),"Other Archaea","Other Bacteria"))

p2a <- ggplot(sub, aes(x = group, y = genome_size/1000000)) +
  stat_boxplot(geom ='errorbar', width = 0.3, aes(col = oxyagg), position=position_dodge(0.9), show.legend = FALSE) +
  geom_point(aes(col = oxyagg), position = position_jitterdodge(dodge.width = 0.9,jitter.width = 0.3), alpha= 0.4, size = 1, stroke = 0.4, shape = 1, show.legend = FALSE) +
  geom_boxplot(aes(fill = oxyagg), color = "black", position=position_dodge(0.9), outlier.shape = NA, width = 0.7, show.legend = FALSE) +
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
  labs(x = "", y = labely, title = "") 
p2a


# Fig 2b

# Prepare data

# Get and attach habitat information
sub <- merge(df, hab, by.x="isolation_source", by.y="Type")
sub$CSadj <- factor(sub$CSadj, levels = c("thermal", "gut", "oral", "host_internal", "marine_sediment", "marine_water", "fresh_sediment", "fresh_water","soil"))

#Get only rows with environment and oxygen information
sub <- filter(sub, !is.na(CSadj) & !is.na(oxyagg) & oxyagg %in% c("aerobic","anaerobic"))

#calculate means and medians across both oxygen levels 
#dplyr::summarize needs to be specified because other loaded packages also have versions of summarize with different grammar
m <- select(sub, CSadj, genome_size)

aggm <-m %>% 
  group_by(CSadj) %>%
  #next line calculates means on log-scaled basis then restores to arithmetic scale 
  dplyr::summarize(mean_genome = 10^mean(log10(genome_size), na.rm = TRUE),
                   median_genome = median(genome_size, na.rm = TRUE),
                   sd_genome =sd(genome_size, na.rm = TRUE))

# Create plot

p2b <- ggplot(sub, aes(x = CSadj, y = genome_size/1000000)) +
  
  geom_point(aes(col = oxyagg), position = position_jitterdodge(dodge.width = 0.9), alpha= 0.4, size = 1, stroke = 0.4, shape = 1, show.legend = FALSE) +
  
  stat_boxplot(geom ='errorbar', width = 0.3, aes(col = oxyagg), position=position_dodge(0.9),  show.legend = FALSE) +
  geom_boxplot(aes(fill = oxyagg), colour = "black",  position=position_dodge(0.9), outlier.shape = NA, width = 0.7, show.legend = FALSE) +
  geom_line(data=aggm, aes(x=CSadj, y=mean_genome/1000000, group=1), linetype = "solid", colour = "red", size = 1, show.legend = FALSE) +
  geom_point(data=aggm, aes(x=CSadj, y=mean_genome/1000000, group=1), colour = "black", size = 3, shape = 21, fill = "red", show.legend = FALSE) +
  
  basic_layout +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  
  scale_y_log10() +
  annotation_logticks(sides="l", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +

  scale_color_manual(values = c("black","black")) +
  scale_fill_manual(values = oxygen) +
 
  labs(x = "Habitat group", y = labely, title = "")
p2b


# Save plot

#Save as combined figures
fig2 <- ggarrange(p2a, p2b, ncol = 1, nrow = 2,  heights = c(1, 1), labels="AUTO")
fig2

ggsave(filename = "Fig2.png", plot = fig2, device = "png", path = figures_file_path, units = "cm", width = fig_two_panel_width, height = fig_two_panel_height, dpi = 600, limitsize = TRUE) 
# dev.off()

rm(p2a,p2b,sub)

#######
# Fig 3 [Relationship between MIST gene counts and genome size]
#######


# Fig 3a [MIST]

# General processing

ms2 <- filter(ms, !is.na(oxyagg) & oxyagg %in% c("aerobic","anaerobic") & !is.na(genome_size))
ms2$oxyagg <- factor(ms2$oxyagg)


# Prepare data

#calculating centroids for the aerobic and anaerobic clouds
centroids <- ms2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_logsize=mean(log10(genome_size)), mean_logsignal=mean(log10(majormodes_total))) %>%
  mutate(mean_size = (10^mean_logsize)/1000000) %>%
  mutate(mean_signal=(10^mean_logsignal))
centroids 


# Create plot

p3a <- ggplot(ms2, aes(x = genome_size/1000000, y = majormodes_total, col = oxyagg)) +
  
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_signal, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10(limits = c(10,3000)) + 
  scale_x_log10() +
  scale_color_manual(values = c("#FF8C19","#2471a3")) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = "Genes in major signalling modes", title = "")

p3a

# Fig 3b [COG] (Note, data frame cog already exists)

# Prepare data

m2 <- filter(cog, !is.na(oxyagg) & oxyagg %in% c("aerobic","anaerobic"))
m2$oxyagg <- factor(m2$oxyagg)

#Create combined gene count column
m2$EGPHI <- as.numeric(NA)
m2$EGPHI <- m2$E+m2$G+m2$P+m2$H+m2$I

#Calculate centroids
centroids <- m2 %>%
  group_by(superkingdom, oxyagg) %>%
  summarise(mean_size=mean(log10(genome_size)), mean_COG=mean(log10(EGPHI), na.rm=TRUE)) %>%
  mutate(mean_size = (10^mean_size)/1000000) %>%
  mutate(mean_COG=(10^mean_COG))
centroids 

# Create plot

p3b <- ggplot(m2, aes(x = genome_size/1000000, y = EGPHI, col = oxyagg)) +
  
  geom_point(shape = 1, alpha= 0.5, size=0.6, show.legend = FALSE) + 
  geom_line(data = centroids, colour = "black", arrow=arrow(ends = "last", type = "closed", length = unit(0.1, "cm")), size = 0.8, aes(x = mean_size, y = mean_COG, group = 1)) +
  geom_abline(intercept = 2, slope = 1) +
  basic_layout +
  theme(
    strip.background = element_blank(), 
    strip.text = element_blank()
  ) +
  annotation_logticks(sides = "lb", short = unit(1,"mm"), mid = unit(1,"mm"), long = unit(1,"mm")) +
  scale_y_log10(limits = c(10,3000)) + 
  scale_x_log10() +
  scale_color_manual(values = c("#FF8C19","#2471a3")) +
  facet_grid(. ~ superkingdom) + 
  labs(x = "Genome size (Mbp)", y = "Transport and metabolism", title = "")

p3b

# Save plot

#Save as combined figures
fig3 <- ggarrange(p3a, p3b, ncol = 1, nrow = 2,  heights = c(1, 1), labels="AUTO")
fig3

ggsave(filename = "Fig3_mean_mist.png", plot = fig3, device = "png", path = figures_file_path, units = "cm", width = fig_two_panel_width, height = 14, dpi = 600, limitsize = TRUE) 
# dev.off()

rm(n,centroids)
rm(list=ls(pattern = "^[fig]*[1-9]+"))
rm(list=ls(pattern = "^[p]*[1-9]+"))
