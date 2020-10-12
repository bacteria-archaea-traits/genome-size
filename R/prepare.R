##################################################################################
# Prepare data
#
# "Genome size, oxygen use and ecological strategy across bacteria and archaea"  
# Nielsen DA , Fierer N , Geoghegan JL, Gillings MR, Gumerov V , Madin JS, 
# Moore L , Paulsen IT, Reddy TBK , Tetu SG, Westoby M*
#
# *Corresponding Author
#
##################################################################################

report("Proessing data and general settings",TRUE)

#Note: If dev.off doesn't work, print dev.set(dev.next()) a number of times

# Define input and output paths

figures_file_path <- "output/figures"
stats_file_path <- "output/stats"
data_file_path <- "data"



#############
# Load data #
#############

report("Loading data")

#Main phenotypic data
df <- read.csv(sprintf("%s/madin_et_al/condensed_species_GTDB[NCBI_fill].csv",data_file_path), as.is=TRUE)
df <- df %>% filter(!is.na(superkingdom))
report("Main data loaded")

#Cog categorisation of genes from refseq sequences
cog <- read.csv(sprintf("%s/oneCOGcounts2018-8-6.csv",data_file_path), as.is=TRUE) 
#Get cog name translations
cog_names <- read.csv(sprintf("%s/cog_naming.csv",data_file_path), as.is=TRUE)
report("COG data loaded")

#Data on transporters from MIST
ms <- read.csv(sprintf("%s/mist01022019.csv",data_file_path), as.is=TRUE)
report("MIST data loaded")

#Get environment information
hab <- read.csv(sprintf("%s/madin_et_al/conversion_tables/environments.csv",data_file_path), as.is=TRUE)
report("Habitat data loaded")

# Load raw NCBI taxonomy tables: takes a while but only done once
# These tables match species names to taxonomy ids and enable merging
# across data sets despite variation in organism naming

report("Loading NCBI phylogenetic data. This takes a bit..")
nam <- read.csv(sprintf("%s/madin_et_al/taxonomy_names.csv",data_file_path), as.is=TRUE)
nam <- nam[, c("tax_id","name_txt")] #Removes columns that are not needed
tax <- read.csv(sprintf("%s/madin_et_al/ncbi_taxmap.csv",data_file_path), as.is=TRUE)
tax <- unique(tax[, names(tax)]) #Removes duplicate entries

report("Done")



################
# Prepare data #
################

report("Preparing data")

# Prepare main data #
#####################

report("Preparing main data")

# Remove all intracellular organisms
df <- df %>% filter(is.na(df$intracellular))
report("-> Removed intracellular prokaryotes")

# Order data in output by setting levels manually
df$metabolism <- factor(df$metabolism, levels = c("obligate aerobic", "aerobic",  "facultative", "microaerophilic", "anaerobic","obligate anaerobic"))

#Create oxyagg column
df$oxyagg <- NA
df$oxyagg[!is.na(df$metabolism) & df$metabolism %in% c("anaerobic", "obligate anaerobic")] <- "anaerobic"
df$oxyagg[!is.na(df$metabolism) & df$metabolism %in% c("aerobic", "obligate aerobic")] <- "aerobic"
df$oxyagg[!is.na(df$metabolism) & df$metabolism %in% c("facultative", "microaerophilic")] <- "both"
df$oxyagg <- factor(df$oxyagg)

#Simplified terminology
df$oxysim <- NA
df$oxysim[!is.na(df$metabolism) & df$metabolism %in% c("anaerobic", "obligate anaerobic")] <- "no"
df$oxysim[!is.na(df$metabolism) & df$metabolism %in% c("aerobic", "obligate aerobic")] <- "yes"
df$oxysim[!is.na(df$metabolism) & df$metabolism %in% c("facultative", "microaerophilic")] <- "both"
df$oxysim <- factor(df$oxysim)

#Create shapeagg column
df$shapeagg <- df$cell_shape
df$shapeagg[!is.na(df$shapeagg) & df$shapeagg %in% c("coccus","coccobacillus")] <- "spheroid"
df$shapeagg[!is.na(df$shapeagg) & df$shapeagg %in% c("bacillus")] <- "rod"
df$shapeagg[!is.na(df$shapeagg) & df$shapeagg %in% c("pleomorphic","filament", "star", "spiral", "vibrio", "irregular", "flask", "spindle", "fusiform", "disc", "disc ", "square", "branced", "triangular")] <- NA

#Create motagg column
df$motagg <- df$motility
df$motagg[!is.na(df$motagg) & df$motagg %in% c("axial filament","flagella", "gliding")] <- "yes"

#Change gram stain terminology
df$gram_stain[df$gram_stain %in% c("yes")] <- "positive"
df$gram_stain[df$gram_stain %in% c("no")] <- "negative"

#calculate a mid-diameter d1_mid in radial direction
#this is mean of high and low end of range, where range was given
#where range was not given, d1_mid is the same as d1_lo

df$d1_mid <- ifelse(!is.na(df$d1_up), (df$d1_lo + df$d1_up)/2, df$d1_lo)

report("Done")



# Prepare COG data #
####################

report("Preparing COG data")

#Map tax_ids onto cog using the taxonomy_names.csv:
cog <- cog %>% inner_join(nam, by = c("Species"="name_txt"))

#Map ncbi taxonomy onto the tax ids
cog <- cog %>% inner_join(tax[, c("tax_id","species_tax_id")], by = "tax_id")

# condense to species averages for the important cogs
cog1 <- cog %>%
  group_by(species_tax_id) %>%
  summarise(C = mean(C), D=mean(D), E=mean(E), F=mean(F), G=mean(G), H=mean(H), I=mean(I), J=mean(J), K=mean(K), L=mean(L), M=mean(M), N=mean(N), O=mean(O), P=mean(P), Q=mean(Q), R=mean(R), S=mean(S), T=mean(T), U=mean(U), V=mean(V))

#merge together with condensed species data
cog <- merge(cog1, df, by.x="species_tax_id", by.y="species_tax_id", all=FALSE)

#Clean up
rm(cog1)

report("Done")



# Prepare MIST data #
#####################

report("Preparing MIST data")

#Attach species_tax_id and full phylogeny
ms2 <- inner_join(ms, tax, by = "tax_id")
#Remove rows with no data (majormodes_total = 0 AND chemotaxis_total = 0)
ms3 <- ms2[!(ms2$majormodes_total == 0 & ms2$chemotaxis_total == 0),]

#tcp.chemotaxis is interpreted as logical
ms3$tcp.chemotaxis <- as.integer(ms3$tcp.chemotaxis)

#Calculate average values per species for each gene
ms4 <- ms3 %>% group_by(species_tax_id) %>%
  summarise(ocp = mean(ocp, na.rm = TRUE),
            tcp.hk = mean(tcp.hk, na.rm = TRUE),
            tcp.hhk = mean(tcp.hhk, na.rm = TRUE),
            tcp.rr = mean(tcp.rr, na.rm = TRUE),
            tcp.hrr = mean(tcp.hrr, na.rm = TRUE),
            tcp_total = sum(tcp.hk,tcp.hhk,tcp.rr,tcp.hrr,tcp.chemotaxis,tcp.other, na.rm = TRUE),
            tcp.chemotaxis = mean(tcp.chemotaxis, na.rm = TRUE),
            ecf = mean(ecf, na.rm = TRUE),
            other = mean(other, na.rm = TRUE),
            majormodes_total = mean(majormodes_total, na.rm = TRUE),
            chemotaxis_total = mean(chemotaxis_total, na.rm = TRUE))
ms4[ms4 == "NaN"] <- NA

ms <- inner_join(df,ms4, by = "species_tax_id")

rm(ms2,ms3,ms4)

report("Done")



# Prepare figure settings #
###########################

report("Preparing figure formats")

# Prepare figure formats

# Define fixed formats for all figures
text_size <- 9
text_color <- "black"
font_family <- "sans" #Science prefer Helvetica

plot_line_color <- "black"
plot_line_width <- 0.5 #mm

#Define colour sets
oxygen <- c("#FF8C19","#2471a3","gray")
oxygen_all <- c("red","orange","darkgray","darkgreen","blue", "black")
presence_absence <- c("gray","red","green")

# Define theme standards 

basic_layout <- 
  theme_bw() + 
  theme(
    panel.border = element_rect(color = plot_line_color, size = plot_line_width),
    panel.grid = element_blank(),
    axis.title = element_text(size = text_size, family=font_family, color=text_color),
    axis.text = element_text(size = text_size, family=font_family, color=text_color),
    axis.line = element_blank(),#element_line(color = plot_line_color, size = plot_line_width), #Not needed when having box around plot 
    axis.ticks = element_line(color = plot_line_color, size = plot_line_width)
  )

#Define two main colours to use for all plots
cols <- c("#2471a3","orange")

fig_one_panel_width <- 5.5
fig_one_panel_height <- 8
fig_two_panel_width <- 12
fig_two_panel_height <- 16


#Generate axis label
labely_genome_size <- "Genome size (Mbp)"
labelx_metabolism <- "Oxygen use"


print("Finished preparing data")