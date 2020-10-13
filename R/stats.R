##################################################################################
# Process statistics
#
# "Genome size, oxygen use and ecological strategy across bacteria and archaea"  
# Nielsen DA , Fierer N , Geoghegan JL, Gillings MR, Gumerov V , Madin JS, 
# Moore L , Paulsen IT, Reddy TBK , Tetu SG, Westoby M*
#
# *Corresponding Author
#
##################################################################################

#For testing so I don't have to rerun merger every time I restart
#df <- read.csv("merged.csv")


# Prepare correlation descriptions
# This is used to attach correlation descriptions to output tables
traits <- c("genome_size",
            "superkingdom",
            "oxyagg",
            "growth_tmp",
            "sporulation",
            "motagg",
            "shapeagg",
            "gram_stain",
            "d1_mid",
            "doubling_h",
            "rRNA16S_genes")

Description = c("Genome size (log)",
                 "Bacteria > Archaea",
                 "Aerobic > Anaerobic",
                 "growth temperature",
                 "Sporulating > Not",
                 "Motile > Not",
                 "Shape (rod > spheroid)",
                 "Gram positive > Negative",
                 "Radial diameter (log)",
                 "Doubling time (log)",
                 "rRNA16S gene copies")

descr <- data.frame("trait" = traits)
descr <- cbind(descr, data.frame(Description))


# Prepare data

#First aggregate categoricals and convert into numbers. 
#This requires factors to be converted first to character then to number. 
#Then select and order columns and convert genome size, doubling time 
#and cell diameter to log10. The working dataframe is called datcorr

#Copy main data
datec <- df

#Convert traits to binomial values

#Shapeagg is is either rod or spheroid
datec$shapeagg <- ifelse(datec$shapeagg == "rod",1,0)
datec$shapeagg <- as.numeric(datec$shapeagg)
unique(datec$shapeagg)

#Motagg is either yes or no
datec$motagg <- ifelse(datec$motagg == "yes",1,0)
datec$motagg <- as.numeric(datec$motagg)

#Sporulation is either yes or no
datec$sporulation <- ifelse(datec$sporulation == "yes",1,0)
datec$sporulation <- as.numeric(datec$sporulation)

#Short form of metabolism has three values
datec$oxyagg <- as.character(datec$oxyagg)
datec$oxyagg[!is.na(datec$oxyagg) & datec$oxyagg == "anaerobic"] <- 0
datec$oxyagg[!is.na(datec$oxyagg) & datec$oxyagg == "aerobic"] <- 1
datec$oxyagg[!is.na(datec$oxyagg) & datec$oxyagg == "both"] <- NA
datec$oxyagg <- as.numeric(datec$oxyagg)

#Superkingdom is either Bacteria or Archaea
datec$superkingdom <- ifelse(datec$superkingdom == "Bacteria",1,0)
datec$superkingdom <- as.numeric(datec$superkingdom)

#Gram stain is either postive or negative
datec$gram_stain <- ifelse(datec$gram_stain == "positive",1,0)
datec$gram_stain <- as.numeric(datec$gram_stain)

#Grab data for correlation analysis
datcorr <- datec %>% dplyr::select(genome_size, superkingdom, oxyagg, growth_tmp, sporulation, motagg, shapeagg, gram_stain, d1_mid, doubling_h,  rRNA16S_genes)

#Log (10) transform genome size, doubling time and d1_mid
datcorr$genome_size <- log10(datcorr$genome_size)
datcorr$d1_mid <- log10(datcorr$d1_mid)
datcorr$doubling_h <- log10(datcorr$doubling_h)

#Perform correlation analysis (outputs r values)
t <- cor(datcorr, use = "pairwise.complete.obs", method = c("pearson"))
#Square r values to produce r^2 and convert to data frame
t <- as.data.frame(t^2)
#Reduce decimal places
t <- round(t,3)

#Extract rownames to column
t <- t %>% tibble::rownames_to_column("trait")
#Attach correlation descriptions
t <- t %>% left_join(descr, by = "trait")

#Get n and p values for each correlation
t_stats <- rcorr(as.matrix(datcorr))



# Table 1 - correlations, number of species n, partial correlations and phylogenetic genealised least squares
# correlation between log10 genome size and other traits or categories
#
# Table S3 - Properties of the phylogenetic trees used for estimating phylogenetic generalised least squares
# correlations between log10 genome size and other traits or categories
##################

report("Creating table 1 & S3")

t1 <- t

#Add number of species for trait pair to correlation table
t1$n <- t_stats$n["genome_size",]
#Add P value for correlation 
t1$P <- round(t_stats$P["genome_size",],4)
#Only keep genome_size correlation
t1 <- t1[,c("trait","genome_size","n")]
#Change name of column with r-squared values
t1 <- t1 %>% rename(r2 = "genome_size")
#Remove first row as this shows correlation with self
t1 <- t1 %>% filter(!(trait == "genome_size"))


# Calculate correlations after partialling for superkingdom, metabolism and growth temperature

#in the partial.r command the second c-group lists the variable numbers that correlations are to be partialled by, and the first c-group lists the variable numbers where partial correlation table is to be produced 
tp <- partial.r(datcorr, c("genome_size","sporulation","motagg","shapeagg","gram_stain","d1_mid","doubling_h","rRNA16S_genes"), c("superkingdom","oxyagg","growth_tmp"),  use="pairwise.complete.obs", method=c("pearson"))
#Convert r values to r2
tp <- tp^2

#Attach to main table
t1$correlation_after_partialling_for_kingdom_metabolism_growth_tmp[t1$trait == "sporulation"] <- round(tp["genome_size", "sporulation"],3)
t1$correlation_after_partialling_for_kingdom_metabolism_growth_tmp[t1$trait == "motagg"] <- round(tp["genome_size","motagg"],3)
t1$correlation_after_partialling_for_kingdom_metabolism_growth_tmp[t1$trait == "shapeagg"] <- round(tp["genome_size","shapeagg"],3)
t1$correlation_after_partialling_for_kingdom_metabolism_growth_tmp[t1$trait == "gram_stain"] <- round(tp["genome_size","gram_stain"],3)
t1$correlation_after_partialling_for_kingdom_metabolism_growth_tmp[t1$trait == "d1_mid"] <- round(tp["genome_size","d1_mid"],3)
t1$correlation_after_partialling_for_kingdom_metabolism_growth_tmp[t1$trait == "doubling_h"] <- round(tp["genome_size","doubling_h"],3)
t1$correlation_after_partialling_for_kingdom_metabolism_growth_tmp[t1$trait == "rRNA16S_genes"] <- round(tp["genome_size","rRNA16S_genes"],3)

#Add correlation descriptions
t1 <- t1 %>% left_join(descr, by = "trait")




# Phylogenetic generalised least squares
###

# Prepare data 
report("Preparing data for generalised least squares correlations")

#Select only columns with phylogenetic information
taxa <- subset(df, select = c(superkingdom,phylum,class,order,family,genus,species))
#Remove rows with any NAs
taxa <- taxa[complete.cases(taxa), ]
#Convert all columns to factors (required)
taxa[names(taxa)] <- lapply(taxa[names(taxa)], factor)

#Load modified as.phylo.formula function
#This script overrides the original as.phylo.formula function
#The new version adds branch lengths to each node which is 
#required to run phylolm below.
source("R/as.phylo.formula.R")

#Create phylo tree using formula
frm <- ~superkingdom/phylum/class/order/family/genus/species
#Create phylogenetic tree
report("Creating phylo tree. This may take a while...")
tree <- as.phylo.formula(frm, data = taxa)
report("Done")


#For exploring effect of collapsing
#And use of different functions
################
#source("R/as.phylo.formula.R")
#rm(as.phylo.formula) #Remove function to re-load original
#source("R/packages.R")
# taxa2 <- taxa[1:50,]
# tree <- as.phylo.formula(frm, data = taxa2, collapse = FALSE)
# tree
# plot(tree, type = "cladogram")
# tree <- collapse.singles(tree)
# tree
# plot(tree, type = "cladogram")
################


#Use phylolm to calculate phylogenetic generalised least squares for each variable
#This has to be done on each variable independently, as the function leaves out any 
#record with any missing data

#Start output table by adding trait and description for each row
tableS3 <- descr

r2 <- c()
tips <- c()
nodes <- c()

#Do phylogenetic correction for each trait (except genome size and superkingdom)
#This for loop runs through each trait in tableS3 to calculate least squares 
#one trait at a time (phylolm cannot deal with NA)

dat <- df
#Remove oxyagg value "both" from dat - we only include aerobic and anaerobic
dat$oxyagg[!is.na(dat$oxyagg) & dat$oxyagg == "both"] <- NA
dat$oxyagg <- droplevels(dat$oxyagg)

for(i in 1:nrow(tableS3)) {
  if(!(tableS3$trait[i] %in% c("genome_size","superkingdom"))) {
    
    #Get trait
    this_trait <- as.character(tableS3$trait[i])
    
    #Get data for this trait
    tmp <- dat[, c("species", "genome_size", this_trait)]
    names(tmp) <- c("species", "genome_size", "trait")
    
    #Remove any rows with no data
    tmp <- tmp[!is.na(tmp$genome_size) & !is.na(tmp$trait),]

    #Reduce main tree to contain only species with this trait data (listed in tmp)
    sub_tree <- drop.tip(phy = tree, tree$tip.label[!(tree$tip.label %in% tmp$species)])
    
    #Reduce trait table to only contain organisms that are present in tree
    tmp <- filter(tmp, tmp$species %in% sub_tree$tip.label)
    
    #Add species name to rownames
    rownames(tmp) <- tmp$species
    
    #Run phylogenetic generalised least squares on genome size vs trait
    if(tableS3$trait[i] %in% c("d1_mid","doubling_h")) {
      #d1_mid and doubling_h should be log10 transformed
      phylm <- phylolm(log10(tmp$genome_size) ~ log10(tmp$trait), data = tmp, phy = sub_tree)
    } else {
      phylm <- phylolm(log10(tmp$genome_size) ~ tmp$trait, data = tmp, phy = sub_tree)
    }
    
    #Check residuals
    # qqnorm(phylm$residuals)
    # qqline(phylm$residuals)
    # hist(phylm$residuals)
    
    this_r2 <- phylm$r.squared
    this_tips <- phylm$n
    this_nodes <- sub_tree$Nnode
    
  } else {
    this_r2 <- "-"
    this_tips <- "-"
    this_nodes <- "-"
  }
  
  #Add data to holding vectors
  r2 <- c(r2,this_r2)
  tips <- c(tips,this_tips)
  nodes <- c(nodes,this_nodes)
}

rm(dat)

#Add data from holding vectors to table S3
tableS3 <- cbind(tableS3, data.frame(r2))
tableS3 <- cbind(tableS3, data.frame(tips))
tableS3 <- cbind(tableS3, data.frame(nodes))

#convert "-" to NA
tableS3[tableS3 == "-"] <- NA

#Adjust column names for output
names(tableS3) <- c("trait","Description","PGLS R_squared","Number of tips (species)","Number of nodes")

#Remove trait column
tableS3$trait <- NULL

#Save table S3
write.table(tableS3, file = sprintf("%s/tableS3.csv",stats_file_path), sep = ",", quote = FALSE, row.names = F)


#Add PGLS data to table 1
table1 <- t1 %>% left_join(tableS3[,c("Description","PGLS R_squared")], by = "Description")

#Reorganise table 1
table1$trait <- NULL
table1 <- table1[,c(4,1,2,3,5)]

#Save table 1
write.table(table1, file = sprintf("%s/table1.csv",stats_file_path), sep = ",", quote = FALSE, row.names = F)

report("Done")


# if(memory.size() > 1000) {
#   report("Freeing memory")
#   gc()
# }

# Table S1 - Correlations among log10 genome size and ten other traits or catgories
###################

report("Creating table S1")

# Combine t and t_stats into one table
ts1 <- t
#Get all traits, but exclude the first (genome size)
traits <- ts1$trait
for(i in 1:length(traits)) {
  trait1 <- traits[i]
  for(a in i:length(traits)) {
    trait2 <- traits[a]
    if(trait1 != trait2) {
      ts1[ts1$trait == trait2,trait1] <- t_stats$n[trait1,trait2]
    }
  }
}
#Remove trait column
ts1 <- subset(ts1, select = -trait)
#Move description column to first position
ts1 <- subset(ts1, select = c(Description,1:11))

#Rename columns to same as description
for(i in 1:length(names(ts1))) {
  if(!(names(ts1)[i] == "Description")) {
    #Get description name from descr table
    colnames(ts1)[i] <- descr$Description[descr$trait == names(ts1)[i]]
  }
}

#Save table S1
write.table(ts1, file = sprintf("%s/tableS1.csv",stats_file_path), sep = ",", quote = FALSE, row.names = F)
report("Done")

##############



# Tables S2a and b, regression for Fig 1b. 
# Significance tests for terms of ancova 
##############

report("Running analyses for table S2")

#Table S2a

#fit_all is the general fit to test for all interactions -- corresponds to Table S2a 
#genome size has already been log transformed in datcorr, do not transform again for this model
fit_all <- lm(genome_size ~ superkingdom*oxyagg*growth_tmp, data = datcorr)

#Save model data
sink(sprintf("%s/data_for_tableS2a.txt",stats_file_path))
print(summary(fit_all))
sink()  

#Create table
predictors <- c("Intercept","Bacteria vs archaea","Aerobic vs anaerobic", "Growth temp","Superkingdom vs oxyagg", "Superkingdom vs growth temp", "Oxyagg vs growth temp", "Superkingdom vs oxyagg vs growth temp")
tableS2a <- data.frame("Description" = predictors)
#coefficients
tableS2a <- cbind(tableS2a, data.frame(round(as.numeric(summary(fit_all)$coefficients[, 1]), 4)))
#Standard errors
tableS2a <- cbind(tableS2a, data.frame(round(as.numeric(summary(fit_all)$coefficients[, 2]), 4)))
#t value
tableS2a <- cbind(tableS2a, data.frame(round(as.numeric(summary(fit_all)$coefficients[, 3]), 3)))
#P value
tableS2a <- cbind(tableS2a, data.frame(as.numeric(summary(fit_all)$coefficients[, 4])))
names(tableS2a) <- c("Description","Coefficient","SE","t value","P")

#Save tableS2a
write.table(tableS2a, file = sprintf("%s/tableS2a.csv",stats_file_path), sep = ",", quote = FALSE, row.names = F)



#Table S2b

#fit_reduced with all interactions left out has essentially the same R^2 as the full model, 
#and hence this one will be adopted for Table 2b and for the line fits in Fig 1a 
fit_reduced <- lm(genome_size ~ superkingdom + oxyagg + growth_tmp, data = datcorr)

#Save model data
sink(sprintf("%s/data_for_tableS2b.txt",stats_file_path))
print(summary(fit_reduced))
sink()

#Create table
predictors <- c("Intercept","Bacteria vs archaea","Aerobic vs anaerobic", "Growth temp")
tableS2b <- data.frame("Description" = predictors)
#coefficients
tableS2b <- cbind(tableS2b, data.frame(round(as.numeric(summary(fit_reduced)$coefficients[, 1]),4)))
#Standard errors
tableS2b <- cbind(tableS2b, data.frame(round(as.numeric(summary(fit_reduced)$coefficients[, 2]),4)))
names(tableS2b) <- c("Description","Coefficient","SE")

#Add equivalent percent difference
tableS2b$equivalent_percent_difference <- abs(round((10^tableS2b$Coefficient-1)*100,1))
#Make equivalent percent difference for growth temperature per 10 degrees celsious
tableS2b$equivalent_percent_difference[tableS2b$Description == "Growth temp"] <- tableS2b$equivalent_percent_difference[tableS2b$Description == "Growth temp"]*10
#Add percent sign
tableS2b$equivalent_percent_difference <- sprintf("%.1f %%", tableS2b$equivalent_percent_difference)

#Remove value for intercept which is nonsense
tableS2b$equivalent_percent_difference[tableS2b$Description == "Intercept"] <- NA

#Save tableS2b
write.table(tableS2b, file = sprintf("%s/tableS2b.csv",stats_file_path), sep = ",", quote = FALSE, row.names = F)

report("Done")
##############





# Table S4 - Breakdown of variation in log10 genome size associated with habita groups and oxyagg
# aov does type I SS, working from left to right in model, where lm does type III SS
##############

report("Creating table S4")

#Add environment classification from Cobo-Simon and Tamames.
m1 <- datec %>% inner_join(hab, by = c("isolation_source"="Type"))
#Arrange environments via factor levels
m1$CSadj <- factor(m1$CSadj, levels = c("thermal", "gut", "oral", "host_internal", "marine_sediment", "marine_water", "fresh_sediment", "fresh_water","soil"))
#Only include data with both environment term and metabolism information
m1 <- filter(m1, !is.na(CSadj) & !is.na(oxyagg))

fit1 <- aov(log10(genome_size) ~ CSadj, data = m1)
fit2 <- aov(log10(genome_size) ~ CSadj + oxyagg, data = m1)
fit3 <- aov(log10(genome_size) ~ oxyagg + CSadj, data = m1)

#Create data frame
tableS4 <- data.frame("Description" = as.character(), "sum_of_squares" = as.numeric(), "df" = as.numeric())
tableS4[nrow(tableS4)+1,] <- rbind(c("Habitat group",round(as.numeric(unlist(summary(fit1))["Sum Sq1"]),2),unlist(summary(fit1))["Df1"]))
tableS4[nrow(tableS4)+1,] <- rbind(c("Aerobic vs anaerobic",round(as.numeric(unlist(summary(fit2))["Sum Sq2"]),2),unlist(summary(fit2))["Df2"]))
tableS4[nrow(tableS4)+1,] <- rbind(c("Habitat after aerobic vs anaerobic",round(as.numeric(unlist(summary(fit3))["Sum Sq2"]),2),unlist(summary(fit3))["Df2"]))

#Save table S4
write.table(tableS4, file = sprintf("%s/tableS4.csv",stats_file_path), sep = ",", quote = FALSE, row.names = F)
report("Done")


#Notes on converting to model for linear regression figure 1b
# OK so the slopes should all be the same, they come from the coefficient
# of response to growth temperature. Then for the intercepts, the baseline
# intercept should apply to archaea and to anaerobic, assuming these are
# the zeros in the zero-one contrasts. Then to get the intercept for
# aerobic archaea add the coefficient for aerobe vs anaerobe to the
# baseline intercept. To get the intercept for anaerobic bacteria, add the
# coefficient for bact vs arch to the baseline intercept. And to get the
# intercept for aerobic bacteria add both those coefficients to the
# baseline intercept.



# Extract and save all outliers from figure 1
##############

report("Extract and save outlier organisms in figure 1")

mets <- c("obligate aerobic","aerobic","facultative","microaerophilic","anaerobic","obligate anaerobic")
all <- data.frame()
for(i in 1:length(mets)) {
  sub <- df[!is.na(df$metabolism) & !is.na(df$genome_size) & df$metabolism == mets[i],]
  #Produce boxplot statistics
  vals <- boxplot(data = sub, main = "NX", log(genome_size,10) ~ metabolism, las = 2)
  #Grab species
  get <- df[!is.na(df$genome_size) & !is.na(df$metabolism) & log(df$genome_size,10) %in% vals$out,]
  if(nrow(all)>0) {
    all <- all %>% bind_rows(get)
  } else {
    all <- get
  }
}
all <- all %>% dplyr::select(species_tax_id,species,genome_size,metabolism,isolation_source)
all <- all %>% left_join(tax[!duplicated(tax$species_tax_id),c("species_tax_id","species")], by = "species_tax_id")
#Rename species.x and species.y 
colnames(all)[which(names(all) == "species.x")] <- "species_gtdb"
colnames(all)[which(names(all) == "species.y")] <- "species_ncbi"
write.csv(all,sprintf("%s/outlier_organisms.csv",stats_file_path), row.names=FALSE)
rm(all,get,sub)

report("Done")
##############


report("Data processing completed.", TRUE)