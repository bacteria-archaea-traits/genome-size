##################################################################################
# Run time pipeline
#
# "Genome size, oxygen use and ecological strategy across bacteria and archaea"  
# Nielsen DA , Fierer N , Geoghegan JL, Gillings MR, Gumerov V , Madin JS, 
# Moore L , Paulsen IT, Reddy TBK , Tetu SG, Westoby M*
#
# *Corresponding Author
#
##################################################################################

# Retrieve large "taxonomy_names.csv" file not in the GitHub repo
if (!file.exists("data/madin_et_al/taxonomy_names.csv")) {
  download.file(url="https://ndownloader.figshare.com/files/14875220?private_link=ab40d2a35266d729698c", destfile = "data/madin_et_al/taxonomy_names.csv")
}

#Load study specific run-time files
# source("R/packages.R") # I'd remove this and leave the below
# These seem to be the reuiqred packages; leave at top in main file, so people know to auto-install when opening project. 
library(tidyverse)
library(phylolm)
library(phytools)
library(ggpubr)
library(Hmisc)
library(psych)

source("R/functions.R") 
source("R/prepare.R") # This works now I have data file. 
source("R/figures.R") # I needed to remove the dev.off() throghout to make 
source("R/figures_supp.R")
source("R/stats.R")
