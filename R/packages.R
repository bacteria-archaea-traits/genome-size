##################################################################################
# Load packages required for pipeline
#
# "Genome size, oxygen use and ecological strategy across bacteria and archaea"  
# Nielsen DA , Fierer N , Geoghegan JL, Gillings MR, Gumerov V , Madin JS, 
# Moore L , Paulsen IT, Reddy TBK , Tetu SG, Westoby M*
#
# *Corresponding Author
#
##################################################################################

# Report
report("## Loading R packages ##", TRUE)

# Vector of packages

packages <- c("tidyverse",
              "ggpubr",
              "ggsignif",
              "png",
              "scales",
              "grid",
              "Hmisc",
              "psych",
              "phytools",
              "devtools",
              "phangorn",
              "phylolm")

# Install and/or load packages

for(i in 1:length(packages)) {
  #Check if package exists, else install it
  if (!packages[i] %in% installed.packages()) { 
    
    if(packages[i] == "phylolm") {
      report("Installing phylolm from lamho at github using devtools.")
      devtools::install_github("lamho86/phylolm")
    } else {
      report(sprintf("Installing package %s",packages[i])) 
      install.packages(packages[i])
    }
  }
  #Load package
  report(sprintf("Loading package %s",packages[i]))
  library(packages[i], character.only = TRUE)
}

# Clean up

rm(i,packages)

# Report
report("## R packages loaded ##")
