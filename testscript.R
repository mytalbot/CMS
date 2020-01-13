library(usethis)
library(devtools)

usethis::use_description()
package.skeleton(name = "cms")
use_r("cms_load")
use_r("cms_clusters")
use_r("cms_analysis")

install_github("mytalbot/cms")


library(cms)
file          <- "C:/MHH Bleich/Aktuelles/Maarten Paper/Maarten Epilepsy Score/all_raw.txt"
mydat         <- cms_load(file, remove_this="elec_tel_no SE")

cms_cl        <- cms_clusters(mydat, 
                              runs        = 100, 
                              emptysize   = 0.2, 
                              trainsize   = 0.8, 
                              idvariable  = "animal_id", 
                              varstart    = 14, 
                              exclude     = "Seizures_n",
                              
                              scorevars   =  c("Sacc_pref", "social_interaction", "burrowing_rat", "openfield_rat"))

thresholds           <- cms_cl[[1]] 
cluster_distribution <- cms_cl[[2]]  

cms_analysis(mydat, thresholds, cluster_distribution, savepath="C:/MHH Bleich/Packages/cms/cms")










