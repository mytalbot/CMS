#' Composite measurement analysis function
#' 
#' The function takes in the results from the cms_clusters function (frequency distributions of each run of the 80 percent subsampling of the training data) and calculates
#' general attributes as well as graphical distribution outputs.
#'
#' @param raw requires the raw data list (also input in the cms_load function)
#' @param thresholds specification of the threshold values from the listed output of cms_clusters 
#' @param cluster_distribution output from the cms_cluster function (IMPORTANT: scorevars requires a vector of input variables!)
#' @param savepath include the path for the baplot here (no name spec for the plot necessary, it is called "Cluster_distros.png")
#' 
#' @import ggplot2
#' @import utils
#' @importFrom stats sd median
#' 
#' @return Data frame with the cluster distributions/severity attributions.+
#'
#' @export
#'
#'

cms_analysis <- function(raw, thresholds, cluster_distribution, savepath=NA){

  # calculate 95% CI for the thresholds between clusters ---------------------
  d          <- raw[raw[,"repeats"]==3,]
  
  mean(thresholds$threshold_1_2, na.rm = T) + (1.96*(sd(thresholds$threshold_1_2, na.rm = T)))/sqrt(NROW(d))
  mean(thresholds$threshold_1_2, na.rm = T) - (1.96*(sd(thresholds$threshold_1_2, na.rm = T)))/sqrt(NROW(d))
  
  mean(thresholds$threshold_2_3, na.rm = T) + (1.96*(sd(thresholds$threshold_2_3, na.rm = T)))/sqrt(NROW(d))
  mean(thresholds$threshold_2_3, na.rm = T) - (1.96*(sd(thresholds$threshold_2_3, na.rm = T)))/sqrt(NROW(d))
  
  
  
  # cluster distribution results --------------------------------------------
  # take the mean across all runs
  distr_means <- aaply(laply(cluster_distribution, as.matrix), c(2, 3), mean)
  # take the sd across all runs
  distr_sds   <- aaply(laply(cluster_distribution, as.matrix), c(2, 3), sd)
  
  # calculate percentage for each run 
  distr_perc  <- NULL
  distr_perc  <- lapply(cluster_distribution, function(x){
    apply(x, 1, function(y){100/sum(y)*y})
  })
  
  # mean sd over percentages 
  distr_perc_sd <- aaply(laply(distr_perc, as.matrix), c(2, 3), sd)
  
  # calculate in percentage 
  distr_means <- apply(distr_means, 1, function(x){100/sum(x)*x})
  
  # melt data for the graph
  distr_means <- melt(distr_means)
  
  #split the grouping name into two columns
  DM <- cbind(distr_means, 
                       data.frame(do.call(rbind, strsplit(as.vector(distr_means$X1), split = "_"))))
  
  #re label the column names
  distr_means$cluster   <- NULL
  distr_means$treat_mod <- NULL
  distr_means$perc      <- NULL
  distr_means$mod       <- NULL
  distr_means$treat     <- NULL
  
  distr_means$cluster   <- DM[,1]
  distr_means$treat_mod <- DM[,2]
  distr_means$perc      <- DM[,3]
  distr_means$mod       <- DM[,4]
  distr_means$treat     <- DM[,5]
  
  #names(distr_means)  <- c("cluster","treat_mod","perc","mod","treat")
 
  
  # change the names and order of the different groups (to make them appear in the graph the way we want)
  distr_means$treat       <- factor(distr_means$treat, levels=c("naive","sham","SE"))
  distr_means$mod         <- factor(distr_means$mod, levels=c("kind","chem","elec"))
  levels(distr_means$mod) <- c("Kindling","Chemical Post-SE","Electrical Post-SE")
  distr_means$cluster     <- factor(distr_means$cluster, levels=c("cluster3","cluster2","cluster1"))
  
  # standard deviation across groups in perc 
  distr_perc_sd[distr_perc_sd==0] <- NA
  apply(distr_perc_sd, 2, mean, na.rm=T)
  
  # create new column to get the group names right 
  # the "SE" group is split in "kindled" for the kindling model and "post-SE" for the other two models 
  distr_means$n_treat <- NULL
  distr_means$n_treat <- as.character(distr_means$treat)
  distr_means[distr_means [,"mod"] =="Kindling" & distr_means[,"treat"]=="SE","n_treat"] <- "Kindled"
  distr_means[!distr_means[,"mod"] =="Kindling" & distr_means[,"treat"]=="SE","n_treat"] <- "Post-SE"
  
  # re-name and re-order some things to make them appear nice in the graph
  distr_means$n_treat         <- as.factor(distr_means$n_treat)
  levels(distr_means$n_treat) <- c("Kindled","Naive","Post-SE","Sham")
  distr_means$n_treat         <- factor(distr_means$n_treat, levels=c("Naive","Sham","Kindled","Post-SE"))
  levels(distr_means$cluster) <- c("cluster1","cluster2","cluster3")
  distr_means$cluster         <- factor(distr_means$cluster, levels=c("cluster3","cluster2","cluster1"))
  
  #print(head(distr_means))
  
  # create graph of the mean distributions of the 100 runs 
  p <- ggplot(distr_means, aes(x=n_treat, y=perc, fill=cluster)) +
    geom_bar(stat = "identity") +
    facet_grid(~mod, scales="free") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_fill_brewer(palette="Reds", direction=-1) +
    labs(x="",y="distribution [%]")
  
  if(sum(is.na(savepath))==TRUE){ 
    
  }else{
    ggsave("Cluster_distros.png", plot = last_plot(), device = NULL, path = savepath,
           scale = 1, width = 6, height = 6, dpi = 300)
  }
    

  return(list(distr_means, p))
}


