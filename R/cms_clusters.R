#' Cluster and frequency distribution of composite measures for severity attribution
#'
#' The algorithm takes the cleaned input data, does some curation for missing values and then calculates the composite measures schemes.
#' The function can be used in two ways: 1. with the scorevars set to NA the data are screened for the most prominent feature contributions; 2. 
#' the so found feature sets can be specified, e.g., scorevars=c("Sacc_pref", "social_interaction", "burrowing_rat", "openfield_rat"). This will
#' result in the calculation of a severity-based cluster distribution analysis using the input variables.
#'
#' @param raw raw data (unprocessed or from cms_load)
#' @param runs number of random samplings for the PCA-based feature determination
#' @param emptysize fraction of empty data that will be removed
#' @param trainsize size of the "training data" in the fold subsamplings
#' @param idvariable column header of the animal id variable in the data
#' @param varstart column number with the first variable to start with - there shall be no general infomation on the right side of this number
#' @param varend column number with the last index. defaults to NA for the full table 
#' @param exclude naming column headers that shall be excluded from the data set
#' @param scorevars if NA (default) the function will determine the most prominent variables in the sample data; NA can also be replaced by a vector of variables
#' 
#' @import made4  
#' @import ade4 
#' @import RColorBrewer 
#' @import gplots
#' @import scatterplot3d
#' @import lattice
#' @import caret
#' @import reshape2
#' @import plyr
#' @import corrplot
#' @importFrom stats kmeans predict   
#'
#' @return A list with composite score thresholds and k-means cluster attributions.
#' 
#' @export
#'
cms_clusters  <- function(raw=raw, runs=100, emptysize=0.2, trainsize=0.8, idvariable="animal_id",
                          varstart=14, varend=NA, exclude="Seizures_n", scorevars=NA){

  # prepare the framework
  PC1loadings <- data.frame(matrix(nrow = 100, ncol = 10))
  PC2loadings <- data.frame(matrix(nrow = 100, ncol = 10))
  PCA_perc    <- NULL
  thresholds  <- data.frame("threshold_1_2"=NA,"threshold_2_3"=NA)
  cluster_distribution <- list()
  
  for(j in 1:runs){
    set.seed(j)
    
    # train and test set
    d        <- raw[raw[,"repeats"]==3,]
    d        <- d[, -which(colMeans(is.na(d)) > emptysize)]
    
    subset80 <- sample(d[[idvariable]], trainsize*length(d[[idvariable]]))
    d_80     <- d[ d[,idvariable] %in% subset80,]
    d_20     <- d[!d[,idvariable] %in% subset80,]
    
    # clean data and exclude single variables
    if(is.na(varend)){
      d_pca          <- d_80[,varstart:dim(d_80)[2]]  # varend
      d_pca[exclude] <- NULL
    }else{
      d_pca          <- d_80[,varstart:varend]  # varend
      d_pca[exclude] <- NULL
      
    }
      
      
    # curate missing values in PCA data & transform data (BoxCox)
    missing <- c()
    for(i in 1:ncol(d_pca)){
      missing[i]                 <- sum(is.na(d_pca[,i]))
      d_pca[is.na(d_pca[,i]), i] <- mean(d_pca[,i], na.rm = TRUE)
    }
    #print(paste("Missing data: ", sum(missing),"(",round((sum(missing)/ (dim(d_pca)[1]*dim(d_pca)[2]))*100,2),"%)", sep="")) 
    
    pre.proc  <- preProcess(d_pca, method=c("BoxCox", "center", "scale"))
    d_pca     <- predict(pre.proc, d_pca)
    
    # do the PCA
    d_pca_ord <- ord((d_pca), type="pca")
    #print(head(d_pca))
    
    ### extract the LOADINGs and sort them
    temp            <- sqrt((d_pca_ord$ord$co[,1:2])^2)
    temp            <- temp[order(temp[,1], decreasing = T),]
    PC1loadings[j,] <- rownames(temp)[1:10]
    
    temp            <- temp[order(temp[,2], decreasing = T),]
    PC2loadings[j,] <- rownames(temp)[1:10]
    
    # save percentage explained
    PCA_perc        <- rbind(PCA_perc, (d_pca_ord$ord$eig * 100/sum(d_pca_ord$ord$eig))[1:5])
    
    # add PC1 and PC2 to the data sheet
    d_80$PC1        <- d_pca_ord$ord$li$Axis1
    d_80$PC2        <- d_pca_ord$ord$li$Axis2
    
    if(sum(is.na(scorevars))==TRUE){
      # nüscht
      
    }else{
      # Create a composite score based on n variables ---------------------------
      d_80$comp_score <- c(apply(scale(d_80[, scorevars]) ,1, sum, na.rm=T))
      d_80            <- d_80[!is.na(d_80$comp_score),]
      
      ### clustering of the score
      cl              <- kmeans(d_80$comp_score, 3)
      d_80$clusters   <- cl$cluster
      
      centers         <- data.frame(cluster=rownames(cl$centers), center=cl$centers)
      centers         <- centers[order(centers$center),] 
      centers$cluster_new_order <- 1:3
      
      
      # calculate the thresholds by checking the median between min and max of two clusters
      thresholds[j,]  <- data.frame("threshold_1_2"=
                                      median(c(  max(d_80[d_80[,"clusters"]==centers[1,"cluster"],"comp_score"]),
                                                 min(d_80[d_80[,"clusters"]==centers[2,"cluster"],"comp_score"])))
                                    ,"threshold_2_3"=
                                      median(c(  max(d_80[d_80[,"clusters"]==centers[2,"cluster"],"comp_score"]),
                                                 min(d_80[d_80[,"clusters"]==centers[3,"cluster"],"comp_score"]))) )
      
      # add re labeled clusters to data set
      # the lowest cluster is always 1, the highest 3
      d_80[d_80[,"clusters"]==centers[1,"cluster"],"n_cluster"] <- 1
      d_80[d_80[,"clusters"]==centers[2,"cluster"],"n_cluster"] <- 2
      d_80[d_80[,"clusters"]==centers[3,"cluster"],"n_cluster"] <- 3
      
      
      d_80$treat_mod <- as.factor(paste(d_80$mod,d_80$treatment, sep="_"))
      d_80$n_cluster <- as.factor(d_80$n_cluster)
      
      # count the number of clusters for each group
      temp          <- data.frame(melt(tapply(d_80$n_cluster, d_80$treat_mod, table)))
      
      # save the results as a data.frame (not the most elegant solution), which is then stored in a list
      cluster_distribution[[length(cluster_distribution)+1]] <- data.frame(row.names = levels(d_80$treat_mod),
                                                                           cluster1=melt(lapply(temp$value, "[[", 1))$value,
                                                                           cluster2=melt(lapply(temp$value, "[[", 2))$value,
                                                                           cluster3=melt(lapply(temp$value, "[[", 3))$value)
    }
  }
  
  
  # Outside the Luhp - show the best parameter selections when no selection was provided
  if(sum(is.na(scorevars))==TRUE){
    temp      <- count(c(PC2loadings$X1,PC2loadings$X2,PC2loadings$X3,PC2loadings$X4))
    temp$perc <- 100/sum(temp$freq)*temp$freq
    return(temp[order(-temp$perc),])
  }else{ 
    return(list(thresholds, cluster_distribution))
  }
}




