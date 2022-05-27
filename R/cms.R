#' Composite measures function
#' 
#' The function takes in the results from the cms_clusters function (frequency distributions of each run of the 80 percent subsampling of the training data) and calculates
#' general attributes as well as graphical distribution outputs.
#'
#' @param raw raw data input (filter the group variables first; each group 
#' requires an extra analysis)
#' @param runs the number of repeats (for feature aggregation)
#' @param idvariable id variable name of the subjects (e.g. animal_id)
#' @param emptysize fraction of data that limits the imputation threshold 
#' (e.g. everything below emptysize=0.2 will be imputed; avoid when possible)
#' @param setsize fraction size of the subsets (e.g., setsize=0.8 means that in
#' each run 80% of the data are randomly chosen to do the cms)
#' @param variables explicitly name the variables that shall be included in the
#' cms analysis (yes, all of them!)
#' @param maxPC the maximum number of principal components that shall be
#' evaluated (remember: this number must be smaller or equal as the number of
#' analyzed variables)
#' @param clusters the number of clusters that shall be applied to the cms 
#' analysis
#' @param seeding sets the seeding constant (TRUE) or not (FALSE)
#' @param showplot show the distribution plot of the variables after cms 
#' analysis
#' @param legendpos legend position (shift to right when there are many vars)
#' @param verbose sets the verbosity on variable handlings during the 
#' calculation (default=FALSE)
#' 
#' @import ggplot2
#' @import utils
#' @import stats
#' @import factoextra
#' 
#' @return Data frame with the cluster distributions/severity attributions.+
#'
#' @export
#'
#'

cms <- function(raw=NULL, runs=NULL, idvariable=NULL, emptysize=NULL,
                setsize=NULL,variables=NULL, maxPC=4, clusters=3, seeding=TRUE, 
                showplot=TRUE, legendpos="top", verbose=FALSE){
  
  # set seeding constant?
  if(seeding==TRUE){
    set.seed(1)
  }else{}
  
  # ALLE Daten OHNE Selection!
  d   <- raw
   
  # start the luhp
  ELS          <- NULL
  Values       <- NULL
  reportdata   <- NULL
  event        <- 0
  imputed      <- 0
  excludedvars <- 0
  InfoContent  <- NULL
  for(j in 1:runs){
    subset80 <- sample(d[[idvariable]], setsize*length(d[[idvariable]]))
    trainorg <- d[ d[,idvariable] %in% subset80, ]
    traindat <- d[ d[,idvariable] %in% subset80,variables]
    testdat  <- d[!d[,idvariable] %in% subset80,variables]
    
    # curate missing values in PCA data 
    imputed <- 0
    for(i in 1:ncol(traindat)){
      imputed <- imputed + sum(is.na(traindat[,i]))
      traindat[is.na(traindat[,i]), i] <- mean(traindat[,i], na.rm = TRUE)
    }
    
    # NaN to NAs
    traindat[sapply(traindat, is.nan)] <- NA
    
    #  check if there is zero variance & skipt the run 
    if(is.na(sum(apply(traindat,2,sd, na.rm=TRUE))) |
       sum(apply(traindat,2,sd, na.rm=TRUE)==0, na.rm=TRUE)>=1){
      
    }else{
      # sum positive events (zero variance events are skipped!)
      event     <- event +1
      
      
      # Do the PCA
      res       <- prcomp(traindat, center=TRUE, scale = TRUE)
      contribs  <- get_pca_var(res)$contrib
      contribs  <- contribs[,1:maxPC]
      sortVars  <- NA
      sortVars  <- apply(contribs, 1, sum, na.rm=TRUE)
      sortVars  <- sortVars[order(sortVars,decreasing = TRUE)]
      ELS       <- rbind(ELS , names(sortVars)  )
      
      
      # cumulative variance
      vars      <- apply(res$x, 2, var,na.rm=TRUE)   
      props     <- round(vars / sum(vars),3)
      props     <- sum(props[1:maxPC])
      
      InfoContent <- rbind(InfoContent, data.frame(run         = j,
                                                   maxPC       = maxPC,
                                                   CumVariance = round(props,2)))
      # get the PCA scores (must equal the row numbers in traindat)
      scores    <- res$x[,1:maxPC]
  
      # extract the FIRST TWO PCs for plotting the PCA; add these to traindat 
      traindat$PC1 <- scores[,1]
      traindat$PC2 <- scores[,2]
      
      # calculate the composite score from the "scores" (simple sum score)
      traindat$compscore <- NA
      traindat$compscore <- apply(scores, 1, sum, na.rm = TRUE)
      traindat$compscore <- round( traindat$compscore,3)
      
      # clustering of the score
      cl                 <- kmeans(traindat$compscore, clusters)
      traindat$cluster   <- cl$cluster
      
      centers         <- data.frame(cluster=rownames(cl$centers), center=cl$centers)
      centers         <- centers[order(centers$center),] 
      centers$cluster_new_order <- 1:clusters
      
      # add re labeled clusters to data set
      # the lowest cluster is always 1, the highest 3
      for(l in 1:clusters){
        traindat[traindat[,"cluster"]==centers[l,"cluster"],"n_cluster"] <- l
      }
      traindat$cluster <- NULL
      names(traindat)[names(traindat)=="n_cluster"] <- "cluster"
      
      # re-arrange the training data for pooling
      reportdata         <- rbind(reportdata, data.frame(run       = j,
                                                         treatment = trainorg$treatment,
                                                         mod       = trainorg$mod,
                                                         traindat))
      
     }
    }
  # some reportings
  if(verbose==TRUE){
    print(paste(runs - event," zero veriance run(s).",sep=""))
    print(paste(excludedvars," variables were eliminated.",sep=""))
    print(paste(imputed," values were imputed (",
                round(imputed/(dim(traindat)[1]*dim(traindat)[2])*100,2),"%).",sep=""))
  }else{}
  
  
  
  # Analysis ----------------------------------------------------------------
  # the percents are biased towards the total truly calculated events
  FRQ      <- NULL
  for(p in 1:dim(ELS)[2]){
    frq      <- data.frame(position   = p,
                           plyr::count( ELS[,p]))
    frq$perc <- round(frq$freq/event*100 ,2)
    FRQ      <- rbind(FRQ,frq)
  }
  
  
  ### show the FRQc as plot
  # this means: in pos 1, it cannot decide between weight and mgs but weight 
  # is more frequent
  p1 <- ggplot(FRQ, aes(x = factor(position), y = freq)) +
    geom_bar(
      aes(color = x, fill = x),
      color="black",
      stat = "identity", position = position_dodge(0.8),
      width = 0.7) +
    labs(x        = "Variable position",
         y        = "Variable Frequency",
         fill     = "",
         color    = "" ) +
    theme_bw()
  p1 <- p1 +  theme(legend.position  = legendpos,
                    #panel.border     = element_blank(), 
                    #panel.grid.major = element_blank(),
                    #panel.grid.minor = element_blank(),
                    axis.line        = element_line(colour = "black"),
                    strip.background = element_rect(fill = "white", colour = "black", size = 0.8),
                    strip.text       = element_text(size = 12),
                    axis.text.x      = element_text(size = 12),
                    axis.title.x     = element_text(size = 12),
                    axis.text.y      = element_text(size = 12),
                    axis.title.y     = element_text(size = 12))
  
  if(showplot==TRUE){
    print(p1)
  }else{}
  
  return(list(p=p1, reportdata=reportdata, Labels=ELS, imputed=imputed, 
              InfoContent=InfoContent, FRQ=FRQ ))
  
}
  
  


# # calculate the number of clusters heuristic
# pscree    <- fviz_nbclust(scores, kmeans, method = "wss") +
#   geom_vline(xintercept = clusters, linetype = 2)         +
#   labs(title = NULL)
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


