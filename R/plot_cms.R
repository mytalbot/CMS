#' Plot cms Clusters
#' 
#' The function is a wrapper for subgroup filtering of the cms output and 
#' lotting the cluster distributions.
#'
#' @param cmsresult requires the output object from the cms function
#' @param rotateX rotates the x labels (default=NULL; 45 means 45 degrees)
#' 
#' @import ggplot2
#' @import utils
#' @import stats
#' 
#' @return Data frame with the cluster distributions/severity attributions.+
#'
#' @export
#' 
#' 
#' 
plot_cms <- function(cmsresult=NULL, rotateX=NULL){
  
  rep <- cmsresult$reportdata %>%
  group_by(treatment, mod, cluster) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  as.data.frame()
  
  p <- ggplot(rep, aes(x=treatment, y=freq, fill=factor(cluster))) +
    geom_bar(stat = "identity") +
  facet_grid(~mod, scales="free") +
  labs(fill="Cluster") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_brewer(palette="Reds", direction=-1) +
  labs(x="",y="distribution [%]")
  
  p <- p  +  theme(legend.position  = "top",
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
  
  # rotate the x labels
  if(is.null(rotateX)){
  }else{
    p <- p  + theme(axis.text.x = element_text(angle = rotateX, vjust = 1, hjust=1))
  }
  
  return(list(clustering=rep, p=p))
 
}
  
  