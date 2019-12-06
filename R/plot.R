### fit_summaries
##' An S4 class to represent summaries of fit
##'
##' @slot pps data frame with predictions for response proportions
##' @slot RTs data frame with predictions for RTs
##' @export
setClass("fit_summaries",
         slots = c(
           pps = "data.frame",
           RTs = "data.frame"))


##' @rdname plot-methods
##' @export
setMethod("plot", "fit_summaries",
          function(x)
          {
            list(plot_RP(x),
            plot_RT(x))
          })


### plot fit summaries RPs
##' @import ggplot2
##' @export
plot_RP <- function (df, include.NR=FALSE, xaxis = 'R', panels.ppage=4, 
                           nrow=NULL, ncol=NULL) 
  # xaxis is the name of the factor to put on the X axis  
{
  if(class(df)=="fit_summaries") df <- df@pps
  #remove cases where the combination of factors was not present in design  
  df <- df[!is.nan(df$data),]
  
  grid <- colnames(df) [!colnames (df) %in% c(xaxis, "median", "lower", "upper", "data")]
  
  if (length(grid)==0) {
    n.plots <-1 
    plot.ind <- rep(1, length(df$data)) 
  } else {
    n.plots <- sum (!is.na((tapply(df$data, df[,c(grid)], function (x) 1)))) 
    plot.ind <- as.numeric(factor(with(df, interaction(df[,grid]))))
  }
  
  plot.pages <- ceiling(n.plots/panels.ppage)
  
  grid_labels <- labeller(label_value, .multi_line=F)
  
  plots <- list()
  for (j in 1:plot.pages) {
    
    active.df <- df[plot.ind %in% ((j-1)*panels.ppage+1):(j* panels.ppage),]
    
    plot <- ggplot(active.df, aes_string(x = xaxis, y= 'median')) + 
      geom_point(size=3) + geom_errorbar(aes(ymax = upper, ymin = lower), width= 0.2) +
      geom_point(aes_string(x = xaxis, y= 'data'), pch=21, size=4, colour="black") +
      geom_line(aes(group = 1, y=data), linetype=2) + ylab("Response Proportion")
    
    if (length(grid)!=0) 
      plot <- plot + facet_wrap(grid, labeller = grid_labels, scales = 'free_x', 
                                nrow=nrow, ncol=ncol)
    plots[[j]] <- plot
  }
  
  if (length(plots)==1) plots[[1]] else plots
}


### plot fit summaries RAs
##' @import ggplot2
##' @export
plot_RA <- function (df, include.NR=FALSE, xaxis = NA, panels.ppage=4, 
                           nrow=NULL, ncol=NULL, acc.fun=
                             function(x){as.numeric(x$S)==as.numeric(x$R)}) 
  # xaxis is the name of the factor to put on the X axis  
{
  if(is.na(xaxis)) stop("Must include x axis")
  if(class(df)=="fit_summaries") df <- df@pps
  #remove cases where the combination of factors was not present in design  
  df <- df[!is.nan(df$data),]
  df <- df[acc.fun(df),]
  df <- df[,!colnames(df) %in% "R"]
  
  grid <- colnames(df) [!colnames (df) %in% c(xaxis, "median", "lower", "upper", "data")]
  if (length(grid)==0) {
    n.plots <-1 
    plot.ind <- rep(1, length(df$data)) 
  } else {
    n.plots <- sum (!is.na((tapply(df$data, df[,c(grid)], function (x) 1)))) 
    plot.ind <- as.numeric(factor(with(df, interaction(df[,grid]))))
  }
  
  plot.pages <- ceiling(n.plots/panels.ppage)
  
  grid_labels <- labeller(label_value, .multi_line=F)
  
  plots <- list()
  for (j in 1:plot.pages) {
    
    active.df <- df[plot.ind %in% ((j-1)*panels.ppage+1):(j* panels.ppage),]
    
    plot <- ggplot(active.df, aes_string(x = xaxis, y= 'median')) + 
      geom_point(size=3) + geom_errorbar(aes(ymax = upper, ymin = lower), width= 0.2) +
      geom_point(aes_string(x = xaxis, y= 'data'), pch=21, size=4, colour="black") +
      geom_line(aes(group = 1, y=data), linetype=2) + ylab("Accuracy")
    
    if (length(grid)!=0) 
      plot <- plot + facet_wrap(grid, labeller = grid_labels, scales = 'free_x', 
                                nrow=nrow, ncol=ncol)
    plots[[j]] <- plot
  }
  
  if (length(plots)==1) plots[[1]] else plots
}

### plot fit summaries RTs
##' @import ggplot2
##' @export
plot_RT <- function (df, xaxis = 'R', panels.ppage=4, do.quantiles=TRUE,
                           nrow=NULL, ncol=NULL,exclude.response=NA) 
  # xaxis is the name of the factor to put on the X axis  
{
  if(class(df)=="fit_summaries") df <- df@RTs
  df <- df[!is.nan(df$median) & !is.na(df$median),]
  # In case this wipes out a reponse level
  if (any("R" %in% colnames(df))) df$R <- factor(as.character(df$R))
  
  # get factors (other than the xaxis factor) for the grid
  if (do.quantiles) grid <- colnames(df) [!colnames (df) %in% 
                                            c(xaxis, "median", "lower", "upper", "data", "quantile")] else
                                              grid <- colnames(df) [!colnames (df) %in% 
                                                                      c(xaxis, "median", "lower", "upper", "data")]     
  
  if (length(grid)==0) {
    n.plots <-1 
    plot.ind <- rep(1, length(df$data)) 
  } else {
    n.plots <- sum (!is.na((tapply(df$data, df[,c(grid)], function (x) 1)))) 
    plot.ind <- as.numeric(factor(with(df, interaction(df[,grid]))))
  }
  
  plot.pages <- ceiling(n.plots/panels.ppage)
  
  # each step of the loop grabs enough data to create number of plots p page
  plots <- list()
  for (j in 1:plot.pages) {
    
    active.df <- df[plot.ind %in% ((j-1)*panels.ppage+1):(j* panels.ppage),]
    
    plot <- ggplot(active.df, aes_string(x = xaxis, y= 'median')) + geom_point(size=3) + 
      geom_errorbar(aes(ymax = upper, ymin = lower), width= 0.2) +
      geom_point(aes_string(x = xaxis, y= 'data'), pch=21, size=4, colour="black") +
      ylab("RT (s)")
    if (do.quantiles) 
      plot <- plot + geom_line(aes(group = quantile, y=data), linetype=2) else
        plot <- plot + geom_line(aes(group=1,y=data), linetype=2)
    
    if(length(grid)!=0) plot <- plot + 
      facet_wrap(grid, labeller = labeller(label_value, .multi_line=F), scales = 'free',
                 nrow=nrow, ncol=ncol)
    
    plots[[j]] <- plot
  }
  
  if (length(plots)==1) plots[[1]] else plots
}


