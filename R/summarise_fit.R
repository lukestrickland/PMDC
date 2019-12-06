### get response Ps 
##' @import dplyr
get_ps_eachrep <- function(df, facs=c()){
  df %>% group_by(!!!syms(c("reps",facs))) %>% count(R) %>%
    mutate(out = n / sum(n)) %>% select(-n)
}

### get RT quantiles 
##' @import dplyr
##' @import tidyr
get_RTs_eachrep <- function(df, facs=c(), quantiles=c(0.1,0.5,0.9), noR=FALSE){
  df %>% group_by(!!!syms(c("reps",facs))) %>%  
    summarise(out = list(quantile(RT, probs=c(0.1,0.5,0.9)))) %>%
    unnest_longer(out, indices_to="quantile")
}

### Summarise with posterior medians/CIs
##' @import dplyr
##' @import tidyr
post_med_CIs <- function(post_summaries)
{
  facs <- colnames(post_summaries)[!colnames(post_summaries) %in% c("reps", "out")]
  
  med_CIs <- post_summaries %>% group_by(!!!syms(facs)) %>% 
    summarise(q = list(
      quantile(out, probs=c(0.1,0.5,0.9)))) %>%
    unnest_wider(q, names_sep="_", names_repair=
                   function(x) gsub("%", "", x))
  colnames(med_CIs)[(length(med_CIs)-2):length(med_CIs)] <- 
                  c("lower", "median", "upper")
  med_CIs
}


##' @docType methods
##' @rdname summarise_fit-methods
##' @export
setGeneric("summarise_fit", function(x, ... ) {
  warning("Class ", class(x), " not defined for summarise_fit")
  return(NULL)
}
)

##' @rdname summarise_fit-methods
##' @export
setMethod("summarise_fit", "data.frame",
          function(x, ...)
          {
            summarise_one(x, ...)
          })

##' @rdname summarise_fit-methods
##' @export
setMethod("summarise_fit", "list",
          function(x, ...)
          {
            simmed_data <- do.call("rbind", x)
            
            attr(simmed_data, "data") <- do.call("rbind",
                                    
                                    lapply(x, function(x)
                                      attr(x, "data"))
                                    )
            
            summarise_one(simmed_data, ...)
          })

### Summarise with posterior medians/CIs
##' @import dplyr
##' @import tidyr
##' @export
summarise_one <- function(simmed_data, facs=c(),
                               RT_quantiles=c(0.1,0.5,0.9), 
                               noR=FALSE) {
  
  pp_reps <- get_ps_eachrep(simmed_data, facs=facs)
  
  pp_data <- attr(simmed_data, "data") %>% group_by(!!!syms(facs)) %>% count(R) %>%
    mutate(data = n / sum(n)) %>% select(-n)
  
  pps <- full_join(post_med_CIs(pp_reps),
                   pp_data)
  
  if(!noR) RT_facs <- c(facs, "R") else RT_facs <- facs
  
  RTs_reps <- get_RTs_eachrep(simmed_data, facs=RT_facs, noR=noR,
                              quantiles=RT_quantiles)
  
  RT_data <- attr(simmed_data, "data") %>% group_by(!!!syms(RT_facs)) %>%  
    summarise(data = list(quantile(RT, probs=c(0.1,0.5,0.9)))) %>%
    unnest_longer(data, indices_to="quantile")
  
  RTs <- full_join(post_med_CIs(RTs_reps),
                   RT_data)
  
  out <- new("fit_summaries", pps=pps, RTs= RTs)
  out
}






