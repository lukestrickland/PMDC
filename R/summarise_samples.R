
function_on_thetas <- function (samples, fun) {
  #Put the thetas into a data frame so it's easy to write function for it
  theta <- samples@theta
  dim(theta) <- c(dim(theta)[1], prod(dim(theta)[2], dim(theta)[3]))
  row.names(theta) <- samples@pnames
  theta_df <- as.data.frame(t(theta))
  fun(theta_df)[,1]
}

get_effect_summaries <- function(effect) {
  plot(density(effect), main="",xlab="", xlim=c(0, max(effect)))
  M <-  mean(effect)
  SD <- sd(effect)
  Z <- M/SD
  p <- ecdf(effect)(0)
  c(M=M, SD=SD, Z=Z, p=p)
}

##' @docType methods
##' @rdname summarise_effect-methods
##' @export
setGeneric("summarise_effect", function(samples, fun) {
  warning("Class ", class(samples), " not defined for summarise_effect")
  return(NULL)
}
)

##' @rdname summarise_effect-methods
##' @export
setMethod("summarise_effect", "posterior",
          function(samples, fun)
          {
            effect <- function_on_thetas(samples, fun)
            get_effect_summaries(effect)
          })

##' @rdname summarise_effect-methods
##' @export
setMethod("summarise_effect", "list",
          function(samples, fun)
          {

            all_s_effects <- lapply(samples, function_on_thetas, fun=fun)
            effect <- Reduce("+", all_s_effects) / length(all_s_effects)
            get_effect_summaries(effect)
          })

##' @rdname summarise_effect-methods
##' @export
setMethod("summarise_effect", "hyper",
          function(samples, fun)
          {
            
            all_s_effects <- lapply(samples@individuals, function_on_thetas, fun=fun)
            effect <- Reduce("+", all_s_effects) / length(all_s_effects)
            get_effect_summaries(effect)
          })


### Average posterior samples over subjects
##' @export
average_post_samples <- function (samples) {
  
  #handle different nmcs 
  #Different numbers of nmc for each participant... use the min number and then
  #for participants wtih more randomly sample out that many
  #print a warning if this happens
  nmcs<- sapply(samples, function(x) x@nmc)
  nmc <- min(nmcs)
  for (i in 1:length(samples)){
    if (nmcs[i] > nmc) {samples[[i]]@theta <- 
      samples[[i]]$theta[,,sample(1:dim(samples[[i]]@theta)[3], nmc)]
      warning(paste("nmcs uneven for", names(samples)[i], ",", nmc, "used"))
    }
    
  }
  
  #Get all the thetas for all participants and put in a big array
  thetas <- lapply(samples, function(x)
    x@theta)
  theta_array <- unlist(thetas)
  #Dimensions of the array:
  #All the previous dimensions, and one final dimension
  #for participant number
  dim_tmp <-
    c(dim(thetas[[1]]), length(theta_array) / prod(dim(thetas[[1]])))
  dim(theta_array) <- dim_tmp
  #Average over the last dimension of the array to get
  #subject averages for each posterior sample
  avg_thetas <- apply(theta_array, c(1, 2, 3), mean)
  ## Make a dummy posterior sample object out of the first 
  ## participant,
  ## then assign that object the group averaged thetas
  ##NOTE some of these attributes may not make sense for the 
  ##group level - for example the dmi attribute contains a
  ## data frame for the first participant
  average_samples <- samples[[1]]
  average_samples@theta <- avg_thetas
  average_samples
}

##' @docType methods
##' @rdname PickStuck-methods
##' @export
setGeneric("get_post_cred", function(x, ... ) {
  warning("Class ", class(x), " not defined for get_post_cred")
  return(NULL)
}
)

### Get posterior credible intervals from summary object
##' @import ggdmc
##' @export
get_post_cred_one <- function(samples){
  post_summary <- summary(samples)
  qs<- post_summary$quantiles[, c("2.5%", "50%", "97.5%")]
  dimnames(qs)[[2]] <- c("q2.5", "q50", "q97.5")
  as.data.frame(qs)
}

##' @rdname get_post_cred-methods
##' @export
setMethod("get_post_cred", "posterior",
          function(x)
          {
            get_post_cred_one(x)
          })

##' @rdname get_post_cred-methods
##' @export
setMethod("get_post_cred", "list",
          function(x)
          {
            get_post_cred_one(
              average_post_samples(x)
              )
          })

##' @rdname get_post_cred-methods
##' @export
setMethod("get_post_cred", "hyper",
          function(x)
          {
            get_post_cred_one(
              average_post_samples(x@individuals)
            )
          })


##' @docType methods
##' @rdname post_p-methods
##' @export
setGeneric("post_p", function(x, ... ) {
  warning("Class ", class(x), " not defined for get_post_cred")
  return(NULL)
}
)
