
##' @param object a posterior samples object
##' @return a data frame
##' @examples
##' @export
##' @docType methods
##' @rdname predict-methods
setMethod("predict", "posterior", 
          function(object, npost = 100, rand = TRUE, factors = NA,
xlim = NA, seed = NULL)
{
  model <- attr(attr(object, "dmi"), "model")
  facs <- names(attr(model, "factors"))
  
  if (!is.null(factors))
  {
    if (any(is.na(factors))) factors <- facs
    if (!all(factors %in% facs))
      stop(paste("Factors argument must contain one or more of:",
                 paste(facs, collapse=",")))
  }
  
  resp <- attr(model, "dimnames")[[3]]
  ns   <- table(attr(attr(object, "dmi"), "data")[,facs], dnn = facs)
  npar   <- attr(object, "npar")
  nchain <- attr(object, "nchain")
  nmc    <- attr(object, "nmc")
  ntsample <- nchain * nmc
  pnames   <- nmc    <- attr(object, "pnames")
  thetas <- matrix(aperm(attr(object, "theta"), c(3,2,1)), ncol = npar)
  
  colnames(thetas) <- pnames
  
  if (is.na(npost)) {
    use <- 1:ntsample
  } else {
    if (rand) {
      use <- sample(1:ntsample, npost, replace = F)
    } else {
      use <- round(seq(1, ntsample, length.out = npost))
    }
  }
  
  npost  <- length(use)
  posts   <- thetas[use, ]
  nttrial <- sum(ns) ## number of total trials
  
  v <- lapply(1:npost, function(i) {
    simulate(model, n = ns, ps = posts[i,], seed = seed)
  })
  out <- data.table::rbindlist(v)
  reps <- rep(1:npost, each = nttrial)
  out <- cbind(reps, out)
  
  if (!any(is.na(xlim)))
  {
    out <- out[RT > xlim[1] & RT < xlim[2]]
  }
  
  attr(out, "data") <- attr(attr(object, "dmi"), "data")
  return(out)
})


##' @param object a posterior samples object - a list of participants
##' @return a data frame
##' @examples
##' @export
##' @docType methods
##' @rdname predict-methods
##' 
setMethod("predict", "list", 
          function(object)
          {
            lapply(object, function(x) predict(x))
           })


##' @param object a posterior samples object with hyper prior
##' @return a data frame
##' @examples
##' @export
##' @docType methods
##' @rdname predict-methods
##' 
setMethod("predict", "hyper", 
          function(object)
          {
            lapply(object@individuals, function(x) predict(x))
          })