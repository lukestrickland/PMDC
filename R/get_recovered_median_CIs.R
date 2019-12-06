#' #' Gets posterior medians and CIs for each participant
#' #'
#' #' @param hsamples A list of posterior samples
#' #' @return The posterior median and CIs for each participant
#' #' @export
#' get_participant_median_CIs <-
#' function(hsamples) {
#'   samps <- lapply(hsamples, gg_thetas)
#' 
#'   post_medians <- do.call("rbind",
#'                           lapply(samps, function(x) apply(x, 1, mean))
#'   )
#' 
#'   post_LCI <- do.call("rbind",
#'                       lapply(samps, function(x) apply(
#'                         x, 1, quantile, prob=0.025)
#'                       )
#'   )
#' 
#'   post_HCI <- do.call("rbind",
#'                       lapply(samps, function(x) apply(
#'                         x, 1, quantile, prob=0.975)
#'                       )
#'   )
#' 
#'   out <- list(post_medians, post_LCI, post_HCI)
#'   names(out) <- c("medians", "LCI", "HCI")
#'   out
#' }
