indexMatrix <- function(ff, n_col=NA, n_row=NA){
  indexed <- as.data.frame(ff@exprs)
  indexed <- indexed[which(indexed$`X-index`>=1),]
  x_max <- max(indexed$`X-index`)
  y_max <- max(indexed$`Y-index`)
  plate <- data.frame()
  if (!(is.na(n_col) & is.na(n_row))) {
    x_max <- n_col
    y_max <- n_row
  }
  for (i in c(1:x_max)) {
    for (k in c(1:y_max)) {
      evts <- length(which(indexed$`X-index`==i & indexed$`Y-index`==k))
      plate[k,i] <- evts
    }
  }
}
