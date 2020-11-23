indexPlot <- function(ff, n_col=NA, n_row=NA) {
  if (!("X-index" %in% colnames(ff) & "Y-index" %in% colnames(ff))) {
    stop("Please use an indexed flowframe. See astrioindex")
  }
  indexed <- as.data.frame(ff@exprs)
  indexed <- indexed[which(indexed$`X-index`>=1),]
  x_max <- max(indexed$`X-index`)
  y_max <- max(indexed$`Y-index`)
  plate <- data.frame(X=integer(),
                      Y=integer(),
                      events=integer())
  u <- 1
  if (!(is.na(n_col) & is.na(n_row))) {
    x_max <- n_col
    y_max <- n_row
  }
  for (i in c(1:x_max)) {
    for (k in c(1:y_max)) {
      plate[u,"X"] <- i
      plate[u,"Y"] <- k
      plate[u,"events"] <- length(which(indexed$`X-index`==i & indexed$`Y-index`==k))
      u <- u+1
    }
  }
  ggplot(plate, aes(x=X, y=Y,)) +
    geom_point(aes(size=events)) +
    scale_size_binned_area() +
    theme_minimal() +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}
