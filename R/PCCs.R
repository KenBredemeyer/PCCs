plot_PCCs <- function(data_matrix, betas, class_intervals = 4) {
  stopifnot(class(data_matrix) == "data.frame" && class(betas) == "numeric")

	# display minimum number of comparisons per script
	nc <- apply(data_matrix, 2, function(x) length(na.omit(x)))
  if (min(nc) < class_intervals) warning("not enough comparisons")

	# Form class intervals
	pw <- vector("list", ncol(data_matrix))
	index <- vector("list", ncol(data_matrix))                    # index is a list of which ordered scripts are involved in the comparison
	index <- apply(data_matrix, 2, function(x) which(!is.na(x)))
	index <- lapply(index, function(x) unname(x, force = TRUE))

	for (i in 1:length(index)) {
	  cicn <- (length(index[[i]]))/class_intervals  #   average number of scripts in each class interval
	  l_o <- cicn - floor(cicn)

	  r1 <- (1 - l_o) * class_intervals
	  r2 <- l_o*class_intervals
	  a <- rep(floor(cicn), round(r1, 0))
	  bb <- rep(ceiling(cicn), round(r2, 0))
	  a <- data.frame(a)                            # rbind.fill needs data frames as arguments.
	  bb <- data.frame(bb)
	  pw.r <- vector("numeric", r1+r2)
	  pw.r[] <- na.omit(unlist(plyr::rbind.fill(a, bb)))   # na.omit removes NA formed by rbind.fill

	  x <- c(1:class_intervals)        # class interval number
	  pw[[i]] <- rep(x, round(pw.r, 0))
	}

	#   data_matrix[index[[2]], 2]
	#unname(data_matrix[index[[2]], 2])
	#comparisons <- cbind(unname(data_matrix[index[[2]], 2]), b[(index[[2]])], pw[[2]])

	# Create a matrix of comparisons for each script (a list of matrices)
	comparison <- vector("list", nrow(data_matrix))
	for (i in 1:nrow(data_matrix)) {
	  comparison[[i]] <- cbind(unname(data_matrix[index[[i]], i]), betas[(index[[i]])], pw[[i]])
	  colnames(comparison[[i]]) <- c("Proportions", "Locations", "class interval")
	}

	xx <- seq(-13, 13, 0.01)

	y <- vector("numeric", length(xx))
	# store mean proportion and ability for each CI for all scripts
	means_all <- vector("list", nrow(data_matrix))

	par(ask = TRUE)
	for (n in 1:nrow(data_matrix)) {
	  # calculate mean proportions & mean locations
	  compare <- data.table::data.table(comparison[[n]])
	  means <- compare[ , .(mean_p = mean(Proportions), mean_a = mean(Locations)), by = "class interval"]
	  means <- data.frame(means)   # with = FALSE  (normal subsetting)
	  means_all[[n]] <- means

	  #points for plotting
	  for (i in 1:length(xx)) {
	    y[i] <-  exp(xx[i] - b[n])/(1 + exp(xx[i] - b[n]))
	  }

	  txt <- rle(pw[[n]])$lengths
	  # plot window range = range of ability estimates (for the group of persons)
	  plot(xx, y, xlim = c(min(b)-2, max(b)+1.2), ylim = c(0, 1.1), type = "l", xlab = "PCC for script ID",
	    ylab = "Expected Value", col="red", main = "Performance Characteristic Curve", sub = rownames(data_matrix)[n])
	  axis(side=1, tick=TRUE, at= round(b[n], 3), padj=1, lwd.ticks = 2, col.ticks="red")
	  points(means[,3], means[,2], pch = 16)
	  text(means[,3], means[,2], col = "cornflowerblue", labels = txt, pos = 3, offset = 0.5)
	}
}

