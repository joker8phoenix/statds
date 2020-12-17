far.from.others <- function(all.groups,...,log.tran.x=FALSE, log.tran.y=FALSE,debug=FALSE){
  
  cn = colnames(all.groups)
  colnames(all.groups) = c("x", "y", "group")
  if(log.tran.x){
    all.groups$x = log(all.groups$x)
    all.groups = all.groups %>% filter(!is.infinite(x*y))
  }
  if(log.tran.y){
    all.groups$y = log(all.groups$y)
    all.groups = all.groups %>% filter(!is.infinite(x*y))
  }
  
  group.data <- split(all.groups, all.groups$group)
  group.list <- list()
  for(groups in names(group.data)){
    ## Run linear interpolation to get a set of points on which we
    ## could place the label (this is useful for e.g. the lasso path
    ## where there are only a few points plotted).
    approx.list <- with(group.data[[groups]], approx(x, y))
    if(debug){
      with(approx.list, grid.points(x, y, default.units="cm"))
    }
    group.list[[groups]] <- data.frame(approx.list, groups)
  }
  
  # xlimits = grid::convertX(unit(c(0, 1), "npc"), "cm", valueOnly = TRUE)
  
  output <- data.frame()
  for(group.i in seq_along(group.list)){
    one.group <- group.list[[group.i]]
    ## From Mark Schmidt: "For the location of the boxes, I found the
    ## data point on the line that has the maximum distance (in the
    ## image coordinates) to the nearest data point on another line or
    ## to the image boundary."
    dist.mat <- matrix(NA, length(one.group$x), 3)
    colnames(dist.mat) <- c("x","y","other")
    ## dist.mat has 3 columns: the first two are the shortest distance
    ## to the nearest x and y border, and the third is the shortest
    ## distance to another data point.
    for(xy in c("x", "y")){
      xy.vec <- one.group[,xy]
      xy.mat <- rbind(xy.vec, xy.vec)
      # lim.fun <- get(sprintf("%slimits", xy))
      diff.mat <- xy.mat - lim.fun()
      diff.mat <- xy.mat - grid::convertX(unit(c(0, 1), "npc"), "cm", valueOnly = TRUE)
      dist.mat[,xy] <- apply(abs(diff.mat), 2, min)
    }
    other.groups <- group.list[-group.i]
    other.df <- do.call(rbind, other.groups)
    for(row.i in 1:nrow(dist.mat)){
      r <- one.group[row.i,]
      other.dist <- with(other.df, (x-r$x)^2 + (y-r$y)^2)
      dist.mat[row.i,"other"] <- sqrt(min(other.dist))
    }
    shortest.dist <- apply(dist.mat, 1, min)
    # picked <- calc.boxes(one.group[which.max(shortest.dist),])
    picked <- one.group[which.max(shortest.dist),]
    ## Mark's label rotation: "For the angle, I computed the slope
    ## between neighboring data points (which isn't ideal for noisy
    ## data, it should probably be based on a smoothed estimate)."
    # left <- max(picked$left, min(one.group$x))
    # right <- min(picked$right, max(one.group$x))
    # neighbors <- approx(one.group$x, one.group$y, c(left, right))
    # slope <- with(neighbors, (y[2]-y[1])/(x[2]-x[1]))
    # picked$rot <- 180*atan(slope)/pi
    output <- rbind(output, picked)
  }
  if(log.tran.x){
    output$x = exp(output$x)
  }
  if(log.tran.y){
    output$y = exp(output$y)
  }
  colnames(output) = cn
  output
}
