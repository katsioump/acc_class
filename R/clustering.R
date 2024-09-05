# options(digits.secs = 3)

# Visualize acceleration (as VeDBA) 
# require(ggplot2)

# ggplot(tl) + 
#   geom_point(aes(x=date_time, y=VeDBA)) +
#   scale_y_continuous("VeDBA (m/s2)") + # , sec.axis = sec_axis(~ . * 10, name = "Temperature (ºC)")) +
#   scale_x_datetime("Date") +
#   theme_bw(base_size = 12) +
#   theme(text = element_text(size=30), 
#         axis.text = element_text(colour = "black"),
#         axis.title.y = element_text(vjust = +2),
#        legend.position = "none") # c(.1, .8))

############################# RUN CODE ###########################

# the dataset (tl) used is the output of the 'calculate metrics' function
# @example

# tl$date_time <- paste (tl$Date, tl$Time)
# tl$date_time <- strptime(tl$date_time, format = "%Y-%m-%d %H:%M:%OS")
# tl$date_time <- as.POSIXct(tl$date_time)

# metrics <- c('DBAx','SDdbax', 'MAXdbax')
# columns <- c('Time','date_time', 'VeDBA','DBAx','SDdbax', 'MAXdbax')
#
# source('clustering.R')
# thresholds <- clustering(data = tl, columns = columns, metrics = metrics)
# thresholds

clustering <- function(data = data, columns = columns, metrics = metrics){
  
  tl_c <- subset(tl, select = columns)
  
  na_locs <- which(!complete.cases(tl_c))
  tl_c <- tl_c[-na_locs,]
  
  night <- which(tl_c$Time <= "06:29:00.000" | tl_c$Time >= "20:30:00.000")
  tl_night <- tl_c[night, ]
  
  # 1st step is data scaling.
  tl_c_scaled <- scale(tl_c[,metrics])
  tl_night_scaled <- scale(tl_night[,metrics])
  
  require(cluster)
  
  clus <- clara(tl_c_scaled,
                k = 2,
                stand = T,
                samples = 100,
                metric = "manhattan",
                pamLike = T)
  
  tl_c$clustering <- as.factor(clus$clustering)
  
  
  clus_night <- clara(tl_night_scaled,
                      k = 2,
                      stand = T,
                      samples = 100,
                      metric = "manhattan",
                      pamLike = T)
  
  tl_night$clustering <- as.factor(clus_night$clustering)
  
  threshold_all <- sort(with(tl_c, tapply(VeDBA, clustering, max)))
  threshold_night <- sort(with(tl_night, tapply(VeDBA, clustering, max)))
  
  
  tl_c$Move <- factor(ifelse(tl_c$VeDBA <= threshold_all[1], "immobile", "mobile"))
  tl_night$Move <- factor(ifelse(tl_night$VeDBA <= threshold_night[1], "immobile", "mobile"))
  
  require(ggplot2)
  
  ### specifically for this example to take less time to run, take it off afterwards
  # require(lubridate)
  # tl_c <- subset(tl_c, day(tl_c[,'date_time']) == 23)
  # tl_night <- subset(tl_night, day(tl_night[,'date_time']) == 23)
  
  threshold1 <- format(round(threshold_all[1], 3), nsmall = 3)
  threshold2 <- format(round(threshold_night[1], 3), nsmall = 3)
  
  p1 <- ggplot(tl_c) + 
    geom_point(aes(x=date_time, y=VeDBA, col= as.factor(VeDBA <= threshold_all[1]))) +
    scale_y_continuous("VeDBA (g)") +
    scale_x_datetime("Date") +
    geom_hline(yintercept = threshold_all[1]) +
    scale_color_discrete(name = "Cluster", label = c('Mobile', 'Immobile')) +
    ggtitle(paste0('Clustering of acceleration metrics for all observations \nMovement threshold =', threshold1, "g")) +
    theme_bw(base_size = 12) +
    theme(text = element_text(size=15), 
          axis.text = element_text(colour = "black"),
          axis.title.y.left = element_text(vjust = +2),
          axis.title.y.right = element_text(vjust = +2),
          legend.position = c(.1, .8)) 
  
  
  p2 <- ggplot(tl_c) + 
    geom_point(aes(x=date_time, y=VeDBA, col= as.factor(VeDBA <= threshold_night[1]))) +
    scale_y_continuous("VeDBA (g)") +
    scale_x_datetime("Date") +
    geom_hline(yintercept = threshold_night[1]) +
    # scale_color_discrete(name = "Cluster", label = c('Mobile', 'Immobile')) +
    ggtitle(paste0('Clustering of acceleration metrics for nighttime observations \nMovement threshold =', threshold2, "g")) +
    theme_bw(base_size = 12) +
    theme(text = element_text(size=15), 
          axis.text = element_text(colour = "black"),
          axis.title.y.left = element_text(vjust = +2),
          axis.title.y.right = element_text(vjust = +2),
          legend.position = 'none') 
  
  require(gridExtra)
  grid.arrange(p1, p2, nrow = 2)
  
  out <- list(threshold_all[1], threshold_night[1])
  
  return(out)
  
}
