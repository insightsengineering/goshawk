#' Create overall distribution box plot with treatment arms 
#'



require(ggplot2)

dist.boxplot.int <- function(data,
                         biomarker,
                         arm,
                         loq_flag,
                         unit,
                         timepoint,
                         color_manual,
                         shape_manual,
                         box = TRUE,
                         logscale = FALSE,
                         facet = NULL) {
  

  # Base plot
 plot1 <-  ggplot() +
    geom_jitter(data = data,
                aes_string( x = arm,
                            y = biomarker,
                            color = arm,
                            shape = loq_flag),
                position = position_jitter(width = 0.2, height = 0),
                size=1) +
    scale_color_manual(values = color_manual, name = 'Arm') +
    scale_fill_manual(values = color_manual, name = 'Arm')  +
    scale_shape_manual(values = shape_manual, name = 'LoQ') +
    theme_bw() +
    xlab('Arm') +
    ylab(paste0(biomarker,' ','(',unit,')')) +
    ggtitle(paste0(biomarker,' distribution @ ',timepoint,' per arm'))
 
  
  # Add boxes if there is enough observatoin
  if (box) {
    plot1 <- plot1 +
      geom_boxplot(data = data,
                   aes_string( x = arm,
                               y = biomarker,
                               color = arm,
                  fill = NULL),
                  outlier.shape = NA) +
      geom_jitter(data = data,
                  aes_string( x = arm,
                              y = biomarker,
                              color = arm,
                              shape = loq_flag),
                  position = position_jitter(width = 0.2, height = 0),
                  size=1)
  } 
  
  #Adjust scale to log
  if (logscale){
    plot1 <- plot1 +
      coord_trans( y = "log10")
  }
  
  #Add facetting
  if (!is.null(facet)){
    plot1 <- plot1 +
      facet_grid(as.formula(paste0('.~',facet)))
  }
 
 
  
  plot1
  
}




# Example:

# Create dataset
# biomarker <- runif(100,3,1000)
# arm <- c(rep('trt',50),rep('pbo',50))
# visit <- c(rep(c(0,4,8,12),25))
# loq_flag <- c(rep(c(rep('YES',10),rep('NO',10)),5))
# example <- data.frame(biomarker = biomarker, arm = arm, loq_flag = loq_flag, visit = visit)
# colnames(example) <- c('IGG','arm','loq_flag','visit')
# unit <- 'g/L'
# timepoint <- 'screening'
#color_manual <- c('pbo' = "#1F78B4", 'trt' = "#33A02C")
#shape_manual <- c('NO' = 1, 'YES' = 2, 'NA' = 0)

# Call function without facet
# dist.boxplot.int(example,'IGG','arm','loq_flag',
#                 unit, timepoint, color_manual,
#                 shape_manual,
#                 box = TRUE,
#                 logscale = FALSE)


# Call function with facet
# timepoint <- 'over time'
# dist.boxplot.int(example,'IGG','arm','loq_flag',
#                 unit, timepoint, color_manual,
#                 shape_manual,
#                 box = TRUE,
#                 logscale = FALSE,
#                  facet = 'visit')

