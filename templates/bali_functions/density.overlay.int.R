# Create overall density plot with treatment arms overlaid 


require(ggplot2)

density.overlay.int <- function(data,
                            biomarker,
                            arm,
                            unit,
                            timepoint,
                            color_manual,
                            logscale = FALSE,
                            facet = NULL) {

  
  plot1 <- ggplot() +
    geom_density(data = data, aes(x = eval(parse(text = biomarker)), linetype = 'all')) + 
    geom_density(data = data, aes_string(x = biomarker, colour = arm)) +
    scale_color_manual(values = color_manual, name = 'Arm') +
    scale_linetype_manual(name = "All", values = c(all="solid", per_dose="solid")) +
    theme_bw() +
    ggtitle(paste0(biomarker,' density overall & per dose group @ ',timepoint)) +
    xlab(paste0(biomarker,' (',unit,')'))

  
  #Adjust scale to log
  if (logscale){
    plot1 <- plot1 +
      coord_trans( x = "log10")
  }
  
  #Add faceting
  if (!is.null(facet)){
    plot1 <- plot1 +
      facet_grid(as.formula(paste0('.~',facet)))
  }
  
  plot1

}


# Example:
#
# biomarker <- runif(100,3,1000)
# arm <- c(rep('trt',50),rep('pbo',50))
# visit <- c(rep(c(0,4,8,12),25))
# example <- data.frame(biomarker = biomarker, arm = arm, visit = visit)
# colnames(example) <- c('IGG','arm','visit')
# unit <- 'g/L'
# timepoint <- 'screening'
# color_manual <- c('pbo' = "#1F78B4", 'trt' = "#33A02C")


# Call function without facet
# density.overlay.int(example,'IGG','arm',
#                    unit,timepoint,
#                    color_manual,
#                    logscale = FALSE,
#                    facet = NULL)


# Call function with facet
# density.overlay.int(example,'IGG','arm',
#                    unit,timepoint,
#                    color_manual,
#                    logscale = FALSE,
#                    facet = 'visit')
