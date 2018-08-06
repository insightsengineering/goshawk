#' Create line plot over time per treatment arm 
#'
# libr
#require(ggplot2)
#require(dplyr)

line.plot.int <- function(label = 'Line Plot',
                          data,
                          biomarker,
                          trt_group,
                          time,
                          unit,
                          timepoint,
                          color_manual,
                          median = FALSE,
                          hline = NULL,
                          pct = FALSE) {
  
  # Summary statistics
  sum_data <- data %>%
    group_by(eval(parse(text = time)),
             eval(parse(text = trt_group))) %>%
    summarise(count = sum(!is.na(eval(parse(text = biomarker)))),
              mean = mean(eval(parse(text = biomarker)),na.rm = TRUE),
              CIup = mean(eval(parse(text = biomarker)),na.rm = TRUE) + 1.96 * sd(eval(parse(text = biomarker)), na.rm = TRUE)/sqrt(n()),
              CIdown = mean(eval(parse(text = biomarker)),na.rm = TRUE) - 1.96 * sd(eval(parse(text = biomarker)), na.rm = TRUE)/sqrt(n()),
              median = median(eval(parse(text = biomarker)),na.rm = TRUE),
              quant25 = quantile(eval(parse(text = biomarker)), 0.25, na.rm = TRUE),
              quant75 = quantile(eval(parse(text = biomarker)), 0.75, na.rm = TRUE))
  colnames(sum_data)[1:2] <- c(time,trt_group)
  
  
  # Base plot
  pd <- position_dodge(0.8)
  
  if (median) {
    line <- 'median'
    up_limit <- 'quant75'
    down_limit <- 'quant25'
  } else {
    line <- 'mean'
    up_limit <- 'CIup'
    down_limit <- 'CIdown'
  }
  
  title <- ''
  if (pct) {
    title <- paste0(' pct of ', timepoint)
  }
  
  
  plot1 <-  ggplot(data = sum_data,
                   aes_string(x = time,
                              y = line,
                              color = trt_group,
                              group = trt_group)) +
    geom_point(position = pd) +
    geom_line(position = pd) +
    geom_errorbar(aes_string(ymin = down_limit,
                             ymax = up_limit),
                  width=1,
                  position = pd) +
    theme_bw() +
    scale_color_manual(values = color_manual, name = 'Dose') +
    theme(legend.position = "bottom") +
    ggtitle(paste0(biomarker, ' ', line, ' over time')) +
    xlab('time') + ylab(paste0(biomarker, ' ', line, title))
    
    
    #Add horizontal line
    if (!is.null(hline)){
      plot1 <- plot1 +
        geom_hline(aes(yintercept = hline))
    }
    
    
    #Format y scale
    if (pct){
      plot1 <- plot1 +
        scale_y_continuous(
          breaks = seq(floor(min(sum_data[[line]] / 10)) * 10,
                       ceiling(max(sum_data[[line]] / 10)) * 10,
                               by = 20)) +
        coord_cartesian(ylim=c(0, 200))
      }
    

  plot1
  
}   


# Example:

# Create dataset
 biomarker <- runif(100,3,100)
 arm <- c(rep('trt',50),rep('pbo',50))
 visit <- c(rep(c(0,4,8,12),25))
 loq_flag <- c(rep(c(rep('YES',10),rep('NO',10)),5))
 example <- data.frame(biomarker = biomarker, arm = arm, loq_flag = loq_flag, visit = visit)
 colnames(example) <- c('IGG','arm','loq_flag','visit')
 unit <- 'g/L'
 timepoint <- 'screening'
 color_manual <- c('pbo' = "#1F78B4", 'trt' = "#33A02C")
 shape_manual <- c('NO' = 1, 'YES' = 2, 'NA' = 0)

# remove(arm)
# remove(biomarker)



# Call function simple
line.plot.int (example,
               'IGG',
               'arm',
               'visit',
                unit,
                timepoint,
                color_manual)


# Call function median pct
# line.plot.int (example,
#               'IGG',
#               'arm',
#               'visit',
#                unit,
#                timepoint,
#                color_manual,
#                median = TRUE,
#                pct = TRUE,
#                hline = 30)


  