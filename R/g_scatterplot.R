#' Function to generate a scatter plot
#' Output rendered by teal.goshawk module \code{g_scatterplot} returns scatter plot visualiztion
#'
#' @param add as many param lines as there are params that this function expects
#' @param add as many param lines as there are params that this function expects
#'
#' @author Balazs Toth
#' @author Nick Paszty
#'
#' @details provide additional information as needed. link to specification file \url{http://rstudio.com}
#'
#' @return \code{scatterplot} object
#'
#' @export
#'
#' @examples
#' # Create dataset
#' biomarker <- runif(100,3,100)
#' biomarker.bl <- NULL
#'  for (i in 0:24) {
#'    biomarker.bl <- c ( biomarker.bl, rep(biomarker[i * 4 + 1], 4))
#'  }
#' arm <- c(rep('trt',50),rep('pbo',50))
#' visit <- c(rep(c(0,4,8,12),25))
#' loq_flag <- c(rep(c(rep('YES',10),rep('NO',10)),5))
#' example <- data.frame(biomarker = biomarker, biomarker.bl = biomarker.bl, arm = arm, loq_flag = loq_flag, visit = visit)
#' colnames(example) <- c('IGG', 'IGG.bl', 'arm','loq_flag','visit')
#' unit <- 'g/L'
#' timepoint <- 'screening'
#' color_manual <- c('pbo' = "#1F78B4", 'trt' = "#33A02C")
#' shape_manual <- c('NO' = 1, 'YES' = 2, 'NA' = 0)

#' # Call function simple
#' g_scatterplot (data = example,
#'                   biomarker = 'IGG',
#'                   biomarker.bl = 'IGG.bl',
#'                   trt_group = 'arm',
#'                   time = 'visit',
#'                   loq_flag = 'loq_flag',
#'                   unit,
#'                   timepoint,
#'                   color_manual,
#'                   shape_manual)
#' 
#' # Call function facet
#' g_scatterplot (data = example,
#'                   biomarker = 'IGG',
#'                   biomarker.bl = 'IGG.bl',
#'                   trt_group = 'arm',
#'                   time = 'visit',
#'                   loq_flag = 'loq_flag',
#'                   unit,
#'                   timepoint,
#'                   color_manual,
#'                   shape_manual,
#'                   logscale = TRUE,
#'                   facet = 'arm',
#'                   pct = FALSE,
#'                   reg.line = FALSE)
#' 
#' # Call function facet and logscale
#' g_scatterplot (data = example,
#'                   biomarker = 'IGG',
#'                   biomarker.bl = 'IGG.bl',
#'                   trt_group = 'arm',
#'                   time = 'visit',
#'                   loq_flag = 'loq_flag',
#'                   unit,
#'                   timepoint,
#'                   color_manual,
#'                   shape_manual,
#'                   logscale = TRUE,
#'                   facet = 'arm',
#'                   pct = FALSE,
#'                   reg.line = FALSE)
#' 
#' # Call function facet and pct
#' g_scatterplot (data = example,
#'                   biomarker = 'IGG',
#'                   biomarker.bl = 'IGG.bl',
#'                   trt_group = 'arm',
#'                   time = 'visit',
#'                   loq_flag = 'loq_flag',
#'                   unit,
#'                   timepoint,
#'                   color_manual,
#'                   shape_manual,
#'                   logscale = FALSE,
#'                   facet = 'arm',
#'                   pct = TRUE,
#'                   reg.line = FALSE)
#' 
#' # Call function regression line
#' g_scatterplot (data = example,
#'                   biomarker = 'IGG',
#'                   biomarker.bl = 'IGG.bl',
#'                   trt_group = 'arm',
#'                   time = 'visit',
#'                   loq_flag = 'loq_flag',
#'                   unit,
#'                   timepoint,
#'                   color_manual,
#'                   shape_manual,
#'                   logscale = FALSE,
#'                   facet = 'arm',
#'                   pct = FALSE,
#'                   reg.line = TRUE)
#' 
#' # Call function regression line
#' g_scatterplot (data = example,
#'                   biomarker = 'IGG',
#'                   biomarker.bl = 'IGG.bl',
#'                   trt_group = 'arm',
#'                   time = 'visit',
#'                   loq_flag = 'loq_flag',
#'                   unit,
#'                   timepoint,
#'                   color_manual,
#'                   shape_manual,
#'                   logscale = FALSE,
#'                   facet = 'arm',
#'                   pct = TRUE,
#'                   reg.line = TRUE)
#' 
#' # Call function regression line
#' g_scatterplot (data = example,
#'                   biomarker = 'IGG',
#'                   biomarker.bl = 'IGG.bl',
#'                   trt_group = 'arm',
#'                   time = 'visit',
#'                   loq_flag = 'loq_flag',
#'                   unit,
#'                   timepoint,
#'                   color_manual,
#'                   shape_manual,
#'                   logscale = FALSE,
#'                   facet = 'arm',
#'                   pct = FALSE,
#'                   reg.line = TRUE)
#'
#'

# library includes - move these calls to app.R when testing is completed
#library(ggplot2)
#library(dplyr)
#library(mcr)

g_scatterplot <- function(data, biomarker, biomarker.bl, trt_group, time, loq_flag, unti, timepoint, color_manual, shape_manual, 
                          logscale = FALSE, facet = NULL, pct = FALSE, reg.line = FALSE){


# Create line plot over time per treatment arm 
#
# Helper
  min <- min(data[[biomarker]], na.rm = TRUE)
  max <- max(data[[biomarker]], na.rm = TRUE)

  
  # Base plot
  plot1 <-  ggplot(data = data,
                   aes_string(x = biomarker.bl,
                              y = biomarker,
                              color = trt_group)) +
    geom_point(aes_string(shape = loq_flag), size = 1) +
    facet_wrap(as.formula(paste0('~',time))) +
    theme_bw() +
    scale_color_manual(values = color_manual, name = 'Dose') +
    scale_shape_manual(values = shape_manual, name = 'LoQ') +
    xlim(min, max) + ylim(min, max) +
    ggtitle(paste0(biomarker,' ', '(',unit,')',' scatter plot; over time pairwise')) +
    xlab(paste0(biomarker, ' @ ',timepoint)) +
    ylab(paste0(biomarker, ' @ follow up'))
    
    
    # Add facet
    if (!is.null(facet)){
      plot1 <- plot1 +
        facet_grid(as.formula(paste0(facet,' ~ ',time)))
    }
    
    
    # Format scale
    if (logscale & !pct){
      plot1 <- plot1 +
        coord_trans( x = "log10", y = "log10")
      
    } else if (logscale & pct) {
      plot1 <- plot1 +
        coord_trans( x = "log10", y = "identity", limy = c(0, 200)) +
        scale_y_continuous(
          breaks = seq(floor(min(data[[biomarker]] / 10)) * 10,
                       ceiling(max(data[[biomarker]] / 10)) * 10,
                       by = 20))
          #coord_cartesian(ylim=c(0, 200)) +
          
      
    } else if (!logscale & pct) {
      plot1 <- plot1 +
        scale_y_continuous(
          breaks = seq(floor(min(data[[biomarker]] / 10)) * 10,
                       ceiling(max(data[[biomarker]] / 10)) * 10,
                       by = 20)) +
        coord_cartesian(ylim=c(0, 200))
    }

    
    # Add regression line
    if (reg.line){
      
      slope <- function(x, y) {
        ratio <- sd(x)/sd(y)
        reg <- mcr:::mc.deming(y, x, ratio)
        return(c(round(reg$b0,2),round(reg$b1,2)))
      }
      
      
      sub_data <- data %>%
        group_by(eval(parse(text = trt_group)),
                 eval(parse(text = time))) %>%
        mutate(intercept = slope(eval(parse(text = biomarker)),
                                 eval(parse(text = biomarker.bl)))[1]) %>%
        mutate(slope = slope(eval(parse(text = biomarker)),
                             eval(parse(text = biomarker.bl)))[2]) %>%
        mutate(corr = cor(eval(parse(text = biomarker)),
                          eval(parse(text = biomarker.bl)),
                          method = "spearman",
                          use = 'complete.obs'))
        
        plot1 <- plot1 +
          geom_abline(data = sub_data,
                      aes_string(intercept = 'intercept',
                                 slope = 'slope',
                                 color = trt_group)) +
          geom_text(data = sub_data, aes(x = -Inf,
                                         y = Inf,
                                         hjust = 0,
                                         vjust = 1,
                                         label = paste0('y=',round(intercept,2),
                                                        '+',
                                                        round(slope,2),
                                                        'X', "\n",
                                                        'cor=',
                                                        round(corr,2)),
                                         color = eval(parse(text = trt_group))),
                    size = 3)
        
      } else if (!pct) {
        plot1 <- plot1 +
          geom_abline(intercept = 0, slope = 1)
    }
    

  plot1
  
  #return(scatterplot)

}
