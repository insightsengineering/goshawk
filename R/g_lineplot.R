#' Function to create line plot of summary statistics over time.
#' 
#' @param label text string to be displayed as plot label.
#' @param data data frame with variables to be summarized and generate statistics which will display in the plot.
#' @param biomarker_var name of variable containing biomarker names.
#' @param biomarker_var_label name of variable containing biomarker labels.
#' @param biomarker biomarker name to be analyzed. 
#' @param value_var name of variable containing biomarker results.
#' @param unit_var name of variable containing biomarker result unit.
#' @param trt_group name of variable representing treatment group.
#' @param trt_group_level vector that can be used to define the factor level of trt_group.
#' @param shape Name of variable to determine shape of points. Allows splitting by two levels 
#' @param time name of vairable containing visit names.
#' @param time_level vector that can be used to define the factor level of time. Only use it when x-axis variable is character or factor.
#' @param color_manual vector of colors.
#' @param ylim numeric vector to define y-axis range.
#' @param median boolean whether to display median results.
#' @param hline numeric value represnting intercept of horizontal line.
#' @param xtick numeric vector to define the tick values of x-axis when x variable is numeric. Default value is waiver().
#' @param xlabel vector with same length of xtick to define the label of x-axis tick values. Default value is waiver().
#' @param rotate_xlab boolean whether to rotate x-axis labels.
#' @param font_size control font size for title, x-axis, y-axis and legend font.
#' @param dodge control position dodge
#' @param plot_height Height of produced plot. 989 pixels by default
#' 
#' @import ggplot2
#' @import dplyr
#' @import grid
#' @importFrom stringr str_wrap
#' @importFrom stringr str_to_title
#' @importFrom gridExtra grid.arrange
#' @importFrom grid unit.pmax
#' @importFrom cowplot plot_grid
#'
#' @author Balazs Toth (toth.balazs@gene.com)
#' @author Wenyi Liu (wenyi.liu@roche.com)
#'
#' @details Currently, the output plot can display mean and median of input value. For mean, the error bar denotes
#' 95\% confidence interval. For median, the error bar denotes median-25% quartile to median+75% quartile.
#'
#' @return \code{ggplot} object
#'
#' @export
#'
#' @examples
#' 
#'\dontrun{
#' # EXAMPLE:
#' 
#' library(dplyr)
#' library(ggplot2)
#' library(gridExtra)
#' library(grid)
#' library(stringr)
#'
#' ANL <- expand.grid(
#'   USUBJID = paste0("p-",1:100),
#'   VISITN = c(1, 4:10),
#'   ARM = c("ARM A", "ARM B", "ARM C"),
#'   SEX = c("M", "F"),
#'   PARAMCD = c("CRP", "IGG", "IGM"),
#'   PARAM = c("C-reactive protein", "Immunoglobulin G", "Immunoglobulin M")
#' )
#' ANL$VISIT <- paste0("visit ", ANL$VISITN)
#' ANL$AVAL <- rnorm(nrow(ANL))
#' ANL$CHG <- rnorm(nrow(ANL), 2, 2)
#' ANL$CHG[ANL$VISIT == "visit 1"] <- NA
#' ANL$PCHG <- ANL$CHG/ANL$AVAL*100
#' ANL$AVALU <- 'mg'
#' 
#' ANL$ARM <- factor(ANL$ARM)
#' ANL$VISIT <- factor(ANL$VISIT)
#' ANL <- ANL[-115,]
#' 
#' g_lineplot(label = 'Line Plot',
#'            data = ANL,
#'            biomarker_var = 'PARAMCD',
#'            biomarker = 'CRP',
#'            value_var = 'AVAL',
#'            trt_group = 'ARM',
#'            shape = "SEX",
#'            time = 'VISIT',
#'            color_manual = NULL,
#'            median = FALSE,
#'            hline = NULL,
#'            rotate_xlab = FALSE)
#'            
#'}
#'

g_lineplot <- function(label = 'Line Plot',
                       data,
                       biomarker_var = 'PARAMCD',
                       biomarker_var_label = 'PARAM',
                       biomarker,
                       value_var = 'AVAL',
                       unit_var = 'AVALU',
                       ylim = NULL,
                       trt_group,
                       trt_group_level = NULL,
                       shape = NULL,
                       time,
                       time_level = NULL,
                       color_manual = NULL,
                       median = FALSE,
                       hline = NULL,
                       xtick = waiver(),
                       xlabel = xtick,
                       rotate_xlab = FALSE,
                       font_size = 12,
                       dodge = 0.4,
                       plot_height = 989) {
  
  ## Pre-process data
  if(!is.null(trt_group_level)){
    data[[trt_group]] <- factor(data[[trt_group]],
                                levels = trt_group_level)
  } else {
    data[[trt_group]] <- factor(data[[trt_group]])
  }
  
  if(is.factor(data[[time]]) | is.character(data[[time]])){
    xtype <- 'discrete'
  } else {
    xtype <- 'continuous'
  }
  
  if(xtype == 'discrete'){
    if(!is.null(time_level)){
      data[[time]] <- factor(data[[time]],
                             levels = time_level)
    } else {
      data[[time]] <- factor(data[[time]])
    }
  }
  
  groupings <- c(time, trt_group, shape)
  
  ## Summary statistics
  sum_data <- data %>%
    filter(eval(parse(text = biomarker_var)) == biomarker) %>%
    group_by_at(groupings) %>%
    summarise(count = sum(!is.na(eval(parse(text = value_var)))),
              mean = mean(eval(parse(text = value_var)),na.rm = TRUE),
              CIup = mean(eval(parse(text = value_var)),na.rm = TRUE) + 1.96 * sd(eval(parse(text = value_var)), na.rm = TRUE)/sqrt(n()),
              CIdown = mean(eval(parse(text = value_var)),na.rm = TRUE) - 1.96 * sd(eval(parse(text = value_var)), na.rm = TRUE)/sqrt(n()),
              median = median(eval(parse(text = value_var)),na.rm = TRUE),
              quant25 = quantile(eval(parse(text = value_var)), 0.25, na.rm = TRUE),
              quant75 = quantile(eval(parse(text = value_var)), 0.75, na.rm = TRUE)) %>% 
    arrange_at(c(trt_group, shape))
  
  
  listin <- list()
  listin[[trt_group]] <- sum_data[[trt_group]]
  if(!is.null(shape)){
    listin[[shape]] <- sum_data[[shape]]
  }
  
  
  int <- unique_name("int", names(sum_data))
  
  sum_data[[int]] <- new_interaction(listin, sep = " ")
  sum_data[[int]] <- str_wrap(sum_data[[int]], 12)
  sum_data[[int]] <- factor(sum_data[[int]], sort(unique(sum_data[[int]])))


  unfiltered_data <- sum_data
  
  
  ## Base plot
  pd <- position_dodge(dodge)

  if (median) {
    line <- 'median'
    up_limit <- 'quant75'
    down_limit <- 'quant25'
  } else {
    line <- 'mean'
    up_limit <- 'CIup'
    down_limit <- 'CIdown'
  }
  
  filtered_data <- data %>% 
    filter_at(biomarker_var, any_vars(.==biomarker))
  
  unit <- filtered_data %>% 
    pull(unit_var) %>% 
    unique()
  
  unit1 <- ifelse(is.na(unit) | unit == "",
                  " ",
                  paste0(' (', unit, ') '))
  
  biomarker1 <- filtered_data %>% 
    pull(biomarker_var_label) %>% 
    unique() 
    
  
  gtitle <- paste0(biomarker1, unit1, str_to_title(line), ' by Treatment @ Visits')
  gylab <- paste0(biomarker1, ' ', str_to_title(line), ' of ', value_var, ' Values')
  
  # re-establish treatment variable label
  if (trt_group == "ARM"){
    attributes(sum_data$ARM)$label <- "Planned Arm"
  } else {
    attributes(sum_data$ACTARM)$label <- "Actual Arm"
  }
  
  # Setup legend label
  if(is.null(attr(sum_data[[trt_group]], "label"))){
    trtLabel <- "Dose"
  } else {
    trtLabel <- attr(sum_data[[trt_group]], "label")
  }
  
  if (is.null(shape)){
    plot1 <-  ggplot(data = sum_data,
                     aes_string(x = time,
                                y = line,
                                color = trt_group,
                                group  = int)) + theme_bw()  +
      geom_point(position = pd)
    # Add manual color
    if (!is.null(color_manual)){
      
      vals <- color_manual
      plot1 <- plot1 +
        scale_color_manual(values = vals, name = trtLabel)
    }
      
  }else{
    
    ncol <- nlevels(as.factor(unfiltered_data[[trt_group]]))
    nshape <- nlevels(as.factor(unfiltered_data[[shape]]))
    
    plot1 <-  ggplot(data = sum_data,
                     aes_string(x = time,
                                y = line,
                                color = int,
                                group  = int,
                                shape = int)) + theme_bw()


    # Add manual color
    
    if (!is.null(color_manual)){
      vals <- rep(color_manual, rep(nshape, ncol))
      
      plot1 <- plot1 +
        scale_color_manual(" ",values = as.character(vals))
    }else{
      colors <- gg_color_hue(ncol)
      vals <- rep(colors, rep(nshape, ncol))
      plot1 <- plot1 +
        scale_color_manual(" ",values = vals)
    }
    shapes <- c(15, 16, 17, 18, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
                0, 1, 2)
    
    if (nshape>length(shapes)){
      warning("Number of available shapes exceeded, values will cycle!")
    }
    
    select <- (1:nshape)%%(length(shapes))
    select <- ifelse(select==0, length(shapes), select)
    
    selected_shapes <- shapes[select]
    
    vals <- rep(selected_shapes, ncol)
    
    plot1 <- plot1 + scale_shape_manual(" ",
      values = vals )+
      theme(legend.key.size=unit(1, "cm")) +
      geom_point(position = pd, size =3)
  }
  
  plot1 <-  plot1 +
    geom_line(position = pd) +
    geom_errorbar(aes_string(ymin = down_limit,
                             ymax = up_limit),
                  width=0.9,
                  position = pd) +
    ggtitle(gtitle) +
    labs(caption = paste("The output plot can display mean and median of input value.
                         For mean, the error bar denotes 95% confidence interval.
                         For median, the bar denotes the first to third quartile.")) +
    xlab(time) + 
    ylab(gylab)+
    theme(legend.position = "bottom", legend.direction = "horizontal", 
          plot.title = element_text(size=font_size, margin = margin(), hjust = 0.5),
          axis.title.y = element_text(margin = margin(r = 20)))+
    guides(color=guide_legend(byrow=TRUE))
 
  # Apply y-axis zoom range
  if(!is.null(ylim)){
    plot1 <- plot1 + coord_cartesian(ylim = ylim)
  }
  
  # Format x-label
  if(xtype == 'continuous') {
    plot1 <- plot1 + 
      scale_x_continuous(breaks = xtick, labels = xlabel, limits = c(NA, NA))
  }
  
  if (rotate_xlab){
    plot1 <- plot1 +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }



  #Add horizontal line
  if (!is.null(hline)){
    plot1 <- plot1 +
      geom_hline(aes(yintercept = hline), color="red", size=0.5)
  }
  
  # Format font size
  if (!is.null(font_size)){
    plot1 <- plot1 +
      theme(axis.title.x = element_text(size = font_size),
            axis.text.x = element_text(size = font_size),
            axis.title.y = element_text(size = font_size),
            axis.text.y = element_text(size = font_size),
            legend.title = element_text(size = font_size),
            legend.text = element_text(size = font_size))
  }

  labels <- rev(levels(sum_data[[int]]))
  lines <- sum(str_count(unique(labels), "\n")) * 1/2 + length(unique(labels))
  
  minline <- 36
  tabletotal <- lines*minline
  
  plotsize <- plot_height - tabletotal
  
  if(plotsize <= 250){
    stop("plot height is not sufficient to display!")
  }
  
  tbl <- ggplot(sum_data, aes_string(x = time, y = int, label = 'count')) +
    geom_text(size = 4.5) +
    ggtitle("Number of observations") + 
    theme_minimal() +
    scale_y_discrete(labels = labels) + 
    theme(panel.grid.major = element_blank(), legend.position = "none",
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), axis.text.x =  element_blank(),
          axis.ticks =  element_blank(),
          axis.title.x=element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size=font_size),
          plot.title = element_text(face = "bold", size=font_size))
  

  
  #Plot the two grobs using plot_grid
 
    
    plot_grid(plot1, tbl, align = "v", ncol = 1, rel_heights = c(plotsize, tabletotal))


 
}


new_interaction <- function(args, drop = FALSE, sep = ".", lex.order = FALSE){
  for (i in 1:length(args)){
    if (is.null(args[[i]])){
      args[[i]] <- NULL
    }
  }
  if (length(args) == 1){
    return(paste0(names(args), ":", args[[1]]))
  }
  args <- mapply(function(n,val) paste0(n, ":", val), names(args), args, SIMPLIFY = FALSE)
  interaction(args, drop = drop, sep = sep, lex.order = lex.order)
}

unique_name <- function(newname, old_names){
  if (newname %in% old_names){
    unique_name(paste0(newname,"1"), old_names)
  }
  newname
}


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}