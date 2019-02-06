#' Function to create a correlation plot.
#'
#' Default plot displays correlation facetted by visit with color attributed treatment arms and symbol attributed LOQ values.
#'
#' @param label text string to used to identify plot.
#' @param data ADaM structured analysis laboratory data frame e.g. ALB.  
#' @param param_var name of variable containing biomarker codes e.g. PARAMCD.
#' @param xaxis_param x-axis biomarker to visualize e.g. IGG. 
#' @param xaxis_var name of variable containing biomarker results displayed on X-axis e.g. BASE.
#' @param yaxis_param y-axis biomarker to visualize e.g. IGG. 
#' @param yaxis_var name of variable containing biomarker results displayed on Y-axise.g. AVAL.
#' @param trt_group name of variable representing treatment group e.g. ARM.
#' @param visit name of variable containing nominal visits e.g. AVISITCD.
#' @param loq_flag_var name of variable containing LOQ flag e.g. LOQFL.
#' @param unit name of variable containing biomarker unit e.g. AVALU.
#' @param xmin x-axis lower zoom limit.
#' @param xmax x-axis upper zoom limit.
#' @param ymin y-axis lower zoom limit.
#' @param ymax y-axis upper zoom limit.
#' @param color_manual vector of colors applied to treatment values.
#' @param shape_manual vector of symbols applied to LOQ values.
#' @param facet_ncol number of facets per row.
#' @param facet set layout to use treatment facetting.
#' @param facet_var variable to use for facetting beyond visit.
#' @param reg_line include regression line and annotations for slope and coefficient in visualization. Use with facet = TRUE.
#' @param hline y-axis value to position a horizontal line.
#' @param rotate_xlab 45 degree rotation of x-axis label values.
#' @param font_size font size control for title, x-axis label, y-axis label and legend.
#' @param dot_size plot dot size.
#' @param reg_text_size font size control for regression line annotations.
#' 
#' @import dplyr
#' @import ggplot2
#' @import mcr
#' @import tidyr
#' 
#' @author Nick Paszty (npaszty) paszty.nicholas@gene.com
#' @author Balazs Toth (tothb2)  toth.balazs@gene.com
#'
#' @details Regression uses deming model.
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' # Example using ADaM structure analysis dataset.
#' # ALB refers to biomarker data stored in expected laboratory structure.
#' # FOR TESTING: woud come from teal.goshawk.tm_g_correlationplot.R
#' 
#' xaxis_param <- c("CRP") 
#' yaxis_param <- c("ADIGG") 
#' 
#' plot1 <- g_correlationplot(label = 'Correlation Plot',
#'            data = ALB,
#'            param_var = 'PARAMCD', 
#'            xaxis_param = xaxis_param,
#'            xaxis_var = 'AVAL',
#'            yaxis_param = yaxis_param,
#'            yaxis_var = 'BASE',
#'            trt_group = 'ARM',
#'            visit = 'AVISITCD',
#'            loq_flag_var = 'LOQFL',
#'            unit = 'AVALU',
#'            xmin = 0,
#'            xmax = 200,
#'            ymin = 0,
#'            ymax = 2000,
#'            color_manual = color_manual,
#'            shape_manual = shape_manual,
#'            facet_ncol = 4,
#'            facet = FALSE,
#'            facet_var = "ARM",
#'            reg_line = FALSE,
#'            hline = NULL,
#'            rotate_xlab = FALSE,
#'            font_size = 14,
#'            dot_size = 2,
#'            reg_text_size = 3)
#' plot1 
#' 
#' }
#' 

g_correlationplot <- function(label = 'Correlation Plot',
                              data,
                              param_var = 'PARAMCD',
                              xaxis_param = "CRP",
                              xaxis_var = 'BASE',
                              yaxis_param = "IGG",
                              yaxis_var = 'AVAL',
                              trt_group = "ARM",
                              visit = "AVISITCD",
                              loq_flag_var = "LOQFL",
                              unit = "AVALU",
                              xmin = NA,
                              xmax = NA,
                              ymin = NA,
                              ymax = NA,
                              color_manual = NULL,
                              shape_manual = NULL,
                              facet_ncol = 2,
                              facet = FALSE,
                              facet_var = "ARM",
                              reg_line = FALSE,
                              hline = NULL,
                              rotate_xlab = FALSE,
                              font_size = 12,
                              dot_size = NULL,
                              reg_text_size = 3){

# create correlation plot over time pairwise per treatment arm 
plot_data <<- data %>%
  filter(eval(parse(text = param_var)) == xaxis_param | eval(parse(text = param_var)) == yaxis_param) 

param_lookup <- unique(plot_data[c("PARAMCD", "PARAM")])
unit_lookup <- unique(plot_data[c("PARAMCD", "AVALU")])
lookups <- inner_join(param_lookup, unit_lookup, by=c("PARAMCD"))

xparam_meta <- lookups %>% 
  filter(PARAMCD == xaxis_param)
xparam <- xparam_meta$PARAM
xunit <- xparam_meta$AVALU

yparam_meta <- lookups %>% 
  filter(PARAMCD == yaxis_param)
yparam <- yparam_meta$PARAM
yunit <- yparam_meta$AVALU

# setup the ggtitle label.  Combine the biomarker and the units (if available)
ggtitleLabel <- ifelse(is.null(unit), paste(xparam, "and", yparam, "@ Visits"), 
                       ifelse(plot_data[[unit]] == "", paste(xparam, "and", yparam, "@ Visits"), 
                              paste0(xparam, " (", xunit,") and ", yparam,  " (", yunit,") @ Visits"))
)

# setup the x-axis label.  Combine the biomarker and the units (if available)
xaxisLabel <- ifelse(is.null(unit), paste(xparam, xaxis_var, "Values"), 
                     ifelse(plot_data[[unit]] == "", paste(xparam, xaxis_var, "Values"), 
                            paste0(xparam," (", xunit, ") ", xaxis_var, " Values"))
)

# setup the y-axis label.  Combine the biomarker and the units (if available)
yaxisLabel <- ifelse(is.null(unit), paste(yparam, yaxis_var, "Values"), 
                     ifelse(plot_data[[unit]] == "", paste(yparam, yaxis_var, "Values"), 
                            paste0(yparam," (", yunit,") ", yaxis_var, " Values"))
)

plot_data_no_attribs <- plot_data %>% 
  mutate_all(~`attributes<-`(., NULL))

# given the 2 param and 2 analysis vars we need to transform the data
plot_data_t1 <<- plot_data_no_attribs %>% gather(ANLVARS, ANLVALS, xaxis_var, yaxis_var, LOQFL) %>% 
  mutate(ANL.PARAM = ifelse(ANLVARS == "LOQFL", paste0(ANLVARS, "_", PARAMCD), paste0(ANLVARS, ".", PARAMCD))) %>%
  select(USUBJID, ARM, ARMCD, AVISITN, AVISITCD, ANL.PARAM, ANLVALS) %>%
  spread(ANL.PARAM, ANLVALS)

# assign the values of the analysis variable in the transformed data to shorter variable names to use in code below
xvar <- paste0(xaxis_var, ".", xaxis_param)
yvar <- paste0(yaxis_var, ".", yaxis_param)
xloqfl <- paste0("LOQFL_", xaxis_param)
yloqfl <- paste0("LOQFL_", yaxis_param)

# the transformed analysis value variables are character and need to be converetd to numeric for ggplot
# remove records where either of the analysis variables are NA since they will not appear on the plot and 
# will ensure that LOQFL = NA level is removed
plot_data_t2 <<- plot_data_t1 %>%
  subset(!is.na(.[[xvar]]) & !is.na(.[[yvar]])) %>% 
  mutate_at(vars(contains(".")), as.numeric) %>% 
  mutate(LOQFL_COMB = ifelse(.[[xloqfl]] == "Y" | .[[yloqfl]] == "Y", "Y", "N"))

  # create plot foundation
  plot1 <- ggplot2::ggplot(data = plot_data_t2,
                   aes_string(x = xvar,
                              y = yvar,
                              color = trt_group)) +
    geom_point(aes_string(shape = "LOQFL_COMB"), size = dot_size, na.rm = TRUE) +
    coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    facet_wrap(as.formula(paste0('~', visit)), ncol = facet_ncol) +
    theme_bw() +
    ggtitle(ggtitleLabel) +
    theme(plot.title = element_text(size = font_size, hjust = 0.5)) +
    xlab(xaxisLabel) +
    ylab(yaxisLabel)
    
    
# add grid faceting to foundation 
    if (facet){
      plot1 <- plot1 +
        facet_grid(as.formula(paste0(facet_var ,' ~ ', visit)))
    }

# add regression line
    if (reg_line){
      slope <- function(x, y) {
        ratio <- sd(x)/sd(y)
        if (!is.na(ratio) & ratio > 0){
          reg <- mcr:::mc.deming(y, x, ratio)
          # return the evaluation of the ratio condition as third value in numeric vector for conttroling downstream processing
          return(c(round(reg$b0,2), round(reg$b1,2), !is.na(ratio) & ratio > 0))
        }
        # if ratio condition is not met then assign NA to returned vector so that NULL condition does not throw error below
        return(as.numeric(c(NA, NA, NA)))
      }
      
      sub_data <- subset(plot_data_t2, !is.na(eval(parse(text = yvar))) &
                           !is.na(eval(parse(text = xvar)))) %>%
        group_by_(.dots = c(trt_group, visit)) %>%
        mutate(intercept =  slope(eval(parse(text = yvar)),
                                  eval(parse(text = xvar)))[1]) %>%
        mutate(slope = slope(eval(parse(text = yvar)),
                             eval(parse(text = xvar)))[2]) %>%
        mutate(corr = ifelse(((slope(eval(parse(text = yvar)),
                                     eval(parse(text = xvar))))[3]),
                             cor(eval(parse(text = yvar)),
                                 eval(parse(text = xvar)),
                                 method = "spearman",
                                 use = 'complete.obs'),
                             NA))

        plot1 <- plot1 +
        geom_abline(data = filter(sub_data, row_number() == 1), # only need to return 1 row within group_by to create annotations
                    aes_string(intercept = 'intercept',
                               slope = 'slope',
                               color = trt_group)) +
        geom_text(data = filter(sub_data, row_number() == 1), 
                  aes( x = -Inf,
                       y = Inf,
                       hjust = 0,
                       vjust = 1,
                       label = ifelse(!is.na(intercept) & !is.na(slope) & !is.na(corr),
                       sprintf("y=%.2f+%.2fX\ncor=%.2f", intercept, slope, corr),
                       paste0("Insufficient Data For Regression")),
                       color = eval(parse(text = trt_group))),
                       size = reg_text_size) +
                       labs(caption = paste("Deming Regression Model, Spearman Correlation Method"))
      }
 
  # Add abline
    if (yaxis_var %in% c('AVAL', 'AVALL2', 'BASE2', 'BASE2L2', 'BASE', 'BASEL2')) {plot1 <- plot1 + geom_abline(intercept = 0, slope = 1)}
    
    if (yaxis_var %in% c('CHG2', 'CHG')) {plot1 <- plot1 + geom_abline(intercept = 0, slope = 0)}
    
    if (yaxis_var %in% c('PCHG2', 'PCHG')) {plot1 <- plot1 + geom_abline(intercept = 100, slope = 0)}
    
  # Format font size
  if (!is.null(font_size)){
    plot1 <- plot1 +
      theme(axis.title.x = element_text(size = font_size),
            axis.text.x = element_text(size = font_size),
            axis.title.y = element_text(size = font_size),
            axis.text.y = element_text(size = font_size),
            legend.title = element_text(size = font_size),
            legend.text = element_text(size = font_size),
            strip.text.x = element_text(size = font_size),
            strip.text.y = element_text(size = font_size))
  }

  # Format treatment color
  if (!is.null(color_manual)){
    plot1 <- plot1 +
      scale_color_manual(values = color_manual, name = 'Dose')
  }
  
  # Format LOQ flag symbol shape
  if (!is.null(shape_manual)){
    plot1 <- plot1 +
      scale_shape_manual(values = shape_manual, name = 'LOQ')
  }
  
  # Format dot size
  if (!is.null(dot_size)){
    plot1 <- plot1 +
      geom_point(aes_string(shape = "LOQFL_COMB"))
      #geom_point(aes_string(shape = sprintf("LOQFL_%s == 'Y' | LOQFL_%s == 'Y'", xaxis_param, yaxis_param)), size = dot_size, na.rm = TRUE)
  }
  
  # Format x-label
  if (rotate_xlab){
    plot1 <- plot1 +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # Add horizontal line
  if (!is.null(hline)){
    plot1 <- plot1 +
      geom_hline(aes(yintercept = hline), color="red", linetype="dashed", size=0.5)
  }
  
  plot1
  
}
