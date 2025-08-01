#' Plot Liability and Net Reported Acres Faceted by Disaggregation Variable
#'
#' This function creates a two-panel bar chart that visualizes (A) liability in U.S. dollars 
#' and (B) net reported acres over time, grouped by a user-defined disaggregation variable. 
#' A shared x-axis and a unified bottom legend are included.
#'
#' @param data A `data.frame` containing the columns: `crop_year`, `value`, `variable`, 
#' and a column specified by `disag`. The `variable` column should include 
#' "(A) Liability in U.S. dollars" and "(B) Net reported acres" as possible values.
#' @param disag A `character` string specifying the name of the column in `data` 
#' to be used for grouping in the plots (e.g., `"commodity"`, `"state"`).
#' @param time_scale_theme An optional ggplot2 theme object controlling time axis styling. 
#' If `NULL`, the function expects a globally defined `myTime` theme.
#' @param general_theme An optional ggplot2 theme object for general plot styling. 
#' If `NULL`, the function expects a globally defined `myTheme`.
#'
#' @return A `gtable` object combining:
#' \itemize{
#'   \item Panel A: Liability by year and group
#'   \item Panel B: Net reported acres by year and group
#'   \item Shared x-axis label
#'   \item Combined legend at the bottom
#' }
#'
#' @details
#' The function uses `ggplot2`, `gridExtra`, and `gtable` for construction and layout. 
#' You can pass custom themes to override the defaults.
#'
#' @import ggplot2
#' @import gridExtra
#' @import gtable
#' @importFrom grid unit
#'
#' @examples
#' \dontrun{
#' plot_liability_and_acres(
#'   data = data_comm, 
#'   disag = "commodity",
#'   time_scale_theme = theme(axis.text.x = element_text(angle = 45)),
#'   general_theme = theme_minimal()
#' )
#' }
#'
#' @export
plot_liability_and_acres <- function(
    data,
    disag,
    time_scale_theme = NULL,
    general_theme = NULL){
  
  #  data <- data_comm;footnote <- footnote;disag <- "commodity"
  data$disag <- data[,disag]
  NN <- length(unique(as.character(data$disag)))
  figA <- ggplot()+
    geom_bar(data=data[data$variable %in% "(A) Liability in U.S. dollars",],
             aes(x = crop_year, y= value,
                 group=disag,color=disag,fill=disag),
             stat = "identity",color="black") +
    labs(subtitle="(A) Liability in U.S. dollars",y ="Billion U.S. dollars\n") +
    myTheme + myTime +
    theme(plot.caption = element_blank(),
          plot.subtitle = element_text(size = 12),
          axis.title.x= element_blank(),
          axis.text.x = element_text(size=10,color="black",angle = 90,vjust = 0.5),
          legend.position ="none",
          strip.background = element_blank())
  
  
  figB <- ggplot()+
    geom_bar(data=data[data$variable %in% "(B) Net reported acres",],
             aes(x = crop_year, y= value,
                 group=disag,color=disag,fill=disag),
             stat = "identity",color="black") +
    labs(subtitle="(B) Net reported acres",y ="Million acres\n") +
    myTheme + myTime +
    theme(plot.caption = element_blank(),
          plot.subtitle = element_text(size = 12),
          axis.title.x= element_blank(),
          axis.text.x = element_text(size=10,color="black",angle = 90,vjust = 0.5),
          legend.position ="none",
          strip.background = element_blank())
  
  
  xlabT <- gtable_filter(
    ggplot_gtable(
      ggplot_build(
        figA + labs(x="Year") + theme(axis.title.x= element_text(size=10,color="black"))
      )), "xlab-b")
  
  
  ldgnd <- gtable_filter(
    ggplot_gtable(
      ggplot_build(
        figA +
          guides(fill = guide_legend(nrow = ifelse(NN>=8,2,1),override.aes = list(size=3))) +
          theme(legend.position = "bottom",
                legend.key.size = unit(0.5,"cm"),
                legend.text=element_text(size=7.5))
        
      )), "guide-box-botto")
  
  
  # caption <- gtable_filter(
  #   ggplot_gtable(
  #     ggplot_build(
  #       figA + labs(caption = footnote) +
  #         theme(plot.caption = element_text(size=8,hjust = 0.01))
  #     )), "caption")
  
  fig <- grid.arrange(figA,figB,widths=c(1,1),nrow = 1)
  fig <- grid.arrange(fig,xlabT,heights=c(1,0.05),ncol = 1)
  fig <- grid.arrange(fig,ldgnd,heights=c(1,0.10),ncol = 1)
  #fig <- grid.arrange(fig,caption,heights=c(0.2,0.05),ncol = 1)
  fig
  return(fig)
}










