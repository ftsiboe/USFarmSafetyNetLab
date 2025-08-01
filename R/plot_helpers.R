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

#' Add U.S. Farm Policy Vertical Lines and Labels to a ggplot
#'
#' This function overlays vertical lines and text labels on a ggplot object to mark major 
#' U.S. agricultural policy events (e.g., Farm Bills or Acts). The vertical lines are drawn 
#' at specific years, and labels are positioned based on values provided in the `pty` vector.
#'
#' @param pty A numeric vector of y-axis positions for label placement. 
#' The length of the vector determines which policy annotations are added:
#' \itemize{
#'   \item 1st: 1980 Act
#'   \item 2nd: 1994 Act
#'   \item 3rd: 1996 Farm Bill
#'   \item 4th: 2000 Agricultural Risk Protection Act
#'   \item 5th: 2008 Farm Bill
#'   \item 6th: 2014 Farm Bill
#'   \item 7th: 2018 Farm Bill
#' }
#' @param plot A `ggplot` object to which policy lines and labels will be added.
#' @param size A numeric value indicating the text size of the policy labels.
#'
#' @return A `ggplot` object with added vertical dashed lines and corresponding 
#' text labels for each policy year provided.
#'
#' @details
#' The lines and labels are added in brown color with dashed lines (`lty=5`), and labels 
#' are rotated vertically. Labels are only drawn if the corresponding index exists in `pty`.
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' base_plot <- ggplot(data, aes(x = crop_year, y = value)) +
#'   geom_line()
#' policy_positions <- c(10, 15, 20, 25, 30, 35, 40)
#' annotated_plot <- policytime(policy_positions, base_plot, size = 3)
#' print(annotated_plot)
#' }
#'
#' @export
policytime <- function(pty,plot,size){
  plotx <- plot + geom_vline(aes(xintercept=1980), lwd=0.5, lty=5,color = "brown") + 
    geom_text(aes(x=1980 + .5, label="1980 Act",y=pty[1]),
              colour="brown", angle=90, size=size,check_overlap = TRUE, fontface = "bold")
  
  if(length(pty) >=2){
    plotx <- plotx + geom_vline(aes(xintercept=1994), lwd=0.5, lty=5,color = "brown") + 
      geom_text(aes(x=1994 + .5, label="1994 Act",y=pty[2]),
                colour="brown", angle=90, size=size,check_overlap = TRUE,fontface = "bold")
  }
  if(length(pty) >=3){
    plotx <- plotx + geom_vline(aes(xintercept=1996), lwd=0.5, lty=5,color = "brown") + 
      geom_text(aes(x=1996 + .5, label="1996 Farm Bill",y=pty[3]),
                colour="brown", angle=90, size=size,check_overlap = TRUE,fontface = "bold")
  }
  if(length(pty) >=4){
    plotx <- plotx +  geom_vline(aes(xintercept=2000), lwd=0.5, lty=5,color = "brown") + 
      geom_text(aes(x=2000 + .5, label="2000 Agricultural Risk Protection Act",y=pty[4]),
                colour="brown", angle=90, size=size,check_overlap = TRUE,fontface = "bold")
  }
  if(length(pty) >=5){
    plotx <- plotx + geom_vline(aes(xintercept=2008), lwd=0.5, lty=5,color = "brown") + 
      geom_text(aes(x=2008 + .5, label="2008 Farm Bill",y=pty[5]),
                colour="brown", angle=90, size=size,check_overlap = TRUE,fontface = "bold")
  }
  if(length(pty) >=6){
    plotx <- plotx + geom_vline(aes(xintercept=2014), lwd=0.5, lty=5,color = "brown") + 
      geom_text(aes(x=2014 + .5, label="2014 Farm Bill",y=pty[6]),
                colour="brown", angle=90, size=size,check_overlap = TRUE,fontface = "bold")
  }
  if(length(pty) >=7){
    plotx <- plotx + geom_vline(aes(xintercept=2018), lwd=0.5, lty=5,color = "brown") + 
      geom_text(aes(x=2018 + .5, label="2018 Farm Bill",y=pty[7]),
                colour="brown", angle=90, size=size,check_overlap = TRUE,fontface = "bold")
  }
  return(plotx)
}





