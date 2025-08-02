#' Plot Liability and Net Reported Acres Faceted by a Grouping Variable
#'
#' Produces a two-panel bar chart:
#' \enumerate{
#'   \item \strong{Panel A} - Liability (in U.S.\ dollars) by year and group.
#'   \item \strong{Panel B} - Net reported acres (in acres) by year and group.
#' }
#' The panels share the same x-axis (commodity years) and a single legend
#' that is displayed beneath the figure.
#'
#' @param data A `data.frame` containing at least these columns:
#'   \itemize{
#'     \item `commodity_year` - numeric or factor; the x-axis.
#'     \item `value` - numeric; the bar heights.
#'     \item `variable` - factor with the levels
#'       `"(A) Liability in U.S. dollars"` and
#'       `"(B) Net reported acres"`.
#'     \item The column referenced by `grouping_variable`.
#'   }
#' @param grouping_variable A character scalar giving the name of the column in
#'   `data` that defines the groups (e.g.\ `"commodity"`, `"state"`).
#' @param grouping_name Optional character scalar used as the legend title.
#'   Defaults to the value of `grouping_variable` if `NULL`.
#' @param time_scale_theme Optional `ggplot2` scale or theme element that
#'   controls the x-axis breaks/labels.  If `NULL`, the function applies
#'   `scale_x_continuous(breaks = unique(data$commodity_year), labels = unique(data$commodity_year))`.
#' @param general_theme Optional `ggplot2` theme applied to both panels.
#'   If `NULL`, `ers_theme()` is used with additional tweaks for font sizes,
#'   legend layout, and facet strips.
#' @param label_liability Y-axis label for Panel A.  Default
#'   `"Billion U.S. dollars\\n"`.
#' @param label_net_reported_acres Y-axis label for Panel B.  Default
#'   `"Million acres\\n"`.
#' @param palette A character vector of hex colors used to fill the bars.
#'   The default is an 11-color palette aligned with ERS/NDSU branding.
#'
#' @return A named list with five objects:
#' \describe{
#'   \item{`fig`}{A `gtable` containing the assembled two-panel figure, shared
#'     x-axis label, and bottom legend.}
#'   \item{`figA`}{The `ggplot` object for Panel A.}
#'   \item{`figB`}{The `ggplot` object for Panel B.}
#'   \item{`ldgnd`}{The extracted legend as a `gtable`.}
#'   \item{`xlabT`}{The shared x-axis grob as a `gtable`.}
#' }
#'
#' @details
#' Internally the function:
#' \enumerate{
#'   \item Duplicates the column named by `grouping_variable` into
#'     `data$grouping_variable` for convenient aesthetic mapping.
#'   \item Builds two separate `ggplot` bar charts (one per `variable` level),
#'     applies user themes and palettes, and hides their legends.
#'   \item Extracts a shared legend and x-axis grob with `gtable_filter()`.
#'   \item Assembles everything with `gridExtra::grid.arrange()`.
#' }
#'
#' @import ggplot2
#' @import gridExtra
#' @import gtable
#' @importFrom grid unit
#'
#' @examples
#' \dontrun{
#' # Group by commodity with a custom x-axis theme
#' plot_liability_and_acres(
#'   data = data_comm,
#'   grouping_variable = "commodity",
#'   grouping_name = "Commodity",
#'   time_scale_theme = ggplot2::scale_x_continuous(
#'     breaks = seq(2008, 2024, 2),
#'     labels = seq(2008, 2024, 2)
#'   ),
#'   general_theme = ggplot2::theme_minimal()
#' )
#' }
#'
#' @export
plot_liability_and_acres <- function(
    data,
    grouping_variable,
    grouping_name = NULL,
    time_scale_theme = NULL,
    general_theme = NULL,
    label_liability = "Billion U.S. dollars\n",
    label_net_reported_acres = "Million acres\n",
    palette = c("#003524", #  (Dark Green)
                "#00583D", #  (NDSU Green)
                "#A0BD78", #  (Sage)
                "#BED73B", #  (Lime Green)
                "#BE5E27", #  (Rust)
                "#FFC425", #  (NDSU Yellow)
                "#FEF389", #  (Lemon Yellow)
                "#D7E5C8", #  (Pale Sage)
                "#9DD9F7", #  (Morning Sky)
                "#51ABA0", #  (Teal)
                "#0F374B") #  (Night)
    ){

  if(is.null(time_scale_theme)){
    time_scale_theme = scale_x_continuous(breaks = unique(data$commodity_year),labels = unique(data$commodity_year))
  }
  
  if(is.null(general_theme)){
    general_theme <- ers_theme() +
      theme(plot.title= element_text(size=10.5),
            axis.title= element_text(size=10,color="black"),
            axis.text = element_text(size=10,color="black"),
            axis.title.y= element_text(size=10,color="black"),
            legend.title=element_blank(),
            legend.text=element_text(size=9),
            plot.caption = element_text(size=10),
            strip.text = element_text(size = 10),
            strip.background = element_rect(fill = "white", colour = "black", size = 1))
  }
  
  data$grouping_variable <- data[,grouping_variable]
  NN <- length(unique(as.character(data$grouping_variable)))
  figA <- ggplot()+
    geom_bar(data=data[data$variable %in% "(A) Liability in U.S. dollars",],
             aes(x = commodity_year, y= value,
                 group=grouping_variable,color=grouping_variable,fill=grouping_variable),
             stat = "identity",color="black") +
    labs(subtitle="(A) Liability in U.S. dollars",y = label_liability) +
    general_theme + time_scale_theme +
    theme(plot.caption = element_blank(),
          plot.subtitle = element_text(size = 12),
          axis.title.x= element_blank(),
          axis.text.x = element_text(size=10,color="black",angle = 90,vjust = 0.5),
          legend.position ="none",
          strip.background = element_blank())

  figB <- ggplot()+
    geom_bar(data=data[data$variable %in% "(B) Net reported acres",],
             aes(x = commodity_year, y= value,
                 group=grouping_variable,color=grouping_variable,fill=grouping_variable),
             stat = "identity",color="black") +
    labs(subtitle="(B) Net reported acres",y = label_net_reported_acres) +
    general_theme + time_scale_theme +
    theme(plot.caption = element_blank(),
          plot.subtitle = element_text(size = 12),
          axis.title.x= element_blank(),
          axis.text.x = element_text(size=10,color="black",angle = 90,vjust = 0.5),
          legend.position ="none",
          strip.background = element_blank())
  
  if(!is.null(palette)){
    figA <- figA + scale_fill_manual(values = palette,na.value = "white", name = grouping_name)
    figB <- figB + scale_fill_manual(values = palette,na.value = "white", name = grouping_name)
  }

  xlabT <- gtable_filter(
    ggplot_gtable(
      ggplot_build(
        figA + labs(x="Commodity year") + theme(axis.title.x= element_text(size=10,color="black"))
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
  
  fig <- grid.arrange(figA,figB,widths=c(1,1),nrow = 1)
  fig <- grid.arrange(fig,xlabT,heights=c(1,0.05),ncol = 1)
  fig <- grid.arrange(fig,ldgnd,heights=c(1,0.10),ncol = 1)
  return(list(fig=fig,figA=figA,figB=figB,ldgnd=ldgnd,xlabT=xlabT))
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
#' base_plot <- ggplot(data, aes(x = commodity_year, y = value)) +
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
