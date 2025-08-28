#' Plot an Annotated US States Choropleth Map
#'
#' @description
#' Creates a choropleth map of US states colored by a categorical value, with state labels
#' and special treatment for small states (VT, NH, and other small ones) using repelled text.
#' Optionally embeds a table grob in the bottom-left corner of the map.
#'
#' @param data A data frame containing at least:
#'   \itemize{
#'     \item `state_code`: numeric FIPS code matching `urbnmapr::get_urbn_map("states")`
#'     \item `value_cat`: categorical variable used for fill colors
#'     \item `value`: numeric variable used to display numeric labels
#'   }
#' @param legend_title Character; title for the fill legend. If `NULL`, no title is shown.
#' @param palette Character vector of colours (hex codes) to use for the categories.
#'   Defaults to a 10-colour NDSU-inspired palette.
#' @param table_grob A `grob` object (e.g. from `gridExtra::tableGrob`) to annotate on the map;
#'   if `NULL`, no table is added.
#' @param label_size label size for sgeom_sf_text
#' @return A `ggplot` object showing the US states choropleth with annotated labels.
#'
#' @details
#' - Uses `urbnmapr::get_urbn_map(map = "states", sf = TRUE)` to fetch a US states basemap.
#' - Joins the input data on `state_code` and filters out states with missing `value_cat`.
#' - Computes equal-area centroids (EPSG:5070) to place labels.
#' - Flags states with area < 50,000 km^2 as "small" and applies repelled text labels.
#' - Standard states get text labels placed via `geom_sf_text()`.
#' - Small states in the east and west are nudged horizontally; VT & NH get custom nudges.
#' - Adds an optional table in the bottom-left via `annotation_custom()`.
#'
#' @import ggplot2
#' @import sf
#' @import urbnmapr
#' @import ggrepel
#' @importFrom grid unit
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' # Assume `my_data` has columns: state_code, value_cat, value
#' gg <- plot_us_states_choropleth(
#'   data = my_data,
#'   legend_title = "Category",
#'   palette = c("#003524", "#00583D", "#A0BD78", "#BED73B",
#'               "#BE5E27", "#FFC425", "#FEF389", "#D7E5C8",
#'               "#9DD9F7", "#51ABA0", "#0F374B")
#' )
#' print(gg)
#' }
plot_us_states_choropleth <- function(
    data,
    legend_title = NULL,
    palette = c(
      "#003524", # Dark Green
      "#00583D", # NDSU Green
      "#A0BD78", # Sage
      "#BED73B", # Lime Green
      "#BE5E27", # Rust
      "#FFC425", # NDSU Yellow
      "#FEF389", # Lemon Yellow
      "#D7E5C8", # Pale Sage
      "#9DD9F7", # Morning Sky
      "#51ABA0", # Teal
      "#0F374B"  # Night
    ),
    table_grob = NULL,
    label_size = 2.5
) {
  # If no legend title is provided, set blank to suppress default
  if (is.null(legend_title)) {
    legend_title <- ""
  }
  
  # Load base map of US states with FIPS codes
  us_sf <- urbnmapr::get_urbn_map(map = "states", sf = TRUE)
  us_sf$state_code <- as.numeric(as.character(us_sf$state_fips))
  
  # Join user data to base map and drop missing categories
  sf_object <- us_sf %>%
    dplyr::left_join(data, by = "state_code") %>%
    dplyr::filter(!is.na(value_cat))
  
  # Create labels: two-line state abbreviation and rounded value
  sf_object$label <- paste0(
    sf_object$state_abbv, "\n",
    sprintf("%.1f", round(sf_object$value, 1))
  )
  
  # Transform to equal-area projection for area and centroid calculations
  sf_eqarea <- st_transform(sf_object, 5070)
  
  # Compute area in km^2 and flag "small" states (< 50,000 km^2)
  sf_object <- sf_object %>%
    dplyr::mutate(
      area_km2 = as.numeric(st_area(sf_eqarea) / 1e6),
      is_small = area_km2 < 50000
    )
  
  # Extract centroids for small states
  small_states <- sf_object |>
    dplyr::filter(is_small) |>
    dplyr::mutate(
      centroid = st_centroid(geometry),
      cx = st_coordinates(centroid)[,1],
      cy = st_coordinates(centroid)[,2]
    )
  
  # Big states plotted normally
  big_states <- dplyr::filter(sf_object, !is_small)
  
  # Compute map bounding box and offsets for label nudging
  bb    <- st_bbox(us_sf)
  mid_x <- (bb$xmin + bb$xmax) / 2
  x_off <- (bb$xmax - bb$xmin) * 0.05  # 5% width
  y_off <- (bb$ymax - bb$ymin) * 0.10  # 10% height
  
  # Separate small states into east, west, and VT/NH groups
  vt_nh      <- dplyr::filter(small_states, state_abbv %in% c("VT", "NH"))
  east_small <- dplyr::filter(small_states, cx > mid_x, !state_abbv %in% c("VT", "NH"))
  west_small <- dplyr::filter(small_states, cx <= mid_x)
  
  # Build the ggplot object
  fig <- ggplot() +
    # Fill states by category
    geom_sf(
      data = sf_object,
      aes(fill = value_cat),
      colour = NA, size = 0.2
    ) +
    # Draw state borders
    geom_sf(
      data = us_sf[us_sf$state_abbv %in% unique(sf_object$state_abbv),],
      colour = "gray", fill = NA, size = 0.01
    ) +
    # Labels for big states
    geom_sf_text(
      data = big_states,
      aes(label = label),
      size = label_size, fontface = "bold"
    ) +
    # Repelled labels for small western states
    geom_text_repel(
      data = west_small,
      aes(x = cx, y = cy, label = label),
      nudge_x = -x_off, hjust = 1, direction = "y",
      size = label_size, segment.size = 0.3, min.segment.length = 0,
      fontface = "bold"
    ) +
    # Repelled labels for small eastern states
    geom_text_repel(
      data = east_small,
      aes(x = cx, y = cy, label = label),
      nudge_x = x_off, hjust = 0, direction = "y",
      size = label_size, segment.size = 0.3, min.segment.length = 0,
      fontface = "bold"
    ) +
    # Special placement for VT and NH
    geom_text_repel(
      data = vt_nh,
      aes(x = cx, y = cy, label = label),
      nudge_x = -1.5 * x_off, nudge_y = y_off,
      hjust = 0, direction = "y", size = label_size,
      segment.size = 0.3, min.segment.length = 0,
      fontface = "bold"
    ) +
    # Apply custom palette and legend title
    scale_fill_manual(
      values = palette,
      na.value = "white",
      name = legend_title
    ) +
    guides(fill = guide_legend(ncol = 1)) +
    theme_bw() +
    theme(
      panel.grid.major   = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.ticks         = element_blank(),
      axis.text          = element_blank(),
      axis.title.x       = element_blank(),
      axis.title.y       = element_blank(),
      legend.position    = c(0.08, 0.80),
      legend.background  = element_blank(),
      legend.key.size    = unit(0.3, "cm"),
      legend.text        = element_text(size = 9),
      legend.title       = element_text(size = 9),
      plot.title         = element_text(size = 8),
      strip.background   = element_blank()
    ) +
    coord_sf()
  
  # Optionally add a table grob in the bottom-left
  if (!is.null(table_grob)) {
    fig <- fig +
      annotation_custom(
        grob = table_grob,
        xmin = -900000,
        ymin = -5300000
      )
  }
  
  return(fig)
}


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
  
  fig <- gridExtra::grid.arrange(figA,figB,widths=c(1,1),nrow = 1)
  fig <- gridExtra::grid.arrange(fig,xlabT,heights=c(1,0.05),ncol = 1)
  fig <- gridExtra::grid.arrange(fig,ldgnd,heights=c(1,0.10),ncol = 1)
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
