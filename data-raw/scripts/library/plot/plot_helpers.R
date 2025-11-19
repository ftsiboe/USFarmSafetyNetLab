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
#' @param na.value The aesthetic value to use for missing (NA) values
#' @param keep_all_states keep all states 
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
plot_us_states_choropleth <- function(
    data,
    legend_title = NULL,
    palette = c(
      "#BE5E27", # Rust
      "#FFC425", # NDSU Yellow
      "#FEF389", # Lemon Yellow
      "#BED73B", # Lime Green
      "#A0BD78", # Sage
      "#00583D", # NDSU Green
      "#003524", # Dark Green
      "#D7E5C8", # Pale Sage
      "#9DD9F7", # Morning Sky
      "#51ABA0", # Teal
      "#0F374B"  # Night
    ),
    table_grob = NULL,
    label_size = 2.5,
    na.value = "white",
    keep_all_states = FALSE
) {
  # If no legend title is provided, set blank to suppress default
  if (is.null(legend_title)) {
    legend_title <- ""
  }
  
  # Load base map of US states with FIPS codes
  us_sf <- urbnmapr::get_urbn_map(map = "states", sf = TRUE)
  us_sf$state_code <- as.numeric(as.character(us_sf$state_fips))
  
  # Join user data to base map and drop missing categories
  sf_object <- us_sf |>
    dplyr::left_join(data, by = "state_code") |>
    dplyr::filter(!is.na(value_cat))
  
  # Create labels: two-line state abbreviation and rounded value
  sf_object$label <- paste0(
    sf_object$state_abbv, "\n",
    sprintf("%.2f", round(sf_object$value, 1))
  )
  
  # Transform to equal-area projection for area and centroid calculations
  sf_eqarea <- st_transform(sf_object, 5070)
  
  # Compute area in km^2 and flag "small" states (< 50,000 km^2)
  sf_object <- sf_object |>
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
  if(keep_all_states){
    fig <- ggplot() + geom_sf(data = us_sf,colour = "black", fill = na.value, size = 0.1)
  }else{
    fig <- ggplot()
  }
  
  fig <- fig +
    # Fill states by category
    geom_sf(data = sf_object,aes(fill = value_cat),colour = NA, size = 0.2) + 
    geom_sf(
    data = us_sf[us_sf$state_abbv %in% unique(sf_object$state_abbv),],
    colour = "black", fill = NA, size = 0.1) +
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
      na.value = na.value,
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


#' Plot FCIP Main Outcomes (Faceted Bar Charts)
#'
#' Creates a faceted bar chart of FCIP outcomes over time by a chosen grouping,
#' with a shared x-axis (commodity year) and a legend. Facets are determined by
#' an "outcome" column in `data` (e.g., liability vs. acres), allowing one or
#' more panels depending on the unique values present.
#'
#' @description
#' The function expects `data` to contain:
#' - a numeric/factor year column (specified via `colume_year`);
#' - a numeric `value` column (bar heights);
#' - a categorical "outcome" column (specified via `colume_outcome`) used for
#'   faceting; and
#' - an optional grouping column (specified via `colume_grouping`) used to color/
#'   fill bars and determine legend entries.
#'
#' For legend ordering, the function ranks groups using the subset of rows from
#' the most recent year *within the facet whose label equals*
#' `"(B) Liability amount in U.S. dollars"`. If your outcome labels differ, you
#' may want to harmonize them or adjust the code that builds `labs`.
#'
#' @param data A `data.frame` (or `data.table`) with at least the columns:
#'   `value`, the column named by `colume_year`, the column named by
#'   `colume_outcome`, and (optionally) the column named by `colume_grouping`.
#' @param colume_outcome Character scalar. Name of the column in `data` that
#'   defines facet panels (e.g., outcome labels such as
#'   `"(A) Net reported acres"` and `"(B) Liability amount in U.S. dollars"`).
#' @param colume_year Character scalar. Name of the year column in `data`
#'   (x-axis, typically the FCIP commodity year).
#' @param colume_grouping Character scalar or `NULL`. Name of the grouping
#'   column in `data` used for fill/color and legend entries. If `NULL`, all
#'   bars are treated as a single group.
#' @param time_scale_theme Optional `ggplot2` scale/theme element controlling the
#'   x-axis (e.g., `scale_x_continuous(...)`). If `NULL`, a default scale is
#'   applied using the unique values of the supplied year column.
#' @param general_theme Optional `ggplot2` theme applied to the figure. If
#'   `NULL`, `ers_theme()` is used and further tweaked (text sizes, legend
#'   layout, facet strips). Requires that `ers_theme()` is available in scope.
#' @param palette Optional character vector of HEX colors used by
#'   `scale_fill_manual()`. If provided, it is applied to the grouping legend.
#'
#' @return A `ggplot` object.
#'
#' @details
#' Internally, the function:
#' 1. Copies the columns named by `colume_outcome`, `colume_grouping`, and
#'    `colume_year` into standard placeholders used in aesthetics.
#' 2. Builds a ranking of groups from the latest year within the facet whose
#'    label equals `"(B) Liability amount in U.S. dollars"`, then maps that
#'    ranking to legend order.
#' 3. Draws a bar chart of `value ~ year`, colored/filled by group, and
#'    `facet_wrap()`s by outcome (free y-scales).
#' 4. Applies the supplied or default x-axis scale and theme, plus optional
#'    manual fill palette.
#'
#' @note
#' The parameter names use **`colume_*`** (typo preserved for backward
#' compatibility). Consider aliasing/renaming in a future version.
#'
#' @import ggplot2
#' @importFrom grid unit
#' @export
plot_fcip_main_outcomes <- function(
    data,
    colume_outcome,
    colume_year,
    colume_grouping = NULL,
    time_scale_theme = NULL,
    general_theme = NULL,
    palette = c("#003524", #  (Dark Green)
                "#00583D", #  (NDSU Green)
                "#A0BD78", #  (Sage)
                "#BED73B", #  (Lime Green)
                "#D7E5C8", #  (Pale Sage)
                "#FEF389", #  (Lemon Yellow)
                "#FFC425", #  (NDSU Yellow)
                "#BE5E27", #  (Rust)
                "#9DD9F7", #  (Morning Sky)
                "#51ABA0", #  (Teal)
                "#0F374B") #  (Night)
){
  
  data <- as.data.frame(data)
  data$colume_outcome  <- data[,colume_outcome]
  data$colume_grouping <- data[,colume_grouping]
  data$colume_year     <- data[,colume_year]

  if(is.null(time_scale_theme)){
    time_scale_theme = scale_x_continuous(breaks = unique(data$colume_year),labels = unique(data$colume_year))
  }
  
  if(is.null(general_theme)){
    general_theme <- ers_theme() +
      theme(
        plot.title       = element_text(size=10.5),
        plot.caption     = element_blank(),
        plot.subtitle    = element_text(size = 12),
        axis.title.y     = element_text(size=10, color="black"),
        axis.title.x     = element_blank(),
        axis.text.y      = element_text(size=9),
        axis.text.x      = element_text(size=9, color="black", angle = 90, vjust = 0.5),
        legend.position  = c(0.80,0.10),
        legend.key.size  = unit(0.5,"cm"),
        legend.title     = element_blank(),
        legend.text      = element_text(size=7.5),
        strip.text       = element_text(size = 10),
        strip.background = element_blank())
  }
  
  labs <- data[grepl("liability",tolower(data$colume_outcome)) & 
                 data$colume_year %in% max(data$colume_year,na.rm=T),]
  
  labs <- labs[order(-labs$value),]
  
  data$ranking <- as.numeric(as.character(factor(data$colume_grouping,levels = labs$colume_grouping, labels = 1:nrow(labs))))
  
  data$ranking <- factor(data$ranking,levels = 1:nrow(labs), labels = labs$colume_grouping)
  
  NN <- length(unique(as.character(data$colume_grouping)))
  
  fig <- ggplot()+
    geom_bar(data=data,aes(x = colume_year, y= value,
                 group=ranking,color=ranking,fill=ranking,color=ranking),
             stat = "identity") +
    labs(x="\nCommodity year", y = "") +
    facet_wrap(~colume_outcome, ncol = 2, scales ="free") +
    guides(fill = guide_legend(nrow = NN,override.aes = list(size=3))) +
    general_theme #+ time_scale_theme 
  
  if(!is.null(palette)){
    fig <- fig + scale_fill_manual(values = palette,na.value = "white", name = colume_grouping)
    fig <- fig + scale_color_manual(values = palette,na.value = "white", name = colume_grouping)
  }
  
  fig

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
                "#D7E5C8", #  (Pale Sage)
                "#FEF389", #  (Lemon Yellow)
                "#FFC425", #  (NDSU Yellow)
                "#BE5E27", #  (Rust)
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
