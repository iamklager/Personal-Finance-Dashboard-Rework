#### HCSankey
# A sankey chart (because I saw one on Reddit).

HCSankey <- function(df, total_name, dark_mode_on) {
  if (nrow(df) == 0 | is.null(df)) {
    return(
      shiny::validate(
        need((nrow(df) != 0), "No data available based on your selection")
      )
    )
  }
  l_Sankey <- list()
  for (category in unique(df$Category)) {
    df_Sub <- df[df$Category == category, ]
    l_Sankey[[category]] <- list(
      from = total_name,
      to = category,
      weight = sum(df_Sub$Amount)
    )
    names(l_Sankey) <- NULL
    df_Sub[df_Sub$Category == category, "Source"] <- unlist(lapply(
      df_Sub[df_Sub$Category == category, "Source"], function(x) {
        paste0(x, " ")
      }
    ))
    
    for (source in unique(df_Sub$Source)) {
      df_Sub2 <- df_Sub[df_Sub$Source == source, ]
      l_Sankey[[source]] <- list(
        from = category,
        to = source,
        weight = sum(df_Sub2$Amount)
      )
      names(l_Sankey) <- NULL
      df_Sub2[df_Sub2$Source %in% c(category, source), "Product"] <- unlist(lapply(
        df_Sub2[df_Sub2$Source %in% c(category, source), "Product"], function(x) {
          paste0(x, "  ")
        }
      ))
      
      for (product in unique(df_Sub2$Product)) {
        df_Sub3 <- df_Sub2[df_Sub2$Product == product, ]
        l_Sankey[[product]] <- list(
          from = source,
          to = product,
          weight = sum(df_Sub3$Amount)
        )
        names(l_Sankey) <- NULL
      }
    }
  }
  df_Sankey <- as.data.frame(dplyr::bind_rows(l_Sankey))
  df_Sankey$weight = round(df_Sankey$weight, 2)
  
  res <- hchart(df_Sankey, "sankey")
  res <- hc_plotOptions(hc = res, series = list(dataLabels = list(allowOverlap = FALSE)))
  if (dark_mode_on) {
    res <- hc_add_theme(res, hc_theme_dark())
  } else {
    res <- hc_add_theme(res, hc_theme_light())
  }
  
  res
}

