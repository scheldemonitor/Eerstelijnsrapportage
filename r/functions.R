# functions to be used in the eerstelijnsrapportage
require(tidyverse)
require(viridis)
require(leaflet)
require(lubridate)
require(mblm)
require(Kendall)
require(plotly)
# require(rworldxtra)
# require(rworldmap)
require(sf)

# data(countriesHigh)
# bbox_scheldt <- st_bbox(c(xmin = 2.5, xmax = 4.7, ymax = 51.7, ymin = 51.2), crs = st_crs(4326))
# backgroundmap <- countriesHigh %>% st_as_sf() %>%
#   select(ADMIN) %>%
#   filter(ADMIN %in% c("Netherlands", "Belgium")) %>%
#   st_crop(st_bbox(bbox_scheldt)) %>%
#   st_transform(28992)
# save(backgroundmap, file = "data/backgroundmap.Rdata")

load("data/backgroundmap.Rdata")

###=== plot styles ==============================

# backgroundfill = "#ebf5ff"  # lichtblauw uit Deltares palette, werd minder mooi gevonden.
backgroundfill = "white"
textcolor = "#2E89BF"

trendplotstyle =   theme(
  plot.title=element_text(size=10, hjust=0.5, face="bold", colour="maroon", vjust=-1),
  text = element_text(color = textcolor),
  line = element_line(color = textcolor),
  plot.background = element_rect(fill = backgroundfill, color = "transparent"),
  legend.background = element_rect(fill = backgroundfill, color = "transparent"),
  panel.background = element_rect(fill = backgroundfill),
  panel.border = element_rect(color = textcolor, fill = "transparent"),
  panel.grid.major = element_line(color = textcolor, size = 0.1),
  panel.grid.minor = element_line(color = textcolor, size = 0.1),
  strip.background = element_rect(fill = backgroundfill, color = "white"),
  strip.text = element_text(color = textcolor, face = "bold"),
  axis.text = element_text(color = textcolor)
)

mapplotstyle =   theme(
  text = element_text(color = textcolor),
  line = element_line(color = textcolor),
  plot.background = element_rect(fill = backgroundfill, color = "transparent"),
  legend.background = element_rect(fill = backgroundfill, color = "transparent"),
  panel.background = element_rect(fill = backgroundfill),
  # panel.grid.major = element_line(color = "#2E89BF", size = 0.1),
  # panel.grid.minor = element_line(color = "#2E89BF", size = 0.1),
  # strip.background = element_rect(fill = backgroundfill, color = "white"),
  # strip.text = element_text(color = "#2E89BF", face = "bold"),
  axis.text = element_text(color = "#2E89BF")
)


# Kendall regression function with weights = 0 for use in geom_smooth()
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

###==== lijsten =============================

maanden <- c("januari", "februari", "maart", "april", "mei", "juni", "juli", 
             "augustus", "september", "oktober", "november", "december")


###==== plot functies =============================

plotLocations <- function(df, nudge__x = -1000, nudge__y = -3000, angle__ = 0, html = F){
  if(html){
    df %>% group_by(stationname) %>%
      summarize(latitude = mean(latitude, na.rm = T), longitude = mean(longitude, na.rm = T)) %>% 
      leaflet() %>%
      addTiles() %>%
      addCircleMarkers(label = ~stationname, labelOptions = labelOptions(noHide = T))
  } else
    df %>% 
    group_by(stationname) %>%
    summarize(latitude = mean(latitude, na.rm = T), longitude = mean(longitude, na.rm = T)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(28992) %>%
    ggplot() +
    geom_sf(data = backgroundmap, aes(), fill = "lightgrey", alpha = 0.5) +
    geom_sf_text(aes(label = stationname), hjust = 0, nudge_x = nudge__x, nudge_y = nudge__y, size = 3, angle = angle__)  +
    geom_sf(size = 4) + 
    coord_sf(datum=28992)  +
    theme(legend.position = "none") +
    mapplotstyle +
    theme_void() +
    theme(panel.background=element_blank(),
          panel.spacing = unit(c(0, 0, 0, 0), "cm"),       
          plot.background = element_rect(fill = "white",colour = NA),
          plot.margin = unit(c(0, 0, 0, 0), "null"),  # Edited code
          legend.position = 'none')
  
}

# Make table with lm statistics
statTable <- function(df, parname, rounding, meanorder = "decreasing", sf = F) {
  
  if(sf) df <- df %>% st_drop_geometry()
  
  anydata <- df %>% filter(parametername == parname) %>% nrow()
  if(anydata == 0){
    return(paste("Er zijn geen data gevonden voor", parname))
  }
  
  if(anydata > 0){
    stats <- df %>%
      drop_na(value) %>%
      filter(parametername == parname) %>%
      mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
      filter(year >= 2000) %>%
      # group_by(stationname, year, month) %>% 
      # summarize(monthlymean = mean(value)) %>%
      group_by(stationname, year) %>% 
      summarize(yearlymedian = median(value)) %>%
      drop_na(yearlymedian) %>%
      group_by(stationname) %>%
      do(broom::tidy(sen(yearlymedian ~ year, data = .))) %>% 
      filter(term == "year") 
    
    df %>% 
      drop_na(value) %>%
      filter(parametername == parname) %>% 
      mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
      filter(year >= 2000) %>%
      group_by(stationname) %>% 
      summarize(
        median = median(value, na.rm = T), 
        `10-perc` = quantile(value, 0.1, na.rm = T), 
        `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
      left_join(stats)  %>%
      mutate(across(where(is.numeric), round, 3)) %>%
      mutate(across(where(is.numeric), signif, rounding)) %>%
      select(Station = stationname,
             Mediaan = median,
             `90-perc`,
             `10-perc`,
             Trend = estimate,
             p = p.value) #%>%
    # arrange(-Gemiddelde)
  }
}



# Make table with statistics
statTableParams <- function(df, parnames, statname, rounding, meanorder = "decreasing", sf = F) {
  
  if(sf) df <- df %>% st_drop_geometry()
  
  anydata <- df %>% filter(parametername %in% parnames & stationname == statname) %>% nrow()
  if(anydata == 0){
    return(paste("Er zijn geen data gevonden voor", parnames))
  }
  
  if(anydata > 0){
    stats <- df %>%
      filter(parametername %in% parnames & stationname == statname) %>%
      mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
      filter(year >= 2000) %>%
      # group_by(stationname, year, month) %>% 
      # summarize(monthlymean = mean(value)) %>%
      group_by(parametername, stationname, year) %>% 
      summarize(yearlymedian = median(value)) %>%
      drop_na(yearlymedian) %>%
      group_by(parametername, stationname) %>%
      do(broom::tidy(sen(yearlymedian ~ year, data = .))) %>% 
      filter(term == "year") 
    
    df %>% 
      filter(parametername %in% parnames & stationname == statname) %>% 
      mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
      filter(year >= 2000) %>%
      group_by(parametername, stationname) %>% 
      summarize(
        median = median(value, na.rm = T), 
        `10-perc` = quantile(value, 0.1, na.rm = T), 
        `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
      left_join(stats)  %>%
      mutate(across(where(is.numeric), round, 3)) %>%
      mutate(across(where(is.numeric), signif, rounding)) %>%
      select(Parameter = parametername,
             Mediaan = median,
             `90-perc`,
             `10-perc`,
             Trend = estimate,
             p = p.value) #%>%
    # arrange(-Gemiddelde)
    
  }
}

statGraphParams <- function(df, parnames, meanorder = "decreasing", sf = F) {
  
  if(sf) df <- df %>% st_drop_geometry()
  
  anydata <- df %>% filter(parametername %in% parnames) %>% nrow()
  if(anydata == 0){
    return(paste("Er zijn geen data gevonden voor", parnames))
  }
  
  if(anydata > 0){
    
    stats <- df %>%
      # filter(stationname == "Vlissingen boei SSVH") %>%
      filter(parametername %in% parnames) %>%
      mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
      filter(year >= 2000) %>%
      # group_by(stationname, year, month) %>% 
      # summarize(monthlymean = mean(value)) %>%
      group_by(parametername, stationname, year) %>% 
      summarize(yearlymedian = median(value, na.rm = T), .groups = 'drop') %>%
      drop_na(yearlymedian) %>%
      drop_na(yearlymedian) %>%
      group_by(parametername, stationname) %>%
      do(broom::tidy(lm(yearlymedian ~ year, data = .))) %>% 
      filter(term == "year") 
    
    p <- df %>% 
      filter(parametername %in% parnames) %>%
      # filter(stationname == "Vlissingen boei SSVH") %>% 
      mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
      filter(year >= 2000) %>%
      group_by(parametername, stationname) %>% 
      summarize(
        median = median(value, na.rm = T), 
        `10-perc` = quantile(value, 0.1, na.rm = T), 
        `90-perc` = quantile(value, 0.9, na.rm = T), .groups = 'drop') %>%
      left_join(stats)  %>%
      # mutate(across(where(is.numeric), signif, 3)) %>%
      mutate(parametername = str_replace(parametername, " drooggewicht in zwevend stof", " in zs")) %>%
      select(Parameter = parametername,
             Locatie = stationname,
             Mediaan = median,
             `90-perc`,
             `10-perc`,
             Trend = estimate,
             p = p.value) %>% 
      mutate(Locatie = as.factor(Locatie)) %>%
      mutate(showTrend = as.factor(ifelse(p <= 0.05, sign(Trend), NA))) %>%
      ggplot(aes(Parameter, Locatie)) +
      geom_tile(aes(fill = showTrend)) +
      coord_flip() +
      scale_fill_manual(values = c("green", "white", "red")) +
      theme(axis.text.y = element_text(size = 7))
    scale_fill_gradient2(low = "green", high = "red", midpoint = 0, mid = "white", na.value = "grey")
    
    p
  }
}






fytStatTable <- function(df, statname){
  df %>% 
    filter(stationname == statname) %>%
    group_by(stationname, parametername) %>%
    summarize(mediaan = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
    mutate(across(where(is.double), round, 2)) %>% ungroup()
}



# Plot trends of nutrients
plotTrends <- function(df, parname, statmethod = sen, sf = F, trend = T, beginjaar = 1998, eindjaar = dataJaar) {
  
  if(sf) df <- df %>% st_drop_geometry()
  
  anydata <- df %>% filter(parametername == parname) %>% nrow()
  if(anydata == 0){
    return(paste("Er zijn geen data gevonden voor", parname))
  } else
    
    p <- df %>%
    dplyr::filter(parametername == parname) %>%
    dplyr::mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    dplyr::group_by(stationname, year) %>% 
    dplyr::summarize(median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
    dplyr::select(Station = stationname,
                  Jaar = year,
                  Mediaan = median,
                  `90-perc`,
                  `10-perc`) %>%
    dplyr::arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`), fill = "lightgrey", alpha = 0.7) +
    geom_line() + geom_point(fill = "white", shape = 21)
  if(trend)    p <- p + geom_smooth(method = statmethod, formula = y~x, color = "#2E89BF", fill = "#2E89BF", alpha = 0.2)
  p <- p + facet_wrap(~Station, ncol = 2, scales = "free") +
    theme_minimal() +
    ggtitle(label = parname) +
    coord_cartesian(xlim = c(beginjaar, eindjaar), ylim = c(0,NA)) +
    trendplotstyle
  return(p)
}

plotTrendsLimits <- function(df, parname, sf = F, trend = T, statmethod = sen) {
  
  if(sf) df <- df %>% st_drop_geometry()
  
  anydata <- df %>% filter(parametername == parname) %>% nrow()
  if(anydata == 0){
    return(paste("Er zijn geen data gevonden voor", parname))
  } else
    
    p <- df %>%
    separate(originalvalue, c("limiet", "originalvalue"), " ") %>%
    dplyr::filter(parametername == parname) %>%
    dplyr::mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    dplyr::group_by(stationname, year) %>% 
    dplyr::summarize(
      `n(<)` = ifelse(sum(limiet == "<") == 0 , NA, sum(limiet == "<")), 
      `n(=)` = ifelse(sum(!limiet %in% c("<", ">")) == 0 , NA, sum(!limiet %in% c("<", ">"))),
      `n(>)` = ifelse(sum(limiet == ">") == 0 , NA, sum(limiet == ">")), 
      median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)
    ) %>%
    dplyr::select(Station = stationname,
                  Jaar = year,
                  Mediaan = median,
                  `90-perc`,
                  `10-perc`,
                  `n(=)`,
                  `n(<)`,
                  `n(>)`) %>%
    dplyr::arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`), fill = "lightgrey") +
    geom_line() + 
    geom_point(aes(size = `n(=)`), fill = "white", shape = 21) #+
  # geom_text(aes(y = 0, label = `n(<)`), size = 4) +
  # geom_text(aes(y = 10, label = `n(>)`), size = 4)
  
  if(trend)    p <- p + geom_smooth(method = statmethod, fill = "blue", alpha = 0.2)
  p <- p + facet_wrap(~Station, ncol = 2) +
    # theme_minimal() +
    labs(title = parname, x = "jaar", y = "mediaan") +
    # ggtitle(parname) +
    coord_cartesian(ylim = c(0,NA)) +
    trendplotstyle
  return(p)
}

plotTrendsLimits2 <- function(df, parname, stations = trendstations, sf = F, trend = T, statmethod = sen) {
  
  if(sf) df <- df %>% st_drop_geometry()
  
  anydata <- df %>% filter(parametername %in% parname) %>% nrow()
  if(anydata == 0){
    return(paste("Er zijn geen data gevonden voor", parname))
  } else
    
    p <- df %>%
    separate(originalvalue, c("limiet", "originalvalue"), " ") %>%
    dplyr::filter(parametername %in% parname, stationname %in% stations) %>%
    dplyr::mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    dplyr::group_by(parametername, stationname, year) %>% 
    dplyr::summarize(
      `n(<)` = ifelse(sum(limiet == "<") == 0 , NA, sum(limiet == "<")), 
      `n(=)` = ifelse(sum(!limiet %in% c("<", ">")) == 0 , NA, sum(!limiet %in% c("<", ">"))),
      `n(>)` = ifelse(sum(limiet == ">") == 0 , NA, sum(limiet == ">")), 
      median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)
    ) %>%
    mutate(parametername = str_replace(parametername, " in mg/kg drooggewicht in zwevend stof", "")) %>%
    mutate(parametername = str_replace(parametername, " in ug/kg drooggewicht in zwevend stof", "")) %>%
    # mutate(parametername = ifelse(   # werkt niet helemaal goed
    #   str_detect(parametername, "PCB[0-9]{2,3}"), 
    #   str_extract(parname, "PCB[0-9]{2,3}" ),
    #   parametername
    # )) %>%
    dplyr::select(Station = stationname,
                  parametername,
                  Jaar = year,
                  Mediaan = median,
                  `90-perc`,
                  `10-perc`,
                  `n(=)`,
                  `n(<)`,
                  `n(>)`) %>%
    dplyr::arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`), fill = "lightgrey") +
    geom_line() + 
    geom_point(aes(size = `n(=)`), fill = "white", shape = 21) #+
  # geom_text(aes(y = 0, label = `n(<)`), size = 4) +
  # geom_text(aes(y = 10, label = `n(>)`), size = 4)
  
  if(trend)    p <- p + geom_smooth(method = statmethod, fill = "blue", alpha = 0.2)
  p <- p + facet_grid(parametername ~ Station, scales = "free") +
    theme_minimal() +
    ggtitle(label = parname) +
    coord_cartesian(ylim = c(0,NA)) +
    trendplotstyle
  return(p)
}


plotTrendsLimitsBiota <- function(df, statname, cat, sciname, sf = F, trend = T, statmethod = sen) {
  
  if(sf) df <- df %>% st_drop_geometry()
  
  anydata <- df %>% filter(
    stationname %in% statname,
    category %in% cat, 
    scientificname == sciname
  ) %>% nrow()
  if(anydata == 0){
    return(paste("Er zijn geen data gevonden voor", cat))
  } else
    
    p <- df %>%
    separate(originalvalue, c("limiet", "originalvalue"), " ") %>%
    dplyr::filter(
      stationname %in% statname,
      category %in% cat, 
      scientificname == sciname
    ) %>%
    dplyr::mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    dplyr::group_by(parametername, year) %>% 
    dplyr::summarize(
      `n(<)` = ifelse(sum(limiet == "<") == 0 , NA, sum(limiet == "<")), 
      `n(=)` = ifelse(sum(!limiet %in% c("<", ">")) == 0 , NA, sum(!limiet %in% c("<", ">"))),
      `n(>)` = ifelse(sum(limiet == ">") == 0 , NA, sum(limiet == ">")), 
      median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)
    ) %>%
    dplyr::select(Parameter = parametername,
                  Jaar = year,
                  Mediaan = median,
                  `90-perc`,
                  `10-perc`,
                  `n(=)`,
                  `n(<)`,
                  `n(>)`) %>%
    dplyr::arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`), fill = "lightgrey") +
    geom_line() + 
    geom_point(aes(size = `n(=)`), fill = "white", shape = 21) 
  
  if(trend)    p <- p + geom_smooth(method = statmethod, fill = "blue", alpha = 0.2)
  p <- p + facet_wrap(~Parameter, ncol = 2, scales = "free_y") +
    # ylab() +
    ggtitle(label = paste(statname, sciname, sep = ", ")) +
    coord_cartesian(ylim = c(0,NA)) +
    trendplotstyle
  return(p)
}



# Plot trends of nutrients
plotLogTrends <- function(df, parname, sf = F, trend = T, statmethod = sen) {
  
  if(sf) df <- df %>% st_drop_geometry()
  p <- df %>%
    dplyr::filter(parametername == parname) %>%
    dplyr::mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    dplyr::group_by(stationname, year) %>% 
    dplyr::summarize(median = exp(median(log(value), na.rm = T)), `10-perc` = exp(quantile(log(value), 0.1, na.rm = T)), `90-perc` = exp(quantile(log(value), 0.9, na.rm = T))) %>%
    dplyr::select(Station = stationname,
                  Jaar = year,
                  Mediaan = median,
                  `90-perc`,
                  `10-perc`) %>%
    dplyr::arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`), fill = "lightgrey") +
    geom_line() + geom_point(fill = "white", shape = 21)
  if(trend)    p <- p + geom_smooth(method = statmethod, fill = "blue", alpha = 0.2)
  p <- p + facet_wrap(~Station, ncol = 2) +
    theme_minimal() +
    ggtitle(label = parname) +
    coord_cartesian(ylim = c(NA,NA)) +
    scale_y_log10() +
    trendplotstyle
  return(p)
}

# plot (ZS) anomaly
plotLogAnomalies <- function(df, parname, sf = F) {
  
  require(zoo)
  
  jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  my_breaks = c(1, 2, 5, 10, 20, 50, 100, 250, 500)
  k = 13
  
  if(sf) df <- df %>% st_drop_geometry()
  p <- df %>%
    dplyr::filter(parametername == parname) %>%
    dplyr::mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    mutate(logwaarde = log(value)+0.001) %>%
    group_by(stationname, year, month) %>% 
    summarize(logmaandgemiddelde = mean(logwaarde, na.rm = T)) %>% 
    ungroup() %>%
    group_by(stationname) %>% 
    mutate(anomalie = logmaandgemiddelde - mean(logmaandgemiddelde, na.rm = T)) %>% 
    ungroup() %>%
    complete(stationname, year, month, fill = list(logmaandgemiddelde = NA, anomalie = NA)) %>%
    mutate(datum = as.Date(lubridate::ymd(paste(year, month, "15")))) %>%
    arrange(stationname, datum) %>%
    # mutate(anomalie = oce::fillGap(anomalie)) %>%
    group_by(stationname) %>% 
    mutate(rM = zoo::rollmean(anomalie, k, na.pad=TRUE, align="center", na.rm = T)) %>%
    # mutate(stationname = factor(stationname, levels = plotlocaties)) %>%
    ggplot(aes(datum, exp(anomalie))) + 
    geom_point(aes(color = exp(logmaandgemiddelde)), alpha = 0.2) +   #size = `n/year` color = exp(logmaandgemiddelde)
    geom_line(aes(y=exp(rM)), color = "blue", size = 1)  +
    ggtitle(label = parname) +
    facet_wrap(~ stationname, scales = "free", ncol = 2) +
    scale_color_gradientn(colours = jet.colors(7), name = "maandgemiddelde",
                          trans = "log", breaks = my_breaks, labels = my_breaks) +
    coord_cartesian(ylim = c(0,NA)) +
    scale_x_date(breaks = scales::pretty_breaks(), date_minor_breaks = "1 year", date_labels = "%Y") +
    trendplotstyle
  
  return(p)
}

plotTrendsWaterstand <- function(df, parname, locname, sf = F) {
  
  if(sf) df <- df %>% st_drop_geometry()
  p <- df %>%
    dplyr::filter(parametername %in% parname, stationname %in% locname) %>%
    dplyr::mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    dplyr::group_by(stationname, year, parametername) %>% 
    dplyr::summarize(mean = mean(value, na.rm = T)) %>%
    dplyr::select(Station = stationname,
                  Jaar = year,
                  Gemiddelde = mean,
                  Parameter = parametername) %>%
    dplyr::arrange(-Gemiddelde) %>%
    ggplot(aes(Jaar, Gemiddelde)) +
    geom_line(aes(), size = 1) + #color=Parameter 
    geom_point(aes(), fill = "white", shape = 21, size = 3) + #color=Parameter 
    facet_grid(Parameter ~ ., scales="free_y") +
    # theme_light() +
    ylab("Jaargemiddeld hoog- en laagwater in cm+NAP")+
    # scale_y_continuous(expand = expansion(mult = 0.1)) +
    coord_cartesian(xlim = c(1996, dataJaar)) +
    theme(
      legend.position="none",
      strip.text.y = element_text(angle = 0, face = "bold")) +
    trendplotstyle
  return(p)
}

plotTrendsGolven <- function(df, parname, locname, sf = F) {
  
  if(sf) df <- df %>% st_drop_geometry()
  p <- df %>%
    dplyr::filter(parametername %in% parname, stationname %in% locname) %>%
    dplyr::group_by(stationname, datetime, parametername) %>% 
    dplyr::select(Station = stationname,
                  Maand = datetime,
                  Gemiddelde = value,
                  Maximum = value_max,
                  Parameter = parametername) %>%
    mutate(shortParam = gsub(":.*$","",Parameter)) %>%
    dplyr::arrange(-Gemiddelde) %>%
    pivot_longer(c(Maximum, Gemiddelde), names_to = "Statistiek", values_to = "Waarde") %>%
    ggplot(aes(Maand, Waarde)) +
    geom_line(aes(color=Statistiek)) + 
    geom_point(aes(color=Statistiek), fill = "white", shape = 21) + 
    facet_grid(shortParam ~ ., scales="free_y") +
    theme_minimal() +
    ggtitle(label = parname) +
    theme(legend.position="none") +
    trendplotstyle +
    theme(strip.text.y = element_text(angle = 0))
  return(p)
}

plotTrendsByParameter <- function(df, parname, sf = F) {
  
  if(sf) df <- df %>% st_drop_geometry()
  p <- df %>%
    dplyr::filter(parametername %in% parname) %>%
    dplyr::select(Station = stationname,
                  Jaar = jaar,
                  Value = value,
                  Parameter = parametername) %>%
    dplyr::arrange(-Value) %>%
    ggplot(aes(Jaar, Value)) +
    geom_line(aes(color=Parameter)) + 
    geom_point(aes(color=Parameter), fill = "white", shape = 21) + 
    #facet_grid(Parameter ~ ., scales="free_y") +
    theme_minimal() +
    ggtitle(label = parname) +
    trendplotstyle
  return(p)
}

plotTrendsByLocation <- function(df, parname, sf = F) {
  
  if(sf) df <- df %>% st_drop_geometry()
  p <- df %>%
    dplyr::filter(parametername %in% parname) %>%
    dplyr::select(Station = stationname,
                  Jaar = jaar,
                  Value = value,
                  Parameter = parametername) %>%
    dplyr::arrange(-Value) %>%
    ggplot(aes(Jaar, Value)) +
    geom_line(aes(color=Station)) + 
    geom_point(aes(color=Station), fill = "white", shape = 21) + 
    #facet_grid(Parameter ~ ., scales="free_y") +
    theme_minimal() +
    ggtitle(label = parname) +
    trendplotstyle
  return(p)
}

plotTrendsByLocationClass <- function(df, parname, classname, sf = F) {
  
  if(sf) df <- df %>% st_drop_geometry()
  p <- df %>%
    dplyr::filter(parametername %in% parname, class %in% classname) %>%
    dplyr::mutate(year = lubridate::year(datetime)) %>%
    dplyr::group_by(stationname, year, parametername, class) %>% 
    dplyr::summarize(mean = mean(value, na.rm = T)) %>%
    dplyr::select(Station = stationname,
                  Jaar = year,
                  Mean = mean,
                  Parameter = parametername) %>%
    dplyr::arrange(-Mean) %>%
    ggplot(aes(Jaar, Mean)) +
    geom_line(aes(color=Station)) + 
    geom_point(aes(color=Station), fill = "white", shape = 21) + 
    #facet_grid(Parameter ~ ., scales="free_y") +
    theme_minimal() +
    ggtitle(label = paste(parname, classname)) +
    trendplotstyle
  return(p)
}

plotTrendsBar <- function(df, parname, sf = F) {
  
  if(sf) df <- df %>% st_drop_geometry()
  df %>%
    dplyr::filter(parametername == parname) %>%
    dplyr::mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    dplyr::group_by(stationname, year) %>% 
    dplyr::summarize(median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
    dplyr::select(Station = stationname,
                  Jaar = year,
                  Mediaan = median,
                  `90-perc`,
                  `10-perc`) %>%
    dplyr::arrange(-Mediaan) %>%
    ggplot(aes(x = Jaar, y = Mediaan)) +
    geom_col() +
    facet_wrap(~Station) +
    ggtitle(label = parname) +
    coord_cartesian(ylim = c(0,NA)) +
    trendplotstyle
}

plotTrendsSeizoen <- function(df, parname, sf = T) {
  if(sf) df <- df %>% st_drop_geometry()
  df %>%
    filter(parametername == parname) %>%
    mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    mutate(seizoen = case_when(
      month %in% c(4:9) ~ "zomer",
      !month %in% c(4:9) ~ "winter"
    )) %>%
    mutate(seizoen = factor(seizoen, levels = c("zomer", "winter"))) %>%
    group_by(stationname, year, seizoen) %>% 
    summarize(median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
    select(Station = stationname,
           Jaar = year,
           Mediaan = median,
           `90-perc`,
           `10-perc`,
           seizoen) %>%
    arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`, fill = seizoen), alpha = 0.4) +
    geom_line(aes(color = seizoen), size = 0.75) + 
    geom_point(aes(color = seizoen), fill = "white", shape = 21, size = 1.5) +
    # geom_smooth(method = "lm", fill = "blue", alpha = 0.2) +
    facet_wrap(~Station, ncol = 2, scales = "free") +
    theme_minimal() +
    ggtitle(label = parname) +
    coord_cartesian(ylim = c(0,NA)) +
    trendplotstyle
}

plotLogTrendsSeizoen <- function(df, parname, sf = T) {
  if(sf) df <- df %>% st_drop_geometry()
  df %>%
    filter(parametername == parname) %>%
    mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    mutate(seizoen = case_when(
      month %in% c(4:9) ~ "zomer",
      !month %in% c(4:9) ~ "winter"
    )) %>%
    mutate(seizoen = factor(seizoen, levels = c("zomer", "winter"))) %>%
    group_by(stationname, year, seizoen) %>% 
    summarize(median = exp(median(log(value), na.rm = T)), `10-perc` = exp(quantile(log(value), 0.1, na.rm = T)), `90-perc` = exp(quantile(log(value), 0.9, na.rm = T))) %>%
    select(Station = stationname,
           Jaar = year,
           Mediaan = median,
           `90-perc`,
           `10-perc`,
           seizoen) %>%
    arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`, fill = seizoen), alpha = 0.4) +
    geom_line(aes(color = seizoen), size = 0.75) + 
    geom_point(aes(color = seizoen), fill = "white", shape = 21, size = 1.5) +
    # geom_smooth(method = "lm", fill = "blue", alpha = 0.2) +
    facet_wrap(~Station, ncol = 2) +
    theme_minimal() +
    ggtitle(label = parname) +
    scale_y_log10() +
    trendplotstyle
}


plotTrendsMaand <- function(df, parname, sf = T) {
  if(sf) df <- df %>% st_drop_geometry()
  df %>%
    filter(parametername == parname) %>%
    mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    group_by(stationname, year, month) %>% 
    summarize(median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
    select(Station = stationname,
           Jaar = year,
           Mediaan = median,
           `90-perc`,
           `10-perc`,
           Maand = month) %>%
    mutate(MaandNaam = maanden[Maand]) %>%
    mutate(MaandNaam = factor(MaandNaam, levels = maanden)) %>%
    # arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    #geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`, fill = month), alpha = 0.4) +
    geom_line(aes(color = Station), size = 0.7) + #geom_point(aes(color = Station), fill = "white", shape = 21) +
    # geom_smooth(method = "lm", fill = "blue", alpha = 0.2) +
    facet_wrap(~MaandNaam, ncol = 3) +
    theme_minimal() +
    ggtitle(label = parname) +
    coord_cartesian(ylim = c(0,NA)) +
    trendplotstyle
}


plotTrendsCZVMaand <- function(df, parname, sf = T) {
  if(sf) df <- df %>% st_drop_geometry()
  df %>%
    filter(parametername %in% c(parname, "Saliniteit in PSU in oppervlaktewater")) %>%
    group_by(datetime, parametername, stationname) %>% summarize(value = mean(value, na.rm = T)) %>%
    mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    group_by(stationname, parametername, year, month) %>% 
    summarize(median = median(value, na.rm = T)) %>%
    select(Station = stationname,
           parametername,
           Jaar = year,
           Mediaan = median,
           Maand = month) %>%
    mutate(MaandNaam = maanden[Maand]) %>%
    mutate(MaandNaam = factor(MaandNaam, levels = maanden)) %>%
    arrange(-Mediaan) %>% 
    pivot_wider(id_cols = c(Jaar, MaandNaam, Station), names_from = parametername, values_from = Mediaan) %>% 
    drop_na() %>%
    mutate(`chloride in g/l` = `Saliniteit in PSU in oppervlaktewater`/1.8066) %>%
    mutate(detectielimiet = `chloride in g/l`*10) %>%
    select(-`Saliniteit in PSU in oppervlaktewater`, -`chloride in g/l`) %>%
    ggplot(aes(Jaar, `Chemisch zuurstofverbruik in mg/l uitgedrukt in Zuurstof (O2) in oppervlaktewater`)) +
    #geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`, fill = month), alpha = 0.4) +
    geom_line(aes(color = Station), size = 0.7) + #geom_point(aes(color = Station), fill = "white", shape = 21) +
    geom_line(aes(y = detectielimiet, color = Station), linetype = 2, size = 0.7) + #geom_point(aes(color = Station), fill = "white", shape = 21) +
    # geom_smooth(method = "lm", fill = "blue", alpha = 0.2) +
    facet_wrap(~MaandNaam) +
    theme_minimal() +
    ggtitle(label = parname) +
    coord_cartesian(ylim = c(0,60)) +
    trendplotstyle
}

plotLogTrendsMaand <- function(df, parname, sf = T) {
  if(sf) df <- df %>% st_drop_geometry()
  df %>%
    filter(parametername == parname) %>%
    mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    group_by(stationname, year, month) %>% 
    summarize(median = exp(median(log(value), na.rm = T)), `10-perc` = exp(quantile(log(value), 0.1, na.rm = T)), `90-perc` = exp(quantile(log(value), 0.9, na.rm = T))) %>%
    select(Station = stationname,
           Jaar = year,
           Mediaan = median,
           `90-perc`,
           `10-perc`,
           Maand = month) %>%
    mutate(MaandNaam = maanden[Maand]) %>%
    mutate(MaandNaam = factor(MaandNaam, levels = maanden)) %>%
    arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    #geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`, fill = month), alpha = 0.4) +
    geom_line(aes(color = Station), size = 0.75) + #geom_point(aes(color = Station), fill = "white", shape = 21) +
    # geom_smooth(method = "lm", fill = "blue", alpha = 0.2) +
    facet_wrap(~MaandNaam) +
    theme_minimal() +
    ggtitle(label = parname) +
    scale_y_log10() +
    trendplotstyle
}


plotTrendFyto <- function(df, statname){
  df %>% ungroup() %>%
    filter(stationname == statname) %>%
    mutate(jaar = lubridate::year(datetime), maand = lubridate::month(datetime)) %>%
    mutate(seizoen = ifelse(maand %in% c(4:9), "zomer", "winter")) %>%
    group_by(jaar, seizoen, parametername) %>%
    summarize(
      `90-perc` = quantile(value, 0.9, na.rm = T),
      mediaan = median(value, na.rm = T),
      gemiddelde = mean(value, na.rm = T)
    ) %>% ungroup() %>% 
    ggplot(aes(x = jaar, y = mediaan)) +
    geom_point(aes(color = seizoen), size = 3) +
    geom_point(data = df.fyt.groep %>% ungroup() %>%
                 mutate(jaar = lubridate::year(datetime), maand = lubridate::month(datetime)) %>%
                 mutate(seizoen = ifelse(maand %in% c(4:9), "zomer", "winter")) %>%
                 filter(stationname == statname, parametername == "fytoplankton - Phaeocystis") %>%
                 group_by(jaar, parametername, seizoen) %>%
                 summarize(`90-perc` = quantile(value, probs = 0.9, na.rm = T)) %>% ungroup() %>%
                 filter(seizoen == "zomer"),
               aes(x = jaar, y = `90-perc`, color = seizoen),
               shape = "-", size = 10
    ) +
    geom_smooth(aes(color = seizoen), alpha = 0.2) +
    # geom_line(aes(y = gemiddelde, color = seizoen)) +
    geom_hline(linetype = 2, color = "blue",
               data = df.fyt.groep %>% 
                 filter(stationname == statname) %>%
                 group_by(parametername) %>%
                 summarize(mediaan = median(value, na.rm = T)) %>% ungroup(),
               aes(yintercept = mediaan)
    ) +
    facet_wrap(~ parametername, ncol = 2, scales = "free") +
    scale_y_log10() +
    labs(subtitle = statname) +
    trendplotstyle +
    ylab(bquote("celaantal in " ~ 10^6 ~ "/l"))
}


plotTrendFytoGroup <- function(df, groupname){
  df %>% ungroup() %>%
    filter(parametername == groupname) %>%
    mutate(jaar = lubridate::year(datetime)) %>%
    group_by(jaar, stationname) %>%
    summarize(
      `90-perc` = quantile(value, 0.9, na.rm = T),
      mediaan = median(value, na.rm = T)
    ) %>% ungroup() %>% 
    ggplot(aes(jaar, mediaan)) +
    geom_point(aes(color = stationname), size = 3) +
    # geom_smooth(aes(color = stationname), alpha = 0.2, method = "loess", span = 0.5) +
    geom_point(data = df.fyt.groep %>% ungroup() %>%
                 mutate(jaar = lubridate::year(datetime), maand = lubridate::month(datetime)) %>%
                 filter(parametername == groupname) %>%
                 filter(maand %in% c(4:9)) %>%
                 filter(parametername == "fytoplankton - Phaeocystis") %>%
                 group_by(jaar, stationname) %>%
                 summarize(`90-perc` = quantile(value, probs = 0.9, na.rm = T)) %>% ungroup(),
               aes(x = jaar, y = `90-perc`, color = stationname),
               shape = "-", size = 10
    ) +
    geom_line(aes(color = stationname)) +
    geom_hline(linetype = 2, color = "blue",
               data = df.fyt.groep %>%
                 filter(parametername == groupname) %>%
                 summarize(mediaan = median(value, na.rm = T)) %>% ungroup(),
               aes(yintercept = mediaan)
    ) +
    scale_y_log10() +
    labs(subtitle = groupname) +
    trendplotstyle +
    ylab(bquote("celaantal in " ~ 10^6 ~ "/l"))
  
}

plotCDF <- function(df, parname) {
  p <- df %>%
    dplyr::filter(parametername == parname) %>%
    dplyr::group_by(stationname) %>% 
    dplyr::select(Station = stationname,
                  Value = value) %>%
    ggplot(aes(x=Value,color=Station)) + 
    stat_ecdf(size=0.7) +
    theme_minimal() +
    xlab(parname) +
    ylab("F(x)") +
    #coord_cartesian(xlim = c(0,150)) +
    trendplotstyle
  return(p)
}


stationMean <- function(df, parname){
  df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    group_by(stationname) %>% 
    summarize(mean = mean(value, na.rm = T), latitude = mean(latitude), longitude = mean(longitude)) %>%
    select(Station = stationname,
           Gemiddelde = mean,
           latitude,
           longitude
    )
}

stationWeightedMean <- function(df, parname){
  df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    group_by(stationname) %>% 
    summarize(mean = weighted.mean(value, datapoints, na.rm = T), latitude = mean(latitude), longitude = mean(longitude)) %>%
    select(Station = stationname,
           Gemiddelde = mean,
           latitude,
           longitude
    )
}

stationMeanClass <- function(df, parname, classname){
  df %>% #st_drop_geometry() %>%
    filter(parametername == parname, class == classname) %>%
    group_by(stationname) %>% 
    summarize(mean = mean(value, na.rm = T), latitude = mean(latitude), longitude = mean(longitude)) %>%
    select(Station = stationname,
           Gemiddelde = mean,
           latitude,
           longitude
    )
}

stationMedian <- function(df, parname){
  df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    group_by(stationname) %>% 
    summarize(median = median(value, na.rm = T), latitude = mean(latitude), longitude = mean(longitude)) %>%
    select(Station = stationname,
           Mediaan = median,
           latitude,
           longitude
    )
}

stationMax <- function(df, parname){
  df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    group_by(stationname) %>% 
    summarize(max = max(value, na.rm = T), latitude = mean(latitude), longitude = mean(longitude)) %>%
    select(Station = stationname,
           Max = max,
           latitude,
           longitude
    )
}

plotMeanMap <- function(df, parname, html = F) {
  
  values = df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    filter(year(datetime) >= 2000) %>%
    group_by(stationname) %>% summarize(mean = mean(value, na.rm = T)) %>%
    select(mean) %>% unlist() %>% unname()
  
  if(html){  
    pal <- colorNumeric(viridis(n = 7),
                        domain = values
    )
    
    df %>% #st_drop_geometry() %>%
      filter(parametername == parname) %>%
      filter(year(datetime) >= 2000) %>%
      group_by(stationname) %>% 
      summarize(mean = mean(value, na.rm = T), latitude = mean(latitude), longitude = mean(longitude)) %>%
      select(Station = stationname,
             Gemiddelde = mean,
             latitude,
             longitude
      ) %>% 
      leaflet() %>%
      addTiles() %>%
      addCircleMarkers(fillColor = ~pal(Gemiddelde), fillOpacity = 1, stroke = F, label = ~paste(Station, signif(Gemiddelde, 2), parname)) %>%
      leaflet::addLegend("topright", pal, values, opacity = 1)
  } else
    
    df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    filter(year(datetime) >= 2000) %>%
    group_by(stationname) %>% 
    summarize(mean = mean(value, na.rm = T), latitude = mean(latitude), longitude = mean(longitude)) %>%
    select(Station = stationname,
           Gemiddelde = mean,
           latitude,
           longitude
    ) %>% 
    ggplot(aes(longitude, latitude)) +
    geom_point(aes(color = Gemiddelde), size = 4)
    
}

plotWeightedMeanMap <- function(df, parname, html = F) {
  
  values = df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    filter(year(datetime) >= 2000) %>%
    group_by(stationname) %>% summarize(mean = weighted.mean(value, datapoints, na.rm = T)) %>%
    select(mean) %>% unlist() %>% unname()
  
  if(html){pal <- colorNumeric(viridis(n = 7),
                      domain = values
  )
  
  df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    filter(year(datetime) >= 2000) %>%
    group_by(stationname) %>% 
    summarize(mean = weighted.mean(value, datapoints, na.rm = T), latitude = mean(latitude), longitude = mean(longitude)) %>%
    select(Station = stationname,
           Gemiddelde = mean,
           latitude,
           longitude
    ) %>% 
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(fillColor = ~pal(Gemiddelde), fillOpacity = 1, stroke = F, label = ~paste(Station, signif(Gemiddelde, 2), parname)) %>%
    leaflet::addLegend("topright", pal, values, opacity = 1)
  } else
    df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    filter(year(datetime) >= 2000) %>%
    group_by(stationname) %>% 
    summarize(mean = weighted.mean(value, datapoints, na.rm = T), latitude = mean(latitude), longitude = mean(longitude)) %>%
    select(Station = stationname,
           Gemiddelde = mean,
           latitude,
           longitude
    ) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(28992) %>%
    ggplot(aes()) +
    geom_sf(data = backgroundmap, aes(), fill = "lightgrey", alpha = 0.5) +
    geom_sf_label(aes(label = Station), nudge_y = -3000, size = 3)  +
    geom_sf(aes(color = Gemiddelde), size = 6) +
    ggtitle(label = parname) +
    coord_sf(datum=28992)  +
    scale_color_viridis() +
    theme(axis.title = element_blank(),
          axis.text = element_blank()) +
    guides(fill = "none") +
    mapplotstyle +
    theme_void()
  
}

plotMedianMap <- function(df, parname, reverse_scale = FALSE, html = F) {
  values = df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    filter(year(datetime) >= 2000) %>%
    group_by(stationname) %>% summarize(median = median(value, na.rm = T)) %>%
    select(median) %>% unlist() %>% unname()
  
if(html){  pal <- colorNumeric(viridis(n = 7), 
                      reverse = reverse_scale,
                      domain = values
  )
  
  df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    filter(year(datetime) >= 2000) %>%
    group_by(stationname) %>% 
    summarize(median = median(value, na.rm = T), latitude = mean(latitude, na.rm = T), longitude = mean(longitude, na.rm = T)) %>%
    select(Station = stationname,
           Mediaan = median,
           latitude,
           longitude
    ) %>% 
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(fillColor = ~pal(Mediaan), fillOpacity = 1, stroke = F, label = ~paste(Station, signif(Mediaan, 2), parname)) %>%
    leaflet::addLegend("topright", pal, values, opacity = 1)
    } else
      df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    filter(year(datetime) >= 2000) %>%
    group_by(stationname) %>% 
    summarize(median = median(value, na.rm = T), latitude = mean(latitude, na.rm = T), longitude = mean(longitude, na.rm = T)) %>%
    select(Station = stationname,
           Mediaan = median,
           latitude,
           longitude
    ) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(28992) %>%
    ggplot(aes()) +
    geom_sf(data = backgroundmap, aes(), fill = "lightgrey", alpha = 0.5) +
    # geom_sf_label(aes(label = Station), nudge_y = -3000, size = 3)  +
    geom_sf(aes(color = Mediaan, size = Mediaan)) +
    ggtitle(label = parname) +
    coord_sf(datum=28992)  +
    scale_color_viridis() +
    scale_size_continuous(range = c(2,10), guide = "none") +
    # theme(axis.title = element_blank(),
    #       axis.text = element_blank()) +
    # guides(fill = "none", size = "none") +
    mapplotstyle +
    theme_void() +
    theme(panel.background=element_blank(),
          panel.spacing = unit(c(0, 0, 0, 0), "cm"),       
          plot.background = element_rect(fill = "white",colour = NA),
          plot.margin = unit(c(0, 0, 0, 0), "null")#,  # Edited code
          # legend.position = 'none'
          )
  }


plotLogMedianMap <- function(df, parname, reverse_scale = FALSE, html = F) {

  values = df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    filter(year(datetime) >= 2000) %>%
    group_by(stationname) %>% summarize(median = exp(median(log(value), na.rm = T))) %>%
    select(median) %>% unlist() %>% unname()
  
  if(html){pal <- colorNumeric(viridis(n = 7),
                               reverse = reverse_scale,
                               domain = values
  )
  
  df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    filter(year(datetime) >= 2000) %>%
    group_by(stationname) %>% 
    summarize(median = exp(median(log(value), na.rm = T)), latitude = mean(latitude, na.rm = T), longitude = mean(longitude, na.rm = T)) %>%
    select(Station = stationname,
           Mediaan = median,
           latitude,
           longitude
    ) %>% 
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(fillColor = ~pal(Mediaan), fillOpacity = 1, stroke = F, label = ~paste(Station, signif(Mediaan, 2), parname)) %>%
    leaflet::addLegend("topright", pal, values, opacity = 1)
  } else
    df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    filter(year(datetime) >= 2000) %>%
    group_by(stationname) %>% 
    summarize(median = exp(median(log(value), na.rm = T)), latitude = mean(latitude, na.rm = T), longitude = mean(longitude, na.rm = T)) %>%
    select(Station = stationname,
           Mediaan = median,
           latitude,
           longitude
    ) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(28992) %>%
    ggplot(aes()) +
    geom_sf(data = backgroundmap, aes(), fill = "lightgrey", alpha = 0.5) +
    geom_sf_label(aes(label = Station), nudge_y = -3000, size = 3)  +
    geom_sf(aes(color = Mediaan), size = 6) +
    ggtitle(label = parname) +
    coord_sf(datum=28992)  +
    scale_color_viridis() +
    theme(axis.title = element_blank(),
          axis.text = element_blank()) +
    guides(fill = "none") +
    mapplotstyle +
    theme_void()
}


# Plot trends of substances in biota
plotTrendsBiota <- function(df, parname, sf = F, trend = T, statmethod = sen, beginjaar = 1998, eindjaar = dataJaar) {
  
  if(sf) df <- df %>% st_drop_geometry()
  
  anydata <- df %>% filter(parametername == parname) %>% nrow()
  if(anydata == 0){
    return(paste("Er zijn geen data gevonden voor", parname))
  } else
    
    p <- df %>%
    dplyr::filter(parametername == parname) %>%
    dplyr::mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    dplyr::group_by(stationname, year) %>% 
    dplyr::summarize(median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
    dplyr::select(Station = stationname,
                  Jaar = year,
                  Mediaan = median,
                  `90-perc`,
                  `10-perc`) %>%
    dplyr::arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`), fill = "lightgrey", alpha = 0.7) +
    geom_line() + geom_point(fill = "white", shape = 21)
  if(trend)    p <- p + geom_smooth(method = statmethod, color = "#2E89BF", fill = "#2E89BF", alpha = 0.2)
  p <- p + facet_wrap(~Station, ncol = 2, scales = "free") +
    theme_minimal() +
    ggtitle(label = parname) +
    coord_cartesian(xlim = c(beginjaar, eindjaar), ylim = c(0,NA)) +
    trendplotstyle
  return(p)
}


plotTrendsBiota2 <- function(df, statname, cat, sciname, sf = F, trend = T, statmethod = sen) {
  
  if(sf) df <- df %>% st_drop_geometry()
  
  anydata <- df %>% filter(
    stationname %in% statname,
    category %in% cat, 
    scientificname == sciname
  ) %>% nrow()
  if(anydata == 0){
    return(paste("Er zijn geen data gevonden voor", cat))
  } else
    
    p <- df %>%
    # separate(originalvalue, c("limiet", "originalvalue"), " ") %>%
    dplyr::filter(
      stationname %in% statname,
      category %in% cat, 
      scientificname == sciname
    ) %>%
    dplyr::mutate(year = lubridate::year(datetime), month = lubridate::month(datetime)) %>%
    dplyr::group_by(parametername, year) %>% 
    # dplyr::summarize(
    #   `n(<)` = ifelse(sum(limiet == "<") == 0 , NA, sum(limiet == "<")), 
    #   `n(=)` = ifelse(sum(!limiet %in% c("<", ">")) == 0 , NA, sum(!limiet %in% c("<", ">"))),
    #   `n(>)` = ifelse(sum(limiet == ">") == 0 , NA, sum(limiet == ">")), 
    #   median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)
    # ) %>%
    dplyr::select(Parameter = parametername,
                  Jaar = year,
                  gemiddelde = value,
                  # `90-perc`,
                  # `10-perc`,
                  # `n(=)`,
                  # `n(<)`,
                  # `n(>)`
    ) %>%
    # dplyr::arrange(-Mediaan) %>%
    ggplot(aes(Jaar, gemiddelde)) +
    # geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`), fill = "lightgrey") +
    geom_line() + 
    geom_point(aes(), fill = "white", shape = 21) 
  
  if(trend)    p <- p + geom_smooth(method = statmethod, fill = "blue", alpha = 0.2)
  p <- p + facet_wrap(~Parameter, ncol = 2, scales = "free_y") +
    # ylab() +
    ggtitle(label = paste(statname, sciname, sep = ", ")) +
    coord_cartesian(ylim = c(0,NA)) +
    trendplotstyle
  return(p)
}

