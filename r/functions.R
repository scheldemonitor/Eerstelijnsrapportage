# functions to be used in the eerstelijnsrapportage
require(tidyverse)
require(viridis)
require(leaflet)

# nog te maken functies
# filternutriendata
#   - if value < detection limiet, dan value <- 0.* detectielimiet
#   - Completteer data met alle combinaties van tijdstip, locatie en parameter
#   - Bereken schatting van ontbrekende data per seizoen
#   - 

plotLocations <- function(df){
  df %>% group_by(stationname) %>%
    summarize(latitude = mean(latitude, na.rm = T), longitude = mean(longitude, na.rm = T)) %>% 
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(label = ~stationname, labelOptions = labelOptions(noHide = T))
}


# Make table with statistics
statTable <- function(df, parname, rounding, meanorder = "decreasing", sf = F) {
  
  if(sf) df <- df %>% st_drop_geometry()
  
anydata <- df %>% filter(parametername == parname) %>% nrow()
if(anydata == 0){
  return(paste("Er zijn geen data gevonden voor", parname))
}
  
if(anydata > 0){
  stats <- df %>%
    filter(parametername == parname) %>%
    mutate(year = year(datetime), month = month(datetime)) %>%
    # group_by(stationname, year, month) %>% 
    # summarize(monthlymean = mean(value)) %>%
    group_by(stationname, year) %>% 
    summarize(yearlymedian = median(value)) %>%
    group_by(stationname) %>%
    do(broom::tidy(lm(yearlymedian ~ year, data = .))) %>% 
    filter(term == "year") 
  
  df %>% 
    filter(parametername == parname) %>% 
    mutate(year = year(datetime), month = month(datetime)) %>%
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


fytStatTable <- function(df, statname){
  df %>% 
    filter(stationname == statname) %>%
    group_by(stationname, parametername) %>%
    summarize(median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
    mutate(across(where(is.double), round, 3)) %>% ungroup()
}



# Plot trends of nutrients
plotTrends <- function(df, parname, sf = F, trend = T) {
  
  
  
if(sf) df <- df %>% st_drop_geometry()

anydata <- df %>% filter(parametername == parname) %>% nrow()
if(anydata == 0){
  return(paste("Er zijn geen data gevonden voor", parname))
} else

  p <- df %>%
    dplyr::filter(parametername == parname) %>%
    dplyr::mutate(year = year(datetime), month = month(datetime)) %>%
    dplyr::group_by(stationname, year) %>% 
    dplyr::summarize(median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
    dplyr::select(Station = stationname,
           Jaar = year,
           Mediaan = median,
           `90-perc`,
           `10-perc`) %>%
    dplyr::arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`), fill = "lightgrey") +
    geom_line() + geom_point(fill = "white", shape = 21)
  if(trend)    p <- p + geom_smooth(method = "lm", fill = "blue", alpha = 0.2)
    p <- p + facet_wrap(~Station, ncol = 2) +
    theme_minimal() +
    ylab(parname) +
    coord_cartesian(ylim = c(0,NA))
    return(p)
}


plotTrendsLimits <- function(df, parname, sf = F, trend = T) {
  
  if(sf) df <- df %>% st_drop_geometry()
  p <- df %>%
    separate(originalvalue, c("limiet", "originalvalue"), " ") %>%
    dplyr::filter(parametername == parname) %>%
    dplyr::mutate(year = year(datetime), month = month(datetime)) %>%
    dplyr::group_by(stationname, year) %>% 
    dplyr::summarize(
      `n(<)` = ifelse(sum(limiet == "<") == 0 , NA, sum(limiet == "<")), 
      `n(=)` = ifelse(sum(limiet == "=") == 0 , NA, sum(limiet == "=")),
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
    geom_point(aes(size = `n(=)`), fill = "white", shape = 21) +
    geom_text(aes(y = 0, label = `n(<)`), size = 4) +
    geom_text(aes(y = 10, label = `n(>)`), size = 4)
  
  if(trend)    p <- p + geom_smooth(method = "lm", fill = "blue", alpha = 0.2)
  p <- p + facet_wrap(~Station, ncol = 2) +
    theme_minimal() +
    ylab(parname) +
    coord_cartesian(ylim = c(0,NA))
  return(p)
}

# Plot trends of nutrients
plotLogTrends <- function(df, parname, sf = F, trend = T) {
  
  if(sf) df <- df %>% st_drop_geometry()
  p <- df %>%
    dplyr::filter(parametername == parname) %>%
    dplyr::mutate(year = year(datetime), month = month(datetime)) %>%
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
  if(trend)    p <- p + geom_smooth(method = "lm", fill = "blue", alpha = 0.2)
  p <- p + facet_wrap(~Station, ncol = 2) +
    theme_minimal() +
    ylab(parname) +
    coord_cartesian(ylim = c(NA,NA)) +
    scale_y_log10()
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
    dplyr::mutate(year = year(datetime), month = month(datetime)) %>%
    mutate(logwaarde = log(value)+0.001) %>%
    group_by(stationname, year, month) %>% summarize(logmaandgemiddelde = mean(logwaarde, na.rm = T)) %>% ungroup() %>%
    group_by(stationname) %>% mutate(anomalie = logmaandgemiddelde - mean(logmaandgemiddelde, na.rm = T)) %>% 
    complete(stationname, year, month, fill = list(logmaandgemiddelde = NA, anomalie = NA)) %>%
    mutate(datum = as.Date(lubridate::ymd(paste(year, month, "15")))) %>%
    arrange(stationname, datum) %>%
    # mutate(anomalie = oce::fillGap(anomalie)) %>%
    group_by(stationname) %>% mutate(rM=rollmean(anomalie,k, na.pad=TRUE, align="center", na.rm = T)) %>%
    # mutate(stationname = factor(stationname, levels = plotlocaties)) %>%
    ggplot(aes(datum, exp(anomalie))) + 
    geom_point(aes(color = exp(logmaandgemiddelde)), alpha = 0.4) +   #size = `n/year`
    geom_line(aes(y=exp(rM)), color = "blue")  +
    ylab(parname) +
    facet_wrap(~ stationname, scales = "free", ncol = 2) +
    scale_color_gradientn(colours = jet.colors(7), name = "maandgemiddelde",
                          trans = "log", breaks = my_breaks, labels = my_breaks) +
    coord_cartesian(ylim = c(0,4)) +
    theme_minimal()

  return(p)
}



plotTrendsWaterstand <- function(df, parname, locname, sf = F) {
  
  if(sf) df <- df %>% st_drop_geometry()
  p <- df %>%
    dplyr::filter(parametername %in% parname, stationname %in% locname) %>%
    dplyr::mutate(year = year(datetime), month = month(datetime)) %>%
    dplyr::group_by(stationname, year, parametername) %>% 
    dplyr::summarize(mean = mean(value, na.rm = T)) %>%
    dplyr::select(Station = stationname,
                  Jaar = year,
                  Gemiddelde = mean,
                  Parameter = parametername) %>%
    dplyr::arrange(-Gemiddelde) %>%
    ggplot(aes(Jaar, Gemiddelde)) +
    geom_line(aes(color=Parameter)) + 
    geom_point(aes(fill=Parameter), color = "white", shape = 21) + 
    facet_grid(Parameter ~ ., scales="free_y") +
    theme_light() +
    ylab("Jaargemiddeld hoog- en laagwater in cm+NAP") +
    theme(
      legend.position="none",
      strip.text.y = element_text(angle = 0, face = "bold"))
  return(p)
}

plotTrendsGolven <- function(df, parname, locname, sf = F) {
  
  if(sf) df <- df %>% st_drop_geometry()
  p <- df %>%
    dplyr::filter(parametername %in% parname, stationname %in% locname) %>%
    dplyr::mutate(year = year(datetime), month = month(datetime)) %>%
    dplyr::group_by(stationname, year, month, parametername) %>% 
    dplyr::summarize(mean = mean(value, na.rm = T)) %>%
    dplyr::select(Station = stationname,
                  Jaar = year,
                  Maand = month,
                  Gemiddelde = mean,
                  Parameter = parametername) %>%
    dplyr::arrange(-Gemiddelde) %>%
    ggplot(aes(Jaar, Gemiddelde)) +
    geom_line(aes(color=Parameter)) + 
    geom_point(aes(fill=Parameter), color = "white", shape = 21) + 
    #facet_grid(Parameter ~ ., scales="free_y") +
    theme_minimal() +
    ylab(parname) +
    theme(legend.position="none")
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
    geom_point(aes(fill=Station), color = "white", shape = 21) + 
    #facet_grid(Parameter ~ ., scales="free_y") +
    theme_minimal() +
    ylab(parname)
  return(p)
}

plotTrendsByLocationClass <- function(df, parname, classname, sf = F) {
  
  if(sf) df <- df %>% st_drop_geometry()
  p <- df %>%
    dplyr::filter(parametername %in% parname, class %in% classname) %>%
    dplyr::mutate(year = year(datetime)) %>%
    dplyr::group_by(stationname, year, parametername, class) %>% 
    dplyr::summarize(mean = mean(value, na.rm = T)) %>%
    dplyr::select(Station = stationname,
                  Jaar = year,
                  Mean = mean,
                  Parameter = parametername) %>%
    dplyr::arrange(-Mean) %>%
    ggplot(aes(Jaar, Mean)) +
    geom_line(aes(color=Station)) + 
    geom_point(aes(fill=Station), color = "white", shape = 21) + 
    #facet_grid(Parameter ~ ., scales="free_y") +
    theme_minimal() +
    ylab(paste(parname, classname))
  return(p)
}

plotTrendsBar <- function(df, parname, sf = F) {
  
  if(sf) df <- df %>% st_drop_geometry()
  df %>%
    dplyr::filter(parametername == parname) %>%
    dplyr::mutate(year = year(datetime), month = month(datetime)) %>%
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
    theme_minimal() +
    ylab(parname) +
    coord_cartesian(ylim = c(0,NA))
}

plotTrendsSeizoen <- function(df, parname, sf = T) {
  if(sf) df <- df %>% st_drop_geometry()
  df %>%
    filter(parametername == parname) %>%
    mutate(year = year(datetime), month = month(datetime)) %>%
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
    geom_line(aes(color = seizoen)) + geom_point(aes(color = seizoen), fill = "white", shape = 21) +
    # geom_smooth(method = "lm", fill = "blue", alpha = 0.2) +
    facet_wrap(~Station, ncol = 2) +
    theme_minimal() +
    ylab(parname) +
    coord_cartesian(ylim = c(0,NA))
}

plotLogTrendsSeizoen <- function(df, parname, sf = T) {
  if(sf) df <- df %>% st_drop_geometry()
  df %>%
    filter(parametername == parname) %>%
    mutate(year = year(datetime), month = month(datetime)) %>%
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
    geom_line(aes(color = seizoen)) + geom_point(aes(color = seizoen), fill = "white", shape = 21) +
    # geom_smooth(method = "lm", fill = "blue", alpha = 0.2) +
    facet_wrap(~Station, ncol = 2) +
    theme_minimal() +
    ylab(parname) +
    scale_y_log10()
}


plotTrendsMaand <- function(df, parname, sf = T) {
  if(sf) df <- df %>% st_drop_geometry()
  df %>%
    filter(parametername == parname) %>%
    mutate(year = year(datetime), month = month(datetime)) %>%
    group_by(stationname, year, month) %>% 
    summarize(median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
    select(Station = stationname,
           Jaar = year,
           Mediaan = median,
           `90-perc`,
           `10-perc`,
           Maand = month) %>%
    arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    #geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`, fill = month), alpha = 0.4) +
    geom_line(aes(color = Station)) + #geom_point(aes(color = Station), fill = "white", shape = 21) +
    # geom_smooth(method = "lm", fill = "blue", alpha = 0.2) +
    facet_wrap(~Maand, ncol = 3) +
    theme_minimal() +
    ylab(parname) +
    coord_cartesian(ylim = c(0,NA))
}


plotTrendsCZVMaand <- function(df, parname, sf = T) {
  if(sf) df <- df %>% st_drop_geometry()
  df %>%
    filter(parametername %in% c(parname, "Saliniteit in PSU in oppervlaktewater")) %>%
    group_by(datetime, parametername, stationname) %>% summarize(value = mean(value, na.rm = T)) %>%
    mutate(year = year(datetime), month = month(datetime)) %>%
    group_by(stationname, parametername, year, month) %>% 
    summarize(median = median(value, na.rm = T)) %>%
    select(Station = stationname,
           parametername,
           Jaar = year,
           Mediaan = median,
           Maand = month) %>%
    arrange(-Mediaan) %>% 
    pivot_wider(id_cols = c(Jaar, Maand, Station), names_from = parametername, values_from = Mediaan) %>% 
    drop_na() %>%
    mutate(`chloride in g/l` = `Saliniteit in PSU in oppervlaktewater`/1.8066) %>%
    mutate(detectielimiet = `chloride in g/l`*10) %>%
    select(-`Saliniteit in PSU in oppervlaktewater`, -`chloride in g/l`) %>%
    ggplot(aes(Jaar, `Chemisch zuurstofverbruik in mg/l uitgedrukt in Zuurstof (O2) in oppervlaktewater`)) +
    #geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`, fill = month), alpha = 0.4) +
    geom_line(aes(color = Station), size = 1) + #geom_point(aes(color = Station), fill = "white", shape = 21) +
    geom_line(aes(y = detectielimiet, color = Station), linetype = 2, size = 1) + #geom_point(aes(color = Station), fill = "white", shape = 21) +
    # geom_smooth(method = "lm", fill = "blue", alpha = 0.2) +
    facet_wrap(~Maand) +
    theme_minimal() +
    ylab(parname) +
    coord_cartesian(ylim = c(0,60))
}

plotLogTrendsMaand <- function(df, parname, sf = T) {
  if(sf) df <- df %>% st_drop_geometry()
  df %>%
    filter(parametername == parname) %>%
    mutate(year = year(datetime), month = month(datetime)) %>%
    group_by(stationname, year, month) %>% 
    summarize(median = exp(median(log(value), na.rm = T)), `10-perc` = exp(quantile(log(value), 0.1, na.rm = T)), `90-perc` = exp(quantile(log(value), 0.9, na.rm = T))) %>%
    select(Station = stationname,
           Jaar = year,
           Mediaan = median,
           `90-perc`,
           `10-perc`,
           Maand = month) %>%
    arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    #geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`, fill = month), alpha = 0.4) +
    geom_line(aes(color = Station)) + #geom_point(aes(color = Station), fill = "white", shape = 21) +
    # geom_smooth(method = "lm", fill = "blue", alpha = 0.2) +
    facet_wrap(~Maand) +
    theme_minimal() +
    ylab(parname) +
    scale_y_log10()
}


plotTrendFyto <- function(df, statname){
  df %>% ungroup() %>%
    filter(stationname == statname) %>%
    mutate(jaar = year(datetime), maand = month(datetime)) %>%
    mutate(seizoen = ifelse(maand %in% c(4:9), "zomer", "winter")) %>%
    group_by(jaar, seizoen, parametername) %>%
    summarize(`90-perc` = quantile(value, 0.9, na.rm = T)) %>% ungroup() %>% 
    ggplot(aes(jaar, `90-perc`)) +
    geom_col(aes(fill = seizoen), position = 'dodge') +
    geom_hline(linetype = 2, color = "blue",
               data = df.fyt.groep %>% 
                 filter(stationname == statname) %>%
                 group_by(parametername) %>%
                 summarize(gemiddelde = mean(value, na.rm = T)) %>% ungroup(),
               aes(yintercept = gemiddelde)
    ) +
    facet_wrap(~ parametername, ncol = 2, scales = "free_y")
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

plotMeanMap <- function(df, parname) {
  
  values = df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    group_by(stationname) %>% summarize(mean = mean(value, na.rm = T)) %>%
    select(mean) %>% unlist() %>% unname()

  pal <- colorNumeric(viridis(n = 7),
                      domain = values
  )
  
  df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    group_by(stationname) %>% 
    summarize(mean = mean(value, na.rm = T), latitude = mean(latitude), longitude = mean(longitude)) %>%
    select(Station = stationname,
           Gemiddelde = mean,
           latitude,
           longitude
    ) %>% 
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(fillColor = ~pal(Gemiddelde), fillOpacity = 1, stroke = F) %>%
    leaflet::addLegend("topright", pal, values, opacity = 1)
}


plotMedianMap <- function(df, parname) {
  values = df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    group_by(stationname) %>% summarize(median = median(value, na.rm = T)) %>%
    select(median) %>% unlist() %>% unname()
  
  pal <- colorNumeric(viridis(n = 7),
                      domain = values
  )
  
  df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
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
}


plotLogMedianMap <- function(df, parname) {
  values = df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    group_by(stationname) %>% summarize(median = exp(median(log(value), na.rm = T))) %>%
    select(median) %>% unlist() %>% unname()
  
  pal <- colorNumeric(viridis(n = 7),
                      domain = values
  )
  
  df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
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
}
