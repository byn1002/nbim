library(shiny)
library(ggplot2)
library(readxl)
library(leaflet)
library(rnaturalearth)
library(sf)
library(dplyr)
library(tidyr)
library(plotly)
library(countrycode)

# --------------------- READ DATA ----------------------------

nbim <- read_excel("nbim-1747985187496.xls")
colnames(nbim)[1:5] <- c("Year", "Krone rate", "Return", "Inflows (after management costs)", "Total")

nbim_long <- nbim %>%
  pivot_longer(cols = -Year, names_to = "Metric", values_to = "Value")

nbim_class <- read_excel("nbim-class.xls") 
colnames(nbim_class)[1:6] <- c("Year", "Renewable", "Real_estate", "Fixed_income", "Equity", "Total_market_value")

nbim.class <- nbim_class %>%
  pivot_longer(cols = c("Renewable", "Real_estate", "Fixed_income", "Equity"),
               names_to = "Asset", values_to = "Value")


equity_usd <- read.csv("eq_by_country(USD).csv", check.names = FALSE)
equity_nok <- read.csv("eq_by_country(NOK).csv", check.names = FALSE)

equity_usd_long <- equity_usd %>%
  rename(name = Country) %>% 
  pivot_longer(cols = -name, names_to = "Year", values_to = "Equities") %>%
  mutate(Year = as.numeric(Year))

equity_usd_long$continent <- countrycode(sourcevar = equity_usd_long$name,
                                         origin = "country.name",
                                         destination = "continent")
equity_nok_long <- equity_nok %>%
  rename(name = Country) %>% 
  pivot_longer(cols = -name, names_to = "Year", values_to = "Equities") %>%
  mutate(Year = as.numeric(Year))

equity_nok_long$continent <- countrycode(sourcevar = equity_nok_long$name,
                                         origin = "country.name",
                                         destination = "continent")

eq_holding = read_excel("nbim-eq-holding.xls", col_names = TRUE)
colnames(eq_holding) <- c("Year","Europe","America, Africa, Middle East", "Asia, Oceania", "Global holding")

eq_holding_long <- eq_holding %>%
  pivot_longer(
    cols = -Year,
    names_to = "Region",
    values_to = "Holding"
  )



fi_usd <- read.csv("fi_by_country(USD).csv", check.names = FALSE)
fi_nok <- read.csv("fi_by_country(NOK).csv", check.names = FALSE)

fi_usd_long <- fi_usd %>%
  rename(name = Country) %>% 
  pivot_longer(cols = -name, names_to = "Year", values_to = "Fixed") %>%
  mutate(Year = as.numeric(Year))

fi_usd_long$continent <- countrycode(sourcevar = fi_usd_long$name,
                                     origin = "country.name",
                                     destination = "continent")

fi_nok_long <- fi_nok %>%
  rename(name = Country) %>% 
  pivot_longer(cols = -name, names_to = "Year", values_to = "Fixed") %>%
  mutate(Year = as.numeric(Year))
fi_nok_long$continent <- countrycode(sourcevar = fi_nok_long$name,
                                     origin = "country.name",
                                     destination = "continent")

fi_holding = read_excel("nbim-fi-holding.xls", col_names = TRUE)
colnames(fi_holding) <- c("Year",
                          "Government bonds",
                          "Corporate bonds", 
                          "Global holding")
fi_holding_long <- fi_holding %>%
  pivot_longer(
    cols = -Year,
    names_to = "Instrument",
    values_to = "Holding"
  )



re_usd <- read.csv("re_by_country(USD).csv", check.names = FALSE)
re_nok <- read.csv("re_by_country(NOK).csv", check.names = FALSE)

re_usd_long <- re_usd %>%
  rename(name = Country) %>% 
  pivot_longer(cols = -name, names_to = "Year", values_to = "Real") %>%
  mutate(Year = as.numeric(Year))

re_usd_long$continent <- countrycode(sourcevar = re_usd_long$name,
                                     origin = "country.name",
                                     destination = "continent")

re_nok_long <- re_nok %>%
  rename(name = Country) %>% 
  pivot_longer(cols = -name, names_to = "Year", values_to = "Real") %>%
  mutate(Year = as.numeric(Year))
re_nok_long$continent <- countrycode(sourcevar = re_nok_long$name,
                                     origin = "country.name",
                                     destination = "continent")

inf_usd <- read.csv("inf_by_country(USD).csv", check.names = FALSE)
inf_nok <- read.csv("inf_by_country(NOK).csv", check.names = FALSE)
inf_usd_long <- inf_usd %>%
  rename(name = Country) %>% 
  pivot_longer(cols = -name, names_to = "Year", values_to = "Infra") %>%
  mutate(Year = as.numeric(Year))
inf_usd_long$continent <- countrycode(sourcevar = inf_usd_long$name,
                                       origin = "country.name",
                                       destination = "continent")
inf_nok_long <- inf_nok %>%
  rename(name = Country) %>% 
  pivot_longer(cols = -name, names_to = "Year", values_to = "Infra") %>%
  mutate(Year = as.numeric(Year))
inf_nok_long$continent <- countrycode(sourcevar = inf_nok_long$name,
                                       origin = "country.name",
                                       destination = "continent")


annual_return_class<- read_excel("annual-return-by-class.xls", col_names = TRUE)
colnames(annual_return_class) <- c("Year","Equity","Fixed income","real estate","infrastructure")
annual_return_class <- annual_return_class %>%
  pivot_longer(
    cols = c("Equity", "Fixed income", "real estate", "infrastructure"),
    names_to = "Class",
    values_to = "Return"
  )

yearly_annual_return <- read_excel("yearly-annual-return.xls", col_names = TRUE)
colnames <- c("Year","Annual return","Accumulated return")
yearly_annual_return <- yearly_annual_return %>%
  setNames(colnames) %>%
  mutate(Year = as.numeric(Year))

annual_relative_return <- read_excel("annual-relative-return.xls", col_names = TRUE)
colnames <- c("Year", "Annual relative return", "Accumulated relative return")
annual_relative_return <- annual_relative_return %>%
  setNames(colnames) %>%
  mutate(Year = as.numeric(Year))


total_usd <- equity_usd_long %>%
  full_join(fi_usd_long, by = c("name", "Year")) %>%
  full_join(re_usd_long, by = c("name", "Year")) %>%
  full_join(inf_usd_long, by = c("name", "Year")) %>%
  mutate(
    Equities = replace_na(Equities, 0),
    Fixed = replace_na(Fixed, 0),
    Real = replace_na(Real, 0),
    Infra = replace_na(Infra, 0),
    Total = Equities + Fixed + Real + Infra
  )

total_nok <- equity_nok_long %>%
  full_join(fi_nok_long, by = c("name", "Year")) %>%
  full_join(re_nok_long, by = c("name", "Year")) %>%
  full_join(inf_nok_long, by = c("name", "Year")) %>%
  mutate(
    Equities = replace_na(Equities, 0),
    Fixed = replace_na(Fixed, 0),
    Real = replace_na(Real, 0),
    Infra = replace_na(Infra, 0),
    Total = Equities + Fixed + Real + Infra
  )



world <- ne_countries(returnclass = "sf")




render_world_map <- function(world, total_usd,selected_year = NULL, value_col = "Total",currency = "USD") {
 data_year <- total_usd %>% filter(Year == selected_year)
  merged <- left_join(world, data_year, by = "name")
    if (currency == "USD") {
      merged <- merged %>%
        mutate(popup_text = sprintf(
          "<b>%s</b><br/>Equity: %.2f USD<br/>Fixed income: %.2f USD<br/>Real estate: %.2f USD<br/>Infrastructure: %.2f USD",
          name, Equities, Fixed, Real, Infra
        ))
    } else {
      merged <- merged %>%
        mutate(popup_text = sprintf(
          "<b>%s</b><br/>Equity: %.2f NOK<br/>Fixed income: %.2f NOK<br/>Real estate: %.2f NOK<br/>Infrastructure: %.2f NOK",
          name, Equities, Fixed, Real, Infra
        ))
    }
  merged <- merged %>%
    mutate(
      Total = replace_na(Total, 0),
      Total_log = log10(Total + 1)
    )
  
  pal <- colorBin(
    palette = colorRampPalette(c("#F0F8FF", "#B3DDF2", "#80C1E3", "#4DA6D3", "#1E90C2", "#106BA0","#02274F"))(30),
    domain = merged$Total_log,
    bins = 30,
    na.color = "#cccccc"
  )
  
  leaflet(merged) %>%
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    addPolygons(
      fillColor = ~pal(log10(Total + 1)),
      weight = 1, color = "white", fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
      ),
      label = ~name,  # 只显示国家名
      popup = ~popup_text,  # 显示多行详情
      popupOptions = popupOptions(maxWidth = 300)
    )%>%
    
    setView(lng = 10, lat = 30, zoom = 3)
}


render_equity_map <- function(world, equity_usd_long, selected_year, value_col = "Equities",currency = "USD") {
  data_year <- equity_usd_long %>% filter(Year == selected_year)
  merged <- left_join(world, data_year, by = "name")
  merged <- merged %>%
    mutate(
      Equities = replace_na(Equities, 0),
      Equities_log = log10(Equities + 1)
    )
  merged <- merged %>%
    mutate(
      label_text = if (currency == "USD") {
        paste0(name, ": ", .data[[value_col]], " USD")
      } else {
        paste0(name, ": ", .data[[value_col]], " NOK")
      }
    )
  
  pal <- colorBin(
    palette = colorRampPalette( c("#F0F8FF", "#B3DDF2", "#80C1E3", "#4DA6D3", "#1E90C2", "#106BA0","#02274F"))(30),
    domain = merged$Equities_log,
    bins = 30,
    na.color = "#cccccc"
  )
  merged_circles <- merged %>% filter(Equities > 0)
  leaflet(merged) %>%
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    addPolygons(
      fillColor = ~pal(Equities_log),
      weight = 1, opacity = 1, color = "white", fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
      ),
      label = ~label_text,  # 显示国家名和Equities值
    ) %>%
  
    setView(lng = 10, lat = 30, zoom = 3)
}


render_fi_map <- function(world, fi_usd_long, selected_year, value_col = "Fixed",currency = "USD") {
  data_year <- fi_usd_long %>% filter(Year == selected_year)
  merged <- left_join(world, data_year, by = "name")
  merged <- merged %>%
    mutate(
      Fixed = replace_na(Fixed, 0),
      fi_log = log10(Fixed + 1)
    )
  merged <- merged %>%
    mutate(
      label_text = if (currency == "USD") {
        paste0(name, ": ", .data[[value_col]], " USD")
      } else {
        paste0(name, ": ", .data[[value_col]], " NOK")
      }
    )
  
  pal <- colorBin(
    palette = colorRampPalette( c("white","#EDF8FB", "#B2E2E2", "#66C2A4", "#2CA25F", "#00441B")
)(30),
    domain = merged$fi_log,
    bins = 30,
    na.color = "#cccccc"
  )
  
  
  leaflet(merged) %>%
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    addPolygons(
      fillColor = ~pal(fi_log),
      weight = 1, opacity = 1, color = "white", fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
      ),
      label = ~label_text,  # 显示国家名和Fixed值
    ) %>%
    
    setView(lng = 10, lat = 30, zoom = 3)
}

render_re_map <- function(world, re_usd_long,selected_year,  value_col = "Real",currency = "USD") {
  data_year <- re_usd_long %>% filter(Year == selected_year)
  merged <- left_join(world, data_year, by = "name")
  merged <- merged %>%
    mutate(
      Real = replace_na(Real, 0),
      Real_log = log10(Real + 1)
    )
  merged <- merged %>%
    mutate(
      label_text = if (currency == "USD") {
        paste0(name, ": ", .data[[value_col]], " USD")
      } else {
        paste0(name, ": ", .data[[value_col]], " NOK")
      }
    )
  pal <- colorBin(
    palette = colorRampPalette( c("#F0F8FF", "#B3DDF2", "#80C1E3", "#4DA6D3", "#1E90C2", "#106BA0","#02274F"))(30),
    domain = merged$Real_log,
    bins = 30,
    na.color = "#cccccc"
  )
  
  leaflet(merged) %>%
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    addPolygons(
      fillColor = ~pal(Real_log),
      weight = 1, opacity = 1, color = "white", fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
      ),
      label = ~label_text, 
    ) %>%
    
    setView(lng = -60, lat = 40, zoom = 3)
}


render_inf_map <- function(world, inf_usd_long, selected_year,value_col = "Infra",currency = "USD") {
  data_year <- inf_usd_long %>% filter(Year == selected_year)
  merged <- left_join(world, data_year, by = "name")
  merged <- merged %>%
    mutate(
      Infra = replace_na(Infra, 0),
      Infra_log = log10(Infra + 1)
    )
  merged <- merged %>%
    mutate(
      label_text = if (currency == "USD") {
        paste0(name, ": ", .data[[value_col]], " USD")
      } else {
        paste0(name, ": ", .data[[value_col]], " NOK")
      }
    )
  pal <- colorBin(
    palette = colorRampPalette(c("white","#EDF8FB", "#B2E2E2", "#66C2A4", "#2CA25F", "#00441B"))(30),
    domain = merged$Infra_log,
    bins = 30,
    na.color = "#cccccc"
  )
  
  leaflet(merged) %>%
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    addPolygons(
      fillColor = ~pal(Infra_log),
      weight = 1, opacity = 1, color = "white", fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
      ),
      label = ~label_text
    ) %>%
    
    setView(lng = 10, lat = 55, zoom = 4)
}



ui <- navbarPage("Investment Dashboard",
                 

tabPanel("Value Development",
         fluidPage(
           tags$head(includeCSS("style1.css")),
           
           fluidRow(
             column(
               width = 4,
               div(class = "left-panel",
                   span(
                     h1("Value Development"),
                     style = "color:#045a8d; font-style: normal;"
                   ),
                   span(tags$i(h4("Norwegian Government Pension Fund Global")), style="color:#045a8d"),
                   span(tags$i(h6("The fund is a global investor with a long-term investment horizon. 
                      The aim is to achieve the highest possible return. 
                      It is only invested abroad so that the Norwegian economy does not overheat.")), style="color:#045a8d"),
                   p(HTML("<b>Norway’s Oil and Gas Legacy</b><br>
Norway struck oil in the North Sea in 1969, transforming its economy almost overnight. 
The discovery of the Ekofisk field launched a world-class offshore industry. <br>
Today, Norway is not only a top crude oil exporter but also Europe’s <i>second-largest</i> supplier of natural gas, with revenues fueling the world’s largest sovereign wealth fund."), 
                     style = "color:#045a8d; font-style: normal;"),
                   
                   p(HTML("<b>Origin of the Fund</b><br>
Norway established its sovereign wealth fund in 1990 to manage oil revenues for future generations."), 
                     style = "color:#045a8d; font-style: normal;"),
                   p(HTML("<b>Growth</b><br>
The first capital transfer came in 1996. Since then, the fund has grown into the world’s largest, investing globally."), 
                     style = "color:#045a8d; font-style: normal;"),
                   p(HTML("<b>Purpose</b><br>
It ensures long-term stability, shielding the economy from oil price shocks and demographic shifts."), 
                     style = "color:#045a8d; font-style: normal;"),
                   
                   
                   span(h2("Select Mapping Data"),
                        style = "color:#045a8d; font-style: normal;"),
                   sliderInput("year", "Select Year:",
                               min = 1998, max = 2024,
                               value = 2016, step = 1,
                               sep = "",
                               width = "100%"),
                   
                   
                   span(h2("Value Growth"),
                   style = "color:#045a8d; font-style: normal;"),
                   
                   div(
                     style = "font-style: normal;",
                     p(HTML("Through a diversified portfolio across equity, bonds, real estate, and infrastructure, 
                            the Government Pension Fund Global aims to balance risk and return, ensuring long-term value for Norway’s future generations.")), 
                     p(HTML("<b>Equities:</b> ")),
                     p(HTML("Equities represent ownership stakes in over 8,500 companies worldwide. They account for <b>71.4%</b> of the fund’s total investments, totaling <b>14,113 billion kroner</b>.")),
                     p(HTML("<b>Fixed Income:</b> ")),
                     p(HTML("Fixed income investments are primarily bonds — loans to governments and corporations. These comprise <b>26.6%</b> of the fund’s total assets, amounting to <b>5,253 billion kroner</b>.")),
                     p(HTML("<b>Real Estate:</b> ")),
                     p(HTML("The fund invests in unlisted office buildings, retail properties in major cities, and logistics centers. Real estate makes up <b>1.8%</b> of the portfolio, valued at <b>364 billion kroner</b>.")),
                     p(HTML("<b>Infrastructure:</b> ")),
                     p(HTML("Infrastructure investments focus on renewable energy, particularly wind and solar power projects. This category represents <b>0.1%</b> of the total portfolio, with an investment of <b>25 billion kroner</b>."))
                   ),
                   
                   div(class = "plot-block",
                       sliderInput("start_year_value", "Select Start Year:",
                                   min = min(nbim_long$Year),
                                   max = max(nbim_long$Year),
                                   value = min(nbim_long$Year),
                                   step = 1,
                                   sep = "",
                                   width = "100%"),
                   ),
                   fluidRow(
                     column(12,
                            div(class = "plot-block",
                                plotlyOutput("valuePlot"))
                     )
                   ),
                   span(h3("Value development by asset class"),
                        style = "color:#045a8d; font-style: normal;"),
                   p(HTML("The fund has been invested in equities and fixed-income instruments since 1998. The first investment in unlisted real estate was made in 2011, and the first investment in unlisted renewable energy infrastructure was made in 2021.")),
                   
                   
                   fluidRow(
                     column(12,
                            div(class = "plot-block",
                                plotlyOutput("stackedInteractive", height = "280px"))
                     )
                   ),
                   
                   fluidRow(
                     column(12,
                            div(class = "plot-block",
                                plotlyOutput("investmentPie", height = "280px"))
                     )
                   ),
                   fluidRow(
                     column(12,
                            div(class = "plot-block",
                                plotlyOutput("investmentShareLine", height = "280px"))
                     )
                   ),
                   p(HTML("<b>Shift in Investment Strategy After 2008</b><br>
Before the 2008 global financial crisis, the Norwegian Government Pension Fund Global focused primarily on fixed income investments, with equities accounting for around 40% of the portfolio. <br>
However, after the crisis, Norges Bank significantly increased its global equity holdings. 
As a result, equity assets grew to represent nearly 70% of the fund’s total value."), 
                     style = "font-style: normal;"),
                   
                   
                   span(
                     h2("Holdings"),
                     style = "color:#045a8d; font-style: normal;"
                   ),
                   sliderInput("start_year_eq", "Select Start Year:",
                               min = min(eq_holding_long$Year),
                               max = max(eq_holding_long$Year),
                               value = min(eq_holding_long$Year),
                               step = 1,
                               sep = "",
                               width = "100%"),
                   span(
                     h2("Holdings in Equity Markets"),
                     style = "color:#045a8d; font-style: normal;"
                   ),
                   p(HTML("The equity investments consists of ownership shares in more than 8,500 companies world wide. On average the fund owns 1.5 percent of all listed companies.")),
                   plotlyOutput("eqHoldingPlot"),
                   p(HTML("Overall, Europe has consistently been the primary focus of the fund's equity investments, maintaining the highest share throughout the period. The holdings in America, Africa, and the Middle East, Asia and Oceania, and global equities have remained relatively similar in scale. Notably, Asia and Oceania have experienced strong growth in recent years, gradually surpassing other regions to become a more prominent component of the portfolio.")),
                   
                   span(
                     h2("Holdings in Fixed-Income Markets"),
                     style = "color:#045a8d; font-style: normal;"
                   ),
                   p(HTML("The fund's fixed-income investments are allocated 70 percent to bonds issued by governments and related institutions and 30 percent to securities issued by the corporate sector.")),
                   plotlyOutput("fiHoldingPlot"),
                   p(HTML("In the early years, the Norwegian central bank made no investments in government or corporate bonds, possibly due to a strategic focus on equity markets during the initial phase of the fund’s development or a lack of fixed-income investment mandates. Over time, however, allocations to fixed-income instruments increased steadily, with government bonds consistently representing a larger share than corporate bonds, reflecting a more conservative, low-risk strategy. By recent years, government bonds have stabilized around a higher proportion, while corporate bond holdings remain significant but slightly lower, indicating a balanced but risk-aware approach in the fixed-income market.")),
                   
                   span(
                     h2("Returns"),
                     style = "color:#045a8d; font-style: normal;"),
                     p(HTML("The fund seeks to achieve the highest possible long-term return with an acceptable risk.")),
                    
                   sliderInput("start_year_return", "Select Start Year:",
                               min = min(annual_return_class$Year),
                               max = max(annual_return_class$Year),
                               value = min(annual_return_class$Year),
                               step = 1,
                               sep = "",  # 不加千位分隔符
                               width = "100%"),
                   span(
                     h3("Yearly annual return"),
                     style = "color:#045a8d; font-style: normal;"
                   ),
                   fluidRow(
                     column(
                       12,
                       div(class = "plot-block",
                           plotlyOutput("annualVsAccumulatedPlot",height = "320px"))
                     )
                   ),
                   span(
                     h3("Annual return by asset class"),
                     style = "color:#045a8d; font-style: normal;"
                   ),
                   fluidRow(
                     column(
                       12,
                       div(class = "plot-block",
                           plotlyOutput("annualReturnBar", height = "320px"))
                     )
                   ),
                   span(
                     h2("Relative Returns"),
                     style = "color:#045a8d; font-style: normal;"),
                   p(HTML("Relative return is a measure of how well the fund's investments perform compared to the return of the benchmark index set by the Ministry of Finance.")),
                   
                   span(
                     h3("Annual relative return"),
                     style = "color:#045a8d; font-style: normal;"
                   ),
                   sliderInput("start_year_relative", "Select Start Year:",
                               min = min(annual_relative_return$Year),
                               max = max(annual_relative_return$Year),
                               value = min(annual_relative_return$Year),
                               step = 1,
                               sep = "",  # 不加千位分隔符
                               width = "100%"),
                   fluidRow(
                     column(
                       12,
                       div(class = "plot-block",
                           plotlyOutput("relativeReturnPlot", height = "320px"))
                     )
                   )
                     
                  
                   
               )
             ),
             
             # 右侧地图固定显示
             column(
               width = 8,
               div(class = "map-panel",
                   leafletOutput("backgroundMap"),
                   absolutePanel(
                     id = "controls", class = "panel panel-default",
                     bottom = 20, right = 55, width = 200, draggable = TRUE, fixed = TRUE,
                     height = "auto",
                     h5("Currency:"),
                     radioButtons("currency", NULL,
                                  choices = c("USD", "NOK"),
                                  selected = "USD",
                                  inline = TRUE)
                   )
                   
               )
             )
           )
         )
),


   
   
   
                 
   tabPanel("Equities",
      div(class = "outer",
          
          tags$head(includeCSS("styles.css")),
          
          leafletOutput("mapEquities", width = "100%", height = "100%"),
          
          absolutePanel(id = "controls", class = "panel panel-default",
                        top = 75, left = 55, width = 300, fixed = TRUE,
                        draggable = TRUE, height = "auto",
                        
                        h3("Equity Overview", align = "left"),
                        p("Most of the fund is invested in equities, which are ownership interest in about 9,000 companies."),
                        sliderInput("year", "Select Year:",
                                    min = 1998, max = 2024,
                                    value = 2016, step = 1),
                        textOutput("equity_return_text"),
                        plotlyOutput("eq_continentPie", height = "250px"),
                        p("This section provides an overview of equity investments by continent."),
                        plotlyOutput("eq_holdingbar", height = "250px"),
                        p("The values are percentage of the market value of equities 
                          in the benchmark index as at 31 December 2024.")
          ),
          absolutePanel(
            id = "controls", class = "panel panel-default",
            bottom = 20, right = 55, width = 200, draggable = TRUE, fixed = TRUE,
            height = "auto",
            
            div(style = "display: flex; align-items: center; justify-content: center; margin-top: 10px; margin-bottom: 10px;",
                tags$div(style = "
        height: 120px;
        width: 20px;
        background: linear-gradient(to top,
          #02274F,
          #106BA0,
          #1E90C2,
          #4DA6D3,
          #80C1E3,
          #B3DDF2,
          #F0F8FF
        );
        border-radius: 4px;
        margin-right: 10px;
      "),
                tags$div(style = "height: 120px; display: flex; flex-direction: column; justify-content: space-between;",
                         tags$span(style = "font-size: 12px;", "Lowest"),
                         tags$span(style = "font-size: 12px;", "Equity"),
                         tags$span(style = "font-size: 12px;", "Highest")
                )
            ),
            
            h5("Currency"),
            radioButtons("currency",NULL ,
                         choices = c("USD", "NOK"),
                         selected = "USD",
                         inline = TRUE)
          )
      )
   ),
   
   
                 
   tabPanel("Fixed Income",
      div(class = "outer",
          
          tags$head(includeCSS("styles.css")), 
          
          leafletOutput("mapFixed_Income", width = "100%", height = "100%"),
          
          absolutePanel(id = "controls", class = "panel panel-default",
                        top = 75, left = 55, width = 300, fixed = TRUE,
                        draggable = TRUE, height = "auto",
                        
                        h3("Fixed Income Overview", align = "left"),
                        p("Another part is invested in bonds, which are a type of loan to governments and companies."),
                        sliderInput("year", "Select Year:",
                                    min = 1998, max = 2024,
                                    value = 2012, step = 1),
                        textOutput("fi_return_text"),
                        plotlyOutput("fi_continentPie", height = "250px"),
                        p("This section provides an overview of fixed income investments by continent."),
                        plotlyOutput("fi_holdingbar", height = "250px"),
                        p("The values are percentages of market value of bonds 
                        in the benchmark index as at 31 December 2024.

")
          ),
          absolutePanel(
            id = "controls", class = "panel panel-default",
            bottom = 20, right = 55, width = 200, draggable = TRUE, fixed = TRUE,
            height = "auto",
            
            div(style = "display: flex; align-items: center; justify-content: center; margin-top: 10px; margin-bottom: 10px;",
                tags$div(style = "
        height: 120px;
        width: 20px;
        background: linear-gradient(to top,
          #00441B,
          #2CA25F,
          #66C2A4,
          #B2E2E2,
          #EDF8FB,
          white
        );
        border-radius: 4px;
        margin-right: 10px;
      "),
                tags$div(style = "height: 120px; display: flex; flex-direction: column; justify-content: space-between;",
                         tags$span(style = "font-size: 12px;", "Lowest"),
                         tags$span(style = "font-size: 12px;", "Fixed Income"),
                         tags$span(style = "font-size: 12px;", "Highest")
                )
            ),
            h5("Currency"),
            radioButtons("currency", NULL,
                         choices = c("USD", "NOK"),
                         selected = "USD",
                         inline = TRUE)
            
          )
      )
   ),
   
   tabPanel("Real Estate",
    div(class = "outer",
        
        tags$head(includeCSS("styles.css")),
        
        leafletOutput("mapReal", width = "100%", height = "100%"),
        
        absolutePanel(id = "controls", class = "panel panel-default",
                      top = 75, left = 55, width = 300, fixed = TRUE,
                      draggable = TRUE, height = "auto",
                      
                      h3("Real Estate Overview", align = "left"),
                      p("A smaller portion is invested in unlisted office and retail properties in major cities, and in logistics properties."),
                      sliderInput("year", "Select Year:",
                                  min = 2011, max = 2024,
                                  value = 2024, step = 1),
                      textOutput("re_return_text"),
                      p("This section provides an overview of real estate investments."),
                      plotlyOutput("re_continentPie", height = "250px")
        ),
        absolutePanel(
          id = "controls", class = "panel panel-default",
          bottom = 20, right = 55, width = 200, draggable = TRUE, fixed = TRUE,
          height = "auto",
          div(style = "display: flex; align-items: center; justify-content: center; margin-top: 10px; margin-bottom: 10px;",
              tags$div(style = "
        height: 120px;
        width: 20px;
        background: linear-gradient(to top,
          #02274F,
          #106BA0,
          #1E90C2,
          #4DA6D3,
          #80C1E3,
          #B3DDF2,
          #F0F8FF
        );
        border-radius: 4px;
        margin-right: 10px;
      "),
              tags$div(style = "height: 120px; display: flex; flex-direction: column; justify-content: space-between;",
                       tags$span(style = "font-size: 12px;", "Highest"),
                       tags$span(style = "font-size: 12px;", "Real Estate"),
                       tags$span(style = "font-size: 12px;", "Lowest")
              )
          ),
          h5("Currency"),
          radioButtons("currency", NULL,
                       choices = c("USD", "NOK"),
                       selected = "USD",
                       inline = TRUE)
        )
    )
   ),
   
   tabPanel("Infrastructure",
        div(class = "outer",
            
            tags$head(includeCSS("styles.css")),
            
            leafletOutput("mapInfra", width = "100%", height = "100%"),
            
            absolutePanel(id = "controls", class = "panel panel-default",
                          top = 75, left = 55, width = 300, fixed = TRUE,
                          draggable = TRUE, height = "auto",
                          
                          h3("Infrastructure Overview", align = "left"),
                          p("A final slice is invested in renewable energy infrastructure, mainly in wind and solar projects."),
                          sliderInput("year", "Select Year:",
                                      min = 2021, max = 2024,
                                      value = 2024, step = 1),
                          textOutput("infra_return_text"),
                          plotlyOutput("infra_continentPie", height = "300px"),
                          p(HTML("
The Norwegian sovereign wealth fund has significantly expanded its investments in infrastructure in recent years. <br>
The Netherlands has consistently received the largest share, with investments decreasing from 1.62 billion NOK in 2021 to 1.08 billion NOK in 2024. <br>
Germany, Spain, and the United Kingdom began receiving infrastructure investments from 2023 onwards. 
"))
                          
            ),
            absolutePanel(
              id = "controls", class = "panel panel-default",
              bottom = 20, right = 55, width = 200, draggable = TRUE, fixed = TRUE,
              height = "auto",
              div(style = "display: flex; align-items: center; justify-content: center; margin-top: 10px; margin-bottom: 10px;",
                  tags$div(style = "
        height: 120px;
        width: 20px;
        background: linear-gradient(to top,
          #00441B,
          #2CA25F,
          #66C2A4,
          #B2E2E2,
          #EDF8FB,
          white
        );
        border-radius: 4px;
        margin-right: 10px;
      "),
                  tags$div(style = "height: 120px; display: flex; flex-direction: column; justify-content: space-between;",
                           tags$span(style = "font-size: 12px;", "Lowest"),
                           tags$span(style = "font-size: 12px;", "Infrastructure"),
                           tags$span(style = "font-size: 12px;", "Highest")
                  )
              ),
              h5("Currency"),
              radioButtons("currency", NULL,
                           choices = c("USD", "NOK"),
                           selected = "USD",
                           inline = TRUE)
            )
   )
)
)


# ----- Server -----
server <- function(input, output, session) {
  
  
  output$backgroundMap <- renderLeaflet({
    if (input$currency == "USD") {
      render_world_map(world, total_usd, input$year,value_col = "Total","USD")
    } else {
      render_world_map(world, total_nok, input$year, value_col = "Total","NOK")
    }%>%
      setView(lng = 10, lat = 30, zoom = 2)
  })
  



  
  output$valuePlot <- renderPlotly({
    df <- nbim_long %>%
      filter(Year >= input$start_year_value) %>%
      mutate(Metric = factor(
        Metric,
        levels = c("Krone rate", "Return", "Inflows (after management costs)", "Total")
      ))
    
    
    total_df <- df %>% 
      filter(Metric == "Total") %>%
      mutate(Value = Value)  # 确保数值正确
    
    plot_ly() %>%
      add_bars(
        data = filter(df, Metric != "Total"),
        x = ~Year,
        y = ~Value,
        color = ~Metric,
        colors = c("#0072B2", "#009E73", "#F0E442"),
        text = ~paste("Year:", Year,
                      "<br>Metric:", Metric,
                      "<br>Value:", round(Value, 2), "billion kroner"),
        hoverinfo = "text",
        name = ~Metric
      ) %>%
      add_lines(
        data = total_df,
        x = ~Year,
        y = ~Value,
        name = "Total",
        line = list(color = "grey", width = 3),
        text = ~paste("Year:", Year,
                      "<br>Total Value:", round(Value, 2), "billion kroner"),
        hoverinfo = "text"
      ) %>%
      layout(
        barmode = "stack",
        title = paste0("Value Development (", input$start_year_value, "–2024)"),
        xaxis = list(
          title = "Year",
          type = "category", 
          tickangle = -45    
        ),
        yaxis = list(
          title = "Billion kroner", 
          tickformat = ",.0f"
        ),
        legend = list(
          title = list(text = "Indicator"), 
          orientation = "h", 
          x = 0.5, 
          xanchor = "center",
          y = -0.3
        ),
        margin = list(b = 100, l = 60, r = 30, t = 80),
        hovermode = "x unified"  
      )  
  })
  
  
  
  
  
  output$stackedInteractive <- renderPlotly({
    filtered_class <- nbim.class %>% filter(Year >= input$start_year_value)
    filtered_total <- nbim_class %>% filter(Year >= input$start_year_value)
    
    p <- ggplot() +
      geom_col(data = filtered_class,
               aes(x = Year, y = Value, fill = Asset,
                   text = paste0(Asset, ": ", Value, " billion NOK")),
               width = 0.8) +
      geom_line(data = filtered_total,
                aes(x = Year, y = Total_market_value, group = 1,
                    text = paste0("Total market value: ", 
                                  Total_market_value, " billion NOK")),
                color = "#00579a", size = 0.5) +
      geom_point(data = filtered_total,
                 aes(x = Year, y = Total_market_value),
                 color = "#00579a", size = 1) +
      scale_fill_manual(
        values = c(
          "Renewable" = "#0187d1",
          "Real_estate" = "#0177bd",
          "Fixed_income" = "#02a9f3",
          "Equity" = "lightblue"
        )
      ) +
      labs(title = NULL, x = "Year", y = "Billion NOK", fill = "Class") +
      theme_minimal(base_size = 12)
    
    ggplotly(p, tooltip = "text")
  })
  
  
  output$investmentPie <- renderPlotly({
    req(input$year)
    
    year_data <- nbim_class %>% filter(Year == input$year)
    
    pie_data <- data.frame(
      Asset = c("Renewable", "Real estate", "Fixed income", "Equity"),
      Value = as.numeric(year_data[, c("Renewable", "Real_estate", "Fixed_income", "Equity")])
    )
    
    plot_ly(
      data = pie_data,
      labels = ~Asset,
      values = ~Value,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "label+value+percent",
      marker = list(colors = c(
        "#0187d1",
        "#0177bd",
        "#02a9f3",
        "lightblue"
      ))
    ) %>%
      layout(
        title = paste("Investment Breakdown in", input$year),
        margin = list(t = 60, b = 40, l = 80, r = 100),
        showlegend = TRUE,
        legend = list(
          orientation = "v",  
          x = 1.05,           
          y = 1,
          xanchor = "left"
        )
      )
  })
  
  output$investmentShareLine <- renderPlotly({
    # 先过滤年份
    filtered_percent <- nbim.class %>%
      filter(Year >= input$start_year_value) %>%
      mutate(Percentage = Value / Total_market_value * 100)
    
    # 画折线图
    p <- ggplot(filtered_percent, aes(x = Year, y = Percentage, color = Asset,
                                      text = paste0(Asset, ": ", round(Percentage, 2), "%"))) +
      geom_line(aes(group = Asset), size = 1)+
      geom_point(size = 1.5) +
      scale_color_manual(
        values = c(
          "Renewable" = "#0187d1",
          "Real_estate" = "#0177bd",
          "Fixed_income" = "#02a9f3",
          "Equity" = "lightblue"
        )
      ) +
      labs(title = NULL, x = "Year", y = "Percentage", color = "Asset") +
      theme_minimal(base_size = 10)
    
    ggplotly(p, tooltip = "text")%>%
      layout(title = "Percentage of Market Value",
        margin = list(t = 60, b = 40, l = 40, r = 0))
  })
  
  
  
  output$eqHoldingPlot <- renderPlotly({
    df <- eq_holding_long %>%
      filter(Year >= input$start_year_eq) %>%
      mutate(Region = factor(Region, levels = unique(Region)))  # 保证颜色顺序一致
    
    plot_ly(
      data = df,
      x = ~Year,
      y = ~Holding,
      color = ~Region,
      colors = c("#1f77b4", "#2ca02c", "#ff7f0e", "#9467bd"),
      type = 'scatter',
      mode = 'lines+markers',
      line = list(width = 2),
      marker = list(size = 5),
      text = ~paste("Year:", Year, "<br>Region:", Region, "<br>Holding:", round(Holding, 4)),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste0("Holdings by Region (", input$start_year_eq, "–2024)"),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Holding (%)", tickformat = ",.0f"),
        legend = list(title = list(text = "Region"), orientation = "h", x = 0.1, y = -0.2),
        margin = list(b = 80)
      )
  })
  
  
  output$fiHoldingPlot <- renderPlotly({
    df <- fi_holding_long %>%
      filter(Year >= input$start_year_eq) %>%
      mutate(Instrument = factor(Instrument, levels = unique(Instrument)))
    
    plot_ly(
      data = df,
      x = ~Year,
      y = ~Holding,
      color = ~Instrument,
      colors = c("#1f77b4", "#2ca02c", "#9467bd"),  
      type = 'scatter',
      mode = 'lines+markers',
      line = list(width = 2),
      marker = list(size = 5),
      text = ~paste("Year:", Year, "<br>Instrument:", Instrument, "<br>Holding:", round(Holding * 100, 2), "%"),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste0("Holdings by Instrument (", input$start_year_eq, "–2024)"),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Holding (%)"),
        legend = list(title = list(text = "Instrument"), orientation = "h", x = 0.1, y = -0.2),
        margin = list(b = 80)
      )
  })
  
  
  
  output$annualReturnBar <- renderPlotly({
    filtered_data <- annual_return_class %>%
      filter(Year >= input$start_year_return)
    
    plot_ly(
      data = filtered_data,
      x = ~Year,
      y = ~Return,
      color = ~Class,
      colors = c(
        "Equity" = "#1f77b4",
        "Fixed income" = "#2ca02c",
        "real estate" = "#ff7f0e",
        "infrastructure" = "#9467bd"
      ),
      type = 'bar',
      text = ~paste0(Class, ": ", Return, "%"),
      hoverinfo = "text"
    ) %>%
      layout(
        barmode = "group",
        title = list(text = paste0("Annual Returns by Asset Class (", input$start_year_return, "–2024)")),
        xaxis = list(title = "Year", tickangle = -45),
        yaxis = list(title = "Return (%)"),
        legend = list(title = list(text = "Asset Class"), orientation = "h", x = 0.1, y = -0.3),
        margin = list(b = 100)
      )
  })
  output$annualVsAccumulatedPlot <- renderPlotly({
    filtered_data <- yearly_annual_return %>%
      filter(Year >= input$start_year_return)
    
    all_y_values <- c(filtered_data$`Annual return`, filtered_data$`Accumulated return`)
    y_min <- min(all_y_values, 0)
    y_max <- max(all_y_values, 0)
    
    plot_ly(data = filtered_data) %>%
      add_bars(
        x = ~Year,
        y = ~`Annual return`,
        name = "Annual return",
        marker = list(color = '#1f77b4'),
        hoverinfo = "y",
        yaxis = "y"
      ) %>%
      # Accumulated return: 折线图
      add_lines(
        x = ~Year,
        y = ~`Accumulated return`,
        name = "Accumulated return",
        line = list(color = '#ff7f0e', width = 3),
        hoverinfo = "y",
        yaxis = "y2"
      ) %>%
      layout(
        title = paste0("Annual vs. Accumulated Return (", input$start_year_return, "–2024)"),
        xaxis = list(title = "Year"),
        yaxis = list(
          title = "Annual return (%)",
          side = "left",
          range = c(y_min, y_max),
          zeroline = TRUE,
          zerolinecolor = "gray",
          zerolinewidth = 1
        ),
        yaxis2 = list(
          title = "Accumulated return (%)",
          side = "right",
          overlaying = "y",
          showgrid = FALSE,
          showline = TRUE,
          linecolor = "black",
          tickfont = list(color = "black"),
          titlefont = list(color = "black"),
          range = c(y_min, y_max),  # 关键：使用相同的范围
          zeroline = TRUE,
          zerolinecolor = "gray",
          zerolinewidth = 1
        ),
        legend = list(orientation = "h", x = 0.1, y = -0.3),
        margin = list(b = 80,r = 40)
      )
  })
  
  
  output$relativeReturnPlot <- renderPlotly({
    filtered_data <- annual_relative_return %>%
      filter(Year >= input$start_year_relative)
    
    all_vals <- c(filtered_data$`Annual relative return`, filtered_data$`Accumulated relative return`)
    y_min <- min(all_vals, 0)
    y_max <- max(all_vals, 0)
    
    plot_ly(data = filtered_data) %>%
      add_bars(
        x = ~Year,
        y = ~`Annual relative return`,
        name = "Annual relative return",
        marker = list(color = '#1f77b4'),
        hoverinfo = "y",
        yaxis = "y"
      ) %>%
      add_lines(
        x = ~Year,
        y = ~`Accumulated relative return`,
        name = "Accumulated relative return",
        line = list(color = '#ff7f0e', width = 3),
        hoverinfo = "y",
        yaxis = "y2"
      ) %>%
      layout(
        title = paste0("Relative vs. Accumulated Relative Return"),
        xaxis = list(title = "Year"),
        yaxis = list(
          title = "Annual relative return (%)",
          side = "left",
          range = c(y_min, y_max),
          zeroline = TRUE,
          zerolinecolor = "gray",
          zerolinewidth = 1
        ),
        yaxis2 = list(
          title = "Accumulated relative return (%)",
          side = "right",
          overlaying = "y",
          range = c(y_min, y_max),
          showline = TRUE,
          linecolor = "black",
          tickfont = list(color = "black"),
          titlefont = list(color = "black"),
          zeroline = TRUE,
          zerolinecolor = "gray",
          zerolinewidth = 1
        ),
        legend = list(orientation = "h", x = 0.1, y = -0.3),
        margin = list(b = 80, r = 40)
      )
  })
  

  
  output$mapEquities <- renderLeaflet({
    if (input$currency == "USD") {
      render_equity_map(world, equity_usd_long, input$year,value_col = "Equities",currency = "USD")
    } else {
      render_equity_map(world, equity_nok_long, input$year, value_col = "Equities",currency = "NOK")
    }
  })
  
  output$eq_continentPie <- renderPlotly({
    req(input$year)
    
    pie_data <- equity_usd_long %>%
      filter(Year == input$year) %>%
      group_by(continent) %>%
      summarise(total_investment = sum(Equities, na.rm = TRUE)) %>%
      filter(!is.na(continent))
    
    plot_ly(
      data = pie_data,
      labels = ~continent,
      values = ~total_investment,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "label+value+percent",
      marker = list(colors = RColorBrewer::brewer.pal(n = 4, name = "Set2"))
    ) %>%
      layout(
        title = paste("Continent Distribution in", input$year),
        margin = list(t = 80, b = 60, l = 60, r = 100), 
        showlegend = TRUE,
        legend = list(
          orientation = "v",
          x = 1.05,  
          y = 1,
          xanchor = "left"
        )
      )
  })
  
  
  output$eq_holdingbar <- renderPlotly({
    req(input$year)
    
    year_data <- eq_holding_long %>% filter(Year == input$year)
    
    plot_ly(
      data = year_data,
      x = ~Region,
      y = ~Holding,
      type = 'bar',
      color = ~Region,
      colors = "Set2",
      text = ~paste("Region:", Region, "<br>Holding:", Holding,"%"),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Holdings by Region in", input$year),
        xaxis = list(title = "Region"),
        yaxis = list(title = "Holding"),
        showlegend = FALSE
      )
  })
  
  output$equity_return_text <- renderText({
    req(input$year)
    value <- annual_return_class %>%
      filter(Year == as.numeric(input$year),, Class == "Equity") %>%
      pull(Return)
    
    if (length(value) == 0 || is.na(value)) {
      return("Annual Return (Equity): N/A")
    }
    
    paste0("Annual Return (Equity): ", round(value, 2), "%")
  })
  

  
  output$mapFixed_Income <- renderLeaflet({
    if (input$currency == "USD") {
      render_fi_map(world, fi_usd_long, input$year, value_col = "Fixed",currency = "USD")
    } else {
      render_fi_map(world, fi_nok_long, input$year, value_col = "Fixed",currency = "NOK")
    }
  })
  
  output$fi_continentPie <- renderPlotly({
    req(input$year)
    
    # 汇总每个大洲当年的投资
    pie_data <- fi_usd_long %>%
      filter(Year == input$year) %>%
      group_by(continent) %>%
      summarise(total_investment = sum(Fixed, na.rm = TRUE)) %>%
      filter(!is.na(continent))
    
    plot_ly(
      data = pie_data,
      labels = ~continent,
      values = ~total_investment,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "label+value+percent",
      marker = list(colors = RColorBrewer::brewer.pal(n = 4, name = "Set2"))
    ) %>%
      layout(
        title = paste("Continent Distribution in", input$year),
        margin = list(t = 60, b = 60, l = 65, r = 100),  
        showlegend = TRUE,
        legend = list(
          orientation = "v",  
          x = 1.05,         
          y = 1,
          xanchor = "left"
        )
      )
  })
  
  
  
  output$fi_holdingbar <- renderPlotly({
    req(input$year)
    
    year_data <- fi_holding_long %>% filter(Year == input$year)
    
    plot_ly(
      data = year_data,
      x = ~Instrument,
      y = ~Holding,
      type = 'bar',
      color = ~Instrument,
      colors = "Set2",
      hovertemplate = paste(
        "Instrument: %{x}<br>",
        "Holding: %{y}%<extra></extra>"
      )
    ) %>%
      layout(
        title = paste("Holdings by Instrument in", input$year),
        xaxis = list(title = "Instrument"),
        yaxis = list(title = "Holding"),
        showlegend = FALSE
      )
  })
  
  output$fi_return_text <- renderText({
    req(input$year)
    value <- annual_return_class %>%
      filter(Year == as.numeric(input$year),, Class == "Fixed income") %>%
      pull(Return)
    
    if (length(value) == 0 || is.na(value)) {
      return("Annual Return (Fixed Income): N/A")
    }
    
    paste0("Annual Return (Fixed Income): ", round(value, 2), "%")
  })
  
  
  
  
  
  
  output$mapReal <- renderLeaflet({
    if (input$currency == "USD") {
      render_re_map(world, re_usd_long, input$year,value_col = "Real", currency = "USD")
    } else {
      render_re_map(world, re_nok_long, input$year,value_col = "Real", currency = "NOK")
    }
  })
  
  output$re_continentPie <- renderPlotly({
    req(input$year)
    
    
    pie_data <- re_usd_long %>%
      filter(Year == input$year) %>%
      group_by(continent) %>%
      summarise(total_investment = sum(Real, na.rm = TRUE)) %>%
      filter(!is.na(continent))
    
    plot_ly(
      data = pie_data,
      labels = ~continent,
      values = ~total_investment,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "label+value+percent",
      marker = list(colors = RColorBrewer::brewer.pal(n = 4, name = "Set2"))
    ) %>%
      layout(
        title = paste("Continent Distribution in", input$year),
        margin = list(t = 80, b = 40, l = 40, r = 100), 
        showlegend = TRUE,
        legend = list(
          orientation = "v", 
          x = 1.05,           
          y = 1,
          xanchor = "left"
        )
      )
  })
  
  
  
  output$re_return_text <- renderText({
    req(input$year)
    value <- annual_return_class %>%
      filter(Year == as.numeric(input$year), Class == "real estate") %>%
      pull(Return)
    
    if (length(value) == 0 || is.na(value)) {
      return("Annual Return (Real Estate): N/A")
    }
    
    paste0("Annual Return (Real Estate): ", round(value, 2), "%")
  })
  
  
  
  output$mapInfra <- renderLeaflet({
    if (input$currency == "USD") {
      render_inf_map(world, inf_usd_long, input$year,value_col = "Infra", currency = "USD")
    } else {
      render_inf_map(world, inf_nok_long, input$year,value_col = "Infra", currency = "NOK")
    }
  })
  
  output$infra_continentPie <- renderPlotly({
    req(input$year)
    
    pie_data <- inf_usd_long %>%
      filter(Year == input$year) %>%
      group_by(continent) %>%
      summarise(total_investment = sum(Infra, na.rm = TRUE)) %>%
      filter(!is.na(continent))
    
    plot_ly(
      data = pie_data,
      labels = ~continent,
      values = ~total_investment,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "label+value+percent",
      marker = list(colors = RColorBrewer::brewer.pal(n = 4, name = "Set2"))
    ) %>%
      layout(
        title = paste("Continent Distribution in", input$year),
        margin = list(t = 30, b = 50, l = 0, r = 0),
        showlegend = TRUE
      )
  })
  
  output$infra_return_text <- renderText({
    req(input$year)
    value <- annual_return_class %>%
      filter(Year == as.numeric(input$year), Class == "infrastructure") %>%
      pull(Return)
    
    if (length(value) == 0 || is.na(value)) {
      return("Annual Return (Infrastructure): N/A")
    }
    
    paste0("Annual Return (Infrastructure): ", round(value, 2), "%")
  })
}

shinyApp(ui = ui, server = server)
