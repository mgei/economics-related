# examining the trade imbalances and graphically illustrating as networks

# Note: for years, Gross Exports is the sum or Exports and Re-exports. Now, Gross Exports include Re-Exports.  Consequently, one need to subtract Re-Exports from Exports to obtain Exports/Net Exports.
# GrossExports = Exports + ReExports 

library(tidyverse)
library(visNetwork)
library(zoo)

wto <- read_csv("merchandise_values_annual_dataset.csv")

wto %>% filter(Reporter_code == "US") %>% 
  group_by(Partner_description) %>% count()


wto %>% filter(Reporter_description == "China") %>% 
  group_by(Partner_description, Year) %>% count() %>% arrange(-n)

oecd <- read_csv("DP_LIVE_27042019202457539.csv")

oecd


tot <- read_csv("DP_LIVE_27042019203244982.csv")

tot %>% group_by(LOCATION) %>% count() %>% arrange(-n)

library(httr)
library(xml2)

mydata <- GET("https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/TISP_EBOPS2010?pid=ec9e6a54-5342-47c6-9226-0ea8ea247ec9")

mydata <- read_html("http://wits.worldbank.org/API/V1/SDMX/V21/datasource/TRN/reporter/840/partner/000/product/020110/year/2000/datatype/reported")

mydata %>% xml2::xml_structure()

mydata %>% xml2::as_list()

nomencla <- read_csv("DataJobID-1571813_1571813_tryFist.csv")


# Worldbank GDP data 
# https://data.worldbank.org/indicator/ny.gdp.mktp.cd

gdp <- read_csv("API_NY.GDP.MKTP.CD_DS2_en_csv_v2_10576830.csv", skip = 4)

gdp %>% filter(`Country Name` == "Switzerland") %>% select(`2017`) %>% pull() %>% scales::number()



# nodes ----
gdp <- read_csv("API_NY.GDP.MKTP.CD_DS2_en_csv_v2_10576830.csv", skip = 4)

# gdp %>% select(`Country Code`, `Country Name`, `2014`, `2015`, `2016`, `2017`) %>% filter(is.na(`2017`)) %>% print(n = 24)

gdp %>% 
  gather(Year, GDP, -`Country Name`, -`Country Code`, -`Indicator Name`, -`Indicator Code`) %>% 
  mutate(Year = as.integer(Year)) %>% 
  arrange(`Country Code`, Year) %>% 
  group_by(`Country Code`) %>% 
  na.locf() %>% 
  ungroup() %>% 
  filter(Year == 2017, `Country Code` != "EUN") %>% 
  select(`Country Name`, `Country Code`, Year, GDP) -> nodes_gdp

nodes_gdp %>% 
  mutate(`Country Code` = case_when(`Country Code` == "SRB" ~ "SER",
                                    `Country Code` == "MNE" ~ "MNT",
                                    `Country Code` == "ROU" ~ "ROM",
                                    `Country Code` == "SDN" ~ "SUD",
                                    `Country Code` == "TLS" ~ "TMP",
                                    T ~ `Country Code`)) -> nodes_gdp


# edges
comtrade <- read_csv("DataJobID-1571813_1571813_tryFist.csv")

comtrade %>% 
  mutate(Year = as.integer(Year)) %>% 
  filter(Year == 2017, 
         TradeFlowName == "Export",
         ReporterISO3 != "EUN", PartnerISO3 != "EUN",
         ReporterISO3 != "OAS", PartnerISO3 != "OAS") %>% 
  select(ReporterISO3, ReporterName, PartnerISO3, PartnerName,
         Year, TradeFlowName, `TradeValue in 1000 USD`) -> edges_trade

nodes_gdp %>% 
  semi_join(edges_trade, by = c("Country Code" = "ReporterISO3")) -> nodes_gdp

edges_trade %>% anti_join(nodes_gdp, by = c("ReporterISO3" = "Country Code"))

edges_trade

# country coordinates
library(rgeos)
library(rworldmap)

# # get world map
# wmap <- getMap(resolution="high")
# 
# # get centroids
# centroids <- gCentroid(wmap, byid=TRUE)
# 
# # get a data.frame with centroids
# df <- as.data.frame(centroids) %>% 
#   rownames_to_column("Country Name") %>% 
#   as_tibble() 
# 
# df %>% saveRDS("centroids.RDS")

df <- readRDS("centroids.RDS")

df <- df %>%  
  mutate(`Country Name` = case_when(`Country Name` == "United States of America" ~ "United States",
                                    T ~ `Country Name`))

# edges_trade


# small example with the 10 largest economies
nodes_gdp %>% top_n(10, GDP) %>% arrange(-GDP) -> nodes_gdp_10

nodes_gdp_10 %>% left_join(df, by = "Country Name") -> nodes_gdp_10

edges_trade %>% 
  filter(ReporterISO3 %in% (nodes_gdp_10 %>% dplyr::select(`Country Code`) %>% pull()),
         PartnerISO3  %in% (nodes_gdp_10 %>% dplyr::select(`Country Code`) %>% pull())) %>% 
  filter(`TradeValue in 1000 USD` > 0) -> edges_trade_10

nodes_gdp_10 %>% 
  rename(id = `Country Code`, label = `Country Name`, value = GDP) %>% 
  # mutate(color = c("darkred", "grey", "orange", "darkblue", "purple", 
  #                  "red", "green", "blue", "yellow", "black")) %>% 
  mutate(y = -y) -> nodes

edges_trade_10 %>% 
  rename(from = ReporterISO3, to = PartnerISO3, 
         label = `TradeValue in 1000 USD`, value = `TradeValue in 1000 USD`) -> edges


visNetwork(nodes, edges %>% filter(value > 10000000)) %>%
  visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor = 2))) %>% 
  visPhysics(enabled = T, stabilization = T)
  
country <- "CHE"

edges <- edges_trade %>% 
  filter(ReporterISO3 == country | PartnerISO3 == country) %>% 
  filter(`TradeValue in 1000 USD` > 10000) %>% # more than 10 milion USD
  rename(from = ReporterISO3, to = PartnerISO3, 
         label = `TradeValue in 1000 USD`, value = `TradeValue in 1000 USD`)

unique(c(edges %>% pull(from),
         edges %>% pull(to))) -> partners

nodes_gdp %>% 
  filter(`Country Code` %in% partners) %>% 
  rename(id = `Country Code`, value = GDP) %>% 
  mutate(label = id) -> nodes


visNetwork(nodes %>% select(id, value, label), 
           edges %>% select(from, to, value)) %>% 
  visPhysics(enabled = F)


# %>% filter(value > 10000000)) %>%
#   visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor = 2))) %>% 
#   visPhysics(enabled = T, stabilization = T)

# flags https://www.free-country-flags.com/flag_pack.php?id=1
path_to_images <- "./flags/"

nodes <- data.frame(id = 1:4, 
                    shape = c("image", "circularImage"),
                    image = rep("https://www.free-country-flags.com/countries/Angola/1/small/Angola.png", 4),
                    label = "I'm an image")

edges <- data.frame(from = c(2,4,3,3), to = c(1,2,4,2))

visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)

library(ggnetwork)
library(ggimage)

d <- data.frame(x = rnorm(10),
                y = rnorm(10),
                image = rep(paste0(path_to_images, "Austria", ".png"), 10))

# plot2
ggplot(d, aes(x, y)) + geom_image(aes(image=image), size=.05)
