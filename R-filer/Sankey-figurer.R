#CREATE A SANKEY
library(networkD3)
library(htmlwidgets)
library(htmltools)
#Create nodes: source, target and flows
nodes = data.frame("name" = 
                     c("Hushold. outflow", # Node 0
                       "Virksom. outflow", # Node 1
                       "Statens outflow", # Node 2
                       "Verdens outflow", # Node 3
                       "Bankernes outflow", # Node 4
                       "Hushold. inflow", # Node 5
                       "Virksom. inflow", # Node 6
                       "Statens inflow", # Node 7
                       "Verdens inflow", # Node 8
                       "Bankernes inflow", # Node 9
                       "Forbrug", # Node 10
                       "Skatter", # Node 11
                       "Løn", # Node 12
                       "Investeringer", # Node 13
                       "Produktionsoverskud", # Node 14
                       "Afkast", # Node 15
                       "Pensionsrettigheder",# Node 16
                       "Sociale ydelser", # Node 17
                       "Import", # Node 18
                       "Eksport")) # Node 19 
#Create the flows
links = as.data.frame(matrix(c(
  0, 10, 955867, # Each row represents a link. The first number
  0, 11, 553757, # represents the node being conntected from. 
  0, 13, 87460,
  0, 15, 43111,
  1, 11, 375821, # the second number represents the node connected to.
  1, 12, 1049056,
  1, 13, 228342,
  1, 14, 260493,
  1, 15, 234492,
  1, 18, 969507,
  2, 10, 520797,
  2, 13, 73292,
  2, 15, 31866,
  2, 17, 410430,
  3, 11, 6525,
  3, 15, 176941,
  3, 19, 1119546,
  4, 11, 10359,
  4, 13, 11373,
  4, 15, 244603,
  4, 16, 76423,
  10, 6, (955867+520797),
  11, 7, 946462,
  12, 5, 1038827,
  12, 8, 10229,
  13, 6, 400467,
  14, 5, 153111,
  14, 7, 58754,
  14, 9, 48628,
  15, 5, 126494,
  15, 6, 199671,
  15, 7, 20107,
  15, 8, 107405,
  15, 9, 277336,
  16, 5, 76423,
  17, 5, 271934,
  17, 6, 28141,
  17, 8, 34821,
  17, 9, 75533,
  18, 8, 969507,
  19, 6, 1119546),# The third number is the value of the node
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
my_color <- 'd3.scaleOrdinal() .domain([]) .range(["black","blue","green","yellow","red","purple","khaki","peru","violet","cyan","pink","orange","beige","chocolate","coral"])'

# Make the Network. I call my colour scale with the colourScale argument
#sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", colourScale=my_color)
#sankey<-
r1 <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name", colourScale=my_color,
              fontSize= 30, nodeWidth = 30)
#htmlwidgets::prependContent(sankey, htmltools::tags$h1(""))
#htmlwidgets::prependContent(sankey, htmltools::tags$h1("Flows in Denmark 2015"))
###########
##########
#############FINANCIAL TRANSACTIONS###########
nodes = data.frame("name" = 
                     c("Hushold. passiv", # Node 0
                       "Virksom. passiv", # Node 1
                       "Statens passiv", # Node 2
                       "Verdens passiv", # Node 3
                       "Bankernes passiv", # Node 4
                       "Hushold. aktiver", # Node 5
                       "Virksom. aktiver", # Node 6
                       "Verdens aktiver", # Node 7
                       "Bankernes aktiver", # Node 8
                       "Rentebærende aktiver (net)", # Node 9
                       "Aktier (net)", # Node 10
                       "Pension")) # Node 11 
#Create the flows
links = as.data.frame(matrix(c(
  0, 9, 78203, # Each row represents a link. The first number
  1, 9, 36569, # the second number represents the node connected to.
  2, 9, 35609,
  3, 9, 14716,
  4, 9, 13457,
  3, 10, 165767,
  4, 10, 40004,
  4, 11, 58624,
  9, 8, 86896,
  9, 8, 78203,
  9, 5, 13457,
  10, 5, 37939,
  10, 6, 167832,
  11, 5, 56535,
  11, 7, 2172),# The third number is the value of the node
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
my_color <- 'd3.scaleOrdinal() .domain([]) .range(["black","blue","green","yellow","red","purple","khaki","peru","violet","cyan","pink","orange","beige"])'

# Make the Network. I call my colour scale with the colourScale argument
#sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value", NodeID = "name", colourScale=my_color)
#sankey<-
r2 <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name", colourScale=my_color,
              fontSize= 30, nodeWidth = 30)
