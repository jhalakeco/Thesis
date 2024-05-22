# Preparation ----
## Loading Libraries ----

library(tidyverse)
library(tidygraph)
library(writexl)
library(igraph)
library(stringr)
library(ggthemes)
library(ggraph)

options(scipen = 99)

## Setting up WD ----
setwd("~/Documents/Study/FSU MSc/Thesis/OECD_REGPAT_202208")
## Functions ----

### Most freq country function for normal network----
get_most_frequent_country <- function(node_name) {
  edges <- E(n1_net)[.inc(node_name)]
  country_codes <- edges$ctry_code
  if (length(country_codes) == 0) {
    return(NA)  # Return NA if no edges are connected to the node
  }
  most_frequent_country <- names(sort(table(country_codes), decreasing = TRUE))[1]
  return(most_frequent_country)
}

### Most freq country function for citation network ----
ncit_get_most_frequent_country <- function(node_name, graph) {
  edges <- E(graph)[.inc(node_name)]
  country_codes <- edges$ctry_code
  
  if (length(country_codes) == 0) {
    return(NA)
  }
  most_frequent_country <- names(sort(table(country_codes), decreasing = TRUE))[1]
  return(most_frequent_country)
}


### Defining igraph object function ----
create_grpah <- function(data, country_code, column_1 = "inv_name", column_2 = "appln_id") {
  filtered_data <- data %>%
    filter(ctry_code == country_code) %>% 
    select(!!as.name(column_1), !!as.name(column_2))
  
  two_mode_table <- table(filtered_data)
  adjacency_matrix <- two_mode_table %*% t(two_mode_table)
  diag(adjacency_matrix) <- 0
  graph_object <- graph.adjacency(adjacency_matrix, mode = "undirected", weight = TRUE)
  return(graph_object)
}

### defining color palettes ----
tableau_10 <- setNames(c("#d62728", "#7f7f7f", "#59A14F",
                         "#EDC948", "#1f77b4"),
                       c("DE", "NL", "FR", "GB", "IT"))


# Data Preparation ----
##Loading .txt files ----
EPO_Inv <- read.csv("202308_EPO_inv_reg.txt", header = TRUE, sep = "|")
EPO_IPC <- read.csv("202308_EPO_IPC.txt", header = TRUE, sep = "|")
EPO_Cit <- read.csv("202308_EPO_CITATIONS.txt", header = TRUE, sep = "|")

## Data Cleaning ----
### EPO Inv ----
colnames(EPO_Inv)
EPO_Inv_EU <- EPO_Inv %>% 
  select(appln_id, inv_name, city, ctry_code) %>%
  filter(ctry_code == c("DE", "SE", "FI", "FR", "GB"))

### EPO IPC ----
EPO_IPC_EU <- EPO_IPC %>% 
  select(appln_id, app_year, IPC)

### Saving data in r as individual data ----
save(EPO_Inv_EU, file = "EPO_Inv_EU23.rdb")
save(EPO_IPC_EU, file = "EPO_IPC_EU23.rdb")
save(EPO_Cit, file = "EPO_Cit_EU23.rdb")

### Loading data ----

load("j_epo.rdb")
load("EPO_Cit_EU23.rdb")

## EPO IPC data cleaning ----
EPO_IPC_EU <- EPO_IPC_EU %>%
  filter(substr(IPC, 1, 4) == "F03D")  # only filtering IPC class F03D

## Joining the datasets ----
joined_IPC_Inv_EU <- right_join(EPO_Inv_EU, EPO_IPC_EU, by = "appln_id", relationship = "many-to-many")


joined_IPC_Inv_EU <- joined_IPC_Inv_EU %>%
  mutate(city = replace(city, city == "", NA)) %>% # making the empty values NA
  mutate(city = tolower(city))

## Fixing city names----
unique(j_epo$city[grep("^Mu",
                       j_epo$city, ignore.case = TRUE)]) # checking all the values first
unique(j_epo$city[grep("^Han",
                       j_epo$city, ignore.case = TRUE)]) # checking all the values first

j_epo$city <- tolower(j_epo$city) # renaming all the city names into lower case

mun_pattern <- "muenchen|munich"
han_pattern <- "hannover"

j_epo$city <- gsub(mun_pattern, "munchen", j_epo$city) #changing all the values
j_epo$city <- gsub(han_pattern, "hanover", j_epo$city) #changing all the values

j_epo$city <- ifelse(j_epo$city == "beijing", "bremen", j_epo$city)
j_epo$city <- ifelse(j_epo$city == "rioja", "hamburg", j_epo$city)
j_epo$city <- ifelse(j_epo$city == "quebec", "kleinniedesheim", j_epo$city)
j_epo$city <- ifelse(j_epo$city == "malaga", "aurich", j_epo$city)
j_epo$city <- ifelse(j_epo$city == "granada", "munchen", j_epo$city)
j_epo$city <- ifelse(j_epo$city == "hubei", "staffhorst", j_epo$city)
j_epo$city <- ifelse(j_epo$city == "vancouver", "hamburg", j_epo$city)
j_epo$city <- ifelse(j_epo$city == "high street", "coriano", j_epo$city)


sum(duplicated(j_epo)) # checking if there is any duplicate values

# dropping rows with no city names
j_epo <- (j_epo %>%
           mutate(city = replace(city, city == "", NA)))#making the empty values NA
j_epo <- na.omit(j_epo)

sum(duplicated(j_epo)) # checking again after cleaning NAs if there is any duplicate values

# All the patent affiliated cities
all_cities <- j_epo %>%
  group_by(city) %>%          # Group by the 'city' variable
  summarize(patents = n()) %>%  # Count the occurrences of each 'city'
  arrange(desc(patents))
dim(j_epo)


# writing this dataframe as a text file 
write.csv(all_cities, "all_cities.csv", row.names = FALSE)

write.csv(EPO_Inv_EU, "EPO_Inv_EU.csv")
write.csv(EPO_IPC_EU, "EPO_IPC_EU.csv")

## Parsing inventor data ----
j_epo_parsed <- j_epo %>%
  select(-IPC)

dim(j_epo_parsed)

# Convert inventor names to lowercase
j_epo_parsed$inv_name <- tolower(j_epo_parsed$inv_name)

# Define a list of patterns to remove
patterns_to_remove <- c(
  "dr\\.-ing\\.-habil\\.",
  "c/o",
  "prof\\. ",
  "dr\\. ",
  "prof\\. dr\\. ",
  "-ing",
  "dipl\\.-wirt\\.-ing\\.",
  "dipl\\.",
  "dr\\.-ing\\.",
  "prof\\. dr\\.-ing\\.",
  "dipl\\.-ing\\.",
  "prof\\.",
  "-wirt\\$",
  " -wirt$",
  " ing\\.$",
  " dr\\.$", # Remove " dr." at the end
  "dr\\.-ing\\.$",
  "ing\\. grad\\.",
  " dr\\.\\.",
  "[,\\.]+$"
)
# Remove specified patterns from inventor names
for (pattern in patterns_to_remove) {
  j_epo_parsed$inv_name <- str_replace_all(j_epo_parsed$inv_name, pattern, "")
}

# Remove NAs
j_epo_parsed <- na.omit(j_epo_parsed)

# checking inventor's names and length
unique_inv_names <- as.list(sort(unique(j_epo_parsed$inv_name)), decreasing = TRUE)
length(unique_inv_names)

## Fixing inventor names----
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "barlow, nicolas dudley", "barlow, nicholas dudley", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "dumnov, daniil sergeevich", "dumnov, daniil", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "jannsen, willhelm", "jansen, wilhelm", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "kalén, hans", "kalen, hans", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "kassen, dirk ", "kassen, dirk", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "riesberg, andré", "riesberg, andre", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "scholte-wassink, hartmut a", "scholte-wassink, hartmut", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "scholte-wassink, hartmut andreas", "scholte-wassink, hartmut", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "schulten, cristoph", "schulten, christoph", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "wagner, juergen", "wagner, jürgen", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "wickstroem, anders", "wickström, anders", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "winkelmann, joerg", "winkelmann, jörg", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "wobben aloys", "wobben, aloys", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "aloys wobben", "wobben, aloys", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "zeller, lenz simon", "zeller, lenz", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "zuse, konrad, . mult.e.h., mult. rer. nat. h.c., dr.tech. h.c., h.c. sc.techn", "zuse, konrad", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "balz, willi, -wirt", "balz, willi", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "heusser, martin, . univ", "heusser, martin", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "biasiotto, marco,  strada torino", "biasiotto, marco", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "bergfelder, jürgen prof-.dr.ing", "bergfelder, jürgen", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "nies, jacob j", "nies, jacob johannes", j_epo_parsed$inv_name)
j_epo_parsed$inv_name <- ifelse(j_epo_parsed$inv_name == "nies, jacob", "nies, jacob johannes", j_epo_parsed$inv_name)





# again checking the corrected name length
unique_inv_names <- as.list(sort(unique(j_epo_parsed$inv_name)), decreasing = TRUE)
length(unique_inv_names) # total number of inventors (all the clean names with at least city entries)

length(unique(j_epo_parsed$city)) # umber of total cities

# Merging patent citation data frame----

f03d_cit_eu <- j_epo_parsed %>%
  select(appln_id, city, ctry_code, inv_name) # keeping inventor name and their cities

f03d_cit_eu <- merge(f03d_cit_eu, EPO_Cit, by.x = "appln_id", by.y = "Citing_appln_id") # filtering only the f03d patents resembling to joined_Inv_EU_parsed

## Removing NA values ----
f03d_cit_eu <- f03d_cit_eu %>% 
  mutate(Cited_pub_nbr = replace(Cited_pub_nbr, Cited_pub_nbr == "", NA)) %>% 
  filter(!is.na(Cited_pub_nbr))


# removing all the big rdb files
rm(EPO_Cit)

# Inventors by country ----
f03d_inv <- j_epo_parsed %>% 
  group_by(ctry_code) %>% 
  summarize(patents = n()) %>%
  arrange(desc(patents))

sum(f03d_inv$patents)

write_xlsx(f03d_inv,"pat_countries.xlsx")

## Bar plot of country-wise inventors ----
par(mar= c(0,0,1,0))
ggplot(f03d_inv, aes(x = reorder(ctry_code, -patents), y = patents, fill = ctry_code)) +
  geom_col(width = .5) +
  labs(
    title = "F03D Patent Count by EU Countries",
    y = "Number of Patents"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  #axis.title.y = element_blank()) +
  #coord_flip() +  # Flip the coordinates for horizontal bars and descending order
  geom_text(aes(label = patents, vjust = -.3)) +
  scale_x_discrete(labels = c("Germany", "Great Britain", "France", "Netherlands","Italy")) +
  scale_fill_manual(values = tableau_10)

# Number of patents over time ----
yearly_patents <- j_epo %>%
  select(appln_id, app_year) %>% 
  group_by(app_year) %>% 
  summarise(patents=n())

length(yearly_patents$patents) # for how many year's of data we have
sum(yearly_patents$patents) # checking total number of patents
length(j_epo_parsed$appln_id) # checking total number of patents (application made)

## Graph of yearly patents ----
ggplot(yearly_patents, aes(app_year, patents)) +
  geom_line(color = "#4285F4", linewidth = 1) +
  geom_point(color = "#DB4437", size = 1.5) + 
  labs(
    #title = "Yearly Patent Applications",
    y = "Number of Patents",
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "none"  # Remove legend
  )
# number of patents grouped by the inventors and their countries
inv_pat_count <- j_epo_parsed %>%
  select(inv_name,ctry_code) %>% 
  group_by(inv_name, ctry_code) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Distribution of the inventor's patents application ----
ggplot(inv_pat_count, aes(x = count)) +
  geom_histogram(binwidth = .75, fill = "#4285F4", color = "#666666") +
  labs(title = "Distribution of Inventor Patent Counts",
       y = "Frequency") +
  stat_bin(binwidth = 1,
           geom = "text",
           aes(label=ifelse (..count.. > 92 |
                             ..count.. >= 45,  ..count.., "")), color = "#DB4437", vjust=-1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank()) +
  #scale_x_continuous(breaks = seq(0, max(inv_pat_count$count), by = 10)) +
  xlim(0, 100)


write_xlsx(inv_pat_count, "pat_inv_hist.xlsx")

#summary statistic of the inventor's data
summary(inv_pat_count)

# Numbers of unique cities and inventors----
length(unique(j_epo_parsed$inv_name)) # total number of inventors
length(unique(j_epo_parsed$city)) # total number of cities


length(inv_pat_count$inv_name)

inv_pat_count[duplicated(inv_pat_count$inv_name), ] ##### IMPORTANT ####

# Top 10 applicants ----
top_10_applicants <- j_epo_parsed %>%
  group_by(inv_name, ctry_code) %>%
  summarise(patents = n(), .groups = 'drop') %>%
  top_n(10, patents) %>%
  arrange(desc(patents))

# Top 10 cities ----
top_10_cities <- j_epo_parsed %>% 
  group_by(city, ctry_code) %>%
  summarise(patents = n(), .groups = 'drop') %>%
  top_n(10, patents) %>%
  arrange(desc(patents))

write_xlsx(top_10_cities, "top_10_cities.xlsx")

# Networks, graphs and measurements ----
## N1 All the inventors of all the cities ----
n1_net <- (j_epo_parsed %>% 
             select(inv_name, appln_id, ctry_code))

n1_net <- graph_from_data_frame(n1_net, directed = F)


# Get the most frequent country code for each node
n1_node_countries <- sapply(V(n1_net)$name, get_most_frequent_country, USE.NAMES = FALSE)


# Convert the list to a vector
n1_node_countries <- unlist(n1_node_countries)

# Checking if there are any country codes not covered in palette
missing_countries <- setdiff(n1_node_countries, names(tableau_10))
if(length(missing_countries) > 0) {
  warning("Missing colors for countries: ", paste(missing_countries, collapse = ", "))
}

n1_vertex_colors <- ifelse(n1_node_countries %in% names(tableau_10), #coloring the nodes
                           tableau_10[n1_node_countries],
                           "gray") # any missing country will get a default gray color 

V(n1_net)$color <- n1_vertex_colors # applying the colors to the nodes

### Plotting ----
set.seed(100)
par(mar = c(0, 0, 2, 0))
plot(n1_net,
     vertex.label = NA,
     vertex.size = 2,
     vertex.frame.width = .5,
     vertex.frame.color = "black",
     vertex.color = V(n1_net)$color) # Apply the colors from the Tableau palette
     #main = "N1. Network of all the Inventors")

legend("top",
       legend = names(tableau_10),
       col = tableau_10,
       pch = 19,
       cex = 0.8,
       horiz = TRUE)


### N1 Measurements ---- 
#### N1 Graph density ----
graph.density(n1_net)
#### N1 Degree Centrality----
max(degree(n1_net,
           v = V(n1_net),
           mode = c("all", "out", "in", "total"),
           loops = TRUE, normalized = FALSE))


## N2 City network (Patent application and cities) ----
n2_city_all <- (j_epo_parsed %>% 
                  select(city, appln_id))
n2_city_all_2mode <- table(n2_city_all)
n2_city_all_adj <- n2_city_all_2mode %*% t(n2_city_all_2mode)
diag(n2_city_all_adj) <-0
n2_city_all_ig <- graph.adjacency(n2_city_all_adj, mode = "undirected", weight = TRUE)
vcount(n2_city_all_ig)


### N2 plotting----
set.seed(10120)
par(mar=c(0,0,2,0))
plot(n2_city_all_ig,
     vertex.label=NA,
     vertex.size=degree(n2_city_all_ig)*.5,
     vertex.frame.width=.5,
     vertex.frame.color="black",
     vertex.color=V(n2_city_all_ig),
     edge.width = edge_betweenness(n2_city_all_ig)*.002)
     #main="N2. All city's network")


### N2 Measurements ---- 
#### N2 Graph density ----
graph.density(n2_city_all_ig)
#### N2 Degree Centrality----
max(degree(n2_city_all_ig,
           v = V(n2_city_all_ig),
           mode = c("all", "out", "in", "total"),
           loops = TRUE, normalized = FALSE))

## N2 without isolated nodes ----
isolated_n2 <- V(n2_city_all_ig)[degree(n2_city_all_ig) == 0] # filtering the isolated nodes from the network
n2_no_isolated_ig <- delete_vertices(n2_city_all_ig, isolated_n2) # making a new net obj without isolated nodes
vcount(n2_no_isolated_ig)


set.seed(10120)
par(mar=c(0,0,2,0))
plot(n2_no_isolated_ig,
     vertex.label=ifelse(degree(n2_no_isolated_ig)> 7, V(n2_no_isolated_ig)$name, NA),
     vertex.size=degree(n2_no_isolated_ig)*.5,
     vertex.frame.width=.5,
     vertex.label.font = 2,
     vertex.frame.color="black",
     vertex.color=V(n2_city_all_ig),
     edge.width = edge_betweenness(n2_no_isolated_ig)*.004)
#main="N2. All city's network")


## N3 city and inventor network ----

# setting up the data frame (appln_id and ctry_code)
n3_city_inv <- (j_epo_parsed %>% 
                  select(city, inv_name))
n3_city_inv_2mode <- table(n3_city_inv)
n3_city_inv_adj <- n3_city_inv_2mode %*% t(n3_city_inv_2mode)
diag(n3_city_inv_adj) <-0
n3_city_inv_ig <- graph.adjacency(n3_city_inv_adj, mode = "undirected", weight = TRUE)

#color codings
city_country_color <- j_epo_parsed$ctry_code[match(V(n3_city_inv_ig)$name, j_epo_parsed$city)] # matching the corresponding city colors with country colors 
city_color <- sapply(city_country_color, function(ctry_code) tableau_10[ctry_code]) # applying for all the cities


#### N3 Plotting with isolated nodes ----
set.seed(100)
par(mar=c(0,0,2,0))
plot(n3_city_inv_ig,
     vertex.label=NA,
     vertex.size= degree(n3_city_inv_ig)*.8,
     vertex.frame.color= "black",
     vertex.frame.width = .5,
     vertex.color = city_color,
     edge.width = edge_betweenness(n3_city_inv_ig)*.02)
     #main="N3. Network of the inventors of all cities")

legend("top",
       legend = names(tableau_10),
       col = tableau_10,
       pch = 19,
       cex = 0.8,
       horiz = TRUE)

vcount(n3_city_inv_ig)
#### N3 Measurements ---- 
##### N3 Graph density ----
graph.density(n3_city_inv_ig)
##### N3 Degree Centrality----
max(degree(n3_city_inv_ig,
           v = V(n3_city_inv_ig),
           mode = c("all", "out", "in", "total"),
           loops = TRUE, normalized = FALSE))


#### N3 without isolated nodes ----
isolated_n3 <- V(n3_city_inv_ig)[degree(n3_city_inv_ig) == 0] # filtering the isolated nodes from the network
n3_no_isolated_ig <- delete_vertices(n3_city_inv_ig, isolated_n3) # making a new net obj without isolated nodes
vcount(n3_no_isolated_ig)

# color codings 
city_country_no_isolated <- j_epo_parsed$ctry_code[match(V(n3_no_isolated_ig)$name, j_epo_parsed$city)]
city_color_no_isolated <- sapply(city_country_no_isolated, function(ctry_code) tableau_10[ctry_code])


##### N3 Plotting without isolated nodes----
set.seed(1023438)
par(mar=c(0,0,2,0))
plot(n3_no_isolated_ig,
     vertex.size=degree(n3_no_isolated_ig)*1.3,
     vertex.frame.color="black",
     vertex.color = city_color_no_isolated,
     vertex.label= ifelse(degree(n3_no_isolated_ig)> 3, V(n3_no_isolated_ig)$name, NA),
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.label.font = 2,
     vertex.label.dist = .5,
     edge.width = .8,
     #main="N3. Network of the inventors of all cities\n(without isolated nodes)",
     layout = layout_with_dh(n3_no_isolated_ig))

legend("top",
       legend = names(tableau_10),
       col = tableau_10,
       pch = 19,
       cex = 0.6,
       horiz = TRUE)

#### Number of cities without any connections
vcount(n3_city_inv_ig) - vcount(n3_no_isolated_ig) # cities with no connections

#Country-application wise network----
## Germany ----
### N4.a German Inventor's network ----
n4.a_de <- (j_epo_parsed %>%
              filter(ctry_code=="DE") %>% 
              select(inv_name, appln_id))
n4.a_de_2mode <- table(n4.a_de)
n4.a_de_adj <- n4.a_de_2mode %*% t(n4.a_de_2mode)
diag(n4.a_de_adj) <-0
n4.a_de_ig <- graph.adjacency(n4.a_de_adj, mode = "undirected", weight = TRUE)

### N4.a plotting ----
set.seed(100)
par(mar=c(0,0,2,0))
plot(n4.a_de_ig,
     vertex.label = ifelse(degree(n4.a_de_ig)> 10, V(n4.a_de_ig)$name, NA),
     vertex.label.cex = .5,
     vertex.label.dist = .5,
     vertex.size= degree(n4.a_de_ig)*.8,
     vertex.color = "#d62728",
     vertex.frame.color="black",
     vertex.frame.width=0.5,
     edge.width = 1)
     #main="N4.a German Inventor's Network")

### N4.a Network Measurements ----
#### Total Node Count ----
vcount(n4.a_de_ig) # number of German inventors

#### N4.a Graph density ----
graph.density(n4.a_de_ig)
#### Degree Centrality ----
max(degree(n4.a_de_ig, v = V(n4.a_de_ig), 
           mode = c("all", "out", "in", "total"), 
           loops = TRUE, normalized = FALSE))


### N4.b German city network ----
n4.b_de <- (j_epo_parsed %>%
              filter(ctry_code=="DE") %>% 
              select(city, appln_id))
n4.b_de_2mode <- table(n4.b_de)
n4.b_de_adj <- n4.b_de_2mode %*% t(n4.b_de_2mode)
diag(n4.b_de_adj) <-0
n4.b_de_ig <- graph.adjacency(n4.b_de_adj, mode = "undirected", weight = TRUE)

### N4.b with isolated nodes ----
set.seed(11010)
par(mar=c(0,0,2,0))
plot(n4.b_de_ig,
     vertex.label = ifelse(degree(n4.b_de_ig) > 15, V(n4.b_de_ig)$name, NA),
     vertex.label.cex = .9,
     vertex.label.dist = 1,
     vertex.label.font = 2,
     vertex.size=degree(n4.b_de_ig)*.5,
     vertex.color = "#d62728",
     vertex.frame.color="black",
     edge.width = 1)
     #main="N4.b German City Network of Inventor's")

### N4.b without isolated nodes ----

isolated_n4.b_de_ig <- V(n4.b_de_ig)[degree(n4.b_de_ig) == 0] # filtering the isolated nodes from the network
n4.b_de_no_isolated_ig <- delete_vertices(n4.b_de_ig, isolated_n4.b_de_ig) # making a new net obj without isolated nodes

set.seed(100)
par(mar=c(0,0,2,0))
plot(n4.b_de_no_isolated_ig,
     vertex.size=degree(n4.b_de_no_isolated_ig)*.5,
     vertex.frame.color="black",
     vertex.color = "#d62728",
     vertex.label=ifelse(degree(n4.b_de_no_isolated_ig) > 7, V(n4.b_de_no_isolated_ig)$name, NA),
     vertex.label.cex = .9,
     vertex.label.font = 2,
     vertex.label.dist = .2,
     edge.width = 1,
     #main="N4.b German city network Without Isolated nodes",
     layout = layout_nicely(n4.b_de_no_isolated_ig))



## Great Britain ----
### N5.a GB inventor's network ----
n5.a_gb <- (j_epo_parsed %>%
              filter(ctry_code=="GB") %>% 
              select(inv_name, appln_id))
n5.a_gb_2mode <- table(n5.a_gb)
n5.a_gb_adj <- n5.a_gb_2mode %*% t(n5.a_gb_2mode)
diag(n5.a_gb_adj) <-0
n5.a_gb_ig <- graph.adjacency(n5.a_gb_adj, mode = "undirected", weight = TRUE)

### N5.a plotting ----
set.seed(11320)
par(mar=c(0,0,2,0))
plot(n5.a_gb_ig,
     vertex.label = ifelse(degree(n5.a_gb_ig)> 5, V(n5.a_gb_ig)$name, NA),
     vertex.label.cex = .9,
     vertex.label.dist = .5,
     vertex.size=degree(n5.a_gb_ig)*.8,
     vertex.color = "#EDC948",
     vertex.label.font = 2,
     vertex.frame.color="black",
     edge.width = 1)
     #main="N5.a GB Inventor's Network")
### N5.a Network Measurements ----
#### Total Node Count ----
vcount(n5.a_gb_ig) # number of british inventors
##### N5.a Graph density ----
graph.density(n5.a_gb_ig)
#### Degree Centrality ----
max(degree(n5.a_gb_ig, v = V(n5.a_gb_ig), 
           mode = c("all", "out", "in", "total"), 
           loops = TRUE, normalized = FALSE))


### N5.b GB city network ----
n5.b_gb_ig <- create_grpah(j_epo_parsed,"GB","city","appln_id")
n5.b_gb <- j_epo_parsed %>% 
  filter(ctry_code=="GB") %>% 
  select(city,appln_id)
n5.b_gb_2mode <- table(n5.b_gb)
n5.b_gb_adj <- n5.b_gb_2mode %*% t(n5.b_gb_2mode)
diag(n5.b_gb_adj) <-0
n5.b_gb_ig <- graph.adjacency(n5.b_gb_adj, mode = "undirected", weight = TRUE)

### N5.b plotting ----
set.seed(102340)
par(mar=c(0,0,2,0))
plot(n5.b_gb_ig,
     vertex.label = ifelse(degree(n5.b_gb_ig)> 1, V(n5.b_gb_ig)$name, NA),
     vertex.label.cex = .7,
     vertex.label.dist = .5,
     vertex.label.font = 2,
     vertex.size=degree(n5.b_gb_ig)*1.5,
     vertex.color = "#EDC948",
     vertex.frame.color="black",
     edge.width = 1,
     layout = layout_nicely(n5.b_gb_ig))
     #main="N5.b GB City Network of Inventor's")

### N5.b without isolated nodes ----
isolated_n5.b_gb_ig <- V(n5.b_gb_ig)[degree(n5.b_gb_ig) == 0] # filtering the isolated nodes from the network
n5.b_gb_no_isolated_ig <- delete_vertices(n5.b_gb_ig, isolated_n5.b_gb_ig) # making a new net obj without isolated nodes

set.seed(1021310)
par(mar=c(0,0,2,0))
plot(n5.b_gb_no_isolated_ig,
     vertex.size=degree(n5.b_gb_no_isolated_ig)*2,
     vertex.frame.color="black",
     vertex.color = "#EDC948",
     vertex.label=ifelse(degree(n5.b_gb_no_isolated_ig) > 1, V(n5.b_gb_no_isolated_ig)$name, NA),
     vertex.label.cex = .9,
     vertex.label.font = 2,
     vertex.label.dist = .2,
     edge.width = 1,
     #main="N4.b GB city network Without Isolated Nodes",
     layout = layout_nicely(n5.b_gb_no_isolated_ig))


## France ----
### N6.a French Inventor's network ----
n6.a_fr_ig <- create_grpah(j_epo_parsed,"FR","inv_name","appln_id")

### N6.a plotting ----
set.seed(10110)
par(mar=c(0,0,2,0))
plot(n6.a_fr_ig,
     vertex.label = ifelse(degree(n6.a_fr_ig)> 3, V(n6.a_fr_ig)$name, NA),
     vertex.label.cex = .9,
     vertex.label.dist = .5,
     vertex.size=degree(n6.a_fr_ig)*1.5,
     vertex.color = "#59A14F",
     vertex.label.font = 2,
     vertex.frame.color="black",
     edge.width = 1)
     #main="N6.a French Inventor's Network")


### N6.a Network Measurements ----
#### Total Node Count ----
vcount(n6.a_fr_ig) # number of british inventors
##### N6.a Graph density ----
graph.density(n6.a_fr_ig)
#### Degree Centrality ----
max(degree(n6.a_fr_ig, v = V(n6.a_fr_ig), 
           mode = c("all", "out", "in", "total"), 
           loops = TRUE, normalized = FALSE))



### N6.b French city network ----
n6.b_fr_ig <- create_grpah(j_epo_parsed,"FR","city","appln_id")

### n6.b plotting ----
set.seed(1010)
par(mar=c(0,0,2,0))
plot(n6.b_fr_ig,
     vertex.label = ifelse(degree(n6.b_fr_ig)> 2, V(n6.b_fr_ig)$name, NA),
     vertex.label.cex = .9,
     vertex.label.dist = .5,
     vertex.size=degree(n6.b_fr_ig)*1.5,
     vertex.color = "#59A14F",
     vertex.frame.color="black",
     vertex.label.font = 2,
     edge.width = 1)
     #main="N6.b French City Network of Inventors")


### N6.b without isolated nodes ----
isolated_n6.b_fr_ig <- V(n6.b_fr_ig)[degree(n6.b_fr_ig) == 0] # filtering the isolated nodes from the network
n6.b_fr_no_isolated_ig <- delete_vertices(n6.b_fr_ig, isolated_n6.b_fr_ig) # making a new net obj without isolated nodes

set.seed(1021310)
par(mar=c(0,0,2,0))
plot(n6.b_fr_no_isolated_ig,
     vertex.size=degree(n6.b_fr_no_isolated_ig)*2,
     vertex.frame.color="black",
     vertex.color = "#59A14F",
     vertex.label=ifelse(degree(n6.b_fr_no_isolated_ig) > 2, V(n6.b_fr_no_isolated_ig)$name, NA),
     vertex.label.cex = .9,
     vertex.label.font = 2,
     vertex.label.dist = .2,
     edge.width = 1,
     layout = layout_nicely(n6.b_fr_no_isolated_ig))
     #main="N4.b French city network Without Isolated Nodes",
     


## Netherlands ----
### N7.a Dutch Inventor's Network ----
n7.a_nl_ig <- create_grpah(j_epo_parsed,"NL","inv_name","appln_id")

### N7.a plotting ----
set.seed(1023202)
par(mar=c(0,0,2,0))
plot(n7.a_nl_ig,
     vertex.label = ifelse(degree(n7.a_nl_ig)> 3, V(n7.a_nl_ig)$name, NA),
     vertex.label.cex = .9,
     vertex.label.dist = .5,
     vertex.size=degree(n7.a_nl_ig)*3,
     vertex.color = "#7f7f7f",
     vertex.label.font = 2,
     vertex.frame.color="black",
     vertex.frame.width= .7,
     edge.width = 1)
     #main="N7.a Dutch Inventor's Network")


### N7.a Network Measurements ----
#### Total Node Count ----
vcount(n7.a_nl_ig) # number of Dutch inventors
##### N7.a Graph density ----
graph.density(n7.a_nl_ig)
#### Degree Centrality ----
max(degree(n7.a_nl_ig, v = V(n7.a_nl_ig), 
           mode = c("all", "out", "in", "total"), 
           loops = TRUE, normalized = FALSE))



### N7.b Dutch City Network ----
n7.b_nl_ig <- create_grpah(j_epo_parsed,"NL","city","appln_id")

### N7.b plotting ----
set.seed(100)
par(mar=c(0,0,2,0))
plot(n7.b_nl_ig,
     vertex.label = ifelse(degree(n7.b_nl_ig)> .5, V(n7.b_nl_ig)$name, NA),
     vertex.label.cex =.9,
     vertex.label.dist = .5,
     vertex.size=degree(n7.b_nl_ig)*1.5,
     vertex.color = "#7f7f7f",
     vertex.label.font = 2,
     vertex.frame.color="black",
     edge.width = 1)
     #main="N7.b Dutch City Network of Inventors")

## Italy ----
### N8.a Italian Inventor's Network ----
n8.a_it_ig <- create_grpah(j_epo_parsed,"IT","inv_name","appln_id")

### N8.a plotting ----
set.seed(100)
par(mar=c(0,0,2,0))
plot(n8.a_it_ig,
     vertex.label = ifelse(degree(n8.a_it_ig)> 2, V(n8.a_it_ig)$name, NA),
     vertex.label.cex = 1,
     vertex.label.dist = .5,
     vertex.size=degree(n8.a_it_ig)*1.5,
     vertex.color = "#1f77b4",
     vertex.label.font= 2,
     vertex.frame.color="black",
     edge.width = 1)
     #main="N8.a Italian Inventor's Network")


### N8.a Network Measurements ----
#### Total Node Count ----
vcount(n8.a_it_ig) # number of Italian inventors
##### N8.a Graph density ----
graph.density(n8.a_it_ig)
#### Degree Centrality ----
max(degree(n8.a_it_ig, v = V(n8.a_it_ig), 
           mode = c("all", "out", "in", "total"), 
           loops = TRUE, normalized = FALSE))

### N8.b Italian City Network ----
n8.b_it_ig <- create_grpah(j_epo_parsed,"IT","city","appln_id")

### N8.b plotting ----
set.seed(100)
par(mar=c(0,0,2,0))
plot(n8.b_it_ig,
     vertex.label = ifelse(degree(n8.b_it_ig)> 2, V(n8.b_it_ig)$name, NA),
     vertex.label.cex = 1,
     vertex.label.dist = .5,
     vertex.label.font = 2,
     vertex.size=degree(n8.b_it_ig)*1.5,
     vertex.color = "#1f77b4",
     vertex.frame.color="black",
     edge.width = 1)
     #main="N8.b Italian City Network of Inventors")


### N8.b without isolated nodes ----
isolated_n8.b_it_ig <- V(n8.b_it_ig)[degree(n8.b_it_ig) == 0] # filtering the isolated nodes from the network
n8.b_it_no_isolated_ig <- delete_vertices(n8.b_it_ig, isolated_n8.b_it_ig) # making a new net obj without isolated nodes

set.seed(1021310)
par(mar=c(0,0,2,0))
plot(n8.b_it_no_isolated_ig,
     vertex.size=degree(n8.b_it_no_isolated_ig)*2,
     vertex.frame.color="black",
     vertex.color = "#1f77b4",
     vertex.label=ifelse(degree(n8.b_it_no_isolated_ig) > 2, V(n8.b_it_no_isolated_ig)$name, NA),
     vertex.label.cex = .9,
     vertex.label.dist = .4,
     vertex.label.font = 2,
     edge.width = 1,
     #main="N4.b Italian city network Without Isolated Nodes",
     layout = layout_nicely(n8.b_it_no_isolated_ig))



# Backward Citation network ----
n_cit_bw <- f03d_cit_eu %>% 
  select(appln_id, Cited_pub_nbr, ctry_code)
n_cit_bw_gfd <- graph_from_data_frame(n_cit_bw, directed = T)# making the network graph from data frame


# Use node names to determine the most frequent country for each node
ncit_node_countries <- sapply(V(n_cit_bw_gfd)$name, function(node_name) {
  ncit_get_most_frequent_country(node_name, n_cit_bw_gfd)
}, USE.NAMES = FALSE)

# test of a vector
ncit_node_countries <- unlist(ncit_node_countries)

ncit_vertex_colors <- tableau_10[ncit_node_countries] # naming nodes with the colors names

V(n_cit_bw_gfd)$color <- ncit_vertex_colors #coloring nodes according to the vertex names

### Plotting ----
set.seed(100)
par(mar = c(0, 0, 1, 0))
plot(n_cit_bw_gfd,
     vertex.label = NA,
     vertex.size = 1.5,
     vertex.frame.color = "black",
     vertex.frame.width = 0.5,
     vertex.color = V(n_cit_bw_gfd)$color, # Use the vertex color attribute
     edge.width = 0.3,
     edge.arrow.size = 0.1,
     edge.color = "black",
     #main = "Backward Citation Network",
     layout = layout_with_mds(n_cit_bw_gfd))

# Adding a color legend
legend("top",                     
       legend = names(tableau_10),
       col = tableau_10,
       pch = 19,                       
       cex = 0.8,
       horiz = TRUE)

### Backward Citation Network Measurements ----
#### Network density ----
graph.density(n_cit_bw_gfd)

#### Network clustering ----
transitivity(n_cit_bw_gfd, type = "global")

#### number of clusters/components ----
n_cit_com_inf <- components(n_cit_bw_gfd)
cluster_num <- length(n_cit_com_inf$csize)
print(cluster_num)

#### in-degree and out-degree centrality ----
in_deg <- degree(n_cit_bw_gfd, mode = "in")
t10_in_deg <- sort(in_deg, decreasing = TRUE)[1:10]

out_deg <- degree(n_cit_bw_gfd, mode = "out")
t10_out_deg <- sort(out_deg, decreasing = TRUE)[1:10]

print(t10_in_deg) # most BW cited publications
print(t10_out_deg) # most BW citing publications

# Forward citation network ----

n_cit_fw_common <- f03d_cit_eu %>% 
  filter(Cited_Appln_id %in% appln_id) # filtered the EU F03D patents which got backward citations
length(unique(n_cit_fw_common$appln_id)) # checking the unique value lengths

n_cit_fw <- n_cit_fw_common %>% 
  select(appln_id, Cited_Appln_id, ctry_code)

n_cit_fw_gfd <- graph_from_data_frame(n_cit_fw, directed = T) # making the network graph from data frame
n_cit_fw_gfd <- simplify(n_cit_fw_gfd, remove.multiple = FALSE, remove.loops = TRUE) # removing self loos

### Forward citation network plotting ---- 
set.seed(1010)
par(mar=c(0,0,2,0))
plot(n_cit_fw_gfd,
     vertex.label= NA, #ifelse(degree(n_cit_fw_gfd, mode = "in") >= 15, V(n_cit_fw_gfd)$name, NA),
     #vertex.size= 2,
     vertex.label.cex = NA,
     vertex.size= degree(n_cit_fw_gfd, mode = "in"), #log(degree(n_cit_bw_gfd))*.8,
     vertex.frame.color = "black",
     vertex.frame.width = .3,
     vertex.color =  V(n_cit_fw_gfd),
     #vertex.color = "salmon2",
     edge.width = .3,
     edge.arrow.size=.09,
     edge.color = "black")
     #main= "Forward Citation Network")

### Forward Citation Network Measurements ----
#### Top 10 application ids with forward citations
sort(degree(n_cit_fw_gfd, mode = "in"), decreasing = TRUE)[1:10]

#### Network density ----
graph.density(n_cit_fw_gfd)

#### Network clustering ----
transitivity(n_cit_fw_gfd, type = "global")

#### number of clusters/components ----
n_cit_fw_com_inf <- components(n_cit_fw_gfd)
cluster_num_fw <- length(n_cit_fw_com_inf$csize)
print(cluster_num_fw)

#### in-degree and out-degree centrality ----
in_deg_fw <- degree(n_cit_fw_gfd, mode = "in")
t10_in_deg_fw <- sort(in_deg_fw, decreasing = TRUE)[1:10] # top 10 forward citation received patent applications

out_deg_fw <- degree(n_cit_fw_gfd, mode = "out")
t10_out_deg_fw <- sort(out_deg_fw, decreasing = TRUE)[1:10] 

print(t10_in_deg_fw) # top forward cited patents
print(t10_out_deg_fw) # top forward citing patents

vcount(n_cit_fw_gfd)

# IPC code analysis ----
ipc_counts <- j_epo %>%
  select(IPC) %>% 
  group_by(IPC) %>% 
  summarise(patents = n()) %>% 
  arrange(desc(patents))

## Bar plot of total patents in each IPC category ----
ggplot(ipc_counts, aes(x = reorder(IPC, patents), y = patents)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(x = "IPC Categories", y = "Count of Patents", title = "Count of Patents by IPC Categories") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  coord_flip() + # Flipping axes to make the plot horizontal
  geom_point(size = .3) +
  geom_line()

ipc_counts_t15 <- slice_head(ipc_counts, n = 15) #slicing top 15 patents from the ipc_counts dataframe

writexl::write_xlsx(ipc_counts_t15, "ipc_t15.xlsx")

####



## Top 10 IPC categories in yearly manner----
top_10_ipc <- ipc_counts %>% 
  slice(1:10)

write_xlsx(top_10_ipc, "top_10_ipc.xlsx")



# Patent quality analysis ----
## highest forward citing patents ----
highest_fw_citing_patents <- f03d_cit_eu %>%
  group_by(appln_id) %>%
  summarize(citation_count = n()) %>%
  arrange(desc(citation_count))

head(highest_fw_citing_patents, 10)

## highest forward citing inventors ----
highest_fw_citing_inventors <- f03d_cit_eu %>% 
  group_by(inv_name) %>% 
  summarise(total_citation = n()) %>% 
  arrange(desc(total_citation))

print(highest_fw_citing_inventors)

## highest forward citing cities ----

highest_fw_citing_cities <- f03d_cit_eu %>% 
  group_by(city, ctry_code) %>% 
  summarise(total_citation = n()) %>% 
  arrange(desc(total_citation))
print(highest_fw_citing_cities)

## highest backward cited patents ----

highest_bw_cited_patents <- f03d_cit_eu %>%
  filter(Cited_Appln_id %in% appln_id) %>% 
  group_by(inv_name, city, ctry_code, Cited_Appln_id) %>% 
  summarise(total_citation = n(), .groups = "drop") %>%
  arrange(desc(total_citation))
print(highest_bw_cited_patents)


# Correlation and regression analysis of the total citation per patent and the citing lag ----
## Correlation analysis ----
cor_data <- na.omit(f03d_cit_eu[, c("Cit_Total", "Citn_lag_year")])
cor_coef <- cor(cor_data$Cit_Total, cor_data$Citn_lag_year)
print(cor_coef)

## Regression analysis ----
reg_model <- lm(Cit_Total ~ Citn_lag_year, cor_data)
summary(reg_model)

