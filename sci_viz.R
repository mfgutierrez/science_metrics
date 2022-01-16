library(tidyverse)

df <- readr::read_tsv("./scimagojr country rank 2020.txt")
colnames(df)[3] <- "Continent"
pop <- readr::read_csv("./population-since-1800.csv")
pop <- pop %>% filter(Year == 2020)
df <- df %>%
  mutate(Country = recode(str_trim(Country), 
                            "Russian Federation" = "Russia",
                            "United States" = "USA",
                            "United Kingdom" = "UK",
                            "Korea (Rep.)" = "South Korea",
                            "C\x99te d\xd5Ivoire" = "Ivory Coast",
                            "Brunei Darussalam" = "Brunei",
                            "Viet Nam" = "Vietnam",
                            "Congo" = "Democratic Republic of the Congo")) %>% mutate(Continent = recode(str_trim(Continent),
                                "Africa/Middle East" = "Africa"                                                                         
                            ) )
                            
pop <- pop %>% mutate(Entity = recode(str_trim(Entity), 
                            "Russian Federation" = "Russia",
                            "United States" = "USA",
                            "United Kingdom" = "UK",
                            "Korea (Rep.)" = "South Korea",
                            "Cote d'Ivoire" = "Ivory Coast",
                            "Congo" = "Democratic Republic of the Congo"))

world_map <- map_data("world") 

df1 <- inner_join(world_map, df, by = c("region" = "Country"))
df2 <- inner_join(df1, pop, by = c("region" = "Entity"))
df2 <- df2 %>% mutate(per_capita = `Citable documents`/`Population (historical estimates)`)  

world_sci <- ggplot(data = df2, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = (per_capita)), color = "black", size= 0.1) +
  scale_fill_distiller(palette ="RdYlBu", direction = 1, trans = "log", name = "") + ggtitle("Scientific publications per capita (2020)") + theme_void() 

world_sci

df2 %>% filter(`Population (historical estimates)` > 500000) %>% distinct(region, .keep_all = T) %>% group_by(Continent) %>% slice_max(order_by = per_capita,n = 5)  %>% dplyr::select(region, per_capita) %>% ggplot(., aes(x= reorder(region, -per_capita), y = per_capita)) + geom_col() + facet_wrap(~Continent, scales = "free") + theme_minimal()

df2 %>% distinct(region, .keep_all = T) %>% filter(`Population (historical estimates)` > 500000)  %>% slice_max(per_capita,n = 10) %>% dplyr::select(region, per_capita) %>% dplyr::select(region, per_capita) %>% ggplot(., aes(x= reorder(region, -per_capita), y = per_capita)) + geom_col()  + theme_minimal()
