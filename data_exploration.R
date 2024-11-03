## Library ==== 
library(tidyverse)
library(gapminder)
library(ggthemes)
library(ggrepel)
gapminder_tib <- gapminder


gapminder_2007_tib <- gapminder_tib |>
    filter(year == 2007)

gapminder_2007_tib |> 
    ggplot() + 
    geom_point(
        aes(y = lifeExp,
        x = gdpPercap,
        colour = continent
        )
    )

gapminder_tib |> 
    filter((country=='Germany')|(country=="France"))


country_filter <- c("Australia",
                    "Germany",
                    "United States","Bangladesh",
                    "Afghanistan")
selected_country_tib <- gapminder_tib |> 
    filter(country %in% country_filter)


selected_country_tib |> 
    ggplot() + 
    geom_point(
        aes(x = year,
            y =  lifeExp,
            colour = country
        )
    ) + geom_line(
        aes(x = year,
            y =  lifeExp,
            colour = country
        )
    )
    




gapminder_2007_tib |> 
    ggplot(aes(y =  lifeExp,
               colour =continent |> fct_reorder(lifeExp)
    )) + 
    geom_boxplot(
        
    ) 


## DATA Viz  ---- 


plot_gbp_life <- gapminder_2007_tib |> 
    ggplot(aes(x = gdpPercap,y = lifeExp,color = continent,size = pop)) + 
    geom_point()

plot_gbp_life + 
    guides(size = "none") + 
    labs( 
        x = 'GDP Per Captia',
        y = 'Life Expectancy',
        color = 'Continent',
        title = 'GDP per Captia vs Life Expectancy',
        subtitle = 'The size of the bubbles relate to population of the country'
        )+
    theme_bw()+ theme (legend.position = 'bottom') 

 
### adding the text to plot 
selectedCountry <- selected_country_tib |> 
    filter(year == 2007)


plot_gbp_life + 
    ggrepel::geom_text_repel(data = selectedCountry,
              mapping =  aes(label = country),size = 5,
              show.legend = F,
              box.padding = 1.5,
              point.padding = 0.5
              )+
    guides(size = "none")+ theme_hc()+ scale_colour_hc()






## DATA Wrangling  ---- 
percentage_of_users <- read_csv('data/share-of-individuals-using-the-internet.csv')


percentage_of_users <- percentage_of_users |> 
    janitor::clean_names()


percentage_of_users_wider <- percentage_of_users |> 
    pivot_wider(names_from = 'year',
                values_from = 'individuals_using_the_internet_percent_of_population'
                )


selected_rearranged_internet_users <- percentage_of_users_wider |> 
    filter(entity %in% country_filter) |> 
    pivot_longer(
        cols = -c(entity,code),
        names_to = 'year',
        values_to = 'percentage',
        names_transform = list(year = as.integer)
    ) 



selected_rearranged_internet_users |> 
    ggplot(aes(x = year,
               y = percentage,
               color = entity
               
               
               ))+ 
    geom_line() + 
    geom_point() + 
    lims(x = c(1990,2020)) + 
    theme_hc()+ scale_colour_hc()
    

cars_tibble <- cars |> as_tibble()


cars_tibble |> 
    ggplot(aes(speed,dist))+
    geom_point()+
    theme_hc()+ scale_colour_hc()

#### Grouping ---- 

summary_evolution <- gapminder_tib |> 
    group_by(continent,year) |> 
    summarize(
        mean_life = mean(lifeExp),
        median_life = median(lifeExp),
        lowerquantitle  = quantile(lifeExp,0.25),
        upperquantitle  = quantile(lifeExp,0.75),
        n = n(),
        .groups = 'drop'
        )

summary_evolution

# Slice min value 

gapminder_2007_tib |> 
    group_by(continent) |> 
    slice_min(lifeExp)

summary_evolution |> 
    mutate(newcol= mean(lowerquantitle)) |> 
    slice(1)


### 
### Flight dataset 
library(nycflights13)


flight_selected <- flights |> 
    select(year:dep_time,origin,dest)
origin_airport_name  <- airports |> 
    select(faa,name) |> 
    rename(origin = faa,origin_airport = name)

destination_airport_name  <- airports |> 
    select(faa,name) |> 
    rename(dest = faa,destination_airport = name)


flight_join <- flight_selected |> 
    left_join(origin_airport_name,by = 'origin') |> 
    left_join(destination_airport_name,by='dest') |> 
    select( -c(origin,dest))

flight_join

#### creating flight number 

flight_number <- seq_along(flight_join$year)


flight_join |> 
    bind_cols(flight_number = flight_number)


###
delays <- flights |> 
    select(year:dep_delay)


delays |> 
    mutate(across(.fns = is.na)) |> 
    summarise(across(.fns = sum))

## Considering only flights that departed. 

delays_departed <- delays |> 
    filter(!is.na(dep_time))


delays_departed |> 
    mutate(across(.fns = is.na)) |> 
    summarise(across(.fns = sum))



summary_delay <- delays_departed |> 
    group_by(year,month,day) |> 
    summarize(
        min_delay = min(dep_delay),
        max_delay = max(dep_delay),
        mean_delay = mean(dep_delay),
        median_delay = median(dep_delay),
        lowerquantitle  = quantile(dep_delay,0.25),
        upperquantitle  = quantile(dep_delay,0.75),
        n = n(),
        .groups = 'drop'
    ) |> 
    pivot_longer(
        cols = -(year:day),
        values_to = 'value',
        names_to = 'summary'
        
    ) |> 
    mutate(date = lubridate::make_date(year,month,day))


summary_level <- c(
    'min_delay',
    'lowerquantitle',
    'median_delay',
    'mean_delay',
    'upperquantitle',
    'max_delay'
)


summary_delay |> 
    filter(summary !='n') |> 
    mutate(summary = factor(summary,levels = summary_level)) |> 
    ggplot(aes(x = date, y = value , colour = summary))+
    geom_line(show.legend = F,alpha = 0.5)+
    geom_smooth(show.legend = F,se = F,span=0.1,size = 1) + 
    facet_wrap(vars(summary),scale='free_y') + 
    scale_x_date(date_labels = '%m')+ 
    labs(
        x = 'Month',
        y = 'Delay(in Min)'
    )+
    theme_bw()


summary_delay |> 
    filter(! summary %in% c('n','max_delay','min_delay')) |> 
    mutate(summary = factor(summary,levels = summary_level)) |> 
    ggplot(aes(x = date, y = value , colour = summary))+
    geom_smooth(,span=0.1) + 
    scale_x_date(date_labels = '%m',date_breaks = '1 month')+ 
    labs(
        x = 'Month',
        y = 'Delay(in Min)'
    )+
    theme_bw()
