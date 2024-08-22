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


### DATA Viz  ---- 


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






### DATA Wrangling  ---- 
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




