
#  Loading required packages
  
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  gt,
  car,
  dplyr,
  ggplot2,
  magrittr,

  janitor,
  flextable
)


#  A glimpse about the data

df <- gapminder::gapminder
dim(df)
head(df, 5)
summary(df)
str(df)
unique(df$continent)
unique(df$year)
any(is.na(df))

  
#  Wrangling, Transformation and Manipulation
  
df1 <- df %>%
  mutate(gdp_status=case_when(
    gdpPercap < 1000 ~"lower income",
    gdpPercap >=1000 & gdpPercap < 4000 ~ "lower middle income",
    gdpPercap >= 4000 & gdpPercap < 12000 ~ "upper middle income",
    TRUE ~ "high income"))
tabyl(df1$gdp_status)


high_life <- df1 %>% 
  select(country, 
         year,
         lifeExp,
         continent) %>%
  rename(High_life_Exp2007 = lifeExp) %>%
  filter(year ==2007) %>% 
  arrange(desc(High_life_Exp2007)) 
gt(head(high_life,10))


low_gdp <-  df1 %>% 
  select(country, 
         year,
         gdpPercap,
         continent) %>%
  rename(low_gdp_2002 = gdpPercap) %>% 
  filter(year ==2002) %>% 
  arrange(low_gdp_2002)  
gt(head(low_gdp,10))


df1%>% 
  select(country, 
         year, 
         continent, 
         gdpPercap) %>%
  filter(year ==2002 & continent =='Africa' & gdpPercap >5000) %>% 
  arrange(desc(gdpPercap)) 


df2 <- df %>% 
  select(continent, 
         year, 
         lifeExp) %>%
  filter(year==2002) %>% 
  group_by(continent) %>%
  summarise(meanlife_exp=mean(lifeExp)) %>% 
  arrange(desc(meanlife_exp)) 
flextable(print(df2,10))


data1 <-df %>% 
  select(continent,
         year,
         pop, 
         country) %>% 
  group_by(continent) %>% 
  filter(year==1997) %>% 
  summarise(total_pop=sum(pop),
            countries=n_distinct(country)) %>% 
  arrange(desc(total_pop))
flextable(data1)


Avg_pop <- data1 %>% 
  select(continent,
         total_pop,
         countries) %>% 
  mutate(avg_pop_per_cntry=total_pop/countries) 
flextable(Avg_pop)


df3<- df %>% 
  filter(continent=="Asia" & year>1990 & year<=2000) %>% 
  summarise(Avglife.exp=mean(lifeExp),
            Avgpop=mean(pop),
            Avggdp= mean(gdpPercap))  
flextable(df3)


data2 <- df1 %>% 
  select( continent, 
          pop, 
          lifeExp ) %>% 
  group_by(continent) %>% 
  summarise(Total_pop=sum(pop),
            avglife= mean(lifeExp)) %>%  
  arrange(desc(avglife)) 
flextable(data2)


data3 <- df %>% 
  group_by(year, 
           continent) %>% 
  summarise(Year_avg.mean=mean(lifeExp),
            Year_pop=sum(pop)) 
gt(head(data3,n=10))



 # Visualization, Hypothesis testing, Correlation and Regression


data4 <- df %>% 
  select(!c(continent,
            year, 
            pop, 
            gdpPercap)) %>% 
  subset(country=="Burundi"|country=="Uganda") 
  
data4 %>% 
  group_by(country) %>% 
  ggplot(data=data4,
         mapping=aes(x=country, 
                     y=lifeExp, 
                     col=country,
                     fill=country))+
  geom_boxplot(alpha=0.5,
               show.legend = FALSE)+
  labs(title="BOX PLOT OF LIFE EXPECTANCY BY COUNTRY", 
       x="Country", 
       y="Life Expectancy")

# Test for equality of Variances
leveneTest(data4$lifeExp~data4$country)

# Sample t-test for testing equality of means between two groups
# H0: The means of two groups are equal
t.test(data=data4,
       lifeExp~country, 
       alternative='two.sided',               
       conf.level=0.95,
       var.eq=TRUE, 
       paired= FALSE)

data5 <- df %>% 
  select(!c(country,
            year,
            lifeExp)) %>% 
  subset(continent=="Europe"|continent=="Asia") 
  print(data5)

  ggplot(data=data5,
         mapping=aes(x=continent,
                     y=gdpPercap, 
                     col=continent, 
                     fill =continent))+
  geom_boxplot(alpha=0.5,
               show.legend = FALSE)+
  labs(title="BOX PLOT OF GDP-PER CONTINENT", 
       x="Continent", 
       y="GDP-percapita")


leveneTest(data5$gdpPercap~data5$continent)
t.test(data=data5,
       gdpPercap~data5$continent, 
       alternative='two.sided', 
       conf.level=0.95,
       var.eq=TRUE, 
       paired= FALSE)


data6<- df%>% 
  select(!c(country,
            year,
            gdpPercap)) %>% 
  subset(continent=="Europe"|continent=="Asia") 
  print(data6)

ggplot(data=data6,
       mapping=aes(x=continent, 
                   y=lifeExp, 
                   col=continent, 
                   fill=continent))+
  geom_boxplot(alpha=0.5,
               show.legend = FALSE)+
  labs(title="BOX PLOT OF LIFE EXPECTANCY CONTINENT", 
       x="Continent", 
       y="Life expectancy")

leveneTest(data6$lifeExp~data6$continent)

#H0: The mean life expectancy of Asia is greater than Europe
t.test(data=data6,
       lifeExp~continent, 
       alternative='greater', 
       conf.level=0.95,
       var.eq=FALSE, 
       paired= FALSE)

df1 %>% 
  ggplot(mapping=aes(x=continent, 
                     y=lifeExp, 
                     col=continent, 
                     fill=continent))+
  geom_boxplot(alpha=0.5,
               show.legend = FALSE)+
  labs(title="BOX PLOT OF LIFE EXPECTANCY BY CONTINENT", 
       x="Continent", 
       y="Life Expectancy")

# Testing for difference in mean between more than two groups
 ANOVA1 <- aov(lifeExp~continent, 
               data=df1)
 
 # Undestanding where diffences lie
 summary(ANOVA1)
 TukeyHSD(ANOVA1)
 
 df1%>% 
   ggplot(mapping=aes(x=continent, 
                      y=gdpPercap, 
                      col=continent))+
   geom_boxplot(alpha=0.5,
                show.legend = FALSE)+
   labs(title="BOX PLOT OF GDP-PERCAPITA BY CONTINENT",
        x="Continent", 
        y="GDP-percapita")
  
 ANOVA2 <- aov(gdpPercap~continent, 
               data=df1) 
 summary(ANOVA2)
 TukeyHSD(ANOVA2)
 
 # Testing for difference in means in categoricals
 gdp_status <-df%>% 
   mutate(GDP_status = ifelse(gdpPercap>5000,"high_gdp", 
                              "low_gdp"), 
  life_exp_status = ifelse(lifeExp>60,"high_life.exp", 
                           "low_life.exp"))

 tabyl(gdp_status$GDP_status)
 
 
 
  TAB <-  table(gdp_status$life_exp_status,
                gdp_status$GDP_status)
  barplot(TAB,beside=T, 
          legend.text = c('High life expectancy',
                          'low life expectancy'),
          main = 'GDP status VS Life expectancy status',
          xlab='GDP status', 
          col=c(2,4))

  chisq.test(TAB, 
             correct=TRUE)
  
  # Undestanding distribution of data
  hist(df1$lifeExp, 
       breaks = 10, 
       xlim=c(30,90), 
       col='light blue',
       main  ='Histogram for Life Expectancy',
       xlab='Life expectancy')
  hist(log(df1$gdpPercap))
  
  ggplot(data=df1,
         mapping=aes(x=lifeExp,
                     y=log(gdpPercap)))+
      geom_point(col='green', 
                 size=2)+
      geom_smooth(method='lm',
                  col= 'blue')+
    labs(title="Scatterplot:Life Expectancy Vs GDP-percapita",
         x="Life expetancy", 
        y="GDP-percapita")
      
  # Correlation and regression
  df %>% 
    filter(gdpPercap < 5000) %>% 
    ggplot(aes(x=gdpPercap, 
               y=lifeExp, 
               col=continent))+
    geom_point(alpha=0.3)+
    geom_smooth(method=lm)+
    facet_wrap(~continent)
  
cor(df$lifeExp,
    df$gdpPercap, 
    method = 'pearson')
lm(df$lifeExp ~ df$gdpPercap)
summary(lm(df$lifeExp ~ df$gdpPercap))
  


            
              
  
  
