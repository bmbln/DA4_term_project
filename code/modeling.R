library(tidyverse)
library(data.table)

library(estimatr)
library(lspline)
library(huxtable)

#first of all, let's call our clean_df
clean_df <- read_csv("https://raw.githubusercontent.com/bmbln/DA4_term_project/main/data/clean/clean_df.csv")


#### FEATURE ENGINEERING
fin_df <- clean_df %>% 
  #calculate the vote share per years
  mutate( jobbik_share18 = jobbik18 / no_votes18 * 100 ,
          fidesz_share18 = fidesz_18 / no_votes18 * 100, 
          fidesz_share22 = fidesz22 / no_votes22 * 100, 
          opp_alliance_share22 = opp_alliance22 / no_votes22 * 100 , 
          mi_hazank_share22 = mi_hazank22 / no_votes22 * 100) %>% 
  #claculate the difference for fidesz and the ratio for mi hazank / jobbik
  mutate( d_fidesz = fidesz_share22 - fidesz_share18 , 
          d_mh_jobbik = mi_hazank_share22 / jobbik_share18 * 100 ) %>% 
  #create dummies for the party affiliations, one of them is our var of interest
  #factors could also work, but they make the interpretation cumbersome
  #also transform to factor
  mutate( is_jobbik22 = ifelse( opp_affiliation22 == "JOBBIK" , 1 , 0 ) , 
          is_DK22 = ifelse( opp_affiliation22 == "DK" , 1 , 0 ) , 
          is_LMP22 = ifelse( opp_affiliation22 == "LMP" , 1 , 0 ),
          is_MOMENTUM22 = ifelse( opp_affiliation22 == "MOMENTUM" , 1 , 0 ) ,
          is_PARBESZED22 = ifelse( opp_affiliation22 == "PÁRBESZÉD" , 1 , 0 ) ,
          is_UVNP22 = ifelse( opp_affiliation22 == "ÚVNP" , 1 , 0 ) ,
          m_type = as.factor(m_type) , 
          opp_affiliation22 = as.factor(opp_affiliation22) ) %>% 
  #calculate a dummy for the confounders, wehter Jobbik 2018 candidate was active
  #not necessary for checking if Jobbik 2018 went to Mi Hazank
  mutate( jobbik18_active = jobbik18on_opp22list + jobbik18on_preelection ) %>% 
  mutate( jobbik18_active = ifelse( jobbik18_active > 0 , 1 , 0 ) )

#it seems, that in some municipalities, Mi Hazank got more votes in 2022 than Jobbik (in some cases Jobbik had no votes in 2018)
#in these (very) small municipalities, Mi Hazank activized other voters as well
#as an analyst decision, we are going to drop municipalities where Mi Hazank got more than 75% percent of the votes 
#it's roughly 2.5x the standard deviation from the mean, we can outrule municipalities this way where Mi Hazank voters are very much mixed with non-ex-Jobbik voters
fin_df <- fin_df %>% 
  #in some municipalities there were no votes for Jobbik in 2018 or for Mi Hazank in 2022. let's impute these with 0
  mutate( d_mh_jobbik = ifelse( is.na(d_mh_jobbik) , 0 , d_mh_jobbik ) ) %>% 
  #first filter out infinite values
  filter( d_mh_jobbik != Inf) %>% 
  filter( d_mh_jobbik < 75 )


#the plot is very interesting on itself, BUT
#the takeaway is that we might need to model the fidesz vote's change with an lspline instead of linear. 0 and 10 cut-offs
ggplot(fin_df , aes( d_fidesz , d_mh_jobbik  , weight = no_votes22 ) ) +
  geom_point( size = .2 ) +
  geom_smooth( method = 'loess' ) + 
  labs( title = "Fidesz increase and Mi Hazank share of Jobbik 2018 votes" , 
        subtitle = "per municpalities" ,
        x = "Change in Fidesz share of votes (% points)" , 
        y = "Mi Hanzak share\n proportional to \nJobbik 2018 share (%)" , 
        caption = "loess weighted by number of votes in 2022") +
  theme_bw()


### MODELING
m1_vars <- as.formula( d_mh_jobbik ~ is_jobbik22 )
model1 <- lm( data = fin_df , 
              m1_vars , weight = no_votes22 )
huxreg( model1 )


m2_vars <- as.formula( d_mh_jobbik ~ is_jobbik22 + 
                         is_DK22 + is_LMP22 + is_MOMENTUM22 + is_PARBESZED22 + is_UVNP22 )
model2 <- lm( data = fin_df , 
              m2_vars , weight = no_votes22 )
huxreg( model2 )



m3_vars <- as.formula( d_mh_jobbik ~ is_jobbik22  + 
                         opp22is_jobbik18 + jobbik18_active )
model3 <- lm( data = fin_df , 
              m3_vars , weight = no_votes22 )
huxreg( model3 )



m4_vars <- as.formula( d_mh_jobbik ~ is_jobbik22  +
                         jobbik18on_mihazank22list )
model4 <- lm( data = fin_df , 
              m4_vars , weight = no_votes22 )
huxreg( model4 )



m5_vars <- as.formula( d_mh_jobbik ~ is_jobbik22 + 
                         lspline(d_fidesz, c(0,10) ) )
model5 <- lm( data = fin_df , 
              m5_vars , weight = no_votes22 )
huxreg( model5 )



m6_vars <- as.formula( d_mh_jobbik ~ is_jobbik22 + 
                         m_type )
model6 <- lm( data = fin_df , 
              m6_vars , weight = no_votes22 )
huxreg( model6 )



m7_vars <- as.formula( d_mh_jobbik ~ is_jobbik22 +
                         lspline(d_fidesz, c(0,10) ) +
                         is_DK22 + is_LMP22 + is_MOMENTUM22 + is_PARBESZED22 + is_UVNP22 +
                         opp22is_jobbik18 + jobbik18_active +
                         jobbik18on_mihazank22list +
                         m_type )
model7 <- lm( data = fin_df , 
              m7_vars , weight = no_votes22 )
huxreg( model7 )



reg_table <- huxreg( model1 , model2 ,  model3 , model4 , model5 , model6 , model7 ,
                  statistics = c(N = "nobs" , R2 = "r.squared") , 
                  coefs = c( "Constant" = "(Intercept)" , 
                             "Jobbik candidate" = "is_jobbik22" ) )

fin_reg_table <- reg_table %>% 
  insert_row( c("Other opposion parties", "" , "Yes" , "" , "" , "" , "" , "Yes") , after = 5 ) %>% 
  insert_row( c("Jobbik 2018 active", "" , "" , "Yes" , "" , "" , "" , "Yes") , after = 6 ) %>% 
  insert_row( c("Jobbik 2018 in Mi Hazank", "" , "" , "" , "Yes" , "" , "" , "Yes") , after = 7 ) %>% 
  insert_row( c("Change in Fidesz", "" , "" , "" , "" , "Yes" , "" , "Yes") , after = 8 ) %>% 
  insert_row( c("Municipality type", "" , "" , "" , "" , "" , "Yes" , "Yes") , after = 9 )

save( fin_df , 
      model1 , model2 , model3 , model4 , model5 , model6 , model7 , 
      reg_table , fin_reg_table , file = "data/clean/results.RData")

