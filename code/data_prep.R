library(tidyverse)
library(data.table)

#### 2022 data
# reading 2022 data
val_2022 <- read_csv("https://raw.githubusercontent.com/bmbln/DA4_term_project/main/data/raw/2022election.csv")
base_vars_2022 <- c( 'telep' , 'teltipus' , 'ervenyesegyeni'  )
base_vars_2022_renamed <- c( 'municipality' , 'm_type' , 'no_votes22' )

party_vars_2022  <- c( 'FIDESZ-KDNP...7' , 'DK-JOBBIK-LMP-MSZP-MOMENTUM-PÁRBESZÉD' , 'MI HAZÁNK...13' )
party_vars_2022_renamed  <- c( 'fidesz22' , 'opp_alliance22' , 'mi_hazank22' )
other_party_vars_2022 <- c( 'MKKP...9' , 'MEMO...10' , 'MUNKÁSPÁRT-ISZOMM' , 'NORMÁLIS PÁRT...12' ,
                            'Független' , 'VD' , 'IMA' , 'ZÖLDEK PÁRTJA' , 
                            'MAGYAR LIBERÁLIS PÁRT -LIBERÁL' , 'MSZDDSZ' , 'POLGÁRI VÁLASZ' )
other_party_vars_2022_renamed  <- c( 'mkkp' , 'memo' , 'iszomm' , 'normalis' , 'indep' , 'vd' , 'ima' , 'greens' , 'liberal' , 'mszddsz' , 'polg_val' )

val_2022 <- val_2022 %>% 
  select( c( base_vars_2022 , party_vars_2022 , other_party_vars_2022 ) ) %>% #select relevant cols
  filter( !is.na( telep ) ) %>% #filter erroneous row
  mutate_all( ~replace(., is.na(.), 0) )  #replace NA with 0 (the true value is 0)

colnames(val_2022) <- c( base_vars_2022_renamed , party_vars_2022_renamed , other_party_vars_2022_renamed )

val_2022 <- val_2022 %>% 
  mutate( other_votes22 = mkkp + memo + iszomm + normalis + indep + vd + ima + greens + liberal + mszddsz + polg_val ) %>% 
  select( -other_party_vars_2022_renamed ) %>% 
  mutate( data_issue22 = ifelse( ( no_votes22 - fidesz22 - opp_alliance22 - mi_hazank22 - other_votes22) == 0 , FALSE , TRUE  ) ) #if the sum doesn't match, then we have data problems

#by looking at the data_issues and manually cross checking, the no_votes22 was wrong for unknown reasons in the original dataset, so let's fix it
val_2022 <- val_2022 %>% 
  mutate( no_votes22 = fidesz22 + opp_alliance22 + mi_hazank22 + other_votes22 ) %>% 
  select( - c('data_issue22' , 'other_votes22' ) )


### 2018 data
# reading 2018 data
val_2018 <- read_delim("https://raw.githubusercontent.com/bmbln/DA4_term_project/main/data/raw/2018election.csv", delim = ";")

#select cities where JOBBIK data is missng as no in the TOP3
jobbik2018_missing <- val_2018 %>% filter(val_2018$`Első hely párt` != 'JOBBIK' & val_2018$`Második hely párt` != 'JOBBIK' &
                                            val_2018$`Harmadik hely párt` != 'JOBBIK')

#write it into a csv for manual imputation
#write_csv( jobbik2018_missing , "data/raw/2018jobbik_missing.csv")

#read back the manually imputed jobbik data (3 Budapest districts missing as they are ambigous)
jobbik2018_imputed <- read_csv( "https://raw.githubusercontent.com/bmbln/DA4_term_project/main/data/raw/2018jobbik_imputed.csv" )

# keeping only jobbik where they are in top 3 ----
jobbik2018_1 <- val_2018 %>% filter(val_2018$`Első hely párt` == 'JOBBIK') %>% 
  rename(`JOBBIK` = `Első hely`) %>% select( c(1:6) )

jobbik2018_2 <- val_2018 %>% filter(val_2018$`Második hely párt` == 'JOBBIK') %>%
  rename(`JOBBIK` = `Második hely`) %>% select( c(1:5,7) )

jobbik2018_3 <- val_2018 %>% filter(val_2018$`Harmadik hely párt` == 'JOBBIK') %>% 
  rename(`JOBBIK` = `Harmadik hely`) %>% select( c(1:5,8) )

#merge the jobbik votes
jobbik2018 <- rbind( jobbik2018_1 , jobbik2018_2 , jobbik2018_3 , jobbik2018_imputed ) %>% 
  select( -1 )

varnames_2018 <- c( 'municipality' , 'no_votes18' , 'opp_18' , 'fidesz_18' , 'jobbik18')

colnames(jobbik2018) <- varnames_2018

df <- inner_join( jobbik2018 , val_2022 , by = "municipality") %>% 
  distinct(municipality , .keep_all = TRUE ) #some municipalities are duplicated. 4 has different vlaues, upon double-check the first value turns out to be wrong

### merge with municipality and party affiliation

municipality_vk <- read_csv("https://raw.githubusercontent.com/bmbln/DA4_term_project/main/data/raw/municipality_VK.csv")
party_aff <- read_csv("https://raw.githubusercontent.com/bmbln/DA4_term_project/main/data/raw/party_affiliation.csv")

clean_df <- left_join( municipality_vk , df , by = "municipality" ) %>% 
  left_join( party_aff , by = "oevk" )

### CORRECT shared cities 
#now let's correct the shared municipalities
oevk2022 <- read_csv("https://raw.githubusercontent.com/bmbln/DA4_term_project/main/data/raw/2022oevk.csv")
oevk2018 <- read_csv("https://raw.githubusercontent.com/bmbln/DA4_term_project/main/data/raw/2018oevk.csv")

toDeduct <- clean_df %>% 
  filter( ambigous == 0 ) %>% 
  group_by( oevk ) %>% 
  select( where( is.numeric ) ) %>% 
  summarise( across( everything() , list(sum) ) ) %>% 
  ungroup()

# let's check which municipials need adjusting
ambi <- clean_df %>% filter( ambigous == 1 )
ambi$oevk 
#we won't able to calculate the votes if 2 shared municipalities are in the same oevk
#nor in oevk where jobbik18 is missing 
#it's a problem with some Budapest districts 
cant_calc <- unique( c( ambi$oevk[duplicated(ambi$oevk)] , ambi$oevk[is.na(ambi$jobbik18)] ))

oevkTochange <- oevk2018[oevk2018$Választókerület %in% ambi$oevk,] 
#save to scv what we need to impute manually:
#write_csv( oevkTochange[,1:2] , "data/raw/2018jobbik_tobe_imputed_oevk.csv")

asd <- oevkTochange %>% 
  left_join( toDeduct[1:6] , by = c(`Választókerület` = 'oevk') ) %>% 
  mutate_all(~replace(., is.na(.), 0))

jobbik18_oevk <- read_csv("https://raw.githubusercontent.com/bmbln/DA4_term_project/main/data/raw/2018jobbik_imputed_oevk.csv")

asd <- asd %>% left_join(jobbik18_oevk)
asd <- asd[,c(2, 10:11, 17:22)]

asd2018_ambig <- asd %>% mutate(
  no_votes18 = no_votes18 - no_votes18_1,
  fidesz_18 = `Fideszre leadott szavazatok száma` - fidesz_18_1,
  opp_18 = `Összesített ellenzéki szavazatok száma` - opp_18_1,
  jobbik18 = jobbik - jobbik18_1
)

ambigous_fin <- ambi[,-c(4:7,9:13)] %>% 
  left_join( asd2018_ambig[,c(1,9:12)] , by = c( "oevk" = "Választókerület") )


oevk2022 <- oevk2022[1:28]

oevkTochange2022 <-  oevk2022[oevk2022$oevk %in% ambi$oevk,]

asd2022 <- oevkTochange2022 %>% 
  left_join(toDeduct[c(1,7:11)]) %>% 
  mutate_all(~replace(., is.na(.), 0))

asd2022 <- asd2022 %>% 
  mutate(
    no_votes22 = ervenyesegyeni - no_votes22_1,
    fidesz22 = `FIDESZ-KDNP...22` - fidesz22_1,
    opp_alliance22 = `DK-JOBBIK-LMP-MSZP-MOMENTUM-PÁRBESZÉD...23` - opp_alliance22_1,
    mi_hazank22 = `MI HAZÁNK...28` - mi_hazank22_1 ) %>% 
  select( c('oevk' , 'no_votes22' , 'fidesz22' , 'opp_alliance22' , 'mi_hazank22' ) )


ambigous_fin <- ambigous_fin %>% 
  left_join( asd2022 )

#now let's finally remove the Budapest districts that cannot be calculated
ambigous_fin$cant_calc <- ambigous_fin$oevk %in% cant_calc 

ambigous_fin <- ambigous_fin %>% 
  filter( cant_calc == FALSE ) %>% 
  select( -cant_calc )

clean_df <- clean_df %>% 
  filter( ambigous == 0 ) %>% 
  select( -opp_candidate_22) %>% #unnecessary
  rbind( ambigous_fin ) %>% 
  select( -c( 'jobbik_candidate18' , 'ambigous') ) #unnecessary and we won't need the ambigous anymore

#write it into the clean folder, we are done.
#write_csv( clean_df, file = "data/clean/clean_df.csv")
