library(tidycensus)
library(tigris)
library(tidyverse)
library(sf)

census_api_key("YOUR API KEY GOES HERE")
census_api_key("")

variables_d2010 = load_variables(2010, "sf1")
head(variables_d2010)
concepts = variables_d2010 %>% distinct(concept)

# Define which variables I want
v10 = c("P001001", # Total population
        "P003002", # Total population white alone
        "P013001" # Median age both sexes
        )

# Download data from US census (requires Internet connection)
df <- get_decennial(geography = "county", # at the level of county
                    variables = v10, 
                    year = 2010, 
                    output = "wide" # gives one column per variable
                    )

df


# Download geometries for each county from tigris package
geoms = counties(cb = TRUE, year = 2010)
geoms

## join in State Name
data("fips_codes")

geoms = geoms %>% 
  left_join(fips_codes, by = c("COUNTYFP"="county_code", "STATEFP"="state_code")) %>% 
  mutate(GEOID = str_extract(GEO_ID, "[0-9]+$")) %>% # adjust GEO_ID column to match other census data
  select(GEOID, STATEFP, COUNTYFP, 
         state, state_name, county, geometry)

# Combine census data with geometries
df_w_geom = df %>% 
  left_join(geoms, by = "GEOID") %>% 
  st_as_sf()

df_w_geom

# re order columns
df_w_geom = df_w_geom %>% 
  select(GEOID, STATEFP:county, P001001:P013001, geometry) %>% 
  rename(STATE_ABB = state,
         STATE_NAME = state_name,
         COUNTY_NAME = county) %>% 
  rename_all(tolower) # all columns to lowercase for postgres

df_w_geom


# get data types
df_w_geom %>% 
  map(class)

df_w_geom %>% 
  st_drop_geometry() %>% 
  select_if(is.character) %>% 
  map(~max(nchar(.)))


# save everything
saveRDS(variables_d2010, "content/blog/2021-07-26-working-with-postgresql-and-postgis-sql-database-tutorial/data/variables_d2010.rds")
saveRDS(df, 'content/blog/2021-07-26-working-with-postgresql-and-postgis-sql-database-tutorial/data/df.rds')
saveRDS(df_w_geom, 'content/blog/2021-07-26-working-with-postgresql-and-postgis-sql-database-tutorial/data/df_w_geom.rds')
saveRDS(geoms, 'content/blog/2021-07-26-working-with-postgresql-and-postgis-sql-database-tutorial/data/geoms.rds')
saveRDS(counties_query, 'content/blog/2021-07-26-working-with-postgresql-and-postgis-sql-database-tutorial/data/counties_query.rds')

# install.packages("RPostgres")


library(DBI)
library(RPostgres)

pw <- "your-password-goes-here"
source('data/pw.r')

con <- dbConnect(RPostgres::Postgres(),
                 host = 'localhost', # host name, can be website/server
                 port = 5432, # default port of postgres
                 dbname = 'postgres', # name of database in postgres
                 user = 'postgres', # the default user
                 password = pw # password of user
                 # options="-c search_path=us_census" # specify what schema to connect to
)

# view available tables

counties_query = 
  st_read(con, 
          query = 
            "SELECT * FROM us_census_county_2010 
       WHERE ST_DWithin(geometry::geography,
        ST_GeomFromText('POINT(-74.14967 40.79315)', 4269)::geography, 100000);"
  )


st_drivers() %>% 
  filter(grepl("Post", name))

dbListTables(con, schema = "us_census")

dbSendQuery(con, "set search_path to us_census;")
dbSendQuery(con, "CREATE EXTENSION postgis SCHEMA us_census;")
dbSendQuery(con, "CREATE EXTENSION postgis;")
dbSendQuery(con, "DROP EXTENSION postgis;")

  

dbSendQuery(con, "DROP TABLE us_census.us_census_county_2010;")

# df2 = df_w_geom
df_w_geom$geometry

st_write(obj = df_w_geom, 
         dsn = con, 
         # append = TRUE,
         Id(schema="public", table = "us_census_county_2010"))

test = st_read(con, "us_census_county_2010")
map(test, class)


ne_world <- rnaturalearth::ne_countries(returnclass = "sf")
st_write(ne_world, dsn = con, layer = "ne_world")
test = dbGetQuery(con, "SELECT * FROM ne_world LIMIT 10")
map(test, class)

# df2 = df_w_geom %>% mutate(geometry = st_as_binary(geometry))
# 
# st_write(obj = df2, 
#          dsn = con, 
#          Id(schema="us_census", table = "us_census_county_2010"))
# 
library(rpostgis)
pgPostGIS(con)
pgListGeom(con, geog = TRUE)

pgInsert(con, "ne_world", as_Spatial(ne_world), "geometry")
test2 = st_read(con, "ne_world")
test = dbGetQuery(con, "SELECT * FROM ne_world LIMIT 10")
map(test, class)
