doInstall <- TRUE
toInstall <- c("twitteR", "dismo", "maps", "ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

library(dismo)
library(maps)
library(ggplot2)


searchTerm <- "#E32017"
searchResults <- searchTwitter(searchTerm, n = 1000, lang = "es")  # Gather Tweets 
tweetFrame <- twListToDF(searchResults)  # Convert to a nice dF

userInfo <- lookupUsers(tweetFrame$screenName)  # Batch lookup of user info
userFrame <- twListToDF(userInfo)  # Convert to a nice dF

lugares<-".*Spain|Espa�a|Madrid|Barcelona|Valencia|Sevilla|Alicante|M�laga|Murcia|C�diz|
          Vizcaya|La Coru�a|Islas Baleares|Las Palmas|Asturias|Santa Cruz de Tenerife|
          Zaragoza|Pontevedra|Granada|Tarragona|C�rdoba|Gerona|Guip�zcoa|Almer�a|Toledo|
          Badajoz|Ja�n|Navarra|Cantabria|Castell�n|Valladolid|Huelva|Ciudad Real|
          L�rida|C�ceres|Albacete|Burgos|Lugo|Salamanca|�lava|La Rioja|Orense|Guadalajara|
          Huesca|Cuenca|Zamora|Palencia|�vila|Segovia|Teruel|Soria|Melilla|Ceuta.*"


locatedUsers <- grep(lugares,userFrame$location,ignore.case = T)  # Mantiene solo los datos que tienen informaci�n geogr�fica y filtra Espa�a
userNames<-row.names(userFrame[locatedUsers,])
userNames
tweetsV<-matrix(nrow = 1,ncol = ncol(tweetFrame))
frameTweetsV<-twListToDF(tweetsV)

for(a in 1:nrow(tweetFrame)){
indiceTweet<-grepl(tweetFrame$screenName[a],userNames,ignore.case = T)
}

grep(tweetFrame$screenName[a],userNames,ignore.case = T)


indices<-grepl(tweetFrame$screenName[1],userNames,perl = T)

tweetFrame$screenName
userNames

locations <- geocode(userFrame$location[locatedUsers])  # Use amazing API to guess
# approximate lat/lon from textual location data.
with(locations, plot(longitude, latitude))

worldMap <- map_data("world")  # Easiest way to grab a world map shapefile

zp1 <- ggplot(worldMap)
zp1 <- zp1 + geom_path(aes(x = long, y = lat, group = group),  # Draw map
                       colour = gray(2/3), lwd = 1/3)
zp1 <- zp1 + geom_point(data = locations,  # Add points indicating users
                        aes(x = longitude, y = latitude),
                        colour = "RED", alpha = 1/2, size = 1)
zp1 <- zp1 + coord_equal()  # Better projections are left for a future post
zp1 <- zp1 + theme_minimal()  # Drop background annotations
print(zp1)
