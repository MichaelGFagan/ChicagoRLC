rlc <- read.csv("Red_Light_Camera_Violations.csv")
rlc <- rlc[complete.cases(rlc),]

plot(rlc$X.COORDINATE, rlc$Y.COORDINATE)

rlc.sum$radius <- sqrt(rlc.sum$sum / pi)

rlc.sum <- aggregate(rlc$VIOLATIONS, by=list(xco=rlc$X.COORDINATE), FUN=sum)
rlc.sum <- rename(rlc.sum, c("x" = "sum", "xco" = "x"))
rlc.sum <- rlc.sum[order(rlc.sum$x), ]

rlc.sum$y <- unique(x$Y.COORDINATE)

symbols(rlc.sum$x, rlc.sum$y, circles = rlc.sum$radius, inches = 0.1)

xy.coord <- unique(rlc[, c('LATITUDE', 'LONGITUDE')])

rlc.xy <- aggregate(rlc$VIOLATIONS, by=list(lat=rlc$LATITUDE), FUN = sum)
xy.coord <- rename(xy.coord, c("LATITUDE" = "lat", "LONGITUDE" = "long"))
rlc.xy <- merge(rlc.xy, xy.coord, by = "lat")
rlc.xy <- rlc.xy[c(1, 3, 2)]
plot(rlc.xy$long, rlc.xy$lat)
rlc.xy$radius <- sqrt(rlc.xy$x / pi)
symbols(rlc.xy$long, rlc.xy$lat, circles = rlc.xy$radius, inches = 0.1)
points(-87.6335, 41.8297, col = "red")
points(-87.6553, 41.9484, col = "red")
points(-87.6359, 41.8789, col = "red")
points(-87.6480, 41.8719, col = "red")
points(-87.7522, 41.7868, col = "red")
points(-87.6748, 42.0531, col = "red")
points(-87.6244, 41.7224, col = "red")
points(-87.7129, 41.9677, col = "red")


### WORK ON AUGUST 25, 2016 ###

### Wrangling data into 2015 and coordinates ###
rlc <- mutate(rlc, VIOLATION.YEAR = as.integer(substring(as.character(rlc$VIOLATION.DATE), 7, 10)))
rlc2015 <- filter(rlc, VIOLATION.YEAR == 2015)

rlc.coord <- unique(rlc[, c('LATITUDE', 'LONGITUDE')])
camera.list <- unique(MASTER[, c('latitude', 'longitude', 'quad')])
min(camera.list$longitude)

### Plotting cameras ###
ggplot(data = rlc2015, mapping = aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point() + 
  coord_fixed()

### Experimenting with ggmap ###
map <- get_map(location = 'Chicago', zoom = 11)
map4 <- get_map(location = c(-87.85, 40, -87.55, 43), source = "google", maptype = "roadmap")
map4 <- get_map(location = c(-87.85, 41, -87.55, 42.5), source = "stamen", maptype = "toner-background", crop = FALSE)
map3 <- get_map(location = c(lon = -87.64, lat = 41.8540375), source = "google", maptype = "roadmap", zoom = 11)

?get_openstreetmap
ggmap(map4) +
  geom_point(data = rlc.coord, mapping = aes(x = LONGITUDE, y = LATITUDE))

ggmap(map3) +
  geom_point(data = camera.list, mapping = aes(x = longitude, y = latitude, color = quad), size = 2) +
  theme(axis.title=element_blank()) +
  guides(color = guide_legend("Legend"))
  

ggmap(map4) +
  geom_point(data = camera.list, mapping = aes(x = longitude, y = latitude), color = "red3", size = 2) +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())

### RLC camera tickets? ###

rlc.tickets <- read.csv(file.choose(), header = FALSE)
tail(rlc.tickets)
colnames(rlc.tickets) <- c("ID", "timestamp", "plate", "car", "state", "location")
rlc.tickets$year <- substr(rlc.tickets$timestamp, 1, 4)
unique(rlc.tickets$car)
car.count <- count(rlc.tickets, car)

### Printing stuff out ###
head(rlc.tickets)
tail(rlc.tickets)

head(rlc)
tail(rlc)

### Work on 9/8/16 ###
### Checking if rlc$INTERSECTION and rlc.tickets$location are same ###
filter(rlc.tickets, location == "0 S CENTRAL AVENUE")
cam.id <- unique(rlc$CAMERA.ID)
location <- unique(rlc$ADDRESS)
length((cam.id))
length(location)
filter(rlc, unique(rlc$CAMERA.ID))

unique.cams2 <- rlc %>% 
  select(CAMERA.ID, ADDRESS, LATITUDE, LONGITUDE) %>% 
  filter(LATITUDE > 0 & CAMERA.ID > 0) %>%
  distinct %>%
  select(ADDRESS, LATITUDE, LONGITUDE)

unique.cams <- arrange(unique.cams, ADDRESS, CAMERA.ID)
unique.address <- arrange(unique.address, ADDRESS)

unique.ids <- rlc %>%
  select(CAMERA.ID, LATITUDE, LONGITUDE) %>%
  distinct

unique.ids <- filter(unique.ids, CAMERA.ID > 0)
unique.ids <- filter(unique.ids, LATITUDE > 0)
unique.address <- rlc %>%
  select(ADDRESS, LATITUDE, LONGITUDE) %>%
  distinct

ggplot(data = unique.ids) +
  geom_text(mapping = aes(x = LONGITUDE, y = LATITUDE, label = as.character(CAMERA.ID))) +
  coord_quickmap()

by_cam <- group_by(rlc.tickets, location)
cam.total <- summarize(by_cam, count = n())
colnames(cam.total)[1] <- "ADDRESS"

unique.cam.plus <- merge(unique.cams2, unique.locations, by="ADDRESS", all.y = TRUE, all.x = TRUE)
unique.locations$address <- NULL
unique.cam.plus[,is.na(unique.cam.plus$LATITUDE)]
unique.cam.plus <- unique.cam.plus %>%
  distinct
write.csv(unique.cam.plus, file = "uniquecams.csv")
unique.cams4 <- read.csv(file.choose())

ggplot(data = cam.total.plus) +
  geom_text(mapping = aes(x = LONGITUDE, y = LATITUDE, label = as.character(count))) +
  coord_quickmap()

by_cam$date <- as.Date(substr(by_cam$timestamp, 1, 10))
by_cam$date <- as.Date(by_cam$date)

min.date <- summarize(by_cam, min(date, na.rm = TRUE))
max.date <- summarize(by_cam, max(date, na.rm = TRUE))
minmax.date <- merge(min.date, max.date, by = "location")
colnames(minmax.date)[1] <- "ADDRESS"
summary.data <- merge(minmax.date, rlc.totals, by = "location")

rlc.tickets.summary <- rlc.tickets %>%
  group_by(location) %>%
  summarize(
    count = n()
  ) 

ggplot(summary.data, aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point(aes(size = count, alpha=.01)) +
  coord_quickmap()
  

## Work on September 17, 2016 ##
filter(rlc.tickets, is.na(location))
unique.locations <- as.data.frame(unique(rlc.tickets$location))
unique.locations2 <- as.data.frame(unique(rlc.tickets2$location))
colnames(unique.locations) <- c("address")
unique.locations <- arrange(unique.locations, address)

rlc.tickets <- mutate(rlc.tickets,
                      month = substr(timestamp, 6, 7),
                      day = substr(timestamp, 9, 10),
                      time = substr(timestamp, 12, 19)
                      )
rlc.tickets <-mutate(rlc.tickets,
                     date = as.Date(substr(timestamp, 1, 10))
                     )

filter(rlc, ADDRESS == "4800 N CICERO AVE")

rlc.tickets.month <- rlc.tickets %>%
  group_by(location, date) %>%
  summarize(
    count = n()
  )

rlc.total.month <- rlc.tickets %>%
  group_by(year, month) %>%
  summarize(
    count = n()
  )

rlc.totals <- rlc.tickets %>%
  group_by(location) %>%
  summarize(
    count = n()
  )

rlc.totals <- arrange(rlc.totals, desc(count))

Ashland0 <- filter(rlc.tickets.month, location == "0 N ASHLAND AVE")
Belmont400 <- filter(rlc.tickets.month, location == "400 W BELMONT AVE")
Hollywood1000 <- filter(rlc.tickets.month, location == "1000 W HOLLYWOOD AVE")

rlc.total.month$idu <- rownames(rlc.total.month)

ggplot(data = rlc.total.month, mapping = aes(x = as.integer(idu), y = count)) +
  geom_line()

ggplot(data = Belmont400, mapping = aes(x = date, y = count)) + 
  geom_line()

ggplot(data = Hollywood1000, mapping = aes(x = date, y = count)) +
  geom_line()

rlc.tickets.day <- rlc.tickets2 %>%
  group_by(location, date) %>%
  summarize(
    count = n()
  )

rlc.tickets.month <- rlc.tickets2 %>%
  group_by(location)

rlc.tickets.year <- rlc.tickets2 %>%
  group_by(location, year) %>%
  summarize(
    count = n()
  )

summary.year <- summarySE(rlc.tickets.year, measurevar = "count", groupvars = c("year", "NS"))

rlc.tickets.year <- merge(rlc.tickets.year, unique.cams4, by = "location")
summary.NS <- summarySE(rlc.tickets.day, measurevar = "count", groupvars = c("date", "NS"))

rlc.tickets.year$NS <- ifelse(rlc.tickets.year$latitude > 41.8755529, "north", "south")

ggplot(summary.NS, aes(x=date, y=count, colour=NS, group=NS)) + 
  geom_errorbar(aes(ymin=count-ci, ymax=count+ci), colour=NS, width=.1) +
  geom_line() +
  geom_point(size=1)

ggplot(summary.year, aes(x=date, y=count, colour=NS, group=NS)) + 
  geom_errorbar(aes(ymin=count-ci, ymax=count+ci), width=.1) +
  geom_line() +
  geom_point(size=1)

ggplot(summary.hour, aes(x=hour, y=count, colour=NS, group=NS)) + 
  geom_errorbar(aes(ymin=count-ci, ymax=count+ci), width=.1) +
  geom_line() +
  geom_point(size=1)

summary.hour <- summarySE(rlc.tickets.hour, measurevar = "count", groupvars = c("hour", "NS"))

rlc.tickets.year <- rlc.tickets %>%
  group_by(location, year) %>%
  summarize(
    count = n()
  )

rlc.tickets.month <- rlc.tickets %>%
  group_by(location, year, month) %>%
  summarize(
    count = n()
  )

rlc.tickets.month <- mutate(rlc.tickets.month,
       time.index = (as.integer(year) - 2007) * 12 + as.integer(month)
       )

rlc.tickets <- mutate(rlc.tickets,
                            month.index = (as.integer(year) - 2007) * 12 + as.integer(month)
)

rlc.tickets <- mutate(rlc.tickets,
                      season.index = (as.integer(year) - 2007) * 4 + (as.integer(month) + 4) %/% 4)

ggplot(data = rlc.tickets.month) +
  geom_line(mapping = aes(x = time.index, y = count, group = location),
             show.legend = FALSE, alpha = 0.15
             )

rlc.tickets.season <- rlc.tickets %>%
  group_by(location, season.index) %>%
  summarize(
    count = n()
  )
  
ggplot(data = rlc.tickets.season) +
  geom_line(mapping = aes(x = season.index, y = count, group = location),
            show.legend = FALSE, alpha = 0.15
  )  

rlc.tickets <- mutate(rlc.tickets,
                      car.type = ifelse(car == "PAS", "PAS", 
                                        ifelse(car == "TMP", "TMP", 
                                               ifelse(car == "TRK", "TRK", 
                                                      ifelse(car == "TXI", "TXI", "OTH")))
                      )
                      )
  

rlc.tickets.car <- rlc.tickets %>%
  group_by(year, car.type) %>%
  summarize(
    count = n()
  )

rlc.tickets.car <- rlc.tickets %>%
  group_by(season.index, car.type) %>%
  summarize(
    count = n()
  )

rlc.tickets.month <- rlc.tickets %>%
  group_by(location, month.index) %>%
  summarize(
    count = n()
  )

rlc.tickets.month <- merge(rlc.tickets.month, unique.cams4, by = "location")
rlc.tickets.month$NS <- ifelse(rlc.tickets.month$latitude > 41.8755529, "north", "south")
summary.month <- summarySE(rlc.tickets.month, measurevar = "count", groupvars = c("month.index", "NS"))
pd <- position_dodge(0.1)
ggplot(summary.month, aes(x=month.index, y=count, colour=NS, group=NS)) + 
  geom_errorbar(aes(ymin=count-ci, ymax=count+ci), position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size=3)

ggplot(summary.year, aes(x=year, y=count, colour=NS, group=NS)) + 
  geom_errorbar(aes(ymin=count-ci, ymax=count+ci), position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size=3)

ggplot(data = rlc.tickets.car) +
  geom_line(mapping = aes(x = season.index, y = count, group = car.type, color = car.type)
  )  

rlc.tickets <- mutate(rlc.tickets,
                      time = as.integer(substr(time, 1, 2)) * 60 + as.integer(substr(time, 4, 5)))
rlc.tickets <- mutate(rlc.tickets,
                      day.binary = ifelse(time >= 0 & time < 720, "AM", "PM"))

rlc.tickets.AMPM <- rlc.tickets %>%
  group_by(day.binary) %>%
  summarize(
    count = n()
  )

rlc.tickets.AMPM2 <- rlc.tickets %>%
  group_by(month.index, car.type, day.binary) %>%
  summarize(
    count = n()
  )

rlc.tickets.AMPM2 <- mutate(rlc.tickets.AMPM2,
                            car.label = paste(car.type, day.binary))

rlc.tickets <- mutate(rlc.tickets,
                      hour = (time + 60) %/% 60)

ggplot(data = rlc.tickets.AMPM2) +
  geom_line(mapping = aes(x = month.index, y = count, group = car.label, color = car.label)
  ) 

rlc.tickets.time <- rlc.tickets %>%
  group_by(hour) %>%
  summarize(
    count = n()
  )

rlc.tickets.hour <- rlc.tickets2 %>%
  group_by(location, hour) %>%
  summarize(
    count = n()
  )

rlc.tickets.hour <- merge(rlc.tickets.hour, unique.cams4, by = "location")
rlc.tickets.hour$NS <- ifelse(rlc.tickets.hour$latitude > 41.8755529, "north", "south")

rlc.tickets.days <- rlc.tickets %>%
  group_by(location, day, hour) %>%
  summarize(
    count = n()
  )

rlc.tickets <- mutate(rlc.tickets,
                      day = weekdays(date)
                      )

ggplot(data = rlc.tickets.hour) +
  geom_line(mapping = aes(x = hour, y = count, group = location),
            alpha = 0.15
            )

ggplot(data = rlc.tickets.day) +
  geom_line(mapping = aes(x = date, y = count, group = location),
            alpha = 0.15
  )

ggplot(data = rlc.tickets.year) +
  geom_line(mapping = aes(x = year, y = count, group = location),
            alpha = 0.15)

max(rlc.tickets.year$count)
subset(rlc.tickets.year, rlc.tickets.year$count == 19800)
filter(rlc.tickets.year, location == "4200 S CICERO AVENUE")

ggplot(rlc.tickets.days, mapping = aes(x = hour, y = count, group = location)) +
  geom_line(alpha = 0.15) +
  facet_wrap(~ day, nrow = 2)

rlc.tickets.days$day <- factor(
  rlc.tickets.days$day, 
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

rlc.tickets.days %>%
  filter(day == "Saturday") %>%
  ggplot(mapping = aes(x = hour, y = count, group = location)) +
  geom_line(alpha = 0.15)





### Means and Standard Deviations ###
monthSE <- summarySE(rlc.tickets.month, measurevar = "count", groupvars = "month")

ggplot(monthSE, aes(x=month, y=count)) + 
  geom_errorbar(aes(ymin=count-ci, ymax=count+ci), width=.1) +
  geom_line() +
  geom_point(size=3)


filter(rlc, ADDRESS == "3100 S DR MARTIN L KING")
rlc.tickets <- rlc.tickets2
rlc.tickets2$location <- as.factor(rlc.tickets2$location)
rlc.tickets2$location[rlc.tickets2$location == "5000 S ARCHER AVENUE"] <- "5000 S ARCHER AVE"
unique.locations <- as.data.frame(unique(rlc.tickets$location))
colnames(unique.locations) <- "ADDRESS"
merge(unique.locations, unique )

rlc.tickets2 <- rlc.tickets
colnames(unique.cams4) <- c("location", "latitude", "longitude")
rlc.tickets2 <- merge(rlc.tickets2, unique.cams4, by = "location")
filter(rlc.tickets2, is.na(longitude))
rlc.tickets2$NS <- ifelse(rlc.tickets2$latitude > 41.8755529, "north", "south")
summary.NS <- summarySE(rlc.tickets2, measurevar = )

### summarySE code ###
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

unique.cams[unique.cams$ADDRESS == "3216 W ADDISON ST", ]







## October 20, 2016 ##

mean(rlc.tickets.day$count)
sd(rlc.tickets.day$count)
ggplot(rlc.tickets.day, aes(count)) +
  geom_histogram(binwidth = 5)
max(rlc.tickets.day$count)
quantile(rlc.tickets.day$count, probs = seq(0, 1, 0.01))

### Cameras with more than 50 tickets in a day ###
TicketDay100 <- filter(rlc.tickets.day, count > 99)
unique(TicketDay100$location) 


street = "6400 W ARCHER AVE"
filter(rlc.tickets.day, location == street) %>%
  ggplot(., aes(x = date, y = count)) +
  geom_line() + 
  labs(title = street)

### In-state vs. Out-of-state Plates ###

rlc.tickets.instate <- filter(rlc.tickets, state == "IL")
rlc.tickets.outofstate <- filter(rlc.tickets, state != "IL")

rlc.tickets.outofstate %>%
  group_by(date) %>%
  summarize(
    count = n()
  ) %>%
  ggplot(., aes(x = date, y = count)) +
  geom_line()

rlc.tickets.instate %>%
  group_by(month.index) %>%
  summarize(
    count = n()
  ) %>%
  ggplot(., aes(x = month.index, y = count)) +
  geom_line()

cams.by.month <- select(rlc.tickets, month.index, location) %>%
  group_by(month.index) %>%
  summarize(
    cams = length(unique(location))
  )

### Assigning quadrants ###
attach(rlc.tickets.year)
lat = 41.8726365
lon = -87.6659617
rlc.tickets.year$quad <- as.factor(ifelse(latitude > lat & latitude + 0.583858 * longitude > -9.269783, "NE", 
               ifelse(latitude > lat & latitude + 0.583858 * longitude < -9.269783, "NW",
                      ifelse(latitude < lat & longitude > lon, "SE", "SW"))))


map <- get_map(location = 'Chicago', zoom = 11)
ggmap(map) +
  geom_point(data = unique.cams4, mapping = aes(x = longitude, y = latitude, color = Quad)) + 
  geom_abline(intercept = -9.269783, slope = -0.583858)

summary.year2 <- summarySE(rlc.tickets.year, measurevar = "count", groupvars = c("year", "quad"))

ggplot(summary.year2, aes(x=year, y=count, colour=quad, group=quad)) + 
  geom_errorbar(aes(ymin=count-ci, ymax=count+ci), width=.1) +
  geom_line() +
  geom_point(size=1)

summary.month2 <- summarySE(rlc.tickets.month, measurevar = "count", groupvars = c("month.index", "quad"))

ggplot(summary.month2, aes(x=month.index, y=count, colour=quad, group=quad)) + 
  geom_errorbar(aes(ymin=count-ci, ymax=count+ci), width=.1) +
  geom_line() +
  geom_point(size=1)

rlc.tickets.day.mean <- rlc.tickets.day %>% group_by(location) %>%
  summarize(
    mean = mean(count)
  ) %>%
  arrange(., desc(mean))

quantile(rlc.tickets.day.mean$mean, probs = seq(0, 1, 0.05))

ggplot(rlc.tickets.day.mean, aes(mean)) +
  geom_histogram(binwidth = 3) +
  labs(title = "Mean Tickets Issued Per Camera Per Day")

rlc.total.month$idu <- as.integer(rlc.total.month$idu)

ggplot(rlc.total.month, aes(x = idu, y = count)) + 
  geom_line()

### November 18 Work ###

total.ts <- ts(rlc.total.month$count, start = c(2007, 1), end = c(2014, 3), frequency = 12)
acf(total.ts)
plot(decompose(total.ts))
total.decom <- decompose(total.ts)
plot(ts(total.decom$random[7:81]))
acf(total.decom$random[7:81])
sd(total.ts[7:81])
sd(total.ts[7:81] - total.decom$trend[7:81])
sd(total.decom$random[7:81])


plot(stl(total.ts, s.window = "periodic"))







### December 1, 2016 ###

## Finding cameras that have been used throughout the data period ##
early.cams <- as.vector(rlc.tickets2 %>% filter(year < 2008) %>%
  distinct(location))

early.cams.tickets <- filter(rlc.tickets2, rlc.tickets2[, 1] %in% early.cams$location)

early.late.cams <- as.vector(early.cams.tickets %>% filter(year > 2013) %>% distinct(location))

full.cams.tickets <- filter(early.cams.tickets, early.cams.tickets[, 1] %in% early.late.cams$location)

full.cams.loc <- select(full.cams.tickets, location, latitude, longitude) %>% distinct(location, latitude, longitude)

detach(full.cams.loc)
full.cams.loc$quad <- as.factor(ifelse(latitude > lat & latitude + 0.583858 * longitude > -9.285, "NE", 
                                          ifelse(latitude > lat & latitude + 0.583858 * longitude < -9.285, "NW",
                                                 ifelse(latitude < lat & longitude > lon, "SE", "SW"))))

## Plotting full.cams ##
ggmap(map) +
  geom_point(data = full.cams.loc, mapping = aes(x = longitude, y = latitude, color = quad))

table(full.cams.loc$quad)


## Plot of total tickets by month, 'quadrant' ##




## Plot by date ##
full.cams.tickets %>%
  group_by(quad, date) %>%
  summarize(
    count = n()
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = date, y = count, group = quad, color = quad),
            alpha = 0.50
  )

## Plot by month ##
full.cams.tickets %>%
  group_by(quad, month.index) %>%
  summarize(
    count = n()
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = month.index, y = count, group = quad, color = quad),
            alpha = 0.50
  )

## What are these low days? ##
full.cams.tickets %>%
  group_by(quad, date) %>%
  summarize(
    count = n()
  ) %>%
  filter(count < 15) %>%
  print(n = 22)

## Plotting by mean ##
full.cams.tickets %>%
  group_by(location, month.index, quad) %>%
  summarize(
    count = n()
  ) %>%
  group_by(quad, month.index) %>%
  summarize(
    mean = mean(count)
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = month.index, y = mean, group = quad, color = quad),
            alpha = 0.50) +
  scale_x_continuous(minor_breaks = seq(0, 84, 6), breaks = seq(0, 84, 12)
  )

full.cams.tickets %>%
  group_by(location, month.index, quad) %>%
  summarize(
    count = n()
  ) %>%
  group_by(month.index) %>%
  summarize(
    mean = mean(count)
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = month.index, y = mean),
            alpha = 0.50) +
  scale_x_continuous(minor_breaks = seq(0, 84, 6), breaks = seq(0, 84, 12)
  )

## Checking for daily outliers ##
full.cams.tickets %>% 
  group_by(location, date) %>%
  summarize(
    count = n()
  ) %>%
  ungroup %>%
  select(count) %>%
  unlist %>%
  quantile(., probs = seq(0, 1, .01))

full.cams.tickets %>%
  group_by(location, date) %>%
  summarize(
    count = n()
  ) %>%
  filter(count > 100) %>%
  print(n = 40)

## Outliers by quad ##
full.cams.tickets %>%
  group_by(quad, date) %>%
  summarize(
    count = n()
  ) %>%
  ungroup %>%
  select(count) %>%
  unlist %>%
  quantile(., probs = seq(0, 1, .01))

quad.outlier.high <- full.cams.tickets %>%
  group_by(quad, date) %>%
  summarize(
    count = n()
  ) %>%
  filter(count > 356)

quad.outlier.low <- full.cams.tickets %>%
  group_by(quad, date) %>%
  summarize(
    count = n()
  ) %>%
  filter(count < 36)

## NE quad 2009-05-15 ##
filter(full.cams.tickets, date == "2009-05-15", quad == "NE")

## Batch plot of full cam dataframe ##

plotLineBatch <- function(df) {
  x <- df %>%
    group_by(location, date) %>%
    summarize(
      count = n()
    )
  locdf <- distinct(df, location)
  loc <- locdf$location
  for (i in 1:length(loc)) {
    a <- filter(x, location == loc[i])
    plot <- ggplot(a) + geom_line(mapping = aes(x = date, y = count), color = "black") + labs(title = loc[i])
    ggsave(plot, filename = paste("fullcamplot", i, ".png", sep = ""))
  }
}


plotLineBatch(full.cams.tickets)

## Finding outliers in individual cams ##
outliers <- full.cams.tickets %>%
  group_by(location, date) %>%
  summarize(
    count = n()
  ) %>%
  filter((abs(count - mean(count)) > 5 * sd(count)))





outlierCheck <- function(df) {
  locdf <- distinct(df, location)
  loc <- locdf$location
  x <- filter(df, location == loc[1]) %>%
    group_by(date) %>%
    summarize(
      count = n()
    )
  m <- mean(x$count)
  sd <- sd(x$count)
  y <- filter(x, count > mean + 3 * sd)
  for (i in 2:length(loc)) {
    a <- filter(df, location == loc[i]) %>%
      group_by(date) %>%
      summarize(
        count = n()
      )
    m <- mean(x$count)
    sd <- sd(x$count)
    s <- filter(x, count > mean + 3 * sd)
    y <- merge(y, s)
  }
}

outlierCheck(full.cams.tickets)


### January 5, 2017 ###
## Monthly aggregate as time series ##

monthly.total.ts <- ts(rlc.total.month$count, start = c(2007, 1), deltat = 1/12)

## Plotting monthly.total.ts ##
autoplot(as.zoo(monthly.total.ts), geom = c("line", "point"))

## Filtering data to > 2008 and < 2014
monthly.0813.ts <- window(monthly.total.ts, start = c(2008, 1), end = c(2013, 12))

## Plotting subsetted data ##
autoplot(as.zoo(monthly.0813.ts), geom = c("line", "point"))

## Decompose subsetted data ##
plot(decompose(monthly.0813.ts))

## ACF and PACF ##
monthlyacf <- acf(monthly.0813.ts, plot = FALSE)
monthlypacf <- pacf(monthly.0813.ts, plot = FALSE)
monthlyacf$lag <- monthlyacf$lag * 12
monthlypacf$lag <- monthlypacf$lag * 12
plot(monthlyacf)
plot(monthlypacf)

## Determining best model ##
best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:3) for (j in 0:3) {
  fit.aic <- AIC(arima(monthly.0813.ts, order = c(i, 0, j), method="ML"))
  if (fit.aic < best.aic) {
    best.order <- c(i, 0, j)
    best.arma <- arima(monthly.0813.ts, order = best.order, method="ML")
    best.aic <- fit.aic
  }
}

## Plotting prediction ##
new.time <- seq(length(monthly.0813.ts), length = 12)
predict.arma <- predict(best.arma, n.ahead = 12)
monthly.pred <- ts((predict.arma$pred), start = 2014,
                freq = 12)
combined.ts <- ts.union(monthly.0813.ts, monthly.pred, dframe = FALSE)
autoplot(as.zoo(combined.ts), geom = c("line", "point"))
combined.ts
combined.ts <- pmax(combined.ts[,1], combined.ts[,2], na.rm = TRUE)

best.arma

### January 16, 2017 fucking around ###
tsdisplay(diff(monthly.total.ts))

### January 25, 2017 ###

## Number of full cameras by quad ##
full.cams.by.month.quad <- full.cams.tickets %>%
  select(location, quad, month.index) %>%
  group_by(quad, month.index) %>%
  summarize(
    count = length(unique(location))
  )

## Number of full cameras ##
full.cams.by.month <- select(full.cams.tickets, month.index, location) %>%
  group_by(month.index) %>%
  summarize(
    cams = length(unique(location))
  )

ggplot(data = full.cams.by.month.quad) + 
  geom_line(mapping = aes(x = month.index, y = count, group = quad, color = quad),
            alpha = 0.50)

ggplot(data = full.cams.by.month) + 
  geom_line(mapping = aes(x = month.index, y = cams))

## Total number of cameras by quad ##
cams.by.month.quad <- rlc.tickets2 %>%
  select(location, quad, month.index) %>%
  group_by(quad, month.index) %>%
  summarize(
    count = length(unique(location))
  )

ggplot(data = cams.by.month.quad) + 
  geom_line(mapping = aes(x = month.index, y = count, group = quad, color = quad),
            alpha = 0.50)

ggplot(data = cams.by.month) + 
  geom_line(mapping = aes(x = month.index, y = cams))

## Changing quads in full.cams.tickets ##
attach(full.cams.tickets)
full.cams.tickets$quad <- as.factor(ifelse(latitude > lat & latitude + 0.583858 * longitude > -9.285, "NE", 
                                           ifelse(latitude > lat & latitude + 0.583858 * longitude < -9.285, "NW",
                                                  ifelse(latitude < lat & longitude > lon, "SE", "SW"))))

attach(rlc.tickets2)
rlc.tickets2$quad <- as.factor(ifelse(latitude > lat & latitude + 0.583858 * longitude > -9.285, "NE", 
                                           ifelse(latitude > lat & latitude + 0.583858 * longitude < -9.285, "NW",
                                                  ifelse(latitude < lat & longitude > lon, "SE", "SW"))))


## Testing auto.arima ##
autoarimatest <- auto.arima(monthly.0813.ts)
plot(forecast(autoarimatest, h = 36))


## Installing Autobox? ##
## WINDOWS INSTALLATION ##
install.packages("autobox_64.0.1.zip")

## Separate ts objects for each quad ##
# NE #
NE.full <- full.cams.tickets %>%
  filter(quad == "NE") %>%
  group_by(quad, month.index) %>%
  summarize(
    count = n()
  )
NE.full.ts <- ts(NE.full$count, start = c(2007, 1), deltat = 1/12)
NE.full.ts

autoplot(as.zoo(NE.full.ts), geom = c("line", "point"))

NE.monthly.0913.ts <- window(NE.full.ts, start = c(2009, 1), end = c(2013, 12))

# NW #
NW.full <- full.cams.tickets %>%
  filter(quad == "NW") %>%
  group_by(quad, month.index) %>%
  summarize(
    count = n()
  )
NW.full.ts <- ts(NW.full$count, start = c(2007, 1), deltat = 1/12)
NW.full.ts

autoplot(as.zoo(NW.full.ts), geom = c("line", "point"))

NW.monthly.0913.ts <- window(NW.full.ts, start = c(2009, 1), end = c(2013, 12))

#SE
SE.full <- full.cams.tickets %>%
  filter(quad == "SE") %>%
  group_by(quad, month.index) %>%
  summarize(
    count = n()
  )
SE.full.ts <- ts(SE.full$count, start = c(2007, 1), deltat = 1/12)
SE.full.ts

autoplot(as.zoo(SE.full.ts), geom = c("line", "point"))

SE.monthly.0913.ts <- window(SE.full.ts, start = c(2009, 1), end = c(2013, 12))

# SW #
SW.full <- full.cams.tickets %>%
  filter(quad == "SW") %>%
  group_by(quad, month.index) %>%
  summarize(
    count = n()
  )
SW.full.ts <- ts(SW.full$count, start = c(2007, 1), deltat = 1/12)
SW.full.ts

autoplot(as.zoo(SW.full.ts), geom = c("line", "point"))

SW.monthly.0913.ts <- window(SW.full.ts, start = c(2009, 1), end = c(2013, 12))


## auto.arima for quad data ##
auto.arima.NE <- auto.arima(NE.monthly.0913.ts, stepwise=FALSE, approximation=FALSE)
plot(forecast(auto.arima.NE, h = 24))

auto.arima.NW <- auto.arima(NW.monthly.0913.ts, stepwise=FALSE, approximation=FALSE)
plot(forecast(auto.arima.NW, h = 24))

auto.arima.SE <- auto.arima(SE.monthly.0913.ts, stepwise=FALSE, approximation=FALSE)
plot(forecast(auto.arima.SE, h = 24))

auto.arima.SW <- auto.arima(SW.monthly.0913.ts, stepwise=FALSE, approximation=FALSE)
plot(forecast(auto.arima.SW, h = 24))

auto.arima.NE
auto.arima.NW
auto.arima.SE
auto.arima.SW

## auto.arima for full data ##

auto.arima.total <- auto.arima(monthly.0813.ts, stepwise = FALSE, approximation = FALSE)
plot(forecast(auto.arima.total, h = 24))

auto.arima.total

## auto.arima for total data ##
monthly.window.ts <- window(monthly.total.ts, start = c(2007, 1), end = c(2013, 12))
auto.arima.all <- auto.arima(monthly.window.ts, stepwise = FALSE, approximation = FALSE)
plot(forecast(auto.arima.all, h = 24))

SE.full.ts
SE.monthly.0913.ts




### January 27, 2017 ###
## Time series for quads without restrictions ##

#SE
SE.total <- rlc.tickets2 %>%
  filter(quad == "SE") %>%
  group_by(quad, month.index) %>%
  summarize(
    count = n()
  )
SE.total.ts <- ts(SE.total$count, start = c(2008, 1), end = c(2013, 12), deltat = 1/12)

#SW
SW.total <- rlc.tickets2 %>%
  filter(quad == "SW") %>%
  group_by(quad, month.index) %>%
  summarize(
    count = n()
  )
SW.total.ts <- ts(SW.total$count, start = c(2008, 1), end = c(2013, 12), deltat = 1/12)

#NW
NW.total <- rlc.tickets2 %>%
  filter(quad == "NW") %>%
  group_by(quad, month.index) %>%
  summarize(
    count = n()
  )
NW.total.ts <- ts(NW.total$count, start = c(2008, 1), end = c(2013, 12), deltat = 1/12)

#NE
NE.total <- rlc.tickets2 %>%
  filter(quad == "NE") %>%
  group_by(quad, month.index) %>%
  summarize(
    count = n()
  )
NE.total.ts <- ts(NE.total$count, start = c(2008, 1), end = c(2013, 12), deltat = 1/12)

auto.arima.total.NE <- auto.arima(NE.total.ts, stepwise=FALSE, approximation=FALSE)
plot(forecast(auto.arima.total.NE, h = 24))

auto.arima.total.NW <- auto.arima(NW.total.ts, stepwise=FALSE, approximation=FALSE)
plot(forecast(auto.arima.total.NW, h = 24))

auto.arima.total.SE <- auto.arima(SE.total.ts, stepwise=FALSE, approximation=FALSE)
plot(forecast(auto.arima.total.SE, h = 24))

auto.arima.total.SW <- auto.arima(SW.total.ts, stepwise=FALSE, approximation=FALSE)
plot(forecast(auto.arima.total.SW, h = 24))

### February 3, 2017 ###
## Load in weather data ##
ChiWeather <- read.csv("879182.csv")

## Clean date ##
ChiWeather$DATE <- as.character(ChiWeather$DATE)
ChiWeather$DATE <- as.Date(ChiWeather$DATE, format = "%Y%m%d")
ChiWeather$YEAR <- substr(as.character(ChiWeather$DATE), 1, 4)

## Subset data ##
ChiWeather <- filter(ChicagoWeather, STATION_NAME == "CHICAGO 6.0 NNE IL US")
ChiWeather2 <- filter(ChicagoWeather, STATION_NAME == "CHICAGO 6.8 NNE IL US")
ChiWeather3 <- filter(ChicagoWeather, STATION_NAME == "CHICAGO 6.5 NNE IL US")
ChiWeather4 <- filter(ChicagoWeather, STATION_NAME == "CHICAGO 3.0 NW IL US ")
OHareWeather <- filter(ChiWeather, STATION_NAME == "CHICAGO OHARE INTERNATIONAL AIRPORT IL US")

## Subset stations ##
WeatherStations <- unique(ChicagoWeather$STATION_NAME)
WeatherStations

## Plot weather data ##
ggplot(data = ChiWeather2, mapping = aes(x = DATE, y = PRCP)) + 
  geom_line()

## Weather Year ##
ChiWeather2$YEAR <- substr(as.character(ChiWeather2$DATE), 1, 4)

## Subset Chicago 2010 Weather ##
ChiWeather2010 <- filter(ChiWeather2, YEAR == "2010")

## Plot 2010 weather data ##
ggplot(data = ChiWeather2010, mapping = aes(x = DATE, y = PRCP)) + geom_line()

rlc.tickets.by.day <- MASTER %>%
  group_by(date) %>%
  summarize(
    count = n()
  )
rlc.tickets.by.day$year <- substr(as.character(rlc.tickets.by.day$date), 1, 4)
rlc.tickets.2010 <- filter(rlc.tickets.by.day, year == "2010")
Plot1 <- ggplot(data = ChiWeather2010, mapping = aes(x = DATE, y = PRCP)) + geom_line()
Plot2 <- ggplot(data = rlc.tickets.2010, mapping = aes(x = date, y = count)) + geom_line()

grid.arrange(Plot1, Plot2, ncol=2)

## Graph function ##
WeatherTicket <- function(yr, data) {
  x <- filter(data, YEAR == yr)
  y <- filter(rlc.tickets.by.day, year == yr)
  p1 <- ggplot(data = x, mapping = aes(x = DATE, y = PRCP)) + geom_line()
  p2 <- ggplot(data = y, mapping = aes(x = date, y = count)) + geom_line()
  grid.arrange(p1, p2, nrow = 2)
}

WeatherTicket(2011)
WeatherTicket(2008)
WeatherTicket(2010, OHareWeather)

## Population Data ##
PopData <- read.csv("UNdata_Export_20170203_145040771.csv")
ChiPop <- filter(PopData, City == "Chicago (IL)")

write.csv(rlc.tickets2, file = "tickets.csv")

### February 9, 2017 ###
## Import Chicago temperature data ##
ChicagoTemps <- read.csv("886711.csv")
ChicagoTemps$DATE <- as.character(ChicagoTemps$DATE)
ChicagoTemps$DATE <- as.Date(ChicagoTemps$DATE, format = "%Y%m%d")
unique(ChicagoTemps$STATION_NAME)
OHareTemps <- filter(ChicagoTemps, STATION_NAME == "CHICAGO OHARE INTERNATIONAL AIRPORT IL US")

## Combining Weather data frames ##
ChiClimate <- merge(ChiWeather, ChicagoTemps, by=c("STATION", "DATE", "STATION_NAME", "TOBS"))
OHare <- filter(ChiClimate, STATION_NAME == "CHICAGO OHARE INTERNATIONAL AIRPORT IL US")
OHare$YEAR <- substr(as.character(OHare$DATE), 1, 4)
OHare$YEAR <- as.integer(OHare$YEAR)

## Limiting Data to 2007-2013
DayTickets <- filter(rlc.tickets.by.day, year < 2014)
OHare <- filter(OHare, YEAR < 2014)

## Testing covariance/correlation ##
cov(OHare$TMAX, DayTickets$count)
cor(OHare$TMAX, DayTickets$count)
cov(OHare$PRCP, DayTickets$count)
cor(OHare$PRCP, DayTickets$count)

colnames(OHare)[2] <- "date"
Weather <- merge(OHare, DayTickets, by = "date")
diffTempTix <- data.frame(diff(Weather$count), diff(Weather$TMAX))
colnames(diffTempTix) <- c("diffTix", "diffTMax")

ggplot(data = diffTempTix) +
  geom_point(aes(x = diffTMax, y = diffTix))

cor(diffTempTix$diffTix, diffTempTix$diffTMax)

ggplot(data = Weather) +
  geom_point(aes(x = TMAX, y = count)) +
  theme_bw() +
  labs(x = "Max Temperature", y = "Tickets")

## Plotting tickets vs. temps ##
ggplot(data = OHare, mapping = aes(x = TMAX, y = TICKETS)) +
  geom_point()
ggplot(data = OHare, mapping = aes(x = PRCP, y = TICKETS)) +
  geom_point()

## Diffs ##
diffTemps <- diff(OHare$TMAX)
diffPrcp <- diff(OHare$PRCP)
diffTix <- diff(OHare$TICKETS)

ggplot(mapping = aes(x = diffPrcp, y = diffTix)) +
  geom_point()

cor(diffTemps, diffTix)
cor(diffPrcp, diffTix)

ggplot(data = OHare) +
  geom_line(mapping = aes(x = DATE, y = TMAX)) + 
  geom_line(mapping = aes(x = DATE, y = TICKETS))

Plot1 <- ggplot(data = OHare, mapping = aes(x = DATE, y = TMAX)) + geom_line()
Plot2 <- ggplot(data = OHare, mapping = aes(x = DATE, y = PRCP)) + geom_line()
Plot3 <- ggplot(data = OHare, mapping = aes(x = DATE, y = TICKETS)) + geom_line()

grid.arrange(Plot2, Plot3, nrow=2)



### February 15, 2017 ###
## Trying to get unique coordinates ##
locations <- unique(select(rlc.tickets2, location, latitude, longitude))

locationslist <- list(locations$longitude, locations$latitude)
sapply(locationslist, revgeocode)
locationslist

addresses <- do.call(rbind,
        lapply(1:nrow(locations),
               function(i)revgeocode(as.numeric(locations[i,3:2]))))

locationsX <- select(locations, location, zip)

test <- locations
MASTER$longitude <- ifelse(MASTER$longitude == 87.6706585, -87.6706585, MASTER$longitude)

test[test$location == "1600 W DIVERSEY PARKWA", 3] <- -87.67066

revgeocode(c(-87.66680, 41.88145))
addresses[47] = revgeocode(c(-87.67066, 41.93241))
addresses[1] = "1599 W Madison St, Chicago, IL 60607, USA"
addresses[337] = "9500 S Halsted St, Chicago, IL 60628, USA"
locations[308, 4] <- 60617
zipcodes <- as.vector(str_sub(addresses, -10, -6))
locations$zip <- zipcodes
locations$zip <- as.factor(locations$zip)

## Import population ##
ChiPop <- read.csv("ChiPop1.csv")
options(digits = 9)
ChiPop$lat <- as.numeric(str_sub(ChiPop$Location, 1, 9))
colnames(ChiPopX)[1] <- "zip"
ChiPop$lon <- as.numeric(str_sub(ChiPop$Location, 11, 20))
ChiPop$Population <- as.numeric(gsub(",", "", as.character(ChiPop$Population)))
ChiPop$People.Sq.Mile <- as.numeric(gsub(",", "", as.character(ChiPop$People.Sq.Mile)))

ChiPopX <- select(ChiPop, Zip.Code, Population, People.Sq.Mile, ZipLat, ZipLon)
Test <- head(rlc.tickets2)
MASTER <- merge(rlc.tickets2, locationsX, by = "location")

ZipTix <- MASTER %>%
  group_by(zip) %>%
  summarize(
    count = n()
  )

ZipTix <- merge(ZipTix, ChiPopX, by = "zip")
ZipTix$adjcount <- ZipTix$count / 1000
ZipTix$adjpop <- ZipTix$Population / 30000
ZipTix$adjpopmi <- ZipTix$People.Sq.Mile / 10000

cov(ZipTix$count, ZipTix$Population)
cov(ZipTix$count, ZipTix$People.Sq.Mile)
cor(ZipTix$count, ZipTix$Population)
cor(ZipTix$count, ZipTix$People.Sq.Mile)

ggplot(data = ZipTix) +
  geom_point(aes(x = Population, y = count))
ggplot(data = ZipTix) +
  geom_point(aes(x = People.Sq.Mile, y = count))


## Mapping population ##
Plot1 <- ggmap(map) + 
  geom_point(aes(x = ZipLon, y = ZipLat, size = adjpop, color = adjcount), data = ZipTix) +
  scale_size(range = c(3, 10)) +
  scale_colour_gradient(low="blue", high="red")

Plot2 <- ggmap(map) + 
  geom_point(aes(x = ZipLon, y = ZipLat, size = adjpopmi, color = adjcount), data = ZipTix) +
  scale_size(range = c(3, 10)) +
  scale_colour_gradient(low="blue", high="red")

grid.arrange(Plot1, Plot2, ncol=2)


max(MASTER$date)

max(MASTER$longitude)
min(MASTER$longitude)
max(MASTER$latitude)
min(MASTER$latitude)


### February 23, 2017 ###
## Descriptive Statistics ##
# Graph of Tickets per month ##
MASTER %>% 
  group_by(month.index) %>%
  summarize(
    count = n()
  ) %>%
  ggplot() + geom_line(aes(x = month.index, y = count)) + labs(title = "Total Number of Red Light Tickets Issued Per Month", x = "Month", y = "Red Light Tickets Issued")

cam.monthly <- MASTER %>% 
  group_by(month.index, location) %>%
  summarize(
    count = n()
  ) %>%
  group_by(month.index) %>%
  summarize(
    mean = mean(count)
  )


  
ggplot(data = cam.monthly) + geom_line(aes(x = month.index, y = mean)) + labs(title = "Number of Red Light Tickets Issued Per Camera Per Month", x = "Month", y = "Red Light Tickets Issued")

### March 2, 2017 ###
## Tickets by quad ##

write.csv(MASTER %>% group_by(month.index, quad) %>%
            summarize(
              count = n()
            ), file = "Quads.csv")

## Cams by quad per month ##
cams.by.month.quad <- MASTER %>%
  select(location, quad, month.index) %>%
  group_by(quad, month.index) %>%
  summarize(
    count = length(unique(location))
  )

write.csv(cams.by.month.quad, file = "quadcams.csv")

attach(MASTER)
MASTER$quad2 <- as.factor(ifelse(latitude > lat & latitude + 0.583858 * longitude > -9.285, "NE", 
                                                       ifelse(latitude > lat & latitude + 0.583858 * longitude < -9.285, "NW",
                                                              ifelse(latitude < lat & longitude > lon, "SE", "SW"))))

MASTER %>%
  group_by(quad2, month.index) %>%
  summarize(
    count = n()
  ) %>%
  
  ggplot() + geom_line(aes(x = month.index, y = count, color = quad2))



MASTER %>%
  group_by(quad, month.index) %>%
  summarize(
    count = n()
  ) %>%
  filter(month.index < 85) %>%
  ggplot() + geom_line(aes(x = month.index, y = count, color = quad)) + labs(x = "Month Index", y = "Tickets") + theme_bw() + guides(color = guide_legend("Legend"))

cams.by.month.quad2 <- MASTER %>%
  select(location, quad2, month.index) %>%
  group_by(quad2, month.index) %>%
  summarize(
    count = length(unique(location))
  )

ggplot(data = cams.by.month.quad2) +
  geom_line(mapping = aes(x = month.index, y = count, group = quad2, color = quad2)) +
  geom_vline(xintercept = c(12, 24, 36, 48, 60, 72, 84), linetype = 2)

ggplot(data = rlc.total.month) + geom_line(mapping = aes(x = as.integer(idu), y = count)) +
  geom_vline(xintercept = c(12, 24, 36, 48, 60, 72, 84), linetype = 2)
ggplot(data = cams.by.month) + geom_line(mapping = aes(x = month.index, y = cams)) +
  geom_vline(xintercept = c(12, 24, 36, 48, 60, 72, 84), linetype = 2)

### Finding unique cameras between 2009 and 2013 ###
Cam0913 <- MASTER %>%
  select(location, year) %>%
  filter(year > 2008, year < 2014)
length(unique(Cam0913$location))
Cam0709 <- MASTER %>%
  select(location, year) %>%
  filter(year > 2006, year < 2009)
length(unique(Cam0709$location))

### Median tickets per month per camera ###
tix.cam.mon <- MASTER %>%
  group_by(location, month.index) %>%
  summarize(
    count = n()
  )

tix.cam.mon <- merge(tix.cam.mon, unique.cams4, by = "location")

monthly.quad.median <- tix.cam.mon %>%
  group_by(month.index, Quad) %>%
  summarize(
    median = median(count)
  )



ggplot(data = monthly.quad.median) + 
  geom_line(mapping = aes(x = month.index, y = median, group = Quad, color = Quad))

### Average and median cams ###

monthly.quad.averages <- tix.cam.mon %>%
  group_by(month.index) %>%
  summarize(
    mean = mean(count),
    median = median(count)
  )

ggplot(data = monthly.quad.averages) + 
  geom_line(mapping = aes(x = month.index, y = median)) +
  geom_line(mapping = aes(x = month.index, y = mean), linetype = 2)


### March 24, 2017 ###
## Extracting tickets per month ##
tickets.averages <- MASTER %>%
  group_by(location, month.index) %>%
  summarize(
    count = n()
  ) %>%
  group_by(month.index) %>%
  summarize(
    mean = mean(count),
    median = median(count)
  )

ggplot(data = tickets.averages) + 
  geom_line(mapping = aes(x = month.index, y = median)) + 
  geom_line(mapping = aes(x = month.index, y = mean), linetype = 2)

write.csv(tickets.averages, file = "tixavg.csv")

### Plotting mean tickets per camera by quad ###

p1 <- MASTER %>%
  filter(month.index < 85) %>%
  group_by(location, month.index, quad) %>%
  summarize(
    count = n()
  ) %>%
  group_by(quad, month.index) %>%
  summarize(
    mean = mean(count)
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = month.index, y = mean, group = quad, linetype = quad)) +
  labs(x = "Month Index", y = "Mean") +
  scale_x_continuous(minor_breaks = seq(0, 84, 6), breaks = seq(0, 84, 12)
  ) +
  theme_bw()

### Plotting total by quad ###

p2 <- MASTER %>%
  filter(month.index < 85) %>%
  group_by(month.index, quad) %>%
  summarize(
    count = n()
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = month.index, y = count, group = quad, linetype = quad)) +
  labs(x = "Month Index", y = "Count") +
  scale_x_continuous(minor_breaks = seq(0, 84, 6), breaks = seq(0, 84, 12)
  ) +
  theme_bw()

cams.by.month.quad <- as.data.frame(cams.by.month.quad)
p3 <- cams.by.month.quad %>%
  filter(month.index < 85) %>%
  ggplot() +
  geom_line(mapping = aes(x = month.index, y = count, group = quad, linetype = quad)) +
  labs(x = "Month Index", y = "Cameras") +
  scale_x_continuous(minor_breaks = seq(0, 84, 6), breaks = seq(0, 84, 12)
  ) +
  theme_bw()

multiplot(p2, p1, p3, rows = 3)


### Plotting tourists vs. non-tourists ###
MASTER$IL_plate <- as.factor(ifelse(MASTER$state == "IL", 1, 0))

outofstate <- MASTER %>%
  filter(month.index < 85) %>%
  group_by(month.index, IL_plate) %>%
  summarize(
    count = n()
   ) %>%
  filter(IL_plate == 0)

monthlytotal <- MASTER %>%
  filter(month.index < 85) %>%
  group_by(month.index) %>%
  summarise(
    count = n()
  )

outofstate$total <- monthlytotal$count
outofstate$pct <- outofstate$count / outofstate$total
ggplot(data = outofstate) +
  geom_line(aes(x = month.index, y = pct)) +
  theme_bw() +
  labs(x = "Month Index", y = "Percentage of total tickets") +
  scale_x_continuous(minor_breaks = seq(0, 84, 6), breaks = seq(0, 84, 12))

outofstate.cams <- MASTER %>%
  group_by(location, IL_plate) %>%
  summarize(
    count = n()
  ) %>%
  filter(IL_plate == 0)

total.cams <- MASTER %>%
  group_by(location) %>%
  summarize(
    count = n()
  )

outofstate.cams$total <- total.cams$count
outofstate.cams$pct <- outofstate.cams$count / outofstate.cams$total
arrange(outofstate.cams, desc(pct))
arrange(outofstate.cams, pct)
summary(outofstate.cams$pct)
sd(outofstate.cams$pct)
outofstate.cams$rank <- as.factor(ifelse(outofstate.cams$pct > 0.05515, "High",
                                    ifelse(outofstate.cams$pct > 0.02813, "Medium", "Low")))

outofstate.cams <- merge(outofstate.cams, unique.cams4, by = "location")
min(outofstate.cams$longitude)
outofstate.cams[outofstate.cams$location == "1600 W DIVERSEY PARKWA", 8] <- -87.67066

high.cams <- outofstate.cams %>%
  filter(rank == "High") 

ggmap(map3) +
    geom_point(data = high.cams, mapping = aes(x = longitude, y = latitude), size = 2) +
    theme(axis.title=element_blank())
