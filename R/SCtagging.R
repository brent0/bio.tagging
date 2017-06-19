
require(raster)
require(PBSmapping)
require(geosphere)
require(RMySQL)
require(chron)
require(gdistance)
require(rgeos)
require(maptools)
require(rJava)
require(stringr)
require(kmlbuilder)
require(lubridate)
require(jsonlite)
require(opencpu)
require(sp)


simulate.movement = function(movement.rate = NULL, canvas = NULL, release.pos = NULL, ndays = 365*4, bestof = 4, ncrab = 100 ,type = "least.cost"){
  if(is.null(canvas)) canvas = file.path("C:", "workspace","data", "maps", "depthraster2.tif")
  if(is.null(movement.rate)) movement.rate = ave(max.rate.tag(n = 10))
  if(is.null(release.pos)) release.pos = c(-59.9, 46.5)   
  raster.path = canvas
  r <- raster(raster.path)
  mr = as.matrix(r)
  mr[which(mr>-280 & mr< -60)] = -170
  mr = apply(mr, 2, function(x) dnorm(x,mean=-170,sd=60))
  r = setValues(r, mr)
  
  tr <- transition(r, mean, directions=16)
  if(type  == "random.walk"){
    trans <- geoCorrection(tr, type = "r", scl=FALSE)
  }
  if(type  == "least.cost"){
    trans <- geoCorrection(tr, type = "c", scl=FALSE)
  }
  crab = data.frame(1:100)
  crab$path = ""
  crab$curlon = release.pos[1]
  crab$curlat = release.pos[2]
  for(i in 1:ndays){
   
     for(c in 1:nrow(crab)){
  
       cost = 100000000000
       dir = sample(1:360, bestof, replace=TRUE)
       temppos = NULL
       for(b in 1:bestof){
         
         dp = destPoint(c(crab$curlon[c], crab$curlat[c]), dir[b], movement.rate)
         cd = costDistance(tr, c(crab$curlon[c], crab$curlat[c]), dp)
         
         if(cd < cost){
           temppos = dp
           cost = cd
         }
        }
        crab$path[c] = paste(crab$path[c], temppos[1], ",",temppos[2], sep ="")
        crab$curlon[c] = temppos[1]
        crab$curlat[c] = temppos[2]
       
       }
     }
  
  
}
max.rate.tag = function(n = 1){
  data = get.capturedata()
  ind = which(data$area == 'GULF')
  if(length(ind)>0) data = data[-ind,]
  
  ind = which(data$caplat == '0')
  if(length(ind)>0) data = data[-ind,]
  
  
  dist = distHaversine(cbind(as.numeric(data$rellon), as.numeric(data$rellat)), cbind(as.numeric(data$caplon) ,as.numeric(data$caplat)), r=6378137)
inter = interval(data$sampdat, data$capdate)
days = time_length(as.duration(inter), "days")




#Remove days less than 1 year, account for drift
data$dist = dist
data$days = days
ind = which(data$days <= 365 )
if(length(ind)>0) data = data[-ind,]
ind = which(is.na(data$days))
if(length(ind)>0) data = data[-ind,]


data$m.per.day = data$dist/data$days
data = data[order(data$m.per.day, decreasing = T),]

#remove gulf program data  
ind = which(grepl("G", data$PID))
if(length(ind)>0) data = data[-ind,]

data$m.per.day
 return(data$m.per.day[1:n])
}
generate.rewards = function(){
  
  try(if(!exists("ecomod.datadirectory"))
    stop("You must define the ecomod.datadirectory")
  )  
  
  try(if(!exists("enssnowc.user") | !exists("enssnowc.password"))
    stop("You must define enssnowc.user and enssnowc.password")
  )  
  
  
  system(paste("java -jar data/taggingApp.jar", enssnowc.user, enssnowc.password, ecomod.datadirectory, "reward", sep = " "))
  
}

enter.returns.private = function(){
  
  try(if(!exists("ecomod.datadirectory"))
    stop("You must define the ecomod.datadirectory")
  )  
  
  try(if(!exists("enssnowc.user") | !exists("enssnowc.password"))
    stop("You must define enssnowc.user and enssnowc.password")
  )  
  setwd(ecomod.datadirectory)
  
  system(paste("java -jar data/taggingApp.jar", enssnowc.user, enssnowc.password, ecomod.datadirectory, "enter", sep = " "))
  
}

enter.releases = function(){
  browseURL("http://www.enssnowcrab.com/snowcrabgroup/tagentry.html", browser = getOption("browser"),
            encodeIfNeeded = FALSE)
  
}

view.stats.private = function(){
  browseURL("http://www.enssnowcrab.com/snowcrabgroup/tagging.html", browser = getOption("browser"),
            encodeIfNeeded = FALSE)
  
}

view.stats.public = function(){
  browseURL("http://www.enssnowcrab.com/tagging.html", browser = getOption("browser"),
            encodeIfNeeded = FALSE)
  return(TRUE)
  
}

enter.returns.public = function(){
  browseURL("http://www.enssnowcrab.com/tagentry.html", browser = getOption("browser"),
            encodeIfNeeded = FALSE)
  
}

depthpath = file.path("C:", "workspace","data", "maps", "depthraster2.tif") #meters
shortestpaths.SC = function(raster.path, neighborhood = 16, type = "random.walk", redo = F){
  
  x = get.capturedata()
  trans = NULL
  r <- raster(raster.path)
  mr = as.matrix(r)
  mr[which(mr>-280 & mr< -60)] = -170
  mr = apply(mr, 2, function(x) dnorm(x,mean=-170,sd=60))
  r = setValues(r, mr)
  
  tr <- transition(r, mean, directions=neighborhood)
  if(type  == "random.walk"){
    trans <- geoCorrection(tr, type = "r", scl=FALSE)
  }
  if(type  == "least.cost"){
    trans <- geoCorrection(tr, type = "c", scl=FALSE)
  }
  
  local_port = "3309"
  SCtunnel = open.port.SC(local.port = local_port)
  
  con <- dbConnect(dbDriver("MySQL"), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  
  rs <- dbSendQuery(con, statement = "Select * from paths;")
  
  da <- fetch(rs, n = -1)   # extract all rows
  
  dbDisconnect(con) 
  close.port.SC(SCtunnel)
  
  dftowrite = NULL
  append = F
  overwrite = T
  if(!redo){
    append = T
    overwrite = F    
    goodind = which(paste(x$PID, x$capdate) %in% paste(da$id, da$cdat))
    if(length(goodind) > 0) x = x[-goodind,]
    zeroind = which(as.numeric(x$caplat) == 0 | x$caplat == 'unknown')
    if(length(zeroind) > 0) x = x[-zeroind,]
    
    
    
    for(i in 1:nrow(x)){
      
      start <- c(as.numeric(x$rellon[i]), as.numeric(x$rellat[i]))
      end <- c(as.numeric(x$caplon[i]), as.numeric(x$caplat[i]))
      
      if(abs(start[1] - end[1]) < res(trans)[1] && abs(start[2] - end[2]) < res(trans)[1] || is.na(cellFromXY(r, start)) || is.na(cellFromXY(r, end))){
        AtoB = rbind(start, end)
      }
      else{
        AtoB = shortestPath(trans, start, end, output="SpatialLines")
      }
      cor = data.frame(coordinates(AtoB))
      names(cor) = c("x", "y")
      xrep = cor$x[1]
      yrep = cor$y[1]
      for(k in 1:(nrow(cor)-1)){
        
        if(cor$x[k] == xrep){ cor$x[k] = start[1] }
        else{ xrep = 1000000 }
        
        if(cor$y[k] == yrep){ cor$y[k] =  start[2] }
        else{ yrep = 1000000 }
      }
      xrep = cor$x[nrow(cor)]
      yrep = cor$y[nrow(cor)]
      for(k in nrow(cor):2){
        if(cor$x[k] == xrep) cor$x[k] =  end[1]
        else xrep = 1000000
        
        if(cor$y[k] == yrep) cor$y[k] =  end[2]
        else yrep = 1000000
      }
      
      names(cor) = c("X", "Y")
      cor$PID = 1
      cor$POS = 1:nrow(cor)
        tpoly = as.PolySet(cor, projection = "LL")
        leng = calcLength (tpoly, rollup = 3, close = FALSE) #km    
      
      
      
      dftowrite = rbind(dftowrite, cbind(x$PID[i],paste(cor[,1], collapse = ","), paste(cor[,2], collapse = ","), x$capdat[i], leng$length))
   
    }
    
    
  }
  else{
    for(i in 1:nrow(x)){
      start <- c(as.numeric(x$rellon[i]), as.numeric(x$rellat[i]))
      end <- c(as.numeric(x$caplon[i]), as.numeric(x$caplat[i]))
      
      if(abs(start[1] - end[1]) < res(trans)[1] && abs(start[2] - end[2]) < res(trans)[1] || is.na(cellFromXY(r, start)) || is.na(cellFromXY(r, end))){
        AtoB = rbind(start, end)
      }
      else{
        AtoB = shortestPath(trans, start, end, output="SpatialLines")
      }
      cor = data.frame(coordinates(AtoB))
      names(cor) = c("x", "y")
      xrep = cor$x[1]
      yrep = cor$y[1]
      for(k in 1:(nrow(cor)-1)){
      
        if(cor$x[k] == xrep){ cor$x[k] = start[1] }
        else{ xrep = 1000000 }
        
        if(cor$y[k] == yrep){ cor$y[k] =  start[2] }
        else{ yrep = 1000000 }
      }
      xrep = cor$x[nrow(cor)]
      yrep = cor$y[nrow(cor)]
      for(k in nrow(cor):2){
        if(cor$x[k] == xrep) cor$x[k] =  end[1]
        else xrep = 1000000
        
        if(cor$y[k] == yrep) cor$y[k] =  end[2]
        else yrep = 1000000
      }
      
      names(cor) = c("X", "Y")
      cor$PID = 1
      cor$POS = 1:nrow(cor)
      tpoly = as.PolySet(cor, projection = "LL")
      leng = calcLength (tpoly, rollup = 3, close = FALSE) #km    
      
      
      
      
      dftowrite = rbind(dftowrite, cbind(x$PID[i],paste(cor[,1], collapse = ","), paste(cor[,2], collapse = ","), x$capdat[i], leng$length))

      
    }
    
  }
  
  if(!is.null(dftowrite)){
    dftowrite = data.frame(dftowrite)
    names(dftowrite) = c("id", "lon", "lat", "cdat")
    
    SCtunnel = open.port.SC(local.port = local_port)
    con <- dbConnect(dbDriver("MySQL"), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
    
    
    dbWriteTable(con, "paths", dftowrite, row.names = F, overwrite = overwrite, append = append )

    dbDisconnect(con) 
    close.port.SC(SCtunnel)
    print("New paths calculated and written to paths table.")
  }
  else{
  print("No new paths created.")  
  }
  }

create.tag.kml = function(preview = T, write = T, tofile = NULL){
  
  local_port = "3309"
  SCtunnel = open.port.SC(local.port = local_port)
  
  
  con <- dbConnect(dbDriver("MySQL"), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  rs <- dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  rs <- dbSendQuery(con, statement = "Select * from 
                    (SELECT  bio.tag_id, bio.sample_num, str_to_date( capture.date, '%d/%m/%Y' ) date, capture.statsarea, capture.lat_DD_DDDD, capture.long_DD_DDDD, capture.year 
                    from capture join bio where bio.tag_id = capture.tag) t1 
                    JOIN (SELECT trip.trip_id, trip.statsarea, trip.year, trip.captain, trip.Reported, str_to_date( trip.date, '%d/%m/%Y' ) date, sample.lat_DD_DDDD, sample.long_DD_DDDD, sample.sample_id
                    from trip join sample where sample.trip = trip.trip_id)t2
                    ON t1.sample_num = t2.sample_id  
                    ORDER BY captain, trip_id, tag_id, t1.date;")
  
  da <- fetch(rs, n = -1)   # extract all rows
  
  rs <- dbSendQuery(con, statement = "Select * from paths;")
  
  path <- fetch(rs, n = -1)   # extract all rows
  close.port.SC(SCtunnel)
  dbDisconnect(con)
  
  path = path[order(path$cdat),]
  
  da$sample_num = NULL
  da$trip_id = NULL
  da$captain = NULL
  da$Reported = NULL
  
  da$sample_id = NULL
  
  names(da) = c("PID", "capdate", "caparea","caplat", "caplon", "year", "area", "sampyear", "sampdat", "samplat", "samplon")
  
  
  X=NULL
  Y=NULL
  PID=NULL
  POS=NULL
  colour=NULL
  ddif = NULL
  ddate = NULL
  ind = which(as.character(da$caplat) == "0")
  da = da[-ind,]
  
  ind = which(as.character(da$year) == "")
  if(length(ind) > 0 )da = da[-ind,]
  
  ind = which(as.character(da$caparea) == "GULF" & as.character(da$area) == "GULF")
  
  da = da[-ind,]
  dup = 0
  
  mattxt = "unknown"
  
  ind = which(as.character(da$year) == mattxt)
  
  if(length(ind)>0) 
    da = da[-ind,]
  
  mattxt = "unknown unknown"
  
  ind = which(as.character(da$year) == mattxt)
  
  if(length(ind)>0) 
    da = da[-ind,]
  
  
  da = da[order(da$capdat),]
  outframe = NULL
  outarrow = NULL
  
  dx = split(da, da$sampyear)
  for(i in 1:length(dx)){
    
    
    out = NULL
    da_y = dx[[i]]
    
    
    da_tid = split(da_y, da_y$PID)
    
    for(j in 1:length(da_tid)){
      fin_da = da_tid[[j]]
      sampchron = chron(fin_da$sampdat[1], format = "y-m-d")
      
      
      
      fin_path = path[which(path$id == fin_da$PID[1]),]
      sub_out = NULL
      description = "<![CDATA[<b> datetxt  <br><br> disttxt</b>]]>"
      disttxt = ""
      datetxt = ""
      tid = ""
 
      for(k in 1:nrow(fin_path)){
        capchron = chron(  fin_da$capdate[k], format = "y-m-d")
   
        date = as.character(   fin_da$capdate[k])
        
        if(is.na(capchron)){
          date = paste("No date returned, received in ",fin_da$year[k])
          if( fin_da$caparea[k] == "NENS") capchron = chron(paste( fin_da$year[k],"-07-15", sep=""), format = "y-m-d")
          if( fin_da$caparea[k] == "SENS") capchron = chron(paste( fin_da$year[k],"-08-15", sep=""), format = "y-m-d")
          if( fin_da$caparea[k] == "GULF") capchron = chron(paste( fin_da$year[k],"-08-15", sep=""), format = "y-m-d")
          if( fin_da$caparea[k] == "4X") capchron = chron(paste( fin_da$year[k],"-03-15", sep=""), format = "y-m-d")
          
        }
        dif = capchron - sampchron
        dif = as.numeric(dif)
        
        cc = ""
        hr = ""
        if(is.na(dif)){ cc = "000000"
                        hr = "http://enssnowcrab.com/R/tag/go/blarrow.png"
        } 
        else if(dif < 180){ cc = "0000ff" 
                            hr = "http://enssnowcrab.com/R/tag/go/rarrow.png"
        }
        else if(dif < 545){ cc = "00aa00" 
                            hr = "http://enssnowcrab.com/R/tag/go/garrow.png" 
        }
        else if(dif < 1090){ cc = "ff0000" 
                             hr = "http://enssnowcrab.com/R/tag/go/barrow.png"
        }
        else if(dif < 1455){ cc = "00ffff" 
                             hr = "http://enssnowcrab.com/R/tag/go/yarrow.png"
                             
        }
        else{ cc = "7f0055" 
              hr = "http://enssnowcrab.com/R/tag/go/parrow.png" 
              
        }
        
        
        
        lon = unlist(str_split(fin_path[k,]$lon, ","))
        lat = unlist(str_split(fin_path[k,]$lat, ","))
        dist = "unknown"
        
        if(k==1){
          
          disx = cbind(as.numeric(lon), as.numeric(lat))
          names(disx) = c("lon", "lat")
          leng = as.matrix(disx)
          leng = Line(leng)
          llen =  LineLength(leng, longlat = T)
          #dist = (distVincentyEllipsoid(dx, a=6378137, b=6356752.3142, f=1/298.257223563))/1000
          #dist = distVincentySphere(c(chunk$X[k-1],chunk$Y[k-1]), c(chunk$X[k],chunk$Y[k]), r=6378137)
          disttxt = paste("DISPLACEMENT: <br>   From release to 1st capture: ", signif(llen, digits=3), "km <br>", sep = "")
          if(grepl("No date", date))
            datetxt = paste("TAG NUMBER:",fin_da$PID[1], "<br>TAGGED ON: ", as.character(chron(sampchron, out.format = "m d yyyy")), " <br>CAPTURED ON: ", date, sep = "")
          else
            datetxt = paste("TAG NUMBER:",fin_da$PID[1], "<br>TAGGED ON: ", as.character(chron(sampchron, out.format = "m d yyyy")), " <br>CAPTURED ON: ", as.character(chron(as.chron(date), out.format = "m d yyyy")), sep = "")
          bearfro = (length(lon)-3) : length(lon)
          bearfro = bearfro[bearfro > 0]
          
          icon_heading = bearing(p1 = c(as.numeric(lon[bearfro[1]]),as.numeric(lat[bearfro[1]] )), p2 = c(as.numeric(lon[bearfro[length(bearfro)]]),as.numeric(lat[bearfro[length(bearfro)]] )))
          
          
        }
        if(k>1){
          
          disx = cbind(as.numeric(lon), as.numeric(lat))
          names(disx) = c("lon", "lat")
          leng = as.matrix(disx)
          leng = Line(leng)
          llen =  LineLength(leng, longlat = T)
          
          #dist = (distVincentyEllipsoid(dx, a=6378137, b=6356752.3142, f=1/298.257223563))/1000
          #dist = distVincentySphere(c(chunk$X[k-1],chunk$Y[k-1]), c(chunk$X[k],chunk$Y[k]), r=6378137)
          disttxt = paste(disttxt, "<br>   To next capture:", signif(llen, digits=3), "km ") 
          if(grepl("No date", date))
            datetxt = paste(datetxt, "<br>   CAPTURED AGAIN ON: ", date, sep = "")
          else
            datetxt = paste(datetxt, "<br>   CAPTURED AGAIN ON: ", as.character(chron(as.chron(date), out.format = "m d yyyy")), sep = "")
          bearfro = (length(lon)-3) : length(lon)
          bearfro = bearfro[bearfro > 0]
          icon_heading = bearing(p1 = c(as.numeric(lon[bearfro[1]]),as.numeric(lat[bearfro[1]] )), p2 = c(as.numeric(lon[bearfro[length(bearfro)]]),as.numeric(lat[bearfro[length(bearfro)]] )))
          
        }
        
        
        
        pid = paste(fin_da$PID[k], k, sep = ".")
        
        line_color = rep(cc, length(lon))
        icon_href = rep(hr, length(lon))
        if(is.null(sub_out)){
          sub_out = data.frame(cbind(pid, lon, lat, line_color))
        }else{
          sub_out = rbind(sub_out, cbind(pid, lon, lat, line_color))
        }
        
        
        if(is.null(outarrow)){
          outarrow = data.frame(cbind(pid, da_y$sampyear[1], lon[length(lon)], lat[length(lat)], icon_heading, hr, .4))
        }else{
          outarrow = rbind(outarrow, cbind(pid, da_y$sampyear[1], lon[length(lon)], lat[length(lat)],icon_heading, hr, .4))
        }
        
      }
      
      description = sub("disttxt", disttxt, description)
      description = sub("datetxt", datetxt, description)
      sub_out$description = description
      sub_out$POS = 1:nrow(sub_out)
      
      if(is.null(out)){
        out = sub_out
      }else{
        out = rbind(out, sub_out)
      }
      
      
    }
    
    out$inFolder = da_y$sampyear[1]
    
    if(is.null(outframe)){
      outframe = out
    }else{
      outframe = rbind(outframe, out)
    }
    
    
    
    
  }
  outframe$pid = gsub("G", "9999", outframe$pid)
  mykml = RKmlObject()
  
  mykml$addLineStyle(styleid = "lsc1", color = "000000", width = 5)
  mykml$addLineStyle(styleid = "lsc2", color = "0000ff", width = 5)
  mykml$addLineStyle(styleid = "lsc3", color = "00aa00", width = 5)
  mykml$addLineStyle(styleid = "lsc4", color = "ff0000", width = 5)
  mykml$addLineStyle(styleid = "lsc5", color = "00ffff", width = 5)
  mykml$addLineStyle(styleid = "lsc6", color = "7f0055", width = 5)
  
  names(outarrow) = c("pid", "inFolder", "lon", "lat", "icon_heading", "icon_href", "icon_scale")
  
  mykml$addPoint(outarrow)
  mykml$addLineString(outframe, altitudeMode = "relativeToGround")
  mykml$addScreenOverlay(fn = "C:\\Users\\Brent\\Desktop\\ENSsnowcrab.com\\public_html07082012\\R\\tag\\go\\taglegend.png", size_x = .2, size_y = .2, screen_x = .8, screen_y = .8 )
  
  if(preview){
    mykml$preview()
  }
  if(write){
    if(is.null(tofile)){
      mykml$writekml(path = file.path(getwd(), "kml_outputs", "SCtagging_kml.kml"))
    }
    else{
      mykml$writekml(path = tofile)
      
    }
  }
  
}
## Write tag movement data to GeoJSON javascript functions. This is used b the enssnowcrab website to 
## display tag movement data by year. The previous tag kml does not display properly in google maps so this
## method was implemented. It looks better than the old kml since geoJSON has built in arrow markers.
create.tag.geojson = function(filename = NULL){
  
  main = "function add..year..(map, arr, inf, ls){..markers..}" 
  markers = "arr[..arindex..] = new google.maps.Polyline({strokeColor: '#..color..', strokeWeight: 2, path: [..positions..], icons: [{ icon: ..ls.., offset: '100%'}], map: map}); arr[..arindex..].addListener('click', function(event) { inf.setContent('..description..'); inf.position = event.latLng; inf.open(map); inf.setPosition(event.latLng);});"
  positions = "{lat: ..lat.., lng: ..lon..}"
  
  
  local_port = "3309"
  SCtunnel = open.port.SC(local.port = local_port)
  
  
  con <- dbConnect(dbDriver("MySQL"), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  rs <- dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  rs <- dbSendQuery(con, statement = "Select * from 
                    (SELECT  bio.tag_id, bio.sample_num, str_to_date( capture.date, '%d/%m/%Y' ) date, capture.statsarea, capture.lat_DD_DDDD, capture.long_DD_DDDD, capture.year 
                    from capture join bio where bio.tag_id = capture.tag) t1 
                    JOIN (SELECT trip.trip_id, trip.statsarea, trip.year, trip.captain, trip.Reported, str_to_date( trip.date, '%d/%m/%Y' ) date, sample.lat_DD_DDDD, sample.long_DD_DDDD, sample.sample_id
                    from trip join sample where sample.trip = trip.trip_id)t2
                    ON t1.sample_num = t2.sample_id  
                    ORDER BY captain, trip_id, tag_id, t1.date;")
  
  da <- fetch(rs, n = -1)   # extract all rows
  
  rs <- dbSendQuery(con, statement = "Select * from paths;")
  
  path <- fetch(rs, n = -1)   # extract all rows
  close.port.SC(SCtunnel)
  dbDisconnect(con)
  
  path = path[order(path$cdat),]
  
  da$sample_num = NULL
  da$trip_id = NULL
  da$captain = NULL
  da$Reported = NULL
  
  da$sample_id = NULL
  
  names(da) = c("PID", "capdate", "caparea","caplat", "caplon", "year", "area", "sampyear", "sampdat", "samplat", "samplon")
  
  
  X=NULL
  Y=NULL
  PID=NULL
  POS=NULL
  colour=NULL
  ddif = NULL
  ddate = NULL
  ind = which(as.character(da$caplat) == "0")
  da = da[-ind,]
  
  ind = which(as.character(da$year) == "")
  if(length(ind) > 0 )da = da[-ind,]
  
  ind = which(as.character(da$caparea) == "GULF" & as.character(da$area) == "GULF")
  
  da = da[-ind,]
  dup = 0
  
  library(stringr)
  
  mattxt = "unknown"
  
  ind = which(as.character(da$year) == mattxt)
  
  if(length(ind)>0) 
    da = da[-ind,]
  
  mattxt = "unknown unknown"
  
  ind = which(as.character(da$year) == mattxt)
  
  if(length(ind)>0) 
    da = da[-ind,]
  
  
  da = da[order(da$capdat),]
  outframe = NULL
  outarrow = NULL
  jsfinal = ""
  dx = split(da, da$sampyear)
  for(i in 1:length(dx)){
    markernum = 0
    ymain = main
    out = NULL
    da_y = dx[[i]]
    
    ymain = sub("..year..", da_y$sampyear[1], ymain)
    
    da_tid = split(da_y, da_y$PID)
    allmarkers = ""
    for(j in 1:length(da_tid)){
      fin_da = da_tid[[j]]

      sampchron = chron(fin_da$sampdat[1], format = "y-m-d")

      fin_path = path[which(path$id == fin_da$PID[1]),]

      description2 = "<b> datetxt  <br><br> disttxt</b>"
      disttxt = ""
      datetxt = ""
      tid = ""
      markerblock = ""
    
      
      for(k in 1:nrow(fin_path)){
        
        
        
        capchron = chron(  fin_da$capdate[k], format = "y-m-d")
        
        date = as.character(   fin_da$capdate[k])
        
        if(is.na(capchron)){
          date = paste("No date returned, received in ",fin_da$year[k])
          if( fin_da$caparea[k] == "NENS") capchron = chron(paste( fin_da$year[k],"-07-15", sep=""), format = "y-m-d")
          if( fin_da$caparea[k] == "SENS") capchron = chron(paste( fin_da$year[k],"-08-15", sep=""), format = "y-m-d")
          if( fin_da$caparea[k] == "GULF") capchron = chron(paste( fin_da$year[k],"-08-15", sep=""), format = "y-m-d")
          if( fin_da$caparea[k] == "4X") capchron = chron(paste( fin_da$year[k],"-03-15", sep=""), format = "y-m-d")
          
        }
        dif = capchron - sampchron
        dif = as.numeric(dif)
        arrls = "ls[0]"
        cc2 = ""
        if(is.na(dif)){ arrls = "ls[0]"
                        cc2 = "000000" 
        } 
        else if(dif < 180){ arrls = "ls[1]"
                            cc2 = "ff0000"
        }
        else if(dif < 545){ arrls = "ls[2]"
                            cc2 = "00aa00"
        }
        else if(dif < 1090){ arrls = "ls[3]"
                             cc2 = "0000ff"
        }
        else if(dif < 1455){ arrls = "ls[4]"
                             cc2 = "ffff00"
        }
        else{ 
          arrls = "ls[5]"
          cc2 = "55007f"
        }
        
        
        
        lon = unlist(str_split(fin_path[k,]$lon, ","))
        lat = unlist(str_split(fin_path[k,]$lat, ","))
        dist = "unknown"
        
      
        
        if(k==1){
          
          disx = cbind(as.numeric(lon), as.numeric(lat))
          names(disx) = c("lon", "lat")
          
          leng = as.matrix(disx)
          leng = Line(leng)
          llen =  LineLength(leng, longlat = T)
          #dist = (distVincentyEllipsoid(dx, a=6378137, b=6356752.3142, f=1/298.257223563))/1000
          #dist = distVincentySphere(c(chunk$X[k-1],chunk$Y[k-1]), c(chunk$X[k],chunk$Y[k]), r=6378137)
          disttxt = paste("DISPLACEMENT: <br>   From release to 1st capture: ", signif(llen, digits=3), "km <br>", sep = "")
          if(grepl("No date", date))
            datetxt = paste("TAG NUMBER:",fin_da$PID[1], "<br>TAGGED ON: ", as.character(chron(sampchron, out.format = "m d yyyy")), " <br>CAPTURED ON: ", date, sep = "")
          else
            datetxt = paste("TAG NUMBER:",fin_da$PID[1], "<br>TAGGED ON: ", as.character(chron(sampchron, out.format = "m d yyyy")), " <br>CAPTURED ON: ", as.character(chron(as.chron(date), out.format = "m d yyyy")), sep = "")
          
          
        }
        if(k>1){
          
          disx = cbind(as.numeric(lon), as.numeric(lat))
          names(disx) = c("lon", "lat")
          leng = as.matrix(disx)
          leng = Line(leng)
          llen =  LineLength(leng, longlat = T)
          
          #dist = (distVincentyEllipsoid(dx, a=6378137, b=6356752.3142, f=1/298.257223563))/1000
          #dist = distVincentySphere(c(chunk$X[k-1],chunk$Y[k-1]), c(chunk$X[k],chunk$Y[k]), r=6378137)
          disttxt = paste(disttxt, "<br>   To next capture:", signif(llen, digits=3), "km ") 
          if(grepl("No date", date))
            datetxt = paste(datetxt, "<br>   CAPTURED AGAIN ON: ", date, sep = "")
          else
            datetxt = paste(datetxt, "<br>   CAPTURED AGAIN ON: ", as.character(chron(as.chron(date), out.format = "m d yyyy")), sep = "")
          
        }
        
        ymarker = markers
        tespos = paste(paste("{lat: ", lat, ",lng: ", lon, "}", sep = ""), collapse = ",")
        ymarker = sub("..positions..", tespos, ymarker)
        ymarker = sub("..color..", cc2, ymarker)     
        ymarker = gsub("..arindex..", markernum, ymarker)
        ymarker = sub("..ls..", arrls, ymarker)
        markernum = markernum + 1
        markerblock = paste(markerblock, ymarker, sep=" 
                            ")
        
        pid = paste(fin_da$PID[k], k, sep = ".")
        
        
        
        
      }
      
      description2 = sub("disttxt", disttxt, description2)
      description2 = sub("datetxt", datetxt, description2)
      markerblock = gsub("..description..", description2, markerblock)
      
      
      
      allmarkers = paste(allmarkers, markerblock, sep = "
                         ")
      
    }
    ymain = sub("..markers..", allmarkers, ymain)
    jsfinal = paste(jsfinal, ymain, sep = "
                    ")
    
    
    
  }
  if(is.null(filename)){
    sink("jsfinal.js")
    cat(jsfinal)
    sink()
  }
  else{
    sink(filename)
    cat(jsfinal)
    sink()  
  }
  
  }

tags.poly.intersect.plot = function(){
  
  da = get.capturedata()
  
  
  da$sample_num = NULL
  da$trip_id = NULL
  da$captain = NULL
  da$Reported = NULL
  
  da$sample_id = NULL
  
  names(da) = c("PID", "capdate", "caparea","caplat", "caplon", "year", "area", "sampyear", "sampdat", "samplat", "samplon")
  
  
  X=NULL
  Y=NULL
  PID=NULL
  POS=NULL
  colour=NULL
  
  ind = which(as.character(da$caplat) == "0.0")
  if(length(ind)>0) da = da[-ind,]
  ind = which(as.character(da$caplat) == "0")
  if(length(ind)>0) da = da[-ind,]
  
  ii = is.in(are, da$samplon, da$samplat)
  jj = is.in(are, da$caplon, da$caplat)
  ind = which(!(ii | jj))
  
  if(length(ind)>0) 
    da = da[-ind,]
  
  #REMOVE GULF ENTRIES
  ind = which( as.character(da$caparea) == "GULF" & as.character(da$area) == "GULF"  )
  if(length(ind)>0) 
    da = da[-ind,]
  
  
  library(stringr)
  yea = years
  syear = da$sampyear
  if(years != "all"){
    years = strsplit(years, ",")
    years = unlist(years)
    
    syear = match(syear, years)
    
    indic = which(is.na(syear) == TRUE)
    
    if(length(indic)>0) 
      da = da[-indic,]
    
  }
  mattxt = "unknown"
  
  ind = which(as.character(da$year) == mattxt)
  
  if(length(ind)>0) 
    da = da[-ind,]
  
  
  
  
  
  dup = NULL
  for (i in 1:nrow(da)) {
    
    if(i > 1){
      if(da$PID[i] != da$PID[i-1]){
        dup = 1
        
        X = c(X, da$samplon[i])
        Y = c(Y, da$samplat[i])
        POS = c(POS, dup[1])
        PID = c(PID, da$PID[i])
        colour = c(colour, "white")
      }
      sampchron = chron(da$sampdat[i], format = "y-m-d")
      capchron = chron(da$capdat[i], format = "y-m-d")
      
      if(is.na(capchron)){
        if(da$caparea[i] == "NENS") capchron = chron(paste(da$year[i],"-07-15", sep=""), format = "y-m-d")
        if(da$caparea[i] == "SENS") capchron = chron(paste(da$year[i],"-08-15", sep=""), format = "y-m-d")
        if(da$caparea[i] == "GULF") capchron = chron(paste(da$year[i],"-08-15", sep=""), format = "y-m-d")
        if(da$caparea[i] == "4X") capchron = chron(paste(da$year[i],"-03-15", sep=""), format = "y-m-d")
        
      }
      dif = capchron - sampchron
      dif = as.numeric(dif)
      
      cc = ""
      if(is.na(dif)){ cc = "black" } 
      else if(dif < 180){ cc = "red" }
      else if(dif < 545){ cc = "darkgreen" }
      else if(dif < 1090){ cc = "blue" }
      else if(dif < 1455){ cc = "darkgoldenrod2" }
      else{ cc = "purple"}
      dup = dup+1
      
      X = c(X, da$caplon[i])
      Y = c(Y, da$caplat[i])
      POS = c(POS, dup[1])
      PID = c(PID, da$PID[i])
      colour = c(colour, cc)
    }
    else{
      X = c(X,da$samplon[i])
      Y = c(Y,da$samplat[i])
      POS = c(POS, 1)
      PID = c(PID,da$PID[i])
      X = c(X, da$caplon[i])
      Y = c(Y, da$caplat[i])
      POS = c(POS, 2)
      PID = c(PID, da$PID[i])
      colour = c(colour, "white")
      
      sampchron = chron(da$sampdat[i], format = "y-m-d")
      capchron = chron(da$capdat[i], format = "y-m-d")
      
      if(is.na(capchron)){
        if(da$caparea[i] == "NENS") capchron = chron(paste(da$year[i],"-07-15", sep=""), format = "y-m-d")
        if(da$caparea[i] == "SENS") capchron = chron(paste(da$year[i],"-08-15", sep=""), format = "y-m-d")
        if(da$caparea[i] == "GULF") capchron = chron(paste(da$year[i],"-08-15", sep=""), format = "y-m-d")
        if(da$caparea[i] == "4X") capchron = chron(paste(da$year[i],"-03-15", sep=""), format = "y-m-d")
        
      }
      
      dif = capchron - sampchron
      dif = as.numeric(dif)
      
      cc = ""
      if(is.na(dif)){ cc = "black" } 
      else if(dif < 180){ cc = "red" }
      else if(dif < 545){ cc = "darkgreen" }
      else if(dif < 1090){ cc = "blue" }
      else if(dif < 1455){ cc = "darkgoldenrod2" }
      else{ cc = "purple"}
      colour = c(colour, cc)
      dup = 2
    }
  }
  co = c("black", "red", "darkgreen", "blue", "darkgoldenrod2", "purple")
  na = c("unknown" ,"same season", "1 season", "2 seasons", "3 seasons", "4+ seasons")
  cocode = cbind(co, na) 
  cocode = data.frame(cocode)
  dbDisconnect(con)
  
  x = cbind(PID, POS, X, Y, colour)
  x = data.frame(x)
  x$PID = as.character(x$PID)
  x$POS = trunc(as.numeric(as.character(x$POS)))
  x$X = as.numeric(as.character(x$X))
  x$Y = as.numeric(as.character(x$Y))
  
  
  icoco = match(as.character(cocode$co), as.character(unique(x$colour)))
  icoco = which(is.na(icoco) == TRUE)
  
  cocode = cocode[-icoco,]
  x$PID = as.numeric(x$PID)
  
  x$z = colour
  x = as.PolySet(data.frame(x), projection = "LL")
  
  dir = file.path("C:", "project","mapping", "maps", "Emera Line", "ENL_SubseaCable_2km_StudyArea.shp"  )
  el= importShapefile(dir)
  
  require("rgeos")
  require("maptools")
  
  sp1 = PolySet2SpatialPolygons(el, close_polys=TRUE)
  sp2 = PolySet2SpatialLines(x)
  
  tlis = gCrosses(sp2, sp1, byid = T)
  
  sp2 = sp2[which(tlis == T),]
  
  x = SpatialLines2PolySet(sp2)
  
  xlim = c(min(x$X)-.2, max(x$X)+.2)
  ylim = c(min(x$Y)-.2, max(x$Y)+.2)
  plotRaster( file.path(direct,"mapping", "maps","Charts", "801_LL_WGS84_PCT.tif"),
              xlab="Longitude", main = paste("Snow Crab (Spaghetti) Emera Line Crossings | Year(s):", yea, sep=" "), ylab="Latitude", outer = T, axes=T, tck=0,
              fade = .5, cex.main = .5, tckLab=F, xlim = xlim, ylim = ylim, quality = quality, cellcount = NULL)
  
  
  addlinesSCAREA(lwd = 1)
  addMPA()
  
  addDivisions(xlim, ylim)
  
  #addLines(x, col = "blue", cex = .5, arrows = T, length = .05)
  
  addPolys(el, col = rgb(.8, 0, 0, .3))
  #x = x[which(x$PID %in% pids),]
  
  
  
  
  for (i in 1:length(sp2)) {
    chunk = as.data.frame(unlist(coordinates(sp2[i]), recursive = F)[1])
    
    names(chunk) = c("X", "Y")
    
    dd = c("blue")
    
    if(!shortpath){
      arrows(x0 = chunk$X[1:length(chunk$X)-1], y0 =  chunk$Y[1:length(chunk$Y)-1], x1 = chunk$X[2:length(chunk$X)] , y1 = chunk$Y[2:length(chunk$Y)], col = dd, angle= 20, code=2, length = 0.06)
    }
    else{
      len = shortest(chunk$X, chunk$Y, dd)
      
      
    }
    
  }
}

recaps.plot = function(){
  
}
get.paths = function(){
  local_port = "3308"
  
  SCtunnel = open.port.SC(local.port = local_port)
  
  con <- dbConnect(dbDriver("MySQL"), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  rs <- dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  rx <- dbSendQuery(con, statement = "Select * from paths;")
  
  dx <- fetch(rx, n = -1)   # extract all rows
  dbDisconnect(con) 
  close.port.SC(SCtunnel)
  
  return(dx)
}
years = c(2012,2013,2014)
tagReturned_Applied = function(are, years){
  #TAGS APPLIED IN GIVEN AREA AND YEAR
  
 y = get.capturedata()
  x = get.paths()
 
 names(x) = c("PID", "plon", "plat", "capdate", "kms")
 da = merge(x, y, by = c("PID","capdate"))

 ##REMOVE YEARS
 da = da[which(da$sampyear %in%  as.character(years) ),]
  da$sample_num = NULL
  da$sample_id = NULL
  da$trip = NULL
  da$trip_id = NULL
  da$captain = NULL
  da$Reported = NULL
  
  da$sample_id = NULL
  x = nrow(da)

 
  if(x == 0) return(FALSE)
  names(da) = c("PID", "capdate", "plon","plat","kms","caparea", "caplat", "caplong", "capyear", "triparea", "sampyear", "sampdate", "samplat", "samplon")

 ii = is.in(are, da$samplon, da$samplat)
  jj = is.in(are, da$caplon, da$caplat)
  ind = which(!(ii | jj))
  

  if(length(ind)>0) 
    da = da[-ind,]
  
  
  #REMOVE GULF ENTRIES
  ind = which( as.character(da$caparea) == "GULF" & as.character(da$triparea) == "GULF"  )
  if(length(ind)>0) 
    da = da[-ind,]
    
  x = nrow(da)

  if(x == 0) return(FALSE)
  
  z = NULL
  z$ret = nrow(da)
  z$retuni = length(unique(da$PID))
  
  
  mov = data2Poly(da)
  ind = which(mov$X == 0)
  ind = which(mov$PID %in% mov$PID[ind])
  if(length(ind)>0) 
    mov = mov[-ind,]
  
  ind = which(is.na(mov$D))
  ind = which(mov$PID %in% mov$PID[ind])
  if(length(ind)>0) 
    mov = mov[-ind,]
  
  dates = as.character(mov$D)
  mov$D = chron(dates, format = "y-m-d")
  
  x = split(mov, mov$PID)
  ndays = NULL
 nkm = NULL
  pid = NULL
  tofirst = NULL
 tofkm = NULL
  for (i in 1:length(x)) {
    chunk = data.frame(x[i])
    names(chunk) = c("PID", "POS", "X", "Y", "D", "K")
    dd = as.character(chunk$D)
    day = 0
    km = 0
    fkm = chunk$K[2]
    fday = (chunk$D[2] - chunk$D[1])
    for(j in 1:((nrow(chunk))-1)){
      day = day + (chunk$D[j+1] - chunk$D[j])
 
      km = km + as.numeric(chunk$K[j+1])
    }
    ndays = c(ndays, day)
    nkm = c(nkm, km)
    pid = c(pid, chunk$PID[1])
    tofirst = c(tofirst, fday)
    tofkm = c(tofkm, fkm)
  }
  daysince = cbind(pid, ndays, nkm)
  daysince = data.frame(daysince)
  
  ind = which(as.numeric(daysince$ndays) <= 10)
  

  if(length(ind)>0) 
    daysince = daysince[-ind,]
  x = nrow(daysince)

  if(x == 0) return(FALSE)
  
  #m = calcLength (mov, rollup = 3, close = FALSE)
  
  z$mov = mean(daysince$nkm)
  z$lmov = max(daysince$nkm)
  pdf("distances.pdf")
  hist(daysince$nkm,breaks=100, col="red",main="Distances Travelled",xlab="Distance(km)")
  dev.off()
  pdf("days.pdf")
  hist(daysince$ndays,main="Days To Last Known Capture",xlab="Time(days)")
  dev.off()
  pdf("tofirstdays.pdf")
  hist(tofirst,breaks=100, col="red", main="Days To First Capture",xlab="Time(days)")
  dev.off()
  z$day = mean(daysince$ndays)
  z$lday = max(daysince$ndays)
  
  #z$spe = z$mov/(z$day*0.0328549) #Days to month, km/month output
  z$spe = sum(daysince$km)/sum(daysince$ndays*0.0328549) #Days to month, km/month output
  distance = daysince$km
  return(z)
  
}
tagApplied = function(are, years){
  
  years = paste(years, collapse = " OR YEAR = ")
  local_port = "3308"
  
  SCtunnel = open.port.SC(local.port = local_port)
  
  con <- dbConnect(dbDriver("MySQL"), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  rs <- dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  rs <- dbSendQuery(con, statement = paste("SELECT bio.tag_id, bio.sample_num, trip.trip_id, trip.year, trip.statsarea, sample.trip, sample.sample_id, sample.lat_DD_DDDD, sample.long_DD_DDDD
               FROM bio, trip
               JOIN sample
               WHERE bio.sample_num = sample.sample_id 
               AND sample.trip = trip.trip_id 
               AND ( YEAR =", years , ");",sep = ""))
  
                    da <- fetch(rs, n = -1)   # extract all rows
                    dbDisconnect(con) 
                    close.port.SC(SCtunnel)
  da$sample_num = NULL
  da$sample_id = NULL
  da$trip = NULL
  da$trip_id = NULL
  da$captain = NULL
  da$Reported = NULL
  
  da$sample_id = NULL
  x = nrow(da)
  if(x == 0) return(FALSE)
  names(da) = c("PID", "sampyear", "samparea", "samplat", "samplon")
  
  ii = is.in(are, da$samplon, da$samplat)
  
  ind = which(ii == FALSE)
  
  if(length(ind)>0) 
    da = da[-ind,]
  
  #REMOVE GULF ENTRIES
  ind = which( as.character(da$samparea) == "GULF" )
  
  if(length(ind)>0)
    da = da[-ind,]
  
  
  ind = which( as.numeric(da$sampyear) < 2004 )
  
  if(length(ind)>0){ 
    return("This query includes years for which this information is not available")
  }
  else{
    return(nrow(da))
  }
  
  
  
}
stats = function(are, years){
  
  outname = "stats.txt"
  sink(outname)
  if(are == "all")cat(paste("</br>Statistics for", are,"areas", years, sep=" "))
  else cat(paste("</br>Statistics for", are, paste(years, collapse = ","), sep=" "))
  
  if(min(as.numeric(years)) < 2004)
    mess = FALSE
  else
    mess = tagReturned_Year(are, years)
  
  
  
  if(mess == FALSE){
    cat("</br></br>There were no tags returned or the data is not available for this year selection")
  }
  else{
    cat("</br></br>The following data is from captures in the area and years selected:&nbsp")
    cat("</br>&nbsp&nbsp&nbspNumber of tags returned with position and date:&nbsp ")
    cat(mess$a)
    cat("</br>&nbsp&nbsp&nbspNumber of individuals returning tags:&nbsp ")
    cat(mess$b)
  }
  mess = tagApplied(are, years)
  if(mess == FALSE){
    cat("</br></br>There were no tags released")
  }
  else{
    cat("</br></br>The following data is from crab that have been tagged in the area and years selected:&nbsp")
    cat("</br>&nbsp&nbsp&nbspTags Released:&nbsp ")
    cat(mess)
  }
  mess = tagReturned_Applied(are, years)
  if(mess == FALSE){
    cat("</br>&nbsp&nbsp&nbspThere were no valid returns to create stats")
  }
  else{
    
    cat("</br>&nbsp&nbsp&nbsp  Number of these tags returned: &nbsp")
    cat(mess$ret)
    cat("</br>&nbsp&nbsp&nbsp	Number of crab captured more than once:&nbsp")
    x = as.numeric(mess$ret)
    y =  as.numeric(mess$retuni)
    x = x - y
    cat(x)
    cat("</br>Data from these released crab with captures that occured more than 10 days from tag site:&nbsp")
    cat("</br>&nbsp&nbsp&nbsp	Average movement (km):&nbsp")
    cat(mess$mov)
    cat("</br>&nbsp&nbsp&nbsp	Largest movement (km):&nbsp")
    cat(mess$lmov)
    cat("</br>&nbsp&nbsp&nbsp	Average number of days to capture:&nbsp")
    cat(mess$day)
    cat("</br>&nbsp&nbsp&nbsp	Longest number of days between release site and capture:&nbsp")
    cat(mess$lday)
    cat("</br>&nbsp&nbsp&nbsp	Ave. rate of movement (total km/ total months):&nbsp")
    cat(mess$spe)
    
  }
  
  
  sink()
  
}
open.port.SC = function(user = enssnowc.user, password = enssnowc.password, host = "www.enssnowcrab.com", local.port = NULL, remote.port = "3306" ){

  if(is.null(local.port)) local.port = "3308"

  tunnel = NULL
  .jinit()
  
  .jaddClassPath(dir( "data/ssh", full.names=TRUE ))
  .jclassPath()
  
  tunnel <- .jnew("ssh/Sshtunnel")
  .jcall(tunnel,, "setvariables" , user, password, host, local.port, remote.port )
  .jcall(tunnel,, "openTunnel")
  return(tunnel)
} 
close.port.SC = function(tunnel){
  .jcall(tunnel,, "closeTunnel")
}
get.tableSQL = function(table){
  local_port = "3308"
  
  SCtunnel = open.port.SC(local.port = local_port)
  
  con <- dbConnect(dbDriver("MySQL"), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  rs <- dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  rs <- dbSendQuery(con, statement = "Select * from bio")
  da <- fetch(rs, n = -1)   # extract all rows
  dbDisconnect(con) 
  close.port.SC(SCtunnel)
  return(da)
}
get.capturedata = function(){
  local_port = "3308"

  SCtunnel = open.port.SC(local.port = local_port)
  
  con <- dbConnect(dbDriver("MySQL"), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  rs <- dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
rs <- dbSendQuery(con, statement = "Select * from 
                    (SELECT  bio.tag_id, bio.sample_num, str_to_date( capture.date, '%d/%m/%Y' ) date, capture.statsarea, capture.lat_DD_DDDD, capture.long_DD_DDDD, capture.year 
                    from capture join bio where bio.tag_id  = capture.tag) t1 
                    JOIN (SELECT trip.trip_id, trip.statsarea, trip.year, trip.captain, trip.Reported, str_to_date( trip.date, '%d/%m/%Y' ) date, sample.lat_DD_DDDD, sample.long_DD_DDDD, sample.sample_id
                    from trip join sample where sample.trip = trip.trip_id)t2
                    ON t1.sample_num = t2.sample_id  
                    ORDER BY captain, trip_id, tag_id, t1.date;")
  
  da <- fetch(rs, n = -1)   # extract all rows
  dbDisconnect(con) 
  close.port.SC(SCtunnel)
  da = unique(da)
  da$sample_num = NULL
  da$trip_id = NULL
  da$captain = NULL
  da$Reported = NULL
  da$sample_id = NULL
  
  names(da) = c("PID", "capdate", "caparea","caplat", "caplon", "year", "area", "sampyear", "sampdat", "samplat", "samplon")
  previd = ""
 # da = da[order(da$PID),]
  for(i in 1:nrow(da)){
    if(da$PID[i] == previd){
      da$samplat[i] = da$caplat[i-1]
      da$samplon[i] = da$caplon[i-1]
      da$sampdat[i] = da$capdat[i-1]
    }
    previd = da$PID[i] 
  }
  
  names(da) = c("PID", "capdate", "caparea","caplat", "caplon", "year", "area", "sampyear", "sampdat", "rellat", "rellon")
  
  
  
  return(da)
  
}
data2Poly = function(da){
  out = NULL
  X = NULL
  Y = NULL
  D = NULL
  POS = NULL
  PID = NULL
  dup = NULL
  K = NULL
  for (i in 1:nrow(da)) {
    sampchron = da$sampdat[i]
    capchron = da$capdat[i]
    km = da$kms[i]
    
    if(i > 1){
      
      if(da$PID[i] != da$PID[i-1]){
        dup = 1
        
        X = c(X, da$samplon[i])
        Y = c(Y, da$samplat[i])
        D = c(D, sampchron)
        POS = c(POS, dup[1])
        PID = c(PID, da$PID[i])
        K = c(K, 0)
      }

      dup = dup+1
      
      X = c(X, da$caplon[i])
      Y = c(Y, da$caplat[i])
      D = c(D, capchron)
      POS = c(POS, dup[1])
      PID = c(PID, da$PID[i])
      K = c(K, da$km[i])
     
    }
    else{
      X = c(X,da$samplon[i])
      Y = c(Y,da$samplat[i])
      D = c(D, sampchron)
      POS = c(POS, 1)
      PID = c(PID,da$PID[i])
      K = c(K, 0)
      X = c(X, da$caplon[i])
      Y = c(Y, da$caplat[i])
      D = c(D, capchron)
      POS = c(POS, 2)
      PID = c(PID, da$PID[i])
      K = c(K, da$km[i])
      dup = 2
 
    }
  }
  
  out = cbind(PID, POS, X, Y, D, K)
  out = data.frame(out)
  
  out$PID = as.character(out$PID)
  out$PID = as.numeric(gsub("G", "111111", out$PID))
  out$POS = trunc(as.numeric(as.character(out$POS)))
  out$X = as.numeric(as.character(out$X))
  out$Y = as.numeric(as.character(out$Y))
  out$K = as.numeric(as.character(out$K))
  #out = head(out,480)
  return(as.PolySet(out, projection = "LL"))
}
is.in = function(are2, lon, lat){
 
  borders= read.csv(file=file.path(getwd(),"areaborders.csv"), head=T, sep=",")
  
  b=borders[which(borders$area==are2),]
 
  yli=c(b$slat,b$nlat)
  xli=c(-(b$wlon),-(b$elon))

  len = length(lon)
  ew = c(1:len)
  sn = c(1:len)
  fin = c(1:len)
  ew[1:len] = FALSE
  sn[1:len] = FALSE
  fin[1:len] = FALSE

  ew[(lon<xli[1]) & (lon>xli[2])] = TRUE

  sn[(lat>yli[1]) & (lat<yli[2])] = TRUE
  
  fin[(ew == TRUE) & (sn == TRUE)] = TRUE
  
  
  return(fin)
}

tagReturned_Year = function(are, years){
  #TAGS APPLIED IN GIVEN AREA AND YEAR
  
  years = paste(years, collapse = " OR YEAR = ")
  local_port = "3308"
  
  SCtunnel = open.port.SC(local.port = local_port)
  
  con <- dbConnect(dbDriver("MySQL"), user = paste(enssnowc.user, "_admin", sep = ""), password = enssnowc.password, dbname = "enssnowc_Taging",  port = as.numeric(local_port), host = "localhost")
  rs <- dbSendQuery(con, statement = "SET SQL_BIG_SELECTS = 1;")
  rs <- dbSendQuery(con, statement = paste("SELECT capture.person, capture.statsarea, 
                    capture.lat_DD_DDDD, capture.long_DD_DDDD FROM capture WHERE( YEAR =", years , ");", sep = ""))
  
 
  da <- fetch(rs, n = -1)   # extract all rows
  dbDisconnect(con) 
  close.port.SC(SCtunnel)
  
  x = nrow(da)
 

  ii = is.in(are, da$long_DD_DDDD, da$lat_DD_DDDD)
  
  ind = which(ii == FALSE)
 
  if(length(ind)>0) 
    da = da[-ind,]

  dd = NULL
  da$long_DD_DDDD = as.numeric(as.character(da$long_DD_DDDD))
  da$lat_DD_DDDD = as.numeric(as.character(da$lat_DD_DDDD))
  for(i in 1:nrow(da)){
    if(absolutely.in.area(are, da$long_DD_DDDD[i], da$lat_DD_DDDD[i]) == FALSE){
      
      dd = rbind(dd, i)
    }
  }
  if(length(dd)>0) 
    da = da[-dd,]
  
  
  
  x = nrow(da)
  
  
  if(x == 0) return(FALSE)
  y = length(unique(da$person))
  
  z = NULL
  z$a=x
  z$b=y-1  # -1 because unknown is not a person
  
  return(z)
  
}




#' @title  absolutely.in.area
#' @description  Function that determins if a position is inside defined polygons 
#' @param area The area name exa. 'cfa23' 'cfa24' 'nens' 'sens' 'gulf' 'cfa4x'
#' @param lon The longitude of position in question
#' @param lat The latitude of position in question
#' @import sp rgeos
#' @return TRUE or FALSE
#' @export
absolutely.in.area = function(area, abslon, abslat){

  

  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-60, 50.3)
  p4 = c(-68, 50)
  p5 = c(-65.3, 44.25)
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)

  
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  gulf = SpatialPolygons(list(paa), 1:1)
  
  
  
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-57.5, 46)
  p4 = c(-57.78560987011954,46.00106076110973) 
  p5 = c(-59.85247480316924,46.00291960992756) 
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  nens = SpatialPolygons(list(paa), 1:1)
  

  
  p1 = c(-57.78560987011954,46.00106076110973) 
  p2 = c(-59.85247480316924,46.00291960992756) 
  p3 = c(-61, 46)
  p4 = c(-60.66040620990439,45.58083805201212)
  p5 = c(-59.11881785180857,43.67610276909335)
  p6 = c(-56.5, 44)
  p7 = c(-57.78560987011954,46.00106076110973) 
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twothree = SpatialPolygons(list(paa), 1:1)
  
  p1 = c(-61, 46)
  p2 = c(-60.66040620990439,45.58083805201212)
  p3 = c(-59.11881785180857,43.67610276909335)
  p4 = c(-63.33161397633095,42.50186912534272)
  p5 = c(-63.33296001561555,44.33343763428088)
  p6 = c(-63.52502801857509,44.5005704612574)
  p7 = c(-64, 45)
  p8 = c(-61, 46)
  are = rbind(p1, p2, p3, p4, p5, p6, p7, p8)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twofour = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-63.52502801857509,44.5005704612574)
  p2 = c(-63.33296001561555,44.33343763428088)
  p3 = c(-63.33161397633095,42.50186912534272)
  p4 = c(-66.5, 42)
  p5 = c(-65, 45.5)
  p6 = c(-63.52502801857509,44.5005704612574)
  are = rbind(p1, p2, p3, p4, p5, p6)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  xxxx = SpatialPolygons(list(paa), 1:1)
  
  p1 = c(-61.4,44)
  p2 = c(-61.4,45.5)
  p3 = c(-58,45.5)
  p4 = c(-58,44)
  p5 = c(-61.4,44)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  holes = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-62.5,43.8)
  p2 = c(-62.5,45.7)
  p3 = c(-59.2,45.7)
  p4 = c(-59.2,43.8)
  p5 = c(-62.5,43.8)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twofourz = SpatialPolygons(list(paa), 1:1)
  
  
  
  p1 = c(-60.6,44.1)
  p2 = c(-60.6,46.1)
  p3 = c(-57.75,46.1)
  p4 = c(-57.75,44.1)
  p5 == c(-60.6,44.1)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twothreez = SpatialPolygons(list(paa), 1:1)
  
  
  
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-60, 50.3)
  p4 = c(-68, 50)
  p5 = c(-65.3, 44.25)
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  gulf = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-60.41568418984078,47.03853523297152)
  p2 = c(-60.00014897319883,47.83408958793515 )
  p3 = c(-57.5, 46)
  p4 = c(-57.78560987011954,46.00106076110973) 
  p5 = c(-59.85247480316924,46.00291960992756) 
  p6 = c(-61, 46)
  p7 = c(-60.41568418984078,47.03853523297152)
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  nens = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-57.78560987011954,46.00106076110973) 
  p2 = c(-59.85247480316924,46.00291960992756) 
  p3 = c(-61, 46)
  p4 = c(-60.66040620990439,45.58083805201212)
  p5 = c(-59.11881785180857,43.67610276909335)
  p6 = c(-56.5, 44)
  p7 = c(-57.78560987011954,46.00106076110973) 
  are = rbind(p1, p2, p3, p4, p5, p6, p7)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twothree = SpatialPolygons(list(paa), 1:1)
  
  p1 = c(-61, 46)
  p2 = c(-60.66040620990439,45.58083805201212)
  p3 = c(-59.11881785180857,43.67610276909335)
  p4 = c(-63.33161397633095,42.50186912534272)
  p5 = c(-63.33296001561555,44.33343763428088)
  p6 = c(-63.52502801857509,44.5005704612574)
  p7 = c(-64, 45)
  p8 = c(-61, 46)
  are = rbind(p1, p2, p3, p4, p5, p6, p7, p8)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twofour = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-63.52502801857509,44.5005704612574)
  p2 = c(-63.33296001561555,44.33343763428088)
  p3 = c(-63.33161397633095,42.50186912534272)
  p4 = c(-66.5, 42)
  p5 = c(-65, 45.5)
  p6 = c(-63.52502801857509,44.5005704612574)
  are = rbind(p1, p2, p3, p4, p5, p6)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  xxxx = SpatialPolygons(list(paa), 1:1)
  
  p1 = c(-61.4,44)
  p2 = c(-61.4,45.5)
  p3 = c(-58,45.5)
  p4 = c(-58,44)
  p5 = c(-61.4,44)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  holes = SpatialPolygons(list(paa), 1:1)
  
  
  p1 = c(-62.5,43.8)
  p2 = c(-62.5,45.7)
  p3 = c(-59.2,45.7)
  p4 = c(-59.2,43.8)
  p5 = c(-62.5,43.8)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twofourz = SpatialPolygons(list(paa), 1:1)
  
  

  
  p1 = c(-60.6,44.1)
  p2 = c(-60.6,46.1)
  p3 = c(-57.75,46.1)
  p4 = c(-57.75,44.1)
  p5 == c(-60.6,44.1)
  are = rbind(p1, p2, p3, p4, p5)
  pa = Polygon(are)
  paa = Polygons(list(pa), "s1")
  twothreez = SpatialPolygons(list(paa), 1:1)
  

  
  pb = SpatialPoints(rbind(c(as.numeric(abslon), as.numeric(abslat))))
  

  
 # all.holes,44,45.5,61.4,58
  bo = FALSE
  if(area == "cfa23"){
    if(gContains(twothree, pb)) bo = TRUE
  }

  if(area == "cfa23zoom"){
    if(gContains(twothreez, pb)) bo = TRUE
  }
  if(area == "cfa24"){
    if(gContains(twofour, pb)) bo = TRUE
  }
  if(area == "cfa24zoom"){
    if(gContains(twofourz, pb)) bo = TRUE
  }
  if(area == "cfa4x"){
    if(gContains(xxxx, pb)) bo = TRUE
  }
  if(area == "nens"){
    if(gContains(nens, pb)) bo = TRUE
  }  
  if(area == "nens_gulf"){
    if(gContains(nens, pb) | gContains(gulf, pb)) bo = TRUE
  }  

  
  if(area == "sens"){
    if(gContains(twofour, pb) | gContains(twothree, pb)) bo = TRUE
  } 
  if(area == "gulf"){
    if(gContains(gulf, pb)) bo = TRUE
  } 
  if(area == "all" | area == "ens"){
    if(gContains(twofour, pb) | gContains(twothree, pb) | gContains(xxxx, pb) | gContains(nens, pb)) bo = TRUE
  }
  if(area == "allandgulf"){
    if(gContains(twofour, pb) | gContains(twothree, pb) | gContains(xxxx, pb) | gContains(nens, pb) | gContains(gulf, pb)) bo = TRUE
  }
  if(area == "all.holes"){
    if(gContains(holes, pb)) bo = TRUE
  }  

  
  return(bo)
  
}



