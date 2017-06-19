require(RODBC)




#' @title  ret_ent
#' @description  Function that enters returndata entered in the html app 
#' @import RODBC jsonlite stringr sp rgeos
#' @return message to webpage
ret_ent <- function(ddata){
  
  
  conn = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  # Check connection
  if (conn == -1){
    return(toJSON("Connection failed"))
  }
  out = ""
  
  ent = myUrlEncode(ddata)

   ent = unlist(str_split(ent, "&"))
  
  #radio-choice-2 
  rc2 = "";
  per = "";
  tid = "";
  lat = "";
  lon = "";
  date = "";
  depth = "";
  ves = "";

 add = "";
str = "";
rou = "";
loc = "";
  pro = "";
 cou = "";
  poc = "";
  ema = "";
  phoa = "";
  phob = "";
com = "";
  shellcond = "";
  
  
   for(i in 1:length(ent)){
    if(ent[i] != ""){
      
      sa = unlist(str_split(ent[i], "="))
      
    
      if(sa[1] == "radio-choice-2")
        rc2 = sa[2]
      if(sa[1] == "per")
        per = sa[2]
      if(sa[1] == "tid")
        tid = sa[2]
      if(sa[1] == "lat")
        lat = sa[2]
      if(sa[1] == "lon")
        lon = sa[2]
      if(sa[1] == "date")
        date = sa[2]
      if(sa[1] == "depth")
        depth = sa[2]
      if(sa[1] == "ves")
        ves = sa[2]
      
      if(sa[1] == "add")
        add = sa[2]
      if(sa[1] == "str")
        str = sa[2]
      if(sa[1] == "rou")
        rou = sa[2]
      if(sa[1] == "loc")
        loc = sa[2]
      if(sa[1] == "pro")
        pro = sa[2]
      if(sa[1] == "cou")
        cou = sa[2]
      if(sa[1] == "poc")
        poc = sa[2]
      if(sa[1] == "ema")
        ema = sa[2]
      if(sa[1] == "phoa")
        phoa = sa[2]
      if(sa[1] == "phob")
        phob = sa[2]
      if(sa[1] == "comments")
       com = sa[2]
      if(sa[1] == "shellcond")
        shellcond = sa[2]
      
    }
    
  }
  
  lat = str_replace(lat, "N","")
  lon = str_replace(lon, "W","")
  
  rlat = as.character(conpos(lat))
  rlon = as.character((conpos(lon)*-1))
  
  statsarea = "ENS"
  subarea = ""
  

  
  
  if(absolutely.in.area("cfa23", rlon, rlat)){ 
    statsarea = "sens"
    subarea = "(cfa23)(all)(ens)(sens)(allandgulf)(cfa23zoom)(all.holes)"
  }
  if(absolutely.in.area("cfa24", rlon, rlat)){ 
    statsarea = "sens"
    subarea = "(cfa24)(all)(ens)(sens)(allandgulf)(cfa24zoom)(all.holes)"
  }
  if(absolutely.in.area("gulf", rlon, rlat)){ 
    statsarea = "gulf"
    subarea = "(gulf)(nens_gulf)(allandgulf)"
  }
  if(absolutely.in.area("nens", rlon, rlat)){ 
    statsarea = "nens"
    subarea = "(all)(ens)(nens)(nens_gulf)(allandgulf)"
  }
  if(absolutely.in.area("cfa4x", rlon, rlat)){ 
    statsarea = "cfa4x"
    subarea = "(all)(ens)(cfa4x)(allandgulf)"
  }
   

  
  df = unlist(str_split(date, "/"))
  
  
  

  
  year = df[3]
  mon = df[1]
  day = df[2]
  dat = paste(day, mon, year, sep = "/")
  
 
  
  ret = 2
  if(rc2 == "choice-1")ret = 1
  

  add = unlist(str_split(add, ","))[1]
  
  if(add == ""){
    if(rou != "")
     add = paste(str, rou, sep = ", ")
    else add = str
  }

  
  toda1 = paste("insert into SCT_CAPTURE (TAG, DMY, PERSON, PERSON_B, LAT_DDMM_MM, LONG_DDMM_MM, LAT_DD_DDDD, LONG_DD_DDDD, FATHOMS, 
                RELCODE, COMMENTS, CAPTAIN, VESSEL, YEAR, STATSAREA, CARAPACE_COND, REWARDED, SUBAREA) 
                values ('", tid,"','", dat,"','", SQLsafty(per),"','NA','", lat,"','", lon,"','", rlat,"','", rlon,"','", depth,
                "','", ret,"','", SQLsafty(com),"','", SQLsafty(per),"','", SQLsafty(ves),"','", year,"','", statsarea,"','", shellcond,
                "','0','", subarea, "');", sep = "")
  que1 = paste("select count(NAME) num from SCT_PEOPLE where NAME = '", per,"'", sep = "")
  toda2 = paste("insert into SCT_PEOPLE (NAME, CIVIC, TOWN, PROV, POST, EMAIL, PHO1, PHO2, COUNTRY) values ('", SQLsafty(per),"','", SQLsafty(add),"','", SQLsafty(loc),"','", SQLsafty(pro),"','", poc,"','", SQLsafty(ema),"','", SQLsafty(phoa),"','", SQLsafty(phob), "','", SQLsafty(cou), "');", sep = "")
  toda4 = paste("update SCT_PEOPLE set NAME = '", SQLsafty(per),"', CIVIC = '", SQLsafty(add),"', TOWN = '", SQLsafty(loc),"', PROV = '", SQLsafty(pro),"', POST = '", poc,"', EMAIL = '", SQLsafty(ema),"', PHO1 = '", SQLsafty(phoa),
                "', PHO2 = '", SQLsafty(phob),"', COUNTRY = '", SQLsafty(cou),"' where NAME = '", per, "';", sep = "")

  result = RODBC::sqlQuery(conn, que1)
 
  
  toda = toda2
  out = paste("New person added to SCT_PEOPLE table: ", per, sep = "")
  if(result == 1){
    toda = toda4
    out = paste("Updated data in SCT_PEOPLE table for person: ", per, sep = "")
  }
  
  out = paste(out, "\n", sep = "")
  
  result = RODBC::sqlQuery(conn, toda)
 
  result = RODBC::sqlQuery(conn, toda1)
  out = paste(out, "New entry added to SCT_CAPTURE with TAG_ID: ", tid, sep = "")
  out = paste(out, "\n", sep = "")
  out = paste(out, "\n", sep = "")

  odbcClose(conn)
  return(out)
}


#' @title  sample_ent
#' @description  Function that enters release data entered in the html app 
#' @import RODBC jsonlite stringr
#' @return message to webpage
sample_ent <- function(bdata, sdata){
 
  
conn = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
# Check connection
if (conn == -1){
  return(toJSON("Connection failed"))
}
 
samp = myUrlEncode(sdata)


samp = unlist(str_split(samp, "&"))
rc1 = "";
dat = "";
sam = "";
ves = "";
cfa = "";
capt = "";
dep = "";
com = "";
lat = "";
lon = "";

for(i in 1:length(samp)){
  if(samp[i] != ""){

    sa = unlist(str_split(samp[i], "="))



    if(sa[1] == "radio-choice-1")
      rc1 = sa[2]
    if(sa[1] == "date")
      dat = sa[2]
    if(sa[1] == "samp")
      sam = sa[2]
    if(sa[1] == "ves")
      ves = sa[2]
    if(sa[1] == "cfa")
      cfa = sa[2]
    if(sa[1] == "capt")
      capt = sa[2]
    if(sa[1] == "dep")
      dep = sa[2]
    if(sa[1] == "com")
      com = sa[2]
    if(sa[1] == "lat")
      lat = sa[2]
    if(sa[1] == "lon")
      lon = sa[2]


  }

}


lat = str_replace(lat, "N","")
lon = str_replace(lon, "W","")

rlat = as.character(conpos(lat))
rlon = as.character((conpos(lon)*-1))

df = unlist(str_split(dat, "/"))

year = df[3]
mon = df[1]
day = df[2]
dat = paste(day, mon, year, sep = "/")

sta = ""
res = ""
samp = ""
sampsql = ""
out = ""
wrisamp = FALSE;
# //////////////////////////////////////////
#   //Check if sample num exists, if so get sample num
# // else get num row of sample
# 
# 

 sql = paste("SELECT TRIP_ID from SCT_TRIP where DMY = '", dat,  "' AND TECHNICIAN = '",sam,"'", sep = "")
result = RODBC::sqlQuery(conn, sql)
 exis = nrow(result)

rowid = ""

  if (exis > 0){
    res = result[,1]
  }
  



  if (exis == 0) {            
   
    sql = "select TRIP_ID from SCT_TRIP"
    result = RODBC::sqlQuery(conn, sql)
    res = nrow(result) + 1 
 
    
   if(cfa == "xxxx") sta = "4X"
   if(cfa == "nens") sta = "NENS"
   if(cfa == "23") sta = "SENS"
   if(cfa == "24") sta = "SENS"  
   if(cfa == "gulf") sta = "GULF"  				 
   suba = ""
    if(sta == "NENS") suba =  "(all)(ens)(nens)(nens_gulf)(allandgulf)"
    if(sta == "SENS"){
      if(cfa == "23")
         suba =  "(cfa23)(all)(ens)(sens)(allandgulf)(cfa23zoom)(cfa24zoom)(all.holes)"
           if(cfa == "24")
        suba =  "(cfa24)(all)(ens)(sens)(allandgulf)(cfa24zoom)(cfa24zoom)(all.holes)"
    }
 if(sta == "4X") suba =  "(all)(ens)(cfa4x)(allandgulf)";
 if(sta == "GULF") suba =  "(allandgulf)";
    
sql = paste("INSERT INTO SCT_TRIP VALUES( '",res,"' , '",sam,"' , '",SQLsafty(ves),"' , '",cfa,"' , '",dat,"' , '",year,"' , '",sta ,"' , 0 , '",SQLsafty(capt) ,"' , '" ,suba , "')", sep = "")
 
result = RODBC::sqlQuery(conn, sql)



    #   fwrite($myfile, $sql);
    if (length(result) == 0){
      out =  paste(out, "\nNew Trip ", res, " Successfully Added.")
    }
    else{
      out =  paste(out,"\nError: ",  result)
      return(out)
      die()
    }
    
}


 
sql = paste("SELECT SAMPLE_ID FROM SCT_SAMPLE where TRIP = '",res,"' AND LAT_DD_DDDD = '",rlat,"' AND LONG_DD_DDDD = '",rlon,"'", sep = "")
result = RODBC::sqlQuery(conn, sql)
res2 = nrow(result) 

 if (res2 > 0){
  samp = result[,1]
 }
 if (res2 == 0) {   
   sql = "select SAMPLE_ID from SCT_SAMPLE"
   result = RODBC::sqlQuery(conn, sql)
   samp = as.character(nrow(result) + 1) 
   
   sampsql = paste("INSERT INTO SCT_SAMPLE VALUES( '",samp,"' , '",res,"' , '",lat,"' , '",lon,"'  ,  '",rlat,"' , '",rlon,"' , '",dep,"' , '",SQLsafty(com),"')", sep = "")
   wrisamp = TRUE;
   
 }




dd = as.data.frame(fromJSON(bdata)[2:nrow(fromJSON(bdata)),])
names(dd) = fromJSON(bdata)[1,]


# $i = 0;
# $b = "";
 writedata = TRUE
 for(i in 1:nrow(dd)){
   if(i > 0){
     if(!is.na(dd$`Tag Num`[i])){
       
       
       sql = paste("SELECT TAG_ID FROM SCT_BIO where TAG_ID = '", dd$`Tag Num`[i],"'", sep = "")
       result = RODBC::sqlQuery(conn, sql)
       ntn = nrow(result) 
              if(ntn > 0) {
                out = paste(out, "\nCrab with tag " , dd$`Tag Num`[i], " has already been added!! ", sep = "")
                writedata = FALSE;
               
              } 
              
            }
          }
        }		
 

 
 if(writedata){
   for(i in 1:nrow(dd)){
     if(i > 0){
       if(!is.na(dd$`Tag Num`[i])){
         if(is.null(dd$`Durometer`[i])) dd$`Durometer`[i] = NA
         sql = paste("INSERT INTO SCT_BIO VALUES ('",samp,"', '",dd$`Tag Num`[i],"', '",dd$`Carapace`[i],"', '",dd$`Claw`[i],"','",dd$`Shell Cond`[i],"','",dd$`Durometer`[i],"')", sep = "")
         result = RODBC::sqlQuery(conn, sql)
         
         
         
         #   fwrite($myfile, $sql);
         if (length(result) == 0){
          
           out =  paste(out, "\nCrab with  tag " , dd$`Tag Num`[i], " successfully added", sep = "")
         }
         else{
           out =  paste(out,"\nError: ",  result)
           return(out)
           die()
         }
        
       }
     }

   }
   if(wrisamp){
     result = RODBC::sqlQuery(conn, sampsql)
     
     
     if (length(result) == 0){
       out = paste(out,"\nSample from trip ",res, " with pos ",lat, " " ,lon, " successfully added", sep = "")
     }
     else{
       out =  paste(out, "\nError: " ,sampsql , "\n" , result, "\n", sep = "")
       return(out)
       die()
     }
     
   }
 }
 
out = paste(out,"\n\n", sep = "")
# 
# 
# 
# 
# 
# /*
#   $ans = $_GET['radio-choice-2'];
# 
# $ret = "3";
# if ($ans == "choice-1") {          
#   $ret = "1";      
#   
# }
# if ($ans == "choice-2") {          
#   $ret = "2";      
# }
# */
#   
#   fclose($myfile);

# 
# echo json_encode($out);

odbcClose(conn)
return(out)





}

#' @title  enter_data_app
#' @description  This is a test
#' @import opencpu
#' @export
enter_data_app <- function(){
 ocpu_start_app("SCtagging", no_cache = TRUE)

}

#' @title  stop_ent
#' @description  This is a test
#' @import opencpu
#' @export
stop_ent <- function(){
  

}

#' @title  SC_samp_Ent
#' @description  Enter data to sample table
#' @import RMySQL RODBC
SC_samp_Ent <- function(tid = NA, lat = NA, lon = NA, dlat = NA, dlon = NA, fat = NA, com = NA){
  sid = as.integer(SCT_nrows("SCT_SAMPLE") + 1)
  # Exists sample?
  
}

#' @title  SCT_nrows
#' @description  Get the number of rows from specified table
#' @param table The name of the table from which to determine the current number of rows
#' @import RODBC
SCT_nrows <- function(table = ""){
  
  con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  nrows = NULL
  nrows = sqlQuery(con, paste("select count(*) from ", table, sep = "") )
  odbcClose(con)
  return(nrows)
}



#' @title  SQLsafty
#' @description  Relapce ' with '' to perserve entries
#' @param var The variables to modify
#' @export
SQLsafty <- function(var) {
  
  return(gsub("'", "''", var))
}


#' @title  myUrlEncode
#' @description  Decode json url
#' @param string The url to decode
#' @export
myUrlEncode <- function(string) {
  entities = c("%21", "%2A", "%27", "%28", "%29", "%3B", "%3A", "%40", "%26", "%3D", "%2B", "%24", "%2C", "%2F", "%3F", "%25", "%23", "%5B", "%5D", "%C2%B0", "\\+", "%0D%0A")
  replacements = c("!", "*", "'", "(", ")", ";", ":", "@", "%26", "%3D", "+", "$", ",", "/", "?", "%", "#", "[", "]", "", " ", "  ")
  
  
  for(i in 1:length(entities)){
    print(entities[i])
    string = gsub(entities[i], replacements[i], string)
  }
    return(string)
}

#' @title  conpos
#' @description  Decode positional data
#' @param string The pos to decode
#' @export
conpos = function(string) {
  
  if(substr(string, 4, 4) == '.'){
    string = paste(substr(string, 1, 2),'0',substr(string, 3, nchar(string)), sep = "")
  }
  de = as.integer(substr(string, 1, 2)) 
  min = as.integer(substr(string, 3, 4))
  dmin = as.integer(substr(string, 6, 7))/100
  dm = (min + dmin)/60
  nu = de + dm
  return(nu)
}

#' @title  auto_availableP
#' @description Function that help autopopulate people in the html form
#' @import RODBC jsonlite
#' @export
autoavailableP = function(){
  con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  res = NULL
  res = sqlQuery(con, "select NAME from SCT_PEOPLE" )

  odbcClose(con)

    return(toJSON(res))

}
#' @title  auto_availableT
#' @description Function that help autopopulate Tag id in the html form
#' @import RODBC jsonlite
#' @export
autoavailableT = function(){
  con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  res = NULL
  res = sqlQuery(con, "select TAG_ID from SCT_BIO" )

  odbcClose(con)
  
  return(toJSON(res))
  
}

#' @title  autoaddData
#' @description Function that help autopopulate people in the html form
#' @param name The persons name from which to obtain current info
#' @import RODBC jsonlite
#' @export
autoaddData = function(name = ""){

  con = odbcConnect(oracle.snowcrab.server , uid=oracle.snowcrab.user, pwd=oracle.snowcrab.password, believeNRows=F)
  res = sqlQuery(con, paste("select * from SCT_PEOPLE where NAME = '", name, "'", sep = "" ))
  odbcClose(con)
  
  return(toJSON(res))
  
}


