library(RSelenium)
library(magrittr)

# checkForServer()
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100" 
                      , port = 4445L
                      , browserName = "firefox"
                      )

remDr$open()
remDr$screenshot(display = TRUE)
remDr$navigate("https://wesleyan.co1.qualtrics.com/jfe/form/SV_6gTfM2Q0AnAbbyB")

findTutor <- function(tutorName) {
  return(list(Kyle="QR~QID1~38", 
               Natalie="QR~QID1~53", 
               Tiffany="QR~QID1~35", 
               Frederick="QR~QID1~39", 
               Jack="QR~QID1~37", 
               Kelly="QR~QID1~40", 
               Liza="QR~QID1~42", 
               Ann="QR~QID1~43", 
               Carlo="QR~QID1~45", 
               Grace="QR~QID1~52")[[tutorName]] )
}

findDay <- function(day) {
  return(list(Monday="QR~QID3~1", 
              Tuesday="QR~QID3~2", 
              Wednesday="QR~QID3~3", 
              Thursday="QR~QID3~4", 
              Friday="QR~QID3~5", 
              Sunday="QR~QID3~6")[[day]])
}


findMonth <- function(month) {
  return(  list(September="QR~QID4~1",
                 October="QR~QID4~2",
                 November="QR~QID4~3",
                 December="QR~QID4~4",
                 January="QR~QID4~5",
                 February="QR~QID4~6",
                 March="QR~QID4~7",
                 April="QR~QID4~8",
                 May="QR~QID4~9")[[month]] )
}

findTime <- function(time) {
  return(list(Afternoon="QR~QID5~1",
              Evening="QR~QID5~2")[time])
}


findSoftware <- function(software) {
  return(list(Data="QR~QID7~1",
                SAS="QR~QID7~5",
                GIS="QR~QID7~12",
                "Statistical Tools/Concepts"="QR~QID7~2",
                Stata="QR~QID7~6",
                SQL="QR~QID7~11",
                Excel="QR~QID7~3", 
                SPSS="QR~QID7~7",
                Qualtrics="QR~QID7~13",
                Eviews="QR~QID7~4",
                "R-Studio"="QR~QID7~8",
                Other="QR~QID7~10")[[software]])
}


fillInfo <- function(tutors, username, classyear, major, course, day, month, time, softwares, lengthTime, debug=T) {
  remDr$navigate("https://wesleyan.co1.qualtrics.com/jfe/form/SV_6gTfM2Q0AnAbbyB")
  Sys.sleep(2)
  # multiple tutors
  tutors <- strsplit(tutors, ",") %>% unlist() %>% str_trim()
  for (tutor in tutors) {
    tutorbox <- remDr$findElement(using='xpath', 
                                  '//*[@id="QID1group0"]')
    tutor <- remDr$findElement(using='xpath', 
                               paste0('//*[@id="', findTutor(tutor), '"]'))
    tutor$mouseMoveToLocation(webElement=tutor)
    tutor$buttondown()
    tutor$getElementLocation()
    tutor$mouseMoveToLocation(webElement=tutorbox)
    tutor$buttonup()
  }
  
  
  # fill in the form
  usernameELT <- remDr$findElement('id', 'QR~QID2~1')
  usernameELT$sendKeysToElement(list(username))
  
  classyrELT <- remDr$findElement('id', 'QR~QID2~2')
  classyrELT$sendKeysToElement(list(as.character(classyear)))
  
  majorELT <- remDr$findElement('id', 'QR~QID2~3')
  majorELT$sendKeysToElement(list(major))
  
  courseELT <- remDr$findElement('id', 'QR~QID2~4')
  courseELT$sendKeysToElement(list(course))
  
  dayELT <- remDr$findElement(using = 'xpath', 
                           paste0('//*[@id="', findDay(day), '"]'))
  dayELT$clickElement()
  
  monthELT <- remDr$findElement(using = 'xpath', 
                             paste0('//*[@id="', findMonth(month),'"]'))
  monthELT$clickElement()
  
  timeELT <- remDr$findElement(using = 'xpath', 
                            paste0('//*[@id="', findTime(time),'"]'))
  timeELT$clickElement()
  
  if (!is.na(as.numeric(lengthTime))) {
    lengthELT <- remDr$findElement('id', 'QR~QID6')
    lengthELT$sendKeysToElement(list(as.character(lengthTime)))
  }
  
  # multiple checkboxes
  for (software in softwares) {
    buttonELT <- remDr$findElement(using = "xpath",
                              paste0('//*[@id="', findSoftware(software), '"]'))
    buttonELT$clickElement()
  }
  
  if (!debug) {
    # disabled when debugging
    submitELT <- remDr$findElement(using = 'xpath', '//*[@id="NextButton"]')
    submitELT$clickElement()
  }
  
  
}
# sample
fillInfo(c("Kyle", "Ann"), "mkaparakis", "2012", "MATH", "QAC201", "Monday", "September", "Afternoon", c("SAS", "Stata"), 10)
library(dplyr)
library(lubridate)
library(stringr)
data <- read.csv("QAC-Response-v3-QAC-Registration-Form.csv", stringsAsFactors = F)
names(data) <- c("username", 
                 "classyear", 
                 "major", 
                 "course", 
                 "softwares", 
                 "tutornames", 
                 "timestamp", 
                 "token",
                 "whitespace",
                 "lengthTime",
                 "Qualtrics",
                 "isGMTTimeStamp")
data %<>%
  filter(Qualtrics == 0) %>%
  mutate(timestamp = ymd_hms(timestamp, tz = "GMT"),
         timestamp = with_tz(timestamp, tzone = "EST"),
         month = as.character(month(timestamp, label = T, abbr = F)) ,
         hour = hour(timestamp),
         day = as.character(wday(timestamp, label = T, abbr = F)),
         time = ifelse(hour >= 15 & hour <= 6, "Afternoon", "Evening"),
         username = str_replace(username, "@.*", ""))

for (i in 1:nrow(data)) {
  # fillInfo <- function(tutors, username, classyear, major, course, day, month, time, softwares) 
  fillInfo(data$tutornames[i], data$username[i], data$classyear[i], 
           data$major[i], data$course[i], data$day[i], data$month[i], 
           data$time[i], data$softwares[i], data$lengthTime[i], debug=T)
  print(i)
  Sys.sleep(2)
}
  
