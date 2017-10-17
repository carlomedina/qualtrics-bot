library(RSelenium)
library(magrittr)


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
  return(list(DATA="QR~QID7~1",
                SAS="QR~QID7~5",
                GIS="QR~QID7~12",
                Concepts="QR~QID7~2",
                STATA="QR~QID7~6",
                SQL="QR~QID7~11",
                Excel="QR~QID7~3", 
                SPSS="QR~QID7~7",
                Qualtrics="QR~QID7~13",
                Eviews="QR~QID7~4",
                RStudio="QR~QID7~8",
                Other="QR~QID7~10")[[software]])
}


fillInfo <- function(tutors, username, classyear, major, course, day, month, time, softwares) {
  remDr$navigate("https://wesleyan.co1.qualtrics.com/jfe/form/SV_6gTfM2Q0AnAbbyB")
  Sys.sleep(2)
  # multiple tutors
  # fill in the form
  usernameELT <- remDr$findElement('id', 'QR~QID2~1')
  usernameELT$sendKeysToElement(list(username))
  
  classyrELT <- remDr$findElement('id', 'QR~QID2~2')
  classyrELT$sendKeysToElement(list(classyear))
  
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
  
  lengthELT <- remDr$findElement('id', 'QR~QID6')
  lengthELT$sendKeysToElement(list("1"))
  
  
  
  # multiple checkboxes
  for (software in softwares) {
    buttonELT <- remDr$findElement(using = "xpath",
                              paste0('//*[@id="', findSoftware(software), '"]'))
    buttonELT$clickElement()
  }
  
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
  
  ## disable this when debugging
   # submitELT <- remDr$findElement(using = 'xpath', '//*[@id="NextButton"]')
  # submitELT$clickElement()
  
  
}


fillInfo(c("Kyle", "Carlo"), "mkaparakis", "2012", "MATH", "QAC201", "Monday", "September", "Afternoon", c("SAS", "STATA"))
  
