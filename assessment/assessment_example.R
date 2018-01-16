library(mirtCAT)
library('devtools')
library(jsonlite)
library(car)
library(timeDate)
library(gmailR) # fo sending mails. IMPORTANT: has to be gmailR, not "gmailr". https://github.com/trinker/gmailR
#library(RCurl) # SMS

datapath <- "/srv/shiny-server/data/project_1/" # save data files to this directory
setwd("/home/test/assessment/") # set this to the directory containing questionnaire files 
source("/srv/shiny-server/explorer/functions.R")

options(stringsAsFactors = FALSE)

##### Patient information and settings ----
# Configure settings when beginning assessment, do not change afterwards!
# * set patient code
# * set data path (datapath variable)
# * change port
# * set method of contact; contact info

patcode <- "1234" # patient code or number
age <- 33           # age (years)
sex <- "F"               # sex/gender
dia <- "F99.9"           # diagnosis (ICD- or DSM-code)
patinfo <- c(patcode,age,sex,dia)
datapath <- paste0(datapath, patcode, ".R")

method <- "mail" # "mail" or "sms"
mailaddress <- "mail@example.com"
#mobileno <- "49170312352" # format with international code without +, e.g. 49170312352
questionnaire.port.public <- "8081" # port for public access to questionnaire. has to be a string!
questionnaire.port.private <- as.numeric(questionnaire.port.public)-80  # internal port for questionnaire, set automatically

if(file.exists(datapath)){load(paste0(datapath))}
##### base layout ----
title<-""
author<-""
instructions<-c("Anleitung:","beantworten Sie die Fragen und klicken Sie dann auf \"Weiter\"","Weiter")
begin_message<-"Klicken Sie auf 'Weiter' um mit dem Fragebogen zu beginnen."
firstpage<-list(h2("Persönlicher Fragebogen"),"Willkommen bei Ihrem persönlichen Fragebogen.","Bitte schätzen Sie ein, wie zutreffend die folgenden Aussagen Ihr Denken, Fühlen und Verhalten an diesem Tag wiedergeben. Benutzen Sie dazu bitte die vorgegebene Skala von 0 bis 100.")
lastpage <- function(person){return(list(h2("Ende"),"Sie sind am Ende dieses Fragebogens. Bitte klicken Sie abschließend auf \"Weiter\" um Ihre Antworten zu speichern. "))}
temp_file <- paste0("./temp_", patcode, ".rds")
demographics <- list(
            #textInput(
            #    inputId = 'session', 
            #    label = 'Gab es heute ein besonderes Ereignis?', 
            #    value = "",
            #    placeholder = "Wenn ja: bitte eintragen!")
            )


guiList<-list(title = title,
              authors = author,
              instructions = instructions,
              firstpage = firstpage,
              lastpage = lastpage,
              begin_message = begin_message,
              #demographics = demographics,
              #demographics_inputIDs = "session",
              temp_file = temp_file
        #     options = gui_options
)

#### Items ----
#options <- matrix(c("Unzutreffend", "Eher unzutreffend", "Neutral", "Eher zutreffend", "Zutreffend"),nrow = 7, ncol = 5, byrow = TRUE)

# Load questions. This has to be one text file containing item texts and one containing item abbreviations. The GAD-7 is included as an example.
questions <- c( 
    scan("scales/GAD.txt", what = "character", sep = NULL))

questions.short <- c(scan("scales/GAD_short.txt", what = "character", sep = NULL))

questions.both <- cbind(questions,questions.short)

df <- data.frame(Question = questions, Type = "slider", label="test", min=0, pre="Ausprägung: ",post="", max=100,step=1,value=50)#Option = options,

# messages are sent using an HTTP API. This can be configured to work with various providers. 
if(method == "sms"){
httpdata <- paste0("?username=EXAMPLE&validpass=PASSWORD&number=",mobileno,"&message=Ihr%20t%C3%A4glicher%20Fragebogen%20ist%20bereit!%20Klicken%20Sie%20hier%3A%20https%3A%2F%2Fexample.com%3A",questionnaire.port.public,"&receipt=0&flash0&encoding=utf8")
url = "https://www.sms-gateway.at/sms/sendsms.php"
getURI(paste0(url,httpdata))}

if(method == "mail"){
gmail(mailaddress,"PASSWORD","Ihr täglicher Fragebogen ist bereit!", 
paste0("Klicken Sie hier: https://example.com:",questionnaire.port.public), 
"questionnaires@example.com", 
username = "questionnaires@example.com")
}

mirtCAT_preamble(df = df,shinyGUI=guiList)
runApp(createShinyGUI(), port = questionnaire.port.private, host = "127.0.0.1")
person <- getPerson()

if(!exists("answers")){answers<-matrix(nrow=0, ncol=length(questions))}

#if(!exists("outcome")){outcome<-matrix(nrow=0, ncol=length(outcome))}
if(!exists("vec.datum")){
    vec.datum <- Sys.time();} else{
    (vec.datum[length(vec.datum)+1] <- Sys.time());}

#if(!exists("was.session")){
#    was.session <- person$demographics} else{
#    (was.session[length(was.session)+1] <- person$demographics)}

answers <- rbind(answers,person$raw_responses)
colnames(answers)<-questions.short
mat.answers <- as.matrix(answers)

df.answers <- as.data.frame(mat.answers)

df.answers <- cbind(df.answers,vec.datum
#,t(was.session)
)
colnames(df.answers) <- c(questions.short,"date")
#,"session")

save.image(file=datapath)
