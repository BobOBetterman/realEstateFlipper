library(mailR)

sender <- "andy@tfo.vc"
recipients <- c("andy@tiberiusfo.com", "vchiang@gmail.com")

subject <- "This Week's Results"
body <- "Attached are the results for the week"

hostName <- "smtp.gmail.com"
port <- 465
userName <- "andy@tiberiusfo.com"
passWord <- "uwmsjyfyxeinavll"

send.mail(from = sender, to = recipients, subject = subject, body = body,
          smtp = list(host.name = hostName, port = port,
                      user.name = userName, passwd = passWord, ssl = TRUE),
          authenticate = TRUE, send = TRUE,
          attach.files = "allData/flipperStats01.csv")