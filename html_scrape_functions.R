if (require(xlsx) == FALSE) {
  install.packages('xlsx')
  library(xlsx)
}

if (require(stringr) == FALSE) {
  install.packages('stringr')
  library(stringr)
}

rem.lines <- function(stringstart, stringend, text){
  start.len <- nchar(stringstart)
  start.line <- grep(stringstart, text)[1]
  end.line <- grep(stringend, text)
  end.line <- min(end.line[end.line>=start.line])
  these.lines <- text[start.line:end.line]
  return(these.lines)
}


page.clip <- function(web.text, start.string, end.string){
  start <- (grep(start.string, web.text)[1])+1
  clip <- web.text[start:length(web.text)]
  end <- (grep(end.string, clip)[1]-1)
  final.clip <- clip[1:end]
  return(final.clip)
}

string.clip <- function(string, start.string, end.string){
  start <- regexpr(start.string, string)[1]
  start <- start+(nchar(start.string))
  next.string <- substring(string, start)
  end <- regexpr(end.string, next.string)[1]
  final.string <- substring(next.string, 1, end-1)
  return(final.string)
}

re.request <- function(full.internet.request){
  i <- 0
  j <- 0
  
  while(i==0){
    tryCatch({    j <- j+1 
    api <- full.internet.request
    i <- length(api)}, 
    error = function(e){cat("ERROR :", conditionMessage(e))
      Sys.sleep(10)
      print(paste0("Failure On request ", j))
    })
  }
  return(api)
}

add.list <- function(list, new.element){
  index <- length(list)+1
  list[index] <- new.element
  return(list)
}

get.options <- function(html.text, start.line = 1){
  actual.text <- html.text[start.line:length(html.text)]
  option.text <- rem.lines("<select", "</select>", actual.text)
  if(length(grep('<optgroup', option.text))>0){
    for(group in grep('<optgroup', option.text)){
      section <- option.text[group:length(option.text)]
      group.text <- rem.lines('<optgroup', '</optgroup>', section)
      option.list <- data.frame('value' = character(), 'option.label' = character(), stringsAsFactors = FALSE)
      opts <- grep('<option', group.text)
      for(opt in opts){
        val <- string.clip(group.text[opt], '<option value=\"', '\"')
        lab <- string.clip(group.text[opt],'>', '<')
        option.list[nrow(option.list)+1,] <- c(val, lab)
        option.list[,1:2] <- sapply(option.list[,1:2], as.character)
      } 
      opt.label <- string.clip(group.text[1], '<optgroup label=\"', '\"')
      assign(opt.label, option.list, envir = .GlobalEnv )
      
    }
  } else {
    opts <- grep('<option', option.text)
    option.list <- data.frame('value' = character(), 'option.label' = character(), stringsAsFactors = FALSE)
    for(opt in opts){
      val <- string.clip(option.text[opt], '<option value=\"', '\"')
      lab <- string.clip(option.text[opt],'>', '<')
      option.list[nrow(option.list)+1,] <- c(val, lab)
      option.list[,1:2] <- sapply(option.list[,1:2], as.character)
    } 
    opt.label <- 'options'
    assign(opt.label, option.list, envir = .GlobalEnv )
  }
}


get.data.entry <- function(html.line){
  string.start <- regexpr('<td', html.line, ignore.case = TRUE)[1]
  smaller.string <- substring(html.line, string.start)
  field.start <- (regexpr(">", smaller.string)[1])+1
  smaller.string <- substring(smaller.string, field.start)
  field.end <- (regexpr("</td>", smaller.string)[1])-1
  field.content <- substring(smaller.string, 1, field.end)
  return(field.content)
}

get.header.entry <- function(html.line){
  string.start <- regexpr('<th', html.line, ignore.case = TRUE)[1]
  smaller.string <- substring(html.line, string.start)
  field.start <- (regexpr(">", smaller.string)[1])+1
  smaller.string <- substring(smaller.string, field.start)
  field.end <- (regexpr("</th>", smaller.string)[1])-1
  field.content <- substring(smaller.string, 1, field.end)
  return(field.content)
}

get.data.row <- function(html.text, start.line = 1){
  actual.text <- html.text[start.line:length(html.text)]
  row.text <- rem.lines("<tr", "</tr>", actual.text)
  line <- vector()
  if(length(grep("<th", tolower(row.text)))>0){
    fields <- grep("<th", tolower(row.text))
    for(field in fields){
      line <- c(line, get.header.entry(row.text[field]))
    }
  } else {
    fields <- grep("<td", tolower(row.text))
    for(field in fields){
      line <- c(line, get.data.entry(row.text[field]))
    }
  }
  return(line)
}


get.header.row <- function(html.text, start.line = 1){
  actual.text <- html.text[start.line:length(html.text)]
  row.text <- rem.lines("<th", "</th>", actual.text)
  fields <- grep("<th", row.text, ignore.case = TRUE)
  line <- vector()
  for(field in fields){
    line <- c(line, get.data.entry(row.text[field]))
  }
  return(line)
}

get.table.section <- function(html.text, start.line = 1, section = "<table>"){
  if(substring(section, 1, 1)!= "<"){section=paste0("<",  section)}
  if(substring(section, nchar(section), nchar(section))!= ">"){section=paste0(section, ">")}
  actual.text <- html.text[start.line:length(html.text)]
  section.start <- substring(section, 1, nchar(section)-1)
  section.end <- paste0("</", substring(section, 2, nchar(section)))
  section.text <- rem.lines(section.start, section.end, actual.text)
  header.rows <- data.frame()
  data.rows <- data.frame()
  row.numbers <- grep("<tr", section.text)
  if(length(row.numbers)==0){
    datas <- grep("<td", section.text)
    headers <- grep("<th", section.text)
    either <- union(datas, headers)
    either <- either[order(either)]
    line <- vector()
    for(crazy in either){
      if(regexpr("<th", section.text[crazy])>0){
        field <- get.header.entry(section.text[crazy])
      } else {field <- get.data.entry(section.text[crazy])}
      line[length(line)+1] <- field
    }
    data.rows <- data.frame(matrix(NA, nrow=0, ncol = length(line)))
    data.rows <- rbind(data.rows, line)
    data.rows[,1:ncol(data.rows)] <- sapply(data.rows[,1:ncol(data.rows)], as.character)
  }
  
  for(this.row in row.numbers){
    end.row <- grep("</tr", section.text[this.row:length(section.text)])[1]
    if(length(grep("<th", section.text[this.row:this.row+end.row]))>0){
      header <- get.data.row(section.text, start.line = this.row)
      no.rows <- nrow(header.rows)
      if(no.rows<1){
        header.rows <- data.frame(matrix(NA, nrow=0, ncol = length(header)))
        header.rows[,1:ncol(header.rows)] <- sapply(header.rows[,1:ncol(header.rows)], as.character)
      }
      header.rows<-rbind(header.rows,header)
      header.rows[,1:ncol(header.rows)] <- sapply(header.rows[,1:ncol(header.rows)], as.character)
      
    } else {
      line <- get.data.row(section.text, start.line = this.row)
      if(nrow(data.rows)<1){
        data.rows <- data.frame(matrix(NA, nrow=0, ncol = length(line)))
        data.rows[,1:ncol(data.rows)] <- sapply(data.rows[,1:ncol(data.rows)], as.character)
      }
      data.rows <- rbind(data.rows, line)
      data.rows[,1:ncol(data.rows)] <- sapply(data.rows[,1:ncol(data.rows)], as.character)
    }
  }
  if(nrow(header.rows)>1){
    actual.header <- header.rows[1,]
    additional.body <- header.rows[2:nrow(header.rows),]
  } else if(nrow(header.rows)==1){
    actual.header <- header.rows[1,]
    additional.body <- c()
  } else if(nrow(header.rows)==0){
    actual.header <- c()
    additional.body <- c()
  }
  
  if((nrow(data.rows)==0) & (length(additional.body)>0)){
    data.rows <- additional.body
  } else if((nrow(data.rows)>0) & (length(additional.body)>0)){
    data.rows <- rbind(additional.body, rbind)
  } else if(length(actual.header>0) & (nrow(data.rows)==0) & (length(additional.body)==0)){
    data.rows <- actual.header
  }
  names(data.rows) <- actual.header
  if((length(actual.header)>0) & (nrow(data.rows)==1)){
    if(actual.header[,1]==data.rows[1]){
      names(data.rows) <- NULL
    }
  }
  return(data.rows)
}


get.table <- function(html.text, start.line = 1, foot = TRUE){
  actual.text <- html.text[start.line:length(html.text)]
  full.table <- rem.lines("<table", "</table>", actual.text)
  table <- data.frame()
  header <- vector()
  body <- data.frame()
  footy <- data.frame()
  if(length(grep("<thead", full.table))>0){
    header <- get.table.section(full.table, section = "thead")
  }
  if(length(grep("<tbody", full.table))>0){
    body <- get.table.section(full.table, section = "tbody")
  }
  if(length(grep("<tfoot", full.table))>0){
    footy <- get.table.section(full.table, section = "tfoot")
  }
  if(length(header) == 0 & length(body) == 0 & length(footy) == 0){
    table <- get.table.section(actual.text)
  } else if(length(header) == 0 & length(body) > 0 & length(footy) == 0){
    table <- body
  } else if(length(header) > 0 & length(body) == 0 & length(footy) == 0){
    table <- header
  } else if(length(header) == 0 & length(body) == 0 & length(footy) > 0){
    if(foot == TRUE){full.table <- footy}
  } else if(length(header) > 0 & length(body) > 0 & length(footy) == 0){
    table <- body
    names(table) <- header[1,]
  } else if(length(header) > 0 & length(body) == 0 & length(footy) > 0){
    if(foot==TRUE){full.table <- footy}
    names(table) <- header[1,]
  } else if(length(header) > 0 & length(body) > 0 & length(footy) > 0){
    if(foot == TRUE){
    thead <- paste0("V", seq(1:ncol(body)))
    names(body) <- thead
    names(footy) <- thead
    table <- rbind(body, footy)
    } else {table <- body}
    names(table) <- header[1,]
  } else if(foot == TRUE){
    names(body) <- thead
    names(footy) <- thead
    table <- rbind(body, footy)
  } else {table <- body}
  
  return(table)
  
}

get.all.tables <- function(url, excel=FALSE){
  if(excel){
    save.dir <- paste0(path.expand('~'),"/")
  }
  html.text <- readLines(url)
  if(length(grep("<title", html.text))>0){
    t <- rem.lines("<title", "</title>", html.text)
    titlepage <- string.clip(t, "<title", "</title>")
    titlepage <- str_replace_all(titlepage, " ", "_")
    titlepage <- str_replace_all(titlepage, "-", "_")
    if(!is.na(suppressWarnings(as.integer(substring(titlepage,1,1))))){
      titlepage <- paste0("w", titlepage)}
  } else {titlepage <- "Web_Page"}
  if(excel){save.file <- paste0(save.dir, titlepage, ".xlsx")}
#  all.tables <- data.frame("table.id" = character(), "table" = data.frame())
  
  no.tables <- grep("<table", html.text)
  c <- 1
  for(this.table in no.tables){
    full.table <- get.table(html.text, start.line = this.table)
    table.id <- paste(titlepage, as.character(c), sep = "_")
    assign(table.id, full.table, envir = .GlobalEnv )
    if(excel){
      if(c==1){
        write.xlsx(full.table, save.file, sheetName = paste(substring(titlepage, 1, 25), as.character(c), sep = "-"), row.names = FALSE, append = FALSE)
      } else {
        write.xlsx(full.table, save.file, sheetName = paste(substring(titlepage, 1, 25), as.character(c), sep = "-"), row.names = FALSE, append = TRUE)

      }
    }
    c <- c+1
    
  }
}
