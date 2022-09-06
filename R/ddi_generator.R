devtools::load_all("/Users/danielwoulfin/Documents/Github/rddi")
library(tidyverse)
library(xml2)

# splat generation

splat <- function(x, f) {
  do.call(f, x)
}

# element functions to put into splat...

generate_titlStmt <- function(dat) {
  if(!is.na(dat$stdyDscr$citation$titlStmt$titl[[1]]$lang)) {
    r <- ddi_titlStmt(ddi_titl(dat$stdyDscr$citation$titlStmt$titl[[1]]$value, lang = dat$stdyDscr$citation$titlStmt$titl[[1]]$lang))
  } else {
    r <- ddi_titlStmt(ddi_titl(dat$stdyDscr$citation$titlStmt$titl[[1]]$value))
  }

  if(length(dat$stdyDscr$citation$titlStmt$parTitl) > 0) {
    r$content <- append(r$content, descr_parTitl(dat))
  }  

  r
}

descr_parTitl <- function(dat) {
  ds <- tibble(value = character(), lang = character())
  for(pt in dat$stdyDscr$citation$titlStmt$parTitl) {
    ds <- add_row(ds, value = pt$value, lang = pt$lang)
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang),
    .f = function(value, lang) {
      if(!is.na(lang)) {
        ddi_parTitl(value, lang = lang)
      } else {
        ddi_parTitl(value)
      }
    }
  )
}

descr_AuthEnty <- function(dat) {
  ds <- tibble(name = character(), affiliation = character())
  for(auth in dat$stdyDscr$citation$rspStmt$AuthEnty) {
    ds <- add_row(ds, name = auth$name, affiliation = auth$affiliation)
  }
  pmap(
    .l = list(name = ds$name, 
              affiliation = ds$affiliation),
    .f = function(name, affiliation) {
      if(!is.na(affiliation)) {
        ddi_AuthEnty(name, affiliation = affiliation)  
      } else {
        ddi_AuthEnty(name)
      }
      
    }
  )
}

descr_othId <- function(dat) {
  ds <- tibble(name = character(), role = character(), affiliation = character())
  for(othId in dat$stdyDscr$citation$rspStmt$othId) {
    ds <- add_row(ds, name = othId$name, 
                  role = othId$role, 
                  affiliation = othId$affiliation)
  }
  pmap(
    .l = list(name = ds$name, 
              role = ds$role,
              affiliation = ds$affiliation),
    .f = function(name, role, affiliation) {
      if(!is.na(role) & !is.na(affiliation)) {
        ddi_othId(name, role = role, affiliation = affiliation)
      } else if (is.na(role) & !is.na(affiliation)) {
        ddi_othId(name, affiliation = affiliation)
      } else if (!is.na(role) & is.na(affiliation)) {
        ddi_othId(name, role = role)
      } else {
        ddi_othId(name)
      }
    }
  )
}

descr_serStmt <- function(dat) {
  ds <- tibble(ID = character(), 
               URI = character(),
               serName = list(), 
               serInfo = list())
  for(s in dat$stdyDscr$citation$serStmt) {
    ds <- add_row(ds, 
                  ID = s$ID, 
                  URI = s$URI, 
                  serName = list(descr_serName(s$serName)),
                  serInfo = list(descr_serInfo(s$serInfo)))
  }
  pmap(
    .l = list(ID = ds$ID, URI = ds$URI, serName = ds$serName, serInfo = ds$serInfo),
    .f = function(ID, URI, serName, serInfo) {
      if(!is.na(URI) & length(serName) > 0 & length(serInfo) > 0){
        r <- splat(c(ID = ID, URI = URI, serName, serInfo), ddi_serStmt)
      } else if(is.na(URI) & length(serName) > 0 & length(serInfo) > 0){
        r <- splat(c(ID = ID, serName, serInfo), ddi_serStmt)
      } else if(!is.na(URI) & length(serName) == 0 & length(serInfo) > 0){
        r <- splat(c(ID = ID, URI = URI, serInfo), ddi_serStmt)
      } else if(!is.na(URI) & length(serName) > 0 & length(serInfo) == 0){
        r <- splat(c(ID = ID, URI = URI, serName), ddi_serStmt)
      } else if(is.na(URI) & length(serName) == 0 & length(serInfo) > 0){
        r <- splat(c(ID = ID, serInfo), ddi_serStmt)
      } else if(is.na(URI) & length(serName) > 0 & length(serInfo) == 0){
        r <- splat(c(ID = ID, serName), ddi_serStmt)
      } 
      r
    }
  )
}

descr_serName <- function(dat) {
  ds <- tibble(value = character(), abbr = character(), lang = character())
  for(n in dat) {
    ds <- add_row(ds, value = n$value, abbr = n$abbr, lang = n$lang)
  }
  pmap(
    .l = list(value = ds$value, abbr = ds$abbr, lang = ds$lang),
    .f = function(value, abbr, lang) {
      if(!is.na(abbr) & !is.na(lang)) {
        ddi_serName(value, abbr = abbr, lang = lang)        
      } else if(is.na(abbr) & !is.na(lang)) {
        ddi_serName(value, lang = lang)        
      } else if(!is.na(abbr) & is.na(lang)) {
        ddi_serName(value, abbr = abbr)        
      } else {
        ddi_serName(value)        
      }
    }
  )
}

descr_serInfo <- function(dat) {
  ds <- tibble(value = character(), lang = character())
  for(n in dat) {
    ds <- add_row(ds, value = n$value, lang = n$lang)
  }
  pmap(
    .l = list(value = ds$value, 
              lang = ds$lang),
    .f = function(value, lang) {
      if(!is.na(lang)) {
        ddi_serInfo(value, lang = lang)
      } else {
        ddi_serInfo(value)
      }
    }
  )
}

descr_producer <- function(dat) {
  ds <- tibble(name = character(), 
               abbr = character(), 
               affiliation = character(),
               role = character())
  for(n in dat$stdyDscr$citation$prodStmt$producer) {
    ds <- add_row(ds, 
                  name = n$name, 
                  abbr = n$abbr,
                  affiliation = n$affiliation,
                  role = n$role)
  }
  pmap(
    .l = list(name = ds$name, 
              abbr = ds$abbr,
              affiliation = ds$affiliation,
              role = ds$role),
    .f = function(name, abbr, affiliation, role) {
      if(!is.na(abbr) & !is.na(affiliation) & !is.na(role)) {
        ddi_producer(name, abbr = abbr, affiliation = affiliation, role = role)  
      } else if (!is.na(abbr) & !is.na(affiliation) & is.na(role)) {
        ddi_producer(name, abbr = abbr, affiliation = affiliation)  
      } else if (!is.na(abbr) & is.na(affiliation) & !is.na(role)) {
        ddi_producer(name, abbr = abbr, role = role)  
      } else if (is.na(abbr) & !is.na(affiliation) & !is.na(role)) {
        ddi_producer(name, affiliation = affiliation, role = role)  
      } else if (!is.na(abbr) & is.na(affiliation) & is.na(role)) {
        ddi_producer(name, abbr = abbr)  
      } else if (is.na(abbr) & !is.na(affiliation) & is.na(role)) {
        ddi_producer(name, affiliation = affiliation)  
      } else if(is.na(abbr) & is.na(affiliation) & !is.na(role)) {
        ddi_producer(name, role = role)  
      } else {
        ddi_producer(name)
      }
    }
  )
}

descr_prodPlac <- function(dat) {
  ds <- tibble(value = character())
  for(n in dat$stdyDscr$citation$prodStmt$prodPlac) {
    ds <- add_row(ds, 
                  value = n$value)
  }
  pmap(
    .l = list(value = ds$value),
    .f = function(value) ddi_prodPlac(value)
  )
}

descr_prodDate <- function(dat) {
  ds <- tibble(value = character(),
               date = character(),
               lang = character())
  for(n in dat$stdyDscr$citation$prodStmt$prodDate) {
    ds <- add_row(ds, 
                  value = n$value,
                  date = n$date,
                  lang = n$lang)
  }
  pmap(
    .l = list(value = ds$value, date = ds$date, lang = ds$lang),
    .f = function(value, date, lang) {
      if(!is.na(date) & ! is.na(lang)) {
        ddi_prodDate(value, date = date, lang = lang)
      } else if(!is.na(date) & is.na(lang)) {
        ddi_prodDate(value, date = date)
      } else if(is.na(date) & !is.na(lang)) {
        ddi_prodDate(value, lang = lang)
      } else {
        ddi_prodDate(value)
      }
    }
  )
}

descr_fundAg <- function(dat) {
  ds <- tibble(name = character(), 
               abbr = character(), 
               role = character())
  for(n in dat$stdyDscr$citation$prodStmt$fundAg) {
    ds <- add_row(ds, 
                  name = n$name, 
                  abbr = n$abbr,
                  role = n$role)
  }
  pmap(
    .l = list(name = ds$name, 
              abbr = ds$abbr,
              role = ds$role),
    .f = function(name, abbr, role) {
      if(!is.na(abbr) & !is.na(role)) {
        ddi_fundAg(name, abbr = abbr, role = role)
      } else if(is.na(abbr) & !is.na(role)) {
        ddi_fundAg(name, role = role)
      } else if(!is.na(abbr) & is.na(role)) {
        ddi_fundAg(name, abbr = abbr)
      } else {
        ddi_fundAg(name)
      }
    }
  )
}

descr_anlyUnit <- function(dat) {
  ds <- tibble(group = character(), 
               lang = character())
  for(n in dat$stdyDscr$stdyInfo$sumDscr$anlyUnit) {
    ds <- add_row(ds, 
                  group = n$group, 
                  lang = n$lang)
  }
  pmap(
    .l = list(group = ds$group, 
              lang = ds$lang),
    .f = function(group, lang) {
      if(is.na(lang)) {
        ddi_anlyUnit(group)
      } else {
        ddi_anlyUnit(group, lang = lang)  
      }
    }
  )
}

descr_universe <- function(dat) {
  if(!is.data.frame(dat)) {
    ds <- tibble(group = character(), 
                 level = character(),
                 clusion = character(),
                 lang = character())
    for(n in dat) {
      ds <- add_row(ds, 
                    group = n$group,
                    level = n$level,
                    clusion = n$clusion,
                    lang = n$lang)
    }
  } else {
    ds <- dat
  }
  pmap(
    .l = list(group = ds$group, 
              level = ds$level,
              clusion = ds$clusion,
              lang = ds$lang),
    .f = function(group, level, clusion, lang) {
      if(!is.na(level) & !is.na(clusion) & !is.na(lang)) {
        ddi_universe(group, level = level, clusion = clusion, lang = lang)  
      } else if(!is.na(level) & !is.na(clusion) & is.na(lang)) {
        ddi_universe(group, level = level, clusion = clusion)  
      } else if(!is.na(level) & is.na(clusion) & !is.na(lang)) {
        ddi_universe(group, level = level, lang = lang)  
      } else if(!is.na(level) & is.na(clusion) & is.na(lang)) {
        ddi_universe(group, level = level)  
      } else if(is.na(level) & !is.na(clusion) & !is.na(lang)) {
        ddi_universe(group, clusion = clusion, lang = lang)  
      } else if(is.na(level) & !is.na(clusion) & is.na(lang)) {
        ddi_universe(group, clusion = clusion)  
      } else if(is.na(level) & is.na(clusion) & !is.na(lang)) {
        ddi_universe(group, lang = lang)  
      } else {
        ddi_universe(group)
      }
    }
  )
}

descr_nation <- function(dat) {
  ds <- tibble(value = character(), 
               abbr = character(),
               lang = character())
  for(n in dat$stdyDscr$stdyInfo$sumDscr$nation) {
    ds <- add_row(ds, 
                  value = n$value,
                  abbr = n$abbr,
                  lang = n$lang)
  }
  pmap(
    .l = list(value = ds$value, abbr = ds$abbr, lang = ds$lang),
    .f = function(value, abbr, lang) {
      if(!is.na(abbr) & !is.na(lang)) {
        ddi_nation(value, abbr = abbr, lang = lang)  
      } else if(is.na(abbr) & !is.na(lang)) {
        ddi_nation(value, lang = lang)  
      } else if(!is.na(abbr) & is.na(lang)) {
        ddi_nation(value, abbr = abbr)  
      } else {
        ddi_nation(value)  
      }
    }
  )
}

descr_geogCover <- function(dat) {
  ds <- tibble(value = character(), 
               lang = character())
  for(n in dat$stdyDscr$stdyInfo$sumDscr$geogCover) {
    ds <- add_row(ds, 
                  value = n$value,
                  lang = n$lang)
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang),
    .f = function(value, lang) {
      if(!is.na(lang)) {
        ddi_geogCover(value, lang = lang)
      } else {
        ddi_geogCover(value)
      }
    }
  )
}

descr_geogUnit <- function(dat) {
  ds <- tibble(value = character(), 
               lang = character())
  for(n in dat$stdyDscr$stdyInfo$sumDscr$geogUnit) {
    ds <- add_row(ds, 
                  value = n$value,
                  lang = n$lang)
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang),
    .f = function(value, lang) {
      if(!is.na(lang)) {
        ddi_geogUnit(value, lang = lang)
      } else {
        ddi_geogUnit(value)
      }
    }
  )
}

descr_dataKind <- function(dat) {
  ds <- tibble(value = character(),
               type = character(),
               lang = character())
  for(n in dat$stdyDscr$stdyInfo$sumDscr$dataKind) {
    ds <- add_row(ds, 
                  value = n$value,
                  type = n$type,
                  lang = n$lang)
  }
  pmap(
    .l = list(value = ds$value, type = ds$type, lang = ds$lang),
    .f = function(value, type, lang) {
      if(!is.na(type) & !is.na(lang)) {
        ddi_dataKind(value, type = type, lang = lang)
      } else if (is.na(type) & !is.na(lang)) {
        ddi_dataKind(value, lang = lang)
      } else if (!is.na(type) & is.na(lang)) {
        ddi_dataKind(value, type = type)
      } else {
        ddi_dataKind(value)
      }
    }
  )
}

# The goal would be to autogenerate the date in a specific format depending on the language 
# which also isn't taken into account in the app
descr_timePrd <- function(dat) {
  ds <- tibble(value = character(),
               date = character(), 
               event = character(),
               cycle = character(),
               lang = character())
  for(n in dat$stdyDscr$stdyInfo$sumDscr$timePrd) {
    ds <- add_row(ds, 
                  value = n$value,
                  date = n$date,
                  event = n$event,
                  cycle = n$cycle,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(value = ds$value, date = ds$date, event = ds$event, cycle = ds$cycle, lang = ds$lang),
    .f = function(value, date, event, cycle, lang) {
      if(!is.na(date) & !is.na(event) & !is.na(cycle) & !is.na(lang)) {
        ddi_timePrd(value, date = date, event = event, cycle = cycle, lang = lang)  
      } else if(!is.na(date) & !is.na(event) & !is.na(cycle) & is.na(lang)) {
        ddi_timePrd(value, date = date, event = event, cycle = cycle)  
      } else if(!is.na(date) & !is.na(event) & is.na(cycle) & !is.na(lang)) {
        ddi_timePrd(value, date = date, event = event, lang = lang)  
      } else if(!is.na(date) & !is.na(event) & is.na(cycle) & is.na(lang)) {
        ddi_timePrd(value, date = date, event = event)  
      } else if(!is.na(date) & is.na(event) & !is.na(cycle) & !is.na(lang)) {
        ddi_timePrd(value, date = date, cycle = cycle, lang = lang)  
      } else if(!is.na(date) & is.na(event) & !is.na(cycle) & is.na(lang)) {
        ddi_timePrd(value, date = date, cycle = cycle)  
      } else if(!is.na(date) & is.na(event) & is.na(cycle) & !is.na(lang)) {
        ddi_timePrd(value, date = date, lang = lang)  
      } else if(!is.na(date) & is.na(event) & is.na(cycle) & is.na(lang)) {
        ddi_timePrd(value, date = date)  
      } else if(is.na(date) & !is.na(event) & !is.na(cycle) & !is.na(lang)) {
        ddi_timePrd(value, event = event, cycle = cycle, lang = lang)  
      } else if(is.na(date) & !is.na(event) & !is.na(cycle) & !is.na(lang)) {
        ddi_timePrd(value, event = event, cycle = cycle)  
      } else if(is.na(date) & !is.na(event) & is.na(cycle) & !is.na(lang)) {
        ddi_timePrd(value, event = event, lang = lang)  
      } else if(is.na(date) & !is.na(event) & is.na(cycle) & is.na(lang)) {
        ddi_timePrd(value, event = event)  
      } else if(is.na(date) & is.na(event) & !is.na(cycle) & !is.na(lang)) {
        ddi_timePrd(value, cycle = cycle, event = event)  
      } else if(is.na(date) & is.na(event) & !is.na(cycle) & is.na(lang)) {
        ddi_timePrd(value, cycle = cycle)  
      } else if(is.na(date) & is.na(event) & is.na(cycle) & !is.na(lang)) {
        ddi_timePrd(value, lang = lang)  
      } else {
        ddi_timePrd(value)
      }
    }
  )
}

descr_collDate <- function(dat) {
  ds <- tibble(value = character(),
               date = character(), 
               event = character(),
               cycle = character(),
               lang = character())
  for(n in dat$stdyDscr$stdyInfo$sumDscr$collDate) {
    ds <- add_row(ds, 
                  value = n$value,
                  date = n$date,
                  event = n$event,
                  cycle = n$cycle,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(value = ds$value, date = ds$date, event = ds$event, cycle = ds$cycle, lang = ds$lang),
    .f = function(value, date, event, cycle, lang) {
      if(!is.na(date) & !is.na(event) & !is.na(cycle) & !is.na(lang)) {
        ddi_collDate(value, date = date, event = event, cycle = cycle, lang = lang)  
      } else if(!is.na(date) & !is.na(event) & !is.na(cycle) & is.na(lang)) {
        ddi_collDate(value, date = date, event = event, cycle = cycle)  
      } else if(!is.na(date) & !is.na(event) & is.na(cycle) & !is.na(lang)) {
        ddi_collDate(value, date = date, event = event, lang = lang)  
      } else if(!is.na(date) & !is.na(event) & is.na(cycle) & is.na(lang)) {
        ddi_collDate(value, date = date, event = event)  
      } else if(!is.na(date) & is.na(event) & !is.na(cycle) & !is.na(lang)) {
        ddi_collDate(value, date = date, cycle = cycle, lang = lang)  
      } else if(!is.na(date) & is.na(event) & !is.na(cycle) & is.na(lang)) {
        ddi_collDate(value, date = date, cycle = cycle)  
      } else if(!is.na(date) & is.na(event) & is.na(cycle) & !is.na(lang)) {
        ddi_collDate(value, date = date, lang = lang)  
      } else if(!is.na(date) & is.na(event) & is.na(cycle) & is.na(lang)) {
        ddi_collDate(value, date = date)  
      } else if(is.na(date) & !is.na(event) & !is.na(cycle) & !is.na(lang)) {
        ddi_collDate(value, event = event, cycle = cycle, lang = lang)  
      } else if(is.na(date) & !is.na(event) & !is.na(cycle) & !is.na(lang)) {
        ddi_collDate(value, event = event, cycle = cycle)  
      } else if(is.na(date) & !is.na(event) & is.na(cycle) & !is.na(lang)) {
        ddi_collDate(value, event = event, lang = lang)  
      } else if(is.na(date) & !is.na(event) & is.na(cycle) & is.na(lang)) {
        ddi_collDate(value, event = event)  
      } else if(is.na(date) & is.na(event) & !is.na(cycle) & !is.na(lang)) {
        ddi_collDate(value, cycle = cycle, event = event)  
      } else if(is.na(date) & is.na(event) & !is.na(cycle) & is.na(lang)) {
        ddi_collDate(value, cycle = cycle)  
      } else if(is.na(date) & is.na(event) & is.na(cycle) & !is.na(lang)) {
        ddi_collDate(value, lang = lang)  
      } else {
        ddi_collDate(value)
      }
    }
  )
}

descr_abstract <- function(dat) {
  ds <- tibble(value = character(),
               contentType = character(),
               lang = character())
  for(n in dat$stdyDscr$stdyInfo$abstract) {
    ds <- add_row(ds, 
                  value = n$value,
                  contentType = n$contentType,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(value = ds$value, contentType = ds$contentType, lang = ds$lang),
    .f = function(value, contentType, lang) {
      if(!is.na(contentType) & !is.na(lang)) {
        ddi_abstract(value, contentType = contentType, lang = lang)
      } else if(is.na(contentType) & !is.na(lang)) {
        ddi_abstract(value, lang = lang)
      } else if(!is.na(contentType) & is.na(lang)) {
        ddi_abstract(value, contentType = contentType)
      } else {
        ddi_abstract(value)
      }
    }
  )
}

descr_keyword <- function(dat) {
  ds <- tibble(keyword = character(),
               vocab = character(),
               vocabURI = character(),
               lang = character())
  for(n in dat$stdyDscr$stdyInfo$subject) {
    ds <- add_row(ds, 
                  keyword = n$keyword,
                  vocab = n$vocabu,
                  vocabURI = n$vocab_URI,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(keyword = ds$keyword, vocab = ds$vocab, vocabURI = ds$vocabURI, lang = ds$lang),
    .f = function(keyword, vocab, vocabURI, lang) {
      if(!is.na(vocab) & !is.na(vocabURI) & !is.na(lang)) {
        ddi_keyword(keyword, vocab = vocab, vocabURI = vocabURI, lang = lang)  
      } else if(!is.na(vocab) & !is.na(vocabURI) & is.na(lang)) {
        ddi_keyword(keyword, vocab = vocab, vocabURI = vocabURI)  
      } else if(is.na(vocab) & !is.na(vocabURI) & !is.na(lang)) {
        ddi_keyword(keyword, vocabURI = vocabURI, lang = lang)  
      } else if(!is.na(vocab) & is.na(vocabURI) & !is.na(lang)) {
        ddi_keyword(keyword, vocab = vocab, lang = lang)  
      } else if(!is.na(vocab) & is.na(vocabURI) & is.na(lang)) {
        ddi_keyword(keyword, vocab = vocab)  
      } else if(is.na(vocab) & !is.na(vocabURI) & is.na(lang)) {
        ddi_keyword(keyword, vocabURI = vocabURI)  
      } else if(is.na(vocab) & is.na(vocabURI) & !is.na(lang)) {
        ddi_keyword(keyword, lang = lang)  
      } else {
        ddi_keyword(keyword)
      }
    }
  )
}

descr_collectorTraining <- function(dat) {
  ds <- tibble(value = character(),
               type = character(),
               lang = character())
  for(n in dat$stdyDscr$method$dataColl$collectorTraining) {
    ds <- add_row(ds, 
                  value = n$value,
                  type = n$type,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(value = ds$value, type = ds$type, lang = ds$lang),
    .f = function(value, type, lang) {
      if(is.na(type) & is.na(lang)) {
        ddi_collectorTraining(value)
      } else if(is.na(type) & !is.na(lang)) {
        ddi_collectorTraining(value, lang = lang)
      } else if(!is.na(type) & is.na(lang)) {
        ddi_collectorTraining(value, type = type)
      } else {
        ddi_collectorTraining(value, type = type, lang = lang)  
      }
    }
  )
}

descr_timeMeth<- function(dat) {
  ds <- tibble(value = character(),
               lang = character())
  for(n in dat$stdyDscr$method$dataColl$timeMeth) {
    ds <- add_row(ds, 
                  value = n$value,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang),
    .f = function(value, lang) {
      if(!is.na(lang)) {
        ddi_timeMeth(value, lang = lang)  
      } else {
        ddi_timeMeth(value)
      }
      
    }
  )
}

descr_frequenc <- function(dat) {
  ds <- tibble(value = character(),
               lang = character())
  for(n in dat$stdyDscr$method$dataColl$frequenc) {
    ds <- add_row(ds, 
                  value = n$value,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang),
    .f = function(value, lang) {
      if(!is.na(lang)) {
        ddi_frequenc(value, lang = lang)
      } else {
        ddi_frequenc(value)
      }
    }
  )
}

descr_dataCollector <- function(dat) {
  ds <- tibble(value = character(),
               abbr = character(),
               affiliation = character(),
               role = character(),
               lang = character())
  for(n in dat$stdyDscr$method$dataColl$dataCollector) {
    ds <- add_row(ds, 
                  value = n$value,
                  abbr = n$abbr,
                  affiliation = n$affiliation,
                  role = n$role,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(value = ds$value, abbr = ds$abbr, affiliation = ds$affiliation, role = ds$role, lang = ds$lang),
    .f = function(value, abbr, affiliation, role, lang) {
      # all and no attributes
      if(!is.na(abbr) & !is.na(affiliation) & !is.na(role) & !is.na(lang)) {
        ddi_dataCollector(value, abbr = abbr, affiliation = affiliation, role = role, lang = lang)
      } else if(is.na(abbr) & is.na(affiliation) & is.na(role) & is.na(lang)) {
        ddi_dataCollector(value)
      # cases that include abbr
      } else if(!is.na(abbr) & is.na(affiliation) & is.na(role) & is.na(lang)) {
        ddi_dataCollector(value, abbr = abbr)
      } else if(!is.na(abbr) & !is.na(affiliation) & is.na(role) & is.na(lang)) {
        ddi_dataCollector(value, abbr = abbr, affiliation = affiliation)
      } else if(!is.na(abbr) & !is.na(affiliation) & !is.na(role) & is.na(lang)) {
        ddi_dataCollector(value, abbr = abbr, affiliation = affiliation, role = role)
      } else if(!is.na(abbr) & is.na(affiliation) & !is.na(role) & is.na(lang)) {
        ddi_dataCollector(value, abbr = abbr, role = role)
      } else if(!is.na(abbr) & is.na(affiliation) & !is.na(role) & !is.na(lang)) {
        ddi_dataCollector(value, abbr = abbr, role = role, lang = lang)
      } else if(!is.na(abbr) & !is.na(affiliation) & is.na(role) & !is.na(lang)) {
        ddi_dataCollector(value, abbr = abbr, affiliation = affiliation, lang = lang)
      } else if(!is.na(abbr) & is.na(affiliation) & is.na(role) & !is.na(lang)) {
        ddi_dataCollector(value, abbr = abbr, lang = lang)
      } else if(is.na(abbr) & !is.na(affiliation) & is.na(role) & is.na(lang)) {
      #affiliation cases  
        ddi_dataCollector(value, affiliation = affiliation)
      } else if(is.na(abbr) & !is.na(affiliation) & !is.na(role) & is.na(lang)) {
        ddi_dataCollector(value, affiliation = affiliation, role = role)
      } else if(is.na(abbr) & !is.na(affiliation) & !is.na(role) & !is.na(lang)) {
        ddi_dataCollector(value, affiliation = affiliation, role = role, lang = lang)
      } else if(is.na(abbr) & !is.na(affiliation) & is.na(role) & !is.na(lang)) {
        ddi_dataCollector(value, affiliation = affiliation, lang = lang)
      } else if(is.na(abbr) & is.na(affiliation) & !is.na(role) & is.na(lang)) {
      #role cases
        ddi_dataCollector(value, role = role)
      } else if (is.na(abbr) & is.na(affiliation) & !is.na(role) & !is.na(lang)) {
        ddi_dataCollector(value, role = role, lang = lang)
      } else if(is.na(abbr) & is.na(affiliation) & is.na(role) & !is.na(lang)) {
        #lang case
        ddi_dataCollector(value, lang = lang)
      }
    }
  )
}

descr_collMode <- function(dat) {
  ds <- tibble(value = character(),
               lang = character())
  for(n in dat$stdyDscr$method$dataColl$collMode) {
    ds <- add_row(ds, 
                  value = n$value,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang),
    .f = function(value, lang) {
      if(is.na(lang)) {
        ddi_collMode(value)
      } else {
        ddi_collMode(value, lang = lang)
      }
    }
  )
}

descr_collSitu <- function(dat) {
  ds <- tibble(value = character(),
               lang = character())
  for(n in dat$stdyDscr$method$dataColl$collSitu) {
    ds <- add_row(ds, 
                  value = n$value,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang),
    .f = function(value, lang) {
      if(is.na(lang)) {
        ddi_collSitu(value)
      } else {
        ddi_collSitu(value, lang = lang)  
      }
    }
  )
}

descr_resInstru <- function(dat) {
  ds <- tibble(value = character(),
               type = character(),
               lang = character())
  for(n in dat$stdyDscr$method$dataColl$resInstru) {
    ds <- add_row(ds, 
                  value = n$value,
                  type = n$type,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(value = ds$value, type = ds$type, lang = ds$lang),
    .f = function(value, type, lang) {
      if(!is.na(type) & !is.na(lang)) {
        ddi_resInstru(value, type = type, lang = lang)
      } else if(is.na(type) & !is.na(lang)) {
        ddi_resInstru(value, lang = lang)
      } else if(!is.na(type) & is.na(lang)) {
        ddi_resInstru(value, type = type)
      } else {
        ddi_resInstru(value)
      }
    }
  )
}

descr_instrumentDevelopment <- function(dat) {
  ds <- tibble(value = character(),
               type = character(),
               lang = character())
  for(n in dat$stdyDscr$method$dataColl$instrumentDevelopment) {
    ds <- add_row(ds, 
                  value = n$value,
                  type = n$type,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(value = ds$value, type = ds$type, lang = ds$lang),
    .f = function(value, type, lang) {
      if(!is.na(lang) & !is.na(type)) {
        ddi_instrumentDevelopment(value, type = type, lang = lang)  
      } else if(!is.na(lang) & is.na(type)) {
        ddi_instrumentDevelopment(value, lang = lang)  
      } else if(is.na(lang) & !is.na(type)) {
        ddi_instrumentDevelopment(value, type = type)  
      } else {
        ddi_instrumentDevelopment(value)  
      }
    }
  )
}

descr_ConOps <- function(dat) {
  ds <- tibble(value = character(),
               agency = character(),
               lang = character())
  for(n in dat$stdyDscr$method$dataColl$ConOps) {
    ds <- add_row(ds, 
                  value = n$value,
                  agency = n$agency,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(value = ds$value, agency = ds$agency, lang = ds$lang),
    .f = function(value, agency, lang) {
      if(is.na(agency) & is.na(lang)) {
        ddi_ConOps(value)
      } else if (!is.na(agency) & is.na(lang)) {
        ddi_ConOps(value, agency = agency)
      } else if (is.na(agency) & !is.na(lang)) {
        ddi_ConOps(value, lang = lang)
      } else {
        ddi_ConOps(value, agency = agency, lang = lang)
      }
    }
  )
}

descr_actMin <- function(dat) {
  ds <- tibble(value = character(),
               lang = character())
  for(n in dat$stdyDscr$method$dataColl$actMin) {
    ds <- add_row(ds, 
                  value = n$value,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang),
    .f = function(value, lang) {
      if(is.na(lang)) {
        ddi_actMin(value)
      } else {
        ddi_actMin(value, lang = lang)
      }
    }
  )
}

descr_sampProc <- function(dat) {
  ds <- tibble(value = character(), 
               lang = character())
  for(n in dat$stdyDscr$method$dataColl$sampProc) {
    ds <- add_row(ds, 
                  value = n$value, 
                  lang = n$lang)
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang),
    .f = function(value, lang) {
      if(!is.na(lang)) {
        ddi_sampProc(value, lang = lang)  
      } else {
        ddi_sampProc(value)
      }
    }
  )
}

descr_deviat <- function(dat) {
  ds <- tibble(value = character(), 
               lang = character())
  for(n in dat$stdyDscr$method$dataColl$deviat) {
    ds <- add_row(ds, 
                  value = n$value, 
                  lang = n$lang)
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang),
    .f = function(value, lang) {
      if(!is.na(lang)){
        ddi_deviat(value, lang = lang)
      } else {
        ddi_deviat(value)
      }
    }
  )
}

#-------------
# for dataDscr

descr_labl <- function(dat) {
  if(!is.data.frame(dat)) {
    ds <- tibble(value = character(),
                 lang = character(),
                 level = character())
    for(n in dat) {
      ds <- add_row(ds,
                    value = n$value,
                    lang = n$lang,
                    level = n$level
      )
    }
  } else {
    ds <- dat
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang, level = ds$level),
    .f = function(value, lang, level) {
      if(!is.na(lang) & !is.na(level)) {
        ddi_labl(value, lang = lang, level = level)
      } else if(is.na(lang) & !is.na(level)) {
        ddi_labl(value, level = level)
      } else if(!is.na(lang) & is.na(level)) {
        ddi_labl(value, lang = lang)
      } else {
        ddi_labl(value)
      }
    }
  )
}

descr_defntn <- function(dat) {
  if(!is.data.frame(dat)) {
    ds <- tibble(value = character(),
                 lang = character())
    for(n in dat) {
      ds <- add_row(ds,
                    value = n$value,
                    lang = n$lang
      )
    }
  } else {
    ds <- dat
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang),
    .f = function(value, lang) {
      if(!is.na(lang)) {
        ddi_defntn(value, lang = lang)
      } else {
        ddi_defntn(value)
      }
    }
  )
}

descr_concept <- function(dat) {
  if(!is.data.frame(dat)) {
    ds <- tibble(value = character(),
                 vocab = character(),
                 vocabURI = character(),
                 lang = character())
    for(n in dat) {
      ds <- add_row(ds,
                    value = n$value,
                    vocab = n$vocabu,
                    vocabURI = n$vocab_URI,
                    lang = n$lang
      )
    }
  } else {
    ds <- dat
  }
  pmap(
    .l = list(value = ds$value, vocab = ds$vocab, vocabURI = ds$vocabURI, lang = ds$lang),
    .f = function(value, vocab, vocabURI, lang) {
      if(!is.na(vocab) & !is.na(vocabURI) & !is.na(lang)) {
        ddi_concept(value, vocab = vocab, vocabURI = vocabURI, lang = lang)  
      } else if(!is.na(vocab) & !is.na(vocabURI) & is.na(lang)) {
        ddi_concept(value, vocab = vocab, vocabURI = vocabURI)  
      } else if(!is.na(vocab) & is.na(vocabURI) & !is.na(lang)) {
        ddi_concept(value, vocab = vocab, lang = lang)  
      } else if(!is.na(vocab) & is.na(vocabURI) & !is.na(lang)) {
        ddi_concept(value, vocab = vocab, lang = lang)  
      } else if(is.na(vocab) & !is.na(vocabURI) & !is.na(lang)) {
        ddi_concept(value, vocabURI = vocabURI, lang = lang)  
      } else if(!is.na(vocab) & is.na(vocabURI) & is.na(lang)) {
        ddi_concept(value, vocab = vocab)  
      } else if(is.na(vocab) & !is.na(vocabURI) & is.na(lang)) {
        ddi_concept(value, vocabURI = vocabURI)  
      } else if(is.na(vocab) & is.na(vocabURI) & !is.na(lang)) {
        ddi_concept(value, lang = lang)  
      } else {
        ddi_concept(value)
      }
    }
  )
}

descr_varGrp <- function(dat) {
  ds <- tibble(name = character(), 
               labl = list(),
               defntn = list(), 
               universe = list(),
               concept = list()
  )
  for(n in dat$dataDscr$varGrp) {
    ds <- add_row(ds, 
                  name = n$name[[1]], 
                  labl = list(n$labl),
                  defntn = list(n$defntn),
                  universe = list(n$universe),
                  concept = list(n$concept))
  }
  pmap(
    .l = list(name = ds$name, labl = ds$labl, ds$defntn,
              universe = ds$universe, concept = ds$concept),
    .f = function(name, labl, universe, defntn, concept) {
      splat(c(name = name, descr_labl(labl), descr_concept(concept), descr_defntn(defntn), descr_universe(universe)), ddi_varGrp) 
    }
  ) 
}

#---------------------------------------
# For othrStdyMat
generate_othrStdyMat <- function(dat) {
  othrStdyMat <-ddi_othrStdyMat()
  relMat <- descr_relMat(dat)
  relPubl <- descr_relPubl(dat)
  relStdy <- descr_relStdy(dat)
  othRefs <- descr_othRefs(dat)
  if(length(relMat) > 0) othrStdyMat$content <- append(othrStdyMat$content, relMat)
  if(length(relPubl) > 0) othrStdyMat$content <- append(othrStdyMat$content, relPubl)
  if(length(relStdy) > 0) othrStdyMat$content <- append(othrStdyMat$content, relStdy)
  if(length(othRefs) > 0) othrStdyMat$content <- append(othrStdyMat$content, othRefs)  
  othrStdyMat
}

descr_relMat <- function(dat) {
  ds <- tibble(id = character(),
               description = character(),
               biblCit = character(),
               format = character(),
               doi = character(),
               lang  = character())
  for(n in dat$stdyDscr$othrStdyMat$relMat) {
    ds <- add_row(ds, 
                  id = n$id, 
                  description = n$description,
                  biblCit = n$biblCit,
                  format = n$format,
                  doi = n$doi,
                  lang = n$lang)
  }
  pmap(
    .l = list(id = ds$id, description = ds$description, biblCit = ds$biblCit,
              format = ds$format, doi = ds$doi, lang = ds$lang),
    .f = function(id, description, biblCit, format, doi, lang) {
      if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
         !is.na(doi) & !is.na(lang)){
        ddi_relMat(description, ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit, format = format),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_relMat(description, ID = id, 
                   ddi_citation(ddi_biblCit(biblCit, format = format),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no format
      } else if (!is.na(description) & !is.na(biblCit) & is.na(format) &
                 !is.na(doi) & !is.na(lang)){
        ddi_relMat(description, ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & !is.na(biblCit) & is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_relMat(description, ID = id, 
                   ddi_citation(ddi_biblCit(biblCit),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no doi
      } else if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
           is.na(doi) & !is.na(lang)){
          ddi_relMat(description, ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format))
          )
      } else if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
                  is.na(doi) & is.na(lang)) {
          ddi_relMat(description, ID = id, 
                     ddi_citation(ddi_biblCit(biblCit, format = format))
          )
          # no format and no doi
      } else if (!is.na(description) & !is.na(biblCit) & is.na(format) &
                   is.na(doi) & !is.na(lang)){
          ddi_relMat(description, ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit))
          )
      } else if(!is.na(description) & !is.na(biblCit) & is.na(format) &
                  is.na(doi) & is.na(lang)) {
          ddi_relMat(description, ID = id, 
                     ddi_citation(ddi_biblCit(biblCit))
          )
          # no citation 
      } else if(!is.na(description) & is.na(biblCit) &
                     !is.na(doi) & !is.na(lang)){
          ddi_relMat(description, ID = id, lang = lang, 
                     ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
          )
      } else if(!is.na(description) & !is.na(biblCit) &
                !is.na(doi) & is.na(lang)) {
        ddi_relMat(description, ID = id, 
                   ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no description
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                      !is.na(doi) & !is.na(lang)){
        ddi_relMat(ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit, format = format),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_relMat(ID = id, 
                   ddi_citation(ddi_biblCit(biblCit, format = format),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no description no format
      } else if (is.na(description) & !is.na(biblCit) & is.na(format) &
                 !is.na(doi) & !is.na(lang)){
        ddi_relMat(ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(is.na(description) & !is.na(biblCit) & is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_relMat(ID = id, 
                   ddi_citation(ddi_biblCit(biblCit),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & is.na(biblCit) &
                      is.na(doi) & !is.na(lang)) {
        ddi_relMat(description, ID = id, lang = lang)
      } else if(!is.na(description) & is.na(biblCit) & is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_relMat(description, ID = id)
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                is.na(doi) & !is.na(lang)) {
        ddi_relMat(ID = id, lang = lang, 
                   ddi_citation((ddi_biblCit(biblCit, format = format))))
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_relMat(ID = id, 
                   ddi_citation((ddi_biblCit(biblCit, format = format))))
      } else if(is.na(description) & !is.na(biblCit) & is.na(format) &
                is.na(doi) & !is.na(lang)) {
        ddi_relMat(ID = id, lang = lang, 
                   ddi_citation((ddi_biblCit(biblCit))))
      } else if(is.na(description) & !is.na(biblCit) & is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_relMat(ID = id, 
                   ddi_citation((ddi_biblCit(biblCit))))
      } else if(is.na(description) & is.na(biblCit) &
                !is.na(doi) & !is.na(lang)) {
        ddi_relMat(ID = id, lang = lang, 
                   ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
      } else if(is.na(description) & is.na(biblCit) &
                !is.na(doi) & is.na(lang)) {
        ddi_relMat(ID = id, 
                   ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
      } 
    }
  )
}

descr_relPubl <- function(dat) {
  ds <- tibble(id = character(),
               description = character(),
               biblCit = character(),
               format = character(),
               doi = character(),
               lang  = character())
  for(n in dat$stdyDscr$othrStdyMat$relPubl) {
    ds <- add_row(ds, 
                  id = n$id, 
                  description = n$description,
                  biblCit = n$biblCit,
                  format = n$format,
                  doi = n$doi,
                  lang = n$lang)
  }
  pmap(
    .l = list(id = ds$id, description = ds$description, biblCit = ds$biblCit,
              format = ds$format, doi = ds$doi, lang = ds$lang),
    .f = function(id, description, biblCit, format, doi, lang) {
      if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
         !is.na(doi) & !is.na(lang)){
        ddi_relPubl(description, ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit, format = format),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_relPubl(description, ID = id, 
                   ddi_citation(ddi_biblCit(biblCit, format = format),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no format
      } else if (!is.na(description) & !is.na(biblCit) & is.na(format) &
                 !is.na(doi) & !is.na(lang)){
        ddi_relPubl(description, ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & !is.na(biblCit) & is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_relPubl(description, ID = id, 
                   ddi_citation(ddi_biblCit(biblCit),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no doi
      } else if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
                is.na(doi) & !is.na(lang)){
        ddi_relPubl(description, ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit, format = format))
        )
      } else if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_relPubl(description, ID = id, 
                   ddi_citation(ddi_biblCit(biblCit, format = format))
        )
        # no format and no doi
      } else if (!is.na(description) & !is.na(biblCit) & is.na(format) &
                 is.na(doi) & !is.na(lang)){
        ddi_relPubl(description, ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit))
        )
      } else if(!is.na(description) & !is.na(biblCit) & is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_relPubl(description, ID = id, 
                   ddi_citation(ddi_biblCit(biblCit))
        )
        # no citation 
      } else if(!is.na(description) & is.na(biblCit) &
                !is.na(doi) & !is.na(lang)){
        ddi_relPubl(description, ID = id, lang = lang, 
                   ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & !is.na(biblCit) &
                !is.na(doi) & is.na(lang)) {
        ddi_relPubl(description, ID = id, 
                   ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no description
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                !is.na(doi) & !is.na(lang)){
        ddi_relPubl(ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit, format = format),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_relPubl(ID = id, 
                   ddi_citation(ddi_biblCit(biblCit, format = format),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no description no format
      } else if (is.na(description) & !is.na(biblCit) & is.na(format) &
                 !is.na(doi) & !is.na(lang)){
        ddi_relPubl(ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(is.na(description) & !is.na(biblCit) & is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_relPubl(ID = id, 
                   ddi_citation(ddi_biblCit(biblCit),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & is.na(biblCit) &
                is.na(doi) & !is.na(lang)) {
        ddi_relPubl(description, ID = id, lang = lang)
      } else if(!is.na(description) & is.na(biblCit) & is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_relPubl(description, ID = id)
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                is.na(doi) & !is.na(lang)) {
        ddi_relPubl(ID = id, lang = lang, 
                   ddi_citation((ddi_biblCit(biblCit, format = format))))
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_relPubl(ID = id, 
                   ddi_citation((ddi_biblCit(biblCit, format = format))))
      } else if(is.na(description) & !is.na(biblCit) & is.na(format) &
                is.na(doi) & !is.na(lang)) {
        ddi_relPubl(ID = id, lang = lang, 
                   ddi_citation((ddi_biblCit(biblCit))))
      } else if(is.na(description) & !is.na(biblCit) & is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_relPubl(ID = id, 
                   ddi_citation((ddi_biblCit(biblCit))))
      } else if(is.na(description) & is.na(biblCit) &
                !is.na(doi) & !is.na(lang)) {
        ddi_relPubl(ID = id, lang = lang, 
                   ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
      } else if(is.na(description) & is.na(biblCit) &
                !is.na(doi) & is.na(lang)) {
        ddi_relPubl(ID = id, 
                   ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
      } 
    }
  )
}

descr_relStdy <- function(dat) {
  ds <- tibble(id = character(),
               description = character(),
               biblCit = character(),
               format = character(),
               doi = character(),
               lang  = character())
  for(n in dat$stdyDscr$othrStdyMat$relStdy) {
    ds <- add_row(ds, 
                  id = n$id, 
                  description = n$description,
                  biblCit = n$biblCit,
                  format = n$format,
                  doi = n$doi,
                  lang = n$lang)
  }
  pmap(
    .l = list(id = ds$id, description = ds$description, biblCit = ds$biblCit,
              format = ds$format, doi = ds$doi, lang = ds$lang),
    .f = function(id, description, biblCit, format, doi, lang) {
      if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
         !is.na(doi) & !is.na(lang)){
        ddi_relStdy(description, ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit, format = format),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_relStdy(description, ID = id, 
                   ddi_citation(ddi_biblCit(biblCit, format = format),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no format
      } else if (!is.na(description) & !is.na(biblCit) & is.na(format) &
                 !is.na(doi) & !is.na(lang)){
        ddi_relStdy(description, ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & !is.na(biblCit) & is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_relStdy(description, ID = id, 
                   ddi_citation(ddi_biblCit(biblCit),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no doi
      } else if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
                is.na(doi) & !is.na(lang)){
        ddi_relStdy(description, ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit, format = format))
        )
      } else if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_relStdy(description, ID = id, 
                   ddi_citation(ddi_biblCit(biblCit, format = format))
        )
        # no format and no doi
      } else if (!is.na(description) & !is.na(biblCit) & is.na(format) &
                 is.na(doi) & !is.na(lang)){
        ddi_relStdy(description, ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit))
        )
      } else if(!is.na(description) & !is.na(biblCit) & is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_relStdy(description, ID = id, 
                   ddi_citation(ddi_biblCit(biblCit))
        )
        # no citation 
      } else if(!is.na(description) & is.na(biblCit) &
                !is.na(doi) & !is.na(lang)){
        ddi_relStdy(description, ID = id, lang = lang, 
                   ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & !is.na(biblCit) &
                !is.na(doi) & is.na(lang)) {
        ddi_relStdy(description, ID = id, 
                   ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no description
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                !is.na(doi) & !is.na(lang)){
        ddi_relStdy(ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit, format = format),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_relStdy(ID = id, 
                   ddi_citation(ddi_biblCit(biblCit, format = format),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no description no format
      } else if (is.na(description) & !is.na(biblCit) & is.na(format) &
                 !is.na(doi) & !is.na(lang)){
        ddi_relStdy(ID = id, lang = lang, 
                   ddi_citation(ddi_biblCit(biblCit),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(is.na(description) & !is.na(biblCit) & is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_relStdy(ID = id, 
                   ddi_citation(ddi_biblCit(biblCit),
                                ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & is.na(biblCit) &
                is.na(doi) & !is.na(lang)) {
        ddi_relStdy(description, ID = id, lang = lang)
      } else if(!is.na(description) & is.na(biblCit) & is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_relStdy(description, ID = id)
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                is.na(doi) & !is.na(lang)) {
        ddi_relStdy(ID = id, lang = lang, 
                   ddi_citation((ddi_biblCit(biblCit, format = format))))
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_relStdy(ID = id, 
                   ddi_citation((ddi_biblCit(biblCit, format = format))))
      } else if(is.na(description) & !is.na(biblCit) & is.na(format) &
                is.na(doi) & !is.na(lang)) {
        ddi_relStdy(ID = id, lang = lang, 
                   ddi_citation((ddi_biblCit(biblCit))))
      } else if(is.na(description) & !is.na(biblCit) & is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_relStdy(ID = id, 
                   ddi_citation((ddi_biblCit(biblCit))))
      } else if(is.na(description) & is.na(biblCit) &
                !is.na(doi) & !is.na(lang)) {
        ddi_relStdy(ID = id, lang = lang, 
                   ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
      } else if(is.na(description) & is.na(biblCit) &
                !is.na(doi) & is.na(lang)) {
        ddi_relStdy(ID = id, 
                   ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
      } 
    }
  )
}

descr_othRefs <- function(dat) {
  ds <- tibble(id = character(),
               description = character(),
               biblCit = character(),
               format = character(),
               doi = character(),
               lang  = character())
  for(n in dat$stdyDscr$othrStdyMat$othRefs) {
    ds <- add_row(ds, 
                  id = n$id, 
                  description = n$description,
                  biblCit = n$biblCit,
                  format = n$format,
                  doi = n$doi,
                  lang = n$lang)
  }
  pmap(
    .l = list(id = ds$id, description = ds$description, biblCit = ds$biblCit,
              format = ds$format, doi = ds$doi, lang = ds$lang),
    .f = function(id, description, biblCit, format, doi, lang) {
      if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
         !is.na(doi) & !is.na(lang)){
        ddi_othRefs(description, ID = id, lang = lang, 
                    ddi_citation(ddi_biblCit(biblCit, format = format),
                                 ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_othRefs(description, ID = id, 
                    ddi_citation(ddi_biblCit(biblCit, format = format),
                                 ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no format
      } else if (!is.na(description) & !is.na(biblCit) & is.na(format) &
                 !is.na(doi) & !is.na(lang)){
        ddi_othRefs(description, ID = id, lang = lang, 
                    ddi_citation(ddi_biblCit(biblCit),
                                 ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & !is.na(biblCit) & is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_othRefs(description, ID = id, 
                    ddi_citation(ddi_biblCit(biblCit),
                                 ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no doi
      } else if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
                is.na(doi) & !is.na(lang)){
        ddi_othRefs(description, ID = id, lang = lang, 
                    ddi_citation(ddi_biblCit(biblCit, format = format))
        )
      } else if(!is.na(description) & !is.na(biblCit) & !is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_othRefs(description, ID = id, 
                    ddi_citation(ddi_biblCit(biblCit, format = format))
        )
        # no format and no doi
      } else if (!is.na(description) & !is.na(biblCit) & is.na(format) &
                 is.na(doi) & !is.na(lang)){
        ddi_othRefs(description, ID = id, lang = lang, 
                    ddi_citation(ddi_biblCit(biblCit))
        )
      } else if(!is.na(description) & !is.na(biblCit) & is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_othRefs(description, ID = id, 
                    ddi_citation(ddi_biblCit(biblCit))
        )
        # no citation 
      } else if(!is.na(description) & is.na(biblCit) &
                !is.na(doi) & !is.na(lang)){
        ddi_othRefs(description, ID = id, lang = lang, 
                    ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & !is.na(biblCit) &
                !is.na(doi) & is.na(lang)) {
        ddi_othRefs(description, ID = id, 
                    ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no description
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                !is.na(doi) & !is.na(lang)){
        ddi_othRefs(ID = id, lang = lang, 
                    ddi_citation(ddi_biblCit(biblCit, format = format),
                                 ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_othRefs(ID = id, 
                    ddi_citation(ddi_biblCit(biblCit, format = format),
                                 ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
        # no description no format
      } else if (is.na(description) & !is.na(biblCit) & is.na(format) &
                 !is.na(doi) & !is.na(lang)){
        ddi_othRefs(ID = id, lang = lang, 
                    ddi_citation(ddi_biblCit(biblCit),
                                 ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(is.na(description) & !is.na(biblCit) & is.na(format) &
                !is.na(doi) & is.na(lang)) {
        ddi_othRefs(ID = id, 
                    ddi_citation(ddi_biblCit(biblCit),
                                 ddi_titlStmt(ddi_IDNo(doi, agency = "doi")))
        )
      } else if(!is.na(description) & is.na(biblCit) &
                is.na(doi) & !is.na(lang)) {
        ddi_othRefs(description, ID = id, lang = lang)
      } else if(!is.na(description) & is.na(biblCit) & is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_othRefs(description, ID = id)
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                is.na(doi) & !is.na(lang)) {
        ddi_othRefs(ID = id, lang = lang, 
                    ddi_citation((ddi_biblCit(biblCit, format = format))))
      } else if(is.na(description) & !is.na(biblCit) & !is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_othRefs(ID = id, 
                    ddi_citation((ddi_biblCit(biblCit, format = format))))
      } else if(is.na(description) & !is.na(biblCit) & is.na(format) &
                is.na(doi) & !is.na(lang)) {
        ddi_othRefs(ID = id, lang = lang, 
                    ddi_citation((ddi_biblCit(biblCit))))
      } else if(is.na(description) & !is.na(biblCit) & is.na(format) &
                is.na(doi) & is.na(lang)) {
        ddi_othRefs(ID = id, 
                    ddi_citation((ddi_biblCit(biblCit))))
      } else if(is.na(description) & is.na(biblCit) &
                !is.na(doi) & !is.na(lang)) {
        ddi_othRefs(ID = id, lang = lang, 
                    ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
      } else if(is.na(description) & is.na(biblCit) &
                !is.na(doi) & is.na(lang)) {
        ddi_othRefs(ID = id, 
                    ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
      } 
    }
  )
}

#-------------------------------------------
generate_stdyInfo <- function(dat) {
  stdyInfo <- ddi_stdyInfo()
  keyword <- descr_keyword(dat)
  abstract <- descr_abstract(dat)
  sumDscr <- splat(
    c(descr_timePrd(dat), descr_collDate(dat), descr_nation(dat),
      descr_geogCover(dat), descr_geogUnit(dat), 
      descr_anlyUnit(dat),
      descr_universe(dat$stdyDscr$stdyInfo$sumDscr$universe), 
      descr_dataKind(dat)),
    ddi_sumDscr)
  if(length(keyword) > 0) stdyInfo$content <- append(stdyInfo$content, list(splat(descr_keyword(dat), ddi_subject)))
  if(length(abstract) > 0) stdyInfo$content <- append(stdyInfo$content, abstract)
  if(!is.null(sumDscr$content)) stdyInfo$content <- append(stdyInfo$content, list(sumDscr))
  if(length(dat$stdyDscr$othrStdyMat$relPubl) > 0 |
     length(dat$stdyDscr$othrStdyMat$relStdy) > 0 |
     length(dat$stdyDscr$othrStdyMat$relMat) > 0 |
     length(dat$stdyDscr$othrStdyMat$othRefs) > 0) {
  }
  stdyInfo
}

generate_citation <- function(dat) {
  citation <- ddi_citation(generate_titlStmt(dat))
  if(length(dat$stdyDscr$citation$rspStmt$AuthEnty) > 0 | length(dat$stdyDscr$citation$rspStmt$othId) > 0) {
    citation$content <- append(citation$content,
                               list(splat(append(descr_AuthEnty(dat), descr_othId(dat)), ddi_rspStmt)))
  }
  if(length(dat$stdyDscr$citation$prodStmt$producer) > 0 |
     length(dat$stdyDscr$citation$prodStmt$prodPlac) > 0 |
     length(dat$stdyDscr$citation$prodStmt$prodDate) > 0 |
     length(dat$stdyDscr$citation$prodStmt$fundAg) > 0) {
       citation$content <- append(citation$content,
                                  list(descr_prodStmt(dat)))
     }
  citation$content <- append(citation$content, descr_serStmt(dat))
  
  citation
}

descr_prodStmt <- function(dat) {
  prodStmt <- ddi_prodStmt()
  producer <- descr_producer(dat)
  prodPlac <- descr_prodPlac(dat)
  prodDate <- descr_prodDate(dat)
  fundAg <- descr_fundAg(dat)
  if(length(producer) > 0) {
    prodStmt$content <- producer
  }
  if(length(prodDate) > 0 & is.null(prodStmt$content)) {
    prodStmt$content <- prodDate
  } else {
    prodStmt$content <- append(prodStmt$content, prodDate)
  }
  if(length(prodPlac) > 0 & is.null(prodStmt$content)) {
    prodStmt$content <- prodPlac
  } else {
    prodStmt$content <- append(prodStmt$content, prodPlac)
  }
  if(length(fundAg) > 0 & is.null(prodStmt$content)) {
    prodStmt$content <- fundAg
  } else {
    prodStmt$content <- append(prodStmt$content, fundAg)
  }
  prodStmt
}

generate_method <- function(dat) {
  dataColl <- splat(
    c(descr_timeMeth(dat), descr_dataCollector(dat), 
      descr_collectorTraining(dat), descr_frequenc(dat), 
      descr_sampProc(dat), descr_deviat(dat),
      descr_collMode(dat), descr_resInstru(dat), 
      descr_instrumentDevelopment(dat), descr_collSitu(dat),
      descr_actMin(dat), descr_ConOps(dat)
    ), 
    ddi_dataColl)
  if(!is.null(dataColl$content)) {
    method <- ddi_method(dataColl)
  } else {
    method <- ddi_method()
  }
  
  method
}



#-------------------------------------------

generate_ddi_codebook <- function(dat) {
  citation <- generate_citation(dat)
  stdyInfo <- generate_stdyInfo(dat)
  method <- generate_method(dat)
  othrStdyMat <- generate_othrStdyMat(dat)
  varGrp <- descr_varGrp(dat)
  cb <- ddi_codeBook(ddi_stdyDscr(citation))
  if(!is.null(stdyInfo$content)) cb$content[[1]]$content <- append(cb$content[[1]]$content, list(stdyInfo))
  if(!is.null(method$content)) cb$content[[1]]$content <- append(cb$content[[1]]$content, list(method))
  if(!is.null(othrStdyMat$content)) cb$content[[1]]$content <- append(cb$content[[1]]$content, list(othrStdyMat))
  if(length(varGrp) > 0) cb$content <- append(cb$content, list(splat(varGrp, ddi_dataDscr)))

  ddi <- as_xml(cb)
  
  ddi <- gsub("&lt;", "<", ddi)
  ddi <- gsub("&gt;", ">", ddi)
  ddi
}