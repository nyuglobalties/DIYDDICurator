# splat generation

splat <- function(x, f) {
  do.call(f, x)
}

# element functions to put into splat...

generate_titlStmt <- function(dat) {
  if(is.null(dat$stdyDscr$citation$titlStmt$titl[[1]]$lang)) dat$stdyDscr$citation$titlStmt$titl[[1]]$lang <- NA_character_
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
      ddi_parTitl(value, lang = lang)
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
      ddi_AuthEnty(name, affiliation = affiliation)  
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
      ddi_othId(name, role = role, affiliation = affiliation)
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
      splat(c(ID = ID, URI = URI, serName, serInfo), ddi_serStmt)
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
      ddi_serName(value, abbr = abbr, lang = lang)        
    }
  )
}

descr_serInfo <- function(dat) {
  ds <- tibble(value = character(), lang = character())
  for(n in dat) {
    ds <- add_row(ds, value = n$value, lang = n$lang)
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang),
    .f = function(value, lang) {
      ddi_serInfo(value, lang = lang)
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
    .l = list(name = ds$name, abbr = ds$abbr, affiliation = ds$affiliation, role = ds$role),
    .f = function(name, abbr, affiliation, role) {
      ddi_producer(name, abbr = abbr, affiliation = affiliation, role = role)  
    }
  )
}

descr_prodPlac <- function(dat) {
  ds <- tibble(value = character())
  for(n in dat$stdyDscr$citation$prodStmt$prodPlac) {
    ds <- add_row(ds, value = n$value)
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
      ddi_prodDate(value, date = date, lang = lang)
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
    .l = list(name = ds$name, abbr = ds$abbr, role = ds$role),
    .f = function(name, abbr, role) {
      ddi_fundAg(name, abbr = abbr, role = role)
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
      ddi_anlyUnit(group, lang = lang)  
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
    .l = list(group = ds$group, level = ds$level, clusion = ds$clusion, lang = ds$lang),
    .f = function(group, level, clusion, lang) {
      ddi_universe(group, level = level, clusion = clusion, lang = lang)  
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
      ddi_nation(value, abbr = abbr, lang = lang)  
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
      ddi_geogCover(value, lang = lang)
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
      ddi_geogUnit(value, lang = lang)
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
      ddi_dataKind(value, type = type, lang = lang)
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
      ddi_timePrd(value, date = date, event = event, cycle = cycle, lang = lang)  
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
      ddi_collDate(value, date = date, event = event, cycle = cycle, lang = lang)  
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
      ddi_abstract(value, contentType = contentType, lang = lang)
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
      ddi_keyword(keyword, vocab = vocab, vocabURI = vocabURI, lang = lang)  
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
      ddi_collectorTraining(value, type = type, lang = lang)  
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
      ddi_timeMeth(value, lang = lang)  
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
      ddi_frequenc(value, lang = lang)
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
      ddi_dataCollector(value, abbr = abbr, affiliation = affiliation, role = role, lang = lang)
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
      ddi_collMode(value, lang = lang)
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
      ddi_collSitu(value, lang = lang)  
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
      ddi_resInstru(value, type = type, lang = lang)
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
      ddi_instrumentDevelopment(value, type = type, lang = lang)  
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
      ddi_ConOps(value, agency = agency, lang = lang)
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
      ddi_actMin(value, lang = lang)
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
      ddi_sampProc(value, lang = lang)  
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
      ddi_deviat(value, lang = lang)
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
      ddi_labl(value, lang = lang, level = level)
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
      ddi_defntn(value, lang = lang)
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
      ddi_concept(value, vocab = vocab, vocabURI = vocabURI, lang = lang)  
    }
  )
}

descr_varGrp <- function(dat) {
  ds <- tibble(name = character(), 
               type = character(),
               otherType = character(),
               var = character(),
               varGrp = character(),
               labl = list(),
               defntn = list(), 
               universe = list(),
               concept = list()
  )
  for(vg in dat$dataDscr$varGrp) {
    ds <- add_row(ds, 
                  name = vg$name[[1]], 
                  type = vg$type[[1]],
                  otherType = vg$otherType[[1]],
                  var = vg[["var"]][[1]],
                  varGrp = vg$varGrp[[1]],
                  labl = list(vg$labl),
                  defntn = list(vg$defntn),
                  universe = list(vg$universe),
                  concept = list(vg$concept))
  }
  pmap(
    .l = list(name = ds$name, type = ds$type, otherType = ds$otherType, 
              var = ds$var, varGrp = ds$varGrp, labl = ds$labl, defntn = ds$defntn, 
              universe = ds$universe, concept = ds$concept),
    .f = function(name, type, otherType, varGrp, var, labl, universe, defntn, concept) {
      splat(c(ID = name, name = name, type = type, otherType = otherType, 
              varGrp = varGrp, var = var, descr_labl(labl), descr_concept(concept), 
              descr_defntn(defntn), descr_universe(universe)), ddi_varGrp) 
    }
  ) 
}

descr_var <- function(dat) {
  ds <- tibble(name = character(), 
               nature = character(),
               temporal = character(),
               geog = character(),
               labl = list(),
               anlysUnit = list(), 
               respUnit = list(),
               security = list(),
               embargo = list(),
               catgry = list()
  )
  for(v in dat$dataDscr[["var"]]) {
    ds <- add_row(ds, 
                  name = v$name[[1]], 
                  nature = v$nature[[1]],
                  temporal = v$temporal[[1]],
                  geog = v$geog[[1]],
                  labl = list(v$labl),
                  anlysUnit = list(v$anlysUnit),
                  respUnit = list(v$respUnit),
                  security = list(v$security),
                  embargo = list(v$embargo),
                  catgry = list(v$catgry)
    )
  }
  pmap(
    .l = list(name = ds$name, nature = ds$nature, temporal = ds$temporal, 
              geog = ds$geog, labl = ds$labl, anlysUnit = ds$anlysUnit, 
              respUnit = ds$respUnit, security = ds$security, 
              embargo = ds$embargo, catgry = ds$catgry),
    .f = function(name, nature, temporal, geog, labl, anlysUnit, respUnit, 
                  security, embargo, catgry) {
      splat(c(ID = name, varname = name, nature = nature, temporal = temporal, geog = geog,
              descr_labl(labl), descr_anlysUnit(anlysUnit), descr_respUnit(respUnit), 
              descr_security(security), descr_embargo(embargo), descr_catgry(catgry)), ddi_var) 
    }
  ) 
}

descr_anlysUnit <- function(dat) {
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
      ddi_anlysUnit(value, lang = lang)
    }
  )
}

descr_respUnit <- function(dat) {
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
      ddi_respUnit(value, lang = lang)
    }
  )
}

descr_embargo <- function(dat) {
  if(!is.data.frame(dat)) {
    ds <- tibble(value = character(),
                 date = character(),
                 event = character(),
                 lang = character())
    for(n in dat) {
      ds <- add_row(ds,
                    value = n$value,
                    date = n$date,
                    event = n$event,
                    lang = n$lang
      )
    }
  } else {
    ds <- dat
  }
  pmap(
    .l = list(value = ds$value, date = ds$date, event = ds$event, lang = ds$lang),
    .f = function(value, date, event, lang) {
      ddi_embargo(value, date = date, event = event, lang = lang)
    }
  )
}

descr_security <- function(dat) {
  if(!is.data.frame(dat)) {
    ds <- tibble(value = character(),
                 date = character(),
                 lang = character())
    for(n in dat) {
      ds <- add_row(ds,
                    value = n$value,
                    date = n$date,
                    event = n$event,
                    lang = n$lang
      )
    }
  } else {
    ds <- dat
  }
  pmap(
    .l = list(value = ds$value, date = ds$date, lang = ds$lang),
    .f = function(value, date, event, lang) {
      ddi_security(value, date = date, lang = lang)
    }
  )
}

descr_catgry <- function(dat) {
  if(!is.data.frame(dat)) {
    ds <- tibble(catValu = character(),
                 labl = list())
    for(n in dat) {
      ds <- add_row(ds,
                    catValu = n$catValu,
                    labl = list(n$labl),
      )
    }
  } else {
    ds <- dat
  }
  pmap(
    .l = list(catValu = ds$catValu, labl = ds$labl),
    .f = function(catValu, labl) {
      catgry <- splat(descr_labl(labl), ddi_catgry)
      catgry$content <- append(catgry$content, list(ddi_catValu(catValu)))
      catgry
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
      if(!is.na(description)) {
        if(!is.na(biblCit) & !is.na(doi)) {
          ddi_relMat(description, ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format),
                                  ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(is.na(biblCit) & !is.na(doi)) {
          ddi_relMat(description, ID = id, lang = lang, 
                     ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(!is.na(biblCit) & is.na(doi)) {
          ddi_relMat(description, ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format)))
        } else {
          ddi_relMat(description, ID = id, lang = lang)
        } 
      } else {
        if(!is.na(biblCit) & !is.na(doi)) {
          ddi_relMat(ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format),
                                  ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(is.na(biblCit) & !is.na(doi)) {
          ddi_relMat(ID = id, lang = lang, 
                     ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(!is.na(biblCit) & is.na(doi)) {
          ddi_relMat(ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format)))
        } else {
          ddi_relMat(ID = id, lang = lang)
        } 
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
      if(!is.na(description)) {
        if(!is.na(biblCit) & !is.na(doi)) {
          ddi_relPubl(description, ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format),
                                  ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(is.na(biblCit) & !is.na(doi)) {
          ddi_relPubl(description, ID = id, lang = lang, 
                     ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(!is.na(biblCit) & is.na(doi)) {
          ddi_relPubl(description, ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format)))
        } else {
          ddi_relPubl(description, ID = id, lang = lang)
        } 
      } else {
        if(!is.na(biblCit) & !is.na(doi)) {
          ddi_relPubl(ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format),
                                  ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(is.na(biblCit) & !is.na(doi)) {
          ddi_relPubl(ID = id, lang = lang, 
                     ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(!is.na(biblCit) & is.na(doi)) {
          ddi_relPubl(ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format)))
        } else {
          ddi_relPubl(ID = id, lang = lang)
        } 
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
      if(!is.na(description)) {
        if(!is.na(biblCit) & !is.na(doi)) {
          ddi_relStdy(description, ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format),
                                  ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(is.na(biblCit) & !is.na(doi)) {
          ddi_relStdy(description, ID = id, lang = lang, 
                     ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(!is.na(biblCit) & is.na(doi)) {
          ddi_relStdy(description, ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format)))
        } else {
          ddi_relStdy(description, ID = id, lang = lang)
        } 
      } else {
        if(!is.na(biblCit) & !is.na(doi)) {
          ddi_relStdy(ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format),
                                  ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(is.na(biblCit) & !is.na(doi)) {
          ddi_relStdy(ID = id, lang = lang, 
                     ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(!is.na(biblCit) & is.na(doi)) {
          ddi_relStdy(ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format)))
        } else {
          ddi_relStdy(ID = id, lang = lang)
        } 
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
      if(!is.na(description)) {
        if(!is.na(biblCit) & !is.na(doi)) {
          ddi_othRefs(description, ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format),
                                  ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(is.na(biblCit) & !is.na(doi)) {
          ddi_othRefs(description, ID = id, lang = lang, 
                     ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(!is.na(biblCit) & is.na(doi)) {
          ddi_othRefs(description, ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format)))
        } else {
          ddi_othRefs(description, ID = id, lang = lang)
        } 
      } else {
        if(!is.na(biblCit) & !is.na(doi)) {
          ddi_othRefs(ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format),
                                  ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(is.na(biblCit) & !is.na(doi)) {
          ddi_othRefs(ID = id, lang = lang, 
                     ddi_citation(ddi_titlStmt(ddi_IDNo(doi, agency = "doi"))))
        } else if(!is.na(biblCit) & is.na(doi)) {
          ddi_othRefs(ID = id, lang = lang, 
                     ddi_citation(ddi_biblCit(biblCit, format = format)))
        } else {
          ddi_othRefs(ID = id, lang = lang)
        } 
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
  var <- descr_var(dat)
  cb <- ddi_codeBook(ddi_stdyDscr(citation))
  if(!is.null(stdyInfo$content)) cb$content[[1]]$content <- append(cb$content[[1]]$content, list(stdyInfo))
  if(!is.null(method$content)) cb$content[[1]]$content <- append(cb$content[[1]]$content, list(method))
  if(!is.null(othrStdyMat$content)) cb$content[[1]]$content <- append(cb$content[[1]]$content, list(othrStdyMat))
  if(length(varGrp) > 0 & length(var) > 0) {
    cb$content <- append(cb$content, list(splat(c(varGrp, var), ddi_dataDscr)))
  } else if(length(varGrp) > 0 & length(var) == 0) {
    cb$content <- append(cb$content, list(splat(varGrp, ddi_dataDscr)))
  } else if(length(varGrp) == 0 & length(var) > 0) { 
    cb$content <- append(cb$content, list(splat(var, ddi_dataDscr)))
  }
  

  #ddi <- gsub("&lt;", "<", ddi)
  #ddi <- gsub("&gt;", ">", ddi)
  #ddi <- xml2::read_xml(ddi)
  cb
}