devtools::load_all("/Users/danielwoulfin/Documents/Github/rddi")
library(tidyverse)
library(xml2)

# splat generation

splat <- function(x, f) {
  do.call(f, x)
}

# element functions to put into splat...

descr_AuthEnty <- function(dat) {
  ds <- tibble(name = character(), affiliation = character())
  for(auth in dat$stdyDscr$citation$rspStmt$AuthEnty) {
    ds <- add_row(ds, name = auth$name, affiliation = auth$affiliation)
  }
  pmap(
    .l = list(name = ds$name, 
              affiliation = ds$affiliation),
    .f = function(name, affiliation) ddi_AuthEnty(name, affiliation = affiliation)
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
    .f = function(name, role, affiliation) ddi_othId(name, role = role, affiliation = affiliation)
  )
}

descr_serName <- function(dat) {
  ds <- tibble(value = character(), abbr = character(), lang = character())
  for(n in dat$stdyDscr$citation$serStmt$serName) {
    ds <- add_row(ds, value = n$value, abbr = n$abbr, lang = n$lang)
  }
  pmap(
    .l = list(value = ds$value, 
              abbr = ds$abbr,
              lang = ds$lang),
    .f = function(value, abbr, lang) ddi_serName(value, abbr = abbr, lang = lang)
  )
}

descr_serInfo <- function(dat) {
  ds <- tibble(value = character(), lang = character())
  for(n in dat$stdyDscr$citation$serStmt$serInfo) {
    ds <- add_row(ds, value = n$value, lang = n$lang)
  }
  pmap(
    .l = list(value = ds$value, 
              lang = ds$lang),
    .f = function(value, abbr, lang) ddi_serInfo(value, lang = lang)
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
    .f = function(name, abbr, affiliation, role) ddi_producer(name, abbr = abbr, affiliation = affiliation, role = role)
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
    .f = function(name, abbr, role) ddi_fundAg(name, abbr = abbr, role = role)
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
    .f = function(group, lang) ddi_anlyUnit(group, lang = lang)
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
    .f = function(group, level, clusion, lang) ddi_universe(group, level = level, clusion = clusion, lang = lang)
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
    .f = function(value, abbr, lang) ddi_nation(value, abbr = abbr, lang = lang)
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
    .f = function(value, lang) ddi_geogCover(value, lang = lang)
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
    .f = function(value, lang) ddi_geogUnit(value, lang = lang)
  )
}

descr_dataKind <- function(dat) {
  ds <- tibble(value = character(), 
               lang = character())
  for(n in dat$stdyDscr$stdyInfo$sumDscr$dataKind) {
    ds <- add_row(ds, 
                  value = n$value,
                  lang = n$lang)
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang),
    .f = function(value, lang) ddi_dataKind(value, lang = lang)
  )
}

# The goal would be to autogenerate the date in a specific format depending on the language 
# which also isn't taken into account in the app
descr_timePrd <- function(dat) {
  ds <- tibble(date = character(), 
               event = character(),
               cycle = character(),
               value = character())
  for(n in dat$stdyDscr$stdyInfo$sumDscr$timePrd) {
    ds <- add_row(ds, 
                  date = n$date,
                  event = n$event,
                  cycle = n$cycle,
                  value = format(lubridate::ymd(n$date), format = "%B %d, %Y")
    )
  }
  pmap(
    .l = list(date = ds$date, event = ds$event, cycle = ds$cycle, value = ds$value),
    .f = function(date, event, cycle, value) ddi_timePrd(value, date = date, event = event, cycle = cycle, lang = "en")
  )
}

descr_collDate <- function(dat) {
  ds <- tibble(date = character(), 
               event = character(),
               cycle = character(),
               value = character())
  for(n in dat$stdyDscr$stdyInfo$sumDscr$collDate) {
    ds <- add_row(ds, 
                  date = n$date,
                  event = n$event,
                  cycle = n$cycle,
                  value = format(lubridate::ymd(n$date), format = "%B %d, %Y")
    )
  }
  pmap(
    .l = list(date = ds$date, event = ds$event, cycle = ds$cycle, value = ds$value),
    .f = function(date, event, cycle, value) ddi_collDate(value, date = date, event = event, cycle = cycle, lang = "en")
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
    .f = function(value, contentType, lang) ddi_abstract(value, contentType = contentType, lang = lang)
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
                  vocab = n$vocab,
                  vocabURI = n$vocabURI,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(keyword = ds$keyword, vocab = ds$vocab, vocabURI = ds$vocabURI, lang = ds$lang),
    .f = function(keyword, vocab, vocabURI, lang) ddi_keyword(keyword, vocab = vocab, vocabURI = vocabURI, lang = lang)
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
    .f = function(value, type, lang) ddi_collectorTraining(value, type = type, lang = lang)
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
    .f = function(value, lang) ddi_timeMeth(value, lang = lang)
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
    .f = function(value, lang) ddi_frequenc(value, lang = lang)
  )
}

descr_dataCollector <- function(dat) {
  ds <- tibble(value = character(),
               lang = character())
  for(n in dat$stdyDscr$method$dataColl$dataCollector) {
    ds <- add_row(ds, 
                  value = n$value,
                  lang = n$lang
    )
  }
  pmap(
    .l = list(value = ds$value, lang = ds$lang),
    .f = function(value, lang) ddi_dataCollector(value, lang = lang)
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
    .f = function(value, lang) ddi_collMode(value, lang = lang)
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
    .f = function(value, lang) ddi_collSitu(value, lang = lang)
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
    .f = function(value, type, lang) ddi_resInstru(value, type = type, lang = lang)
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
    .f = function(value, type, lang) ddi_instrumentDevelopment(value, type = type, lang = lang)
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
    .f = function(value, agency, lang) ddi_ConOps(value, agency = agency, lang = lang)
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
    .f = function(value, lang) ddi_actMin(value, lang = lang)
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
    .f = function(value, lang) ddi_sampProc(value, lang = lang)
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
    .f = function(value, lang) ddi_sampProc(value, lang = lang)
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
    .f = function(value, lang, level) ddi_labl(value, lang = lang, level = level)
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
    .f = function(value, lang) ddi_defntn(value, lang = lang)
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
                    vocab = n$vocab,
                    vocabURI = n$vocabURI,
                    lang = n$lang
      )
    }
  } else {
    ds <- dat
  }
  pmap(
    .l = list(value = ds$value, vocab = ds$vocab, vocabURI = ds$vocabURI, lang = ds$lang),
    .f = function(value, lang, vocab, vocabURI) ddi_concept(value, vocab = vocab, vocabURI = vocabURI, lang = lang)
  )
}

descr_varGrp <- function(dat) {
  ds <- tibble(name = character(), 
               labl = list(),
               defntn = list(), 
               universe = list(),
               concept = list()
  )
  #problem with below...this loop adds a new row for every list
  for(n in dat$dataDscr$varGrp) {
    ds <- add_row(ds, 
                  name = n$name, 
                  labl = list(n$labl),
                  defntn = list(n$defntn),
                  universe = list(n$universe),
                  concept = list(n$concept))
  }
  pmap(
    .l = list(name = ds$name, labl = ds$labl, 
              universe = ds$universe, concept = ds$concept),
    .f = function(name, labl, universe, concept) {
      splat(c(name = name, descr_labl(labl), descr_universe(universe), descr_concept(concept)), ddi_varGrp) 
    }
  ) # need to add back in defntn after fixing the function
}
#-------------------------------------------
generate_stdyInfo <- function(dat) {
  stdyInfo <- ddi_stdyInfo(
    splat(
      c(descr_dataKind(dat), descr_anlyUnit(dat), descr_universe(dat$stdyDscr$stdyInfo$sumDscr$universe),
        descr_nation(dat), descr_geogCover(dat),descr_geogUnit(dat),
        descr_timePrd(dat), descr_collDate(dat)),
      ddi_sumDscr),
    splat(descr_keyword(dat), ddi_subject)
  )
  
  stdyInfo$content <- append(stdyInfo$content, descr_abstract(dat))
  stdyInfo
}

#-------------------------------------------

generate_ddi_codebook <- function(dat) {
  cb <- ddi_codeBook(
    ddi_stdyDscr(
      ddi_citation(
        
        ddi_titlStmt(
          ddi_titl(dat$stdyDscr$citation$titlStmt$titl)
        ),
        # for rspStmt I have to append the authors and other contributors together...
        splat(append(descr_AuthEnty(dat), descr_othId(dat)), ddi_rspStmt),
        splat(append(descr_serName(dat), descr_serInfo(dat)), ddi_serStmt),
        splat(append(append(descr_producer(dat),descr_fundAg(dat)),
                     list(ddi_prodPlac(dat$stdyDscr$citation$prodStmt$prodPlac),
                          ddi_prodDate(dat$stdyDscr$citation$prodStmt$prodDate, 
                                       date = dat$stdyDscr$citation$prodStmt$`prodDate-date`)
                     )
        ), 
        ddi_prodStmt)
      ),
      generate_stdyInfo(dat),
      ddi_method(splat(
        c(descr_collectorTraining(dat), descr_timeMeth(dat),
          descr_frequenc(dat), descr_dataCollector(dat),
          descr_collMode(dat), descr_collSitu(dat),
          descr_resInstru(dat), descr_instrumentDevelopment(dat),
          descr_ConOps(dat), descr_actMin(dat),
          descr_sampProc(dat), descr_deviat(dat)), 
        ddi_dataColl)
      )
    ),
    splat(descr_varGrp(dat), ddi_dataDscr)
  )
  
  ddi <- as_xml(cb)
}