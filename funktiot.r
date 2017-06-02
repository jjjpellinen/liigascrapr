haeLinkit <- function(url) {
  ## Hakee linkit kauden otteluiden
  ## seurantasivuille ja pelaajatilastosivuille
  kaikkiOttelut <- read_html(url)
  kaikkiOttelut %>%
    html_nodes("a") %>%
    html_attr("href") -> linkit
  
  seurInd <-
    which(substr(
      linkit,
      start = nchar(linkit) - 9,
      stop = nchar(linkit)
    ) == "/seuranta/")
  
  tilInd <-
    which(substr(
      linkit,
      start = nchar(linkit) - 9,
      stop = nchar(linkit)
    ) == "/tilastot/")
  linkit[seurInd] -> seurannat
  linkit[tilInd] -> tilastot
  
  seurannat <- paste0("http://liiga.fi", seurannat)
  tilastot <- paste0("http://liiga.fi", tilastot)
  return(data.frame(
    seuranta = seurannat,
    tilasto = tilastot,
    stringsAsFactors = F
  ))
}


pelaajahaku <- function() {
  message("haetaan linkit...")
  Sivut <- rbind(
    haeLinkit("http://liiga.fi/ottelut/2014-2015/runkosarja/"),
    haeLinkit("http://liiga.fi/ottelut/2014-2015/playoffs/"),
    haeLinkit("http://liiga.fi/ottelut/2015-2016/runkosarja/"),
    haeLinkit("http://liiga.fi/ottelut/2015-2016/playoffs/"),
    haeLinkit("http://liiga.fi/ottelut/2016-2017/runkosarja/"),
    haeLinkit("http://liiga.fi/ottelut/2016-2017/playoffs/")
  )
  message("pelaajadata...")
  msg <- "haetaan pelaajadataa"
  
  pb <- winProgressBar(
    title = "Haetaan pelaajadataa",
    label = " ... 0% ",
    min = 0,
    max = nrow(Sivut),
    initial = 0
  )
  
  ID <- 1
  
  for (i in 1:nrow(Sivut)) {
    
    ## errorin tullessa koitetaan uudelleen 
    ok <- FALSE
    attempt <- 0
    while( ok == FALSE && attempt <= 10) {
      attempt <- attempt + 1
      out <- tryCatch({  
        til <- read_html(Sivut$tilasto[i])  
      },
      error = function(e) e
      )
      if("error" %in% class(out)) {
        Sys.sleep(30)
        cat(".")
      } else {
        ok <- TRUE
      }
    }
    
    til %>% 
      html_nodes("title") %>% 
      html_text() %>%
      str_split(., "[|]") %>% .[[1]] %>% .[2] %>%
      str_split(., " ") %>% unlist %>% .[c(2, 4, 5)] -> otsikko
    
    koti <- otsikko[1]
    vieras <- otsikko[2]
    pvm <- otsikko[3] %>% as.Date("%d.%m.%Y")
    
    Sivut$tilasto[i] %>% str_split("/") %>%
      unlist %>% .[5] %>% str_replace("-", "") -> kausi
    
    taulut <- til %>% html_nodes("table")
    pelIND <- which(taulut %>% html_attr("class") == "player-stats")
    mvIND <- which(taulut %>% html_attr("class") == "goalie-stats")
    
    
    kotipelaajat <-
      taulut %>% .[pelIND[1]] %>%  html_nodes("a") %>% html_attr("href")
    kotipelaajat <- kotipelaajat[which(kotipelaajat != "#")]
    
    kotimv <-
      taulut %>% .[mvIND[1]] %>%  html_nodes("a") %>% html_attr("href")
    kotimv <- kotimv[which(kotimv != "#")]
    
    vieraspelaajat <-
      taulut %>% .[pelIND[2]] %>%  html_nodes("a") %>% html_attr("href")
    vieraspelaajat <- vieraspelaajat[which(vieraspelaajat != "#")]
    
    vierasmv <-
      taulut %>% .[mvIND[2]] %>%  html_nodes("a") %>% html_attr("href")
    vierasmv <- vierasmv[which(vierasmv != "#")]
    
    koti_id <- kotipelaajat %>% 
      str_replace_all(c("/pelaajat/" = "")) %>% 
      str_split("/", simplify=T) %>% .[,1]
    
    vieras_id <- vieraspelaajat %>% 
      str_replace_all(c("/pelaajat/" = "")) %>% 
      str_split("/", simplify=T) %>% .[,1]
    
    
    kotimv_id <- kotimv %>% 
      str_replace_all(c("/pelaajat/" = "")) %>% 
      str_split("/", simplify=T) %>% .[,1]
    
    vierasmv_id <- vierasmv %>% 
      str_replace_all(c("/pelaajat/" = "")) %>% 
      str_split("/", simplify=T) %>% .[,1]
    
    taulut[which(taulut %>% html_attr("class") %in% c("goalie-stats", "player-stats"))] %>% 
      html_table(header = T) -> taulut
    
    kotip_nimet <- str_split(taulut[[1]]$Nimi, ",", simplify = T) %>% .[, 2:1] %>%
      apply(., 1, paste0, collapse = " ") %>%  str_trim()
    
    vierasp_nimet <- str_split(taulut[[3]]$Nimi, ",", simplify = T) %>% .[, 2:1] %>%
      apply(., 1, paste0, collapse = " ") %>%  str_trim()
    
    kotimv_nimet <- str_split(taulut[[2]]$Nimi, ",", simplify = T) %>% .[, 2:1] %>%
      apply(., 1, paste0, collapse = " ") %>% str_trim()
    
    vierasmv_nimet <- str_split(taulut[[4]]$Nimi, ",", simplify = T) %>% .[, 2:1] %>%
      apply(., 1, paste0, collapse = " ") %>% str_trim()
    
    bind_rows(
      taulut[[1]] %>%
        mutate(
          Nimi = kotip_nimet,
          Joukkue = koti,
          pvm = pvm,
          otteluID = ID,
          kausi = kausi,
          pelaajaID = koti_id
        ) ,
      taulut[[3]] %>%
        mutate(
          Nimi = vierasp_nimet,
          Joukkue = vieras,
          pvm = pvm,
          otteluID = ID,
          kausi = kausi,
          pelaajaID = vieras_id
        )
    ) -> pelaajatied
    
    bind_rows(
      taulut[[2]] %>%
        mutate(
          Nimi = kotimv_nimet,
          Joukkue = koti,
          pvm = pvm,
          otteluID = ID,
          kausi = kausi,
          pelaajaID = kotimv_id
        ) ,
      taulut[[4]] %>%
        mutate(
          Nimi = vierasmv_nimet,
          Joukkue = vieras,
          pvm = pvm,
          otteluID = ID,
          kausi = kausi,
          pelaajaID = vierasmv_id
        )
    ) -> mvtied
    
    
    peliajat <- sapply(strsplit(pelaajatied$Aika, ":"), function(x)  {
      x <- as.numeric(x)
      x[1] + x[2] / 60
    })
    
    peliajat_mv <- sapply(strsplit(mvtied$Aika, ":"), function(x)  {
      x <- as.numeric(x)
      x[1] + x[2] / 60
    })
    
    
    pelaajatied %<>% mutate(Aika = peliajat)
    
    mvtied %<>% mutate(Aika = peliajat_mv)
    
    if (i == 1) {
      pelaajadata <- pelaajatied
      maalivahtidata <- mvtied
    } else {
      pelaajadata <- bind_rows(pelaajadata, pelaajatied)
      maalivahtidata <- bind_rows(maalivahtidata, mvtied)
    }
    
    ID <- ID + 1
    info <-
      sprintf("... %d%% valmis, %d / %d", round((i / nrow(Sivut)) * 100), i, nrow(Sivut))
    setWinProgressBar(pb, i, label = info)
  }
  
  close(pb)
  
  return(
    list(
      pelaajadata = pelaajadata,
      maalivahtidata = maalivahtidata
    )
  )
}
