  # Cow-calf ratios from my photos
  # Anna Moeller
  # 8/3/2017

  # Load photos
  load("GitHub/CameraTrapStudy/2015 data/pics.wide20160804.RData")
  
  # What are the cow/calf ratios per group?
  
  # First, group "events"
  source("GitHub/CameraTrapStudy/Image Analysis/eventID_fn.R")
  
  # Make an elk dataframe that is smaller to work with
  elkdata <- select(pics, site, plot, cam, timeLST, dateLST,
                    grep("elk", names(pics), ignore.case = T)) %>%
    do(eventID_fn(data = ., species = "elk", cutoff = 30)) %>%
    group_by(eventID) %>%
    summarise(cows = sum(ElkAntlerless),
              calves = sum(ElkCalf),
              unkn = sum(ElkUnkn),
              male = sum(ElkSpike + ElkRaghorn + ElkMatBull + ElkNubsPeds)) %>%
    mutate(calfcow = calves/cows )
  mean(elkdata$calfcow, na.rm = T)
  range(elkdata$calfcow)
### What is the ratio when there are no cows? 
  
  rat <- elkdata <- select(pics, site, plot, cam, timeLST, dateLST,
                           grep("elk", names(pics), ignore.case = T)) %>%
    summarise(cows = sum(ElkAntlerless),
              calves = sum(ElkCalf),
              unkn = sum(ElkUnkn),
              male = sum(ElkSpike + ElkRaghorn + ElkMatBull + ElkNubsPeds))
  rat$calves/rat$cows

  