library(tidyverse)

dd <- readRDS("metadata/covvarnet_voc.rds")

df <- data.frame()

for(i in names(dd)){
	dates <- rownames(dd[[i]])
	tempdf <- (dd[[i]]
		%>% mutate(province = i
			, dates = dates
			)
		%>% select(dates,province,everything()) 
	)
	rownames(tempdf) <- NULL

	df <- bind_rows(df,tempdf)
}

print(df)



