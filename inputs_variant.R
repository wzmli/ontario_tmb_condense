# ---------------------------
# Prepare variant data
#
# load and tidy data on variant counts and frequencies
# ---------------------------

library(tidyverse)

dd <- readRDS("metadata/covvarnet_voc.rds")

## This repo is for Ontario Only
## We will make a new repo later for other pts
major_prov = c("Ontario")

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

print(names(df))

vardat <- data.frame(
	variant = c("Alpha", "B.1.438.1"
		, "Beta", "Gamma"
		, "Delta", "Delta AY.25", "Delta AY.27"
		, "Omicron BA.1", "Omicron BA.1.1"
		, "Omicron BA.2"
	)
	, varname = c("Alpha", "Alpha"
		, "Alpha", "Alpha" ## Hack! Changing beta and gamma to alpha
		, "Delta", "Delta", "Delta"
		, "Omicron1", "Omicron1"
		, "Omicron2"
	)
)

invaderframe <- data.frame(
	varname = c("Alpha", "Delta", "Omicron1", "Omicron2")
	, start_date = as.Date(c("2020-12-07","2021-03-08","2021-11-22","2022-01-10"))
	, end_date = as.Date(c("2021-03-07","2021-11-21","2022-01-09","2022-04-04"))
)

variantLong <- (df
	%>% filter(province %in% major_prov)
	%>% pivot_longer(names_to = "variant", values_to="count",-c("dates","province"))
	%>% left_join(.,vardat)
	%>% mutate(varname = ifelse(is.na(varname),"other",varname)
		, dates = as.Date(dates)
	)
)
i
print(variantLong,n=Inf)

simple_dat <- (variantLong
	%>% group_by(dates,province,varname)
	%>% summarise(simple_count = sum(count,na.rm=TRUE))
	%>% group_by(dates,province)
	%>% mutate(simple_prop = simple_count/sum(simple_count,na.rm=TRUE))
	%>% arrange(province,dates,varname)
	%>% ungroup()
)

invaderdat <- (simple_dat
	%>% left_join(.,invaderframe)
	%>% filter(!is.na(start_date))
	%>% mutate(invader = between(dates,start_date,end_date))
	%>% filter(invader)
)

print(simple_dat,n=Inf)

gg <- (ggplot(simple_dat, aes(x=dates, y=simple_prop, color=varname, group = varname))
	+ geom_line()
	+ facet_wrap(~province)
	+ scale_x_date(date_labels = "%Y")
	+ theme(legend.position = "bottom")
)

print(gg)

print(gg %+% invaderdat)

print(invaderdat)

## Output
#MLi: How does the voc param timevar look?

## params_voc_timevar <- (invaderdat ...)

## Adding to environment when done
## parameters <- addEnvironment(parameters,c("model"))


