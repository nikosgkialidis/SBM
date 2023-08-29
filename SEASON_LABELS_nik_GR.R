season_labels = c(0)
season_numbers = c(0)
season_labels_full = c(0)
for(yy in 1:length(years)){
  year1<-years[yy]
  year2<-year1+1
  season<-year1
  season_lab = paste0(substring(year1,3,4), "/",substring(year2,3,4))
  season_lab_full = paste0(year1,"/",year2)
  season_numbers = append(season_numbers, season)
  season_labels = append(season_labels, season_lab)
  season_labels_full = append(season_labels_full, season_lab_full)
}

season_labels_full = season_labels_full[-1]
season_labels = season_labels[-1]
season_numbers = season_numbers[-1]

updated_season_labels_full = season_labels_full
updated_season_labels = season_labels
updated_season_numbers = season_numbers
