# COUNTRY SPECIFIC LIFE TABLES:

country_LE <- read.csv("data/lifetables_bycountry.csv")
names(country_LE)

# Compare life tables across countries
plot(NA, NA, xlim=c(0,120), ylim=c(0,100))
for(i in 1:length(unique(country_LE$country))){
  index = which(country_LE$country==unique(country_LE$country)[i] & country_LE$year ==2020)
  lines(country_LE$age_from[index], country_LE$value[index])
  print(c(min(country_LE$value[index][2]), as.character(country_LE$country[index][1])))
}

# Compare YLLs & U5 YLL for Cuba vs Sierra Leone!

# Note that names of countries are written out in full so need matching by ISO code!
unique(LE2020$country_code)

