library(isoWater)

#get data
d = wiDB_data(projects = '00066')
d = d[[1]]

#write data
save(d, file = "data/NNData.rda")

#average data w replicate analyses
t = table(d$Sample_ID)
dd = d
for(i in 1:length(t)){
  if(t[i] > 1){
    #get averages
    id = names(t)[i]
    d2h = mean(d$d2H[d$Sample_ID == id])
    d2h.sd = mean(d$d2H_Analytical_SD[d$Sample_ID == id])
    d18o = mean(d$d18O[d$Sample_ID == id])
    d18o.sd = mean(d$d18O_Analytical_SD[d$Sample_ID == id])
    
    #grab the sample info and update w/ averages
    h = d[match(id, d$Sample_ID),]
    h$d2H = d2h
    h$d2H_Analytical_SD = d2h.sd
    h$d18O = d18o
    h$d18O_Analytical_SD = d18o.sd
    
    #drop replicates and add back updated row
    dd = dd[dd$Sample_ID != id,]
    dd = rbind(dd, h)
  }
}

#summarize sample numbers
table(dd$Type)

#format dates
dd$Start_Date = as.POSIXct(dd$Start_Date, format = "%Y-%m-%d %H:%M:%S")
dd$Collection_Date = as.POSIXct(dd$Collection_Date, format = 
                                  "%Y-%m-%d %H:%M:%S")

#split data
p = dd[dd$Type == "Precipitation",]
g = dd[dd$Type == "Ground",]
l = dd[dd$Type == "Lake",]
r = dd[dd$Type == "River_or_stream",]
s = dd[dd$Type == "Spring",]

####
##Pull out volumes, which are at beginning of sample comments
v = p$Sample_Comments
#first split by spaces
v = strsplit(v, " ")
vv = character(length(v))
for(i in 1:length(v)){
  vv[i] = v[[i]][1]
}
#then split by "g" to get most ones w/o space
v = vv
v = strsplit(v, "g")
for(i in 1:length(v)){
  vv[i] = v[[i]][1]
}
#then split by "S" 
v = vv
v = strsplit(v, "S")
for(i in 1:length(v)){
  vv[i] = v[[i]][1]
}
#then split by "m" 
v = vv
v = strsplit(v, "m")
for(i in 1:length(v)){
  vv[i] = v[[i]][1]
}
#finally split by "," 
v = vv
v = strsplit(v, ",")
for(i in 1:length(v)){
  vv[i] = v[[i]][1]
}
v = vv
v = as.numeric(v)

p$Volume = v

# Oil test -----

#extract no-oil 2016 samples
p.16 = p[format.Date(p$Collection_Date, "%Y") == "2016",]
p.no = p.16[grep("oil", p.16$Sample_Comments),]
#get equivalent oil samples
p.16 = p.16[p.16$Site_ID %in% p.no$Site_ID,]
p.16 = p.16[-grep("oil", p.16$Sample_Comments),]
p.o = p.no[1,]
for(i in unique(p.no$Site_ID)){
  dates = format.Date(p.no$Collection_Date[p.no$Site_ID == i], "%Y-%m-%d")
  p.ad = p.16[p.16$Site_ID == i,]
  p.ad = p.ad[format.Date(p.ad$Collection_Date, "%Y-%m-%d") %in% dates,]
  p.o = rbind(p.o, p.ad)
}
#Clean by removing starter row and one event sample
p.o = p.o[-1,]
p.o = p.o[p.o$Sample_ID != "NN-1117",]

#Remove the oil experiment samples from main dataset
p = p[!(p$Sample_ID %in% p.no$Sample_ID),]

#Split out event data sites
p.fd = p[p$Site_ID == "NN051",]
p.rs = p[p$Site_ID == 'NN052',]

#this analysis requires manual examination and judement calls,
#which were done in Excel sheet 201020_eventVsMonthly.xlsx
#Read in results
library(openxlsx)
p.ev = read.xlsx("data/201020_eventVsMonthly.xlsx", sheet = 2)

plot(p.ev$d18O.e, p.ev$d18O.m, xlim = c(-12.5, -2), ylim = c(-12, 3),
     pch = p.ev$Symbol, bg = p.ev$Color, col = p.ev$Line)

abline(0, 1)

plot(p.ev$Vol.m, p.ev$d18O.m - p.ev$d18O.e, pch = p.ev$Symbol, 
     bg = p.ev$Color, col = p.ev$Line)
abline(0, 0)

t = t.test(p.ev$d18O.m, p.ev$d18O.e, paired = TRUE)
t

t = t.test(p.ev$d18O.m[-12], p.ev$d18O.e[-12], paired = TRUE)
t

t = t.test(p.ev$d18O.m[p.ev$Qual == "g"], p.ev$d18O.e[p.ev$Qual == "g"], 
           paired = TRUE)
t

t = t.test(p.ev$d18O.m[p.ev$Year > 2014], p.ev$d18O.e[p.ev$Year > 2014], 
           paired = TRUE)
t

# Assign months -----

#get month of midpoint of each collection interval
#first fill in missing start dates, assuming these are all monthly samps
md = p$Collection_Date
for(i in 1:nrow(p)){
  if(is.na(p$Start_Date[i])){
    p$Start_Date[i] = p$Collection_Date[i] - (30 * 24 * 3600)
  }
  md[i] = mean(c(p$Start_Date[i], p$Collection_Date[i]))
}
#extract month as numeric
cm = as.numeric(format.Date(md, "%m"))
#allocate to seasons
m = character(length(cm))
for(i in 1: length(cm)){
  m[i] = switch(cm[i], "post", "post", "post", "pre", "pre", "pre", "m", "m",
             "m", "post", "post", "post")
}

#get year
y = as.numeric(format.Date(md, "%Y"))

#append
p$m.n = cm
p$m = m
p$y = y

#split
p.m = p[p$m == "m",]
p.pre = p[p$m == "pre",]
p.post = p[p$m == "post",]

# Evaporation -----
pec.m = pec.pre = pec.post = gec = lec = rec = sec =
  data.frame("source_d2H" = numeric(), "source_d18O" = numeric(), 
             "S" = numeric(), "E" = numeric())
p.m$d2H.ec = p.m$d18O.ec = rep(NA)
p.pre$d2H.ec = p.pre$d18O.ec = rep(NA)
p.post$d2H.ec = p.post$d18O.ec = rep(NA)
g$d2H.ec = g$d18O.ec = rep(NA)
l$d2H.ec = l$d18O.ec = rep(NA)
r$d2H.ec = r$d18O.ec = rep(NA)
s$d2H.ec = s$d18O.ec = rep(NA)

ec.data = list(
  p.pre = list(p.pre = p.pre, pec.pre = pec.pre),
  p.m = list(p.m = p.m, pec.m = pec.m),
  p.post = list(p.post = p.post, pec.post = pec.post),
  g = list(g = g, gec = gec),
  l = list(l = l, lec = lec),
  r = list(r = r, rec = rec),
  s = list(s = s, sec = sec)
)

library(doParallel)
registerDoParallel()

ec.data = foreach(i = 1:length(ec.data)) %dopar% {
  for(j in 1:nrow(ec.data[[i]][[1]])){
    obs = isoWater::iso(ec.data[[i]][[1]]$d2H[j], 
              ec.data[[i]][[1]]$d18O[j], 0.2, 0.1, 0)
    ec = isoWater::mwlSource(obs, slope = c(5, 0.3), ngens = 25e4)
    ec.data[[i]][[2]] = rbind(ec.data[[i]][[2]], ec$results)
    ec.data[[i]][[1]]$d2H.ec[j] = median(ec$results$source_d2H)
    ec.data[[i]][[1]]$d18O.ec[j] = median(ec$results$source_d18O)
  }
  ec.data[[i]]
}

names(ec.data) = c("p.pre", "p.m", "p.post", "g", "l", "r", "s")

stopImplicitCluster()

save(ec.data, file = "out/ec.data.rda")
save(p.pre, p.m, p.post, l, g, s, r, file = "out/waters.rda")

# Load for stats -----
library(MASS)

load("out/ec.data.rda")
load("out/waters.rda")

p.pre = ec.data$p.pre$p.pre
pec.pre = ec.data$p.pre$pec.pre
p.m = ec.data$p.m$p.m
pec.m = ec.data$p.m$pec.m
p.post = ec.data$p.post$p.post
pec.post = ec.data$p.post$pec.post
g = ec.data$g$g
gec = ec.data$g$gec
l = ec.data$l$l
lec = ec.data$l$lec
r = ec.data$r$r
rec = ec.data$r$rec
s = ec.data$s$s
sec = ec.data$s$sec

#evaporation index
for(i in list(pec.pre, pec.m, pec.post, gec, lec, rec, sec)){
  print(paste(mean(i$E), median(i$E)))
}

p.pre.kdf = kde2d(pec.pre$source_d18O, pec.pre$source_d2H, n = 50, 
                lims = c(-20, -0, -150, -10))
p.m.kdf = kde2d(pec.m$source_d18O, pec.m$source_d2H, n = 50, 
                lims = c(-20, -0, -150, -10))
p.post.kdf = kde2d(pec.post$source_d18O, pec.post$source_d2H, n = 50, 
                lims = c(-20, -0, -150, -10))
m.post.kdf = kde2d(c(gec$source_d18O, lec$source_d18O, 
                     rec$source_d18O, sec$source_d18O),
                   c(gec$source_d2H, lec$source_d2H, 
                     rec$source_d2H, sec$source_d2H),
                   n = 50, lims = c(-20, -0, -150, -10))

#some stats
mean(p.m$d18O)
mean(p.m$d2H)
mean(p.post$d18O)
mean(p.post$d2H)
mean(p.pre$d18O)
mean(p.pre$d2H)
sd(p.m$d18O)
sd(p.m$d2H)
sd(p.post$d18O)
sd(p.post$d2H)
sd(p.pre$d18O)
sd(p.pre$d2H)

mean(pec.m$source_d2H)
mean(pec.m$source_d18O)
mean(pec.post$source_d2H)
mean(pec.post$source_d18O)
mean(pec.pre$source_d2H)
mean(pec.pre$source_d18O)
mean(pec.m$E)
mean(pec.pre$E)
mean(pec.post$E)

lakeMod = lm(l$d2H ~ l$d18O)
summary(lakeMod)

#parse out years
p.yr = data.frame("Year" = numeric(), "Period" = character(),
                  "d2H.m" = numeric(), "d18O.m" = numeric(),
                  "d2H.ec.m" = numeric(), "d18O.ec.m" = numeric(),
                  "d2H.sd" = numeric(), "d18O.sd" = numeric(),
                  "d2H.ec.sd" = numeric(), "d18O.ec.sd" = numeric())

for(i in 1:4){
  p.pre.yr = p.pre[p.pre$y == 2013 + i,]
  p.m.yr = p.m[p.m$y == 2013 + i,]
  p.post.yr = p.post[p.post$y == 2013 + i,]
  
  p.yr = rbind(p.yr, data.frame("Year" = 2013 + i, "Period" = "pre",
                               "d2H.m" = mean(p.pre.yr$d2H),
                               "d18O.m" = mean(p.pre.yr$d18O),
                               "d2H.ec.m" = mean(p.pre.yr$d2H.ec),
                               "d18O.ec.m" = mean(p.pre.yr$d18O.ec),
                               "d2H.sd" = sd(p.pre.yr$d2H),
                               "d18O.sd" = sd(p.pre.yr$d18O),
                               "d2H.ec.sd" = sd(p.pre.yr$d2H.ec),
                               "d18O.ec.sd" = sd(p.pre.yr$d18O.ec)))
  p.yr = rbind(p.yr, data.frame("Year" = 2013 + i, "Period" = "m",
                                "d2H.m" = mean(p.m.yr$d2H),
                                "d18O.m" = mean(p.m.yr$d18O),
                                "d2H.ec.m" = mean(p.m.yr$d2H.ec),
                                "d18O.ec.m" = mean(p.m.yr$d18O.ec),
                                "d2H.sd" = sd(p.m.yr$d2H),
                                "d18O.sd" = sd(p.m.yr$d18O),
                                "d2H.ec.sd" = sd(p.m.yr$d2H.ec),
                                "d18O.ec.sd" = sd(p.m.yr$d18O.ec)))
  p.yr = rbind(p.yr, data.frame("Year" = 2013 + i, "Period" = "post",
                                "d2H.m" = mean(p.post.yr$d2H),
                                "d18O.m" = mean(p.post.yr$d18O),
                                "d2H.ec.m" = mean(p.post.yr$d2H.ec),
                                "d18O.ec.m" = mean(p.post.yr$d18O.ec),
                                "d2H.sd" = sd(p.post.yr$d2H),
                                "d18O.sd" = sd(p.post.yr$d18O),
                                "d2H.ec.sd" = sd(p.post.yr$d2H.ec),
                                "d18O.ec.sd" = sd(p.post.yr$d18O.ec)))
}

pcp.tab = read.csv("data/precipAmt.csv")
pcp.pre = pcp.tab[pcp.tab$Month %in% c(4, 5, 6),]
pcp.m = pcp.tab[pcp.tab$Month %in% c(7, 8, 9),]
pcp.post = pcp.tab[pcp.tab$Month %in% c(10, 11, 12, 1, 2, 3),]

pcp.seas = data.frame("Year" = numeric(), "Amt.pre" = numeric(),
                      "Amt.m" = numeric(), "Amt.post" = numeric(),
                      "SD.pre" = numeric(), "SD.m" = numeric(),
                      "SD.post" = numeric())
for(i in 1:4){
  y = 2013 + i
  pcp.add = data.frame("Year" = y, 
                       "Amt.pre" = mean(pcp.pre$Amount[pcp.pre$Year == y], na.rm = TRUE),
                       "Amt.m" = mean(pcp.m$Amount[pcp.m$Year == y], na.rm = TRUE),
                       "Amt.post" = mean(pcp.post$Amount[pcp.post$Year == y], na.rm = TRUE),
                       "SD.pre" = sd(pcp.pre$Amount[pcp.pre$Year == y], na.rm = TRUE),
                       "SD.m" = sd(pcp.m$Amount[pcp.m$Year == y], na.rm = TRUE),
                       "SD.post" = sd(pcp.post$Amount[pcp.post$Year == y], na.rm = TRUE))
  pcp.seas = rbind(pcp.seas, pcp.add)
}

#recreate p including all data
p = rbind(p.pre, p.m, p.post)

# Amount effect -----

#depth of precip
p$Amount = p$Volume / (3.14159 * 5.1 ^ 2) * 10
p.pre$Amount = p.pre$Volume / (3.14159 * 5.1 ^ 2) * 10
p.m$Amount = p.m$Volume / (3.14159 * 5.1 ^ 2) * 10
p.post$Amount = p.post$Volume / (3.14159 * 5.1 ^ 2) * 10

#Evaluate relationship with precipitation amount
amt.eff = data.frame("Period" = c("all", "pre", "m", "post"), 
                     "d2H.beta" = numeric(4),
                     "d2H.rsq" = numeric(4), "d18O.beta" = numeric(4),
                     "d18O.rsq" = numeric(4), "d18O.e.beta" = numeric(4),
                     "d18O.e.rsq" = numeric(4))

pdat = list(p, p.pre, p.m, p.post)

for(i in 1:4){
  d2H.mod = lm(d2H ~ Amount, pdat[[i]])
  d18O.mod = lm(d18O ~ Amount, pdat[[i]])
  d18O.e.mod = lm((d18O - d18O.ec) ~ Amount, pdat[[i]])
  
  amt.eff$d2H.beta[i] = round(d2H.mod$coefficients[2], 3)
  amt.eff$d2H.rsq[i] = round(summary(d2H.mod)$adj.r.squared, 3)
  amt.eff$d18O.beta[i] = round(d18O.mod$coefficients[2], 3)
  amt.eff$d18O.rsq[i] = round(summary(d18O.mod)$adj.r.squared, 3)
  amt.eff$d18O.e.beta[i] = round(d18O.e.mod$coefficients[2], 3)
  amt.eff$d18O.e.rsq[i] = round(summary(d18O.e.mod)$adj.r.squared, 3)
  
  print(summary(d2H.mod))
  print(summary(d18O.mod))
  print(summary(d18O.e.mod))
}

View(amt.eff)

# Clusters -----

###load cluster groups from 2018 paper
clust = read.csv("data/clusters.csv")
sites = unique(p$Site_Name)
sites[!(sites %in% clust$sites)]
p = merge(p, clust, by.x = "Site_Name", by.y = "sites")

tser = data.frame("cl" = numeric(), "mt" = numeric(), 
                  "h" = numeric(), "o" = numeric(), 
                  "h.ec" = numeric(), 
                  "o.ec" = numeric(), 
                  "h.sd" = numeric(), "o.sd" = numeric(), 
                  "h.ec.sd" = numeric(), "o.ec.sd" = numeric())

for(cl in 1:5){
  for(mt in 4:10){
    psub = p[p$group == cl & p$m.n == mt, ] 
    h = mean(psub$d2H)
    o = mean(psub$d18O)
    h.ec = mean(psub$d2H.ec)
    o.ec = mean(psub$d18O.ec)
    h.sd = sd(psub$d2H)
    o.sd = sd(psub$d18O)
    h.ec.sd = sd(psub$d2H.ec)
    o.ec.sd = sd(psub$d18O.ec)
    tser = rbind(tser, data.frame(cl, mt, h, o, h.ec,
                                  o.ec, h.sd, o.sd,
                                  h.ec.sd, o.ec.sd))
  }
}

# Elevation effect -----
#Evaluate relationship with elevation
elev.eff = data.frame("Period" = c("all", "pre", "m", "post"), 
                     "d2H.beta" = numeric(4),
                     "d2H.rsq" = numeric(4), "d18O.beta" = numeric(4),
                     "d18O.rsq" = numeric(4), "d18O.e.beta" = numeric(4),
                     "d18O.e.rsq" = numeric(4))

for(i in 1:4){
  d2H.mod = lm(d2H ~ Elevation, pdat[[i]])
  d18O.mod = lm(d18O ~ Elevation, pdat[[i]])
  d18O.e.mod = lm((d18O - d18O.ec) ~ Elevation, pdat[[i]])
  
  elev.eff$d2H.beta[i] = round(d2H.mod$coefficients[2] * 1000, 1)
  elev.eff$d2H.rsq[i] = round(summary(d2H.mod)$adj.r.squared, 3)
  elev.eff$d18O.beta[i] = round(d18O.mod$coefficients[2] * 1000, 1)
  elev.eff$d18O.rsq[i] = round(summary(d18O.mod)$adj.r.squared, 3)
  elev.eff$d18O.e.beta[i] = round(d18O.e.mod$coefficients[2] * 1000, 1)
  elev.eff$d18O.e.rsq[i] = round(summary(d18O.e.mod)$adj.r.squared, 3)
  
  print(summary(d2H.mod))
  print(summary(d18O.mod))
  print(summary(d18O.e.mod))
}

View(elev.eff)

# Mixing -----

#sources
sites = unique(p.m$Site_ID)
smeans.m = data.frame("Site_ID" = character(), "d2H.m" = numeric(),
                    "d18O.m" = numeric(), "d2H.se" = numeric(),
                    "d18O.se" = numeric(), "cov" = numeric())
for(i in 1:length(sites)){
  psub = p.m[p.m$Site_ID == sites[i],]
  source.h = mean(psub$d2H) 
  source.o = mean(psub$d18O)
  source.h.se = sd(psub$d2H) / sqrt(sum(!is.na(psub$d2H)))
  source.o.se = sd(psub$d18O) / sqrt(sum(!is.na(psub$d18O)))
  source.cov = sqrt(cov(psub$d2H, psub$d18O)) / sqrt(nrow(psub))
  smeans.m = rbind(smeans.m, data.frame("Site_ID" = sites[i], 
                        "d2H.m" = source.h,
                        "d18O.m" = source.o, 
                        "d2H.se" = source.h.se,
                        "d18O.se" = source.o.se,
                        "cov" = source.cov))
}

library(mvtnorm)

HO.m = matrix(nrow = 1e5, ncol = 2)
for(i in 1:1e5){
  j = sample.int(length(sites), 3)
  HO1 = rmvnorm(1, c(smeans.m[j[1], 2],
                     smeans.m[j[1], 3]), 
                matrix(c(smeans.m[j[1], 4]^2,
                         smeans.m[j[1], 6]^2,
                         smeans.m[j[1], 6]^2,
                         smeans.m[j[1], 5]^2), nrow = 2))
  HO2 = rmvnorm(1, c(smeans.m[j[2], 2],
                     smeans.m[j[2], 3]), 
                matrix(c(smeans.m[j[2], 4]^2,
                         smeans.m[j[2], 6]^2,
                         smeans.m[j[2], 6]^2,
                         smeans.m[j[2], 5]^2), nrow = 2))
  HO3 = rmvnorm(1, c(smeans.m[j[3], 2],
                     smeans.m[j[3], 3]), 
                matrix(c(smeans.m[j[3], 4]^2,
                         smeans.m[j[3], 6]^2,
                         smeans.m[j[3], 6]^2,
                         smeans.m[j[3], 5]^2), nrow = 2))
  HO.m[i,] = c(mean(HO1[1], HO2[1], HO3[1]), 
               mean(HO1[2], HO2[2], HO3[2]))
}

H = mean(HO.m[,1])
O = mean(HO.m[,2])
H.sd = sd(HO.m[,1])
O.sd = sd(HO.m[,2])
HO.cov = cov(HO.m[,1], HO.m[,2])

p.w = p[p$m.n %in% c(11, 12, 1, 2, 3),]
for(i in 1:1e5){
  j = sample.int(nrow(p.w), 3)
  HO.m[i,] = c(mean(p.w$d2H[j]), mean(p.w$d18O[j]))
}
H = c(H, mean(HO.m[,1]))
O = c(O, mean(HO.m[,2]))
H.sd = c(H.sd, sd(HO.m[,1]))
O.sd = c(O.sd, sd(HO.m[,2]))
HO.cov = c(HO.cov, cov(HO.m[,1], HO.m[,2]))

hsource = iso(H, O, H.sd, O.sd, HO.cov)

#groundwater
library(doParallel)
registerDoParallel()

sites = unique(g$Site_ID)

g.mix = foreach(i = 1:length(sites), .combine = rbind) %dopar% {
  g.sub = g[g$Site_ID == sites[i],]
  obs = iso(g.sub$d2H, g.sub$d18O, 0.4, 0.1, 0)

  post = mixSource(obs, hsource, c(5, 0.3), shp = 1,
                 ngens = 2e5)
  
  post$results
}

#rivers
sites = unique(r$Site_ID)

r.mix = foreach(i = 1:length(sites), .combine = rbind) %dopar% {
  g.sub = r[r$Site_ID == sites[i],]
  obs = iso(g.sub$d2H, g.sub$d18O, 0.4, 0.1, 0)
  
  post = mixSource(obs, hsource, c(5, 0.3), shp = 1, 
                 ngens = 2e5)
  
  post$results
}

#lakes
sites = unique(l$Site_ID)

l.mix = foreach(i = 1:length(sites), .combine = rbind) %dopar% {
  g.sub = l[l$Site_ID == sites[i],]
  obs = iso(g.sub$d2H, g.sub$d18O, 0.4, 0.1, 0)
  post = mixSource(obs, hsource, c(5, 0.3), shp = 1,
                 ngens = 2e5)
  
  post$results
}

#springs
sites = unique(s$Site_ID)

s.mix = foreach(i = 1:length(sites), .combine = rbind) %dopar% {
  g.sub = s[s$Site_ID == sites[i],]
  obs = iso(g.sub$d2H, g.sub$d18O, 0.4, 0.1, 0)
  
  post = mixSource(obs, hsource, c(5, 0.3), shp = 1,
                 ngens = 2e5)
  
  post$results
}
stopImplicitCluster()

save(g.mix, s.mix, r.mix, l.mix, file = "out/mix.rda")

# Tucson -----
tmp = read.csv("data/tucson.csv")
plot(tmp$Month, tmp$d18O)

tuc = data.frame("Month" = seq(1:12), "d18O" = numeric(12),
                 "d2H" = numeric(12))

for(i in 1:12){
  d18O = tmp$d18O[tmp$Month == tuc$Month[i]]
  d2H = tmp$d2H[tmp$Month == tuc$Month[i]]
  narm = !(is.na(d18O) | is.na(d2H))
  d18O = d18O[narm]
  d2H = d2H[narm]
  
  tuc$d18O[i] = mean(d18O)
  tuc$d2H[i] = mean(d2H)
  tuc$d18O.sd[i] = sd(d18O)
  tuc$d2H.sd[i] = sd(d2H)
  tuc$HOc[i] = cov(d18O, d2H)
}

for(i in 4:10){
  obs = iso(tuc$d2H[i], tuc$d18O[i], tuc$d2H.sd[i],
            tuc$d18O.sd[i], tuc$HOc[i])
  post = mwlSource(obs, slope = c(4.8, 0.3), ngens = 25e4)
  tuc$d18O.ec[i] = median(post$results$source_d18O)
  tuc$E[i] = median(post$results$E)
}

tuc = tuc[4:10,]

plot(tuc$Month, tuc$d18O)
plot(tuc$Month, tuc$d18O.ec)
plot(tuc$Month, tuc$E)
# Figures -----

#Figure 2 - xplots of all data
#colors
cols = c("white", "green", "red", "light blue")

png("out/Fig2.png", units = "in", width = 7.3, height = 7.3, res = 600)
layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE),
       widths = c(lcm(4 * 2.54), lcm(3.3 * 2.54)), 
       heights = c(lcm(3.3 * 2.54), lcm(4 * 2.54)))

par(mai = c(0.3, 1, 0.1, 0.1))
plot(p$d18O, p$d2H, type = "n", xlab = "", 
     ylab = expression(delta^{"2"}*"H (VSMOW)"), axes = FALSE)
axis(1, labels = FALSE)
axis(2)
box()
abline(10, 8)
abline(lm(p.pre$d2H ~ p.pre$d18O), lty = 3)
points(p.pre$d18O, p.pre$d2H, pch = p.pre$y - 1993, 
       bg = cols[p.pre$y - 2013])
text(-18, 48, "(a)")

par(mai = c(0.3, 0.3, 0.1, 0.1))
plot(p$d18O, p$d2H, type = "n", xlab = "", ylab = "", axes = FALSE)
axis(1, labels = FALSE)
axis(2, labels = FALSE)
box()
abline(10, 8)
abline(lm(p.m$d2H ~ p.m$d18O), lty = 3)
points(p.m$d18O, p.m$d2H, pch = p.m$y - 1993, 
       bg = cols[p.m$y - 2013])
text(-18, 48, "(b)")
legend(5, -80, c("2014", "2015", "2016", "2017"),
       pch = c(21, 22, 23, 24), pt.bg = cols, bty = "n")

par(mai = c(1, 1, 0.1, 0.1))
plot(p$d18O, p$d2H, type = "n", 
     xlab = expression(delta^{"18"}*"O (VSMOW)"),
     ylab = expression(delta^{"2"}*"H (VSMOW)"))
abline(10, 8)
abline(lm(p.post$d2H ~ p.post$d18O), lty = 3)
points(p.post$d18O, p.post$d2H, pch = p.post$y - 1993, 
       bg = cols[p.post$y - 2013])
text(-18, 48, "(c)")

par(mai = c(1, 0.3, 0.1, 0.1))
plot(p$d18O, p$d2H, type = "n", ylab = "", 
     xlab = expression(delta^{"18"}*"O (VSMOW)"), axes = FALSE)
axis(1)
axis(2, labels = FALSE)
box()
abline(10, 8)
points(g$d18O, g$d2H, col = "brown")
points(l$d18O, l$d2H, col = "light blue")
points(r$d18O, r$d2H, col = "blue")
points(s$d18O, s$d2H, col = "grey")
text(-18, 48, "(d)")
legend(5, -80, c("Ground", "Spring", "Stream", "Lake"),
       col = c("brown", "grey", "blue", "light blue"),
       bty = "n", pch = 1)

dev.off()

### Figure 3

png("out/Fig3.png", units = "in", width = 7.3, height = 7.3, res = 600)
layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE),
       widths = c(lcm(4 * 2.54), lcm(3.3 * 2.54)), 
       heights = c(lcm(3.3 * 2.54), lcm(4 * 2.54)))

par(mai = c(0.3, 1, 0.1, 0.1))
image(p.pre.kdf, axes = FALSE, xlab = "",
      ylab = expression(delta^{"2"}*"H (VSMOW)"))
axis(1, labels = FALSE)
axis(2)
abline(10, 8)
contour(p.pre.kdf, labels = "", levels = seq(0.002, 0.008, by = 0.002), 
        add = TRUE)
box()
text(-19, -18, "(a)")

par(mai = c(0.3, 0.3, 0.1, 0.1))
image(p.m.kdf, axes = FALSE)
axis(1, labels = FALSE)
axis(2, labels = FALSE)
abline(10, 8)
contour(p.m.kdf, labels = "", levels = seq(0.002, 0.008, by = 0.002), 
        add = TRUE)
box()
text(-19, -18, "(b)")

par(mai = c(1, 1, 0.1, 0.1))
image(p.post.kdf, axes = FALSE,
      xlab = expression(delta^{"18"}*"O (VSMOW)"),
      ylab = expression(delta^{"2"}*"H (VSMOW)"))
axis(1)
axis(2)
abline(10, 8)
contour(p.post.kdf, labels = "", levels = seq(0.002, 0.008, by = 0.002), 
        add = TRUE)
box()
text(-19, -18, "(c)")

par(mai = c(1, 0.3, 0.1, 0.1))
image(m.post.kdf, axes = FALSE,
      xlab = expression(delta^{"18"}*"O (VSMOW)"), ylab = "")
axis(1)
axis(2, labels = FALSE)
abline(10, 8)
contour(m.post.kdf, labels = "", levels = seq(0.006, 0.03, by = 0.006), 
        add = TRUE)
box()
text(-19, -18, "(d)")

dev.off()

###
png("out/Fig4.png", width = 4, height = 7.2, units = "in", res = 600)

layout(matrix(c(1, 2, 3), ncol = 1), 
       heights = c(lcm(2.3 * 2.54), lcm(2.3 * 2.54), lcm(2.6 * 2.54)))

#panel A
par(mai = c(0.3, 0.6, 0.1, 0.1))
plot(pcp.seas$Year, pcp.seas$Amt.pre, type = "l", 
     ylim = c(0, 70), xlab = "Year", ylab = "Precipitation (mm)",
     axes = FALSE)
axis(1, labels = FALSE, xaxp = c(2014, 2017, 3))
axis(2)
box()

polygon(c(pcp.seas$Year, rev(pcp.seas$Year)),
        c(pcp.seas$Amt.pre + pcp.seas$SD.pre, 
          rev(pcp.seas$Amt.pre - pcp.seas$SD.pre)),
        col = rgb(0.7, 0, 0, 0.3), lty = 0)
lines(pcp.seas$Year, pcp.seas$Amt.pre + pcp.seas$SD.pre,
      col = rgb(0.7, 0, 0), lty = 3)
lines(pcp.seas$Year, pcp.seas$Amt.pre - pcp.seas$SD.pre,
      col = rgb(0.7, 0, 0), lty = 3)

polygon(c(pcp.seas$Year, rev(pcp.seas$Year)),
        c(pcp.seas$Amt.m + pcp.seas$SD.m, 
          rev(pcp.seas$Amt.m - pcp.seas$SD.m)),
        col = rgb(0, 0, 0.9, 0.3), lty = 0)
lines(pcp.seas$Year, pcp.seas$Amt.m + pcp.seas$SD.m,
      col = rgb(0, 0, 0.9), lty = 3)
lines(pcp.seas$Year, pcp.seas$Amt.m - pcp.seas$SD.m,
      col = rgb(0, 0, 0.9), lty = 3)

polygon(c(pcp.seas$Year, rev(pcp.seas$Year)),
        c(pcp.seas$Amt.post + pcp.seas$SD.post, 
          rev(pcp.seas$Amt.post - pcp.seas$SD.post)),
        col = rgb(0, 0.5, 0, 0.3), lty = 0)
lines(pcp.seas$Year, pcp.seas$Amt.post + pcp.seas$SD.post,
      col = rgb(0, 0.5, 0), lty = 3)
lines(pcp.seas$Year, pcp.seas$Amt.post - pcp.seas$SD.post,
      col = rgb(0, 0.5, 0), lty = 3)

lines(pcp.seas$Year, pcp.seas$Amt.pre, col = rgb(0.7, 0, 0),
      lwd = 2)
lines(pcp.seas$Year, pcp.seas$Amt.m, col = rgb(0, 0, 0.9),
      lwd = 2)
lines(pcp.seas$Year, pcp.seas$Amt.post, col = rgb(0, 0.5, 0),
      lwd = 2)
text(2014, 68, "(a)")

#panel B
plot(p.yr$Year[p.yr$Period == "pre"], p.yr$d18O.m[p.yr$Period == "pre"],
     type = "l", col = "brown", ylim = c(-14, 1), xlab = "Year",
     xlim = c(2014, 2017), ylab = expression(delta^{18}*"O (VSMOW)"),
     axes = FALSE)
axis(1, labels = FALSE, xaxp = c(2014, 2017, 3))
axis(2)
box()

polygon(c(p.yr$Year[p.yr$Period == "pre"], 
          rev(p.yr$Year[p.yr$Period == "pre"])),
        c(p.yr$d18O.m[p.yr$Period == "pre"] + 
            p.yr$d18O.sd[p.yr$Period == "pre"], 
          rev(p.yr$d18O.m[p.yr$Period == "pre"] - 
                p.yr$d18O.sd[p.yr$Period == "pre"])),
        col = rgb(0.7, 0, 0, 0.3), lty = 0)
lines(p.yr$Year[p.yr$Period == "pre"], 
      p.yr$d18O.m[p.yr$Period == "pre"] + 
        p.yr$d18O.sd[p.yr$Period == "pre"],
      col = rgb(0.7, 0, 0), lty = 3)
lines(p.yr$Year[p.yr$Period == "pre"], 
      p.yr$d18O.m[p.yr$Period == "pre"] - 
        p.yr$d18O.sd[p.yr$Period == "pre"],
      col = rgb(0.7, 0, 0), lty = 3)

polygon(c(p.yr$Year[p.yr$Period == "m"], 
          rev(p.yr$Year[p.yr$Period == "m"])),
        c(p.yr$d18O.m[p.yr$Period == "m"] + 
            p.yr$d18O.sd[p.yr$Period == "m"], 
          rev(p.yr$d18O.m[p.yr$Period == "m"] - 
                p.yr$d18O.sd[p.yr$Period == "m"])),
        col = rgb(0, 0, 0.9, 0.3), lty = 0)
lines(p.yr$Year[p.yr$Period == "m"], 
      p.yr$d18O.m[p.yr$Period == "m"] + 
        p.yr$d18O.sd[p.yr$Period == "m"],
      col = rgb(0, 0, 0.9), lty = 3)
lines(p.yr$Year[p.yr$Period == "m"], 
      p.yr$d18O.m[p.yr$Period == "m"] - 
        p.yr$d18O.sd[p.yr$Period == "m"],
      col = rgb(0, 0, 0.9), lty = 3)

polygon(c(p.yr$Year[p.yr$Period == "post"], 
          rev(p.yr$Year[p.yr$Period == "post"])),
        c(p.yr$d18O.m[p.yr$Period == "post"] + 
            p.yr$d18O.sd[p.yr$Period == "post"], 
          rev(p.yr$d18O.m[p.yr$Period == "post"] - 
                p.yr$d18O.sd[p.yr$Period == "post"])),
        col = rgb(0, 0.5, 0, 0.3), lty = 0)
lines(p.yr$Year[p.yr$Period == "post"], 
      p.yr$d18O.m[p.yr$Period == "post"] + 
        p.yr$d18O.sd[p.yr$Period == "post"],
      col = rgb(0, 0.5, 0), lty = 3)
lines(p.yr$Year[p.yr$Period == "post"], 
      p.yr$d18O.m[p.yr$Period == "post"] - 
        p.yr$d18O.sd[p.yr$Period == "post"],
      col = rgb(0, 0.5, 0), lty = 3)

lines(p.yr$Year[p.yr$Period == "pre"], 
      p.yr$d18O.m[p.yr$Period == "pre"], 
      col = rgb(0.7, 0, 0),
      lwd = 2)
lines(p.yr$Year[p.yr$Period == "m"], 
      p.yr$d18O.m[p.yr$Period == "m"], 
      col = rgb(0, 0, 0.9),
      lwd = 2)
lines(p.yr$Year[p.yr$Period == "post"], 
      p.yr$d18O.m[p.yr$Period == "post"], 
      col = rgb(0, 0.5, 0),
      lwd = 2)
text(2014, 0.5, "(b)")

#panel C
par(mai = c(0.6, 0.6, 0.1, 0.1))
plot(p.yr$Year[p.yr$Period == "pre"], p.yr$d18O.ec.m[p.yr$Period == "pre"],
     type = "l", col = "brown", ylim = c(-16, -4), xlab = "Year",
     xlim = c(2014, 2017), ylab = expression("E-corrected "*delta^{18}*"O (VSMOW)"),
     axes = FALSE)
axis(1, xaxp = c(2014, 2017, 3))
axis(2)
box()

polygon(c(p.yr$Year[p.yr$Period == "pre"], 
          rev(p.yr$Year[p.yr$Period == "pre"])),
        c(p.yr$d18O.ec.m[p.yr$Period == "pre"] + 
            p.yr$d18O.ec.sd[p.yr$Period == "pre"], 
          rev(p.yr$d18O.ec.m[p.yr$Period == "pre"] - 
                p.yr$d18O.ec.sd[p.yr$Period == "pre"])),
        col = rgb(0.7, 0, 0, 0.3), lty = 0)
lines(p.yr$Year[p.yr$Period == "pre"], 
      p.yr$d18O.ec.m[p.yr$Period == "pre"] + 
        p.yr$d18O.ec.sd[p.yr$Period == "pre"],
      col = rgb(0.7, 0, 0), lty = 3)
lines(p.yr$Year[p.yr$Period == "pre"], 
      p.yr$d18O.ec.m[p.yr$Period == "pre"] - 
        p.yr$d18O.ec.sd[p.yr$Period == "pre"],
      col = rgb(0.7, 0, 0), lty = 3)

polygon(c(p.yr$Year[p.yr$Period == "m"], 
          rev(p.yr$Year[p.yr$Period == "m"])),
        c(p.yr$d18O.ec.m[p.yr$Period == "m"] + 
            p.yr$d18O.ec.sd[p.yr$Period == "m"], 
          rev(p.yr$d18O.ec.m[p.yr$Period == "m"] - 
                p.yr$d18O.ec.sd[p.yr$Period == "m"])),
        col = rgb(0, 0, 0.9, 0.3), lty = 0)
lines(p.yr$Year[p.yr$Period == "m"], 
      p.yr$d18O.ec.m[p.yr$Period == "m"] + 
        p.yr$d18O.ec.sd[p.yr$Period == "m"],
      col = rgb(0, 0, 0.9), lty = 3)
lines(p.yr$Year[p.yr$Period == "m"], 
      p.yr$d18O.ec.m[p.yr$Period == "m"] - 
        p.yr$d18O.ec.sd[p.yr$Period == "m"],
      col = rgb(0, 0, 0.9), lty = 3)

polygon(c(p.yr$Year[p.yr$Period == "post"], 
          rev(p.yr$Year[p.yr$Period == "post"])),
        c(p.yr$d18O.ec.m[p.yr$Period == "post"] + 
            p.yr$d18O.ec.sd[p.yr$Period == "post"], 
          rev(p.yr$d18O.ec.m[p.yr$Period == "post"] - 
                p.yr$d18O.ec.sd[p.yr$Period == "post"])),
        col = rgb(0, 0.5, 0, 0.3), lty = 0)
lines(p.yr$Year[p.yr$Period == "post"], 
      p.yr$d18O.ec.m[p.yr$Period == "post"] + 
        p.yr$d18O.ec.sd[p.yr$Period == "post"],
      col = rgb(0, 0.5, 0), lty = 3)
lines(p.yr$Year[p.yr$Period == "post"], 
      p.yr$d18O.ec.m[p.yr$Period == "post"] - 
        p.yr$d18O.ec.sd[p.yr$Period == "post"],
      col = rgb(0, 0.5, 0), lty = 3)

lines(p.yr$Year[p.yr$Period == "pre"], 
      p.yr$d18O.ec.m[p.yr$Period == "pre"], 
      col = rgb(0.7, 0, 0),
      lwd = 2)
lines(p.yr$Year[p.yr$Period == "m"], 
      p.yr$d18O.ec.m[p.yr$Period == "m"], 
      col = rgb(0, 0, 0.9),
      lwd = 2)
lines(p.yr$Year[p.yr$Period == "post"], 
      p.yr$d18O.ec.m[p.yr$Period == "post"], 
      col = rgb(0, 0.5, 0),
      lwd = 2)
text(2014, -4.5, "(c)")

dev.off()

### Figure 5

png("out/Fig5.png", width = 4, height = 7.2, units = "in", res = 600)

layout(matrix(c(1, 2, 3), ncol = 1), 
       heights = c(lcm(2.3 * 2.54), lcm(2.3 * 2.54), lcm(2.6 * 2.54)))

#d18O per area
par(mai = c(0.3, 0.6, 0.1, 0.1))
plot(tser$mt[tser$cl == 1], tser$o[tser$cl == 1], 
     type = "l", lwd = 2, ylim = c(-13, -2),
     axes = FALSE, xlab = "", 
     ylab = expression(delta^{18}*"O (VSMOW)"))
axis(1, labels = FALSE)
axis(2)
box()
lines(tuc$Month, tuc$d18O, lty = 3, lwd = 2, col = 8)
for(i in 1:5){
  lines(tser$mt[tser$cl == i], tser$o[tser$cl == i],
        col = i, lwd = 2)
}
text(4, -2.3, "(a)")

#d18O.ec per area
plot(tser$mt[tser$cl == 1], tser$o.ec[tser$cl == 1], 
     type = "l", lwd = 2, ylim = c(-16, -5), 
     axes = FALSE, xlab = "", 
     ylab = expression("E-corrected "*delta^{18}*"O (VSMOW)"))
axis(1, labels = FALSE)
axis(2)
box()
lines(tuc$Month, tuc$d18O.ec, lty = 3, lwd = 2, col = 8)
for(i in 1:5){
  lines(tser$mt[tser$cl == i], tser$o.ec[tser$cl == i],
        col = i, lwd = 2)
}
text(4, -5.3, "(b)")

#evap per area
par(mai = c(0.6, 0.6, 0.1, 0.1))
plot(tser$mt[tser$cl == 1], tser$o[tser$cl == 1] -
       tser$o.ec[tser$cl == 1], type = "l", 
     lwd = 2, ylim = c(0, 8), xlab = "Month",
     ylab = "Evaporation index")
for(i in 1:5){
  lines(tser$mt[tser$cl == i], tser$o[tser$cl == i] -
          tser$o.ec[tser$cl == i],
        col = i, lwd = 2)
}
lines(tuc$Month, tuc$E, lty = 3, lwd = 2, col = 8)
ltx = c("South", "North", "West", "Mountain", "East",
        "Tucson")
legend(8, 8, ltx, col = c(1:5, 8), lty = c(rep(1, 5), 3), 
       lwd = 2, bty = "n")
polygon(c(3.8, 3.8, 4.2, 4.2), c(8.1, 7.5, 7.5, 8.1), border = NA, 
        col = "white")
text(4, 7.8, "(c)")

dev.off()

### Figure 8

png("out/Fig8.png", units = "in", width = 5, height = 5.7, 
    res = 600)
layout(matrix(c(1, 2), nrow = 2), 
       heights = c(lcm(2.6 * 2.54), lcm(3.1 * 2.54)))

par(mai = c(0.1, 1, 0.1, 0.1))
boxplot(g.mix$s1_fraction, s.mix$s1_fraction, 
        r.mix$s1_fraction, l.mix$s1_fraction, 
        outline = FALSE, ylab = "NAM source fraction")
text(0.5, 0.55, "(a)")

par(mai = c(0.6, 1, 0.1, 0.1))
boxplot(g.mix$E, s.mix$E, 
        r.mix$E, l.mix$E, 
        outline = FALSE, names = c("Ground", "Spring",
                                   "Stream", "Lake"),
        ylab = "Evaporation index")
text(0.5, 12.6, "(b)")

dev.off()
