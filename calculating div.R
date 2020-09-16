library(quantmod)
library(dplyr)
start = "2017-06-27"
end="2019-12-31"

getSymbols("AAPL",from="2019-06-27", to="2019-08-01")
AAPL_div <- data.frame(getDividends("AAPL",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))

#AAPL_j <- left_join(AAPL, AAPL_div, by="Date")

#sum(AAPL_j$AAPL.div,na.rm = TRUE) == sum(AAPL_div$AAPL.div)

#AAPL_j <- AAPL_j %>% 
  #mutate(div_yield = 0)

#AAPL_dd <- which(complete.cases(AAPL_j$AAPL.div ))
#AAPL_div_yield = rep(0,nrow(AAPL_j))

#AAPL_div_yield[1:AAPL_dd[1]-1]=0

#AAPL_div_yield[AAPL_dd[1]:1+22*2-AAPL_dd[1]+1] = log(1-(AAPL_j[AAPL_dd[1],3]/ AAPL_j[1,2]))*(-252/(1+22*2-AAPL_dd[1]+1))
a <- data.frame(AAPL)
rownames(a)[2:22]
Date = rownames(a)[2:22]

AAPL_div_yield = data.frame(Date, div_yield = 0)

z <- data.frame(getDividends("AXP",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
div_yield = c(rep(0,3),c(rep(log(1-(0.39/123.940002))*(-252/18),18)))
AXP_div_yield = data.frame(Date, div_yield = div_yield)


z <- data.frame(getDividends("BA",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
BA_div_yield = data.frame(Date, div_yield = 0)


z <- data.frame(getDividends("CAT",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
div_yield = c(rep(0,14),rep(log(1-(1.03/135.5))*(-252/7),7))
CAT_div_yield = data.frame(Date, div_yield = div_yield)


z <- data.frame(getDividends("CSCO",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
div_yield = c(rep(0,3),c(rep(log(1-(0.35/55.73))*(-252/18),18)))
CSCO_div_yield = data.frame(Date, div_yield = div_yield)


z <- data.frame(getDividends("CVX",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
CVX_div_yield = data.frame(Date, div_yield = 0)

z <- data.frame(getDividends("DIS",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
div_yield = c(rep(0,4),c(rep(log(1-(0.88/139.300003))*(-252/17),17)))
DIS_div_yield = data.frame(Date, div_yield = div_yield)

z <- data.frame(getDividends("GS",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
GS_div_yield = data.frame(Date, div_yield = 0)

z <- data.frame(getDividends("HD",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
HD_div_yield = data.frame(Date, div_yield = 0)


z <- data.frame(getDividends("IBM",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
IBM_div_yield = data.frame(Date, div_yield = 0)


z <- data.frame(getDividends("INTC",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
INTC_div_yield = data.frame(Date, div_yield = 0)



z <- data.frame(getDividends("JNJ",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
JNJ_div_yield = data.frame(Date, div_yield = 0)




z <- data.frame(getDividends("JPM",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
div_yield = c(rep(0,3),c(rep(log(1-(0.8/108.839996))*(-252/18),18)))
JPM_div_yield = data.frame(Date, div_yield = div_yield)



z <- data.frame(getDividends("KO",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
KO_div_yield = data.frame(Date, div_yield = 0)



z <- data.frame(getDividends("MCD",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
MCD_div_yield = data.frame(Date, div_yield = 0)



z <- data.frame(getDividends("MMM",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
MMM_div_yield = data.frame(Date, div_yield = 0)



z <- data.frame(getDividends("MRK",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
MRK_div_yield = data.frame(Date, div_yield = 0)




z <- data.frame(getDividends("MSFT",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
MSFT_div_yield = data.frame(Date, div_yield = 0)



z <- data.frame(getDividends("NKE",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
NKE_div_yield = data.frame(Date, div_yield = 0)


z <- data.frame(getDividends("PFE",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
PFE_div_yield = data.frame(Date, div_yield = 0)


z <- data.frame(getDividends("PG",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
div_yield = c(rep(0,13),c(rep(log(1-(0.7459/109.779999))*(-252/8),8)))
PG_div_yield = data.frame(Date, div_yield = div_yield)


z <- data.frame(getDividends("TRV",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
TRV_div_yield = data.frame(Date, div_yield = 0)



z <- data.frame(getDividends("UNH",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
UNH_div_yield = data.frame(Date, div_yield = 0)


z <- data.frame(getDividends("UTX",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
UTX_div_yield = data.frame(Date, div_yield = 0)



z <- data.frame(getDividends("V",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
V_div_yield = data.frame(Date, div_yield = 0)



z <- data.frame(getDividends("VZ",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
div_yield = c(rep(0,6),c(rep(log(1-(0.6025/57.25))*(-252/15),15)))
VZ_div_yield = data.frame(Date, div_yield = div_yield)




z <- data.frame(getDividends("WBA",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
WBA_div_yield = data.frame(Date, div_yield = 0)



z <- data.frame(getDividends("WMT",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
WMT_div_yield = data.frame(Date, div_yield = 0)



z <- data.frame(getDividends("XOM",from = start, to=end, verbose = T)) %>%
  mutate(Date = rownames(.))
XOM_div_yield = data.frame(Date, div_yield = 0)





write.csv(AAPL_div_yield, file = "~/data/AAPL_div_yield.csv", row.names = F)
write.csv(AXP_div_yield, file = "~/data/AXP_div_yield.csv", row.names = F)
write.csv(BA_div_yield, file = "~/data/BA_div_yield.csv", row.names =F)
write.csv(CAT_div_yield, file = "~/data/CAT_div_yield.csv", row.names = F)
write.csv(CSCO_div_yield,file = "~/data/CSCO_div_yield.csv", row.names = F)
write.csv(CVX_div_yield, file = "~/data/CVX_div_yield.csv", row.names = F)
write.csv(DIS_div_yield, file = "~/data/DIS_div_yield.csv", row.names = F)

write.csv(GS_div_yield, file = "~/data/GS_div_yield.csv", row.names = F)
write.csv(HD_div_yield, file = "~/data/HD_div_yield.csv", row.names = F)
write.csv(IBM_div_yield, file = "~/data/IBM_div_yield.csv", row.names = F)
write.csv(INTC_div_yield, file = "~/data/INTC_div_yield.csv", row.names = F)
write.csv(JNJ_div_yield, file = "~/data/JNJ_div_yield.csv", row.names = F)
write.csv(JPM_div_yield, file = "~/data/JPM_div_yield.csv", row.names = F)
write.csv(KO_div_yield, file = "~/data/KO_div_yield.csv", row.names = F)
write.csv(MCD_div_yield, file = "~/data/MCD_div_yield.csv", row.names = F)
write.csv(MMM_div_yield, file = "~/data/MMM_div_yield.csv", row.names = F)
write.csv(MRK_div_yield, file = "~/data/MRK_div_yield.csv", row.names = F)
write.csv(MSFT_div_yield, file = "~/data/MSFT_div_yield.csv", row.names = F)
write.csv(NKE_div_yield, file = "~/data/NKE_div_yield.csv", row.names = F)
write.csv(PFE_div_yield, file = "~/data/PFE_div_yield.csv", row.names = F)
write.csv(PG_div_yield, file = "~/data/PG_div_yield.csv", row.names = F)
write.csv(TRV_div_yield, file = "~/data/TRV_div_yield.csv", row.names = F)
write.csv(UNH_div_yield, file = "~/data/UNH_div_yield.csv", row.names = F)
write.csv(VZ_div_yield, file = "~/data/VZ_div_yield.csv", row.names =F)
write.csv(WBA_div_yield, file = "~/data/WBA_div_yield.csv", row.names =F)
write.csv(WMT_div_yield, file = "~/data/WMT_div_yield.csv", row.names =F)
write.csv(XOM_div_yield, file = "~/data/XOM_div_yield.csv", row.names =F)
write.csv(V_div_yield, file = "~/data/V_div_yield.csv", row.names =F)
write.csv(UTX_div_yield, file = "~/data/UTX_div_yield.csv", row.names =F)


























