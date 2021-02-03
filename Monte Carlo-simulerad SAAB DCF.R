################################################################################
# Modell för att värdera börsbolag genom att diskontera kassaflöden
# Zino Engdalen Ricciuti - Feb 2021
################################################################################

##############
# Dependencies
##############
install.packages("ggplot2", dependencies = TRUE)
install.packages("fGarch", dependencies = TRUE)

library(ggplot2)
library(fGarch)

####################
# Pre-work
####################


revenue_defense <- c(0.0218, -0.0108, -0.0094, 0.1555, 0.0532, 0.0965, 0.0561, 
                     0.0687, -0.0192, 0.023, -0.0311, 0.0204, 0.0453, 0.1114, 
                     0.1586, -0.2655,  -0.0387, -0.1193, 0.0148, 0.1666, 0.0565,
                     0.0761, 0.1125, 0.0934,  -0.0607, 0.0104, -0.085, 0.0879, 
                     0.0597, -0.0318, -0.0234, 0.0882)

ggplot(revenue_defense_df, aes(x = revenue_defense)) + geom_density()

revenue_defense_df <- data.frame(revenue_defense)



opex_defense <- c(0.875, 0.915, 0.943, 0.929, 0.93, 0.937, 0.931, 0.932, 0.917, 
                  0.867, 0.849, 0.834, 0.87, 0.856, 0.864, 0.871, 0.884, 1.033, 
                  0.906, 0.901, 0.875, 0.884, 0.875, 0.865, 0.864, 0.857, 0.868,
                  0.904, 0.952, 0.916, 0.911, 0.902, 0.918, 0.905, 0.896)

opex_defense_df <- data.frame(opex_defense)

ggplot(opex_defense_df, aes(x = opex_defense)) + geom_density()


sd(revenue_defense)
hg_revenue_sim <- rsnorm(n = 5000, mean = 0.06, sd = 0.0877, xi=0.04)
hist(hg_revenue_sim)

sd(opex_defense)
hg_opex_of_revenue_sim <- rsnorm(n = 5000, 0.90, sd = 0.038, xi = 1.4)
hist(hg_opex_of_revenue_sim)



###################################
# Current inputs
# outstanding = antal aktier utestående 
# revenue= omsättning
# depreciation = nedskrivningar
###################################

outstanding <- 133.4
revenue <- 34977
depreciation <- 354
capex <- 1624
debt_outstanding <- 8773

######################################################################
# High growth period (hg) - värden för de kommande 5 åren

# hg_growth = genomsnittlig omsättningstillväxt närmaste 5 åren
# hg_opex_of_revenue = opex som andel av omsättningen de närmaste 5 åren
# hg_capex_and_depr_growth = genomsnittlig nedskrivnings- samt capextillväxt
#                                                         de närmaste 5 åren
# hg_wc_of_rev = andel working capital som andel av omsättning
# taxrate = skattesats
# hg_debt = andel av tillgångarna som är skuld-finansierade
# hg_beta = beta de närmaste 5 åren
# bondrate = nuvarande avkastning på statsobligationer
# mrp = market risk premium
# hg_cod = företagets räntesats för skuldfinansiering år 1-5
######################################################################
hg_growth <- hg_revenue_sim
hg_capex_and_depr_growth <- hg_revenue_sim
hg_opex_of_revenue <- hg_opex_of_revenue_sim
hg_wc_of_rev <- 0.034
taxrate <- 0.227
hg_debt <- 0.65
hg_beta <- 1.13
bondrate <- 0.01
mrp <- 0.0523
hg_cod <- 0.03



######################################################################
# Stable period (sp) - värden för år 6-10

# sp_growth = genomsnittlig omsättningstillväxt år 6-10
# sp_opex_of_rev = opex som andel av omsättningen år 6-10
# sp_capex_of_depr = capex som andel av nedskrivningar
# sp_capex_and_depr_growth = genomsnittlig nedskrivnings- samt capextillväxt
# sp_debt = andel av tillgångarna som är skuld-finansierade                                                      
# sp_cod = företagets räntesats för skuldfinansiering år 6-10
# sp_beta = beta år 6-10
######################################################################
sp_growth <- 0.02
sp_opex_of_rev <- 0.92
sp_capex_of_depr  <- 4.59
sp_capex_and_depr_growth <- 0.02
sp_debt <- 0.65
sp_cod <- 0.06
sp_beta <- 1.05

######################################################################
# weighted average cost of capital (wacc)

# hg - high growth period, tv = terminal value
# coe = cost of equity
# cod = cost of debt
# prop_of_d (/eq) = proportionen mellan skuld och eget kapital i balansrälningen
######################################################################
coe <- (bondrate + hg_beta * mrp) 
prop_of_eq <- (1 - hg_debt)
cod <- (hg_cod * (1 - taxrate))
prop_of_debt <- (1 - prop_of_eq)
wacc <- (coe * prop_of_eq + cod * prop_of_debt)
hg_wacc <- wacc
tv_coe <- (bondrate + mrp * sp_beta)
tv_prop_of_eq <- 1 - sp_debt
tv_cod <- (sp_cod + ((hg_cod - sp_cod) / 5) * (10 - 10)) * (1 - taxrate)
tv_prop_of_d <- 1 - tv_prop_of_eq
tv_wacc <- tv_coe * tv_prop_of_eq + tv_cod * tv_prop_of_d

coe6 <- tv_wacc + (wacc - tv_wacc)/5 * (10-6)
prop_of_eq6 <- (1 - sp_debt) + (prop_of_eq - (1 - sp_debt))/5 * (10 - 6)
cod6 <- sp_cod + ((hg_cod - sp_cod)/5) * (10 - 6) * (1 - taxrate)
prop_of_d6 <- (1 - prop_of_eq6)
wacc6 <- (coe6 * prop_of_eq6) + (cod6 * prop_of_d6)

coe7 <- tv_wacc + (wacc - tv_wacc)/5 * (10-7)
prop_of_eq7 <- (1 - sp_debt) + (prop_of_eq - (1 - sp_debt))/5 * (10 - 7)
cod7 <- sp_cod + ((hg_cod - sp_cod)/5) * (10 - 7) * (1 - taxrate)
prop_of_d7 <- (1 - prop_of_eq7)
wacc7 <- (coe7 * prop_of_eq7) + (cod7 * prop_of_d7)

coe8 <- tv_wacc + (wacc - tv_wacc)/5 * (10-8)
prop_of_eq8 <- (1 - sp_debt) + (prop_of_eq - (1 - sp_debt))/5 * (10 - 8)
cod8 <- sp_cod + ((hg_cod - sp_cod)/5) * (10 - 8) * (1 - taxrate)
prop_of_d8 <- (1 - prop_of_eq8)
wacc8 <- (coe8 * prop_of_eq8) + (cod8 * prop_of_d8)

coe9 <- tv_wacc + (wacc - tv_wacc)/5 * (10-9)
prop_of_eq9 <- (1 - sp_debt) + (prop_of_eq - (1 - sp_debt))/5 * (10 - 9)
cod9 <- sp_cod + ((hg_cod - sp_cod)/5) * (10 - 9) * (1 - taxrate)
prop_of_d9 <- (1 - prop_of_eq9)
wacc9 <- (coe9 * prop_of_eq9) + (cod9 * prop_of_d9)

######################################################################
# nå fritt kassaflöde för de kommande 10 åren (fcf)
# delta_wc = förändringen i working capital
######################################################################

# Y1
rev_1 <- revenue * (1 + hg_growth)
opex1 <- rev_1 * hg_opex_of_revenue
ebit1 <- rev_1 - opex1


ebit1_t <- ebit1 * (1 - taxrate)
depr1 <- depreciation * (1 + hg_capex_and_depr_growth)
capex1 <- capex * (1 + hg_capex_and_depr_growth)
delta_wc1 <- hg_wc_of_rev * (rev_1 - revenue)
fcf1 <- ebit1_t + depr1 - capex1 - delta_wc1 

# Y2
rev2 <- rev_1 * (1 + hg_growth)
opex2 <- rev2 * hg_opex_of_revenue
ebit2 <- rev2 - opex2

ebit2_t <- ebit2 * (1 - taxrate)
depr2 <- depr1 * (1 + hg_capex_and_depr_growth)
capex2 <- capex1 * (1 + hg_capex_and_depr_growth)
delta_wc2 <- hg_wc_of_rev * (rev2 - rev_1)
fcf2 <- ebit2_t + depr2 - capex2 - delta_wc2

# Y3
rev3 <- rev2 * (1 + hg_growth)
opex3 <- rev3 * hg_opex_of_revenue
ebit3 <- rev3 - opex3

ebit3_t <- ebit3 * (1 - taxrate)
depr3 <- depr2 * (1 + hg_capex_and_depr_growth)
capex3 <- capex2 * (1 + hg_capex_and_depr_growth)
delta_wc3 <- hg_wc_of_rev * (rev3 - rev2)
fcf3 <- ebit3_t + depr3 - capex3 - delta_wc3

# Y4
rev4 <- rev3 * (1 + hg_growth)
opex4 <- rev4 * hg_opex_of_revenue
ebit4 <- rev4 - opex4

ebit4_t <- ebit4 * (1 - taxrate)
depr4 <- depr3 * (1 + hg_capex_and_depr_growth)
capex4 <- capex3 * (1 + hg_capex_and_depr_growth)
delta_wc4 <- hg_wc_of_rev * (rev4 - rev3)
fcf4 <- ebit4_t + depr4 - capex4 - delta_wc4

# Y5
rev5 <- rev4 * (1 + hg_growth)
opex5 <- rev5 * hg_opex_of_revenue
ebit5 <- rev5 - opex5

ebit5_t <- ebit5 * (1 - taxrate)
depr5 <- depr4 * (1 + hg_capex_and_depr_growth)
capex5 <- capex4 * (1 + hg_capex_and_depr_growth)
deltaWC5 <- hg_wc_of_rev * (rev5 - rev4)
fcf5 <- ebit5_t + depr5 - capex5 - deltaWC5

# Y6
rev6 <- rev5 * (1 + sp_growth + ((hg_growth - sp_growth) / 5) *(10 - 6))
opex6 <- rev6 * (sp_opex_of_rev + ((hg_opex_of_revenue - sp_opex_of_rev) / 5) 
                 * (10 - 6))
ebit6 <- rev6 - opex6

ebit6_t <- ebit6 * (1 - taxrate)
depr6 <- depr5 * (1 + sp_growth + ((hg_growth - sp_growth) / 5) *(10 - 6))
capex6 <- depr6 * sp_capex_of_depr
delta_wc6 <- hg_wc_of_rev * (rev6 - rev5)
fcf6 <- ebit6_t + depr6 - capex6 - delta_wc6

# Y7
rev7 <- rev6 * (1 + sp_growth + ((hg_growth - sp_growth) / 5) *(10 - 7))
opex7 <- rev7 * (sp_opex_of_rev + ((hg_opex_of_revenue - sp_opex_of_rev) / 5) 
                 * (10 - 7))
ebit7 <- rev7 - opex7

ebit7_t <- ebit7 * (1 - taxrate)
depr7 <- depr6 * (1 + sp_growth + ((hg_growth - sp_growth) / 5) * (10 - 7))
capex7 <- depr7 * sp_capex_of_depr
delta_wc7 <- hg_wc_of_rev * (rev7 - rev6)
fcf7 <- ebit7_t + depr7 - capex7 - delta_wc7

# Y8
rev8 <- rev7 * (1 + sp_growth + ((hg_growth - sp_growth) / 5) *(10 - 8))
opex8 <- rev8 * (sp_opex_of_rev + ((hg_opex_of_revenue - sp_opex_of_rev) / 5) 
                 * (10 - 8))
ebit8 <- rev8 - opex8

ebit8_t <- ebit8 * (1 - taxrate)
depr8 <- depr7 * (1 + sp_growth + ((hg_growth - sp_growth) / 5) * (10 - 8))
capex8 <- depr8 * sp_capex_of_depr
delta_wc8 <- hg_wc_of_rev * (rev8 - rev7)
fcf8 <- ebit8_t + depr8 - capex8 - delta_wc8


# Y9
rev9 <- rev8* (1 + sp_growth + ((hg_growth - sp_growth) / 5) *(10 - 9))
opex9 <- rev9 * (sp_opex_of_rev + ((hg_opex_of_revenue - sp_opex_of_rev) / 5) 
                 * (10 - 9))
ebit9 <- rev9 - opex9

ebit9_t <- ebit9 * (1 - taxrate)
depr9 <- depr8 * (1 + sp_growth + ((hg_growth - sp_growth) / 5) * (10 - 9))
capex9 <- depr9 * sp_capex_of_depr
delta_wc9 <- hg_wc_of_rev * (rev9 - rev8)
fcf9 <- ebit9_t + depr9 - capex9 - delta_wc9


# Y10
rev10 <- rev9* (1 + sp_growth + ((hg_growth - sp_growth) / 5) *(10 - 10))
opex10 <- rev10 * (sp_opex_of_rev + ((hg_opex_of_revenue - sp_opex_of_rev) / 5) 
                   * (10 - 10))
ebit10 <- rev10 - opex10

ebit10_t <- ebit10 * (1 - taxrate)
depr10 <- depr9 * (1 + sp_growth + ((hg_growth - sp_growth) / 5) * (10 - 10))
capex10 <- depr10 * sp_capex_of_depr
delta_wc10 <- hg_wc_of_rev * (rev10 - rev9)
fcf10 <- ebit10_t + depr10 - capex10 - delta_wc10

######################################################################
# Terminal value
######################################################################
tv <-  fcf10 * (1 + sp_growth) / (tv_wacc - sp_growth)

######################################################################
# Present value - diskontera kassaflödena till nuvärde
######################################################################

pv1 <- fcf1 / (1 + wacc)
pv2 <- fcf2 / (1 + wacc)^2
pv3 <- fcf3 / (1 + wacc)^3
pv4 <- fcf4 / (1 + wacc)^4
pv5 <- fcf5 / (1 + wacc)^5
pv6 <- fcf6 / (1 + wacc6)^6
pv7 <- fcf7 / (1 + wacc7)^7
pv8 <- fcf8 / (1 + wacc8)^8
pv9 <- fcf9 / (1 + wacc9)^9
pv10 <- fcf10 / (1 + tv_wacc)^10
pv_tv <-  tv / (1 + tv_wacc)^10

######################################################################
# Firm value
# share_value = värde per aktie
######################################################################
firm_value <- pv1 + pv2 + pv3 + pv4 + pv5 + pv6 + pv7 + pv8 + pv9 + pv10 + pv_tv
debt_value <- debt_outstanding
equity_value <- firm_value - debt_value
share_value <- equity_value / outstanding

share_value



#########################################################################

share_value = (abs(share_value) + share_value)/2

saab_dataframe <- data.frame(share_value)

ggplot(saab_dataframe,aes(x=share_value)) + geom_histogram()

ggplot(saab_dataframe,aes(x=share_value)) + geom_density(fill = "#FF6666") 

decile <- quantile(x=share_value, probs = seq(0.1, 0.9, length = 9))

decile_df <- data.frame(decile)

quart <- quantile(x= share_value, probs = seq(0.25,0.75, length = 3))

quart_df <- data.frame(quart)

ggplot(saab_dataframe,aes(x=share_value)) + 
  geom_histogram(fill="white", colour = "black", bins = 40) + 
  geom_vline(data = decile_df, aes(xintercept = decile), linetype=
               "longdash", size=1)


ggplot(saab_dataframe,aes(x=share_value)) + 
  geom_histogram(fill="white", colour = "black", bins = 40) + 
  geom_vline(data = quart_df, aes(xintercept = quart), linetype=
               "longdash", size=1.25, colour = "green")


ggplot(saab_dataframe,aes(x=share_value)) + 
  geom_density(fill="red", colour = "black") + 
  geom_vline(data = quart_df, aes(xintercept = quart), linetype=
               "longdash", size=1.25, colour = "green")




