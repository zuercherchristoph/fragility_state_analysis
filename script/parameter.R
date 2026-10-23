circ <- circleFun(c(0,0),2,npoints = 500)

#--------------------------------------
lang<-getOption("highcharter.lang")
lang$decimalPoint<-"."
lang$thousandsSep<-","
lang$drillUpText<-"<span style='font-size:11px; color:blue'> Back to</span> <span style='font-size:11px; color:blue'>{series.name}</span>"
options(highcharter.lang = lang)
#----------------------------------- Graph style--------------
gtitlestyle<-list(fontFamily="Arial", fontSize="16px")
gsubtitlestyle<-list(fontFamily="Arial", fontSize="14px")

vdem_extract<-c("country_text_id"="Code" ,             "year"="year",             
                "v2xnp_client"="v2xnp_client"  ,    "v2x_libdem"="v2x_libdem"    ,    "v2caviol"="v2caviol" ,        
                "v2xnp_regcorr"="v2xnp_regcorr"  ,   "v2clrspct"="v2clrspct"  ,       "v2x_rule"="v2x_rule",         
                "v2svstterr"="v2svstterr" )
vdem_col_select<-c("Country.Territory","country_text_id","year", 
                   "v2xnp_client" ,    "v2x_libdem"   ,    "v2caviol" ,        
                   "v2xnp_regcorr" ,   "v2clrspct"  ,       "v2x_rule",         
                   "v2svstterr")

geo_group<-c("Individual"="Country.Territory",
             "Geographic group"="Communiste",
             "Income group"="Income.group"#,
             #"Post-Socialist"="Communiste"
             )

FSIvariable<-c("C1: Security Apparatus"="C1",
               "C2: Factionalized Elites"="C2" ,       
               "C3: Group Grievance"="C3",
               "E1: Economy"="E1"  ,                   
               "E2: Economic Inequality"="E2",
               "E3: Human Flight and Brain Drain"="E3",
               "P1: State Legitimacy"="P1",
               "P2: Public Services"="P2" ,            
               "P3: Human Rights"="P3",
               "S1: Demographic Pressures"="S1" ,      
               "S2: Refugees and IDPs"="S2",
               "X1: External Intervention"="X1" )


WGIvariable<-c("Voice and Accountability"="Voice",
               "Political Stability No Violence"="Political",
               "Government Effectiveness"="Government",
               "Regulatory Quality"="Regulatory",
               "Rule of Law"="Rule",
               "Control of Corruption"="Control")


VDemCYvariable<-c("Clientelism Index"="v2xnp_client",
                  "Liberal democracy index"="v2x_libdem",
                  "Political violence"="v2caviol",
                  "Regime corruption"="v2xnp_regcorr",
                  "Rigorous and impartial public administration"="v2clrspct",
                  "Rule of law index"="v2x_rule",
                  "State authority over territory"="v2svstterr")

outputvariable<-c("Composite Index 1"="Factor1",
                  "Composite Index 2"="Factor2",
                  "Normalized Composite Index 1"="NFactor1",
                  "Normalized Composite Index 2"="NFactor2")

# Preferred default "Input indicators" selection per data source, applied on
# app start and whenever "Input data" is switched, so the app opens on the
# PI's preferred indicators/settings instead of requiring manual reselection
# each time. Sources without an entry here fall back to their first 3
# variables, matching prior behavior.
default_indicators <- list(
  WGI    = c("Political", "Government", "Rule", "Control"),
  VDemCY = c("v2x_rule", "v2caviol", "v2xnp_regcorr", "v2clrspct")
)
default_output_indicator <- "NFactor1"

Yearlydata<-data.frame(Inter=seq(1,19,by=1)/19,Percent=seq(5,95,by=5))

datavariable<-c("WGI data"="WGI",
                "FSI data"="FSI", 
                "VDemCY data"="VDemCY")


listcolor1<-list(list(from=1, to=1, color="#FF0000", name="Very high"),
                 list(from=2, to=2, color="#FF5E00", name="High"),
                 list(from=3, to=3, color="#FFFF00", name="Medium"),
                 list(from=4, to=4, color="#009d00", name="Low")
)

class_group<-c("Very high"=1,"High"=2, "Medium"=3, "Low"=4)
