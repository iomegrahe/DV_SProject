require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(extrafont)

##### SCATTER PLOT #####

df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from males"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jso464', PASS='orcl_jso464', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))

ggplot() + 
  coord_cartesian() + 
  scale_x_continuous() +
  scale_y_continuous() +
  labs(title='Males education and wage for each occupation') +
  labs(x="SCHOOL", y=paste("WAGE")) +
  layer(data=df, 
        mapping=aes(x=as.numeric(as.character(SCHOOL)), y=as.numeric(as.character(WAGE)), color=OCCUPATION), 
        stat="identity", 
        stat_params=list(), 
        geom="point",
        geom_params=list(), 
        position=position_jitter(width=0.3, height=.01)
  )

##### CROSSTAB #####

KPI_Low_Max_value = 7     
KPI_Medium_Max_value = 10

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select OCCUPATION, ETHN, ROUND(average_exper, 2) as AVERAGE_EXPER, INDUSTRY, KPI as RATIO, 
case
when kpi < "p1" then \\\'03 Low\\\'
when kpi < "p2" then \\\'02 Medium\\\'
else \\\'01 High\\\'
end kpi
from (select OCCUPATION, ETHN, AVG(EXPER) as average_exper, INDUSTRY, 
AVG(EXPER) as KPI
from Males
group by INDUSTRY, ETHN, OCCUPATION);"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cag4255', PASS='orcl_cag4255', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value, p2=KPI_Medium_Max_value), verbose = TRUE))); View(df)

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  facet_wrap(~OCCUPATION, ncol=1) +
  labs(title='MALES Crosstab\nAVERAGE_EXPER') +
  labs(x=paste("INDUSTRY"), y=paste("OCCUPATION/ETHNICITY")) +
  layer(data=df, 
        mapping=aes(x=INDUSTRY, y=ETHN, label=round(AVERAGE_EXPER,2)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black"), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=INDUSTRY, y=ETHN, fill=KPI), 
        stat="identity", 
        stat_params=list(), 
        geom="tile",
        geom_params=list(alpha=0.50), 
        position=position_identity()
  ) +
  scale_fill_manual(values=c("green", "blue", "red")
  )

##### BAR CHART #####

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select OCCUPATION, HEALTH, AVERAGE_WAGE, AVG(AVERAGE_WAGE) 
OVER (PARTITION BY HEALTH) as AGG_AVG_WAGE
from (select OCCUPATION, HEALTH, AVG(WAGE) as AVERAGE_WAGE
  from Males
  group by HEALTH, OCCUPATION) order by OCCUPATION;"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jso464', PASS='orcl_jso464', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE))); View(df)

df <- df %>% mutate(TOTAL_AVG = mean(AGG_AVG_WAGE))

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
  facet_wrap(~OCCUPATION, ncol=9) +
  labs(title='MALES BARCHART\nAVERAGE WAGE, AGGREGATE AVERAGE WAGE') +
  labs(x=paste("OCCUPATION/HEALTH"), y=paste("AVERAGE WAGE")) +
  layer(data=df, 
        mapping=aes(x=HEALTH, y=AVERAGE_WAGE, fill=HEALTH), 
        stat="identity", 
        stat_params=list(), 
        geom="bar",
        geom_params=list(alpha=rv$alpha),
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(x=HEALTH, y=AVERAGE_WAGE, label=round(AVERAGE_WAGE,3)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=0.5, vjust = -1), 
        position=position_identity()
  ) +
  layer(data=df, 
        mapping=aes(yintercept = TOTAL_AVG), 
        geom="hline",
        geom_params=list(colour="BLACK")
  ) 

##### BLENDING #####

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"select YEAR || \\\'   Avg Wage\\\' as measure_names, avg(WAGE) as measure_values from MALES
group by YEAR
union all
select YEAR || \\\'   Avg Hours Worked\\\' as measure_names, avg(lnhr) as measure_values from LABORSUPPLY
where YEAR > 1979 and YEAR < 1988
group by YEAR
order by 1;"
')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_jso464', PASS='orcl_jso464', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE))); View(df)

ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
  labs(title='Blending MALES and LABORSUPPLY') +
  labs(x=paste("Year"), y=paste("Wage and Hours Worked")) +
  layer(data=df, 
        mapping=aes(x=MEASURE_NAMES, y=MEASURE_VALUES), 
        stat="identity", 
        stat_params=list(), 
        geom="bar",
        geom_params=list(fill="orange"), 
        position=position_identity()
  ) + coord_flip() +
  layer(data=df, 
        mapping=aes(x=MEASURE_NAMES, y=MEASURE_VALUES, label=round(MEASURE_VALUES,3)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=1.1), 
        position=position_identity()
  ) 

