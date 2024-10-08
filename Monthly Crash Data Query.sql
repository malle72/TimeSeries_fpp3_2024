select 
CrashYear,
MonthSort,
HighwayClass,
COUNT(1) AS crashCount,
SUM(CAST(Pedestrian as int)) AS Pedestrian,
SUM(CAST(Bicycle as int)) AS Bicycle,
SUM(CAST(NonMotorist as int)) AS NonMotorist,
SUM(CAST(Motorcycle as int)) AS Motorcycle,
SUM(CAST(Fatal as int)) AS Fatal
from FactCrash
where CrashYear between 2021 and 2023
and ParishCode = 17
group by CrashYear, MonthSort, HighwayClass
order by CrashYear, MonthSort, CAST(HighwayClass as int)
