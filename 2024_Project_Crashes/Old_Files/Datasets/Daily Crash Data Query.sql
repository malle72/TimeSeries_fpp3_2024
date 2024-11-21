select 
CrashDate,
HighwayClass,
COUNT(1) AS crashCount,
SUM(CAST(Pedestrian as int)) AS Pedestrian,
SUM(CAST(Bicycle as int)) AS Bicycle,
SUM(CAST(NonMotorist as int)) AS NonMotorist,
SUM(CAST(Motorcycle as int)) AS Motorcycle,
SUM(CAST(Fatal as int)) AS Fatal,
SUM(CASE 
	WHEN RoadwaySurfaceConditionCode in (100,104,105) THEN 1
	ELSE 0
END) as iceCrashes
from FactCrash
where CrashYear between 2018 and 2023
and ParishCode = 17
group by CrashDate, HighwayClass
order by CrashDate, CAST(HighwayClass as int)
