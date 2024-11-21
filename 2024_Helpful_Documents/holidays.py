import pandas as pd
import datetime

# Define the date range
start_date = datetime.datetime(2018, 1, 1)
end_date = datetime.datetime(2023, 12, 31)
date_range = pd.date_range(start_date, end_date)

# Define major holidays relevant to Baton Rouge, Louisiana
# (assuming federal holidays and some culturally significant ones in Louisiana)
holidays = {
    "New Year's Day": [(1, 1)],
    "Martin Luther King Jr. Day": ["third_monday_january"],
    "Mardi Gras": ["mardi_gras"],
    "Good Friday": ["good_friday"],
    "Memorial Day": ["last_monday_may"],
    "Independence Day": [(7, 4)],
    "Labor Day": ["first_monday_september"],
    "Thanksgiving Day": ["fourth_thursday_november"],
    "Christmas Day": [(12, 25)]
}

# Function to calculate dynamic holidays
def calculate_holidays(year):
    calculated_holidays = []
    
    # Martin Luther King Jr. Day (third Monday in January)
    jan_third_monday = pd.date_range(start=f"{year}-01-01", end=f"{year}-01-31", freq='W-MON')[2]
    calculated_holidays.append(jan_third_monday)
    
    # Mardi Gras (47 days before Easter)
    easter = pd.to_datetime(datetime.datetime(year, 3, 21)) + pd.offsets.Easter()
    mardi_gras = easter - pd.Timedelta(days=47)
    calculated_holidays.append(mardi_gras)
    
    # Good Friday (2 days before Easter)
    good_friday = easter - pd.Timedelta(days=2)
    calculated_holidays.append(good_friday)
    
    # Memorial Day (last Monday in May)
    may_last_monday = pd.date_range(start=f"{year}-05-01", end=f"{year}-05-31", freq='W-MON')[-1]
    calculated_holidays.append(may_last_monday)
    
    # Labor Day (first Monday in September)
    sep_first_monday = pd.date_range(start=f"{year}-09-01", end=f"{year}-09-30", freq='W-MON')[0]
    calculated_holidays.append(sep_first_monday)
    
    # Thanksgiving Day (fourth Thursday in November)
    nov_fourth_thursday = pd.date_range(start=f"{year}-11-01", end=f"{year}-11-30", freq='W-THU')[3]
    calculated_holidays.append(nov_fourth_thursday)
    
    # Fixed date holidays
    for month, day in [(1, 1), (7, 4), (12, 25)]:
        calculated_holidays.append(pd.Timestamp(year=year, month=month, day=day))
    
    return calculated_holidays

# Generate the binary holiday variable
holiday_dates = set()
for year in range(2018, 2024):
    holiday_dates.update(calculate_holidays(year))

# Create the DataFrame
data = {
    "Date": date_range.strftime("%m/%d/%Y"),
    "Holiday": [1 if date in holiday_dates else 0 for date in date_range]
}

df = pd.DataFrame(data)

# Save to CSV
file_path = "/mnt/data/holiday_dates_baton_rouge.csv"
df.to_csv(file_path, index=False)
file_path
