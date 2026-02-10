from fredapi import Fred
import pandas as pd
import numpy as np
import warnings
import matplotlib.pyplot as plt
from statsmodels.nonparametric.smoothers_lowess import lowess
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score
from matplotlib.cm import ScalarMappable
from matplotlib.colors import Normalize

# Basic settings
pd.options.mode.chained_assignment = None  # default='warn'
pd.set_option('display.max_rows', None)
pd.set_option('display.max_columns', None)
pd.set_option('display.width', None)
pd.set_option('display.max_colwidth', None)
warnings.filterwarnings("ignore")

# Initialize FRED API connection
f = Fred(api_key='1164d44bf68a3d4f5461d99fd5bda7fc')

# List of series IDs
columns = ['JTSJOL', 'UNEMPLOY', 'LFWA64TTUSM647S']

# Initialize an empty DataFrame
df = pd.DataFrame()

# Retrieve each series and add it to the DataFrame
for i in columns:
    series = f.get_series(i, observation_start='2001-1-1')
    df[i] = series

df = df.rename(columns={
    'JTSJOL': 'Job_Openings',
    'UNEMPLOY': 'Unemployment',
    'LFWA64TTUSM647S': 'Working_Age_Population'})

df['Working_Age_Population'] = df['Working_Age_Population'] / 1000

# Calculate the ratios
df['Unemployment_Population_Ratio'] = df['Unemployment'] / df['Working_Age_Population']
df['JobOpenings_Population_Ratio'] = df['Job_Openings'] / df['Working_Age_Population']

# Define periods
periods = {
    'Pre-GreatRecession': ('2001-01-01', '2007-12-31'),
    'GreatRecession': ('2008-01-01', '2009-06-30'),
    'PostGreatRecession': ('2009-07-01', '2020-03-31'),
    'PandemicRecession': ('2020-02-01', '2022-04-30'),
    'PostPandemicRecession': ('2022-05-01', '2023-08-31')
}

# Add smoothing to each period's plot
def plot_smoothed(subset, x_metric, y_metric, label):
    # Applying LOWESS smoothing
    smoothed = lowess(subset[y_metric], subset[x_metric], frac=0.7)
    x_smooth = smoothed[:, 0]
    y_smooth = smoothed[:, 1]

    # Plotting the smoothed curve
    plt.plot(x_smooth, y_smooth, label=f"Smoothed {label}")

# Plotting
plt.figure(figsize=(12, 8))

# Question 6
all_dates = df.index
ordinals = [d.toordinal() for d in all_dates]
min_ordinal = min(ordinals)
max_ordinal = max(ordinals)

# Create normalization based on the ordinal range
norm = Normalize(vmin=min_ordinal, vmax=max_ordinal)

for period, (start_date, end_date) in periods.items():
    subset = df[(df.index >= start_date) & (df.index <= end_date)]
    # Convert subset dates to colors
    subset_ordinals = [d.toordinal() for d in subset.index]
    colors = [plt.cm.viridis(norm(o)) for o in subset_ordinals]
    plt.scatter(subset['Unemployment_Population_Ratio'], subset['JobOpenings_Population_Ratio'], c=colors, label=period, alpha=0.6)

    plot_smoothed(subset, 'Unemployment_Population_Ratio', 'JobOpenings_Population_Ratio', period)

# Add a colorbar to represent dates
sm = ScalarMappable(cmap='viridis', norm=norm)
sm.set_array([])
cbar = plt.colorbar(sm)
cbar.set_label('Date')

plt.title("Beveridge Curve: Unemployment vs. Job Openings Ratios")
plt.xlabel("Unemployment-Working_Age_Population Ratio")
plt.ylabel("Job Openings-Working_Age_Population Ratio")
plt.legend()
plt.grid(True)
plt.show()

# Question 7
# Filter data for the two periods
pandemic_recession = df[(df.index >= '2020-02-01') & (df.index <= '2022-04-30')]
post_pandemic_recession = df[(df.index >= '2022-05-01') & (df.index <= '2023-08-31')]

combined = pd.concat([pandemic_recession, post_pandemic_recession])

# Convert dates to ordinals for the color scale
date_ordinals = [d.toordinal() for d in combined.index]
norm = Normalize(vmin=min(date_ordinals), vmax=max(date_ordinals))
colors = [plt.cm.viridis(norm(o)) for o in date_ordinals]

# Plotting
plt.figure(figsize=(12, 8))

# Scatter points for pandemic_recession
plt.scatter(pandemic_recession['Unemployment_Population_Ratio'], pandemic_recession['JobOpenings_Population_Ratio'], c=colors[:len(pandemic_recession)], label='Pandemic Recession', alpha=0.6)

# Scatter points for post_pandemic_recession
plt.scatter(post_pandemic_recession['Unemployment_Population_Ratio'], post_pandemic_recession['JobOpenings_Population_Ratio'], c=colors[-len(post_pandemic_recession):], label='Post Pandemic Recession', alpha=0.6)

# LOWESS smoothed curve for both periods
plot_smoothed(pandemic_recession, 'Unemployment_Population_Ratio', 'JobOpenings_Population_Ratio', 'Pandemic Recession')
plot_smoothed(post_pandemic_recession, 'Unemployment_Population_Ratio', 'JobOpenings_Population_Ratio', 'Post Pandemic Recession')

# Labels, title, and grid
plt.title("Beveridge Curve: Highlighting Pandemic Recession vs. Post Pandemic Recession")
plt.xlabel("Unemployment-Working_Age_Population Ratio")
plt.ylabel("Job Openings-Working_Age_Population Ratio")
plt.legend()

# Adding colorbar to the plot
sm = plt.cm.ScalarMappable(cmap=plt.cm.viridis, norm=norm)
sm.set_array([])
cbar = plt.colorbar(sm)
cbar.set_label('Dates')

plt.grid(True)
plt.show()


# Qstn 8
df_ee = pd.read_excel('EE-FMP-August2023.xlsx', sheet_name='Data')
df_ee = df_ee[['year1', 'month1', 'FMP']]
df_ee['Date'] = pd.to_datetime(df_ee['year1'].astype(str) + '-' + 
                               df_ee['month1'].astype(str).str.zfill(2) + '-01')

df_ee.set_index('Date', inplace=True)
df_ee = df_ee.loc['2001-01-01':]
# 2. Construct the Measures
# Calculate the employment level
df['Employment_Level'] = df['Working_Age_Population'] * (1 - df['Unemployment_Population_Ratio'])
# Realized EE transitions
df['Realized_EE_Transitions'] = df['Employment_Level'] * df_ee['FMP']
# Construct the measure of unemployed and realized EE transitions as a share of the population
df['UE_and_EE_as_Share_of_Population'] = (df['Unemployment'] + df['Realized_EE_Transitions']) / df['Working_Age_Population']

# Plotting
plt.figure(figsize=(12, 8))

for period, (start_date, end_date) in periods.items():
    subset = df[(df.index >= start_date) & (df.index <= end_date)]
    plt.scatter(subset['UE_and_EE_as_Share_of_Population'], subset['JobOpenings_Population_Ratio'], label=period, alpha=0.6)
    plot_smoothed(subset, 'UE_and_EE_as_Share_of_Population', 'JobOpenings_Population_Ratio', period)
   

plt.title("Scatter Plot of UE & EE vs Job Openings-Population Ratio")
plt.xlabel("UE & EE as Share of Population")
plt.ylabel("Job Openings-Population Ratio")
plt.legend()
plt.grid(True)
plt.show()



# Qstn9
# 1. Calculate the number of job seekers
df['Job_Seekers'] = df['Employment_Level'] * 0.22

# 2. Calculate the effective employed job seekers
df['Effective_Job_Seekers'] = df['Job_Seekers'] / 2

# 3. Construct the measure
df['UE_and_Effective_Job_Seekers_as_Share_of_Population'] = (df['Unemployment'] + df['Effective_Job_Seekers']) / df['Working_Age_Population']

# Plotting the Data
plt.figure(figsize=(12, 8))

for period, (start_date, end_date) in periods.items():
    subset = df[(df.index >= start_date) & (df.index <= end_date)]
    plt.scatter(subset['UE_and_Effective_Job_Seekers_as_Share_of_Population'], subset['JobOpenings_Population_Ratio'], label=period, alpha=0.6)
    plot_smoothed(subset, 'UE_and_Effective_Job_Seekers_as_Share_of_Population', 'JobOpenings_Population_Ratio', period)

plt.title("Beveridge Curve with Adjusted Job Seekers")
plt.xlabel("UE & Effective Job Seekers as Share of Population")
plt.ylabel("Job Openings-Population Ratio")
plt.legend()
plt.grid(True)
plt.show()


# Qstn 10

def plot_smoothed_curve(dataframe, x_col, y_col, label, linestyle='-'):
    smoothed = lowess(dataframe[y_col], dataframe[x_col], frac=0.3)
    plt.plot(smoothed[:, 0], smoothed[:, 1], linestyle, label=label, alpha=0.8)

plt.figure(figsize=(15, 10))

# Traditional Beveridge Curve
for period, (start_date, end_date) in periods.items():
    subset = df[(df.index >= start_date) & (df.index <= end_date)]
    plt.scatter(subset['Unemployment_Population_Ratio'], subset['JobOpenings_Population_Ratio'], label=f"Traditional {period}")
    plot_smoothed_curve(subset, 'Unemployment_Population_Ratio', 'JobOpenings_Population_Ratio', f"Traditional Smoothed {period}")

# Beveridge Curve with UE & EE
for period, (start_date, end_date) in periods.items():
    subset = df[(df.index >= start_date) & (df.index <= end_date)]
    plt.scatter(subset['UE_and_EE_as_Share_of_Population'], subset['JobOpenings_Population_Ratio'], label=f"UE & EE {period}", marker='x')
    plot_smoothed_curve(subset, 'UE_and_EE_as_Share_of_Population', 'JobOpenings_Population_Ratio', f"UE & EE Smoothed {period}", '--')
    
# Beveridge Curve with Adjusted Job Seekers
for period, (start_date, end_date) in periods.items():
    subset = df[(df.index >= start_date) & (df.index <= end_date)]
    plt.scatter(subset['UE_and_Effective_Job_Seekers_as_Share_of_Population'], subset['JobOpenings_Population_Ratio'], label=f"Adjusted {period}", marker='+')
    plot_smoothed_curve(subset, 'UE_and_Effective_Job_Seekers_as_Share_of_Population', 'JobOpenings_Population_Ratio', f"Adjusted Smoothed {period}", '-.')

plt.title("Comparison of Three Beveridge Curves")
plt.xlabel("Unemployment Measures as Share of Population")
plt.ylabel("Job Openings-Population Ratio")
plt.legend(loc='upper right')
plt.grid(True)
plt.show()

# Define the metrics we're interested in
metrics = [
    'Unemployment_Population_Ratio',
    'UE_and_EE_as_Share_of_Population',
    'UE_and_Effective_Job_Seekers_as_Share_of_Population'
]

labels = [
    "Traditional Beveridge Curve",
    "Beveridge Curve with UE & EE",
    "Beveridge Curve with Adjusted Job Seekers"
]

colors = ['blue', 'red', 'green']



df.dropna(inplace = True)


for idx, metric in enumerate(metrics):
    
    x_values = []
    y_values = []
    
    for period, (start_date, end_date) in periods.items():
        subset = df[(df.index >= start_date) & (df.index <= '2023-08-31')]
        x_values.extend(subset[metric])
        y_values.extend(subset['JobOpenings_Population_Ratio'])
    
    # Applying LOWESS smoothing
    smoothed = lowess(y_values, x_values, frac=0.3)
    x_smooth = smoothed[:, 0]
    y_smooth = smoothed[:, 1]

    # Plotting the smoothed curve
    plt.plot(x_smooth, y_smooth, color=colors[idx], label=labels[idx])
    
    # You can also include the scatter points if desired
    plt.scatter(x_values, y_values, color=colors[idx], alpha=0.5, s=5)

plt.title("Smoothed Beveridge Curves using LOWESS")
plt.xlabel("Unemployment Measures as Share of Population")
plt.ylabel("Job Openings-Population Ratio")
plt.legend(loc='upper right')
plt.grid(True)
plt.show()

def compute_slope(x, y):
    x = x.values.reshape(-1, 1)  # reshaping for sklearn
    model = LinearRegression().fit(x, y)
    return model.coef_[0]

slopes = {}

for period, (start_date, end_date) in periods.items():
    subset = df[(df.index >= start_date) & (df.index <= end_date)]

    slope_traditional = compute_slope(subset['Unemployment_Population_Ratio'], subset['JobOpenings_Population_Ratio'])
    slope_ue_ee = compute_slope(subset['UE_and_EE_as_Share_of_Population'], subset['JobOpenings_Population_Ratio'])
    slope_adjusted = compute_slope(subset['UE_and_Effective_Job_Seekers_as_Share_of_Population'], subset['JobOpenings_Population_Ratio'])
    
    slopes[period] = {'Unemployment_Population_Ratio': slope_traditional, 'UE_and_EE_as_Share_of_Population': slope_ue_ee, 'UE_and_Effective_Job_Seekers_as_Share_of_Population': slope_adjusted}

    
#format output
for period, metrics in slopes.items():
    print(f"{period}:")
    for metric, value in metrics.items():
        print(f"\t{metric}: {value:.3f}")
    print("\n")

df = df.drop(df.columns[2], axis=1)
print(df)

# Using the `slopes` dictionary you provided, plotting the data
periods_list = list(slopes.keys())

traditional_slopes = [slopes[period]['Unemployment_Population_Ratio'] for period in periods_list]
ue_ee_slopes = [slopes[period]['UE_and_EE_as_Share_of_Population'] for period in periods_list]
adjusted_slopes = [slopes[period]['UE_and_Effective_Job_Seekers_as_Share_of_Population'] for period in periods_list]

plt.figure(figsize=(10, 6))
plt.plot(periods_list, traditional_slopes, marker='o', label='Traditional', color='blue')
plt.plot(periods_list, ue_ee_slopes, marker='o', label='UE & EE', color='green')
plt.plot(periods_list, adjusted_slopes, marker='o', label='Adjusted', color='red')

plt.title('Evolution of Slopes for the Three Curves')
plt.xlabel('Period')
plt.ylabel('Slope')
plt.xticks(rotation=45)
plt.grid(True, which='both', linestyle='--', linewidth=0.5)
plt.legend()
plt.tight_layout()
plt.show()


# Assuming results is your fitted VAR object from statsmodels
forecast_steps = 10
forecasted_values = results.forecast(data.values[-results.k_ar:], steps=forecast_steps)



