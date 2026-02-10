import pandas as pd
import numpy as np
import statsmodels.api as sm
import matplotlib.pyplot as plt
from fredapi import Fred


f = Fred(api_key='1164d44bf68a3d4f5461d99fd5bda7fc')
pd.options.mode.chained_assignment = None  # default='warn'
pd.set_option('display.max_rows', None)
pd.set_option('display.max_columns', None)
pd.set_option('display.width', None)
pd.set_option('display.max_colwidth', None)

# Get the series and set the index as the date
unrate_series = f.get_series('UNRATE', observation_start='1950-01-01', aggregation_method='avg', frequency='q')
gdp_series = f.get_series('GDPC1', observation_start='1950-01-01', frequency='q')

# Combine the series into a DataFrame
df = pd.concat([unrate_series, gdp_series], axis=1)
df.columns = ['UNRATE', 'GDPC1']

# Calculate GDP growth rate and change in unemployment rate
df['GDP_growth'] = df['GDPC1'].pct_change() * 100
df['UNRATE_change'] = df['UNRATE'].diff()
# Remove the rows with NaN values that result from the differencing
df.dropna(inplace=True)

# Split the DataFrame for plots before and after 2019Q4
pre_2019Q4 = df[:'2019-10-01']
post_2019Q4 = df['2020-01-01':]
# Plot before 2019Q4
plt.figure(figsize=(12, 6))
pre_2019Q4['GDP_growth'].plot(label='GDP Growth Rate')
pre_2019Q4['UNRATE_change'].plot(label='Change in Unemployment Rate')
plt.legend()
plt.title('Economic Indicators before 2019Q4')
plt.xlabel('Year')
plt.ylabel('Percentage')
plt.grid(True)
plt.show()

# Plot after 2019Q4
plt.figure(figsize=(12, 6))
post_2019Q4['GDP_growth'].plot(label='GDP Growth Rate')
post_2019Q4['UNRATE_change'].plot(label='Change in Unemployment Rate')
plt.legend()
plt.title('Economic Indicators after 2019Q4')
plt.xlabel('Year')
plt.ylabel('Percentage')
plt.grid(True)
plt.show()
print(df)

# Question 9: Provide a scatter plot with GDP growth rate vs change in unemployment rate.
X = df['UNRATE_change']
y = df['GDP_growth']
X = sm.add_constant(X)  

model = sm.OLS(y, X).fit()

# Scatter plot with trendline
plt.figure(figsize=(12, 6))
plt.scatter(df['UNRATE_change'], df['GDP_growth'], alpha=0.5, label='Data Points')
plt.plot(df['UNRATE_change'], model.predict(X), color='red', label='Trendline')
plt.title('Okun\'s Law: Scatter Plot of GDP Growth vs Change in Unemployment')
plt.xlabel('Change in Unemployment Rate (Percentage Points)')
plt.ylabel('GDP Growth Rate (Percentage)')
plt.legend()
plt.grid(True)
plt.show()

# Correlation coefficient and R-squared
correlation = df['UNRATE_change'].corr(df['GDP_growth'])
r_squared = model.rsquared
intercept = model.params['const']
# Extracting the slope (coefficient) for the UNRATE_change
slope = model.params['UNRATE_change']

# Output the statistics
print(f"Correlation coefficient: {correlation}")
print(f"R-squared: {r_squared}")
print(f"Slope (coefficient for UNRATE_change): {slope}")
print(f"Intercept: {intercept}")

# First, let's prepare the dataset by setting the index as the date and adding a 'decade' column.
df.index = pd.to_datetime(df.index)
df['decade'] = (df.index.year // 10) * 10

# Question 10
decades = df['decade'].unique()
intercept_results = {}


# Create a new figure and axes for the plots
fig, axes = plt.subplots(nrows=int(np.ceil(len(decades) / 2)), ncols=4, figsize=(14, len(decades) * 2))
axes = axes.flatten()  # Flatten the axes array for easy iteration

for i, decade in enumerate(decades):
    # Filter the DataFrame for the decade
    decade_df = df[df['decade'] == decade]
    
    # Prepare the independent and dependent variables
    X_decade = decade_df['UNRATE_change']
    y_decade = decade_df['GDP_growth']
    X_decade = sm.add_constant(X_decade)  # Add a constant term for the intercept
    
    # Perform the regression
    model_decade = sm.OLS(y_decade, X_decade).fit()
    
    # Plot the scatter points and regression line for the decade
    axes[i].scatter(decade_df['UNRATE_change'], decade_df['GDP_growth'], label='Data Points')
    axes[i].plot(decade_df['UNRATE_change'], model_decade.predict(X_decade), color='red', label='Regression Line')
    axes[i].set_title(f'Regression for {decade}s')
    axes[i].set_xlabel('Change in Unemployment Rate (Percentage Points)')
    axes[i].set_ylabel('GDP Growth Rate (Percentage)')
    axes[i].legend()
    axes[i].grid(True)

# Adjust layout to prevent overlap
plt.tight_layout()
plt.show()

# Define a function to split the data into periods and calculate the regression statistics
def compute_regression_stats(df, period_col, y_var, x_var, periods):
    summary_data = []
    for period in periods:
        period_data = df[df[period_col] == period]
        X = period_data[x_var]
        Y = period_data[y_var]
        X = sm.add_constant(X)
        model = sm.OLS(Y, X).fit()
        # We do not report the intercept, only the slope, correlation coefficient, and R-squared
        slope = model.params[x_var]
        r_squared = model.rsquared
        correlation = period_data[x_var].corr(period_data[y_var])
        summary_data.append((period, slope, correlation, r_squared))

    summary_df = pd.DataFrame(summary_data, columns=['Period', 'Slope', 'Correlation coefficient', 'R2'])
    return summary_df


# First, let's prepare the dataset by setting the index as the date and adding a 'period' column.
df.index = pd.to_datetime(df.index)
df['period'] = (df.index.year // 10) * 10

# Let's define the periods for the regression stats as the unique decades in the dataset
periods = df['period'].unique()

# Now we compute the regression statistics for each period
regression_stats_df = compute_regression_stats(df, 'period', 'GDP_growth', 'UNRATE_change', periods)

# Display the computed summary dataframe
print(regression_stats_df)

# Plotting the evolution of slope, correlation coefficient, and R-squared over the periods horizontally
fig, axes = plt.subplots(1, 3, figsize=(20, 5))  # Arrange the plots horizontally

# Evolution of Slope
axes[0].plot(regression_stats_df['Period'], regression_stats_df['Slope'], marker='o', linestyle='-', color='blue')
axes[0].set_title('Evolution of Slope')
axes[0].set_xlabel('Period')
axes[0].set_ylabel('Slope')

# Evolution of Correlation Coefficient
axes[1].plot(regression_stats_df['Period'], regression_stats_df['Correlation coefficient'], marker='o', linestyle='-', color='green')
axes[1].set_title('Evolution of Correlation Coefficient')
axes[1].set_xlabel('Period')
axes[1].set_ylabel('Correlation Coefficient')

# Evolution of R-Squared
axes[2].plot(regression_stats_df['Period'], regression_stats_df['R2'], marker='o', linestyle='-', color='red')
axes[2].set_title('Evolution of R-Squared')
axes[2].set_xlabel('Period')
axes[2].set_ylabel('R2')

plt.tight_layout()
plt.show()
