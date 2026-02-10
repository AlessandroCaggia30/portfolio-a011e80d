from fredapi import Fred
import warnings
import pandas as pd
import numpy as np
from statsmodels.api import OLS
import statsmodels.api as sm
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression
import seaborn as sns

# Basic settings
pd.options.mode.chained_assignment = None  # default='warn'
pd.set_option('display.max_rows', None)
pd.set_option('display.max_columns', None)
pd.set_option('display.width', None)
pd.set_option('display.max_colwidth', None)
warnings.filterwarnings("ignore")

# Initialize FRED API connection
f = Fred(api_key='1164d44bf68a3d4f5461d99fd5bda7fc')

# Initialize an empty DataFrame
df = pd.DataFrame()
series = f.get_series('UNRATE', observation_start='1950-01-01', aggregation_method = 'avg', frequency = 'q')
df['UNRATE'] = series
series = f.get_series('CPIAUCSL', observation_start='1950-01-01', units = 'pc1', aggregation_method = 'avg', frequency = 'q')
df['CPIAUCSL'] = series
series = f.get_series('MICH', observation_start='1950-01-01', aggregation_method = 'avg', frequency = 'q')
df['MICH'] = series

df = df.rename(columns={
    'UNRATE': 'Unemployment_rate',
    'CPIAUCSL': 'CPI',
    'MICH':'Expected_Inflation'
    })

# Set the index to be the date for ease of handling
df.index = pd.to_datetime(df.index)

#Qstn 8
"""
# Shift the CPI series by one period to get the t-1 values
df['CPI_past'] = df['CPI'].rolling(window=2).mean().shift(1)

# Calculate the average of the shifted CPI and the current MICH values
df['Expected_Inflation'] = (df['CPI_past'] + df['Expected_Inflation']) / 2
"""

# Calculate the excess inflation
df['Excess_Inflation'] = df['CPI'] - df['Expected_Inflation']
# a decreasing weighetd average of future expectations? ex 60% weight on 1yr, 25% on 2 yrs and 15% on 3 yrs? or what about suing the yield curve? 
df = df[df.index >= '1960-01-01']

# Drop the NA values that result from shifting
df.dropna(inplace=True)

# Now we can plot the scatter plot and linear fit
X = df['Unemployment_rate']
Y = df['Excess_Inflation']
X = sm.add_constant(X)  # Adds a constant term to the predictor

# Fit the linear regression model
model = OLS(Y, X).fit()

# Scatter plot
plt.scatter(df['Unemployment_rate'], df['Excess_Inflation'], alpha=0.5, label='Data Points')
plt.xlabel('Unemployment Rate')
plt.ylabel('Excess CPI Inflation')

# Add the linear fit line
plt.plot(df['Unemployment_rate'], model.predict(X), color='red', label='Linear Fit')

# Calculate the coefficient of determination (R-squared)
r_squared = model.rsquared
plt.title(f'Phillips Curve with R-squared: {r_squared:.3f}')
plt.legend()

# Show the plot
plt.show()

#Qstn 9
df['Excess_Inflation'] = df['CPI'] - df['Expected_Inflation']

# Define the decades
decades = {
    '1980s': (1982, 1989),
    '1990s': (1990, 1999),
    '2000s': (2000, 2007),
    '2007Q4-2020': (2007, 2020)
}
# Prepare the plot
fig, axes = plt.subplots(nrows=2, ncols=2, figsize=(14, 10))
axes = axes.flatten()  # Flatten the 2D array of axes for easy iteration

for i, (decade, (start, end)) in enumerate(decades.items()):
    # Filter the DataFrame for the decade
    if decade == '2007Q4-2020':
        mask = (df.index >= f'{start}-10-01') & (df.index <= f'{end}-12-31')
    else:
        mask = (df.index.year >= start) & (df.index.year <= end)
    df_decade = df[mask]

    # Prepare data for regression
    X = df_decade['Unemployment_rate']
    print(X)
    Y = df_decade['Excess_Inflation']
    print(Y)
    X = sm.add_constant(X)  # Adds a constant term to the predictor
    
    # Perform linear regression
    model = sm.OLS(Y, X).fit()
    
    # Scatter plot
    axes[i].scatter(df_decade['Unemployment_rate'], df_decade['Excess_Inflation'], alpha=0.5, label='Data Points')
    predicted_values = model.predict(X)
    axes[i].plot(df_decade['Unemployment_rate'], predicted_values, color='red', label='Linear Fit')
    
    # Calculate and annotate the Pearson correlation coefficient
    correlation = df_decade['Unemployment_rate'].corr(df_decade['Excess_Inflation'])
    axes[i].annotate(f'Corr: {correlation:.2f}', xy=(0.05, 0.95), xycoords='axes fraction', 
                     fontsize=9, backgroundcolor='white')

    # Annotate the slope
    slope = model.params[1]  # This is the slope of the line
    # Choosing a point near the middle of the x-range for the annotation
    x_val_for_annotation = df_decade['Unemployment_rate'].median()
    # Ensuring the index is within the bounds of the array
    index_for_annotation = min(df_decade['Unemployment_rate'].searchsorted(x_val_for_annotation), len(predicted_values) - 1)
    y_val_for_annotation = predicted_values.iloc[index_for_annotation]
    axes[i].annotate(f'Slope: {slope:.2f}', xy=(x_val_for_annotation, y_val_for_annotation), 
                     textcoords="offset points", xytext=(10,-10), ha='center')


    # Set title and labels
    axes[i].set_title(f'{decade} (R²: {model.rsquared:.3f})')
    axes[i].set_xlabel('Unemployment Rate')
    axes[i].set_ylabel('Excess CPI Inflation')
    
    # Add legend
    axes[i].legend()

# Adjust layout
plt.tight_layout()
plt.show()


