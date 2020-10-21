# Generalized Comparison Collation (GeCCo) User's Manual 
## Sidebar Functionality
### Intake Tab
#### Previous Dataset File Input
This file input currently accepts CSV, DTA, and FEATHER filetypes.  This input dataset is used as all `previous.` columns for calculation & visualization.

From this input dataset, the **Select Columns to Merge On** >  **Prev Colnames** options are populated.

#### Current Dataset File Input
This file input currently accepts CSV, DTA, and FEATHER filetypes.  This input dataset is used as all `current.` columns for calculation & visualization.

From this input dataset, the **Select Columns to Merge On** > **Curr Colnames** options are populated.

#### Select Columns to Merge On Tabs
Broken down into `Prev` and `Curr` column names, this is where you will select the columns in both datasets that will merge them 1:1.

From these selections, all remaining observed column names in the `previous` and `current` input datasets will be populated into the **Select Column to Compare** Tabs

#### Select Column to Compare Tabs
Broken down into `Prev` and `Curr` column names, this is where you will select the column from both datasets that you would like to compare differences between.  Currently, only columns containing numeric data are operated against.

Once selected, you will see two summary tables populated in the **Overview** > **Summary Statistics** tab with basic summaries of both datasets.

#### Outlier Percentage Numeric Input
This numeric input box allows you to specify the percentage at which rows in your data are flagged as outliers.  This percentage is later used for calculations in the **Overview** > **Browse Data** tab & **Scatter Plots** > **Outlier Table** tab, and is visualized in the **Scatter Plots** > **Plot** tab where outliers become red dots.

#### Update Tables Button
This button executes the merge of the two input datasets on all specified `columns to merge` and performs calculations on the specified `column to compare`.  Click this button each time you make updates to the sidebar options.

After clicking this button, two new lines are printed in the **Overview** > **Summary Statistics** tab reporting the number of rows in the merged dataset and the number of rows where data in the `column to compare` is unequal.

After clicking this button, all other tabs are populated with their respective reporting.

### Advanced Manipulations Tab
#### Aggregate Data Check Box
Select this box if you want to aggregate your data by a specific column in the comparisons.

If selected, two new tabs appear giving you the options for which column you would like to aggregate by in the `previous` and `current` datasets.  You can also select the type of aggregation you want to perform, whether than be a `sum` or a `mean`.

#### Data Contains Draws Check Box
Select this box if your input datasets contain draws and you want to aggregate them before making comparisons.

If selected, two new tabs appear giving you two drop-down options each to select the column containing the draw names (draw_1, draw_2, etc.) and the column containing the draw values in both the `previous` and `current` datasets.

---

## Main Panel Functionality
### Overview Tab
#### Summary Statistics Tab
In this tab, basic information about the two input datasets is presented including the number of rows present and the mean/median/min/max of the `comparison column` selected, Once data is merged together, two new lines are printed reporting the number of rows in the merged dataset and the number of rows where data in the `comparison column` is unequal.

#### Browse Data Tab
In this tab, all merged columns, the comparison column, and the calculations performed are reported for every row in the merged dataset (noting which came from the previous and which came from the current).  The values in the `pct_diff` and `log_pct_diff` columns are used in relation to the specified `outlier percentage` in the sidebar functions.

The `equate_check` column reports whether the `comparison column` is equal in the `previous` and `current` dataset. This column reports **0** when the observations are equal, reports **1** when they are not equal.

The `abs_diff` column reports the absolute value of each previous row's `comparison column` to each current row's `comparison column`.

The `pct_diff` column reports the percentage difference between each row's `comparison column` to each current row's `comparison column`.  Calculated as: `(100 * (previous_comparison_column - current_comparison_column) / current_comparison_column)`

The `log_pct_diff` column reports the percentage difference in log space between each row's `comparison column` to each current row's `comparison column`.  Calculated as: `(100 * (log(log(previous_comparison_column)) - log(log(current_comparison_column))))`

### Scatter Plots Tab
#### Plot Options Check-Boxes
You have two options for ways to alter the scatter plot output.  First you can specify whether you'd like to view the plot in log space (as opposed to the default, natural space).  Second you can specify whether you'd like to facet the plot by a column you merged the datasets together with.

#### Plot Tab
In this tab you will see an interactive scatter plot rendered plotting the `previous comparison column` on the x-axis and the `current comparison column` on the y-axis.  Points calculated to be outliers are labeled in red, while non-outliers are labeled in blue.  You can hover over any point to see the `previous` and `current` observed value for the `comparison column`, and click+drag to zoom in on a desired set of points.

The red dashed line running diagonally through the plot serves as a reference for where points will lie if they are identical between the `previous` and `current` datasets.

#### Outlier Table Tab
In this tab you will see all outliers reported in a sort-able table.  This table will be blank if you see no red dots in the scatter plot (i.e. no outliers flagged at the specified percentage).

### Distribution Charts Tab
#### Box Plot Tab
In this tab you will see side-by-side, interactive box plots reporting the distribution of the `current` and `previous` `comparison column`.  You can hover over any point to see the `previous` and `current` observed value for the `comparison column`, and click+drag to zoom in on a desired set of points.

#### Density Plot Tab
In this tab you will see overlapped, interactive density plots reporting the distribution of the `current` and `previous` `comparison column`.  The dashed line is the mean density for the `previous comparison column` and dotted line is the mean density for the `current comparison column`.  You can hover over any point to see the previous and current observed value for the `comparison column`, and click+drag to zoom in on a desired set of points.
