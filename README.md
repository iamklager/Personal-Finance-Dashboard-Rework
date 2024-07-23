# Personal-Finance-Dashboard-Rework

A rework of my original [Personal-Finance-Dashboard](https://github.com/iamklager/Personal-Finance-Dashboard).

## Major changes
- Hourly updating (i.e., this tool can now be online 24/7 instead of needing to restart it every time). This interval can be set changing the variable *n_RefreshTime* in the code.
- Automatic conversion between different currencies to the user's currency (*c_BaseCurrency* in the code).
- Asset sales are now correctly tracked (i.e., the portfolio is not long-only anymore).
- Proper tracking of profit and loss including unrealized asset gains.
- Changed asset groups from stocks \& metals to stocks \& alternatives.
- Reworked color scheme.
- Added a light/dark mode switch.
- Added a tooltip to each chart.
- Less unnecessary abstraction.


## Demo

<p align = "center">
  <img src = "https://github.com/iamklager/Personal-Finance-Dashboard-Rework/raw/main/.github/summary.png" width = "200" />
  <img src = "https://github.com/iamklager/Personal-Finance-Dashboard-Rework/raw/main/.github/income.png" width = "200" />
  <img src = "https://github.com/iamklager/Personal-Finance-Dashboard-Rework/raw/main/.github/expenses.png" width = "200" />
  <img src = "https://github.com/iamklager/Personal-Finance-Dashboard-Rework/raw/main/.github/assets.png" width = "200" />
</p>

