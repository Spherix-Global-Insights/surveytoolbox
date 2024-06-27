# surveytoolbox

## Update Notes
### 2024.06.27
- stack_loops() now re-applies labels so no metadata is lost

### 2024.02.21
- Added between() function to easily grab variables between two variables
- Added recode_to_mean() function to change values to the mean of the others
- check_primary() can now handle primary questions broken into multiple variables
- error_report() now generates a more extensive report

### 2023.11.10
- Added the check_value function which works similarly to check_seen except it checks for a particular value instead of missing values
- Multiple functions (ex. check_seen, check_seen_multi, check_value) can now take in multiple conditions and check it for its respective variable
- Added the stack_loops function
- Error report now shows the function that was used instead of just the variables
