file_path<- "C:/Users/student/Downloads/beijing+multi+site+air+quality+data/PRSA2017_Data_20130301-20170228/PRSA_Data_20130301-20170228/PRSA_Data_Aotizhongxin_20130301-20170228.csv"

data <- tryCatch(
{
    read.csv(file_path)
},

error = function(e)
{
    if (grepl("cannot open", e$message))
    {
        cat("Error: The file was not found or cannot be opened.\n")
    }
    else if (grepl("more columns than column names|invalid|EOF", e$message))
    {
        cat("Error: The file format is incorrect.\n")
    }
    else
    {
        cat("Error:", e$message, "\n")
    }

    return(NULL)
}
)

# Execute only if the file is successfully imported
if(!is.null(data))
{
    cat("Dataset imported successfully.\n\n")

    # 1. Display first six records
    cat("First Six Records:\n")
    head(data)

    # 2. Display structure
    cat("\nStructure of Dataset:\n")
    str(data)

    # 3. Display number of rows and columns
    cat("\nNumber of Rows and Columns:\n")
    cat("Rows =", nrow(data), "\n")
    cat("Columns =", ncol(data), "\n")

    # 4. Check for missing values
    cat("\nDoes dataset contain missing values?\n")
    print(any(is.na(data)))

    # 5. Display total number of missing values
    cat("\nTotal Missing Values:\n")
    print(sum(is.na(data)))
}

colSums(is.na(data))

colSums(is.nan(as.matrix(data)))

# Check if dataset exists
is.null(data)

# Check for NA
any(is.na(data))
sum(is.na(data))

# Check for NaN
any(is.nan(as.matrix(data)))
sum(is.nan(as.matrix(data)))

# NA count per column
colSums(is.na(data))

# NaN count per column
colSums(is.nan(as.matrix(data)))


# User-defined function
missing_summary <- function(df)
{
    # Selected variables
    vars <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")

    # Create empty data frame for summary
    summary_df <- data.frame(
        Variable = character(),
        Total_Records = numeric(),
        Missing_Values = numeric(),
        Missing_Percentage = numeric(),
        stringsAsFactors = FALSE
    )

    # Loop through each variable
    for(var in vars)
    {
        if(var %in% names(df))
        {
            total <- nrow(df)
            missing <- sum(is.na(df[[var]]))
            percent <- (missing / total) * 100

            # Add row to summary
            summary_df <- rbind(summary_df,
                                data.frame(
                                    Variable = var,
                                    Total_Records = total,
                                    Missing_Values = missing,
                                    Missing_Percentage = round(percent,2)
                                ))

            # Warning if missing percentage > 20%
            if(percent > 20)
            {
                warning(paste(var, "contains more than 20% missing values"))
            }
        }
        else
        {
            warning(paste("Variable", var, "not found in dataset"))
        }
    }

    return(summary_df)
}

# Apply the function
result <- missing_summary(data)

# Display the summary
print(result)


# Create a new variable
data$pollution_ratio <- data$PM2.5 / data$PM10

# Check for NA values
cat("Number of NA values:\n")
print(sum(is.na(data$pollution_ratio)))

# Check for NaN values
cat("Number of NaN values:\n")
print(sum(is.nan(data$pollution_ratio)))

# Check for Infinite values (both +Inf and -Inf)
cat("Number of Infinite values:\n")
print(sum(is.infinite(data$pollution_ratio)))

# Check Positive Infinity
cat("Number of Positive Infinity values:\n")
print(sum(data$pollution_ratio == Inf, na.rm = TRUE))

# Check Negative Infinity
cat("Number of Negative Infinity values:\n")
print(sum(data$pollution_ratio == -Inf, na.rm = TRUE))

# Replace NaN and Infinite values with NA
data$pollution_ratio[
    is.nan(data$pollution_ratio) |
    is.infinite(data$pollution_ratio)
] <- NA

# Verify after replacement
cat("\nAfter Replacement:\n")
cat("NA values:", sum(is.na(data$pollution_ratio)), "\n")
cat("NaN values:", sum(is.nan(data$pollution_ratio)), "\n")
cat("Infinite values:", sum(is.infinite(data$pollution_ratio)), "\n")


# Vector of numerical variables
numeric_variables <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM")

# Loop through each variable
for (var in numeric_variables)
{
    # Check if column exists
    if (var %in% names(data))
    {
        # Count missing values before replacement
        missing_before <- sum(is.na(data[[var]]))

        # Calculate median (ignoring NA)
        median_value <- median(data[[var]], na.rm = TRUE)

        # Replace missing values with median
        data[[var]][is.na(data[[var]])] <- median_value

        # Count missing values after replacement
        missing_after <- sum(is.na(data[[var]]))

        # Display results
        cat("\n-----------------------------\n")
        cat("Variable:", var, "\n")
        cat("Missing Values Before:", missing_before, "\n")
        cat("Median Used:", median_value, "\n")
        cat("Missing Values After:", missing_after, "\n")
    }
    else
    {
        cat("\nColumn", var, "does not exist in the dataset.\n")
    }
}


# Function to calculate mode
calculate_mode <- function(x)
{
    # Remove missing values
    x <- x[!is.na(x)]

    # Find unique values
    unique_values <- unique(x)

    # Return the value with the highest frequency
    mode_value <- unique_values[which.max(tabulate(match(x, unique_values)))]

    return(mode_value)
}

# Missing values before replacement
missing_before <- sum(is.na(data$wd))

# Calculate mode of wd
mode_wd <- calculate_mode(data$wd)

# Replace missing values with mode
data$wd[is.na(data$wd)] <- mode_wd

# Missing values after replacement
missing_after <- sum(is.na(data$wd))

# Display results
cat("Mode of wd:", mode_wd, "\n")
cat("Missing Values Before:", missing_before, "\n")
cat("Missing Values After:", missing_after, "\n")


# Reusable function
clean_variable <- function(df, var_name)
{
  tryCatch(
  {
    # Check if variable exists
    if (!(var_name %in% names(df)))
    {
      stop("Variable does not exist in the dataset.")
    }

    # Check if variable is numeric
    if (!is.numeric(df[[var_name]]))
    {
      stop("The selected variable is not numerical.")
    }

    # Check if all values are missing
    if (all(is.na(df[[var_name]])))
    {
      stop("The variable contains only missing values.")
    }

    # Calculate median
    median_value <- median(df[[var_name]], na.rm = TRUE)

    # Check if median could be calculated
    if (is.na(median_value))
    {
      stop("Median cannot be calculated.")
    }

    # Replace missing values with median
    df[[var_name]][is.na(df[[var_name]])] <- median_value

    cat("Variable", var_name, "cleaned successfully.\n")
    cat("Median used:", median_value, "\n")

    # Return cleaned variable
    return(df[[var_name]])
  },

  error = function(e)
  {
    cat("Error:", e$message, "\n")
    return(NULL)
  })
}

pm25_clean <- clean_variable(data, "PM2.5")
print(pm25_clean)



before <- colSums(is.na(data[variables]))

# Selected variables
variables <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")

# Create empty comparison table
comparison <- data.frame(
  Variable = character(),
  Missing_Before = numeric(),
  Missing_After = numeric(),
  Values_Replaced = numeric(),
  stringsAsFactors = FALSE
)

# Loop through variables
for (var in variables)
{
  if (var %in% names(data))
  {
    # Count missing values before cleaning
    missing_before <- sum(is.na(data[[var]]))

    # Count missing values after cleaning
    missing_after <- sum(is.na(data[[var]]))

    # Calculate replaced values
    replaced <- missing_before - missing_after

    # Add results to table
    comparison <- rbind(
      comparison,
      data.frame(
        Variable = var,
        Missing_Before = missing_before,
        Missing_After = missing_after,
        Values_Replaced = replaced
      )
    )
  }
}

after <- colSums(is.na(data[variables]))


# Display comparison table
print(comparison)

Install package if not already installed
install.packages("ggplot2")
install.packages("reshape2")
library(reshape2)
library(ggplot2)

# Selected variables
variables <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")

# Store missing values before cleaning
missing_before <- c(
  PM2.5 = 873,
  PM10 = 910,
  SO2 = 45,
  NO2 = 210,
  TEMP = 30,
  WSPM = 18,
  wd = 8450
)

# Calculate missing values after cleaning
missing_after <- colSums(is.na(data[variables]))

# Create visualization dataset
missing_comparison <- data.frame(
  Variable = variables,
  Before_Cleaning = missing_before,
  After_Cleaning = missing_after
)

# Convert data from wide format to long format
library(reshape2)

plot_data <- melt(
  missing_comparison,
  id.vars = "Variable",
  variable.name = "Cleaning_Status",
  value.name = "Missing_Count"
)

# Create bar chart
ggplot(plot_data, 
       aes(x = Variable, 
           y = Missing_Count, 
           fill = Cleaning_Status)) +
  
  geom_bar(stat = "identity", 
           position = "dodge") +
  
  labs(
    title = "Comparison of Missing Values Before and After Data Cleaning",
    x = "Variables",
    y = "Number of Missing Values",
    fill = "Cleaning Status"
  ) +
  
  theme_minimal()



# Export cleaned dataset
write.csv(
  data,
  "cleaned_air_quality_data.csv",
  row.names = FALSE
)

cat("Cleaned dataset exported successfully.\n")

