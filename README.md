# Stroke Data Analysis

**Authors:** Hanan Loulou, Nika Curcija, Julian Zacharias

## General Instructions

This repository contains a project analyzing stroke data using the "International Stroke Trial" dataset.\
To ensure smooth execution of the code, please read these instructions before running the scripts.

### Software Requirements

This project was developed using **R Version 4.3.1**. Some of the required packages may not be compatible with older versions of R, and using an earlier version may result in errors.

For a complete list of required packages, refer to the **`settings.R`** file in the root directory.

### Usage

To run the analysis, follow these steps:

1.  Open the **`presentation_praktikum_schlaganfall_update.qmd`** file located in the root directory.
2.  Click **"Run All"** to execute the script.
    -   This will first source the **`settings.R`** file, which installs all necessary packages and loads the required dataset.
    -   The data will then be cleaned and processed.
    -   Relevant functions will be executed to generate plots and other outputs.
    -   The final report will be generated automatically.

## Directory Structure

### Root directory

-   **`settings.R`** – Contains package dependencies and settings.
-   **`presentation_praktikum_schlaganfall_update.qmd`** – Updated main script.
-   **`presentation_praktikum_schlaganfall.qmd`** - Main script.

### `Daten`

This directory contains the original dataset from the **International Stroke Trial**.\
The data is publicly available.

### `Code`

This directory contains scripts for:

-   **Data import**

-   **Data cleaning**

-   **Analysis**

Each script is named according to its function. Key functions include:

-   **`load_data`** – Responsible for correctly loading the dataset.

-   **`cleanup`** – Performs most of the data cleaning required for this project.

-   Other scripts generate plots and may include additional data processing steps.

### `Bilder`

This directory stores all output figures from the analysis in **PNG** format.

------------------------------------------------------------------------

For any questions or issues, please reach out to the repository maintainers.
