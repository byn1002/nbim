# Analysis of the Government Pension Fund of Norway

This project provides an interactive and analytical overview of the **Norwegian Government Pension Fund Global** using R and RShiny. It combines dynamic visualizations, maps, and statistical summaries to help users explore the fund’s historical development, asset allocations, and investment strategies.

## Overall Analysis

In our final report file, `report.html`, we made detailed illustration and analysis for nbim data.

## Rshiny Page

Welcome to our [Rshiny page](https://byn1002.shinyapps.io/nbim/)😊

In the website, we used rich interactive charts (mainly powered by [plotly](https://plotly.com/r/)) and maps (powered by [Leaflet](https://rstudio.github.io/leaflet/)) to perform a comprehensive analysis of NBIM data. 

At the same time, we drew on the COVID Tracker ([https://vac-lshtm.shinyapps.io/ncov_tracker/](https://vac-lshtm.shinyapps.io/ncov_tracker/)) to make the web page both rich in content and aesthetically pleasing.

<img width="1498" height="813" alt="image" src="https://github.com/user-attachments/assets/14a5b9b5-923e-40ba-aa4a-b6ab4aaa2305" />


## Project Structure

- **`nbim/`**  
  Contains the **RShiny application**, including all UI and server components, along with the data files used by the app.

- **`nbim_data/`**  
  Includes datasets used for the **written report**, such as summary statistics, annual returns, and asset class breakdowns.

- **`pic-shiny/`**  
  Stores images generated from the Shiny app, which are included in the **report for explanation and illustration**.

- **`r_project.Rmd`**  
  The **main RMarkdown file** used to generate the written report, including both static and interactive content explanations.

- **`report.html`**  
  The **compiled HTML version** of the final report, combining code, figures, and narrative analysis.
