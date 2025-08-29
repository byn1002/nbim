# Analysis of the Government Pension Fund of Norway

This project provides an interactive and analytical overview of the **Norwegian Government Pension Fund Global** using R and RShiny. It combines dynamic visualizations, maps, and statistical summaries to help users explore the fund’s historical development, asset allocations, and investment strategies.

## Overall Analysis

In our final report file, `report.html`, we made detailed illustration and analysis for nbim data.

<img width="877" height="681" alt="image" src="https://github.com/user-attachments/assets/82b742b3-2254-4d98-8796-e83d801f7cfe" />


## Rshiny Page

**Welcome to our [Rshiny page](https://byn1002.shinyapps.io/nbim/)** 😊

In the website, we used rich interactive charts (mainly powered by [plotly](https://plotly.com/r/)) and maps (powered by [Leaflet](https://rstudio.github.io/leaflet/)) to perform a comprehensive analysis of NBIM data. 

At the same time, we drew on the [COVID Tracker](https://vac-lshtm.shinyapps.io/ncov_tracker/) to make the web page both rich in content and aesthetically pleasing.

### Main Panel:

- The left part of main panel contains yearly and overall analysis for Norwegian Government Pension Fund Global.
- The right part of main panel, which illustrates the investment of Norwegian Government Pension Fund Global on a world map, is designed as an interactive map.

<img width="877" height="482" alt="image" src="https://github.com/user-attachments/assets/14a5b9b5-923e-40ba-aa4a-b6ab4aaa2305" />

### Four Parts:

<img width="877" height="482" alt="image" src="https://github.com/user-attachments/assets/8885d251-fb15-4898-bfc9-cd77757e7459" />


## Project Structure

- **`nbim/`**  
  Contains the **RShiny application**, including all UI and server components, along with the data files used by the app.

- **`nbim_data/`**  
  Includes datasets used for the **written report**, such as summary statistics, annual returns, and asset class breakdowns.

- **`pic-shiny/`**  
  Stores images generated from the Shiny app, which are included in the **report for explanation and illustration**.

- **`report.Rmd`**  
  The **main RMarkdown file** used to generate the written report, including both static and interactive content explanations.

- **`report.html`**  
  The **compiled HTML version** of the final report, combining code, figures, and narrative analysis.
