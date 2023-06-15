## From Words to Votes: How Political Speeches During COVID-19 Influence Public Opinion?

This repository contains the code and resources for the master thesis titled "From Words to Votes: How Political Speeches During COVID-19 Influence Public Opinion?". The study investigates the extent to which the rhetoric of public briefings by heads of government during the COVID-19 pandemic influenced public opinion. The project includes a novel pipeline for extracting linguistic features from political speeches and fitting Hierarchical Bayesian models to measure the effects.

## Table of Contents

-   [Introduction](#introduction)
-   [Repository Structure](#repository-structure)
-   [Requirements](#requirements)
-   [Troubleshooting](#troubleshooting)
-   [Contact](#contact)

## Introduction

The main goal of this project is to understand the relationship between the rhetoric used in political speeches during the COVID-19 pandemic and changes in political support. The pipeline extracts linguistic features such as sentiment and emotions from a database of over 150 speeches from leaders across the world using prompt engineering on OpenAI's `gpt-3.5-turbo` and `text-davinci-003` model. The effects are measured using a series of Hierarchical Bayesian models implemented via the probabilistic programming language Stan.

The findings of this study can serve as a basis for crafting political speeches to address future national crises and provide insights for practitioners in computational social science, NLP, and Machine Learning.

## Repository structure

``` kotlin
├── code/
├── data/
└── README.md
```

-   `README.md`: This file contains project description, structure and requirements.

-   `code/`: This folder contains all the R scripts used for data preparation and modeling.

-   `data/`: This folder contains all the raw datasets needed for the modeling and forecasting as well as the output datasets from any transformations and wrangling.

## Requirements

In order to replicate the results of this project, you will need R version 4.2.3. and Rstudio. All the required libraries are indicated in R scripts files.

## Troubleshooting

If you encounter any issues while setting up or running the project, please check the following:

-   Ensure that you have installed all the required libraries listed in the code.

-   Make sure that you properly installed the Rstan package following this [tutorial](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).

-   If you want to scrap speeches make sure that the websites haven't been updated since the time of scraping (Feb. 2023). If they were you might have to update the URL links.

-   If you want to replicate the translation and GPT-based labeling, you will need to generate API keys from [OpenAI](https://platform.openai.com/docs/introduction) and [Google](https://cloud.google.com/translate/docs/setup).

-    If you still need assistance, please feel free to open an issue on the GitHub repository, and we will do our best to help you resolve the problem.

## Contact

If you have any questions or suggestions, please feel free to contact the author at [gustawkempa\@gmail.com](mailto:gustawkempa@gmail.com). Your feedback is greatly appreciated!
