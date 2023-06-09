## From Words to Votes: How Political Speeches During COVID-19 Influence Public Opinion?
This repository contains the code and resources for the master thesis titled "From Words to Votes: How Political Speeches During COVID-19 Influence Public Opinion?". The study investigates the extent to which the rhetoric of public briefings by heads of government during the COVID-19 pandemic influenced public opinion. The project includes a novel pipeline for extracting linguistic features from political speeches and fitting Hierarchical Bayesian models to measure the effects.

## Table of Contents
* [Introduction](#introduction)
* [Repository Structure](#repository-structure)
* [Requirements](#requirements)
* [Data Prepatation](#data-preparation)
* [Modelling](#modelling)



## Introduction 
The main goal of this project is to understand the relationship between the rhetoric used in political speeches during the COVID-19 pandemic and changes in political support. The pipeline extracts linguistic features such as sentiment and emotions from a database of over 150 speeches from leaders across the world using prompt engineering on OpenAI's `gpt-3.5-turbo` model. The effects are measured using a series of Hierarchical Bayesian models implemented via the probabilistic programming language Stan.

The findings of this study can serve as a basis for crafting political speeches to address future national crises and provide insights for practitioners in computational social science, NLP, and Machine Learning.

## Repository structure

```kotlin
├── code/
├── data/
└── README.md
```

- `README.md`: This file contains project description, structure and requirements.

- `code/`: This folder contains all the R scripts used for data preparation and modeling.

- `data/`: This folder contains all the raw datasets needed for the modeling and forecasting as well as the output datasets from any transformations and wrangling.

'

## Installation
Installation
To set up the project, follow these steps:
Clone the repository:
Copy

git clone https://github.com/Gustaw-Kempa/Words-To-Votes.git
Install the required dependencies:
Copy

pip install -r requirements.txt
Usage
To run the pipeline, execute the following command:
Copy

python main.py
This will process the speeches, extract linguistic features, and fit the Hierarchical Bayesian models.
Dataset
The dataset comprises over 150 speeches from leaders across the world during the COVID-19 pandemic. The speeches are stored in the data/speeches directory. Each speech is saved as a separate text file, with the filename format leader_country_date.txt.
Results
The preliminary findings suggest that the use of persuasive language and expressions of anger are correlated with changes in political support. The results are stored in the results directory, with separate files for each model and analysis.
Contributing
If you would like to contribute to this project, please follow these steps:
Fork the repository
Create a new branch (git checkout -b feature_branch)
Commit your changes (git commit -m 'Add some feature')
Push to the branch (git push origin feature_branch)
Create a new Pull Request
License
This project is licensed under the MIT License. See the LICENSE file for details.
Acknowledgements
I would like to express my gratitude to my supervisor, colleagues, and everyone who provided valuable feedback and support throughout the development of this project.

Additional Resources
For further information on the tools and techniques used in this project, please refer to the following resources:
OpenAI GPT-3.5-turbo: Learn more about the GPT-3.5-turbo model and how to use it for various NLP tasks.
Stan: Explore the probabilistic programming language Stan, which is used for fitting the Hierarchical Bayesian models in this project.
PyStan: Discover PyStan, the Python interface for Stan, which is used in this project for implementing the models.
Troubleshooting
If you encounter any issues while setting up or running the project, please check the following:
Ensure that you have installed all the required dependencies from the requirements.txt file.
Verify that the dataset files are correctly placed in the data/speeches directory.
Check the error messages for any specific issues related to the code or dependencies.
If you still need assistance, please feel free to open an issue on the GitHub repository, and we will do our best to help you resolve the problem.
Citation
If you use this project or the dataset in your research, please cite the following:
Copy

@mastersthesis{Kempa2023,
  author = {Gustaw Kempa},
  title = {Analyzing the Impact of Political Speeches on Public Opinion during COVID-19},
  year = {2023},
  url = {https://github.com/Gustaw-Kempa/Words-To-Votes},
}
Contact
If you have any questions or suggestions, please feel free to contact the author at gustaw.kempa@email.com. Your feedback is greatly appreciated!
