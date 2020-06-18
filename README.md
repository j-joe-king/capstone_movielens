# capstone_movielens
HarvardX Data Science Capstone: Movielens Project

Files:

DataAnalysis.R

Downloads and cleans the data from the Movielens 10M data set.
Splits data between edx (90%) and validation (10%) data sets.
Does some visualisation of the edx dataset
Splits edx between edx_train (90%) and edx_test (10%) data sets.
Trains the machine learning model for movie recommendations taking account of movie, user and time effects to minimise the RMSE on the edx_test dataset.
Improves on the model by using a penalised least squares method.
Validates the model against the validation dataset 

DataAnalysisReport.Rmd

This is used to generate a PDF report outlining the work carried out to analyse to investigate the Movielens 10M dataset, then train a machine learning model to make movie recommendations, validated against a portion of the dataset. 

QuizMovielensDataset.R

This is the R Script to support the Movielens refresher quiz in the Capstone course.

Movielens.rproj

R Studio project file created using R version 3.6.3 (2020-02-29)