## R package to connect with salesforce.com

>install.packages("RForcecom") 

## R package for linear regression

>install.packages("stats")  

## Load packages in R session
>library(RForcecom)                                                              
>library(stats)

>username <- "XXXXXX"
>password <- "XXXXXX"
>loginURL <-"https://na35.salesforce.com"
>apiVersion <- "39.0"

>(session <-rforcecom.login(username,password,loginURL,apiVersion))

## objectName Refers to table in salesforce

>objectName <- "Marketing_Spend"                                                  
>fields <- c("Id", "Year", "Quarter","Revenue","Internet_Campaign", "TV","Radio","Print","Phone","Referral") 

## Each row in the retrieved data will indicate total revenue generated as well as the spend on each mode of campaign per quarter.

>RForcecom.retrieve(session, objectName, fields)   

## Creating linear regressional model as the data is numeric.                              

>CampaignModel <-lm(Revenue ~ Internet_Campaign + TV + Radio + Print + Phone + Referral)  

## Viewing model summary

>summary(CampaignModel)





