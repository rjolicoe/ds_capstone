# Data Science Capstone
This is the project for the data science capstone project.  

In this project we created bi-grams, tri-grams and quad-grams and then model what the next likely word will be.  This will be run in a shiny application.  

The original data source came from: twitter feeds, blog posts and news articles.  This dataset was received from the partnership of JHU, Coursera and Swiftkey.  The logos are associated on the shiny applications.  We then tokenized our data to combine the source:

- Combine into a master datafile
- Sample from our dataset to create our n-gram
- Pull the most common matching to show what the most likely word will be.  
- In addition we will filter out swear words 
- We create the data as a dataframe and utilize the tidytext package

