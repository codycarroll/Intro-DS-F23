# BSDS 100: Intro to Data Science with R

## Cody Carroll

**Email**: cjcarroll@usfca.edu

**Class Time**: 12:45 pm - 2:30 pm,	TR in Cowell 107

**Office Hours**:  11a-12p on [Zoom](https://usfca.zoom.us/my/cody.carroll) and 1p-2p in person in the Hive

**Book**: [R for Data Science](http://r4ds.had.co.nz/index.html) by Hadley Wickham and Garrett Grolemund

**Syllabus**: [Link](https://github.com/codycarroll/Intro-DS-F23/blob/main/Syllabus/BSDS100-Fall-2023-Syllabus.pdf)

## Course Learning Outcomes

By the end of this course, you will be able to

- Proficiently wrangle, manipulate, and explore data using the R programming language
- Utilize contemporary R libraries including *ggplot2*, *tibble*, *tidyr*, *dplyr*, *knitr*, and *stringr*
- Visualize, present, and communicate trends in a variety of data types
- Communicate results using R markdown and R Shiny
- Formulate data-driven hypotheses using exploratory data analysis and introductory model building techniques

## Course Overview

### Assessment

The focus of this course will be to provide you with the basic techniques available for making informed, data-driven decisions using the R programming language. This course will provide you the intuition to make hypotheses about complex questions through visualization, wrangling, manipulation, and exploration of data. The course will be graded based on the following components:

- **Attendance** (20%): You will lose 2% of this grade for every class you miss.
- **Assignments** (40%): You will be assigned a computational assignment to be completed using RStudio and the package *knitr* regularly throughout class. [Grading Standards](https://usfca.instructure.com/courses/1614499/files?preview=71276194)
- **Case Studies** (20%): You will be assigned applied case studies throughout the class that are to be completed using RStudio.
- **Final Project** (20%): The final project will be a computational case study that brings together the techniques learned throughout the semester. The description for this project will be provided towards the mid point of the semester.
- **Extra Credit** (+5%): Create a *well-organized* database of *all* R functions that you use throughout the semester. These include those mentioned in lectures, those introduced in homework, etc. Along with each function, give a brief description that details the use of the function. Also, organize these functions into categories according to their use.

### Schedule

Overall, this course will be split into two main parts: (1) learning the basics of how to code in R and (2) performing data analysis on real case studies and examples using data science techniques in R.


**Introduction**

 | Topic | Reading | Lab Assignment (HW) | Due Date | Code Demos|
 | :---  | :---:  | :---:  | :---:  | :---: |
 | [Introduction - A History of Data Science](https://github.com/codycarroll/Intro-DS-F23/blob/main/Lectures/Lecture%201%20Introduction.pdf) | [Ch. 1 What is Data Science?](https://www.safaribooksonline.com/library/view/doing-data-science/9781449363871/ch01.html)| Anonymous Class Survey (Canvas) | Friday, 8/25| |
 | [R and RStudio](https://github.com/codycarroll/Intro-DS-F23/blob/main/Lectures/R%20and%20RStudio.pdf) | |  |  | [First Coding Demo](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/first_R_script.R) |
 | [Packages and RMarkdown](https://github.com/codycarroll/Intro-DS-F23/blob/main/Lectures/R%20Markdown.pdf)  | | | | [First Knitting Demo](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/MyFirstKnit.Rmd) |
  
  
 **Data Structures in R**
  
 | Topic | Reading | Lab Assignment / HW  | Due Date | Code Demos|
  | :---  | :---:  | :---:  | :---:  | :---: |
  | [Vectors, Matrices, and Arrays](https://github.com/codycarroll/Intro-DS-F23/blob/main/Lectures/Data%20Structures%20I.pdf) | [Ch. 20 in R for Data Science](http://r4ds.had.co.nz/vectors.html) |  [HW1](https://github.com/codycarroll/Intro-DS-F23/blob/main/Assignments/HW1.pdf) <br> [Grading Standards](https://usfca.instructure.com/courses/1614499/files?preview=71276194)| Friday, 9/1| [Data Structures I](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/Data_Structures1.R) & [Data Structures II](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/Data_Structures2.R)|
  | [Lists and Data Frames](https://github.com/codycarroll/Intro-DS-F23/blob/main/Lectures/Data%20Structures%20II.pdf) | | [HW2](https://github.com/codycarroll/Intro-DS-F23/blob/main/Assignments/HW2.pdf)| Friday, 9/15| [Data Structures III](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/Data_Structures3.R) |  
  | [Tibbles](http://r4ds.had.co.nz/tibbles.html)| [Ch. 10 in R for Data Science](http://r4ds.had.co.nz/tibbles.html)|  |  | [Tibbles](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/tibbles.R) |
  | [String Analysis](http://r4ds.had.co.nz/strings.html)|[Ch. 14 in R for Data Science](http://r4ds.had.co.nz/strings.html) |  | | [String Analysis I](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/Strings1.R)|
   | [String Analysis 2](http://r4ds.had.co.nz/strings.html)|[Ch. 14 in R for Data Science](http://r4ds.had.co.nz/strings.html) | [HW3](https://github.com/codycarroll/Intro-DS-F23/blob/main/Assignments/HW3.pdf)| Friday, 10/6 | [String Analysis II](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/Strings2.R) |
  | [Factors](http://r4ds.had.co.nz/factors.html) | [Ch. 15 in R for Data Science](http://r4ds.had.co.nz/factors.html)| | | [Factors](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/Factors.R) |
  
  
  **Data Wrangling and Plotting**
  
 | Topic | Reading | Lab Assignment (HW) | Due Date | Code Demos|
   | :---  | :---:  | :---:  | :---:  | :---: |
   | [Plotting in R](https://github.com/codycarroll/Intro-DS-F23/blob/main/Lectures/Plotting%20in%20R.pdf) | | | | [ggplot](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/ggplot1.R)|
   | [Input and Output]() | | | | [Input and Output](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/Input_Output.R) |
   |[More Plotting with ggplot2](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/ggplot2_dogs.R)|||| [ggplot and dogs](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/ggplot2_dogs.R) | 
   | [Wrangling Data]() | | | |
   
 
 
 **Programming**
 
 | Topic | Reading | Lab Assignment (HW) | Due Date | Code Demos|
 | :---  | :---:  | :---:  | :---:  | :---: |
 | [Control Flow]()|[Ch. 19 in R for Data Science](http://r4ds.had.co.nz/vectors.html) | | | [Functions 1](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/Functions1.R)|
 | [Writing Functions]()| [Ch. 19 in R for Data Science](http://r4ds.had.co.nz/vectors.html)| | | [Functions 2](https://github.com/codycarroll/Intro-DS-F23/blob/main/CodeDemos/Functions2.R)|
 | [Functionals]()| [Ch. 18 in R for Data Science](http://r4ds.had.co.nz/vectors.html)| | | 
 
 **Statistical Modeling in R**
 
 | Topic | Reading | Lab Assignment (HW) | Due Date | Code Demos|
 | :---  | :---:  | :---:  | :---:  | :---: |
 | [Intro to Statistical Modeling in R]() | [Ch. 23 and 24 in R for Data Science](http://r4ds.had.co.nz) | | | 


### Case Studies
| Case Study | Data | Date |
|:--- | :---  | :---:  |
|[CS 1: ]() | | Tues, Sept. 19, 2023 | 
|[CS 2: ]() | | |
|[CS 3: ]()| | |

### Final Project
| Description | Due Date |
|:--- | :---  |
|[Project Signup]() | October XXth at 9:00 AM|
|[Final Project Description]() | November XXth at 9:00 AM|


### Important Dates

- Monday, August 28th - Last day to add the class
- Friday, September 8th - Census date. Last day to withdraw with tuition reversal
- Tuesday, October 17th - Fall break! (**no class**)
- Friday, November 3rd - Last day to withdraw
- Thursday, November 23rd - Thanksgiving Holiday (**no class**)
- Tuesday, December 5th - Last day of class
- Tuesday, December 7th - Review day

## Additional References

### Data Science

* [Data Manipulation with R](https://link.springer.com/book/10.1007%2F978-0-387-74731-6) by Spector.
* [Data Science in R](http://www.crcnetbase.com/doi/book/10.1201/b18325) by Nolan & Temple Lang.
* [Computational and Inferential Thinking](https://www.inferentialthinking.com/) by Adhikari & DeNero. 

### Visualization

* [ggplot2: Elegant Graphics for Data Analysis](https://link.springer.com/book/10.1007%2F978-3-319-24277-4) by Wickham. The official reference for ggplot2.
* [R Graphics Cookbook](http://www.cookbook-r.com/Graphs/) ([library](http://proquest.safaribooksonline.com/9781449363086)) by Chang. Examples for ggplot2.
* [R Graph Gallery](http://www.r-graph-gallery.com/). 
* [R Data Visualization Cheat Sheets.](http://www.r-graph-gallery.com/portfolio/r-dataviz-cheatsheet/)
* [Color Oracle](https://colororacle.org/), a free colorblindness simulator.
* [Color Brewer 2](http://colorbrewer2.org/), a web app to help choose colors.

### Statistics

* [Introductory Statistics with Randomization and Simulation](https://www.openintro.org/stat/textbook.php?stat_book=isrs) by OpenIntro.
* [Introduction to Statistical Learning](http://www-bcf.usc.edu/~gareth/ISL/) by James, Witten, Hastie & Tibshirani. 
* [The Elements of Statistical Learning](https://web.stanford.edu/~hastie/ElemStatLearn/) by Hastie, Tibshirani, & Friedman. 


### Other R Resources

* [UC Davis' Statistical DS Course ](https://github.com/nick-ulle/2018-ucdavis-sta141a/) by Nick Ulle.
* [The R Inferno](http://www.burns-stat.com/documents/books/the-r-inferno/) by Burns. About the difficult and confusing parts of R.
* [UC Berkeley's STA 133 Lecture Notes](https://www.stat.berkeley.edu/classes/s133/schedule.html).
* [RStudio Cheat Sheets](https://www.rstudio.com/resources/cheatsheets/).


### News
* [Data Science Beginnings](https://open.spotify.com/show/5SY1TPw3FubdSxCqrxUKZv?si=a1eb2a858e384db3) A podcast hosted by me and Robert Clements, another prof in the MSDS program.
* [R Weekly](https://rweekly.org/). About R.
* [R-bloggers](https://www.r-bloggers.com/). About R.
* [Data Tau](https://www.datatau.com/). About the DS industry.
