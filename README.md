## Welcome to GitHub Pages

  Throughout the [course] (https://idc9.github.io/stor390/) of the semester, we were taught and introduced to a multitude of different packages in R. For this project, we used data that was obtained from the Bureau of Transportation Statistics website, namely “T-100 Domestic Segment (U.S. Carriers)” data and decided to solely focus on the most recent year, 2016. While there was not any trouble cleaning the data since there were no missing values, we did have to create constraints for a smaller sample size since the data set was too large to work with some of our plots. From here, we divided our project into two distinct parts. Part one consisted of splitting the data into three separate groups based on airlines carrying passengers, freight or mail and then creating three maps that compared the annual volume of respective units of the top 15 routes across the U.S. We experimented with using a variety of colors or varying the thickness of the lines based on frequency but concluded that the best representation involved using the gradient scale of one color. Each route was given a shade pertaining to its popularity with lighter shades indicating less popular flights and darker shades indicating more popular ones. For our shiny app in part two, we filtered our data to only include routes with 5 or more airlines and passenger totals exceeding 5000. This allows the user to choose the route from a list of options and then graphically compares the average seats filled per airline.  
  
  
  Each of the three maps show 2016’s top 15 routes carrying the highest volume with respect to whether they are freight, mail or passenger planes. The color scale provides an easier method of comparison that allows the viewer to obtain the basic, necessary information at a quick glance. It is evident from the shade of the dark navy route from Chicago, IL to New York, NY that it is the most popular. It is also interesting to note from the scale that this particular route indicates carrying more than 1,000,000 more passengers than some of the flights from FL to NY. Map 2 is similar except it compares the top 15 most popular routes for airlines carrying freight. According to the key, the route from Anchorage, AK to Louisville, KY carries the around 500,000,000 pounds of freight a year which is more than double many of the other routes displayed. This is interesting to note because even among the top 15 routes, there is a large discrepancy between the top three routes and the remaining twelve. Map 3 uses the same setup as the other two except compares the volume of mail. Anchorage, AK to Bethel, AK holds holds the highest capacity at around 32,000,000 pounds of mail which is approximately three times as much as routes that fly from Los Angeles, CA to Newark, NJ. This is also the only variable that has 6 out of its top 15 routes in Hawaii and Alaska.  
  
  
  For our shiny app, we filtered our dataset to satisfy two constraints, keeping the routes that only had 5 or more airlines and total passengers over 5000. The app allows the users to choose from a list of 37 routes and then displays the airlines in descending order with the highest average percent of seats filled on the far left. Some of the plots that stood out to us the most were the plots from Los Angeles, CA to San Francisco, CA and Los Angeles, CA to Seattle, WA. The first shows a large variation in the spread amongst the different airlines in respect to the average seats filled with the highest, SkyWest Airlines Inc., hovering around 85% and the lowest, Delta Air Lines Inc., around 65%. The next plot showed results that were all close in proximity. Each of the 6 airlines displayed portrayed averages between 75-80%. Another interesting observation that we concluded was that routes further in distance tended to have higher average percentages and vice versa. The routes from Boston, MA to New York, NY and Boston, MA to Los Angeles, CA depict this well. The first route to New York has a larger range between the top and bottom most flights whereas the route to Los Angeles has a more even spread across the airlines. This could be because shorter flights are usually cheaper so missing the flights has a lower opportunity cost than a further and more expensive flight (to LA for example).  
  
  
  Over the course of the project, we were able to use our initial questions and really elaborate the ideas to visually represent our findings. Both parts of our project provide graphical representation of the data that accurately conveys the message we are trying to portray. We were able to use the information and knowledge that we had gained throughout the course of the semester to test our skills with new  features we had not previously used in R. After our success with the mapping, we decided that we wanted to challenge ourselves further by creating a shiny app that would accompany our project. Through this assignment, we became aware of the amount of resources available to us and were able to excel and reach our goals.  

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/WHF0530/Flight-Routes-Analysis/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://help.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.
