## By JungJun Bae, Ria Sethi, and Hong Fan Wang

  Throughout the [course](https://idc9.github.io/stor390/) of the semester, we were taught and introduced to a multitude of different packages in R. For this project, we used data that was obtained from the Bureau of Transportation Statistics website, namely “T-100 Domestic Segment (U.S. Carriers)” data and decided to solely focus on the most recent year, 2016. While there was not any trouble cleaning the data since there were no missing values, we did have to create constraints for a smaller sample size since the data set was too large to work with some of our plots. From here, we divided our project into two distinct parts. Part one consisted of splitting the data into three separate groups based on airlines carrying passengers, freight or mail and then creating three maps that compared the annual volume of respective units of the top 15 routes across the U.S. We experimented with using a variety of colors or varying the thickness of the lines based on frequency but concluded that the best representation involved using the gradient scale of one color. Each route was given a shade pertaining to its popularity with lighter shades indicating less popular flights and darker shades indicating more popular ones. For our shiny app in part two, we filtered our data to only include routes with 5 or more airlines and passenger totals exceeding 5000. This allows the user to choose the route from a list of options and then graphically compares the average seats filled per airline.  


![Test](passengers.png "Top 15 Popular Routes By Passengers")

![Top 15 Popular Routes By Freight](freight.png "Top 15 Popular Routes By Freight")

![Top 15 Popular Routes By Mail](mail.png "Top 15 Popular Routes By Mail")


  Each of the three maps show 2016’s top 15 routes carrying the highest volume with respect to whether they are freight, mail or passenger planes. The color scale, on the right of the map, provides an easier method of comparison that allows the viewer to obtain the basic, necessary information at a quick glance. It is evident from the shade of the dark navy route from Chicago, IL to New York, NY that it is the most popular. It is also interesting to note from the scale that in one year, this particular route carries over 1,000,000 more passengers than some of the flights from FL to NY. Map 2 is similar except it with with respect to the airlines that are carrying freight. According to the key, the route from Anchorage, AK to Louisville, KY transfers around 500,000,000 pounds of freight a year which is more than double many of the other routes displayed. Among the top 15 routes, there is a large discrepancy between the top 3 routes and the remaining 12. Map 3 uses the same setup as the others except compares the volume of mail. The route from Anchorage, AK to Bethel, AK holds holds the highest capacity at around 32,000,000 pounds of mail which is approximately three times as much as routes that fly from Los Angeles, CA to Newark, NJ. This is also the only variable that has 6 out of its top 15 routes in Hawaii and Alaska.  
	
Since our dataset as a whole was too large, we had to filter the total number of routes for the purpose of our shiny app. The app allows the users to choose from a list of 37 routes and then displays the airlines in descending order with the highest average percent of seats filled on the far left and the lowest on the far right. Some of the plots that stood out to us the most were the routes from Los Angeles, CA to San Francisco, CA and from Los Angeles, CA to Seattle, WA. The first shows a large variation in the spread amongst the different airlines with respect to the average seats filled with the highest, SkyWest Airlines Inc., hovering around 85% and the lowest, Delta Air Lines Inc., around 65%. The next plot showed results that were all a lot closer in proximity with each of the 6 airlines displaying averages between 75-80%. This made us curious about whether or not there was a pattern amongst the routes and the averages. We were able to further show the consistency by comparing other routes in which the further distances had higher average percentages and vice versa. The routes from Boston, MA to New York, NY and Boston, MA to Los Angeles, CA were prime examples.  The first route to New York has a larger range between the highest and lowest flights whereas the route to Los Angeles has a more even spread across the airlines. This could be because shorter flights are usually cheaper so missing the flights has a lower opportunity cost than a further and more expensive flight (to LA for example). Also, it usually tends to be easier to find time in your schedule to rebook a shorter flight instead of having to find time to book a flight that is 6 hours in duration.  

Over the course of the project, we were able to expand on our initial questions about the data and really elaborate the ways in which we visually represented our results. Both parts of our project provide graphical representation of the data that accurately conveys the message we are trying to portray in an easy, visual manner. We were able to use the information and knowledge that we had gained throughout this course to test our skills with new features we had not previously used before in R. After our success with the mapping, we decided that to challenge ourselves further by creating a shiny app that would accompany our project. Through this assignment, we became aware of the amount of resources available to us and were able to excel and reach our goals.  
