# SQLZOO   
https://sqlzoo.net/wiki 

### Select Quiz 
1. Select from world: 

`FROM world SELECT name, 
population BETWEEN 1000000 AND 1250000`

2. Pick the result you would obtain from this code:

`     SELECT name, population
      FROM world
      WHERE name LIKE 'Al%'`

Table-E

 Albania   3200000  
 Algeria   32900000 

3. Select the code which shows the countries that end in A or L

`SELECT name FROM world
 WHERE name LIKE '%a' AND name LIKE '%l'`

4. `SELECT name,length(name)
FROM world
WHERE length(name)=5 and continent='Europe'`
   

  name	  length(name)

 Italy	 5
 
   Malta	 5
 
   Spain	 5


5. `SELECT name, area*2 FROM world WHERE population = 64000`

6. Select the code that would show the countries with an area larger than 50000 and a population smaller than 10000000

`SELECT name, area, population
  FROM world
 WHERE area > 50000 AND population < 10000000`

7. Select the code that shows the population density of China, Australia, Nigeria and France

`SELECT name, population/area
  FROM world
 WHERE name IN ('China', 'Nigeria', 'France', 'Australia')`

### BBC Quiz
1. Select the code which gives the name of countries beginning with U

`SELECT name
  FROM world
 WHERE name LIKE 'U%'`

2. Select the code which shows just the population of United Kingdom?

`SELECT population
  FROM world
 WHERE name = 'United Kingdom'`

3. Select the answer which shows the problem with this SQL code - the intended result should be the continent of France:

`SELECT continent 
   FROM world 
  WHERE 'name' = 'France'`
* 'name' should be name. 

4. Select the result that would be obtained from the following code:

`SELECT name, population / 10 
  FROM world 
 WHERE population < 10000`

5. Select the code which would reveal the name and population of countries in Europe and Asia

`SELECT name, population
  FROM world
 WHERE continent IN ('Europe', 'Asia')`

6. Select the code which would give two rows

`SELECT name FROM world
 WHERE name IN ('Cuba', 'Togo')`

7. Select the result that would be obtained from this code:

`SELECT name FROM world
 WHERE continent = 'South America'
   AND population > 40000000`

### Nobel Quiz 
1. Pick the code which shows the name of winner's names beginning with C and ending in n

`SELECT winner FROM nobel
 WHERE winner LIKE 'C%' AND winner LIKE '%n'`

2. Select the code that shows how many Chemistry awards were given between 1950 and 1960

`SELECT COUNT(subject) FROM nobel
 WHERE subject = 'Chemistry'
   AND yr BETWEEN 1950 and 1960`

3. Pick the code that shows the amount of years where no Medicine awards were given

`SELECT COUNT(DISTINCT yr) FROM nobel
 WHERE yr NOT IN (SELECT DISTINCT yr FROM nobel WHERE subject = 'Medicine')`
4. `SELECT subject, winner FROM nobel WHERE winner LIKE 'Sir%' AND yr LIKE '196%'`

Medicine	Sir John Eccles

Medicine	Sir Frank Macfarlane Burnet

5. Select the code which would show the year when neither a Physics or Chemistry award was given

`SELECT yr FROM nobel
 WHERE yr NOT IN(SELECT yr 
                   FROM nobel
                 WHERE subject IN ('Chemistry','Physics'))`
6. Select the code which shows the years when a Medicine award was given but no Peace or Literature award was

`SELECT DISTINCT yr
  FROM nobel
 WHERE subject='Medicine' 
   AND yr NOT IN(SELECT yr FROM nobel 
                  WHERE subject='Literature')
   AND yr NOT IN (SELECT yr FROM nobel
                   WHERE subject='Peace')`
7. `SELECT subject, COUNT(subject) 
   FROM nobel 
  WHERE yr ='1960' 
  GROUP BY subject`
   
### Nested SELECT Quiz
1. Select the code that shows the name, region and population of the smallest country in each region 

` SELECT region, name, population FROM bbc x WHERE population <= ALL (SELECT population FROM bbc y WHERE y.region=x.region AND population>0)`

2. Select the code that shows the countries belonging to regions with all populations over 50000

`SELECT name,region,population FROM bbc x WHERE 50000 < ALL (SELECT population FROM bbc y WHERE x.region=y.region AND y.population>0)`

3. Select the code that shows the countries with a less than a third of the population of the countries around it

`SELECT name, region FROM bbc x
 WHERE population < ALL (SELECT population/3 FROM bbc y WHERE y.region = x.region AND y.name != x.name)`

4. Select the result that would be obtained from the following code:
`SELECT name FROM bbc
 WHERE population >
       (SELECT population
          FROM bbc
         WHERE name='United Kingdom')
   AND region IN
       (SELECT region
          FROM bbc
         WHERE name = 'United Kingdom')`
   
Table-D
-------
France
Germany
Russia
Turkey

5. Select the code that would show the countries with a greater GDP than any country in Africa (some countries may have NULL gdp values).

`SELECT name FROM bbc
 WHERE gdp > (SELECT MAX(gdp) FROM bbc WHERE region = 'Africa')`
   
6. Select the code that shows the countries with population smaller than Russia but bigger than Denmark

`SELECT name FROM bbc
 WHERE population < (SELECT population FROM bbc WHERE name='Russia')
   AND population > (SELECT population FROM bbc WHERE name='Denmark')`
   
7. Select the result that would be obtained from the following code:
`SELECT name FROM bbc
 WHERE population > ALL
       (SELECT MAX(population)
          FROM bbc
         WHERE region = 'Europe')
   AND region = 'South Asia'`
   
Table-B
----------
Bangladesh
India
Pakistan

### SUM and COUNT Qui 
1. Select the statement that shows the sum of population of all countries in 'Europe'

`SELECT SUM(population) FROM bbc WHERE region = 'Europe'`

2. Select the statement that shows the number of countries with population smaller than 150000

` SELECT COUNT(name) FROM bbc WHERE population < 150000`

3. Select the list of core SQL aggregate functions: 
AVG(), COUNT(), MAX(), MIN(), SUM()

4. Select the result that would be obtained from the following code:
 No result due to invalid use of the WHERE function
`SELECT region, SUM(area)
   FROM bbc 
  WHERE SUM(area) > 15000000 
  GROUP BY region`

5. Select the statement that shows the average population of 'Poland', 'Germany' and 'Denmark'

` SELECT AVG(population) FROM bbc WHERE name IN ('Poland', 'Germany', 'Denmark')`

6. Select the statement that shows the medium population density of each region

` SELECT region, SUM(population)/SUM(area) AS density FROM bbc GROUP BY region`

7. Select the statement that shows the name and population density of the country with the largest population

` SELECT name, population/area AS density FROM bbc WHERE population = (SELECT MAX(population) FROM bbc)`

8. Pick the result that would be obtained from the following code:

`SELECT region, SUM(area) 
   FROM bbc 
  GROUP BY region 
  HAVING SUM(area)<= 20000000`

Table-D
---------
Americas	732240
Middle East	13403102
South America	17740392
South Asia	9437710

### JOIN Quiz 
1. You want to find the stadium where player 'Dimitris Salpingidis' scored. Select the JOIN condition to use:

` game  JOIN goal ON (id=matchid)`

2. You JOIN the tables goal and eteam in an SQL statement. Indicate the list of column names that may be used in the SELECT line:
`matchid, teamid, player, gtime, id, teamname, coach`
3. Select the code which shows players, their team and the amount of goals they scored against Greece(GRE).

`SELECT player, teamid, COUNT(*)
  FROM game JOIN goal ON matchid = id
 WHERE (team1 = "GRE" OR team2 = "GRE")
   AND teamid != 'GRE'
 GROUP BY player, teamid`

4. Select the result that would be obtained from this code:

`SELECT DISTINCT teamid, mdate
  FROM goal JOIN game on (matchid=id)
 WHERE mdate = '9 June 2012'
`

DEN	9 June 2012
GER	9 June 2012
5. Select the code which would show the player and their team for those who have scored against Poland(POL) in National Stadium, Warsaw.

`SELECT DISTINCT player, teamid 
   FROM game JOIN goal ON matchid = id 
  WHERE stadium = 'National Stadium, Warsaw' 
 AND (team1 = 'POL' OR team2 = 'POL')
   AND teamid != 'POL'`

6. Select the code which shows the player, their team and the time they scored, for players who have played in Stadion Miejski (Wroclaw) but not against Italy(ITA).

`SELECT DISTINCT player, teamid, gtime
  FROM game JOIN goal ON matchid = id
 WHERE stadium = 'Stadion Miejski (Wroclaw)'
   AND (( teamid = team2 AND team1 != 'ITA') OR ( teamid = team1 AND team2 != 'ITA'))`

7. Select the result that would be obtained from this code:

`SELECT teamname, COUNT(*)
  FROM eteam JOIN goal ON teamid = id
 GROUP BY teamname
HAVING COUNT(*) < 3`

Netherlands	2
Poland	2
Republic of Ireland	1
Ukraine	2

1. Select the statement which lists the unfortunate directors of the movies which have caused financial loses (gross < budget)

`SELECT name
  FROM actor INNER JOIN movie ON actor.id = director
 WHERE gross < budget`

2. Select the correct example of JOINing three tables

`SELECT *
  FROM actor JOIN casting ON actor.id = actorid
  JOIN movie ON movie.id = movieid`

3. Select the statement that shows the list of actors called 'John' by order of number of movies in which they acted

`SELECT name, COUNT(movieid)
  FROM casting JOIN actor ON actorid=actor.id
 WHERE name LIKE 'John %'
 GROUP BY name ORDER BY 2 DESC`

4. Select the result that would be obtained from the following code:

` SELECT title 
   FROM movie JOIN casting ON (movieid=movie.id)
              JOIN actor   ON (actorid=actor.id)
  WHERE name='Paul Hogan' AND ord = 1`

Table-B
----------------
"Crocodile" Dundee
Crocodile Dundee in Los Angeles
Flipper
Lightning Jack

5. Select the statement that lists all the actors that starred in movies directed by Ridley Scott who has id 351

`SELECT name
  FROM movie JOIN casting ON movie.id = movieid
  JOIN actor ON actor.id = actorid
WHERE ord = 1 AND director = 351`

6. There are two sensible ways to connect movie and actor. They are:

* link the director column in movies with the primary key in actor
* connect the primary keys of movie and actor via the casting table

7. Select the result that would be obtained from the following code:

` SELECT title, yr 
   FROM movie, casting, actor 
  WHERE name='Robert De Niro' AND movieid=movie.id AND actorid=actor.id AND ord = 3`

Table-B
---------
A Bronx Tale	1993
Bang the Drum Slowly	1973
Limitless	2011

### Using NULL Quiz 
1. Select the code which uses an outer join correctly.

` SELECT teacher.name, dept.name FROM teacher LEFT OUTER JOIN dept ON (teacher.dept = dept.id)`

2. Select the correct statement that shows the name of department which employs Cutflower -

` SELECT dept.name FROM teacher JOIN dept ON (dept.id = teacher.dept) WHERE teacher.name = 'Cutflower'`

3. Select out of following the code which uses a JOIN to show a list of all the departments and number of employed teachers

` SELECT dept.name, COUNT(teacher.name) FROM teacher RIGHT JOIN dept ON dept.id = teacher.dept GROUP BY dept.name`

4. Using `SELECT name, dept, COALESCE(dept, 0) AS result FROM teacher` on teacher table will:
display 0 in result column for all teachers without department
   
5. Query: 'four' for Throd

`SELECT name,
       CASE WHEN phone = 2752 THEN 'two'
            WHEN phone = 2753 THEN 'three'
            WHEN phone = 2754 THEN 'four'
            END AS digit
  FROM teacher`

6. Select the result that would be obtained from the following code:

` SELECT name, 
      CASE 
       WHEN dept 
        IN (1) 
        THEN 'Computing' 
       ELSE 'Other' 
      END 
  FROM teacher`

Table-A
-------------------
Shrivell	Computing
Throd	Computing
Splint	Computing
Spiregrain	Other
Cutflower	Other
Deadyawn	Other

### Self JOIN Quiz 
1. Select the code that would show it is possible to get from Craiglockhart to Haymarket

`SELECT DISTINCT a.name, b.name
  FROM stops a JOIN route z ON a.id=z.stop
  JOIN route y ON y.num = z.num
  JOIN stops b ON y.stop=b.id
 WHERE a.name='Craiglockhart' AND b.name ='Haymarket'`

2. Select the code that shows the stops that are on route.num '2A' which can be reached with one bus from Haymarket?

`SELECT S2.id, S2.name, R2.company, R2.num
  FROM stops S1, stops S2, route R1, route R2
 WHERE S1.name='Haymarket' AND S1.id=R1.stop
   AND R1.company=R2.company AND R1.num=R2.num
   AND R2.stop=S2.id AND R2.num='2A'`

3. Select the code that shows the services available from Tollcross?

`SELECT a.company, a.num, stopa.name, stopb.name
  FROM route a JOIN route b ON (a.company=b.company AND a.num=b.num)
  JOIN stops stopa ON (a.stop=stopa.id)
  JOIN stops stopb ON (b.stop=stopb.id)
 WHERE stopa.name='Tollcross'`
