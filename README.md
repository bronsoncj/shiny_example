# shiny_example

This project created a barebones web application made for looking at housing data in King County, WA, USA.
The main feature was building interactive maps (in the form of built in R mapping tools, and using shapefiles).
The intention at this point in the code was to make sure everything worked, so functionality is limited.

The code is broken down into two parts: ui and server.

The ui code is on the top half of the page, the server code is on the bottom half of the page.

The user can filter by things like price, and decade the houses were built etc. The maps handle the breakdown of price
over time by focusing on zipcode and long/lat coordinates of the individual houses.
