library(shiny)
library(ggplot2)
theme_set(theme_bw())
library(insol) #for degrees, radians

# source("app.R"); shinyApp(ui = ui, server = server)
# runApp()

# deploy
# library(rsconnect)
# deployApp("sverdrup")

# options(shiny.error = browser)

# Io = total incoming energy from the sun and the sky, g cal cm^-2 h^-1
# g cal = "gram calorie" = cal
# 1 g cal = 4.183 J

# Mosby's formula:
# Io_bar = 0.026*(1-0.075*C_bar)*h_bar
# C_bar = average cloudiness on the scale 0 to 10
# h_bar = average altitude of the sun
# Iw = energy that passes through the sea surface
# Iw = (1-a)*Io
# a = fraction lost by reflection from sea surface
# a depends upon the altitude of the sun:

# solar radiation cloud cover adjustment
# http://www.shodor.org/os411/courses/_master/tools/calculators/solarrad/
# R = Ro*(1-0.75*(C_bar/100)^3.4)

# sun.altitude = c(5, 10, 20, 30, 40, 50)
# fraction.reflected = a = c(.4, .25, .12, .06, .04, .03)
# a = 0.0271119 + 0.6000495*e^(-0.09642005 * sun.altitude)

# only 20 percent of energy that passes the sea surface due to absorption in the upper one meter (citation?)
# Ie = effect energy that passes the sea surface
# Ie = 0.2*Iw
# k = coefficient of extinction
# k = 0.075 to 0.1
# Iz = Ie*e^(k*z) = 0.2*(1-a)*Io*e^(k*z)
# p(z) = production at depth z, proportional to energy
# r = destruction
# dp = m*Iz*dt
# dr = n*dt
# m and n are factors which are independent of z, but depend upon the character of the population and temperature of the water
# by definition, dp=dr at the compensation depth where Iz=Ic
# therefore n/m = Ic

# in the time T the total production by photosynthesis between the surface and depth z = -D is
# P = m* integral(0,T, integral(-D, 0, Ie*e^(k*z) dt*dz))
# P = m/k*(1 - e^(-k*D))*integral(0,T, Ie*dt)
# R = n*integral(0,T,integral(-D,0,dd*dt)) = n*T*D

# the condition for an increase of the phytoplankton population is
# P > R

# Ie_bar = 0.2/T * integral(0,T, (1-a)*Io*dt)
# remember n/m = Ic
# Dcr = critical depth
# Dcr/(1 - e^(-k*Dcr)) = 1/k * Ie_bar/Ic

# Dcr/Dc = e^(k*Dc)/(k*Dc)

# Jenkin found Ic = 0.13 g cal cm^-2 h^-1 = 0.55 J = 360 lux

########### START WEBSITE
# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Sverdrup's Critical Depth Hypothesis"),

    fluidRow(
    column(
     tabsetPanel(type = "tabs", id= "options", 
      tabPanel("Parameters", value="parameters",
#		  sliderInput("latitude", "Latitude (deg)", min = -90, max = 90, value = 66),
		  sliderInput("day", "Day of Year", min=1, max=365, value=30, animate=T),
#		  sliderInput("hour", "Hour of Day", min=0, max=23, value=12),
		  sliderInput("C_bar", "Cloud cover (%)", min = 0, max = 100, value = 50, step=5),
		  sliderInput("Ic", "Compensation irradiance", min = 0, max = 1, value = 0.15, step=0.01), #(g cal cm^-2 h^-1)
# 		  sliderInput("k", "Coefficient of extinction in seawater", min = 0.05, max = 0.15, value = c(0.075,0.1), step=0.005),
		  sliderInput("k", "Coefficient of extinction in seawater", min = 0.00, max = 0.15, value = c(0.075,0.075), step=0.005),
# 		  sliderInput("m", "fudge factor m", min = 1, max = 100, value = 1, step=1),
		  sliderInput("max.depth", "Maximum depth", min = 10, max = 1000, value = 400, step=10),
 		  sliderInput("D", "Mixed Layer Thickness", min = 0, max = 200, value = 100, step=10)

      )
	), width=3, class="well"
    ),

    column(
     tabsetPanel(type = "tabs", id = "tabs",
        tabPanel("Sverdrup 1953", value="sverdrup", 
             div(style = "height:1000px;", plotOutput("sverdrup", hover = "sverdrup_hover", click = "sverdrup_click", dblclick="sverdrup_dblclick"))
        )        
     ),width=6, height=12, class="well"
    ),
	column(
		div(tableOutput("rhtable"), style = "font-size:100%")	
	,width=3, class="well")
  )
)



# Define server logic
server <- function(input, output, session) {
	
    ###############
    # Sverdrup 1953 plot, Weathership M
    ###############
    output$sverdrup <- renderPlot({


z = -1*seq(0,input$max.depth,1)

s.day = input$day
# s.day = 120
# s.hour = 12
# s.latitude = -90:90

s.hour = 0:23
s.latitude = 66

s.delta = radians(23.45 * sin(radians(360 / 365 * (284 + s.day)))) #declination in radians
s.omega = radians(15*(s.hour-12)) #hour angle in radians
s.phi = radians(s.latitude) #the latitude in radians
h_bar = asin(cos(s.delta)*cos(s.phi)*cos(s.omega) + sin(s.delta)*sin(s.phi))
h_bar[h_bar<0] = 0
h_bar = degrees(mean(h_bar)) #degrees

C_bar = input$C_bar/10
Io_bar = 0.026*(1-0.075*C_bar)*h_bar #in g cal cm^-2 min^-1
Io_bar = 60 * Io_bar #in g cal cm^-2 h^-1

#print(h_bar)
#print(Io_bar)
#Io_bar = Ro*(1-0.75*(input$C_bar/100)^3.4) #for generalizing

a = 0.0271119 + 0.6000495*exp(-0.09642005*h_bar)
Iw = (1-a)*Io_bar
Ie = 0.2*Iw

k.min = input$k[1]
k.max = input$k[2]

Iz.min = Ie*exp(k.min*z)
Iz.max = Ie*exp(k.max*z)

#n/m = Ic
Ic = input$Ic
# m = input$m
m = 1
n = Ic * m

dp.min = m * Iz.min
# print(cumsum(dp.min))
dp.max = m * Iz.max
dr = rep(n,length(z))

# print(cumsum(dr))
# print(cumsum(dp.min)-cumsum(dr))

#P = m/k.min*(1 - exp(-1*k.min*z)) * Ie * T
#R = n*T*z

# set P=R
# rearrange
# Dcr/(1-exp(-1*k.min*Dcr)) = 0.2*(1-a)*Io_bar/Ic * 1/k
# numerically solve for value of Dcr that minimizes difference between lhs and rhs

# lhs.min = z/(1-exp(-1*k.min*z))
# lhs.max = z/(1-exp(-1*k.max*z))
# print(lhs.min)
# 
# rhs.min = 0.2*(1-a)*Io_bar/k.min/Ic
# rhs.max = 0.2*(1-a)*Io_bar/k.max/Ic
# print(rhs.min)
# 
# print(lhs.min - rhs.min)
# 
# Dcr.min = z[which.min(abs(lhs.min - rhs.min))]
# Dcr.max = z[which.min(abs(lhs.max - rhs.max))]

# Critical Depth
Dcr.min = z[which.min(abs(cumsum(dp.min)-cumsum(dr)))]
Dcr.max = z[which.min(abs(cumsum(dp.max)-cumsum(dr)))]

# Compensation Depth
Dc.min = z[which.min(abs(dp.min-dr))]
Dc.max = z[which.min(abs(dp.max-dr))]



# print(Dcr.min)
# print(Dcr.max)

par(mfrow=c(1,2))
plot(Iz.min, z, xlab="Irradiance (Iz)", main="Irradiance", ylab="depth (z)", xlim=c(0,5))
points(Iz.max, z)

# plot(rhs.min - lhs.min, z, col="black")

plot(dp.min, z, xlab="Productivity (dp) or Respiration (dr)", main="Productivity or Respiration", ylab="depth (z)", col="purple",type="l", cex=2, lwd=2)
lines(dp.max,z,col="purple", cex=2, lwd=2)
lines(dr, z, col="red", cex=2, lwd=2)
min.x = min(dp.min)
max.x = max(dp.max)

lines(c(-10,10),c(-1*input$D,-1*input$D),col="black", lwd=2)
text(max.x, -1*input$D - 0.015*min(z),"Mixed Depth", pos=2, col="black")

lines(c(min.x,max.x,max.x,min.x,min.x),c(Dcr.min,Dcr.min,Dcr.max,Dcr.max,Dcr.min), col="grey", cex=2, lwd=2)
text(max.x, Dcr.min - 0.015*min(z),"Critical Depth", pos=2, col="grey")

lines(c(min.x, max.x),c(Dc.min, Dc.max), col="green", cex=2, lwd=2)
text(max.x, Dc.min - 0.015*min(z),"Compensation Depth", pos=2, col="green")

if(Dcr.min < -1*input$D) text(max.x/2, min(z), "BLOOM!", pos=3, cex=4, col="darkgreen")
 
    }, width=600, height=600)

    ###############
    # data table
    ###############
      output$rhtable <- renderPrint({
      cat("<br>Purple = Primary Productivity",
      "<br><br>Red = Respiration (loss)",
      "<br><br>Gray = Critical Depth",
      "<br><br>Black = Mixed Layer Thickness",
      "<br><br>Green = Compensation Depth",
      "<br><br>In this model, a phytoplankton bloom is possible when the Mixed Layer Depth is shallower than the Critical Depth",
      "<br><br><a href=\"http://www.stccmop.org/files/u152/Sverdrup_1953.pdf\">[PDF] On Conditions for the Vernal Blooming of Phytoplankton</a>",
      sep="\n")
      })



}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
