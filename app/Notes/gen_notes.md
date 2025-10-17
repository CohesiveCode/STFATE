# General Notes 

The purpose of this document is to hold general notes about the model and how it works


![Diagram of Descending Hemisphere Cloud](image.png)


I'm confused on how the system evolves. Well either way I'm going to need initial conditions so let's focus on that


Meaning of values in E in DerivD

* E(1) - x Location of the barge. 
    * Coordinate system referenced to barge location. I think this is kind of dumb since the location of the barge moves. Would be better to keep track of the velocity and the barge in global coordinates once the initial location is determined. 

* E(2) - Vertical location of the clouds centroid

* E(3) - z location of the cloud.
    * Coordinate system referenced to barge location.

* E(4) - Mass of the cloud? Entrainment volume * ambient density
* E(5) - x-momentum Check this
* E(6) - z-momentum Check this
* E(7) - Rate of z momentum change?? Check this
* E(8) - Initial Buoyancy. Used in VOLUME=(E(4)+E(8))/ROA(1)
    * I don't get what the purpose 

* E(9) - Vorticity
* E(10) - Stores the volume of class i of solids material. That goes for the rest of these.
* E(11) - 
* E(12) - 
* E(13) - 
* E(14) - 
* E(15) - 
* E(16) - 
* E(17) - 
* E(18) - 
* E(19) - 
* E(20) - 
* E(21) - 
* E(22) - 
