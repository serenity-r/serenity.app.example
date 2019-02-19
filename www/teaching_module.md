---
output: 
  html_document: 
    keep_md: yes
---



<style type="text/css">
body, td {
   font-size: 16px;
}
</style>



# Investigating human-induced evolution in African elephants (Loxodonta africana)

Watch the HHMI BioInteractive video "Selection for Tuskless Elephants"

<iframe width="560" height="315" style="display: block; margin-left: auto; margin-right: auto;" src="https://www.youtube.com/embed/IxJDUrDH9v4" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
 
## How have other African elephant populations changed in response to poaching?

<div style="float: right; width: 300px; margin-left: 10px;">
<img src="Figure1.png"><br>
<span style="float: right">Figure 1. Photo from tusk harvest in the 1880s (Robert H Milligan, New York Public Library)</span>
</div>
As Joyce Poole mentioned, there are many other populations of elephants on the African continent and they may have also experienced similar selective pressures due to poaching as the populations in Gorongosa.To further investigate these effects, we will examine data comparing historical and contemporary populations of elephants along the Kenya-Tanzania border. The populations in Kenya and Tanzania are among the largest on the African continent, according to a recent census by Chase et al. (2016) (Table 1). Similar to the Gorogosa populations, these populations have experienced poaching and have suffered recent population declines (Chase et al. 2016).<br><br>
<table class="table table-bordered" style="width: auto !important; float: left; margin-right: 10px;">
<caption>Table 1. Estimated elephant populations from the Great Elephant Census (GEC)</caption>
 <thead>
  <tr>
   <th style="text-align:left;background-color: #EEE;"> Country </th>
   <th style="text-align:left;background-color: #EEE;"> Elephants </th>
   <th style="text-align:left;background-color: #EEE;"> SE </th>
   <th style="text-align:left;background-color: #EEE;"> 95% CI </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;background-color: white;"> Angola </td>
   <td style="text-align:left;background-color: white;"> 3,395 </td>
   <td style="text-align:left;background-color: white;"> 797 </td>
   <td style="text-align:left;background-color: white;"> 1,778-5,012 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Botswana </td>
   <td style="text-align:left;background-color: white;"> 130,451 </td>
   <td style="text-align:left;background-color: white;"> 6,378 </td>
   <td style="text-align:left;background-color: white;"> 116,957-142,043 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Cameroon </td>
   <td style="text-align:left;background-color: white;"> 148 </td>
   <td style="text-align:left;background-color: white;"> 84 </td>
   <td style="text-align:left;background-color: white;"> 12-313 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Chad </td>
   <td style="text-align:left;background-color: white;"> 743 </td>
   <td style="text-align:left;background-color: white;"> 0 </td>
   <td style="text-align:left;background-color: white;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> DR Congo </td>
   <td style="text-align:left;background-color: white;"> 1,959 </td>
   <td style="text-align:left;background-color: white;"> 150 </td>
   <td style="text-align:left;background-color: white;"> 1,773-2,254 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Ethiopia </td>
   <td style="text-align:left;background-color: white;"> 799 </td>
   <td style="text-align:left;background-color: white;"> 0 </td>
   <td style="text-align:left;background-color: white;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Kenya </td>
   <td style="text-align:left;background-color: white;"> 25,959 </td>
   <td style="text-align:left;background-color: white;"> 1,805 </td>
   <td style="text-align:left;background-color: white;"> 22,421-29,497 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Malawi </td>
   <td style="text-align:left;background-color: white;"> 817 </td>
   <td style="text-align:left;background-color: white;"> 0 </td>
   <td style="text-align:left;background-color: white;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Mali </td>
   <td style="text-align:left;background-color: white;"> 253 </td>
   <td style="text-align:left;background-color: white;"> 0 </td>
   <td style="text-align:left;background-color: white;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Mozambique </td>
   <td style="text-align:left;background-color: white;"> 9,605 </td>
   <td style="text-align:left;background-color: white;"> 1,018 </td>
   <td style="text-align:left;background-color: white;"> 7,610-11,600 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> South Africa </td>
   <td style="text-align:left;background-color: white;"> 17,433 </td>
   <td style="text-align:left;background-color: white;"> 0 </td>
   <td style="text-align:left;background-color: white;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Tanzania </td>
   <td style="text-align:left;background-color: white;"> 42,871 </td>
   <td style="text-align:left;background-color: white;"> 3,102 </td>
   <td style="text-align:left;background-color: white;"> 36,792-48,950 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Uganda </td>
   <td style="text-align:left;background-color: white;"> 4,864 </td>
   <td style="text-align:left;background-color: white;"> 1,031 </td>
   <td style="text-align:left;background-color: white;"> 2,843-6,885 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> W. Africa </td>
   <td style="text-align:left;background-color: white;"> 8,911 </td>
   <td style="text-align:left;background-color: white;"> 1,299 </td>
   <td style="text-align:left;background-color: white;"> 6,366-11,457 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Zambia </td>
   <td style="text-align:left;background-color: white;"> 21,759 </td>
   <td style="text-align:left;background-color: white;"> 2,310 </td>
   <td style="text-align:left;background-color: white;"> 17,232-26,286 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Zimbabwe </td>
   <td style="text-align:left;background-color: white;"> 82,304 </td>
   <td style="text-align:left;background-color: white;"> 4,382 </td>
   <td style="text-align:left;background-color: white;"> 73,715-90,893 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;background-color: white;"> TOTAL </td>
   <td style="text-align:left;font-weight: bold;background-color: white;"> 352,271 </td>
   <td style="text-align:left;font-weight: bold;background-color: white;"> 9,085 </td>
   <td style="text-align:left;font-weight: bold;background-color: white;"> 334,464-370,078 </td>
  </tr>
</tbody>
</table>
<br><br><br><br>
Chiyo et al. (2015) investigated changes in populations of Loxodonta africana located along the Kenya-Tanzania border between the 1960s and the 2000s.    Rather than observing tusklessness like Dr. Poole, Chiyo et al. (2015) examined evidence of subtler changes in tusk morphology over time due to poaching.  They used Historical records to collect height (estimated to the shoulder), tusk length, and tusk circumference from approximately 600 elephants that were culled in Tsavo East National Park (Kenya) and Mkomazi National Park (Tanzania) between 1966 and 1968.  Figure 2 is an example of the historical records used in the study (for more information see: http://ufdc.ufl.edu/AA00013409/00007).  Contemporary data were collected from live animals that were translocated from southern Kenya to Tsavo East National Park (2005), Masai Mara National Reserve (2011), and Meru National Park (2013).<br><br>
<div style="float: right; width: 400px; margin-left: 10px; margin-top: 10px;">
<img src="Figure2.png"><br>
<span style="float: right">Figure 2. Example of historical records of elephant morphology used by Chiyo <emph>et al.</emph> (2015)</span>
</div>
<div style="clear:both;"></div>

## References

Chase, M. J., S. Schlossberg, C. R. Griffin, P. J. C. Bouché, S. W. Djene, P. W. Elkan, S. Ferriera, F. Grossman, E. M. Kohi, K. Landen, P. Omondi, A. Peltier, S. A. F. Selier, and R. Sutcliffe.  2016. Continent-wide survey reveals massive decline in Africa savannah elephants.  PeerJ, 4:e2354. [doi: 10.7717/peerj.2354](https://doi.org/10.7717/peerj.2354)

Chiyo, P. I., V. Obanda, and D. K. Korir. 2015. Illegal tusk harvest and the decline of tusk size in the African elephant.  Ecology and Evolution, 5 (22): 5216-5229. [doi: 10.1002/ece3.1769](https://doi.org/10.1002/ece3.1769)

Chiyo PI, Obanda V, Korir DK. 2015. Data from: Illegal tusk harvest and the decline of tusk size in the African elephant. Dryad Digital Repository. [doi: 10.5061/dryad.h6t7j](https://doi.org/10.5061/dryad.h6t7j)
