# NOTE: The code currently will not run due to intentionally missing files. For client confidentiality, the .csv data file as well as the proprietary font have not been added to this github. If you would like to run this code, please reach out to me at karl+gh@themeyers.org and I will provide anonymized data.

## Guitar Chart Graph Example
This is sample code for a graph created custom for a client. 
The file "AnnualGuitarCharts5.17.23.R" contains the data import, cleaning, and graph creation using ggplot2.

The goal was to compare important factors over a large number (16) of different geographic areas.
The format of a dotplot was chosen as the best way to do so, with color showing the different geographic areas.
Originally this was done with a scatterplot, but the downside was that individual points were difficult to make out due to overlapping.

I then created a dotplot variant, which is a ggplot graph that has dot binning and stacking built in. The result is less clean, but much easier to read.

The graph became part of a report delivered to the client with broader analysis. The client specifically requested a cleaner style than default ggplot provides, so I customized the graph to remove much of the lables and gridlines.
