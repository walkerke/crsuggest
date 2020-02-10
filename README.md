# crsuggest

{crsuggest} is a small R package to help spatial analysts determine an appropriate projected coordinate reference system for their data.  It implements one core function, `suggest_crs()` that attempts to match an input spatial dataset with corresponding coordinate reference systems that will work well for mapping and/or spatial analysis.  

Consider the following common use cases in R:

### Use case 1: Buffering unprojected points

Let's say you've obtained a points dataset in a geographic coordinate system and you'd like to draw buffers around those points:

```r

```
