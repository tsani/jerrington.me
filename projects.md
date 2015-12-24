---------------
title: Projects
---------------

Vent over Tea
-------------

Dan Crisan and I built and maintain the
[Vent over Tea](http://ventovertea.com/)
web site together.

Publications
------------

### The Great Migration and African-American genomic diversity

In 2014 as a research assistant in the lab of
[Simon Gravel](http://simongravel.lab.mcgill.ca/Home.html),
I contributed to [tracts](http://github.com/sgravel/tracts),
which is a tool for modelling local ancestry patterns along the genome.
Simply put, given a model of migration and the ancestry proportions of
individuals in a present population, tracts evaluates the likelihood of the
migration described by that model having occurred.

Tracts had an issue when presented with large populations that would arise due
to an averaging of the present population's ancestry proportions. I resolved
this by implementing a new demographic model in tracts that performs a model
evaluation on multiple subpopulations and combines the results of those
evaluations. By splitting the population in this way, tracts can avoid
discarding the information given by the variance in the present population's
ancestry proportions.

This new evaluation method was used to produce some of the results in the paper
_The Great Migration and African-American genomic diversity_ (publication
pending), for which I am a coauthor.
