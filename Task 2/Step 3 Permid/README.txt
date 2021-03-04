# Written by: Fotis Papailias, 2020
# (c) European Commission, Eurostat
# Please do NOT use/reproduce without citing!
#
# Supply of statistical services in statistical methodology 
# Lot 1: Methodological support
# Smart data for multinational enterprises (MNEs)
# Framework Contract No 2018.0086
# Ref. No.: ESTATMET2-000050-6000054239-REQ-01

Files:

PermID.csv: contains all the company ID's which are used to webscrape the data.


MainWebscraper.py:
Tested on Mac - Python: 3.6.8 |Anaconda, Inc.| (default, Dec 29 2018, 19:04:46)
[GCC 4.2.1 Compatible Clang 4.0.1 (tags/RELEASE_401/final)]

*IMPORTANT: It makes use of ChromeDriver which expects you to have Chrome installed in the default location for your platform.

* Default On Mac OS it's /usr/local/bin.

*python code which  webscrapes the data from PermID website and delivers it in the desired format. 

*stack_output  = True: delivers a unique csv with all companies's info.
