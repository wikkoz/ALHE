#!/bin/sh
R CMD BATCH --slave ./alhe1.R
mv 1\ test.png 1test_$(date +%F-%T).png
mv 2\ test.png 2test_$(date +%F-%T).png
mv 3\ test.png 3test_$(date +%F-%T).png
mv 4\ test.png 4test_$(date +%F-%T).png
exit 0