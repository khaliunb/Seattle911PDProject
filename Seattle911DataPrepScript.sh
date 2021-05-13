#!/bin/bash

#####################################################################
#Ubuntu command line code in this file uses bash script commands to prepare
# trimmed data file from original Seattle_Police_Department_911_Incident_Response.csv file
# It is not necessary for you to run this script.
# This is because github is not allowing me to upload file larger than 100MB
# Therefore I am trimming the data, but doing so, trying to represent
# the original data as much as possible.
# I am using "shuf" command.
#   (!) Note 1: If your environment has no shuf command, the code could not run.
#   (!) Note 2: From R Studio please run this code in the Terminal,
#   not in Console. Script is executable. Therefore you can use
#   # ./Seattle911DataPrepScript.sh
#   (!) Note 3: I have already downloaded the original file.
# Commented by Khaliun.B 2021.05.12
#####################################################################

#Creating the data directory
mkdir data
cd data/
#We are assigning the default values for options variables
DOWDIR="~/Downloads/archive/Seattle_Police_Department_911_Incident_Response.csv";
RESSIZE_MB=80;
RESDIR="SeattlePD911IR_"$RESSIZE_MB"_MB.csv"
RESZIP="SeattlePD911IR_"$RESSIZE_MB"_MB.zip"
HEADDIR="HeadS911PD.csv"
#We are also assigning the default values for temp variables to be used for the script
now=$(date +'%Y%m%d%I%M%S');
TEMPLOGFILE="temp"$now".log";

# These are options, you can change the downloaded file path using -d option
# and total number of MBs you want to get with -s option.But remember, the total size you want,
# should not exceed the actual size of the original file.
# You can also change the resulting file directory and file name with -r option
while getopts d:s:r: option
do case "${option}"
in
d) DOWDIR=($OPTARG);;
s) RESSIZE_MB+=($OPTARG);;
r) RESDIR+=($OPTARG);;
esac
done

#First, we are getting the statistics for original file and storing the results in temporary file
echo $DOWDIR;
wc "$DOWDIR">> $TEMPLOGFILE;

#Next, we are getting the total number of lines from the temporary file and calculating total number of lines necessary
TOTMBDIV=`awk -F" " -v a="$RESSIZE_MB" '{print int($3/(a*1024*1024))}' $TEMPLOGFILE`
TOTSHUFROW=`awk -F" " -v b="$TOTMBDIV" '{print int($1/b)}' $TEMPLOGFILE`
#We are removing the temporary file which has no further use
rm $TEMPLOGFILE
#We are also removing the previous version of result file. Please be careful with this
rm $RESDIR
#Finally, we are shuffling through the original file and picking random lines of total number of necessary rows
#Result is not the exact size, it may actually exceed the intended result,
#but it will be approximation and the data will be represented nicely
echo "Sampling "$TOTSHUFROW" number of rows from file "$DOWDIR". Results will be stored in file: "$RESDIR
awk -F"," '{print $2}' $HEADDIR | tr "\n" "," > $RESDIR
shuf -n $TOTSHUFROW $DOWDIR >> $RESDIR
#zip the resulting file
echo "Zipping file: "$RESDIR" into "$RESZIP
zip $RESZIP *
rm $RESDIR