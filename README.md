# Times

US Sprint Canoe and Kayak Results app

## To update results

1. Navigate to the Results repository
2. Create a folder for the regatta or event and put files there
3. Copy one of the csv files in the `Results/Scrapped Data Files` directory to use as a template.
4. Give the file a short but distinctive name. See other files for examples.
5. Fill in results. Results can be partial. Only some paddlers or only some events. Add "partial" to file name if results are partial.
6. Enter the name in the results exactly as used in the official results. So if name in results "Blow, Joe" for "Joseph Blow" then enter "Blow, Joe" in the results csv.

## Update the paddlers

1. Open the `Results/Scrapped Data Files/nameresolver.csv` file and check that any paddlers added in the new results also appear in this file. If the paddler was racing in 2019 they should already be here. Decide on the 'official name' of the paddler and use that in the name column. Add a new row with each alternate name that appears in the results. So `name=Joseph Blow` and `alt.name=Blow, Joe`.
2. Open the `Results/Scrapped Data Files/paddlers` directory and add any paddlers in the new results that are missing. This can be partial and having a paddler appear multiple places is fine. This show use the official name (so Joseph Blow). You can use NA for unknown information but having the birth year is very helpful.

## For the person updating the application

If new files scrapped

* run `bindandclean.R` in Scrapped Data Files
* Then source `update_mydata.R`

Takes awhile as it does a bit of clean up line by line and gets the birth year. This will update `fullresults.csv`

Once it is done, the app can be updated on shiny.io via the rsconnect feature in RStudio.



