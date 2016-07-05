PiC Data Science Challenge
==========================

This is a rough outline of the methodology and approach used in completing the data science challenge.

## First steps

- Removed the obviously unnecessary/irrelevant columns from CSV file manually: dates of application stages, other things that appeared to be more administrative than useful.
- Take various complex strings to booleans etc
- Nicer column names

## Plot ideas

- Sankey diagram! Nice way to represent percentages of applicants getting through.

---

# Postmortem

This took quite a long time - I spent a lot of time just playing with the data, and also getting the plots just how I wanted. I'm really pleased with the alluvial plots.

### Ideas for Further Work

- Would be nice to look at *when* candidates withdrew from the process - I removed the relevant field early in the process and didn't realise until late on. This could hopefully give insights into what elements of the recruitment "scared off" some candidates.
- There's probably more scope for doing stuff with the continuous data: maybe some scatter plots with the various indicators (including the 2 lower-level ones). That would be pretty much copying what I was showed last week, though.
- I'd like to do some more geographical stuff: a cartogram of some sort with applicant hometowns etc would be fun.
- Better stats: obviously, there's a world of force-directed graphs and k-d trees and machine learning out there
- **One I really wish I'd done**: comparing the scores on the online tests to the PiC achievement score etc!
- Maybe could do something with data from elsewhere: particularly I'd quite like to see something done with university league tables, as arbitrary as they are.

### Mistakes
- Took too long perfecting plots, as per. Furthermore, my love for monochrome plots didn't really work well here - the report looks rather boring. Could be argued that this is more "businessy", though.
- Was **R** the right choice? I do like a lot of its features but it can be a pain to get data in the right form.

### Problems Encountered
- I really wasn't sure what to do with this "in process after role closed" field... I felt that those rows had to be excluded as we didn't know what had happened with them yet.
- Dataset was quite small - once you've got rid of the rubbish there's not so much left, although I still managed to get some statistical significance.
- Hard to do this and the full-stack work in < a week!