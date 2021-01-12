# RShiny_Robomussels

Rshiny_Robomussels is an interactive shiny app that allows any user to visualize the operative temperatures of mussels on a microclimatic scale. The data are collected by ...
and it emphasizes the importance of adjusting the temporal and spatial scale of data for the focal organisms. 

## Prerequisites for opening in Rstudio
Git and Rstudio ([Instructions](https://resources.github.com/whitepapers/github-and-rstudio/))  
Installation of the following R packages:
shiny, leaflet, dplyr, reshape2, tidyr, ggplot2

```
pkgs <- c("shiny", "leaflet", "dplyr", "reshape2", "tidyr", "ggplot2")
lapply(pkgs, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)
```

## Using RShiny_Robomussels
* Opening in Rstudio:  
Click on "Code" on the top right to copy the link to this repository.  
Click ```File```, ```New Project```, ```Version Control```, ```Git```  
Paste the repository URL and click ```Create Project```.

* Alternatively, go to [this link](https://huckley.shinyapps.io/ClimateBiology/).

We have a google doc with questions to guide through the app for further understanding of the topic.

## Contributing to RShiny_Robomussels
<!--- If your README is long or you have some specific process or steps you want contributors to follow, consider creating a separate CONTRIBUTING.md file--->
To contribute to RShiny_Robomussels, follow these steps:

1. Fork this repository.
2. Create a branch: `git checkout -b <branch_name>`.
3. Make your changes and commit them: `git commit -m '<commit_message>'`
4. Push to the original branch: `git push origin <project_name>/<location>`
5. Create the pull request.

Alternatively see the GitHub documentation on [creating a pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request).
