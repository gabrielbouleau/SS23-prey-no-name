# SS23-prey-no-name

Contributors: 

- [Gabriel Bergeron](https://github.com/gabrielbouleau)
- [Emmanuelle Barreau](https://www.researchgate.net/profile/Emmanuelle-Barreau)
- [Francois Thibault]()
- [Irene Roca](https://www.researchgate.net/profile/Irene-Roca)
- [Matthieu Weiss-Blais]()
- [Mélisande Teng]()

### Subject
Advanced Field School in Computational Ecology 2023 / Projet by "Prey with no name team / Prey functional response, sociality, networks and stuff.

## Contribution

To contribute to this repository, clone it on your computer. During the working groupe, makes shure to puch and pull your changes regularly. If you work on major changes, fork the main branch and consult with the team before merging.

## Repository directories & files

Please follow the following conventions:
- Name each directory so that anyone can easily get how it is organized (ex: data_raw, data_cleaned, fct)
- Write functions in distinct files names function_fct.r
- Name folder with explicit names (ex: prey_detection_lenght.r)

## Code and naming convention

- Comment your code with relevent 
- If you use a function from a package, please use the 'package::function' call to keep track of the different packages
- In the head of you code, please list all packages necessary for the analysis
```{r}
# the chunk of code below can be run to load all packages and install those that are not already installed
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}
using("packages names")
```


# Have fun !
