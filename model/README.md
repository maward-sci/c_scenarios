# Model Overview:
## Blue Carbon Restoration Scenarios
We simulate carbon dynamics across a range of methods that represent real-world approaches to seagrass restoration. Our restoration methods are: `Infill`, `Seeding Restoration`, `Transplant Restoration`. We also simulate a `Baseline` scenario.

## Parameters
We simulate:

    1. `methane`
    1. `nitrous_oxide`
    1. `biomass`
    1. `soil`

See `scen_generation::model_params` for the parameter values for each of the above constiuents.

## Assumptions
**Terms**:

    1. carbon constituents: `methane`, `nitrous_oxide`, `biomass`, and `soil`

**Growth of seagress meadows**:

    1. The area of seagrass meadows grows over time as a `logistic` curve, meaning growth is initially somewhat slower, accelerates for a period, and then tapers off, or saturates, at some level that represents that maximum potential area for the restored seagrass meadow.
    1. The logistic growth curve for seagrass meadows depends on the restoration scenario.
    1. `Baseline` simulations for each restoration method grow in area at the same rate as their respective restoration scenario; differences in carbon dynamics are driven solely by the fact that `Baseline` scenarios are comprised of unvegetated habitat.
        1. This assumption is made to facilitate comparison among the restoration method and the baseline. The idea is that as the restored area grows, the amount of unrestored habitat that it represents also grows. That means that to compare the growing, let's say, carbon sequstration rates of restored seagrass meadows to those of an unrestored area half the size would be to overstate the net gain of the restoration. So this assumption lets us avoid that bias.

**Carbon dynamics**:

    1. Carbon constituents are a vary between vegetated (`Infill`, `Seeding Restoration`, and `Transplant Restoration`) and unvegetated (`Baseline`) habitats.
        - Within vegetated habitats, differences in carbon constituent dynamics are driven solely by variation in the amount of vegetated habitat.
    1. carbon constituents are a function of `area` and their per-area values are drawn from a normal distribution with a mean and standard deviation that are derived from the literature (see `Citations` and `Parameter Estimation` sections (once they exist)).
        1. The parameter distribution changes across vegetated and unvegetated habitats.


## Codebase

### Docstrings
**Docstring Template**
```

# DOCSTRINGS TEMPLATE
  #' Paste two items
  #' 
  #' @description This function pastes two items
  #' together.  
  #'
  #' By using the description tag you'll notice that I
  #' can have multiple paragraphs in the description section
  #' 
  #' @param x character. The first item to paste
  #' @param y character. The second item to paste Defaults to "!" but
  #' "?" would be pretty great too
  #' @usage mypaste(x, y)
  #' @return The inputs pasted together as a character string.
  #' @details The inputs can be anything that can be input into
  #' the paste function.
  #' @note And here is a note. Isn't it nice?
  #' @section I Must Warn You:
  #' The reference provided is a good read.
  #' \subsection{Other warning}{
  #'   It is completely irrelevant to this function though.
  #' }
  #' 
  #' @references Tufte, E. R. (2001). The visual display of 
  #' quantitative information. Cheshire, Conn: Graphics Press.
  #' @examples
  #' mypaste(1, 3)
  #' mypaste("hey", "you")
  #' mypaste("single param")
  #' @export
  #' @importFrom base paste
````