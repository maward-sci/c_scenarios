# Model Overview:
## Blue Carbon Restoration Scenarios
We simulate carbon dynamics across a range of methods that represent real-world approaches to seagrass restoration. Our restoration methods are: `Infill`, `Seeding Restoration`, `Transplant Restoration`. We also simulate a `Baseline` scenario.

### Parameters
We simulate:

1. `methane`
1. `nitrous_oxide`
1. `biomass`
1. `soil`

See `scen_generation::model_params` for the parameter values for each of the above constiuents.


---
## Model Specification
### *Full Model*
$$
C_{\mathrm total, t} = C_{\mathrm sed,t} + C_{\mathrm biomass, t} - (C_{\mathrm methane, t} + C_{\mathrm nox, t})
$$

where:

$C_t$    : Total Carbon Sequestration $(grams / year)$ at year $t$

and:

$C_{\mathrm sed,t}$: Sediment Carbon $(grams / year)$ at year $t$

$C_{\mathrm biomass, t}$:  Biomass Carbon $(grams / year)$ at year $t$

$C_{\mathrm methane, t}$:  Methane Carbon $(grams / year)$ at year $t$

$C_{\mathrm nox, t}$: Nitrous Oxide Carbon $(grams / year)$ at year $t$

----

The indvidual constituents are defined below.

---
### *Sediment*
$$
C_{\mathrm sed,t} = (A_{\mathrm veg,t} * \rho_{\mathrm veg} * d_{\mathrm acc,t}) + (A_{\mathrm unveg,t} * \rho_{\mathrm unveg} * d_{\mathrm acc,t}) + (A_{\mathrm infill,t} * \rho_{\mathrm infill} * p_{\mathrm remin} * d_{\mathrm infill,t})
$$

where:

$C_{\mathrm sed,t}$: Sediment Carbon at year t $(grams / year)$

and:

$A_{\mathrm veg,t}=$ Area of vegetated habitat $(m^{2})$ at time $t$

$A_{\mathrm unveg,t}=$ Area of unvegetated habitat $(m^{2})$ at time $t$

$A_{\mathrm infill,t}=$ Area of infill $(m^{2})$ at time $t$

$\rho_{\mathrm veg}=$ Density of carbon in vegetated habitat $(grams / m^{3})$

$\rho_{\mathrm unveg}=$ Density of carbon in unvegetated habitat $(grams / m^{3})$

$\rho_{\mathrm infill}=$ Density of carbon in infill sediment $(grams / m^{3})$

$d_{\mathrm acc,t}=$ Depth of sediment accretion $(m)$ at time $t$ 

$d_{\mathrm infill,t}=$ Depth of infill sediment applied $(m)$ at time $t$ 

----
### *Biomass*
$$
C_{\mathrm biomass,t} = (A_{\mathrm veg,t} * \delta_{\mathrm biomass})
$$

where:

$C_{\mathrm biomass,t}$: Carbon content from biomass $(grams / year)$ at year $t$ 

and:

$A_{\mathrm veg,t}=$ Area of vegetated habitat $(m^{2})$ at time $t$

$\delta_{\mathrm biomass}=$ Density of biomass Carbon $(grams / m^{2})$ at time $t$

----
### *Methane*
$$
C_{\mathrm methane,t} = (A_{\mathrm veg,t} * \delta_{\mathrm methane, veg}) + (A_{\mathrm unveg,t} * \delta_{\mathrm methane, unveg}) + (A_{\mathrm infill,t} * \delta_{\mathrm methane, infill})
$$

where:

$C_{\mathrm methane,t}$: Methane Carbon gain or loss $(grams / year)$ at year $t$

and:

$A_{\mathrm veg,t}=$ Area of vegetated habitat $(m^{2})$ at time $t$

$A_{\mathrm unveg,t}=$ Area of unvegetated habitat $(m^{2})$ at time $t$

$A_{\mathrm infill,t}=$ Area of infill $(m^{2})$ at time $t$

$\delta_{\mathrm methane, veg}=$ Efflux rate of methane in vegetated habitat $(grams / (m^{2} * year))$

$\delta_{\mathrm methane, unveg}=$ Efflux rate of methane in unvegetated habitat $(grams / (m^{2} * year))$

$\delta_{\mathrm methane, infill}=$ Efflux rate of methane in infill sediment $(grams / (m^{2} * year))$

----

### *Nitrous Oxide (Nox)*
$$
C_{\mathrm nox,t} = (A_{\mathrm veg,t} * \delta_{\mathrm nox, veg}) + (A_{\mathrm unveg,t} * \delta_{\mathrm nox, unveg}) + (A_{\mathrm infill,t} * \delta_{\mathrm nox, infill})
$$

where:

$C_{\mathrm nox,t}$: Sediment Carbon at year t $(grams / year)$

and:

$A_{\mathrm veg,t}=$ Area of vegetated habitat $(m^{2})$ at time $t$

$A_{\mathrm unveg,t}=$ Area of unvegetated habitat $(m^{2})$ at time $t$

$A_{\mathrm infill,t}=$ Area of infill $(m^{2})$ at time $t$

$\delta_{\mathrm nox, veg}=$ Efflux rate of nox in vegetated habitat $(grams / (m^{2} * year))$

$\delta_{\mathrm nox, unveg}=$ Efflux rate of nox in unvegetated habitat $(grams / (m^{2} * year))$

$\delta_{\mathrm nox, infill}=$ Efflux rate of nox in infill sediment $(grams / (m^{2} * year))$

----

## Parameter Estimation
The following parameters are assigned in the model by drawing from a normal distribution of possible parameter values whose mean and standard deviation are derived from the literature.

* $\rho_{\mathrm veg}=$ Density of carbon in vegetated habitat $(grams / m^{3})$

* $\rho_{\mathrm unveg}=$ Density of carbon in unvegetated habitat $(grams / m^{3})$

* $\rho_{\mathrm infill}=$ Density of carbon in infill sediment $(grams / m^{3})$

* $d_{\mathrm acc,t}=$ Depth of sediment accretion $(m)$ at time $t$ 

* $\delta_{\mathrm biomass}=$ Density of biomass Carbon $(grams / m^{2})$ at time $t$

* $\delta_{\mathrm nox, veg}=$ Efflux rate of nox in vegetated habitat $(grams / (m^{2} * year))$

* $\delta_{\mathrm nox, unveg}=$ Efflux rate of nox in unvegetated habitat $(grams / (m^{2} * year))$

* $\delta_{\mathrm nox, infill}=$ Efflux rate of nox in infill sediment $(grams / (m^{2} * year))$

* $\delta_{\mathrm nox, veg}=$ Efflux rate of nox in vegetated habitat $(grams / (m^{2} * year))$

* $\delta_{\mathrm nox, unveg}=$ Efflux rate of nox in unvegetated habitat $(grams / (m^{2} * year))$

* $\delta_{\mathrm nox, infill}=$ Efflux rate of nox in infill sediment $(grams / (m^{2} * year))$

For example:

$$
\rho_{\mathrm veg}\sim~N(\mu_{\mathrm veg}, \sigma_{\mathrm veg})
$$

where,

$$
\mu_{\mathrm veg}=20
$$

$$
\sigma_{\mathrm veg}=2
$$

## Assumptions
**Terms**:

  1. carbon constituents: `methane`, `nitrous_oxide`, `biomass`, and `soil`

**Growth of seagress meadows**:

 1. The area of seagrass meadows grows over time as a `logistic` curve, meaning growth is initially somewhat slower, accelerates for a period, and then tapers off, or saturates, at some level that represents the maximum potential area for the restored seagrass meadow.
 2. The logistic growth curve for seagrass meadows depends on the restoration scenario.
 3. `Baseline` simulations for each restoration method grow in area at the same rate as their respective restoration scenario; differences in carbon dynamics are driven solely by the fact that `Baseline` scenarios are comprised of unvegetated habitat.
     1. This assumption is made to facilitate comparison among the restoration method and the baseline. The idea is that as the restored area grows, the amount of unrestored habitat that it represents also grows. That means that to compare the growing, let's say, carbon sequstration rates of restored seagrass meadows to those of an unrestored area half the size would be to overstate the net gain of the restoration. So this assumption lets us avoid that bias.

**Carbon dynamics**:

 1. Carbon constituents are a vary between vegetated (`Infill`, `Seeding Restoration`, and `Transplant Restoration`) and unvegetated (`Baseline`) habitats.
     - Within vegetated habitats, differences in carbon constituent dynamics are driven solely by variation in the amount of vegetated habitat.
 2. carbon constituents are a function of `area` and their per-area values are drawn from a normal distribution with a mean and standard deviation that are derived from the literature (see `Citations` and `Parameter Estimation` sections (once they exist)).
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
```
