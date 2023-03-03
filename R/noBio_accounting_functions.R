# noBio_accounting_functions.R

# Functions to account for biomass negative emissions


#' prepare_sector_carbon_flows
#'
#' Calculate proportions of carbon in and out for each sector and energy input
#'
#'
#' @param energy.consumption.by.tech rgcam output from energy consumption by tech query. Columns must be 'Units', 'scenario', 'region', 'sector', 'subsector', 'technology', 'input', 'year', and 'value'
#' @param service.output.by.tech rgcam output from output by tech query. Columns must be 'Units', 'scenario', 'region', 'sector', 'subsector', 'technology', 'output', 'year', and 'value'
#' @param market.prices rgcam output from prices of all markets query. Columns must be 'Units', 'scenario', 'year', 'market', 'value'
#' @return dataframe of carbon inputs and outputs
#' @author TRW
#' @export

prepare_sector_carbon_flows <- function(energy.consumption.by.tech,
                                        service.output.by.tech,
                                        market.prices){

  # ----------------------------------------
  # get energy demanded by tech
  # ----------------------------------------
  energy.d <- energy.consumption.by.tech %>%
    dplyr::filter(Units == "EJ") %>%
    dplyr::select(-Units) %>%
    dplyr::arrange(scenario) %>%
    dplyr::distinct()


  # ----------------------------------------
  # get the market names for the markets corresponding to each input
  # ----------------------------------------
  inputs_match <- paste0(
    stringr::str_replace_all(
      paste(unique(energy.d$input), collapse = "$|"),
      c("\\(" = "\\\\(", "\\)" = "\\\\)")),
    "$")

  market.d <- market.prices %>%
    dplyr::select(scenario, market) %>%
    dplyr::filter(grepl(inputs_match, market)) %>%
    tidyr::separate(market, into = c("market.name", "input"),
                    sep = paste0("(?=", inputs_match, ")"), extra = "merge") %>%
    dplyr::filter(input %in% unique(energy.d$input))


  # ----------------------------------------
  # merge energy consumption with market names and clean it up
  # ----------------------------------------
  input.d.raw <- merge(energy.d, market.d, all.x=T) %>% dplyr::distinct()

  input.d <- input.d.raw %>%
    # remove the cycle and walk transport sectors
    dplyr::filter(!(input == "renewable" & grepl("trn", sector))) %>%
    # remove non-energy inputs (credits)
    dplyr::filter(!(input %in% c("oil-credits", "bio-ceiling", "ELEC_RPS", "RFS-adv",
                                 "RFS-conv", "elec-ceiling","bio_externality_cost",
                                 "ELEC_SOLAR", "ELEC_WIND"))) %>%
    # assign market name to region name for traded sectors
    dplyr::mutate(market.name = dplyr::case_when(
      grepl("traded", input) & is.na(market.name) ~ region,
      T ~ market.name)) %>%
    # rename values
    dplyr::rename(q.in = value) %>%
    dplyr::distinct()

  # ----------------------------------------
  # get energy outputs by technology
  # ----------------------------------------
  output.d <- service.output.by.tech %>%
    dplyr::select(-Units) %>%
    dplyr::rename(q.out = value)


  # ----------------------------------------
  # get carbon coefs (copied from CO2 emissions by sector (no bio) query)
  # ----------------------------------------
  Ccoef.d <- tibble::tibble(PrimaryFuelCO2Coef.name = c("biomass", "coal", "crude oil", "delivered biomass", "delivered coal",
                                                        "delivered gas", "gas pipeline", "gas processing",
                                                        "limestone", "natural gas", "refined liquids enduse",
                                                        "refined liquids industrial", "refining", "regional biomass",
                                                        "regional biomassOil", "regional coal", "regional corn for ethanol",
                                                        "regional natural gas", "regional oil", "regional sugar for ethanol",
                                                        "traded unconventional oil", "unconventional oil",
                                                        "unconventional oil production", "wholesale gas"),
                            PrimaryFuelCO2Coef = c(0, 27.3, 19.6, 23, 27.3, 14.2, 14.2, 14.2, 0.08, 14.2,
                                                   19.6, 19.6, 19.6, 23, 19.6, 27.3, 19.6, 14.2, 19.6, 19.6,
                                                   19.6, 21.1, 19.6, 14.2))


  # ----------------------------------------
  # merge energy inputs and outputs with carbon coefs
  # and calculate carbon inputs and outputs
  # ----------------------------------------
  input.d.C <- merge(input.d, Ccoef.d, by.x="input", by.y="PrimaryFuelCO2Coef.name", all.x=T) %>%
    dplyr::rename(in.Ccoef = PrimaryFuelCO2Coef) %>%
    dplyr::mutate(
      # give inputs not included in coefs a coef of 0
      in.Ccoef = dplyr::case_when(is.na(in.Ccoef) ~ 0,
                                  T ~ in.Ccoef),
      # multiply energy input by coefs to get carbon input
      C.in = q.in*in.Ccoef) %>%
    # calculate total carbon input per technology (all inputs combined)
    dplyr::group_by(scenario, region, sector, subsector, technology, year) %>%
    dplyr::mutate(C.in.subSector = sum(C.in)) %>% dplyr::ungroup()


  output.d.C <- merge(output.d, Ccoef.d, by.x="sector", by.y= "PrimaryFuelCO2Coef.name", all.x=T) %>%
    dplyr::rename(out.Ccoef = PrimaryFuelCO2Coef) %>%
    dplyr::mutate(
      # give inputs not included in coefs a coef of 0
      out.Ccoef = dplyr::case_when(is.na(out.Ccoef) ~ 0,
                                   T ~ out.Ccoef),
      # multiply energy input by coefs to get carbon input
      C.out = q.out*out.Ccoef)

  # ----------------------------------------
  # merge inputs and outputs together and make adjustments
  # ----------------------------------------
  all.d <- merge(input.d.C, output.d.C, by = c("scenario", "region", "sector", "subsector", "technology", "year"),
                 all.x=T) %>%
    dplyr::mutate(
      # for technologies with multiple input fuels, use the ratio of the input carbon from each input fuel to the
      # total input carbon for the technology to calculate the output carbon for each input fuel
      C.in.ratio = C.in / C.in.subSector,
      C.out.adj = C.out * C.in.ratio,
      C.out = C.out.adj,
      # replace missing carbon outputs with o
      C.out = dplyr::case_when(is.na(C.out) ~ 0,
                               T ~ C.out),
      # combine inputs with their market names and sectors with their regions
      FQ.input = paste0(market.name, input),
      FQ.sector = paste0(region, sector)) %>%
    dplyr::select(region, sector, subsector, technology, scenario, year, C.in, market.name,
                  input, q.in, in.Ccoef, q.out, out.Ccoef, C.out, FQ.input, FQ.sector)

  # ----------------------------------------
  # return final inputs and outputs data
  # ----------------------------------------
  return(all.d)
}

#' generate_sector_output_coefs
#'
#' Make a list of carbon in/out proportions across sectors for a given input
#'
#'
#' @param curr.FQinput the energy input to calculate carbon in/out proportions for
#' @param data output from prepare_sector_carbon_flows filtered to one scenario
#' @return list of carbon in/out proportions for each sector
#' @author TRW
#' @export

generate_sector_output_coefs <- function(curr.FQinput, data) {
  # subset the current input
  data.subset <- data %>%
    dplyr::filter(FQ.input == curr.FQinput)

  # calculate total energy consumption for each year
  input.totals <- data.subset %>%
    dplyr::group_by(FQ.input, year) %>%
    dplyr::summarize(q.total = sum(q.in)) %>%
    dplyr::ungroup()

  # calculate energy consumption and carbon in/out for each region + sector + year combo
  sector_agg <- data.subset %>%
    dplyr::group_by(region, sector, FQ.sector, market.name, input, FQ.input, year) %>%
    dplyr::summarize(q.in = sum(q.in),
                     C.in = sum(C.in),
                     C.out = sum(C.out),
                     q.out = sum(q.in)) %>%
    dplyr::ungroup()

  # calculate out/in coefficient for each region + sector + year combo
  coefs <- merge(sector_agg, input.totals) %>%
    dplyr::mutate(share = q.in/q.total,
                  carbon.ratio = dplyr::case_when(C.in == 0 ~ 0,
                                                  T ~ C.out/C.in),
                  coef = share*carbon.ratio)

  # initiate return list
  ret <- list()

  # if any carbon goes (emissions continue downstream), find the proportion that goes
  coefs.subset.go <- coefs %>%
    dplyr::filter(coef > 0)

  if(nrow(coefs.subset.go) > 0 ) {
    ret[["C.go"]] <- reshape2::dcast(coefs.subset.go, region + sector + FQ.sector + FQ.input ~ year, value.var="coef")
  }

  # if any carbon stays (emissions allocated to the corresponding sector), find the proportion that stays
  coefs.subset.stay <- coefs %>%
    dplyr:: mutate(coef = share*(1.0 - carbon.ratio)) %>%
    dplyr::filter(coef > 0)

  if(nrow(coefs.subset.stay) > 0 ) {
    ret[["C.stay"]] <- reshape2::dcast(coefs.subset.stay, region + sector + FQ.sector + FQ.input ~ year, value.var="coef")
  }

  return(ret)
}


#' apply_coefs
#'
#' apply carbon in/out coefficients recursively through sectors for a given input
#'
#'
#' @param curr.FQ.sector the sector to start with (a biomass sector including the region name, such as 'Thailandregional biomass')
#' @param curr.emiss CO2 emissions by technology data filtered to a given scenario, region, and biomass sector
#' @param coefs list of carbon stay/go coefficients for each input
#' @return list of amounts of emissions staying in and leaving from each sector
#' @author TRW
#' @export

apply_coefs <- function(curr.FQ.sector, curr.emiss, coefs) {

  if(nrow(curr.emiss) > 0 && any(abs(curr.emiss) > 1e-3, na.rm=T)) {
    # get stay coefs and go coefs for the current sector
    curr.coef <- coefs[[curr.FQ.sector]]
    curr.coef.stay <- curr.coef[["C.stay"]]
    curr.coef.go <- curr.coef[["C.go"]]

    # initiate return list
    ret <- list()

    # if any carbon stays in the current sector, calculate the associated emissions
    if(!is.null(curr.coef.stay)) {
      common.years <- intersect(names(curr.coef.stay), names(curr.emiss))
      if(length(common.years) > 1) {
        # multiply stay coefficients by emissions for each sector
        emiss.stay <- cbind(curr.coef.stay[,c("region", "sector")],
                            sweep(curr.coef.stay[,common.years,drop=F], MARGIN=2,
                                  as.numeric(curr.emiss[,common.years]), '*'))
        ret[[1]] <- emiss.stay
      }
    }

    # if any carbon leaves the current sector, calculate the emissions that leave
    # and then call the function recursively on the downstream sectors
    if(!is.null(curr.coef.go)) {
      common.years <- intersect(names(curr.coef.go), names(curr.emiss))
      if(length(common.years) > 1) {
        # multiply go coefficients by emissions for each sector
        # (these emissions are fed into the next call)
        emiss.go <- sweep(curr.coef.go[,common.years,drop=F], MARGIN=2,
                          as.numeric(curr.emiss[,common.years,drop=F]), '*')
        for(row in rownames(curr.coef.go)) {
          # for sectors with any amount of carbon leaving, call this function
          # recursively on the sectors that the sector passes to as an input
          ret <- c(ret, apply_coefs(curr.coef.go[row,"FQ.sector"], emiss.go[row,],
                                    coefs))
        }
      }
    }
    return(ret)
  }
}


#' allocate_biomass_emiss_reductions
#'
#' perform full biomass accounting (transform co2 emissions by sector into co2 emissions by sector (no bio))
#'
#'
#' @param energy.consumption.by.tech rgcam output from energy consumption by tech query. Columns must be 'Units', 'scenario', 'region', 'sector', 'subsector', 'technology', 'input', 'year', and 'value'
#' @param service.output.by.tech rgcam output from output by tech query. Columns must be 'Units', 'scenario', 'region', 'sector', 'subsector', 'technology', 'output', 'year', and 'value'
#' @param market.prices rgcam output from prices of all markets query. Columns must be 'Units', 'scenario', 'year', 'market', and 'value'
#' @param emiss.by.tech rgcam output from CO2 emissions by tech query. Columns must be 'Units', 'scenario', 'region', 'sector', 'subsector', 'technology', 'ghg', 'year', and 'value'
#' @param bio.sectors biomass sectors to account for (e.g. 'regional biomass')
#' @return dataframe of CO2 emissions by sector with biomass negative emissions accounted for
#' @author TRW
#' @export

allocate_biomass_emiss_reductions <- function(energy.consumption.by.tech,
                                              service.output.by.tech,
                                              market.prices,
                                              emiss.by.tech,
                                              bio.sectors){

  # ------------------------------
  # get sector carbon flows
  # ------------------------------
  sector_carbon_flows <- prepare_sector_carbon_flows(energy.consumption.by.tech,
                                                     service.output.by.tech,
                                                     market.prices)


  # ------------------------------
  # prepare emissions data
  # ------------------------------

  scenario_list <- unique(emiss.by.tech$scenario)
  emiss.units <- unique(emiss.by.tech$Units)
  emiss.d <- emiss.by.tech %>%
    dplyr::select(scenario, region, sector, subsector, technology, year, value)


  # ------------------------------
  # loop through scenarios and apply coefficients
  # ------------------------------

  # initiate return tibble
  ret <- tibble::tibble(scenario = character(),
                        region = character(),
                        sector = character(),
                        year = numeric(),
                        value = numeric())

  for(SCE in scenario_list){
    # get sector carbon flows for scenario
    sector_carbon_flows.sce <- sector_carbon_flows %>%
      dplyr::filter(scenario == SCE)

    # get the carbon stay/go coefficients for each input
    all.coefs <- lapply(unique(sector_carbon_flows.sce$FQ.input),
                        generate_sector_output_coefs, sector_carbon_flows.sce)

    names(all.coefs) <- unique(sector_carbon_flows.sce$FQ.input)

    # prepare emissions for scenario
    emiss.d.sce <- emiss.d %>%
      dplyr::filter(scenario == SCE) %>%
      # aggregate emissions to the sector level
      dplyr::group_by(region, sector, year) %>%
      dplyr::summarise(value = sum(value)) %>%
      # widen by years
      tidyr::pivot_wider(names_from = year, values_from = value)

    # get years an regions in the scenario
    year.cols <- grep("\\d{4}", names(emiss.d.sce), value=T)
    regions <- unique(emiss.d.sce$region)

    # initiate list of emissions adjustments
    emiss.adj.list <- list()


    # loop through bio sectors and apply coefficients
    for(bio.sector in bio.sectors) {

      for(reg in regions) {
        # apply coefficients to the input corresponding to the bio sector in each region
        # and add the adjustments to the list
        emiss.adj.list <- c(emiss.adj.list,
                            apply_coefs(paste0(reg, bio.sector),
                                        emiss.d.sce[emiss.d.sce$sector == bio.sector &
                                                      emiss.d.sce$region == reg, year.cols],
                                        all.coefs))
      }
    }


    emiss.adj.d <- plyr::rbind.fill(emiss.adj.list) %>%
      tidyr::pivot_longer(-c(region, sector), names_to = "year", values_to = "value") %>%
      dplyr::filter(!is.na(value))

    # prepare emissions for joining with emissions reductions
    emiss.d.sce.prep <- emiss.d.sce %>%
      tidyr::pivot_longer(-c(region, sector),
                          names_to = "year", values_to = "value") %>%
      dplyr::filter(!is.na(value),
                    !sector %in% bio.sectors)

    # join emissions with emissions reductions
    # and sum by sector
    emiss.final <- rbind(emiss.d.sce.prep, emiss.adj.d) %>%
      dplyr::group_by(region, sector, year) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::mutate(scenario = SCE)

    # add final emiss data to output tibble
    ret <- rbind(ret, emiss.final)
  }

  # add units back in
  if(length(emiss.units == 1)){
    ret <- ret %>%
      dplyr::mutate(Units = emiss.units)
  }

  # return final data table with all scenarios
  return(ret)
}


