# Model of the school garden (see Index.RMD for the explaination and posthoc)

# We need these functions to run our model ####
# value_varrier function to add variability to values
source("functions/vv.R")

# chance event function to assess the chances 
# mostly for risks
source("functions/chance_event.R")

# discount values for NPV (time value for money)
source("functions/discount.R")

# Model testing ####

# make variables for testing our model (only for construction)
source("functions/make_variables.R")
source("functions/estimate_read_csv.R")
make_variables(decisionSupport::estimate_read_csv(paste("inputs_kontum_garden.csv",sep="")))

# The model ####

kontum_garden_function <- function(x, varnames){
  
  # Costs####
  
  # Establishment costs 

  # Could be subject to cut-off values based on land area and number of students
  garden_establishment_costs <- compost_starting + # getting started with the compost
    garden_designing_costs + # garden design costs (hiring a planner) 
    equipment_cost + # this is a high value because we may need a lot of equipment, netting, trellis for plants to climb
    # could be a smart system (full automation)... 
    construction_cost  

  establishment_cost_year_one <- garden_establishment_costs 
  
  garden_maintenance_cost <- maintaining_labor + # technical staff etc
            # 2-3 hours per day to manage a garden of this rough size
    seed_costs + # seeds and seedlings each year
    fertilizer + # EM and other helpers for compost
    plant_protection  # IPM for plant protection
    
  maintenance_cost_annual <- garden_maintenance_cost
  
  # Add up all annual costs
  total_cost <- vv(maintenance_cost_annual, 
                         var_CV = CV_value, 
                         n = number_of_years, 
                         relative_trend = inflation_rate) #percentage of increase each year
  
  # Calculate management plus establishment costs in the first year
  total_cost[1] <- establishment_cost_year_one + maintenance_cost_annual #make sure the first is establishment_cost_year_one
  
  # Risks ####
  
  # These are 'ex-ante' risks, or risks understood when making a decision
  # we use these to multiply the values for the relevant benefits
  # the minimum values are effectively a reduction in the benefits
  # used to multiply benefits (by a number 90% likely)
  
  garden_function_risk <-  min(if_biophysical_good, 
                               if_community_likes, #damage garden
                               if_effective_manage) # well managed garden
  
  garden_nutrition_risk <- min(if_garden_yield_enough, # goes to the meal
                               if_garden_healthy, # good food from the garden
                               if_effective_manage) # well managed garden
  
  # ex-ante community risks
  community_risk <- min(if_community_likes, # support and promote
                        if_effective_manage) # well managed garden makes good impression
  
  # ex-ante ecological risks
  ecological_risk <- min(if_offer_green_space, # offer green space
                        if_reduce_polution) # offer habitat
  
  
  #health benefits from gardens
  health_value <- child_veg_access + child_healthier_choices +
    women_veg_access + women_healthier_choices +
    elderly_veg_access + elderly_healthier_choices +
    others_veg_access + others_healthier_choices 
    
  health_related_value <-  vv(health_value, 
                              CV_value, 
                              number_of_years, 
                              relative_trend = inflation_rate) * garden_nutrition_risk
  
  # green space environment
  environmental_value <- green_space_value + reduce_polution_value 
  
  # some discussion of carbon credit values (not included)
  environment_related_value <-  vv(environmental_value, 
                              CV_value, 
                              number_of_years, 
                              relative_trend = inflation_rate) * ecological_risk
  
  
  # Add up all benefits ####
  total_benefit <- health_related_value + environment_related_value 
    
  # Final result of the costs and benefits
  garden_intervention_result <- total_benefit - total_cost
  
  ## Alternative do nothing result / costs and benefits
  
  # the alternative uses of the land (very crude assessment of other possible uses)
  land_used_yes_no <- chance_event(if_not_fallow, # some chance that the land will be fallow
                                 value_if = 1, 
                                 value_if_not = 0)
  
  non_garden_value <- if (land_used_yes_no == 1) {
     vv(value_of_non_garden_land_use,
                            CV_value, 
                            number_of_years, 
                            relative_trend = inflation_rate) 
  } else {
     vv(value_of_fallow_non_garden_land,
                            CV_value, 
                            number_of_years, 
                            relative_trend = inflation_rate)
  }
  
  
  total_benefit_no <- non_garden_value 
  
  # baseline costs for comparison (rough estimate)
  total_cost_no <- vv(costs_of_non_garden_land_use, 
                         var_CV = CV_value, 
                         n = number_of_years, 
                      relative_trend = inflation_rate)
  
  # subtract 
  
  no_intervention_result <- total_benefit_no - total_cost_no
  
  # The difference is inherent in our calculation so we do not need the 
  # comparative NPV here, just discount the intervention result
  # calculate the Net Present Value (NPV) with with the specified discount rate
  NPV_interv <-
    discount(x = garden_intervention_result, 
             discount_rate = discount_rate, 
             calculate_NPV = TRUE)
  
  # NPV no intervention ####
  NPV_no_interv <-
    discount(x = no_intervention_result, 
             discount_rate = discount_rate, 
             calculate_NPV = TRUE)
  
  # Beware, if we do not name our outputs (left-hand side of the equal sign) in the return section, 
  # the variables will be called output_1, _2, etc.
  return(list(NPV_garden = NPV_interv,
              NPV_no_garden = NPV_no_interv,
              decision = NPV_interv - NPV_no_interv,
              total_costs = sum(total_cost),
              Cashflow_garden = garden_intervention_result))
}

